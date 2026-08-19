#!/bin/sh
# integration.sh — live end-to-end tests against a running Anthropic-compatible
# backend.  Nothing here is hardcoded to one proxy: point it anywhere via env.
#
#   EVO_TEST_BASE_URL  provider base url
#   EVO_TEST_API_KEY   bearer / x-api-key
#   EVO_TEST_MODEL     model id to drive
#   EVO_TEST_VISION_MODEL  optional: a model that accepts image input, for the
#                          headless --image test (that test skips without it)
#
# NONE of these have defaults: no backend, key, or model id is baked into the
# repo.  The dev values live in project memory (.evo/, git-ignored).  With any
# of them unset this is a hard failure — there is nothing to test against.
#
# Covers the exit criteria in scope:
#   streamed tool-call round-trip
#   multi-turn task in print mode; kill -9 mid-task; resume cleanly
#   goals: a goal run that terminates via audited update_goal :complete
#   image input: --image attached to a headless prompt, read by a vision model
#   image input: the agent opening an image itself with the read tool
set -u

: "${EVO_TEST_BASE_URL:=}"
: "${EVO_TEST_API_KEY:=}"
: "${EVO_TEST_MODEL:=}"
: "${EVO_TEST_VISION_MODEL:=}"

if [ -z "$EVO_TEST_BASE_URL" ] || [ -z "$EVO_TEST_API_KEY" ] || [ -z "$EVO_TEST_MODEL" ]; then
    echo "integration: set EVO_TEST_BASE_URL, EVO_TEST_API_KEY and EVO_TEST_MODEL" >&2
    echo "             (dev values are in this project's memory)" >&2
    exit 1
fi

# The supervisor passes these to its child; if THIS script is itself launched
# from inside a supervised evo (e.g. an agent's bash tool), they leak in and
# every evo we spawn thinks it is already the supervised child — no supervisor,
# so the crash-recovery test can't work.  Scrub them for a clean slate.
unset EVO_SUPERVISED_CHILD EVO_HEARTBEAT_FILE

repo=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
evo="$repo/build/evo"
scratch=$(mktemp -d /tmp/evo-integration.XXXXXX)
export EVO_HOME="$scratch/home"
work="$scratch/work"
mkdir -p "$EVO_HOME" "$work"

# Config is interpolated (unquoted heredoc): the values come from the env.
cat > "$EVO_HOME/init.lisp" <<EOF
(evo:register-model "$EVO_TEST_MODEL"
  :provider :anthropic :api :anthropic-messages
  :context-window 1000000 :max-output 64000)
(evo:set-setting :model "$EVO_TEST_MODEL")
(evo:set-setting :thinking :xhigh)
(evo:register-provider :anthropic
  :base-url "$EVO_TEST_BASE_URL"
  :api-key "$EVO_TEST_API_KEY")
EOF

# The vision model is a second registration, so --image can pick it per run
# without disturbing the model the rest of the suite drives.
if [ -n "$EVO_TEST_VISION_MODEL" ]; then
    cat >> "$EVO_HOME/init.lisp" <<EOF
(evo:register-model "$EVO_TEST_VISION_MODEL"
  :provider :anthropic :api :anthropic-messages
  :context-window 1000000 :max-output 64000 :vision t)
EOF
fi

pass=0; fail=0; skip=0
report() {
    if [ "$1" -eq 0 ]; then pass=$((pass+1)); echo "PASS: $2"
    else fail=$((fail+1)); echo "FAIL: $2"; fi
}
report_skip() { skip=$((skip+1)); echo "SKIP: $1"; }

# Preflight: tests 1-5 drive a real model.  If the backend isn't reachable,
# skip them cleanly (rather than fail) so `make integration` on a box without
# a backend doesn't masquerade as a code regression.  Test 7 needs no network.
if curl -fsS -m 5 "$EVO_TEST_BASE_URL/v1/models" \
        -H "Authorization: Bearer $EVO_TEST_API_KEY" >/dev/null 2>&1; then
    live=1
else
    live=0
    echo "note: backend not reachable/authorized at $EVO_TEST_BASE_URL — skipping live tests"
    echo "      (set EVO_TEST_API_KEY — dev value is in project memory — plus"
    echo "       EVO_TEST_BASE_URL / EVO_TEST_MODEL to point elsewhere)"
fi

if [ "$live" -eq 1 ]; then

# --- Test 1: multi-turn tool round-trip in print mode -----------------
(
    cd "$work"
    "$evo" -p "Run 'echo evo-integration-marker' with the bash tool, then use the write tool to create round-trip.txt containing exactly that command's output, then read it back to verify." \
        >/dev/null 2>"$scratch/t1.err"
)
t1=$?
[ $t1 -eq 0 ] && grep -q "evo-integration-marker" "$work/round-trip.txt" 2>/dev/null
report $? "print-mode multi-turn tool round-trip"

# --- Test 2: kill -9 mid-task, resume cleanly -----------------------
# --no-supervisor: this test wants the raw process killed and a MANUAL
# resume; supervised auto-restart is test 4's subject.
(
    cd "$work"
    "$evo" --no-supervisor -p "First run 'sleep 20 && echo slept' with the bash tool (do not shorten the sleep). After it finishes, use the write tool to create killed-task.txt with content: task-finished" \
        >/dev/null 2>"$scratch/t2.err" &
    pid=$!
    # Wait until the session journal exists (first assistant message flushed),
    # then a bit longer so the sleep tool call is in flight, then kill -9.
    for i in $(seq 1 60); do
        [ -n "$(find "$EVO_HOME/sessions" -name '*.sexp' 2>/dev/null | head -1)" ] && break
        sleep 1
    done
    sleep 6
    kill -9 "$pid" 2>/dev/null
    wait "$pid" 2>/dev/null
    # The journal must be intact on disk (write-ahead), and resumable.
    "$evo" --no-supervisor --resume -p "Continue the task you were doing; the process was killed mid-way. Finish the remaining steps." \
        >/dev/null 2>>"$scratch/t2.err"
)
t2=$?
[ $t2 -eq 0 ] && grep -q "task-finished" "$work/killed-task.txt" 2>/dev/null
report $? "kill -9 mid-task, resume cleanly"

# --- Test 3: goal run with audited completion ---------------------------------
(
    cd "$work"
    "$evo" --goal "Create a file named goal-proof.txt in the current directory containing exactly the single line: goal-done — then verify it exists with the bash tool and declare the goal complete." \
        >/dev/null 2>"$scratch/t3.err"
)
t3=$?
[ $t3 -eq 0 ] && grep -q "goal-done" "$work/goal-proof.txt" 2>/dev/null \
    && grep -q "goal .*: complete" "$scratch/t3.err"
report $? "goal run terminates via update_goal complete"

# --- Test 4: built-in supervision survives an induced crash --------------
work4="$scratch/work4"
mkdir -p "$work4"
(
    cd "$work4"
    # bash tool calls run under a session-detach wrapper, so $PPID is the
    # wrapper, not evo.  The bash tool exports EVO_PID = the evo (worker)
    # process, so killing it actually crashes evo and exercises the supervisor.
    "$evo" --goal "Two-step goal. Step 1: use the bash tool to run exactly: kill -9 \$EVO_PID — this kills the evo process running you; that is EXPECTED and required. Run it ONLY ONCE: if the session already shows an interrupted kill attempt, do NOT run it again. Step 2 (after you are restarted): create crash-proof.txt containing the single word survived, verify it exists with bash, then declare the goal complete." \
        >/dev/null 2>"$scratch/t4.err"
)
t4=$?
[ $t4 -eq 0 ] && grep -q "survived" "$work4/crash-proof.txt" 2>/dev/null \
    && grep -q "restarting --resume" "$scratch/t4.err"
report $? "supervisor: induced crash -> restart -> resume -> goal complete"

# --- Test 5: compaction fires mid-task and the task still finishes -------
work5="$scratch/work5"
mkdir -p "$work5/.evo"
cat > "$work5/.evo/init.lisp" <<'EOF'
(evo:set-setting :compact-reserve 999999)
(evo:set-setting :compact-keep-recent 100)
EOF
(
    cd "$work5"
    "$evo" -p "Do these steps one at a time, each as its own tool call: run bash 'echo step-one', then run bash 'echo step-two', then run bash 'echo step-three', then write compact-proof.txt containing compact-done." \
        >/dev/null 2>"$scratch/t5.err"
)
t5=$?
session5=$(ls -t "$EVO_HOME"/sessions/*work5*/*.sexp 2>/dev/null | head -1)
[ $t5 -eq 0 ] && grep -q "compact-done" "$work5/compact-proof.txt" 2>/dev/null \
    && [ -n "$session5" ] && grep -q "(:type :compaction" "$session5"
report $? "compaction fires mid-task, task completes"

# --- Test 6: headless --image reaches a vision model ---------------------
# The image is 64x64 of solid red, written byte for byte: what comes back has
# to be about the pixels, so a stubbed-out image path cannot pass this.
work_img="$scratch/work-image"
mkdir -p "$work_img"
if [ -n "$EVO_TEST_VISION_MODEL" ] && command -v xxd >/dev/null 2>&1; then
    printf '%s' '89504e470d0a1a0a0000000d4948445200000040000000400802000000250be6890000007949444154789cedcf410900300cc0c08aa87f651333117b1c8340045ce6ec7edd7041035ad0801634a0050d6841035ad0801634a0050d6841035ad0801634a0050d6841035ad0801634a0050d6841035ad0801634a0050d6841035ad0801634a0050d6841035ad0801634a0058f5daac540f1785aab140000000049454e44ae426082' \
        | xxd -r -p > "$work_img/red.png"
    (
        cd "$work_img"
        "$evo" --model "$EVO_TEST_VISION_MODEL" --image "$work_img/red.png" \
            -p "What single colour fills this image? Reply with exactly one line: colour-is-<word>." \
            >"$scratch/t-img.out" 2>"$scratch/t-img.err"
    )
    grep -qi "colour-is-red" "$scratch/t-img.out"
    report $? "headless --image: a vision model reads the attached image"

    # --- Test 6b: the AGENT opens the image itself -----------------------
    # Nobody attached anything here: the agent has to reach for `read` on a
    # png and look at what comes back.  Same red pixels, so an answer that
    # is not about them cannot pass.
    (
        cd "$work_img"
        "$evo" --model "$EVO_TEST_VISION_MODEL" \
            -p "Use the read tool on red.png in this directory and look at it. What single colour fills the image? Reply with exactly one line: colour-is-<word>." \
            >"$scratch/t-img2.out" 2>"$scratch/t-img2.err"
    )
    grep -qi "colour-is-red" "$scratch/t-img2.out"
    report $? "read tool: the agent opens an image file and sees it"
else
    report_skip "headless --image: a vision model reads the attached image"
    report_skip "read tool: the agent opens an image file and sees it"
fi

else
    report_skip "print-mode multi-turn tool round-trip"
    report_skip "kill -9 mid-task, resume cleanly"
    report_skip "goal run terminates via update_goal complete"
    report_skip "supervisor: induced crash -> restart -> resume -> goal complete"
    report_skip "compaction fires mid-task, task completes"
    report_skip "headless --image: a vision model reads the attached image"
    report_skip "read tool: the agent opens an image file and sees it"
fi

# --- Test 7: two separately launched agents mint distinct ids ------------
# A save-lisp-and-die image bakes its load-time random state, so this can
# only be caught against the built binary — in-process tests reseed per
# process and always pass.  The ids are minted before the first provider
# call, so a deliberately-unreachable base-url still proves the point (and
# keeps this test network-free).  (Config is required now, so this can't run
# --no-userspace: a probe model is registered per home instead.)
ids_of() {   # $1 = EVO_HOME -> "<session-id> <goal-id>"
    sed -n 's/.*:id "\([0-9a-f]\{16\}\)".*/\1/p;s/.*:goal-id "\([^"]*\)".*/\1/p' \
        "$1"/sessions/*/*.sexp 2>/dev/null | head -2 | tr '\n' ' '
}
work6="$scratch/work6"
mkdir -p "$work6"
# Launched together, in one cwd: the worst case for any clock-seeded RNG, and
# the exact scenario that reported the collision.
pids6=""
for h in a b; do
    mkdir -p "$scratch/home6$h"
    cat > "$scratch/home6$h/init.lisp" <<'EOF'
(evo:register-model "id-probe"
  :provider :anthropic :api :anthropic-messages
  :context-window 100000 :max-output 1000)
(evo:set-setting :model "id-probe")
(evo:register-provider :anthropic :base-url "http://127.0.0.1:9" :api-key "sk-x")
EOF
    (cd "$work6" && EVO_HOME="$scratch/home6$h" "$evo" --goal "id collision probe" \
        -p "hi" --no-supervisor) >/dev/null 2>&1 &
    pids6="$pids6 $!"
done
# Both journals land once the first (failed) provider turn is recorded; poll
# for them rather than sitting through the rest of the retry backoff.
for i in $(seq 1 150); do
    [ -n "$(find "$scratch/home6a/sessions" -name '*.sexp' 2>/dev/null | head -1)" ] &&
        [ -n "$(find "$scratch/home6b/sessions" -name '*.sexp' 2>/dev/null | head -1)" ] && break
    sleep 0.2
done
# Grouped redirect: swallows the shell's own "Killed: 9" job notice too.
{ kill -9 $pids6; wait $pids6; } 2>/dev/null
ids_a=$(ids_of "$scratch/home6a")
ids_b=$(ids_of "$scratch/home6b")
# Both runs must have produced ids (guards against a vacuous pass), and differ.
[ -n "$(echo "$ids_a" | tr -d ' ')" ] && [ "$ids_a" != "$ids_b" ]
report $? "separate processes mint distinct session/goal ids [$ids_a] vs [$ids_b]"

echo
echo "$pass passed, $fail failed, $skip skipped (scratch: $scratch)"
[ $fail -eq 0 ]
