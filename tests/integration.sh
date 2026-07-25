#!/bin/sh
# integration.sh — live tests against the local Anthropic-compatible proxy
# (ark-deepseek-v4-pro, thinking xhigh, per tests config).
#
# Covers the milestone exit criteria in scope for the MVP:
#   M0: streamed tool-call round-trip
#   M1: multi-turn task in print mode; kill -9 mid-task; resume cleanly
#   goals: a goal run that terminates via audited update_goal :complete
set -u

repo=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
evo="$repo/bin/evo"
scratch=$(mktemp -d /tmp/evo-integration.XXXXXX)
export EVO_HOME="$scratch/home"
work="$scratch/work"
mkdir -p "$EVO_HOME" "$work"

cat > "$EVO_HOME/settings.sexp" <<'EOF'
(:model "ark-deepseek-v4-pro"
 :thinking :xhigh
 :providers (:anthropic (:base-url "http://127.0.0.1:8787"
                         :api-key "sk-evo-default-dd196a205a364d844b67b52a9c418c8e")))
EOF

pass=0; fail=0
report() {
    if [ "$1" -eq 0 ]; then pass=$((pass+1)); echo "PASS: $2"
    else fail=$((fail+1)); echo "FAIL: $2"; fi
}

# --- Test 1 (M0/M1): multi-turn tool round-trip in print mode -----------------
(
    cd "$work"
    "$evo" -p "Run 'echo evo-integration-marker' with the bash tool, then use the write tool to create round-trip.txt containing exactly that command's output, then read it back to verify." \
        >/dev/null 2>"$scratch/t1.err"
)
t1=$?
[ $t1 -eq 0 ] && grep -q "evo-integration-marker" "$work/round-trip.txt" 2>/dev/null
report $? "print-mode multi-turn tool round-trip (M0/M1)"

# --- Test 2 (M1 exit): kill -9 mid-task, resume cleanly -----------------------
(
    cd "$work"
    "$evo" -p "First run 'sleep 20 && echo slept' with the bash tool (do not shorten the sleep). After it finishes, use the write tool to create killed-task.txt with content: task-finished" \
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
    "$evo" --resume -p "Continue the task you were doing; the process was killed mid-way. Finish the remaining steps." \
        >/dev/null 2>>"$scratch/t2.err"
)
t2=$?
[ $t2 -eq 0 ] && grep -q "task-finished" "$work/killed-task.txt" 2>/dev/null
report $? "kill -9 mid-task, resume cleanly (M1 exit)"

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

echo
echo "$pass passed, $fail failed (scratch: $scratch)"
[ $fail -eq 0 ]
