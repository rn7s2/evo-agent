"""clean_env — the environment this suite's children start from.

`make swarm-test` and friends are run from wherever a person is, and that is
often *inside* a running evo session: an evo-swarm lane, or an editor's
terminal.  A child that inherited that session would be answering questions
about its caller — its supervision (EVO_SUPERVISED_CHILD, EVO_SUPERVISOR_PID,
EVO_NO_SUPERVISOR), its session directory (EVO_SESSIONS_DIR), its token
(EVO_SERVE_TOKEN), the pid its shell tool exported (EVO_PID) — and a test of
"a server refuses a foreign token" or "a lane keeps its journals to itself"
would be testing the caller instead.

`clean()` is the same rule the Lisp runners apply (tests/env.lisp), with one
difference: these scripts are told what they need on the command line rather
than through the environment, so there is no EVO_TEST_* knob to keep.

Real credentials go too: every one of these suites drives a stub, and a child
that found a provider key would spend the reader's money instead of the stub's.
"""

import os

#: Kept because every caller replaces it with its own throwaway home.
KEPT = ("EVO_HOME",)

#: The documented knobs (Makefile: EVO_TEST_BASE_URL / _API_KEY / _MODEL,
#: EVO_TEST_VISION_MODEL): a suite that is *told* something keeps it.
KEPT_PREFIXES = ("EVO_TEST_",)

#: Provider credentials, by prefix.  A test is not a customer.
DROP_PREFIXES = ("ANTHROPIC_",)

DROP = ("OPENAI_API_KEY", "KIMI_API_KEY")


def dropped(env, extra=()):
    """The names `clean` would remove from ENV: what a run inherits that it must
    not.  A caller that wants to say what it dropped asks here."""
    return sorted(
        name
        for name in env
        if name not in KEPT
        and not name.startswith(KEPT_PREFIXES)
        and (name.startswith("EVO_") or name.startswith(DROP_PREFIXES) or name in DROP or name in extra)
    )


def clean(env=None, extra=(), **set_):
    """ENV (a copy of `os.environ` by default) with the caller's session removed,
    then SET_ applied on top.  EXTRA names go too."""
    out = dict(os.environ if env is None else env)
    for name in dropped(out, extra):
        out.pop(name, None)
    out.update(set_)
    return out
