# clean_env.tcl — the environment an .exp suite starts its TUI from.
#
# The .exp suites are run from wherever a person is, and that is often *inside*
# a running evo session: an evo-swarm lane is where an agent runs them.  A pty
# test would then inherit the caller's own environment and read it as an
# answer about itself.  Two, both real:
#
#   EVO_SESSIONS_DIR  a test that submits a prompt and then reads the journal
#                     it just wrote — out of the scratch home it gave the
#                     child — finds nothing: the journal went to the
#                     *caller's* sessions directory;
#   EVO_BABY_EVO=0    the caller switched an extension off for itself, and a
#                     test that has it on fails.
#
# So a run gets a *regular* environment: CLEAN_SESSION_ENV unsets every EVO_*
# variable the suite inherited, except the two kinds it is handed on purpose —
#
#   EVO_HOME     the suite points it at its own throwaway home;
#   EVO_TEST_*   the documented knobs (Makefile: EVO_TEST_BASE_URL,
#                EVO_TEST_API_KEY, EVO_TEST_MODEL, EVO_TEST_VISION_MODEL).
#
# The rule, and the reason, are tests/env.lisp for the Lisp runners and
# tests/clean_env.py for the Python suites: three places, one rule, so the
# counts are the same out of a shell, out of CI and out of an evo-swarm lane.
# It is a *prefix* rule rather than a list of names, so a variable the product
# grows later is cleared without a second edit here; the cost is that a knob a
# suite must be *told* through the environment has to be named EVO_TEST_*.

proc kept_env_name {name} {
    # Variables a suite sets for the run itself, and must survive the clearing.
    return [expr {$name eq "EVO_HOME" || [string match "EVO_TEST_*" $name]}]
}

proc session_env_name {name} {
    # Whether NAME describes the session that started this run: an EVO_*
    # variable that is not one of the two kinds a run is handed.
    return [expr {[string match "EVO_*" $name] && ![kept_env_name $name]}]
}

proc clean_session_env {} {
    # Unset every inherited EVO_* variable but EVO_HOME and EVO_TEST_*.
    #
    # Naming them is what makes a run that inherited a session say so — names
    # only, because a provider key may be among them.  Returns the names it
    # cleared, sorted, so a caller can assert on them.
    set cleared {}
    foreach name [array names ::env] {
        if {[session_env_name $name]} { lappend cleared $name }
    }
    set cleared [lsort $cleared]
    foreach name $cleared { unset ::env($name) }
    if {[llength $cleared]} {
        puts ";; env: unset [join $cleared {, }] — a test run gets a regular environment"
    }
    return $cleared
}
