#!/bin/sh
# Behavior proof for npda. Fails when output changes.
# Usage: ./tests/proof.sh <path-to-cc-binary>
set -u

BIN=${1:?usage: proof.sh <cc-binary>}
FAIL=0

pass() { echo "PASS $1"; }
fail() { echo "FAIL $1"; FAIL=1; }

# Run a case, then check the exit code and one output marker.
# Args: name, expected-rc, marker, command...
expect() {
    name=$1; want_rc=$2; marker=$3; shift 3
    out=$("$@" 2>&1); rc=$?
    if [ "$rc" != "$want_rc" ]; then
        fail "$name (rc=$rc, want $want_rc)"
        return
    fi
    case "$out" in
        *"$marker"*) pass "$name" ;;
        *) fail "$name (missing marker: $marker)" ;;
    esac
}

expect "npda-apf" 0 "aabb -> accepted=true" "$BIN" npda ./examples/APf/APf-1.txt aabb aab
expect "npda-apv" 0 "0110 -> accepted=true" "$BIN" npda ./examples/APv/APv-2.txt 0110 010
expect "npda-trace" 0 "Accepting path found" "$BIN" npda ./examples/APf/APf-1.txt aabb --trace
expect "turing-anbm" 0 "accepted=" "$BIN" turing ./examples/turing/anbm.turing aabb abb
expect "prf-run" 0 "pow(2, 3) = 8" "$BIN" prf 2 3
expect "prf-help" 0 "Trace mode" "$BIN" prf --help

# Corpus: every example file must run to an exit code of 0 or 1
# and must print non-empty output. This catches crashes and hangs.
for f in examples/APf/*.txt examples/APv/*.txt; do
    out=$("$BIN" npda "$f" aabb 2>&1); rc=$?
    if [ "$rc" -gt 1 ] || [ -z "$out" ]; then
        fail "corpus $f (rc=$rc)"
    fi
done
for f in examples/turing/*.turing; do
    out=$("$BIN" turing "$f" aabb 2>&1); rc=$?
    if [ "$rc" -gt 1 ] || [ -z "$out" ]; then
        fail "corpus $f (rc=$rc)"
    fi
done
echo "corpus done"

exit $FAIL
