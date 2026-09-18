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
expect "explain" 0 "Name a state from the Q line" "$BIN" explain E0007
expect "explain-unknown" 1 "unknown error code" "$BIN" explain BOGUS

# -in/-out/keyboard input paths (npda)
printf 'aabb\naab\n' > /tmp/npda_proof_in.txt
out=$("$BIN" npda ./examples/APf/APf-1.txt --in /tmp/npda_proof_in.txt 2>&1); rc=$?
if [ "$rc" != "0" ]; then fail "npda-in (rc=$rc)"; else
    case "$out" in *"1 accepted, 1 rejected"*) pass "npda-in" ;; *) fail "npda-in (bad summary)" ;; esac
fi
out=$(printf 'aabb\n' | "$BIN" npda ./examples/APf/APf-1.txt 2>&1); rc=$?
if [ "$rc" != "0" ]; then fail "npda-stdin (rc=$rc)"; else
    case "$out" in *"1 accepted, 0 rejected"*) pass "npda-stdin" ;; *) fail "npda-stdin (bad summary)" ;; esac
fi
"$BIN" npda ./examples/APf/APf-1.txt --in /tmp/npda_proof_in.txt --trace --out /tmp/npda_proof_trace.txt >/dev/null 2>&1; rc=$?
if [ "$rc" != "0" ] || [ ! -s /tmp/npda_proof_trace.txt ]; then
    fail "npda-out (rc=$rc)"
else
    case "$(cat /tmp/npda_proof_trace.txt)" in *"=== Exploration Step 0 ==="*) pass "npda-out" ;; *) fail "npda-out (empty trace)" ;; esac
fi

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
