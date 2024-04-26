#!/bin/sh

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

# ---------------------------------------------------------------------------

f_out="unittest-results.txt"
f_good="unittest-results.txt-good"

do_check() {
    cd "$_base"
    echo ">> Checking unittest examples (saved in $f_out)"
    ./run_unittest_tests.pl > "$f_out" 2>&1 
}

do_compare() {
    cd "$_base"
    if diff "$f_out" "$f_good"; then
        echo "[OK] No differences detected in unittest examples"
        exit 0
    else
        echo "[??] Differences detected in unittest examples!"
        echo "Please check manually. If correct, validate with 'testsuite/unittest/runme.sh save'"
        exit 1
    fi
}

do_save() {
    cd "$_base"
    cp "$f_out" "$f_good"
}

do_help() {
    cat <<EOF
See the script source.
EOF
}

ACTION=$1

case ${ACTION} in
    check) do_check ;;
    compare) do_compare ;;
    save) do_save ;;
    *) do_help ;;
esac
