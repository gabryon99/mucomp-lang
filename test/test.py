# Little "bad" python script to test the compiler.

import argparse
import json
import subprocess
import logging

def test(executable, just_fails = False):

    with open("test/fails.json", "r") as fp:
        tests = json.load(fp)
        execute_tests(executable, tests)

    if not just_fails:
        with open("test/tests.json", "r") as fp:
            tests = json.load(fp)
            execute_tests(executable, tests)
    return

def execute_tests(executable, tests):
    for t in tests:
        result = subprocess.run(["dune", "exec", f"test/{executable}", "--", f"test/samples/{t}"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if result.returncode == 0:
            logging.debug(f"[✅] Test `{t} succeded!`")
        else:
            logging.debug(f"[❌] Test `{t} failed! Execute `{' '.join(result.args)}` to check the output!")
    return

def main():

    phases = {
        0: "parser_test.exe",
        1: "semant_test.exe"
    }

    parser = argparse.ArgumentParser(description="Execute test utility to check failures.")
    parser.add_argument("--fail", help="Execute tests only on fail-*.mc files.", default=False, action=argparse.BooleanOptionalAction)
    parser.add_argument("--phase", help="0) Lexer, 1) Semantic", type=int, nargs=1)

    args = parser.parse_args()
    test(phases[args.phase[0]], args.fail)

    return

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="")
    main()