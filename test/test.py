# Little "bad" python script to test the compiler.

import argparse
import json
import subprocess
import logging

def test(executable, just_fails = False, skip = False):

    logging.info(f"Executing test with command: \033[1m{executable}\033[0m")
    tests = []

    with open("test/fails.json", "r") as fp:
        tests.extend(json.load(fp))

    if not just_fails:
        with open("test/tests.json", "r") as fp:
            tests.extend(json.load(fp))

    execute_tests(executable, tests, skip)

    return

def execute_tests(executable, tests, skip):
    for t in tests:
        result = subprocess.run(["dune", "exec", f"test/{executable}", "--", f"test/samples/{t}"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if result.returncode == 0:
            logging.debug(f"[✅] Test \033[1m{t}\033[0m \033[0;32msucceded\033[0m!")
        else:
            logging.debug(f"[❌] Test \033[1m{t}\033[0m \033[0;31mfailed\033[0m! Execute `{' '.join(result.args)}` to check the output!")
            if not skip:
                ch = input("\033[1mWould you like to continue?\033[0m [y/n] ")
                if ch != 'y':
                    return
    return

def main():

    phases = {
        0: "parser_test.exe",
        1: "semant_test.exe"
    }

    parser = argparse.ArgumentParser(description="Execute test utility to check failures.")
    parser.add_argument("--fail", help="Execute tests only on fail-*.mc files.", default=False, action=argparse.BooleanOptionalAction)
    parser.add_argument("--phase", help="0) Lexer, 1) Semantic", type=int, nargs=1)
    parser.add_argument("--skip", help="Skip user input on failed tests.", default=False, action=argparse.BooleanOptionalAction)

    args = parser.parse_args()

    try:
        test(phases[args.phase[0]], args.fail, args.skip)
    except KeyboardInterrupt:
        logging.debug("Quitting from the program...")

    return

if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="[\033[1m%(asctime)s\033[0m] :: %(message)s")
    main()