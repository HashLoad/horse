#!/usr/bin/env python3
"""Run Horse's portable callback regressions with isolated compiler output."""
import argparse
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def run_suite(compiler, compiler_args, source, source_root, mode):
    with tempfile.TemporaryDirectory(prefix="horse-callbacks-") as output:
        command = [compiler, "-B", "-Mdelphi", "-Sh", "-gh", "-gl"]
        command += compiler_args
        if source.stem == "HeaderHashCheck":
            command += ["-Cr", "-Co"]
        command += ["-Fu" + str(source_root), "-FU" + output, "-FE" + output]
        if mode == "funcref":
            command += ["-dHORSE_FPC_FUNCTIONREFERENCES"]
        command += [str(source)]
        result = subprocess.run(command, capture_output=True, text=True, timeout=120)
        if result.returncode:
            print(" ".join(command))
            print(result.stdout + result.stderr)
            return False
        binary = Path(output) / (source.stem + (".exe" if os.name == "nt" else ""))
        result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=30)
        text = result.stdout + result.stderr
        print(text.rstrip())
        return result.returncode == 0 and not re.search(r"[1-9][0-9]* unfreed memory blocks", text)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", default=os.environ.get("FPC", "fpc"))
    parser.add_argument("--compiler-arg", action="append", default=[])
    parser.add_argument("--mode", choices=["legacy", "funcref", "all"], default="all")
    parser.add_argument("--source-root", type=Path)
    args = parser.parse_args()
    tests = Path(__file__).resolve().parent.parent
    sources = args.source_root or tests.parent / "src"
    suites = [
        tests / "fpc-functionreferences/FunctionReferencesCheck.dpr",
        tests / "fpc-exception-propagation/ExceptionPropagationCheck.dpr",
        tests / "fpc-header-hash/HeaderHashCheck.dpr",
        tests / "fpc-url-decoding/UrlDecodingCheck.dpr",
        tests / "fpc-router-lifetime/RouterLifetimeCheck.dpr",
    ]
    modes = ["legacy", "funcref"] if args.mode == "all" else [args.mode]
    for mode in modes:
        selected = suites[:2] if mode == "funcref" else suites
        for source in selected:
            print(f"=== {mode}: {source.stem} ===", flush=True)
            if not run_suite(args.compiler, args.compiler_arg, source, sources.resolve(), mode):
                return 1
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, subprocess.TimeoutExpired) as error:
        print(error, file=sys.stderr)
        sys.exit(1)
