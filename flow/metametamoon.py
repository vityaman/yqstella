#!/usr/bin/env python3
"""Convert metametamoon's Stella tests to yqstella golden tests.

Input structure:
  <root>/
    generics/
      <category>/
        <test>.stella
    no-generics/
      <category>/
        <test>.stella

Output structure:
  <output>/
    <suite>-metametamoon/
      <category>-<test>/
        input.yqst
        diagnostics.txt
"""

import argparse
import sys
from pathlib import Path


SUITES = ("generics", "no-generics")


def test_name(source_path: Path, suite_path: Path) -> str:
    relative_path = source_path.relative_to(suite_path).with_suffix("")
    return "-".join(part.replace("_", "-") for part in relative_path.parts)


def convert(input_dir: Path, output_dir: Path) -> int:
    sources = [
        source_path
        for suite in SUITES
        for source_path in sorted((input_dir / suite).rglob("*.stella"))
    ]
    if not sources:
        print(f"No .stella files found in {input_dir}")
        return 1

    converted = 0
    skipped = 0

    for source_path in sources:
        suite = source_path.relative_to(input_dir).parts[0]
        suite_path = input_dir / suite
        case_name = test_name(source_path, suite_path)
        case_path = output_dir / f"{suite}-metametamoon" / case_name

        if case_path.exists():
            relative_path = source_path.relative_to(input_dir)
            print(f"SKIP {relative_path}: {case_path.relative_to(output_dir)}/ already exists")
            skipped += 1
            continue

        case_path.mkdir(parents=True)
        source = source_path.read_text(encoding="utf-8")
        (case_path / "input.yqst").write_text(source, encoding="utf-8")
        (case_path / "diagnostics.txt").write_text("", encoding="utf-8")

        relative_path = source_path.relative_to(input_dir)
        print(f"OK  {relative_path} -> {case_path.relative_to(output_dir)}/")
        converted += 1

    print(f"\n{converted} converted, {skipped} skipped")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Convert metametamoon's Stella tests to yqstella golden tests"
    )
    parser.add_argument(
        "input_dir",
        nargs="?",
        default="stella-typechecker/tests",
        help="directory containing generics and no-generics (default: %(default)s)",
    )
    parser.add_argument(
        "output_dir",
        nargs="?",
        default="test/golden",
        help="directory to write test cases into (default: %(default)s)",
    )
    args = parser.parse_args()

    input_dir = Path(args.input_dir).resolve()
    output_dir = Path(args.output_dir).resolve()

    missing_suites = [suite for suite in SUITES if not (input_dir / suite).is_dir()]
    if missing_suites:
        print(f"Missing suites in {input_dir}: {', '.join(missing_suites)}")
        return 1

    return convert(input_dir, output_dir)


if __name__ == "__main__":
    sys.exit(main())
