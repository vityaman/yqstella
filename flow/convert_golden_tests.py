#!/usr/bin/env python3
"""
Convert golden test files to yqstella test case format.

Input structure:
  <root>/
    <suite>/
      ok/
        test_ok_foo.golden
      error/
        test_err_bar.golden

Output structure:
  <output>/
    <suite>-malevrovich/
      ok-test-ok-foo/
        input.yqst
        yql.yqls
        parameters.yson
        result.yson
        diagnostics.txt
      error-test-err-bar/
        input.yqst
        ...
"""

import argparse
import sys
from pathlib import Path
from typing import Optional, Tuple


EMPTY_FILES = ["yql.yqls", "parameters.yson", "result.yson", "diagnostics.txt"]


def parse_input_section(content: str) -> str:
    in_input = False
    lines = []
    for line in content.split("\n"):
        if line.startswith("INPUT:"):
            in_input = True
        elif line.startswith(("STDOUT:", "STDERR:", "EXIT_CODE:")):
            in_input = False
        elif in_input:
            lines.append(line)
    return "\n".join(lines).strip()


def derive_placement(golden_path: Path, root: Path) -> Optional[Tuple[str, str]]:
    """Return (suite_name, test_name) or None if ok/error dir not found."""
    parts = golden_path.relative_to(root).parts
    # parts: (<suite>, ..., ok|error, <stem>.golden)
    category = None
    suite = parts[0] if len(parts) > 1 else None
    for part in parts[:-1]:
        if part in ("ok", "error"):
            category = part
            break
    if category is None or suite is None:
        return None

    stem = golden_path.stem.replace("_", "-")
    return suite, f"{category}-{stem}"


def convert(input_dir: Path, output_dir: Path) -> int:
    golden_files = sorted(input_dir.rglob("*.golden"))
    if not golden_files:
        print(f"No .golden files found in {input_dir}")
        return 1

    output_dir.mkdir(parents=True, exist_ok=True)
    converted = 0
    skipped = 0

    for golden_path in golden_files:
        placement = derive_placement(golden_path, input_dir)
        if placement is None:
            print(f"SKIP {golden_path.relative_to(input_dir)}: no ok/error parent directory")
            skipped += 1
            continue

        suite, test_name = placement
        suite_dir = output_dir / f"{suite}-malevrovich"

        content = golden_path.read_text(encoding="utf-8")
        source = parse_input_section(content)

        test_dir = suite_dir / test_name
        if test_dir.exists():
            print(f"SKIP {golden_path.relative_to(input_dir)}: {suite}-malevrovich/{test_name}/ already exists")
            skipped += 1
            continue

        test_dir.mkdir(parents=True, exist_ok=True)

        (test_dir / "input.yqst").write_text(source + "\n" if source else "", encoding="utf-8")
        for name in EMPTY_FILES:
            (test_dir / name).write_text("", encoding="utf-8")

        rel = golden_path.relative_to(input_dir)
        print(f"OK  {rel} -> {suite}-malevrovich/{test_name}/")
        converted += 1

    print(f"\n{converted} converted, {skipped} skipped")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Convert golden tests to yqstella format")
    parser.add_argument("input_dir", help="Root directory containing .golden files")
    parser.add_argument("output_dir", help="Directory to write test cases into")
    args = parser.parse_args()

    input_dir = Path(args.input_dir).resolve()
    output_dir = Path(args.output_dir).resolve()

    if not input_dir.is_dir():
        print(f"Error: {input_dir} is not a directory")
        return 1

    return convert(input_dir, output_dir)


if __name__ == "__main__":
    sys.exit(main())
