#!/usr/bin/env python3
"""Copy config/t3/features.css into the css field of T3 Code themes.

T3 Code has no global stylesheet: custom CSS lives in each theme's `css`
field. features.css is the readable source of truth. This writes it into every
theme under config/t3/themes, or only the theme ids given. With --check it
changes nothing and exits 1 when a theme's css has drifted.
"""

import argparse
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
FEATURES = ROOT / "config/t3/features.css"
THEMES = ROOT / "config/t3/themes"


def theme_paths(theme_ids):
    if not theme_ids:
        return sorted(THEMES.glob("*.json"))
    paths = [THEMES / f"{theme_id}.json" for theme_id in theme_ids]
    missing = [path.name for path in paths if not path.is_file()]
    if missing:
        sys.exit(f"no such theme in {THEMES.relative_to(ROOT)}: {', '.join(missing)}")
    return paths


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("theme_ids", nargs="*", help="theme ids (file names without .json); default: all")
    parser.add_argument("--check", action="store_true", help="report drift instead of writing")
    args = parser.parse_args()

    css = FEATURES.read_text(encoding="utf-8")
    drifted = []
    for path in theme_paths(args.theme_ids):
        theme = json.loads(path.read_text(encoding="utf-8"))
        if theme.get("css") == css:
            continue
        if args.check:
            drifted.append(path.name)
            continue
        theme["css"] = css
        # 2-space JSON in the original key order, matching JSON.stringify(theme, null, 2).
        path.write_text(json.dumps(theme, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
        print(f"updated {path.relative_to(ROOT)}")

    if drifted:
        print(
            f"css differs from {FEATURES.relative_to(ROOT)}: {', '.join(drifted)}\n"
            "run: just t3-theme-features",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
