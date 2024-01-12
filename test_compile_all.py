from pathlib import Path
from subprocess import run
import sys


def main(r: Path):
    for f in (r / "example").iterdir():
        res = run(["mill", "cli", str(f)])
        if res.returncode != 0:
            print(f"failed for file {f}", file=sys.stderr)
            exit(1)


if __name__ == "__main__":
    main(Path("."))
