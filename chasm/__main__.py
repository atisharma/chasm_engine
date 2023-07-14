import hy
import sys

from chasm import repl

if __name__ == "__main__":
 sys.exit(repl.run() or 0)
