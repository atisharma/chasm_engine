import hy
import sys
import asyncio
import chasm

if __name__ == "__main__":
 sys.exit(asyncio.run(chasm.serve()) or 0)
