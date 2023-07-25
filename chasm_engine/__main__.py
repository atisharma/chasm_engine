import hy
import sys
import asyncio
import chasm_engine

if __name__ == "__main__":
 sys.exit(asyncio.run(chasm_engine.serve()) or 0)
