import readline
import logging
import hy
from importlib.metadata import version, PackageNotFoundError

from chasm.server import serve


try:
    __version__ = version("chasm")
except PackageNotFoundError:
    # package is not installed
    __version__ = "unknown, not installed via pip"
    pass
