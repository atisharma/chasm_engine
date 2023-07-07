import readline
import logging
import hy
from importlib.metadata import version, PackageNotFoundError

from chasm.repl import run
from chasm.types import Place, Item, Character, Event, Coords
from chasm.state import get_character, set_character, update_character
from chasm.state import get_place, set_place, update_place
from chasm.state import get_item, set_item, update_item


try:
    __version__ = version("chasm")
except PackageNotFoundError:
    # package is not installed
    __version__ = "unknown, not installed via pip"
    pass
