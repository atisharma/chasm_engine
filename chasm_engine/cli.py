import hy
import sys
import asyncio
import argparse

from chasm_engine import stdlib, server, edit

def chasm():
    "Main entry point."
    args_parser = argparse.ArgumentParser(description="The chasm engine.")

    args_parser.add_argument("--config", "-c",
                             type=str,
                             dest="config",
                             default="server.toml",
                             help="specify a different config file (defaults to server.toml)")

    subparser = args_parser.add_subparsers(required=True, help="Run the server, or edit a chasm entity.")

    p_serve = subparser.add_parser("serve", help="Run the server")
    p_serve.set_defaults(func=_serve)

    p_account = subparser.add_parser("account", help="Edit an account")
    p_account.add_argument("name", help="name of account")
    p_account.set_defaults(func=_edit_account)

    p_character = subparser.add_parser("character", help="Edit a character")
    p_character.add_argument("name", help="name of character")
    p_character.set_defaults(func=_edit_character)

    p_item = subparser.add_parser("item", help="Edit an item")
    p_item.add_argument("name", help="name of item")
    p_item.set_defaults(func=_edit_item)

    p_place = subparser.add_parser("place", help="Edit a place")
    p_place.add_argument("x", type=int, help="Eastings (x-location)")
    p_place.add_argument("y", type=int, help="Northings (y-location)")
    p_place.set_defaults(func=_edit_place)

    args = args_parser.parse_args()
    args.func(args)


def _serve(args):
    sys.exit(asyncio.run(server.serve()) or 0)

def _edit_account(args):
    edit.edit_account(args.name)

def _edit_character(args):
    edit.edit_character(args.name)

def _edit_item(args):
    edit.edit_item(args.name)

def _edit_place(args):
    edit.edit_place(args.x, args.y)
