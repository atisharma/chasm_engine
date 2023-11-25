import hy
import sys
import asyncio
import argparse


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

    p_character = subparser.add_parser("narrative", help="Dump a character's narrative")
    p_character.add_argument("name", help="name of character")
    p_character.set_defaults(func=_dump_narrative)

    p_place = subparser.add_parser("look", help="Look at a place and its objects")
    p_place.add_argument("x", type=int, help="Eastings (x-location)")
    p_place.add_argument("y", type=int, help="Northings (y-location)")
    p_place.set_defaults(func=_look_place)

    p_place = subparser.add_parser("config", help="dump parsed config param")
    p_character.add_argument("param", help="parameter")
    p_place.set_defaults(func=_dump_config)

    args = args_parser.parse_args()
    args.func(args)


def _serve(args):
    from chasm_engine import server
    sys.exit(asyncio.run(server.serve()) or 0)

def _edit_account(args):
    from chasm_engine import edit
    edit.edit_account(args.name)

def _edit_character(args):
    from chasm_engine import edit
    edit.edit_character(args.name)

def _edit_item(args):
    from chasm_engine import edit
    edit.edit_item(args.name)

def _edit_place(args):
    from chasm_engine import edit
    edit.edit_place(args.x, args.y)

def _dump_narrative(args):
    from chasm_engine import state, stdlib
    print(
        stdlib.format_msgs(
            state.get_narrative(args.name)
        )
    )

def _look_place(args):
    from chasm_engine.types import Coords
    from chasm_engine import character, item, place
    print(
        "\n\n".join(
            [
                place.name(Coords(args.x, args.y)),
                character.describe_at(Coords(args.x, args.y)),
                item.describe_at(Coords(args.x, args.y))
            ]
        )
    )

def _dump_config(args):
    from chasm_engine import stdlib
    print(stdlib.config(args.param))
