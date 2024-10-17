# Chasm - CHAracter State Manager (game server)

Chasm is a ***generative text adventure game / interactive fiction*** in a
***world you specify***. It uses generative artificial intelligence to generate
scenes and characters as you play. Unlike simply role-playing with a chatbot,
important state mutates and persists (locations, characters, dialogue etc.)

**This is the server software that clients connect to over the network.**
**It runs the 'world'.**
To play, you need [the client](https://github.com/atisharma/chasm).

Chasm is still very much in an experimental state. It's already pretty great
though, with a good model. Think of it as a playable proof-of-concept.


## Features

* [x] specify initial world with a short description
* [x] persistent world / locations
* [ ] persistent topology
* [x] fuzzy matching names of locations
* [x] narrative persistence
* [x] persistent items
* [x] character inventory
* [x] per-character event memory
* [x] per-character quests
* [x] take, drop, use items
* [ ] streaming narrative
* [ ] streaming markdown markup in client
* [x] markdown markup in client (non-streaming)
* [ ] permanently modify items
* [ ] natural item interaction
* [ ] natural item spawning from narrative
* [ ] NPCs should interact with items
* [ ] NPCs should interact with plot, follow quests
* [x] NPCs should travel
* [x] persistent global event memory (plot events in vector db)
* [x] per-character dialogue memory (snippets in vector db)
* [x] play as any character
* [x] world editor / admin repl for manual world editing
* [x] multiplayer - separate async server with many clients
* [x] player authentication


## Installing and running

### Model recommendations

You can use the server with an open model (like Llama or Mistral) served
locally (e.g. via tabbyAPI or a remote endpoint like deepinfra) or a closed one
like Anthropic's or OpenAI's models. See the config file for examples. For the
narrator, I recommend a strong model like Mistral Large, a 70B Llama3-based
model, GPT-4o-(mini) or Claude. For the backend, you can get away with a
smaller model if it's strong at instruction following.

### Installing

There are a lot of dependencies so it's recommended you install everything in a
virtual environment. You can use the same environment for client and server if
you want (for example, with a single player, it might make sense).

The chasm engine depends on sentence-transformers, which in turn
depends on pytorch. If you want to use only the CPU version of
pytorch (instead of the *huge* cuda libraries which are not needed for
this), you should [install that first](https://pytorch.org/get-started/locally/).

```bash
$ <activate your venv>
# if installing only the CPU version of pytorch...
$ pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu
# to install the server
$ pip install -U git+https://github.com/atisharma/chasm_engine
# then edit server.toml or a copy
$ chasm -c server.toml serve
```

You may want to consider using pyenv for complete control over your python
version.


### Configuring a world on the server

Place a text file named after your world in a subdirectory called "worlds" (see
the config file `server.toml` - copy the `server.toml.example`) and adjust as
necessary. The `context_length` is the context length of the model, which
should match what the API expects. **Check you have enough VRAM and your model
works with the context length if you're running a local model. If you are
getting garbage, it's probably your model.**

The world information (text file) should contain two or three sentences about
the world that won't change (not specific places, characters or items), such as
general location or the period in history. They are universal and invariant
context for your journey.

For example, set `world = "worlds/New York"` in your config file
and create a text file `worlds/New York.txt` with the contents
```
The setting is 1930's New York, and surrounding areas.
There are many buildings specific to the area.
[genre: realist urban fiction]
```
This will create a world called 'New York' and scenes appropriate to it.


## Problems / bugs

There are still many. If you find a bug running a local model, test with one of
the public APIs to compare - it might be a bug in whatever's serving your local
model.
