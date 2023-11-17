# Chasm - CHAracter State Manager (game server)

Chasm is a ***generative text adventure game*** in a ***world you
specify***. It uses generative artificial intelligence to generate
scenes and characters as you play. Unlike simply role-playing with a
chatbot, important state mutates and persists (locations, characters,
dialogue etc.)

**This is the server software that clients connect to.**
**It runs the 'world'.**

To play, you need [the client](https://github.com/atisharma/chasm). You can use it with a local model
(Llama derivative like Wizard-Vicuna) or OpenAI's models. See the
config file for examples. I recommend Llama2-based models or ChatGPT.
It 'kind-of-works' with a 7B model, but the results aren't great.
Llama2 70B derivatives like Euryale are the best so far of the models
I've tried.

Chasm is still being written. It's already pretty great though,
with a good model. Think of it as a playable proof-of-concept.


## Features

* [x] specify initial world with a short description
* [x] persistent world / locations
* [x] fuzzy matching names of locations
* [x] continue / save file for game
* [x] persistent items
* [x] character inventory
* [x] per-character event memory
* [x] per-character quests
* [x] take, drop, use items
* [ ] permanently modify items
* [ ] natural item interaction
* [ ] NPCs should interact with items
* [.] NPCs should interact with plot, follow quests
* [x] NPCs should travel
* [x] persistent global event memory (plot events in vector db)
* [x] per-character dialogue memory (snippets in vector db)
* [x] play as any character
* [.] world editor / admin repl for manual world construction
* [x] multiplayer - separate async server with many clients
* [x] player authentication


## Installing and running

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
# then edit server.toml a copy
$ chasm -c server.toml serve
```

You may want to consider using pyenv for complete control over your python
version.


### Configuring a world on the server

Place a text file named after your world in a subdirectory called
"worlds" (see the config file `server.toml` - copy the
`server.toml.example`) and adjust as necessary. The `context_length`
is the context length of the model, which should match what the API
expects. **Check you have enough VRAM and your model works with the
context length if you're running a local model. If you are getting
garbage, it's probably your model.**

The world information (text file) should contain two or three
sentences about the world that won't change (not specific places,
characters or items), such as general location or the period in
history. They are universal and invariant context for your journey.

For example, set `world = "worlds/New York"` in your config file
and create a text file `worlds/New York.txt` with the contents
```
The setting is 1930's New York, and surrounding areas.
There are many buildings specific to the area.
[genre: realist urban fiction]
```
This will create a world called 'New York' and scenes appropriate to it.


## Problems / bugs

There are still many. If you find a bug running a local model, test
with ChatGPT to compare - it might be a bug in
oobabooga/text-generation-webui or whatever's serving your local
model.
