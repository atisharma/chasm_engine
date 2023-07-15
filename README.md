# Chasm - CHAracter State Manager (game server)

Chasm is a ***generative text adventure game*** in a ***world you
specify***. It uses generative artificial intelligence to generate
scenes and characters as you play. Unlike simply role-playing with a
chatbot, important state persists (locations, characters, dialogue
etc.)

**This is the server software that clients connect to.**
**It runs the 'world'.**

You can use it with a local model (LLaMA derivative like
Wizard-Vicuna) or OpenAI's models. See the config file for examples.
It works very well with a 30B LLaMA model and acceptably with a 13B
one. It 'kind-of-works' with a 7B LLaMA model, but the results aren't
great.

Chasm is still being written. It's already pretty great though,
with a good model.


## Features

* [x] specify initial world with a short description
* [x] persistent world / locations
* [x] fuzzy matching names of locations
* [x] continue / save file for game
* [x] persistent items
* [x] character inventory
* [x] take, drop, use items
* [ ] permanently modify items
* [x] per-character event memory
* [x] per-character quests
* [x] NPCs should travel
* [ ] NPCs should interact with items
* [ ] NPCs should interact with plot, follow quests
* [x] persistent global event memory (plot events in vector db)
* [x] per-character dialogue memory (snippets in vector db)
* [x] play as any character
* [ ] command-line parameters to override config
* [ ] world editor for manual world construction
* [x] multiplayer - separate out server & client


## Installing and running

### Configuring a world on the server

Place a text file named after your world in a subdirectory called
"worlds" (see the config file `server.toml`).

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

There are still many.
