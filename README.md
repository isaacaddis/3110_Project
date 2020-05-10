# Final Project

Singleplayer and multiplayer blackjack in OCaml!

This project was our final project for CS 3110 (Data Structures and Functional Programming) at Cornell.


## Installation

Clone the repository, and follow the instructions in install.txt. The main
dependencies can be installed by running:

```
opam install tls
opam install corebuild
```

### How to Run

To run singleplayer (most features):
```
make play
```
To run multiplayer, launch the server and launch 3 clients. The game requires 3
players to start.

Run server:
```
make server
```

Run a client:
```
make client
```

#### Other commands

`make test` - runs test suite
