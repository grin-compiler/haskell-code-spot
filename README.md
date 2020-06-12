# Haskell Code Spot

Visual tool to spot odd runtime behaviour of Haskell programs.
**Keywords:** *data scientist, GHC eventlog, UI experiments, creative/live coding*

### Learn these for coding

- HTML + CSS + JS
- Svelte *(https://svelte.dev/tutorial/basics)*
- D3.js *(https://www.youtube.com/watch?v=_8V5o2UHG0E)*
- CodeMirror *(https://codemirror.net/doc/manual.html#api)*
- Scotty *(https://hackage.haskell.org/package/scotty)*
- GHC RTS design *(https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)*
- GHC Eventlog *(https://www.well-typed.com/blog/2019/09/eventful-ghc/)*
- ghc-events library *(https://hackage.haskell.org/package/ghc-events)*
- GHC HIE files *(https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)*

### Quick Start Guide

In one terminal:

    $ cd server && stack build && stack exec code-spot-server

In another terminal:

    $ cd src && npm install && npm run dev

Now open [http://localhost:5000/](http://localhost:5000/) in your browser!

You will see a visualisation of [data/grin.eventlog](data/grin.eventlog)
(change in [client/src/App.svelte](client/src/App.svelte)).

### How to build your eventlog

If stack project is used, profiling must be enabled.
After compiled with profiling, one need to run it with RTS options plh.

```
stack build --profile --executable-profiling --library-profiling
stack exec --profile EXECUTABLE -- ARGUMENTS +RTS -p -l -h
```
