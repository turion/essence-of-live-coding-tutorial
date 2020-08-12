# Tutorial for `essence-of-live-coding`

## About the library

essence-of-live-coding is a general purpose and type safe live coding framework in Haskell.
You can run programs in it, and edit, recompile and reload them _while_ they're running.
Internally, the state of the live program is automatically migrated when performing hot code swap.

The library also offers an easy to use FRP interface.
It is parametrized by its side effects,
separates data flow cleanly from control flow,
and allows to develop live programs from reusable, modular components.
There are also useful utilities for debugging and quickchecking.

### Learn more here

* https://github.com/turion/essence-of-live-coding
* https://www.manuelbaerenz.de/#computerscience
* https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf

## Installation

1. Clone this repository and enter it:
   ```
   git clone https://github.com/turion/essence-of-live-coding-tutorial
   cd essence-of-live-coding-tutorial
   ```
2. Either install [`nix`](https://nixos.org/download.html) and launch a `nix-shell`,
   or install the [external dependencies listed below](#external-dependencies)
3. `cabal update`
   (A new version of the library was released recently)
4. Sanity check: Launch `cabal repl`.
   This should succeed without errors.
5. Open `app/Main.hs` in an editor.
6. Run `ghcid` from the console.

You should now be seeing a window containing a solid circle (a ball).
If you click anywhere in the window, the ball will start to move in that direction.
Once you make changes to `Main.hs`,
it will automatically reload.

### External dependencies

* A standard Haskell development environment, including `cabal` and `ghci`.
  (`stack` is not needed.)
* [`ghcid`](https://github.com/ndmitchell/ghcid).
* OpenGL development libraries and PulseAudio development libraries.
  (For other sound setups, see [below](#sound-support).)

### Sound support

Currently, I only have audio support ready for Linux, PulseAudio, since this is the platform on which I develop.
If you have a different system, we will still be able to get sound working if you know of good Haskell bindings to your sound system.
If that is the case, please open an issue on https://github.com/turion/essence-of-live-coding/issues so we can prepare a sound backend before the tutorial.

Either way, the tutorial will focus mainly on video, and only add further backends as time permits.
