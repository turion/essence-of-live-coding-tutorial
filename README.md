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
4. Sanity check: Launch `cabal repl` and close it again.
   This should succeed without errors.
5. Open `Main.hs` in an editor.
6. Run `ghcid` from the console.

You should now be seeing a window containing a solid circle (a ball).
If you click anywhere in the window, the ball will start to move in that direction.
Once you make changes to `Main.hs`,
it will automatically reload.

### External dependencies

* Ideally, a Linux system.
  (See [below](#non-linux-systems).)
* A standard Haskell development environment, including `cabal` and `ghci`.
  (`stack` is not needed.)
* [`ghcid`](https://github.com/ndmitchell/ghcid).
* OpenGL development libraries and PulseAudio development libraries.
  (For other sound setups, see [below](#sound-support).)

### Non-Linux systems

#### Windows

I cannot give Windows support for graphics,
since I don't have a Windows machine.
If you have a Windows machine,
you'd like to get graphics to run on your machine,
and you're willing to test a backend and setup with me,
please contact me via a Github issue.

#### macOS

Graphics/OpenGL support on macOS seems to be broken,
see https://github.com/turion/essence-of-live-coding-tutorial/issues/3.
If you know how to fix such an issue, please comment,
and we'll resolve it so you can use graphics on macOS.

### Sound support

Currently, I only have audio support ready for Linux, PulseAudio, since this is the platform on which I develop.
If you have a different system, we will still be able to get sound working if you know of good Haskell bindings to your sound system.
If that is the case, please open an issue on https://github.com/turion/essence-of-live-coding/issues so we can prepare a sound backend before the tutorial.

Either way, the tutorial will focus mainly on video, and only add further backends as time permits.

## Helpful resources during the tutorial

* Basics of composable vector graphics in Gloss: http://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss-Data-Picture.html
* LiveCoding reference documentation:
  * https://hackage.haskell.org/package/essence-of-live-coding
  * https://hackage.haskell.org/package/essence-of-live-coding-gloss
  * https://hackage.haskell.org/package/essence-of-live-coding-pulse
  * https://hackage.haskell.org/package/essence-of-live-coding-warp
* Vector space operations: http://hackage.haskell.org/package/vector-space-0.16/docs/Data-VectorSpace.html
* Web Application Interface reference documentation:
  * https://hackage.haskell.org/package/wai-3.2.2.1/docs/Network-Wai.html

## Tasks during the tutorial

| Task                                     | Branch                                                                                                            |
| ---------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| Magnet (Ball stops when it is very slow) | [progress1_magnet](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress1_magnet)               |
| Goal hole with high friction             | [progress2_goal](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress2_goal)                   |
| Obstacle type                            | [progress3_obstacle_type](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress3_obstacle_type) |
| Draw obstacles, simulate repulsion       | [progress4_obstacles](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress4_obstacles)         |
| Connect warp backend, print last query   | [progress5_connect_warp](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress5_connect_warp)   |
| Parse warp impulse query                 | [progress6_warp_impulse](https://github.com/turion/essence-of-live-coding-tutorial/tree/progress6_warp_impulse)   |
| Stretch goal: Add sound                  | [solution_pulse](https://github.com/turion/essence-of-live-coding-tutorial/tree/solution_pulse)                   |
