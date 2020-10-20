# Changelog for apecs-gloss-starter

## [2020-02-05] Multi-scene and generic components

### New `playUI` wrapper

A clone of the `apecs-gloss` function. An extra layer is added to

### New `Scene.X.Y` pattern

The "Main" module is split into a series of similarly-structured module trees.
Each contains `SystemW` handlers for their respective tasks.

* `Scene.Example`                - Re-exports of the handler functions for `Main` to import qualified.
* `Scene.Example.Components`     - (Optional) Scene-specific components.
* `Scene.Example.Controls`       - Input event handling with `InputControl` component (see below).
* `Scene.Example.Controls.Types` - Schema enumeration and action containers.
* `Scene.Example.Draw`           - Generate scene- and ui-layer `Picture`s.
* `Scene.Example.Tick`           - Time-based world updates.
* `Scene.Example.Transition`     - New pattern where the Scene components are set up and destroyed.

The starting scenes are:

* `Loading`
* `Intro`
* `Gameplay`
* `Outro`

Use `leaveFor`+`enterFrom` sequence run from e.g. `Controls` and not the transitions themselves, on the pain of import loop.

### Updated starter code

Now contains the staple scene loop and items to extend to your needs.

### New `Delayed` component

Fire up closures later in time.
Run stateful actions in regular intervals.

A staple for animations and action sequences.

### New `InputControls` component

Convert events to actions first.
Bind same actions to key sets.
Attach control schemes to entities.
Filter events by window region.

### New `Fade` components

Simple UI-layer shade transitions.
Pairs nicely with `Delayed` for intermediate steps.

## [2019-10-19] Initial import

### Just the basics

Set up apecs-gloss environment with example components.
One module for the executable and one library for components.

[2020-02-05]: https://gitlab.com/dpwiz/apecs-gloss-starter/tree/at2020.02.05
[2019-10-19]: https://gitlab.com/dpwiz/apecs-gloss-starter/tree/at2019.10.19
