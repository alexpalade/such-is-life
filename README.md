# Such Is Life

Save people from the virus and the killers. You decide where to place the hospital and the quarantine. [Screenshot](screenshot.png).

A game written in Common Lisp for the [Spring Lisp Game Jam 2020](https://itch.io/jam/spring-lisp-game-jam-2020).

## How to run

Run one of the binary builds from the [latest release](https://github.com/alexpalade/such-is-life/releases/latest). Check the [requirements](#requirements) below.

### Alternative: using the source code
Clone or link this repository into ~/quicklisp/local-projects/.

``` common-lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.testing.txt")
(ql:update-all-dists)

(ql:quickload :such-is-life)
(sil-game:run)
```

## How to play

### Controls
1. **Click and drag** to define the quarantine area.
2. **Right-click** to place the hospital.

### Purpose
Try to save as many people, or just try out various settings.

* *Medics* bring people to the hospital to cure them.
* *Police officers* lock up the undisguised *killers*.
* *The virus* infects everyone and has no antidote.

Hints:
* Move the hospital when the area gets too crowded.
* Place the hospital near the sick and away from the killers.
* The killers lose the disguise when they leave the quarantine.

## Requirements
1. OpenGL 2.1
2. x86_64 Windows, GNU/Linux or macOS
3. x86_64 SBCL or CCL

## Credits

The game is written in Common Lisp for the [Spring Lisp Game Jam 2020](https://itch.io/jam/spring-lisp-game-jam-2020). It uses [trivial-gamekit](https://borodust.org/projects/trivial-gamekit/) framework by Pavel Korolev (borodust). Thanks for the help from the #lispgames IRC channel.

The character sprites are emoji characters from [Noto Color Emoji](https://github.com/googlefonts/noto-emoji). Noto is a font family commissioned by Google, licensed under the SIL Open Font License. I modified some slightly with Gimp.

### This game uses these sounds from freesound:
* Winning by [rhodesmas](https://freesound.org/people/rhodesmas/sounds/320775/)
* Healing by [PaulMorek](https://freesound.org/people/PaulMorek/sounds/330047/)
* Prison lock by [JavierZumer](https://freesound.org/people/JavierZumer/sounds/257233/)
* Death by [InspectorJ](https://freesound.org/people/InspectorJ/sounds/484268/)
* Kill by [smokebomb99](https://freesound.org/people/smokebomb99/sounds/147290/)
* Cough by [OwlStorm](https://freesound.org/people/OwlStorm/sounds/151217/)
