# Such Is Life

Save people from the virus and the killers. You decide where to place the hospital and the quarantine.
A game written in Common Lisp for the [Spring Lisp Game Jam 2020](https://itch.io/jam/spring-lisp-game-jam-2020).

![Screenshot](screenshot.png | width=400)

## How to run

Check the [requirements](#requirements) below.
Run one of the binary builds from the [latest release](https://github.com/alexpalade/such-is-life/releases/latest).

### Alternative: using the source code
Clone or link this repository into ~/quicklisp/local-projects/.

``` common-lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.testing.txt")
(ql:update-all-dists)

(ql:quickload :such-is-life)
(decent-game:run)
```

## How to play

### Controls
1. **Click and drag** to define the quarantine area.
2. **Right-click** to place the hospital.

### Purpose
Try to save as many people, or just try out various settings.

* Medics bring people to the hospital to cure them.
* Police officers lock up the undisguised killers.
* The killers attack regular people.
* The virus has no antidote.

Hints:
* The killers lose the disguise after leaving the quarantine.
* Move the hospital when the area gets too crowded.
* Place the hospital near the sick and away from the killers.

## Requirements
1. OpenGL 2.1
2. x86_64 Windows, GNU/Linux or macOS
3. x86_64 SBCL or CCL


## Credits

The game is written in Common Lisp for the [Spring Lisp Game Jam 2020](https://itch.io/jam/spring-lisp-game-jam-2020). It uses [trivial-gamekit](https://borodust.org/projects/trivial-gamekit/) framework by Pavel Korolev (borodust). Thanks for the help of the #lispgames IRC channel.

### This game uses these sounds from freesound:
* Winning by [rhodesmas](https://freesound.org/people/rhodesmas/sounds/320775/)
* Healing by [PaulMorek](https://freesound.org/people/PaulMorek/sounds/330047/)
* Prison lock by [JavierZumer](https://freesound.org/people/JavierZumer/sounds/257233/)
* Death by [InspectorJ](https://freesound.org/people/InspectorJ/sounds/484268/)
* Kill by [smokebomb99](https://freesound.org/people/smokebomb99/sounds/147290/)
* Cough by [OwlStorm](https://freesound.org/people/OwlStorm/sounds/151217/)
