;;;
;;; config/properties.lisp
;;;


;; Workshop required to build
(property-configure prop-workshop-required
                    '(forcefield radiator mycelium ion-cannon flak-gun annihilator
                      manufactory power-core solar-cell backup-core overdrive-core
                      radar transporter replicator drone-bay))

;; Not shown in construction menu
(property-configure prop-not-constructible
                    '(overdrive-core balloon ladder+ stairwell+ stairwell++ cargo-bay
                      crane water ice escape-beacon code basalt snow market-stall))

;; Manufactory required to build
(property-configure prop-manufactory-required
                    '(forcefield* energized-hull ion-fizzler cloak mirror-hull
                      stacked-hull arc-gun nemesis fire-charge sylph-cannon decimator
                      spark-cannon rocket-bomb reactor chaos-core portal dynamite-ii
                      deflector))

;; Not shown in construction menu during tutorials
(property-configure prop-tutorial-disabled
                    '(bronze-hull radiator cloak mirror-hull stacked-hull mycelium
                      annihilator spark-cannon rocket-bomb warhead backup-core windmill
                      ladder ladder+ stairwell+ stairwell++ bridge portal crane
                      weather-engine water water-source dynamite dynamite-ii
                      targeting-computer escape-beacon phase-shifter visualizer))

;; Locked until completing an achievement
(property-configure prop-locked-by-default
                    '(bronze-hull radiator mirror-hull mycelium annihilator spark-cannon
                      windmill bridge weather-engine dynamite dynamite-ii bell
                      tuning-crystal speaker synth visualizer statue lady-liberty
                      fountain coconut-palm lemon-tree banana-plant masonry market-stall))

;; Do not render a roof tile above this block
(property-configure prop-roof-hidden
                    '(hull bronze-hull forcefield forcefield* energized-hull ion-fizzler
                      radiator cloak mirror-hull stacked-hull mycelium barrier cannon
                      ion-cannon flak-gun arc-gun nemesis fire-charge sylph-cannon
                      decimator annihilator spark-cannon incinerator beam-gun particle-lance
                      ballista missile-silo rocket-bomb splitter warhead solar-cell
                      windmill balloon crane weather-engine water water-source ice
                      dynamite dynamite-ii radar targeting-computer escape-beacon
                      drone-bay deflector amplifier phase-shifter bell tuning-crystal
                      speaker synth visualizer statue lady-liberty fountain torch
                      coconut-palm lemon-tree sunflower shrubbery banana-plant masonry
                      code basalt snow market-stall canvas))

;; The island's chimney may originate from these blocks
(property-configure prop-has-chimney
                    '(power-core reactor backup-core war-engine chaos-core overdrive-core))

;; Do not render a chimney above these rooms, because it looks strange.
(property-configure prop-chimney-hidden
                    '(forcefield forcefield* ion-fizzler cloak cannon fire-charge sylph-cannon
                      spark-cannon incinerator beam-gun particle-lance ballista missile-silo
                      rocket-bomb splitter warhead solar-cell windmill balloon bridge
                      weather-engine water water-source ice radar bell tuning-crystal
                      speaker synth visualizer statue lady-liberty fountain coconut-palm
                      lemon-tree sunflower shrubbery banana-plant code basalt market-stall
                      canvas))

(property-configure prop-takes-ion-damage
                    '(forcefield forcefield* energized-hull ion-fizzler cloak reactor
                      targeting-computer deflector amplifier phase-shifter))

(property-configure prop-cancels-ion-damage
                    '(ion-fizzler))

;; The engine is allowed to render a flagpole on top of these blocks
(property-configure prop-flag-mount
                    '(hull bronze-hull radiator mirror-hull stacked-hull mycelium barrier
                      crane weather-engine dynamite dynamite-ii targeting-computer
                      deflector amplifier torch))

;; If hit by a weapon where damage exceeds the block's health, the colliding
;; projectile will not be destroyed.
(property-configure prop-fragile
                    '(windmill balloon bridge water water-source ice bell tuning-crystal
                      speaker synth visualizer statue lady-liberty fountain torch
                      coconut-palm lemon-tree sunflower shrubbery banana-plant masonry
                      code basalt snow market-stall canvas))

;; Only available in adventure mode.
(property-configure prop-adventure-only
                    '(crane))

;; These blocks are not normally constructible, although you can build them in
;; the sandbox.
(property-configure prop-sandbox-only
                    '(barrier incinerator beam-gun particle-lance ballista splitter
                      warhead war-engine plundered-room torch canvas))

(property-configure prop-fluid
                    '(water water-source))

;; These blocks have special sound effects when destroyed.
(property-configure prop-destroy-special
                    '(forcefield forcefield* power-core reactor war-engine water water-source))

;; Cannot be salvaged.
(property-configure prop-salvage-disabled
                    '(mycelium dynamite dynamite-ii))

;; Unsupported in multiplayer. Either because link synchronization is not
;; implemented, or because the blocks start fires, which is too chaotic to sync
;; across linked games.
(property-configure prop-multiplayer-disabled
                    '(fire-charge spark-cannon incinerator rocket-bomb splitter windmill balloon
                      bridge crane weather-engine ice escape-beacon deflector phase-shifter bell
                      tuning-crystal speaker synth visualizer statue lady-liberty fountain torch
                      coconut-palm lemon-tree sunflower shrubbery banana-plant code basalt snow
                      market-stall))

;; Disabled in endless mode.
(property-configure prop-sk-forever-disabled
                    '(crane escape-beacon))

(property-configure prop-fireproof
                    '(forcefield forcefield* barrier bulkhead-door water water-source ice
                      masonry basalt))

;; Fire will easily spread to this block, and will not extinguish on its own.
(property-configure prop-highly-flammable
                    '(mycelium dynamite dynamite-ii torch coconut-palm lemon-tree sunflower
                      shrubbery banana-plant))

;; Characters can walk through these blocks.
(property-configure prop-habitable
                    '(ion-fizzler decimator workshop manufactory power-core reactor
                      backup-core war-engine chaos-core overdrive-core balloon stairwell
                      ladder ladder+ stairwell+ stairwell++ bridge portal bulkhead-door
                      infirmary cargo-bay transporter replicator plundered-room))

(property-configure prop-generates-heat
                    '(radiator))

;; Explodes slightly bigger than a standard block.
(property-configure prop-big-explosion
                    '(cannon ion-cannon flak-gun arc-gun nemesis fire-charge sylph-cannon
                      decimator annihilator spark-cannon incinerator beam-gun
                      particle-lance ballista missile-silo rocket-bomb splitter warhead
                      overdrive-core dynamite dynamite-ii))

;; One allowed per island.
(property-configure prop-singleton
                    '(windmill targeting-computer))

(property-configure prop-human-only
                    '(reactor solar-cell drone-bay))

(property-configure prop-sylph-only
                    '(cloak sylph-cannon deflector amplifier phase-shifter))

(property-configure prop-goblin-only
                    '(nemesis decimator chaos-core))
