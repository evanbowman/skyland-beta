;;;
;;; config/properties.lisp
;;;


(property-configure prop-workshop-required
                    '(forcefield radiator mycelium ion-cannon flak-gun annihilator
                      manufactory power-core solar-cell backup-core overdrive-core
                      radar transporter replicator drone-bay))

(property-configure prop-not-constructible
                    '(overdrive-core balloon ladder+ stairwell+ stairwell++ cargo-bay
                      crane water ice escape-beacon code basalt snow market-stall))

(property-configure prop-plugin
                    '())

(property-configure prop-manufactory-required
                    '(forcefield* energized-hull ion-fizzler cloak mirror-hull
                      stacked-hull arc-gun nemesis fire-charge sylph-cannon decimator
                      spark-cannon rocket-bomb reactor chaos-core portal dynamite-ii
                      deflector))

(property-configure prop-tutorial-disabled
                    '(bronze-hull radiator cloak mirror-hull stacked-hull mycelium
                      annihilator spark-cannon rocket-bomb warhead backup-core windmill
                      ladder ladder+ stairwell+ stairwell++ bridge portal crane
                      weather-engine water water-source dynamite dynamite-ii
                      targeting-computer escape-beacon phase-shifter visualizer))

(property-configure prop-locked-by-default
                    '(bronze-hull radiator mirror-hull mycelium annihilator spark-cannon
                      windmill bridge weather-engine dynamite dynamite-ii bell
                      tuning-crystal speaker synth visualizer statue lady-liberty
                      fountain coconut-palm lemon-tree banana-plant masonry market-stall))

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

(property-configure prop-has-chimney
                    '(power-core reactor backup-core war-engine chaos-core overdrive-core))

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

(property-configure prop-flag-mount
                    '(hull bronze-hull radiator mirror-hull stacked-hull mycelium barrier
                      crane weather-engine dynamite dynamite-ii targeting-computer
                      deflector amplifier torch))

(property-configure prop-fragile
                    '(windmill balloon bridge water water-source ice bell tuning-crystal
                      speaker synth visualizer statue lady-liberty fountain torch
                      coconut-palm lemon-tree sunflower shrubbery banana-plant masonry
                      code basalt snow market-stall canvas))

(property-configure prop-adventure-only
                    '(crane))

(property-configure prop-sandbox-only
                    '(barrier incinerator beam-gun particle-lance ballista splitter
                      warhead war-engine plundered-room torch canvas))

(property-configure prop-fluid
                    '(water water-source))

(property-configure prop-destroy-special
                    '(forcefield forcefield* power-core reactor war-engine water water-source))

(property-configure prop-salvage-disabled
                    '(mycelium dynamite dynamite-ii))

(property-configure prop-multiplayer-disabled
                    '(fire-charge spark-cannon incinerator rocket-bomb splitter windmill balloon
                      bridge crane weather-engine ice escape-beacon deflector phase-shifter bell
                      tuning-crystal speaker synth visualizer statue lady-liberty fountain torch
                      coconut-palm lemon-tree sunflower shrubbery banana-plant code basalt snow
                      market-stall))

(property-configure prop-sk-forever-disabled
                    '(crane escape-beacon))

(property-configure prop-fireproof
                    '(forcefield forcefield* barrier bulkhead-door water water-source ice
                      masonry basalt))

(property-configure prop-highly-flammable
                    '(mycelium dynamite dynamite-ii torch coconut-palm lemon-tree sunflower
                      shrubbery banana-plant))

(property-configure prop-habitable
                    '(ion-fizzler decimator workshop manufactory power-core reactor
                      backup-core war-engine chaos-core overdrive-core balloon stairwell
                      ladder ladder+ stairwell+ stairwell++ bridge portal bulkhead-door
                      infirmary cargo-bay transporter replicator plundered-room))

(property-configure prop-generates-heat
                    '(radiator))

(property-configure prop-big-explosion
                    '(cannon ion-cannon flak-gun arc-gun nemesis fire-charge sylph-cannon
                      decimator annihilator spark-cannon incinerator beam-gun
                      particle-lance ballista missile-silo rocket-bomb splitter warhead
                      overdrive-core dynamite dynamite-ii))

(property-configure prop-singleton
                    '(windmill targeting-computer))

(property-configure prop-human-only
                    '(reactor solar-cell drone-bay))

(property-configure prop-sylph-only
                    '(cloak sylph-cannon deflector amplifier phase-shifter))

(property-configure prop-goblin-only
                    '(nemesis decimator chaos-core))
