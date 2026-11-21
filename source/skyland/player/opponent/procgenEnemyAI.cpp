////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "procgenEnemyAI.hpp"
#include "allocator.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/network.hpp"
#include "skyland/preload.hpp"
#include "skyland/roomTable.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/chaosCore.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/masonry.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



static SHARED_VARIABLE(sf_p1_coin_yield);
static SHARED_VARIABLE(sf_p2_coin_yield);
static SHARED_VARIABLE(sf_p3_coin_yield);
static SHARED_VARIABLE(sf_p4_coin_yield);
static SHARED_VARIABLE(chaos_core_placement_chance);
static SHARED_VARIABLE(enemy_cargo_placement_chance_1);
static SHARED_VARIABLE(enemy_cargo_placement_chance_2);
SHARED_VARIABLE(enemy_cargo_min_value);



static const int adventure_max_level = 16;

static const int escalation_start_level = 20;
static_assert(escalation_start_level > adventure_max_level);

static const int level_threshold_dual_reactor = escalation_start_level;
static const int level_threshold_war_engine = 28;
static const int level_threshold_amplifier = 24;
static const int level_threshold_second_amplifier = 34;

static const int amplifier_radius = 1;
static const int deflector_radius = 3;



void prep_level();



ProcgenEnemyAI::ProcgenEnemyAI(rng::LinearGenerator seed, u8 difficulty)
    : rng_source_(seed), difficulty_(difficulty)
{
}



void ProcgenEnemyAI::update(Time delta)
{
    if (not APP.opponent_island()) {
        generate_level();

        if (APP.game_mode() == App::GameMode::skyland_forever) {
            for (auto it = APP.birds().begin(); it not_eq APP.birds().end();) {
                if ((*it)->island() == opponent_island()) {
                    it = APP.birds().erase(it);
                } else {
                    ++it;
                }
            }
            GenericBird::spawn(*APP.opponent_island(),
                               rng::choice<4>(rng::utility_state));
        }

        if (APP.game_mode() == App::GameMode::co_op) {

            const int prep_seconds = [&] {
                if (levelgen_enemy_count_ < 2) {
                    return 30;
                } else if (levelgen_enemy_count_ < 3) {
                    return 20;
                } else {
                    return 3;
                }
            }();


            auto& g = globals();

            g.multiplayer_prep_seconds_ = prep_seconds;
            g.multiplayer_pauses_remaining_ = 3;
            g.multiplayer_pause_owner_ = false;

            if (state_bit_load(StateBit::multiboot)) {
                // The multiboot peer doesn't run the same game logic, it's just
                // an empty shell that recieves all updates from the server host
                // (this code). We need to tell the multiboot instance what the
                // opponent's island looks like.

                int counter = 0;
                for (auto& room : APP.opponent_island()->rooms()) {
                    if (++counter == 16) {
                        counter = 0;
                        // Wait for some queued transmits to finish
                        PLATFORM.sleep(4);
                    }

                    network::packet::OpponentRoomCreated p;
                    p.x_ = room->position().x;
                    p.y_ = room->position().y;
                    p.metaclass_index_.set(room->metaclass_index());
                    network::transmit(p);
                }
            }
        }
    } else {
        auto mt_prep_seconds = globals().multiplayer_prep_seconds_;

        if (APP.game_mode() == App::GameMode::co_op) {

            if (mt_prep_seconds == 0 and PLATFORM.network_peer().is_host()) {
                EnemyAI::update(delta);
            }
        } else {
            EnemyAI::update(delta);
        }
    }

    if (not APP.opponent_island() or APP.player_island().is_destroyed()) {
        level_text_.reset();
    } else if (not level_text_) {
        level_text_.emplace(
            OverlayCoord{u8(calc_screen_tiles().x -
                            integer_text_length(levelgen_enemy_count_)),
                         0});
    } else {
        level_text_->assign(levelgen_enemy_count_);
    }
}



void ProcgenEnemyAI::generate_level()
{
    // In long Skyland Forever runs, a heavily built-out player island plus a
    // large generated enemy island can exhaust the shared room pool. Level
    // generation treats room placement as best-effort, so just generate a
    // smaller island rather than crashing.
    RoomPoolExhaustionTolerance pool_tolerance;

    for (auto& room : APP.player_island().rooms()) {
        // TODO: gather information about the layout of the player's castle.
        (void)room;
    }

    if (not script_preload_active()) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
            }
        }

        PLATFORM.set_scroll(Layer::map_1_ext, -250, -374);
    }

    PLATFORM.screen().clear();

    auto area = [&] {
        if (levelgen_enemy_count_ < 3) {
            return 20;
        } else if (levelgen_enemy_count_ < 5) {
            return 30;
        } else if (levelgen_enemy_count_ < 8) {
            return 40;
        } else if (levelgen_enemy_count_ < 12) {
            return 50;
        } else {
            return 70;
        }
    }();

    levelgen_size_.y = 4 + rng::choice<5>(rng_source_);

    if (levelgen_enemy_count_ < 4) {
        levelgen_size_.y = std::min(u8(5), levelgen_size_.y);
    }

    levelgen_size_.x = std::min(13, std::max(3, area / levelgen_size_.y));


    APP.create_opponent_island(levelgen_size_.x);
    APP.opponent_island()->show_flag(true);

    generate_power_sources();

    if (levelgen_enemy_count_ > 2 or rng::choice<2>(rng_source_)) {
        generate_stairwells();
    }

    generate_secondary_rooms();
    generate_cargo();
    generate_characters();
    generate_radiators();

    generate_hull();

    int weapon_limit = 0;
    if (levelgen_enemy_count_ < 1) {
        weapon_limit = 1;
    } else if (levelgen_enemy_count_ < 2) {
        weapon_limit = 2;
    } else if (core_count_ < 2) {
        weapon_limit = 3;
    } else if (core_count_ < 3) {
        weapon_limit = 5;
    } else if (core_count_ < 4) {
        weapon_limit = 7;
    } else {
        weapon_limit = 11;
    }
    generate_weapons(weapon_limit);

    generate_support_rooms();

    if (core_count_ >= 2) {
        generate_forcefields();
        generate_missile_defenses();
    }

    generate_foundation();

    cleanup_unused_terrain();

    generate_walls_behind_weapons();

    generate_decorations();

    generate_foundation();


    if (not script_preload_active()) {
        prep_level();
        show_island_exterior(APP.opponent_island());
    }

    // The level generation may have taken a long time. Shouldn't have any
    // bearing on gameplay.
    PLATFORM.delta_clock().reset();

    ++levelgen_enemy_count_;

    if (APP.game_mode() == App::GameMode::skyland_forever or
        APP.game_mode() == App::GameMode::co_op) {
        for (auto& room : APP.opponent_island()->rooms()) {

            auto frac = 0.4f;

            if (levelgen_enemy_count_ > 42) {
                frac = 0.01f;
            }
            if (levelgen_enemy_count_ > 36) {
                frac = (sf_p4_coin_yield * 0.01) / 8;
            } else if (levelgen_enemy_count_ > 28) {
                frac = (sf_p4_coin_yield * 0.01) / 2;
            } else if (levelgen_enemy_count_ > 20) {
                frac = sf_p4_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 16) {
                frac = sf_p3_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 12) {
                frac = sf_p2_coin_yield * 0.01;
            } else if (levelgen_enemy_count_ > 7) {
                frac = sf_p1_coin_yield * 0.01;
            }

            // NOTE: we award coins based on opponent island value. But we
            // generate mirror-hull to discourage certain playing styles, so do
            // not award extra value to players when generating mirror-hull.
            Coins cost = 0;
            if (str_eq(room->name(), "mirror-hull")) {
                cost = 200;
            } else {
                cost = (*room->metaclass())->cost();
            }

            if (APP.game_mode() == App::GameMode::co_op) {
                // For co-op, our score calculation differs slightly. Give each
                // player half of the resulting coins.
                APP.victory_coins() += (frac * cost * 0.5f);
            } else {
                APP.victory_coins() += frac * cost;
            }
        }
    }

    if (APP.game_mode() == App::GameMode::co_op) {
        // Give the player a just a bit more coins in co-op mode, as the coin
        // count is shared with the other player.
        APP.victory_coins() += 500;
    }

    if (APP.game_mode() not_eq App::GameMode::co_op) {
        APP.time_stream().enable_pushes(true);
        APP.time_stream().clear();
    }

    level_text_.emplace(OverlayCoord{
        u8(calc_screen_tiles().x - integer_text_length(levelgen_enemy_count_)),
        0});
}



void ProcgenEnemyAI::set_levelgen_count(int count)
{
    levelgen_enemy_count_ = count;
}



int ProcgenEnemyAI::escalation() const
{
    return std::max(0, levelgen_enemy_count_ - escalation_start_level + 1);
}



static const int level_threshold_two_powercores = 4;



void ProcgenEnemyAI::generate_power_sources()
{
    core_count_ = 0;
    int reactor_count = 0;
    bool war_engine = false;

    if (levelgen_enemy_count_ < level_threshold_two_powercores) {
        core_count_ = 1;
    } else if (levelgen_enemy_count_ < 10) {
        core_count_ = 2;
    } else if (levelgen_enemy_count_ < 14) {
        core_count_ = 3;
    } else if (levelgen_enemy_count_ < level_threshold_dual_reactor) {
        core_count_ = 3;
        reactor_count = 1;
    } else if (levelgen_enemy_count_ < level_threshold_war_engine) {
        // 1100 power from four rooms (previously 850).
        core_count_ = 2;
        reactor_count = 2;
    } else {
        // 1550 power from three rooms. The war-engine carries most of the
        // island's power in a single 400hp block.
        core_count_ = 1;
        reactor_count = 1;
        war_engine = true;
    }

    const int x_start = levelgen_size_.x < 4 ? 1 : levelgen_size_.x / 2 - 1;

    if (war_engine) {
        // Place it first: at 3x4 it's the hardest room to fit.
        if (not place_room_random_loc(x_start, "war-engine")) {
            // Fall back to the dual-reactor layout.
            war_engine = false;
            ++core_count_;
            ++reactor_count;
        }
    }

    for (int i = 0; i < core_count_; ++i) {
        place_room_random_loc(x_start, "power-core");
    }

    for (int i = 0; i < reactor_count; ++i) {
        const char* core = "reactor";
        if (rng::chance(chaos_core_placement_chance, rng_source_)) {
            bool chaos_core_present = false;
            for (auto& room : APP.player_island().rooms()) {
                if (room->cast<ChaosCore>()) {
                    chaos_core_present = true;
                }
            }
            // Late levels place two reactors, don't let both be chaos cores.
            for (auto& room : APP.opponent_island()->rooms()) {
                if (room->cast<ChaosCore>()) {
                    chaos_core_present = true;
                }
            }
            if (not chaos_core_present) {
                core = "chaos-core";
            }
        }
        place_room_random_loc(x_start, core);
    }

    core_count_ += reactor_count;

    if (war_engine) {
        // Keeps core_count_ at 4, so weapon_limit, character count, and
        // transporter generation match the level 14-19 configuration.
        core_count_ += 2;
    }
}



Power ProcgenEnemyAI::power_remaining() const
{
    return APP.opponent_island()->power_supply() -
           APP.opponent_island()->power_drain();
}



void ProcgenEnemyAI::generate_weapons(int max)
{
    const bool co_op = APP.game_mode() == App::GameMode::co_op;

    int missile_count = 0;
    int drone_count = 0;
    int cannon_count = 0;
    int flak_count = 0;
    int nemesis_count = 0;
    int arc_count = 0;
    int forcefield_count = 0;
    int misc_cannon_count = 0;
    [[maybe_unused]] int ion_cannon_count = 0;
    int mycelium_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        const auto category = (*room->metaclass())->category();

        if (str_eq(room->name(), "splitter")) {
            missile_count += 2;
        } else if (str_eq(room->name(), "missile-silo") or
                   str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if (str_eq(room->name(), "drone-bay")) {
            ++drone_count;
        } else if (str_eq(room->name(), "cannon")) {
            ++cannon_count;
        } else if (str_eq(room->name(), "forcefield") or
                   str_eq(room->name(), "forcefield*")) {
            ++forcefield_count;
        } else if (str_eq(room->name(), "flak-gun")) {
            ++flak_count;
        } else if (str_eq(room->name(), "nemesis")) {
            ++nemesis_count;
        } else if (str_eq(room->name(), "arc-gun")) {
            ++arc_count;
        } else if (str_eq(room->name(), "spark-cannon")) {
            ++arc_count;
        } else if (str_eq(room->name(), "ion-cannon")) {
            ++ion_cannon_count;
        } else if (str_eq(room->name(), "mycelium")) {
            ++mycelium_count;
        } else if (category == Room::Category::weapon) {
            ++misc_cannon_count;
        }
    }


    int generic_cannon_count =
        cannon_count + flak_count + arc_count + misc_cannon_count;

    [[maybe_unused]] int player_avg_roof_hull_thickness = 0;

    for (u8 x = 0; x < (int)APP.player_island().terrain().size(); ++x) {
        for (u8 y = construction_zone_min_y; y < 15; ++y) {
            if (auto room = APP.player_island().get_room({x, y})) {
                if ((*room->metaclass())->category() == Room::Category::wall) {
                    ++player_avg_roof_hull_thickness;
                } else {
                    break;
                }
            }
        }
    }

    if (APP.player_island().terrain().size() not_eq 0) {
        player_avg_roof_hull_thickness /= APP.player_island().terrain().size();
    }

    int player_avg_forward_hull_thickness = 0;
    int min_present_y = 15;

    for (u8 y = construction_zone_min_y; y < 15; ++y) {
        for (u8 x = (int)APP.player_island().terrain().size() - 1; x > 1; --x) {
            if (auto room = APP.player_island().get_room({x, y})) {
                if ((*room->metaclass())->category() == Room::Category::wall) {
                    min_present_y = std::min(min_present_y, (int)y);
                    ++player_avg_forward_hull_thickness;
                } else {
                    break;
                }
            }
        }
    }

    const int divisor = 15 - min_present_y;
    if (divisor) {
        player_avg_forward_hull_thickness /= divisor;
    }

    struct Context
    {
        Buffer<RoomMeta*, 200> distribution_;

        struct Pair
        {
            const char* weapon_name_;
            Float probability_;
        };

        bool invalidated_missile_cells_[16][16];
        bool invalidated_cannon_cells_[16][16];

        Buffer<Pair, 30> pairs_;
    };

    auto c = allocate<Context>("procgen-buffer");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            c->invalidated_cannon_cells_[x][y] = false;
            c->invalidated_missile_cells_[x][y] = false;
        }
    }

    auto enq_prob = [&](const char* mt_name, Float prob) {
        c->pairs_.push_back({mt_name, prob});
    };

    if (levelgen_enemy_count_ > 4) {
        Float flak_prob = 100.f;
        flak_prob += (player_avg_forward_hull_thickness < 2 ? 25.f : -25.f);
        if (nemesis_count) {
            flak_prob += 100.f;
        }
        enq_prob("flak-gun", flak_prob);
    }

    if (forcefield_count) {
        enq_prob("ion-cannon", 40.f + forcefield_count * 20.f);
    }

    if (levelgen_enemy_count_ > 9 and difficulty_ > 0 and
        // TODO: send network events in EnemyAI for creating/destroying
        // drones. Until then, we need to disable drone-bays in co-op mode.
        APP.game_mode() not_eq App::GameMode::co_op) {

        if (missile_count < 5) {
            enq_prob("drone-bay",
                     (50.f - missile_count * 10.f) + drone_count * 20.f);
        }
    }

    // Fire spread is really super difficult to synchronize over a low bandwidth
    // GBA link cable. Disabled until I can get it to work reliably.
    const char* fire_charge_substitution =
        PLATFORM.network_peer().is_connected() ? "nemesis" : "fire-charge";

    if (levelgen_enemy_count_ > 8) {
        if (missile_count > 2) {
            enq_prob("cannon", 50.f);
            enq_prob("arc-gun", 220.f);
            enq_prob("nemesis", 30.f);
            enq_prob(fire_charge_substitution, 30.f);
        } else {
            enq_prob("cannon", 100.f);
            if (mycelium_count) {
                enq_prob("arc-gun", 200.f);
                enq_prob(fire_charge_substitution, 90.f);
                enq_prob("nemesis", 30.f);
            } else {
                enq_prob("arc-gun", 120.f);
                enq_prob(fire_charge_substitution, 40.f);
                enq_prob("nemesis", 80.f);
            }
        }

        enq_prob("missile-silo",
                 240.f + 10.f * missile_count + 10 * generic_cannon_count +
                     50.f * drone_count);
        enq_prob(co_op ? "missile-silo" : "rocket-bomb", 60.f);
    } else {
        enq_prob("cannon", 100.f);

        enq_prob("missile-silo",
                 90.f + 10.f * missile_count + 10 * generic_cannon_count +
                     50.f * drone_count);
    }

    const int esc = escalation();
    int antique_limit = 0;

    auto is_antique = [](const char* name) {
        return str_eq(name, "beam-gun") or str_eq(name, "ballista") or
               str_eq(name, "incinerator") or
               str_eq(name, "particle-lance") or str_eq(name, "decimator");
    };

    // Not in co-op: none of these have been tested over the link cable, and the
    // incinerator spreads fire (see fire_charge_substitution).
    if (esc > 0 and not co_op) {
        // Antique weapons. Each takes the slot of a regular weapon, and most fill
        // several tiles with one room, so difficulty goes up without raising
        // the opponent's room count.
        antique_limit = std::min(1 + esc / 8, 4);

        // Against the regular pool's total weight (roughly 800-900), this
        // ramps from about 1% at level 20 to about 18% by level 39.
        const Float w = std::min(esc * 10.f, 200.f);

        Float beam_w = w * 0.35f;
        Float ballista_w = w * 0.25f;
        if (player_avg_forward_hull_thickness >= 2) {
            // The beam damages every block it touches, and the ballista arcs
            // over forward hull: both are strong against a thick front.
            beam_w *= 1.5f;
            ballista_w *= 1.5f;
        }
        enq_prob("beam-gun", beam_w);
        enq_prob("ballista", ballista_w);
        enq_prob("incinerator", w * 0.25f);
        // Destroys itself after one shot.
        enq_prob("particle-lance", w * 0.15f);

        if (levelgen_enemy_count_ >= level_threshold_war_engine) {
            // 160 power. Only worth it once the war-engine is available.
            enq_prob("decimator", w * 0.2f);
        }
    }

    Float total_prob = 0.f;
    for (auto& p : c->pairs_) {
        total_prob += p.probability_;
    }

    if (total_prob == 0.f) {
        Platform::fatal("weapon gen failed!");
    }

    for (auto& p : c->pairs_) {
        p.probability_ /= total_prob;
        auto& mt = require_metaclass(p.weapon_name_);
        for (int i = 0; i < p.probability_ * c->distribution_.capacity(); ++i) {
            c->distribution_.push_back(&mt);
        }
    }

    auto place_missile_silo = [&](RoomMeta* mt) {
        Buffer<RoomCoord, 16> slots;
        const u8 ht = (*mt)->constructed_size().y;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = construction_zone_min_y; y < 14; ++y) {
                auto room = APP.opponent_island()->get_room({x, u8(y + ht)});
                if ((y == 13 or room) and has_space({x, y}, {1, ht}) and
                    not c->invalidated_missile_cells_[x][y] and
                    not c->invalidated_missile_cells_[x][y + 1]) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            for (int yy = 0; yy < s.y; ++yy) {
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_cannon_cells_[s.x][yy] = true;
            }
        }
    };

    auto place_cannon = [&](RoomMeta* mt) {
        Buffer<RoomCoord, 16> slots;

        // All regular weapons are one tile tall, so the multi-row handling
        // below only matters for the 2x2 antiques. For height 1 it does
        // exactly what the original code did.
        const auto sz = (*mt)->constructed_size();

        for (u8 y = construction_zone_min_y; y < 14; ++y) {
            for (u8 x = 0; x < 15; ++x) {

                Room* room = nullptr;
                if (x + sz.x < 16) {
                    room = APP.opponent_island()->get_room({u8(x + sz.x), y});
                }

                bool invalid = false;
                for (int xx = x; xx < std::min(16, x + sz.x); ++xx) {
                    for (int yy = y; yy < std::min(16, y + sz.y); ++yy) {
                        if (c->invalidated_cannon_cells_[xx][yy]) {
                            invalid = true;
                        }
                    }
                }

                Room* below = nullptr;
                if (y + sz.y < 16) {
                    below = APP.opponent_island()->get_room({x, u8(y + sz.y)});
                }

                if ((room or below) and has_space({x, y}, sz) and
                    not invalid) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        // Placing weapons nearer to the leftmost position reduces the chances
        // of placing cannons in the path of other cannons.
        std::sort(slots.begin(), slots.end(), [](auto& lhs, auto& rhs) {
            return lhs.x < rhs.x;
        });

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            for (int yy = s.y; yy < std::min(16, s.y + sz.y); ++yy) {
                for (int xx = 0; xx < 15; ++xx) {
                    c->invalidated_cannon_cells_[xx][yy] = true;
                }

                // Only invalidate the slots in front of the cannon, i.e. it's
                // fine to place missile-silos behind a cannon-type weapon.
                for (int xx = 0; xx < s.x; ++xx) {
                    c->invalidated_missile_cells_[xx][yy] = true;
                }
            }

            if (str_eq((*mt)->name(), "decimator")) {
                // The decimator only reloads while someone is inside.
                // generate_characters() runs before weapons exist, so staff it
                // here. Characters stand on the room's bottom row.
                const RoomCoord pos{s.x, u8(s.y + sz.y - 1)};
                if (not APP.opponent_island()->character_at_location(pos)) {
                    auto e = alloc_entity<Character>(
                        APP.opponent_island(), &APP.opponent(), pos, false);
                    if (e) {
                        APP.opponent_island()->add_character(std::move(e));
                    }
                }
            }

            return true;
        }

        return false;
    };

    auto place_drone_bay = [&](RoomMeta* mt) {
        // We mostly treat a drone bay as if it were simply a differently-shaped
        // missile-silo.

        Buffer<RoomCoord, 16> slots;
        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = construction_zone_min_y; y < 14; ++y) {
                auto room = APP.opponent_island()->get_room({x, u8(y + 1)});
                if ((y == 13 or room) and has_space({x, y}, {2, 1}) and
                    not c->invalidated_missile_cells_[x][y] and
                    not c->invalidated_missile_cells_[x + 1][y]) {

                    slots.push_back({x, y});
                    break;
                } else if (room) {
                    // If we've seen a room, we shouldn't place a missile-silo
                    // under it!
                    break;
                }
            }
        }

        rng::shuffle(slots, rng_source_);

        if (not slots.empty()) {
            auto s = slots[0];
            (*mt)->create(APP.opponent_island(), {s.x, s.y});

            c->invalidated_cannon_cells_[s.x][s.y - 1] = true;
            c->invalidated_cannon_cells_[s.x + 1][s.y - 1] = true;

            for (int yy = 0; yy < s.y; ++yy) {
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_missile_cells_[s.x + 1][yy] = true;
            }

            for (int yy = s.y; yy < 15; ++yy) {
                // Invalidate missile-silos beneath the drone bay
                c->invalidated_missile_cells_[s.x][yy] = true;
                c->invalidated_missile_cells_[s.x + 1][yy] = true;
            }
        }
    };

    [[maybe_unused]] int place_missile_count = 0;
    int placed_ion_cannons = 0;
    int placed_antiques = 0;
    int placed_decimators = 0;
    int placed_lances = 0;
    int fails = 0;

    Buffer<u16, 20> missile_silo_mts;

    for (int i = 0; i < max; ++i) {
    RETRY:
        auto ind = rng::choice(c->distribution_.size(), rng_source_);
        auto sel = c->distribution_[ind];

        if (fails > 40) {
            sel = load_metaclass("cannon");
            fails = 0;
        }

        if (is_antique((*sel)->name())) {
            // Antiques only appear in the distribution when escalation() > 0.
            // Re-roll rather than breaking out of the loop when one doesn't
            // fit the power budget: the decimator alone draws 160, and a break
            // would end weapon generation early.
            if (placed_antiques >= antique_limit or
                (str_eq((*sel)->name(), "decimator") and placed_decimators) or
                (str_eq((*sel)->name(), "particle-lance") and
                 placed_lances) or
                power_remaining() <= (*sel)->consumes_power()) {
                ++fails;
                goto RETRY;
            }

            if (place_cannon(sel)) {
                ++placed_antiques;
                if (str_eq((*sel)->name(), "decimator")) {
                    ++placed_decimators;
                } else if (str_eq((*sel)->name(), "particle-lance")) {
                    ++placed_lances;
                }
            }
            continue;
        }

        if (power_remaining() > (*sel)->consumes_power()) {

            if (str_eq((*sel)->name(), "ion-cannon") and
                placed_ion_cannons > 1) {
                ++fails;
                goto RETRY;
            }

            if (str_eq((*sel)->name(), "ion-cannon")) {
                ++placed_ion_cannons;
            }

            if (str_eq((*sel)->name(), "missile-silo") or
                str_eq((*sel)->name(), "rocket-bomb")) {
                ++place_missile_count;
                missile_silo_mts.push_back(ind);
            } else if (str_eq((*sel)->name(), "drone-bay")) {
                place_drone_bay(sel);
            } else {
                place_cannon(sel);
            }

        } else {
            break;
        }
    }

    for (auto ind : missile_silo_mts) {
        auto sel = c->distribution_[ind];
        place_missile_silo(sel);
    }
}



void ProcgenEnemyAI::generate_missile_defenses()
{
    // Skip it sometimes. Late in SKYLAND Forever, always respond to a
    // missile-heavy player.
    if (escalation() == 0 and rng::choice<4>(rng_source_) == 0) {
        return;
    }

    int missile_count = 0;
    int lateral_count = 0;
    int clump_count = 0;
    int ballista_count = 0;

    auto& shull = require_metaclass("stacked-hull");

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "splitter")) {
            clump_count += 1;
            missile_count += 3;
        } else if (str_eq(room->name(), "missile-silo") or
                   str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if ((*room->metaclass())->category() == Room::Category::weapon) {
            ++lateral_count;
        } else if (str_eq(room->name(), "ballista")) {
            ++ballista_count;
        }
    }

    // If the player has a missile heavy build, create extra defenses.
    if ((ballista_count > 1 and rng::choice<3>(rng_source_)) or
        (clump_count > 0 and rng::choice<2>(rng_source_)) or
        (lateral_count == 0 and missile_count > 0 and
         rng::choice<3>(rng_source_) == 0) or
        (missile_count > lateral_count and missile_count - lateral_count > 3)) {

        bool invalid_rows[16];
        memset(invalid_rows, false, sizeof invalid_rows);
        for (int y = 0; y < construction_zone_min_y; ++y) {
            invalid_rows[y] = true;
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                invalid_rows[room->position().y] = true;
            }
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon and
                not str_eq(room->name(), "missile-silo") and
                not str_eq(room->name(), "rocket-bomb")) {
                auto p = room->position();
                p.y--;
                if (invalid_rows[p.y]) {
                    continue;
                }
                for (int x = 0; x < room->size().x; ++x) {
                    if (not APP.opponent_island()->get_room(p)) {
                        shull->create(APP.opponent_island(), p);
                    }
                    ++p.x;
                }
            }
        }
    }
}



void shift_rooms_right(Island& island);



void ProcgenEnemyAI::generate_forcefields()
{
    struct Context
    {
        struct Slot
        {
            RoomCoord coord_;
            Float weight_;
        };

        Buffer<Slot, 60> slots_;
    };

    auto c = allocate<Context>("procgen-buffer");

    int free_count = 15 - construction_zone_min_y;
    for (int i = 0; i < 15; ++i) {
        if (APP.opponent_island()->get_room({0, (u8)i})) {
            --free_count;
        }
    }
    auto t_size = APP.opponent_island()->terrain().size();
    if (free_count < 5 and t_size < 13) {
        APP.opponent_island()->init_terrain(++t_size, false);
        shift_rooms_right(*APP.opponent_island());
        if (not script_preload_active()) {
            APP.opponent_island()->repaint();
        }
    }


    auto find_ideal_forcefield_locs = [&] {
        c->slots_.clear();

        int player_missile_count = 0;
        int player_cannon_count = 0;
        [[maybe_unused]] int player_ion_cannon_count = 0;
        int player_annihilator_count = 0;
        for (auto& room : APP.player_island().rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "annihilator")) {
                    ++player_annihilator_count;
                } else if (str_eq(room->name(), "missile-silo") or
                           str_eq(room->name(), "rocket-bomb")) {
                    ++player_missile_count;
                } else if (str_eq(room->name(), "ion-cannon")) {
                    ++player_ion_cannon_count;
                } else {
                    ++player_cannon_count;
                }
            }
        }

        int opponent_missile_count = 0;
        int opponent_cannon_count = 0;
        for (auto& room : APP.opponent_island()->rooms()) {
            if ((*room->metaclass())->category() == Room::Category::weapon) {
                if (str_eq(room->name(), "missile-silo") or
                    str_eq(room->name(), "rocket-bomb")) {
                    ++opponent_missile_count;
                } else {
                    ++opponent_cannon_count;
                }
            }
        }

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = construction_zone_min_y; y < 15; ++y) {
                Float weight = 0.f;

                auto get_room = [&](u8 x, u8 y) {
                    return APP.opponent_island()->get_room({x, y});
                };

                if (x < 15) {
                    if (APP.opponent_island()->rooms_view().get(x + 1, y)) {
                        if (auto room = get_room(x + 1, y)) {
                            if ((not str_eq(room->name(), "missile-silo") and
                                 not str_eq(room->name(), "rocket-bomb") and
                                 not str_eq(room->name(), "ion-cannon")) and
                                (*room->metaclass())->category() ==
                                    Room::Category::weapon) {

                                if (opponent_cannon_count >
                                    player_cannon_count) {
                                    weight += 150.f;
                                } else {
                                    weight += 100.f;
                                }
                            }
                        }
                    }
                }

                if (y < 15) {
                    if (APP.opponent_island()->rooms_view().get(x, y + 1)) {
                        if (auto room = get_room(x, y + 1)) {
                            if (str_eq(room->name(), "missile-silo") or
                                str_eq(room->name(), "rocket-bomb") or
                                str_eq(room->name(), "drone-bay")) {
                                if (opponent_missile_count >
                                        player_missile_count or
                                    player_annihilator_count) {
                                    weight += 150.f;
                                } else {
                                    weight += 100.f;
                                }
                            } else if (str_eq(room->name(), "drone-bay")) {
                                if (player_missile_count or
                                    player_annihilator_count) {
                                    weight += 110.f;
                                }
                            }
                        }
                    }
                }

                if (weight not_eq 0.f) {
                    c->slots_.push_back({{x, y}, weight});
                }
            }
        }

        rng::shuffle(c->slots_, rng_source_);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });
    };


    auto& mt = require_metaclass("forcefield");
    auto& mt2 = require_metaclass("forcefield*");

    // Late in SKYLAND Forever, always upgrade forcefields when there's power for
    // it. Short-circuits before the rng call, so earlier levels are unchanged.
    const bool always_upgrade = escalation() > 0;

    while (true) {
        const auto power = power_remaining();

        RoomMeta* sel = &mt;

        if (core_count_ > 2 and
            (always_upgrade or rng::choice<2>(rng_source_)) and
            power > mt2->consumes_power()) {
            sel = &mt2;
        }

        if (power < (*sel)->consumes_power()) {
            return;
        }

        find_ideal_forcefield_locs();

        if (not c->slots_.empty()) {
            bool placed = false;
            for (auto& slot : c->slots_) {
                if (has_space(slot.coord_, {1, 1})) {
                    placed = true;
                    (*sel)->create(APP.opponent_island(), slot.coord_);
                    break;
                }
            }
            if (not placed) {
                return;
            }
        } else {
            return;
        }
    }
}



void ProcgenEnemyAI::generate_hull()
{
    auto& hull = require_metaclass("hull");
    auto& mhull = require_metaclass("mirror-hull");
    auto& shull = require_metaclass("stacked-hull");
    auto& ehull = require_metaclass("energized-hull");
    auto& bhull = require_metaclass("bronze-hull");

    int missile_count = 0;
    int bomb_count = 0;

    int lateral_count = 0;
    int cannon_count = 0;
    int flak_count = 0;
    int arc_count = 0;
    int fire_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "missile-silo") or
            str_eq(room->name(), "rocket-bomb")) {
            ++missile_count;
        } else if (str_eq(room->name(), "cannon")) {
            ++cannon_count;
            ++lateral_count;
        } else if (str_eq(room->name(), "flak-gun")) {
            ++flak_count;
            ++lateral_count;
        } else if (str_eq(room->name(), "arc-gun") or
                   str_eq(room->name(), "spark-cannon")) {
            ++arc_count;
            ++lateral_count;

            if (str_eq(room->name(), "spark-cannon")) {
                ++arc_count;
            }
        } else if (str_eq(room->name(), "fire-charge")) {
            ++fire_count;
            ++lateral_count;
        }
        if (str_eq(room->name(), "rocket-bomb")) {
            ++bomb_count;
        }
    }

    const int max_identical =
        std::max({cannon_count, flak_count, arc_count, fire_count});

    bool missile_defense = false;

    auto generate_roof = [&](bool conservative, int ehull_count) {
        for (u8 x = 0; x < 15; ++x) {

            for (u8 y = construction_zone_min_y; y < 14; ++y) {

                if (APP.opponent_island()->rooms_view().get(x, y + 1)) {
                    auto below =
                        APP.opponent_island()->get_room({x, u8(y + 1)});

                    if (not conservative or
                        (below and (*below->metaclass())->category() not_eq
                                       Room::Category::wall)) {
                        if (not APP.opponent_island()->rooms_view().get(x, y)) {
                            if (ehull_count > 0 and
                                rng::choice<3>(rng_source_) == 0) {
                                --ehull_count;
                                ehull->create(APP.opponent_island(), {x, y});
                            } else if (missile_defense) {
                                shull->create(APP.opponent_island(), {x, y});
                            } else {
                                hull->create(APP.opponent_island(), {x, y});
                            }
                        }
                    }
                }
            }
        }
    };

    generate_roof(true, 0);

    if (missile_count > 2) {
        int ehull_count = 0;
        switch (core_count_) {
        case 1:
            ehull_count = rng::choice<3>(rng_source_);
            break;

        case 2:
            ehull_count = rng::choice<5>(rng_source_);
            break;

        default:
            ehull_count = rng::choice<6>(rng_source_);
            break;
        }

        if (missile_count > 4 or bomb_count > 2) {
            missile_defense = true;
        } else {
            missile_defense = rng::choice<2>(rng_source_);
        }

        if (not missile_defense and missile_count > 0 and lateral_count == 0) {
            if (levelgen_enemy_count_ > 5) {
                missile_defense = rng::choice<2>(rng_source_);
            }
        }
        generate_roof(false, ehull_count);
    }

    auto make_lateral_hull = [&](bool ignore_if_wall) {
        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 0; y < 15; ++y) {

                if (APP.opponent_island()->rooms_view().get(x + 1, y)) {
                    auto right =
                        APP.opponent_island()->get_room({u8(x + 1), y});
                    if (not right) {
                        continue;
                    }
                    if ((*right->metaclass())->category() ==
                            Room::Category::weapon and
                        not str_eq(right->name(), "missile-silo") and
                        not str_eq(right->name(), "rocket-bomb") and
                        not str_eq(right->name(), "ion-cannon")) {
                        continue;
                    }

                    bool place_bhull = false;
                    bool place_mhull = false;
                    if (levelgen_enemy_count_ > 8) {
                        place_mhull = rng::choice<8>(rng_source_) == 0;
                    }

                    if (max_identical > 6 and max_identical == arc_count) {
                        // NOTE: use completely different logic if the player
                        // built lots of arc-guns, as mirror-hull would be weak
                        // to arc-chaining. 1/2 of rooms generated as mirror
                        // hull would be technically the most frustrating thing
                        // for a strategy that heavily favors arc-guns.
                        // But... rather than relying on randomness, let's use
                        // coordinate even/odd parity! That way, every other
                        // hull will be mirrored.
                        place_mhull = x % 2 not_eq y % 2;
                    } else {
                        // If the player just tried to build tons of cannons,
                        // generate some mirror-hull to encourage the player to
                        // attack in a different way.
                        if (max_identical >= 6 and not place_mhull) {
                            place_mhull = true;
                        }
                        if (max_identical >= 6 and not place_mhull) {
                            place_mhull = rng::choice<2>(rng_source_) == 0;
                        } else if (max_identical >= 5 and not place_mhull) {
                            place_mhull = rng::choice<2>(rng_source_) == 0;
                        } else if (max_identical > 3 and not place_mhull) {
                            place_mhull = rng::choice<5>(rng_source_) == 0;
                        }
                    }

                    if (not place_mhull and arc_count > 4) {
                        place_bhull = x % 2 not_eq y % 2;
                    }

                    auto cat = (*right->metaclass())->category();

                    if ((not ignore_if_wall or
                         (ignore_if_wall and
                          cat not_eq Room::Category::wall)) and
                        not APP.opponent_island()->rooms_view().get(x, y)) {

                        if (place_mhull) {
                            mhull->create(APP.opponent_island(), {x, y});
                        } else if (place_bhull) {
                            bhull->create(APP.opponent_island(), {x, y});
                        } else {
                            hull->create(APP.opponent_island(), {x, y});
                        }
                    }
                }
            }
        }
    };

    make_lateral_hull(true);

    if ((flak_count >= 4 or lateral_count > 5)) {
        // Generate an extra later of forward hull if the player has a lot of
        // cannon-type weapons.

        u32 unused_columns = 0;
        for (unused_columns = 0;
             unused_columns < APP.opponent_island()->terrain().size();
             ++unused_columns) {

            u8 x = unused_columns;

            for (u8 y = 0; y < 15; ++y) {
                if (APP.opponent_island()->get_room({x, y})) {
                    goto DONE;
                }
            }
        }
    DONE:
        // But, only generate an extra layer of hull if there's significant
        // space in front of the generated layout.
        if (unused_columns > 0) {
            make_lateral_hull(false);
        }
    }
}



void ProcgenEnemyAI::generate_walls_behind_weapons()
{
    auto& hull = require_metaclass("masonry");

    for (auto& room : APP.opponent_island()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::weapon and
            not str_eq(room->name(), "missile-silo") and
            not str_eq(room->name(), "rocket-bomb")) {

            auto behind = room->position();
            behind.x += (*room->metaclass())->constructed_size().x;

            if (not APP.opponent_island()->get_room(behind)) {
                bool skip = false;
                for (u8 y = behind.y; y < 15; ++y) {
                    if (auto room =
                            APP.opponent_island()->get_room({behind.x, y})) {
                        if (str_eq(room->name(), "missile-silo") or
                            str_eq(room->name(), "rocket-bomb")) {
                            // Don't generate a hull over top of a missile
                            // silo. The enemy will just shoot itself with
                            // missiles.
                            skip = true;
                        }
                    }
                }

                if (not skip) {
                    hull->create(APP.opponent_island(), behind);
                }
            }
        }
    }
}



void ProcgenEnemyAI::generate_stairwells()
{
    // Generate stairwells

    if (levelgen_size_.x <= 3) {
        return;
    }

    struct StairwellPos
    {
        RoomCoord coord_;
        u8 val_;
    };

    struct Context
    {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<StairwellPos, 30> slots;
    };

    auto c = allocate<Context>("procgen-buffer");


    auto mt = load_metaclass("stairwell");

    auto find_ideal_stairwell_slots = [&] {
        c->slots.clear();
        APP.opponent_island()->plot_walkable_zones(c->walkable_zones, nullptr);

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        for (int x = 1; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (APP.opponent_island()->rooms_view().get(x, y)) {
                    c->matrix[x][y] = 0;
                } else {
                    // For a column the size of a stairwell room: how many
                    // walkable tiles would it connect if we placed it starting
                    // at (x,y)?
                    for (int yy = y; yy < std::min(y + 4, 15); ++yy) {
                        if (APP.opponent_island()->rooms_view().get(x, yy)) {
                            continue;
                        }
                        if (x > 0) {
                            if (c->walkable_zones[x - 1][yy]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (x < 15) {
                            if (c->walkable_zones[x + 1][yy]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (yy > 0) {
                            if (c->walkable_zones[x][yy - 1]) {
                                ++c->matrix[x][y];
                            }
                        }
                        if (yy < 15) {
                            if (c->walkable_zones[x][yy + 1]) {
                                ++c->matrix[x][y];
                            }
                        }
                    }
                }
            }
        }
        for (u8 x = 1; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (u8 y = 7; y < 15; ++y) {
                if (c->matrix[x][y]) {
                    c->slots.push_back({{x, y}, c->matrix[x][y]});
                }
            }
        }
        std::sort(c->slots.begin(), c->slots.end(), [](auto& lhs, auto& rhs) {
            return lhs.val_ > rhs.val_;
        });
    };

    find_ideal_stairwell_slots();

    int count = 1;
    if (APP.opponent_island()->rooms().size() >= 4) {
        count = 2;
    }

    int tries = 0;
    while (count and tries < 255) {
        for (auto& slot : c->slots) {
            if (count == 0) {
                break;
            }
            if (has_space(slot.coord_, (*mt)->constructed_size())) {
                (*mt)->create(APP.opponent_island(), slot.coord_);
                --count;
                tries = 0;
                break;
            }
        }
        find_ideal_stairwell_slots();
        ++tries;
    }
}



void ProcgenEnemyAI::generate_radiators()
{
    int player_character_count = 0;
    int player_transporter_count = 0;
    int player_replicator_count = 0;
    int character_count = 0;

    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "replicator")) {
            ++player_replicator_count;
        } else if (str_eq(room->name(), "transporter")) {
            ++player_transporter_count;
        }
        player_character_count += length(room->characters());
    }

    for (auto& room : APP.opponent_island()->rooms()) {
        character_count += length(room->characters());
    }

    if ((player_replicator_count and player_transporter_count > 2) or
        (player_transporter_count > 2 and
         player_character_count > character_count + 3)) {

        // If the player has a replicator and a transporter, or the player has a
        // transporter and more characters than we do, then we may be dealing
        // with a player who's offensively focused on boarding parties. Generate
        // some radiator blocks to make things more difficult.

        const int count = rng::choice<4>(rng_source_);
        for (int i = 0; i < count; ++i) {
            place_room_adjacent("radiator");
        }
        place_room_adjacent("infirmary");
    }
}



void ProcgenEnemyAI::generate_characters()
{
    int transporter_count = 0;
    for (auto& room : APP.opponent_island()->rooms()) {
        if (str_eq(room->name(), "transporter")) {
            ++transporter_count;
        }
    }

    const int chr_count =
        1 + rng::choice(core_count_, rng_source_) +
        (transporter_count > 0 ? rng::choice(transporter_count, rng_source_)
                               : 0);

    struct Context
    {
        struct Slot
        {
            RoomCoord coord_;
            ATP weight_;
        };

        Buffer<Slot, 30> slots_;

        bool matrix_[16][16];
    };

    auto c = allocate<Context>("procgen-buffer");

    for (int i = 0; i < chr_count; ++i) {
        APP.opponent_island()->plot_walkable_zones(c->matrix_, nullptr);

        c->slots_.clear();

        for (u8 x = 0; x < 15; ++x) {
            for (u8 y = 0; y < 15; ++y) {
                if (c->matrix_[x][y]) {
                    if (auto room = APP.opponent_island()->get_room({x, y})) {
                        c->slots_.push_back(
                            {{x, y}, (*room->metaclass())->atp_value()});
                    }
                }
            }
        }

        rng::shuffle(c->slots_, rng_source_);

        std::sort(c->slots_.begin(), c->slots_.end(), [](auto& lhs, auto& rhs) {
            return lhs.weight_ > rhs.weight_;
        });

        if (not c->slots_.empty()) {
            for (auto& slot : c->slots_) {
                if (APP.opponent_island()->character_at_location(slot.coord_)) {
                    continue;
                }
                auto e = alloc_entity<Character>(
                    APP.opponent_island(), &APP.opponent(), slot.coord_, false);
                APP.opponent_island()->add_character(std::move(e));

                break;
            }
        }
    }
}



void ProcgenEnemyAI::generate_decorations()
{
    if (APP.environment().is_cold()) {
        // Don't generate decorative plants and stuff when the weather is snowy.
        return;
    }

    auto& shrubbery_mt = require_metaclass("shrubbery");

    for (u8 x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
        bool empty_column = true;
        u8 y;
        for (y = construction_zone_min_y; y < 14; ++y) {
            if (APP.opponent_island()->rooms_view().get(x, y)) {
                if (auto room = APP.opponent_island()->get_room({x, y})) {
                    if ((*room->metaclass())->category() ==
                            Room::Category::decoration or
                        str_eq(room->name(), "drone-bay")) {
                        empty_column = false;
                        y -= 1;
                        break;
                    }
                    y -= 1;
                    break;
                }
            }
        }


        if (empty_column and y > construction_zone_min_y) {
            switch (rng::choice<5>(rng_source_)) {
            case 0:
            case 1:
            case 2:
                break;

            case 3:
            case 4:
                shrubbery_mt->create(APP.opponent_island(), {x, y});
                break;
            }
        }
    }
}



void ProcgenEnemyAI::place_room_adjacent(const char* room_name)
{
    struct Slot
    {
        RoomCoord coord_;
        u8 val_;
    };

    struct Context
    {
        u8 matrix[16][16];
        bool walkable_zones[16][16];
        Buffer<Slot, 50> slots;
    };

    auto c = allocate<Context>("procgen-buffer");

    auto find_connected_slots = [&](int room_height) {
        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                c->matrix[x][y] = 0;
            }
        }

        c->slots.clear();
        APP.opponent_island()->plot_walkable_zones(c->walkable_zones, nullptr);

        for (int x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
            for (int y = 7; y < 15; ++y) {
                if (APP.opponent_island()->rooms_view().get(x, y)) {
                    c->matrix[x][y] = 0;
                } else {
                    // For a column the size of a stairwell room: how many
                    // walkable tiles would it connect if we placed it starting
                    // at (x,y)?

                    const int yy = y + (room_height - 1);

                    if (x > 0) {
                        if (c->walkable_zones[x - 1][yy]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (x < 15) {
                        if (c->walkable_zones[x + 1][yy]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (yy > 0) {
                        if (c->walkable_zones[x][yy - 1]) {
                            ++c->matrix[x][y];
                        }
                    }
                    if (yy < 15) {
                        if (c->walkable_zones[x][yy + 1]) {
                            ++c->matrix[x][y];
                        }
                    }
                }
            }
        }

        for (u8 x = 0; x < 16; ++x) {
            for (u8 y = 0; y < 16; ++y) {
                if (c->matrix[x][y]) {
                    c->slots.push_back({{x, y}, 0});
                }
            }
        }

        rng::shuffle(c->slots, rng_source_);
    };

    auto try_place_room = [&](RoomMeta& m) {
        int tries = 0;
        while (tries < 255) {
            for (auto& slot : c->slots) {
                if (slot.coord_.x == 0) {
                    continue;
                }
                if (has_space(slot.coord_, m->constructed_size())) {
                    m->create(APP.opponent_island(), slot.coord_);
                    return;
                }
            }
            ++tries;
        }
    };

    auto& mt = require_metaclass(room_name);

    find_connected_slots(mt->constructed_size().y);
    try_place_room(mt);
}



void ProcgenEnemyAI::generate_secondary_rooms()
{
    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng_source_)) {

        place_room_adjacent("infirmary");
    }

    int player_char_count = 0;
    int player_transporter_count = 0;
    for (auto& room : APP.player_island().rooms()) {
        player_char_count += length(room->characters());
        if (str_eq(room->name(), "transporter")) {
            ++player_transporter_count;
        }
    }

    if (player_char_count > 3 or player_transporter_count > 2) {
        place_room_adjacent("infirmary");
    }

    if (levelgen_enemy_count_ > 6 and rng::choice<2>(rng_source_)) {

        int count = rng::choice<3>(rng_source_);
        if (core_count_ == 4) {
            count++;
        }

        for (int i = 0; i < count; ++i) {
            place_room_adjacent("transporter");
        }
    }
}



void ProcgenEnemyAI::generate_cargo()
{
    bool placed_bay = false;

    const int ch1 = enemy_cargo_placement_chance_1;
    const int ch2 = enemy_cargo_placement_chance_2;

    if (ch1 and levelgen_enemy_count_ > 3 and
        rng::choice(ch1, rng_source_) == 0) {
        place_room_adjacent("cargo-bay");
        placed_bay = true;
    }

    if (ch2 and levelgen_enemy_count_ > 10 and
        rng::choice(ch2, rng_source_) == 0) {
        place_room_adjacent("cargo-bay");
        placed_bay = true;
    }

    if (placed_bay) {

        Vector<RoomMeta*> cargo_opts;
        auto meta = room_metatable();

        for (int i = 0; i < meta.second; ++i) {
            if ((meta.first[i])->cost() >= enemy_cargo_min_value) {
                cargo_opts.push_back(&meta.first[i]);
            }
        }

        if (cargo_opts.size() == 0) {
            return;
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            if (auto bay = room->cast<CargoBay>()) {
                auto choice =
                    cargo_opts[rng::choice(cargo_opts.size(), rng_source_)];
                auto cargo_str = format("'(%)", (*choice)->name());
                bay->set_cargo(cargo_str.c_str());
            }
        }
    }
}



void ProcgenEnemyAI::generate_support_rooms()
{
    // Deflectors and amplifiers. Each one goes into an empty tile directly
    // beneath an existing room, which generate_foundation() would otherwise
    // fill with masonry, so this doesn't increase the opponent's room count.
    // It also keeps the amplifier (20hp) covered.

    const int esc = escalation();
    if (esc == 0) {
        return;
    }

    int player_light_weapons = 0;
    for (auto& room : APP.player_island().rooms()) {
        if (str_eq(room->name(), "flak-gun") or
            str_eq(room->name(), "arc-gun") or
            str_eq(room->name(), "spark-cannon")) {
            ++player_light_weapons;
        }
    }

    int amplifier_count = 0;
    if (levelgen_enemy_count_ >= level_threshold_second_amplifier) {
        amplifier_count = 2;
    } else if (levelgen_enemy_count_ >= level_threshold_amplifier) {
        amplifier_count = 1;
    }

    int deflector_count = std::min(1 + esc / 12, 2);
    if (player_light_weapons >= 4) {
        // A flat damage reduction hits weapons that deal many small hits the
        // hardest.
        ++deflector_count;
    }

    auto is_launcher = [](const char* name) {
        return str_eq(name, "missile-silo") or str_eq(name, "rocket-bomb") or
               str_eq(name, "drone-bay");
    };

    // True if a block at (x, y) would sit in one of our own weapons' line of
    // fire.
    auto blocks_weapon = [&](int x, int y) {
        for (auto& room : APP.opponent_island()->rooms()) {
            const bool launcher = is_launcher(room->name());
            if (not launcher and
                (*room->metaclass())->category() not_eq Room::Category::weapon) {
                continue;
            }
            const auto pos = room->position();
            const auto sz = room->size();
            if (launcher) {
                if (x >= pos.x and x < pos.x + sz.x and y < pos.y) {
                    return true;
                }
            } else {
                if (y >= pos.y and y < pos.y + sz.y and x < pos.x) {
                    return true;
                }
            }
        }
        return false;
    };

    // Rooms within radius of (x, y), each counted once even if
    // it covers several of the neighboring tiles.
    auto score_neighbors = [&](int x, int y, int radius, auto room_weight) {
        Buffer<Room*, 49> seen;
        int score = 0;
        for (int dx = -radius; dx <= radius; ++dx) {
            for (int dy = -radius; dy <= radius; ++dy) {
                const int xx = x + dx;
                const int yy = y + dy;
                if ((dx == 0 and dy == 0) or xx < 0 or yy < 0 or xx > 15 or
                    yy > 15) {
                    continue;
                }
                auto room = APP.opponent_island()->get_room({u8(xx), u8(yy)});
                if (not room) {
                    continue;
                }
                bool dup = false;
                for (auto r : seen) {
                    if (r == room) {
                        dup = true;
                    }
                }
                if (dup or seen.size() == seen.capacity()) {
                    continue;
                }
                seen.push_back(room);
                score += room_weight(room);
            }
        }
        return score;
    };

    auto place = [&](const char* name, int radius, auto room_weight) {
        auto& mt = require_metaclass(name);
        if (power_remaining() <= mt->consumes_power()) {
            return;
        }

        int best_score = 0;
        RoomCoord best{};

        for (u8 x = 1; x < levelgen_size_.x; ++x) {
            for (u8 y = construction_zone_min_y; y < 15; ++y) {
                // Cheap checks first: this runs once per tile on the GBA.
                if (not has_space({x, y}, {1, 1})) {
                    continue;
                }

                // Only tiles that generate_foundation() would otherwise fill:
                // a room directly above, and (via blocks_weapon) not in front
                // of a cannon or above a launcher.
                if (y == 0 or
                    not APP.opponent_island()->get_room({x, u8(y - 1)})) {
                    continue;
                }

                if (blocks_weapon(x, y)) {
                    continue;
                }

                const int effect = score_neighbors(x, y, radius, room_weight);
                if (effect == 0) {
                    continue;
                }

                // Break ties toward tiles further from the player.
                if (effect > best_score or
                    (effect == best_score and x > best.x)) {
                    best_score = effect;
                    best = {x, y};
                }
            }
        }

        if (best_score > 0) {
            mt->create(APP.opponent_island(), best);
        }
    };

    for (int i = 0; i < amplifier_count; ++i) {
        // Speeds up recharge for nearby blocks: put it next to weapons.
        place("amplifier", amplifier_radius, [&](Room* room) {
            if ((*room->metaclass())->category() == Room::Category::weapon or
                is_launcher(room->name())) {
                return 1;
            }
            return 0;
        });
    }

    for (int i = 0; i < deflector_count; ++i) {
        // Reduces damage to nearby blocks: protect power and weapons.
        place("deflector", deflector_radius, [&](Room* room) {
            if ((*room->metaclass())->category() == Room::Category::power) {
                return 3;
            }
            if ((*room->metaclass())->category() == Room::Category::weapon or
                is_launcher(room->name())) {
                return 2;
            }
            return 0;
        });
    }
}



void ProcgenEnemyAI::generate_foundation()
{
    // Generate filler structures to ensure that no rooms are floating.

    auto& mt = require_metaclass("masonry");
    auto& dynamite = require_metaclass("dynamite");
    auto& dynamite_ii = require_metaclass("dynamite-ii");

    for (u8 x = 0; x < (int)APP.opponent_island()->terrain().size(); ++x) {
        for (u8 y = 0; y < 15; ++y) {
            if (APP.opponent_island()->rooms_view().get(x, y)) {
                if (auto room = APP.opponent_island()->get_room({x, y})) {
                    if (is_forcefield(room->metaclass())) {
                        // Don't put foundation blocks beneath a forcefield
                        continue;
                    }
                }
                for (u8 yy = y; yy < 15; ++yy) {
                    if (not APP.opponent_island()->rooms_view().get(x, yy)) {
                        if (auto room = APP.opponent_island()->get_room(
                                {u8(x + 1), yy})) {
                            if ((*room->metaclass())->category() ==
                                    Room::Category::weapon and
                                not str_eq(room->name(), "missile-silo") and
                                not str_eq(room->name(), "rocket-bomb")) {
                                // Don't put a foundation block to the left of a
                                // cannon-type weapon.
                                continue;
                            }
                        }
                        if (auto room = APP.opponent_island()->get_room(
                                {x, u8(yy + 1)})) {
                            if (str_eq(room->name(), "drone-bay")) {
                                continue;
                            }
                        }
                        if (rng::choice<18>(rng_source_) == 0) {
                            dynamite->create(APP.opponent_island(), {x, yy});
                        } else if (rng::choice<40>(rng_source_) == 0) {
                            dynamite_ii->create(APP.opponent_island(), {x, yy});
                        } else {
                            mt->create(APP.opponent_island(), {x, yy});
                            if (auto r =
                                    APP.opponent_island()->get_room({x, yy})) {
                                if (auto m = r->cast<Masonry>()) {
                                    if (rng::choice<4>(rng_source_) == 0) {
                                        m->set_gfx(2);
                                    } else if (rng::choice<11>(rng_source_) ==
                                               0) {
                                        m->set_gfx(3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}



void ProcgenEnemyAI::cleanup_unused_terrain()
{
    while (not APP.opponent_island()->terrain().empty()) {
        const u8 x_end = APP.opponent_island()->terrain().size() - 1;

        for (u8 y = 0; y < 16; ++y) {
            if (APP.opponent_island()->get_room({x_end, y})) {
                // Column contains a room, i.e. not unused.
                return;
            }
        }

        // Empty column, reduce terrain size by 1.
        APP.opponent_island()->init_terrain(x_end);
    }
}



bool ProcgenEnemyAI::place_room_random_loc(int x_start, const char* room_name)
{
    auto mt = load_metaclass(room_name);

    if (not mt) {
        Platform::fatal(format("procgen ai: % missing!", room_name).c_str());
    }

    auto make_room = [&](const RoomCoord& position) {
        (*mt)->create(APP.opponent_island(), position);
    };

    int tries = 0;
    while (tries < 255) {
        u8 x = x_start + rng::choice((levelgen_size_.x - x_start), rng_source_);
        u8 y = 14 - rng::choice(levelgen_size_.y, rng_source_);

        if (has_space({x, y}, (*mt)->constructed_size())) {
            make_room({x, y});
            return true;
        }

        ++tries;
    }

    return false;
}



bool ProcgenEnemyAI::has_space(const RoomCoord& loc, const RoomCoord& sz)
{
    for (u8 x = 0; x < sz.x; ++x) {
        for (u8 y = 0; y < sz.y; ++y) {
            if (loc.y + y >= 15 or loc.x + x >= levelgen_size_.x) {
                return false;
            }
            if (APP.opponent_island()->rooms_view().get(loc.x + x, loc.y + y)) {
                return false;
            }
        }
    }

    return true;
}



} // namespace skyland
