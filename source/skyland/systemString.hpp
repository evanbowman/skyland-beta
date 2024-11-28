////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"



// All strings required by the engine. We store the strings themselves in a
// separate file, for localization purposes.



namespace skyland
{



void systemstring_drop_index_cache();



void systemstring_bind_file(const char* path);
const char* systemstring_bound_file();



enum class SystemString {
    empty,
    game_title,

    yes,
    no,

    mt_title,
    mt_hint,
    mt_co_op,
    mt_vs,
    mt_game_mode,
    mt_prep_seconds,
    mt_unhide_prep,
    mt_coins,
    mt_terrain_size,
    mt_waiting,
    mt_timeout_freq,
    mt_timeout_duration,

    block_air,
    block_amplifier,
    block_annihilator,
    block_arcgun,
    block_arch,
    block_backup_core,
    block_balloon,
    block_banana_plant,
    block_barrier,
    block_basalt,
    block_basalt_brick,
    block_basalt_carved,
    block_beam_gun,
    block_big_hull,
    block_war_engine,
    block_bridge,
    block_bronze_hull,
    block_building,
    block_carved_hematite,
    block_carved_masonry,
    block_chaos_core,
    block_cloak,
    block_cocoa,
    block_command_module,
    block_clump_missile,
    block_cesium,
    block_crane,
    block_crops_rotten,
    block_carved_crystal,
    block_crystal,
    block_crystal_pillar,
    block_dome,
    block_snow,
    block_stacked_hull,
    block_mirror_hull,
    block_market_stall,
    block_bulkhead_door,
    block_cannon,
    block_cargo_bay,
    block_power_core,
    block_portal,
    block_decimator,
    block_deflector,
    block_drone_bay,
    block_escape_beacon,
    block_farmhouse,
    block_flak_gun,
    block_fire_charge,
    block_food,
    block_forcefield,
    block_forcefield2,
    block_fountain,
    block_gold,
    block_granary,
    block_harbor,
    block_hematite,
    block_hematite_pillar,
    block_homing_missile,
    block_honey,
    block_hull,
    block_ice,
    block_incinerator,
    block_indigo,
    block_infirmary,
    block_ion_cannon,
    block_ion_fizzler,
    block_ladder,
    block_ladder_plus,
    block_lady_liberty,
    block_lava,
    block_lava_source,
    block_lemon_tree,
    block_light_source,
    block_lumber,
    block_madder,
    block_manufactory,
    block_marble,
    block_masonry,
    block_missile_silo,
    block_mycelium,
    block_nemesis,
    block_ocher,
    block_palm,
    block_pearls,
    block_piston,
    block_plundered_room,
    block_potatoes,
    block_energized_hull,
    block_qr,
    block_radar,
    block_radiator,
    block_reactor,
    block_replicator,
    block_rice,
    block_road_ns,
    block_road_we,
    block_road_hub,
    block_rocket_bomb,
    block_saffron,
    block_sand,
    block_scaffolding,
    block_shellfish,
    block_shrubbery,
    block_singularity,
    block_solar_cell,
    block_spark_cannon,
    block_speaker,
    block_stairwell,
    block_stairwell_plus,
    block_stairwell_plus_plus,
    block_statue,
    block_stone_pillar,
    block_sticky_piston,
    block_sunflower,
    block_switch,
    block_synth,
    block_targeting_computer,
    block_tea,
    block_terrain,
    block_tulips,
    block_dynamite_1,
    block_dynamite_2,
    block_torch,
    block_transporter,
    block_volcanic_soil,
    block_warhead,
    block_water,
    block_water_source,
    block_weather_engine,
    block_wheat,
    block_windmill,
    block_wool,
    block_workshop,
    block_generic,

    tiled_suffix,
    vines_suffix,
    brick_suffix,

    goodboy_suffix,
    pkmn_suffix,
    troll_suffix,
    sonic_suffix,

    red,
    black,

    bird_label,

    character_label_human,
    character_label_goblin,
    character_label_replicant,
    character_label_dog,

    menu_text_adventure,
    menu_text_challenge,
    menu_text_multiplayer,
    menu_text_extras,
    menu_text_macro,

    module_achievements,
    module_credits,
    module_update_loader,
    module_dlc_manager,
    module_factory_reset,
    module_file_browser,
    module_flag_designer,
    module_glossary,
    module_sandbox,
    module_skyland_forever,
    module_text_editor,
    module_highscores,
    module_tutorials,
    module_developer_mode,
    module_checkers,
    module_datetime,
    module_feedback,
    module_bug_report,
    module_cart_viewer,
    module_freebuild,
    module_regression,
    module_colormode,

    months,

    developer_mode_msg,
    developer_mode_msg_2,

    cargo,

    none,

    draw_path,

    construction_build,
    construction_add_terrain,
    construction_insufficient_funds,
    construction_insufficient_power_supply,
    construction_too_many_rooms,
    construction_not_enough_space,
    construciton_one_allowed,

    size,

    category_begin,
    category_wall = category_begin,
    category_weapon,
    category_factory,
    category_power,
    category_misc,
    category_decoration,


    intro_credits_name,
    intro_credits_cpy,

    macro_year,
    macro_create_block,
    macro_build_improvement,
    macro_demolish,
    macro_rotate,
    macro_raise,
    macro_layers,
    macro_visible_layers,
    macro_next_turn,
    macro_harvest_not_ready,
    macro_check_harvest,
    macro_enter,
    macro_create_colony,
    macro_fullsize_colony,
    macro_outpost_colony,
    macro_set_name,
    macro_colony_resources,
    macro_colony_cost,

    macro_help_prefix,
    macro_help_title_1,
    macro_help_title_2,
    macro_help_title_3,
    macro_help_title_4,
    macro_help_title_5,
    macro_help_1,
    macro_help_2,
    macro_help_3,
    macro_help_4,
    macro_help_5,
    macro_help_6,
    macro_help_7,
    macro_help_8,

    macro_trade,
    macro_abandon,
    macro_rename_island,

    macro_fiscal_budget,
    macro_fiscal_employed,
    macro_fiscal_unemployed,
    macro_fiscal_homelessness,
    macro_fiscal_total,
    macro_fiscal_starvation,
    macro_fiscal_happiness,
    macro_fiscal_unhappiness,

    macro_food_supply,
    macro_housing_scarcity,
    macro_population_density,
    macro_total_happiness,

    macro_population_growth,

    macro_cube,
    macro_pancake,
    macro_pillar,
    macro_wide,
    macro_deep,
    macro_superflat,


    macro_mode_lock,


    description_annihilator,
    description_crane,
    description_power_core,
    description_solar_cell,
    description_fire_charge,
    description_missile_silo,
    description_water,
    description_water_source,
    description_workshop,
    description_radar,
    description_ion_fizzler,
    description_lady_liberty,
    description_lava,
    description_lava_source,
    description_lemon_tree,
    description_speaker,
    description_shrubbery,
    description_ion_cannon,
    description_arc_gun,
    description_bronze_hull,
    description_stacked_hull,
    description_mirror_hull,

    description_nemesis,
    description_hull,
    description_forcefield,
    description_forcefield2,
    description_reactor,
    description_bulkhead_door,
    description_transporter,
    description_energized_hull,
    description_flak_gun,
    description_manufactory,
    description_masonry,
    description_statue,
    description_decimator,
    description_stairwell,
    description_plundered_room,
    description_synth,
    description_cargo_bay,
    description_cannon,
    description_radiator,
    description_switch,
    description_banana_plant,
    description_barrier,
    description_escape_beacon,
    description_mycelium,
    description_ice,
    description_basalt,
    description_torch,
    description_cesium,
    description_gold,
    description_rocket_bomb,
    description_weather_engine,
    description_targeting_computer,
    description_backup_core,
    description_ladder,
    description_infirmary,
    description_replicator,
    description_drone_bay,
    description_spark_cannon,
    description_qr,
    description_cloak,
    description_command_module,
    description_market_stall,
    description_incinerator,
    description_beam_gun,
    description_war_engine,
    description_windmill,
    description_clump_missile,
    description_ladder_plus,
    description_stairwell_plus,
    description_stairwell_plus_plus,
    description_balloon,
    description_warhead,
    description_homing_missile,
    description_canvas,
    description_deflector,
    description_portal,
    description_chaos_core,
    description_amplifier,

    gs_paused,
    gs_slow,
    gs_regular,
    gs_fast,
    gs_rewind,
    gs_error,
    gs_prompt,

    start_menu_resume,
    start_menu_glossary,
    start_menu_hibernate,
    start_menu_disable_rooms,
    start_menu_save_sandbox,
    start_menu_load_sandbox,
    start_menu_quit,
    start_menu_scuttle,
    start_menu_sky_map,
    start_menu_hint,
    start_menu_sandbox_help,
    start_menu_save,
    start_menu_load,
    start_menu_macroverse,
    start_menu_next_turn,
    start_menu_adjust_view,
    start_menu_newgame,
    start_menu_link,
    start_menu_share,
    start_menu_spectate,
    start_menu_continue_building,
    start_menu_repl,
    start_menu_end_run,
    start_menu_freebuild_samples,
    start_menu_freebuild_gen_terrain,

    are_you_sure,

    spectate_msg,

    macro_share_please_wait,

    salvage_prompt,
    salvage_option_A,
    salvage_option_B,
    salvage_error_populated,
    salvage_error_disallowed,
    salvage_drone,

    weather_clear,
    weather_overcast,
    weather_rain,
    weather_snow,
    weather_storm,
    weather_dust,
    weather_night,
    weather_solar_storm,

    move_room_prompt,
    move_room_1,
    move_room_2,

    select_rooms_prompt,

    achievement_msg_title,
    achievement_msg_unlocked,

    weapon_group_prompt,

    group_weapon_target,
    group_weapon_group,
    group_pick_group,

    deploy_drone_prompt,
    drone_position_prompt,
    automate_reconstruction_prompt_1,
    automate_reconstruction_prompt_2,

    highscores_title,
    highscores_score,
    highscores_upload,
    highscores_leaderboard,

    highscores_scan_qr_leaderboard,

    score_upload_enter_token,
    score_upload_prompt_1,
    score_upload_prompt_2,
    score_upload_prompt_3,

    a_next,
    qr_prep,

    modifier_keys_title,
    modifier_keys_opt_1,
    modifier_keys_opt_2,
    modifier_keys_opt_3,
    modifier_keys_opt_4,
    modifier_keys_opt_5,
    modifier_keys_opt_6,

    key_combo_prompt,

    level_complete_time,
    level_complete_pauses,
    level_complete_coins,
    level_complete_rooms,

    transporter_transport_char,
    transporter_recover_char,

    mind_control_prompt,

    repair_range,

    create_replicant,

    reset_sandbox_query,
    exit,

    switch_connect_on,
    switch_connect_off,

    on,
    off,

    zone_text,

    loading,

    wg_visited,
    wg_neutral,
    wg_hostile,
    wg_storm,
    wg_quest,
    wg_outpost,
    wg_uncharted,
    wg_quest_marker,
    wg_saved,
    wg_saved_description,
    wg_saved_come_back_soon,
    wg_title,
    wg_storm_label,
    wg_exit,
    wg_save_and_quit,
    wg_save_and_continue,

    sandbox_coins,
    sandbox_terrain_size,
    sandbox_music,
    sandbox_building_dependencies,
    sandbox_weather,
    sandbox_title,
    sandbox_prompt,
    sandbox_characters,

    sf_description,
    sf_hint,
    sf_title,
    sf_difficulty,
    sf_casual,
    sf_normal,
    sf_hard,
    sf_annihilation,

    setup,
    setup_instructions,

    difficulty_hint_easy,
    difficulty_hint_normal,
    difficulty_hint_hard,
    difficulty_hint_annihilation,

    autofire_setting,
    autofire_hint_on,
    autofire_hint_off,

    permadeath_setting,
    permadeath_hint_on,
    permadeath_hint_off,

    exit_tutorial,

    achievement_builder_name,
    achievement_builder_description,
    achievement_architect_name,
    achievement_architect_description,
    achievement_architect2_name,
    achievement_architect2_description,
    achievement_explorer_name,
    achievement_explorer_description,
    achievement_strategist_name,
    achievement_strategist_description,
    achievement_stronghold_name,
    achievement_stronghold_description,
    achievement_dynamite_name,
    achievement_dynamite_description,
    achievement_maestro1_name,
    achievement_maestro1_description,
    achievement_maestro2_name,
    achievement_maestro2_description,
    achievement_triage_name,
    achievement_triage_description,
    achievement_banana_man_name,
    achievement_banana_man_description,
    achievement_ancient_weapon_name,
    achievement_ancient_weapon_description,
    achievement_ship_of_theseus_name,
    achievement_ship_of_theseus_description,
    achievement_lemons_name,
    achievement_lemons_description,
    achievement_new_colossus_name,
    achievement_new_colossus_description,
    achievement_meltdown_name,
    achievement_meltdown_description,
    achievement_completionist_name,
    achievement_completionist_description,
    achievement_mycelium_name,
    achievement_mycelium_description,
    achievement_primitive_name,
    achievement_primitive_description,
    achievement_hero_name,
    achievement_hero_description,
    achievement_end_of_line_name,
    achievement_end_of_line_description,
    achievement_raid_name,
    achievement_raid_description,
    achievement_pacifist_name,
    achievement_pacifist_description,

    options,

    hint_title,
    hint_gamespeed,
    hint_infirmary,
    hint_navigation,
    hint_doors,
    hint_repair,
    hint_hotkeys,
    hint_glossary,
    hint_plunder,
    hint_damaged_core,
    hint_tutorials,
    hint_boss,
    hint_radar,

    glossary_filters,
    filter_begin,
    filter_req_workshop = filter_begin,
    filter_req_manufactory,
    filter_habitable,
    filter_ion_damage,
    filter_highly_flammable,
    filter_surface_weapons,
    filter_end,

    // Place no new enumerations here. (filter_end)

    no_dlc_prompt = filter_end,
    dlc_erase_hint,

    glossary_workshop_required,
    glossary_manufactory_required,
    glossary_not_constructible,

    error_power_out,
    error_friendly,
    error_no_more_pauses,
    error_powered_off,
    error_cannot_divert_power,

    easy_mode_auto_rewind_title,
    easy_mode_auto_rewind_text,

    factory_reset,

    dialog_datacarts_prompt,
    dialog_datacarts_return,
    dialog_tutorial_prompt,
    adventure_completed_message,

    multi_session_connecting,
    multi_connection_failure,

    misc_hibernate_message,
    misc_dlc_message,

    grav_collapse_started,
    grav_collapse_ended,

    freebuild_locked_text,
    freebuild_terrain_size,

    qr_code_size_error,
    qr_code_size_warning,

    error_link_nds,
    link_gba_setup,

    final_zone,

    checkers_ai_thinking,
    checkers_forced_jump,
    checker_wins,
    checkers_jump_again,

    name_chr,
    submit_hint,

    save_prompt,
    move_blocks_prompt,
    gamespeed_prompt,
    sel_menu_prompt,
    ok,
    do_not_show_again,

    retry,

    newgame,
    continue_game,

    repeat_query,

    sel_menu_view_interior,
    sel_menu_view_exterior,
    sel_menu_move_blocks,
    sel_menu_move_crewmember,
    sel_menu_name_crewmember,
    sel_menu_weapon_halt,
    sel_menu_weapon_target,
    sel_menu_salvage_block,
    sel_menu_pause,
    sel_menu_unpause,
    sel_menu_build,
    sel_menu_back,
    sel_menu_spook_bird,
    sel_menu_describe_block,
    sel_menu_edit_flag,
    sel_menu_crewmember_icon,
    sel_menu_upgrade_block,
    sel_menu_pin_crewmember,
    sel_menu_unpin_crewmember,
    sel_menu_poweron,
    sel_menu_powerdown,
    sel_menu_adjust_power,
    sel_menu_show_minimap,
    sel_menu_hide_minimap,
    sel_menu_select_all,
    sel_menu_crewmember_stats,

    crewmember_stats_title,
    crewmember_stats_title_ending,
    crewmember_stats_battles,
    crewmember_stats_vanquished,
    crewmember_stats_repaired,
    crewmember_stats_steps,
    crewmember_stats_fires,

    weapon_target_queue,

    choose_flag,
    flag_default,
    flag_alt1,
    flag_alt2,
    flag_alt3,
    flag_alt4,
    flag_alt5,
    flag_alt6,
    flag_banana,

    upgrade_prompt,
    upgrade_denied_manufactory,
    upgrade_denied_workshop,

    flag_designer_presets,
    flag_designer_flood,

    the_end,

    flag,
    flag_fmt,

    count,
};



using SystemStringBuffer = DynamicMemory<StringBuffer<1900>>;



SystemStringBuffer loadstr(SystemString str);



// Just a shortcut to save myself from having to type this all out hundreds of
// times.
#define SYS_CSTR(TAG) loadstr(SystemString::TAG)->c_str()
#define SYSTR(TAG) loadstr(SystemString::TAG)



} // namespace skyland
