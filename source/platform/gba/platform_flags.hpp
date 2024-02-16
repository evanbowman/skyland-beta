#pragma once



enum class GlobalFlag {
    rtc_faulty,
    gbp_unlocked,
    multiplayer_connected,
    save_using_flash,
    glyph_mode,
    parallax_clouds,
    v_parallax,
    partial_palette_sync,
    palette_sync,
    sound_startup_monkeypatch,
    key_poll_called,
    watchdog_disabled,
    effect_window_mode,
    iris_effect_mode,
    skyland_custom_mgba,
    count
};



void set_gflag(GlobalFlag f, bool val);



bool get_gflag(GlobalFlag f);
