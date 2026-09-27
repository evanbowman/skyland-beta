////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "visualizer.hpp"
#include "ext_workram_data.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



EXT_WORKRAM_DATA u8 visualizer_count = 0;



// Speaker/synth playback state. The PSG channels are shared hardware, so this
// is global rather than per-island: it reflects whatever the speaker blocks are
// currently sending to the sound chip.
struct PsgVisChannel
{
    Time elapsed_; // since note-on
    u8 band_;
    u8 volume_;   // volume at note-on, 0..15
    u8 env_step_; // 0 = envelope off
    u8 env_dir_;  // 0 = decrease, 1 = increase
    bool on_;
    bool noise_;
};



EXT_WORKRAM_DATA PsgVisChannel psg_vis_channels[4] = {};
EXT_WORKRAM_DATA Time psg_vis_idle = 0; // time since the last speaker step
EXT_WORKRAM_DATA bool psg_vis_playing = false;



// The PSG envelope clock runs at 64Hz; one envelope step lasts
// envelope_step_ * 1/64 seconds (in microseconds here).
static constexpr Time psg_envelope_tick = 15625;


// Semitone index (octave_ * 12 + note_ - 1) of the lowest band edge in
// spectrum_bands.py (--fmin 40 Hz, roughly E1). Assumes octave_ follows
// scientific pitch numbering, i.e. octave 4 contains middle C.
static constexpr int psg_band0_semitone = 16;


// Set to 12 if psg_play_note() plays the wave channel an octave below the
// square channels for the same note.
static constexpr int psg_wave_semitone_shift = 0;



static int psg_vis_clamp(int v, int lo, int hi)
{
    if (v < lo) {
        return lo;
    }
    if (v > hi) {
        return hi;
    }
    return v;
}



void Visualizer::psg_note_on(Platform::Speaker::Channel ch,
                             Platform::Speaker::NoteDesc note,
                             Platform::Speaker::ChannelSettings settings)
{
    psg_vis_playing = true;
    psg_vis_idle = 0;

    const int index = (int)ch;
    if (index < 0 or index >= 4) {
        return;
    }

    auto& c = psg_vis_channels[index];

    if (ch == Platform::Speaker::Channel::noise) {
        // ASSUMPTION: a larger frequency_select_ means a lower pitch, as with
        // the shift-clock bits of the GBA noise register. Noise is broadband,
        // so it only picks where in the upper bands the bars cluster.
        c.band_ = 7 - (note.noise_freq_.frequency_select_ >> 5);
        c.noise_ = true;
    } else {
        if (note.regular_.note_ == Platform::Speaker::Note::invalid) {
            // Rest: nothing is retriggered, so the previous note keeps
            // ringing out on its envelope.
            return;
        }

        int semis = note.regular_.octave_ * 12 + (note.regular_.note_ - 1);
        if (ch == Platform::Speaker::Channel::wave) {
            semis -= psg_wave_semitone_shift;
        }

        // The music bands are log-spaced, about 11.5 semitones apiece, so the
        // mapping from semitones to bands is linear.
        c.band_ = psg_vis_clamp((semis - psg_band0_semitone) * 2 / 23, 0, 7);
        c.noise_ = false;
    }

    c.elapsed_ = 0;
    c.volume_ = settings.volume_;
    c.env_step_ = settings.envelope_step_;
    c.env_dir_ = settings.envelope_direction_;
    c.on_ = true;
}



void Visualizer::psg_stop()
{
    psg_vis_playing = false;
    for (auto& c : psg_vis_channels) {
        c.on_ = false;
    }
}



static int psg_vis_level(const PsgVisChannel& c)
{
    if (not c.on_) {
        return 0;
    }

    if (c.env_step_ == 0) {
        return c.volume_;
    }

    const int steps = c.elapsed_ / (c.env_step_ * psg_envelope_tick);
    const int level = c.env_dir_ ? c.volume_ + steps : c.volume_ - steps;

    return psg_vis_clamp(level, 0, 15);
}



// Produces a slice in the same packed format as the .bands files.
static u32 psg_vis_packed()
{
    int levels[8] = {};

    auto raise = [&](int band, int level) {
        if (band >= 0 and band < 8 and level > levels[band]) {
            levels[band] = level;
        }
    };

    for (auto& c : psg_vis_channels) {
        const int level = psg_vis_level(c);
        if (level == 0) {
            continue;
        }

        raise(c.band_, level);

        if (c.noise_) {
            raise(c.band_ - 1, level - 3);
            raise(c.band_ + 1, level - 3);
        }
    }

    u32 packed = 0;
    for (int i = 0; i < 8; ++i) {
        packed |= u32(levels[i]) << (i * 4);
    }

    return packed;
}



void Visualizer::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_visualizer)->c_str();
}



Visualizer::Visualizer(Island* parent, const RoomCoord& position)
    : Canvas(parent, position, name())
{
    tile_ = Tile::visualizer;

    ++visualizer_count;
    if (visualizer_count > 1) {
        apply_damage(9999);
    }
}



Visualizer::~Visualizer()
{
    --visualizer_count;
}



void Visualizer::rewind(Time delta)
{
    Room::rewind(delta);

    update_simple(delta);
}



void Visualizer::update_simple(Time delta)
{
    u32 packed = 0;

    if (psg_vis_playing) {
        packed = psg_vis_packed();
    } else {
        u32 checksum = 0;
        auto current_music = PLATFORM.speaker().current_music();
        for (char c : current_music) {
            checksum += c;
        }

        if (last_music_checksum_ not_eq checksum) {
            last_music_checksum_ = checksum;

            // Remove extension.
            while (not current_music.empty() and
                   current_music[current_music.length() - 1] not_eq '.') {
                current_music.pop_back();
            }
            current_music.pop_back();

            auto fname = format("/scripts/data/music/%.bands", current_music);
            spectrum_file_ = PLATFORM.load_file("", fname.c_str());
        }

        if (not spectrum_file_.second) {
            return;
        }

        const u32* bands = (const u32*)spectrum_file_.first;
        const u32 slice_count = spectrum_file_.second / 4;

        u32 pos = PLATFORM.speaker().get_music_offset();
        u32 slice = pos >> 6; // (pos * 4 samples) / 256
        if (slice >= slice_count) {
            slice = slice_count - 1;
        }

        packed = bands[slice];
    }

    if (not bind_texture()) {
        return;
    }

    draw(packed);
}



void Visualizer::update(Time delta)
{
    Room::update(delta);
    Room::ready();

    if (psg_vis_playing) {
        // NOTE: This advances the shared PSG state, which is only correct
        // while there's at most one visualizer.
        psg_vis_idle += delta;
        for (auto& c : psg_vis_channels) {
            if (c.on_ and c.elapsed_ < milliseconds(2000)) {
                c.elapsed_ += delta;
            }
        }

        // Safety net, in case playback ended without a call to psg_stop().
        if (psg_vis_idle > milliseconds(1000)) {
            psg_stop();
        }
    }

    update_simple(delta);
}



bool Visualizer::bind_texture()
{
    if (img_data_) {
        return true;
    }

    if (canvas_texture_slot_ < 0) {
        canvas_texture_slot_ = alloc_canvas_texture(parent()->layer());
    }

    if (canvas_texture_slot_ < 0) {
        info("too many canvas textures!");
        apply_damage(Room::health_upper_limit());
        return false;
    }

    tile_ = Tile::canvas_tiles_begin + canvas_texture_slot_;
    img_data_ = alloc_img(parent()->layer());

    parent()->schedule_repaint();

    return true;
}



void Visualizer::draw(u32 packed)
{
    for (int i = 0; i < 8; ++i) {
        int level = (packed >> (i * 4)) & 0xF;
        int x;
        for (x = 1; x < level; ++x) {
            (**img_data_).set_pixel(i * 2, 15 - x, 11);
            (**img_data_).set_pixel(i * 2 + 1, 15 - x, 11);
        }
        for (; x < 15; ++x) {
            (**img_data_).set_pixel(i * 2, 15 - x, 1);
            (**img_data_).set_pixel(i * 2 + 1, 15 - x, 1);
        }
    }

    for (int x = 0; x < 16; ++x) {
        (**img_data_).set_pixel(x, 0, 3);
        (**img_data_).set_pixel(x, 15, 2);
    }
    publish_tiles();
}



} // namespace skyland
