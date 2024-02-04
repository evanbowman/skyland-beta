////////////////////////////////////////////////////////////////////////////////
//
// MIT License
//
// Copyright (c) 2020-2024 Evan Bowman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
////////////////////////////////////////////////////////////////////////////////


#include "gba_platform_audio.hpp"
#include "gba.h"
#include "platform/platform.hpp"
#include <array>
#include <limits>


////////////////////////////////////////////////////////////////////////////////
//
// Overview:
//
// GBA audio is quite interesting. On the one hand, the audio chip is very
// simple. The DirectSound hardware has a fifo, you put samples into the fifo at
// a steady rate, and the speaker plays the samples. On the other
// hand... because there is no sound mixing hardware, the CPU has to perform all
// sound mixing logic. Hence the large size of this file :)
//
// Sound mixing isn't too complex, you just simply add samples together. But
// once you want to start rewinding sounds and changing sound volume and rate,
// things start to get complicated :)
//
////////////////////////////////////////////////////////////////////////////////



static int audio_timer_frequency(int words_per_call)
{
    // NOTE: 0xffff represents the max timer value. The subtracted value
    // represents the number of timer tics to wait before running the irq
    // again. Upon each tic of the timer, the direct sound chip loads one sample
    // from audio FIFO A. The fifo is a 32bit register, so we write four samples
    // to the fifo at a time.
    return 0xffff - ((4 * words_per_call) - 1);
}



namespace detail
{
template <std::size_t... Is> struct seq
{
};
template <std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...>
{
};
template <std::size_t... Is> struct gen_seq<0, Is...> : seq<Is...>
{
};



template <class Generator, std::size_t... Is>
constexpr auto generate_array_helper(Generator g, seq<Is...>)
    -> std::array<decltype(g(std::size_t{}, sizeof...(Is))), sizeof...(Is)>
{
    return {{g(Is, sizeof...(Is))...}};
}

template <std::size_t tcount, class Generator>
constexpr auto generate_array(Generator g)
    -> decltype(generate_array_helper(g, gen_seq<tcount>{}))
{
    return generate_array_helper(g, gen_seq<tcount>{});
}
} // namespace detail



constexpr auto make_volume_lut(float scale)
{
    return detail::generate_array<256>(
        [scale](std::size_t curr, std::size_t total) -> s8 {
            const auto real = (s8)((u8)curr);
            return real * scale;
        });
}



// Each table entry contains the whole number space of a signed 8-bit value,
// scaled by a fraction.
static constexpr std::array<VolumeScaleLUT, 20> volume_scale_LUTs = {
    {{make_volume_lut(0.05f)}, {make_volume_lut(0.10f)},
     {make_volume_lut(0.15f)}, {make_volume_lut(0.20f)},
     {make_volume_lut(0.25f)}, {make_volume_lut(0.30f)},
     {make_volume_lut(0.35f)}, {make_volume_lut(0.40f)},
     {make_volume_lut(0.45f)}, {make_volume_lut(0.50f)},
     {make_volume_lut(0.55f)}, {make_volume_lut(0.60f)},
     {make_volume_lut(0.65f)}, {make_volume_lut(0.70f)},
     {make_volume_lut(0.75f)}, {make_volume_lut(0.80f)},
     {make_volume_lut(0.85f)}, {make_volume_lut(0.90f)},
     {make_volume_lut(0.95f)}, {make_volume_lut(1.0f)}},
};



static const VolumeScaleLUT* music_volume_lut = &volume_scale_LUTs[19];
static const VolumeScaleLUT* sound_volume_lut = &volume_scale_LUTs[19];



__attribute__((section(".iwram"), long_call)) void audio_update_fast_isr();



// Global sound variables:
SoundContext snd_ctx;
Buffer<const char*, 4> completed_sounds_buffer;
volatile bool completed_sounds_lock = false;
const char* completed_music;
static Buffer<ActiveSoundInfo, 3> sound_stash;
EWRAM_DATA static volatile bool audio_update_swapflag;
EWRAM_DATA static void (*audio_update_current_isr)() = audio_update_fast_isr;
EWRAM_DATA static u8 audio_update_current_freq;
EWRAM_DATA static u8 audio_update_new_freq;



#define SOUND_MIXER_CALLBACK(NAME, RATE)                                       \
    const SoundMixerCallback NAME##_cb                                         \
    {                                                                          \
        NAME##_isr, RATE                                                       \
    }



////////////////////////////////////////////////////////////////////////////////
//
// Sound mixing functions:
//
// The sound engine registers timer interrupt handlers, which are responsible
// for performing the mixing and writing samples to the sound chip.
//
////////////////////////////////////////////////////////////////////////////////



SOUND_MIXER_CALLBACK(audio_update_fast, 2);



static void audio_update_halfspeed_isr()
{
    // NOTE: rather than change the logic for indices into the music track, I
    // just exploit the unused depth of the sound fifo and run the isr half the
    // time.
    alignas(4) AudioSample mixing_buffer[4];

    // NOTE: audio tracks in ROM should therefore have four byte alignment!
    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos++];

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length)) {
        snd_ctx.music_track_pos = 0;
        completed_music = snd_ctx.music_track_name;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ + 4 >= it->length_)) {
            if (not completed_sounds_lock) {
                completed_sounds_buffer.push_back(it->name_);
            }
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[i] += (u8)it->data_[it->position_];
                ++it->position_;
            }
            ++it;
        }
    }

    alignas(4) AudioSample mixing_buffer_out_1[4];
    alignas(4) AudioSample mixing_buffer_out_2[4];

    mixing_buffer_out_1[0] = mixing_buffer[0];
    mixing_buffer_out_1[1] = mixing_buffer[0];

    mixing_buffer_out_1[2] = mixing_buffer[1];
    mixing_buffer_out_1[3] = mixing_buffer[1];

    mixing_buffer_out_2[0] = mixing_buffer[2];
    mixing_buffer_out_2[1] = mixing_buffer[2];

    mixing_buffer_out_2[2] = mixing_buffer[3];
    mixing_buffer_out_2[3] = mixing_buffer[3];

    REG_SGFIFOA = *((u32*)mixing_buffer_out_1);
    REG_SGFIFOA = *((u32*)mixing_buffer_out_2);
}
SOUND_MIXER_CALLBACK(audio_update_halfspeed, 2);



static void audio_update_music_volume_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // NOTE: audio tracks in ROM should therefore have four byte alignment!
    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos++];

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length)) {
        snd_ctx.music_track_pos = 0;
        completed_music = snd_ctx.music_track_name;
    }

    for (AudioSample& s : mixing_buffer) {
        s = (*music_volume_lut)[s];
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {

        auto pos = it->position_;
        it->position_ += 4;

        if (UNLIKELY(it->position_ >= it->length_)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            auto buf = mixing_buffer;
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos++]];
            *(buf++) += (*sound_volume_lut)[(u8)it->data_[pos]];
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_music_volume, 1);



static void audio_update_slow_rewind_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos];

    std::swap(mixing_buffer[0], mixing_buffer[3]);
    std::swap(mixing_buffer[1], mixing_buffer[2]);

    snd_ctx.music_track_pos -= 1;
    if (snd_ctx.music_track_pos < 1) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 4 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (u8)it->data_[it->position_];
                it->position_ -= 1;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_slow_rewind_music, 1);



static void audio_update_rewind_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];
    alignas(4) AudioSample mixing_buffer2[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 2];

    *((u32*)mixing_buffer2) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 1];

    // NOTE: we're dropping half of the samples and putting the other half into
    // the ouptut buffer in reverse order, to rewind the music at 2x speed. But
    // of course, we can't just drop the first half of the samples and keep the
    // second half, we drop every other note. mixing_buffer[2] happens to be in
    // the correct position already.
    mixing_buffer[3] = mixing_buffer[0];
    mixing_buffer[0] = mixing_buffer2[2];
    mixing_buffer[1] = mixing_buffer2[0];

    snd_ctx.music_track_pos -= 2;
    if (snd_ctx.music_track_pos < 2) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 8 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (u8)it->data_[it->position_];
                it->position_ -= 2;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind_music, 1);



static void audio_update_rewind4x_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // Four-times rewind speed. Pick the first byte of the prior four words.
    mixing_buffer[0] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[1] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[2] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);
    mixing_buffer[3] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos--);

    if (snd_ctx.music_track_pos < 4) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 16 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (s8)it->data_[it->position_];
                it->position_ -= 4;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind4x_music, 1);



static void audio_update_rewind8x_music_isr()
{
    alignas(4) AudioSample mixing_buffer[4];

    // Four-times rewind speed. Pick the first byte of the prior four words.
    mixing_buffer[0] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[1] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[2] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;
    mixing_buffer[3] =
        *(s8*)(((u32*)(snd_ctx.music_track)) + snd_ctx.music_track_pos);
    snd_ctx.music_track_pos -= 2;

    if (snd_ctx.music_track_pos < 8) {
        snd_ctx.music_track_pos = snd_ctx.music_track_length;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ == 0)) {
            it->position_ = it->length_ - 1;
            ++it;
        } else if (UNLIKELY(it->position_ - 32 <= 0)) {
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[3 - i] += (s8)it->data_[it->position_];
                it->position_ -= 8;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_rewind8x_music, 1);



static void audio_update_doublespeed_isr()
{
    alignas(4) AudioSample mixing_buffer[4];
    alignas(4) AudioSample mixing_buffer2[4];

    *((u32*)mixing_buffer) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 2];

    *((u32*)mixing_buffer2) =
        ((u32*)(snd_ctx.music_track))[snd_ctx.music_track_pos - 1];

    mixing_buffer[1] = mixing_buffer[2];
    mixing_buffer[2] = mixing_buffer2[0];
    mixing_buffer[3] = mixing_buffer2[2];


    snd_ctx.music_track_pos += 2;

    if (UNLIKELY(snd_ctx.music_track_pos > snd_ctx.music_track_length + 2)) {
        snd_ctx.music_track_pos = 0;
        completed_music = snd_ctx.music_track_name;
    }

    for (auto it = snd_ctx.active_sounds.begin();
         it not_eq snd_ctx.active_sounds.end();) {
        if (UNLIKELY(it->position_ + 8 >= it->length_)) {
            if (not completed_sounds_lock) {
                completed_sounds_buffer.push_back(it->name_);
            }
            it = snd_ctx.active_sounds.erase(it);
        } else {
            for (int i = 0; i < 4; ++i) {
                mixing_buffer[i] += (u8)it->data_[it->position_];
                it->position_ += 2;
            }
            ++it;
        }
    }

    REG_SGFIFOA = *((u32*)mixing_buffer);
}
SOUND_MIXER_CALLBACK(audio_update_doublespeed, 1);



static void audio_update_swap_isr()
{
    audio_update_current_isr();

    // If the new isr fires less often than the replacement, fill the audio fifo
    // accordingly.
    const s8 gap_fill = (audio_update_new_freq - audio_update_current_freq) - 1;
    for (s8 i = 0; i < gap_fill; ++i) {
        audio_update_current_isr();
    }

    audio_update_swapflag = true;
}



void audio_update_swap(const SoundMixerCallback& cb)
{
    audio_update_swapflag = false;

    audio_update_new_freq = cb.output_words_per_callback_;

    irqSet(IRQ_TIMER1, audio_update_swap_isr);

    // We have to poll on a flag, because we want to change the timer frequency
    // in some cases. We can't write a new value to the timer configuration
    // register until just after it's fired, or sound stuff could get out of
    // sync.
    while (not audio_update_swapflag)
        ;

    audio_update_current_freq = audio_update_new_freq;

    REG_TM1CNT_L = audio_timer_frequency(cb.output_words_per_callback_);
    irqSet(IRQ_TIMER1, cb.isr_);
    audio_update_current_isr = cb.isr_;
}



////////////////////////////////////////////////////////////////////////////////
//
// Sound Data:
//
// All sound data is encoded as raw headerless signed 8-bit mono PCM samples.
// I manually gain-adjusted each sample to prevent clipping.
//
////////////////////////////////////////////////////////////////////////////////



#define STR(X) #X



#define DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, __DIV__)                       \
    {                                                                          \
        STR(__STR_NAME__), (AudioSample*)__TRACK_NAME__,                       \
            __TRACK_NAME__##Len / __DIV__                                      \
    }



#define DEF_MUSIC(__STR_NAME__, __TRACK_NAME__)                                \
    DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, 4)



#define DEF_SOUND(__STR_NAME__, __TRACK_NAME__)                                \
    DEF_AUDIO(__STR_NAME__, __TRACK_NAME__, 1)



#include "data/music_box.hpp"
#include "data/music_life_in_silco.hpp"
#include "data/music_rain.hpp"
#include "data/music_sb_solecism.hpp"
#include "data/music_unaccompanied_wind.hpp"
#include "data/shadows.hpp"



static const int null_music_len = 280 / 4 * 2;
static const u32 null_music[null_music_len] = {0};



static const struct AudioTrack
{
    const char* name_;
    const AudioSample* data_;
    int length_; // NOTE: For music, this is the track length in 32 bit words,
                 // but for sounds, length_ reprepresents bytes.
} music_tracks[] = {
    DEF_MUSIC(box, music_box),
    DEF_MUSIC(shadows, shadows),
    DEF_MUSIC(unaccompanied_wind, music_unaccompanied_wind),
    DEF_MUSIC(rain, music_rain),
    DEF_MUSIC(life_in_silco, music_life_in_silco),
    DEF_MUSIC(solecism, music_sb_solecism),
};



static const AudioTrack* find_music(const char* name)
{
    for (auto& track : music_tracks) {

        if (str_cmp(name, track.name_) == 0) {
            return &track;
        }
    }

    return nullptr;
}



#include "data/music_struttin.hpp"
#include "data/sound_archivist.hpp"
#include "data/sound_beep_error.hpp"
#include "data/sound_bell.hpp"
#include "data/sound_build0.hpp"
#include "data/sound_button_wooden.hpp"
#include "data/sound_cancel.hpp"
#include "data/sound_cannon.hpp"
#include "data/sound_click.hpp"
#include "data/sound_click_negative.hpp"
#include "data/sound_click_wooden.hpp"
#include "data/sound_cling.hpp"
#include "data/sound_coin.hpp"
#include "data/sound_core_destroyed.hpp"
#include "data/sound_creaking.hpp"
#include "data/sound_cursor_click.hpp"
#include "data/sound_digital_click_1.hpp"
#include "data/sound_door.hpp"
#include "data/sound_drone_beep.hpp"
#include "data/sound_explosion1.hpp"
#include "data/sound_explosion2.hpp"
#include "data/sound_fizzle.hpp"
#include "data/sound_footstep1.hpp"
#include "data/sound_footstep2.hpp"
#include "data/sound_footstep3.hpp"
#include "data/sound_glass_break.hpp"
#include "data/sound_gravel.hpp"
#include "data/sound_gust.hpp"
#include "data/sound_gust2.hpp"
#include "data/sound_insert_cart.hpp"
#include "data/sound_ion_cannon.hpp"
#include "data/sound_missile.hpp"
#include "data/sound_missile_explosion.hpp"
#include "data/sound_msg.hpp"
#include "data/sound_open_book.hpp"
#include "data/sound_openbag.hpp"
#include "data/sound_page_flip.hpp"
#include "data/sound_pong_blip1.hpp"
#include "data/sound_pong_blip2.hpp"
#include "data/sound_powerdown.hpp"
#include "data/sound_poweron.hpp"
#include "data/sound_scroll.hpp"
#include "data/sound_seagull_1.hpp"
#include "data/sound_seagull_2.hpp"
#include "data/sound_thunder_1.hpp"
#include "data/sound_thunder_2.hpp"
#include "data/sound_thunder_close_1.hpp"
#include "data/sound_transporter.hpp"
#include "data/sound_tw_bell.hpp"
#include "data/sound_typewriter.hpp"
#include "data/sound_weapon_target.hpp"



static const AudioTrack sounds[] = {
    DEF_SOUND(explosion1, sound_explosion1),
    DEF_SOUND(explosion2, sound_explosion2),
    DEF_SOUND(glass_break, sound_glass_break),
    DEF_SOUND(build0, sound_build0),
    DEF_SOUND(missile, sound_missile),
    DEF_SOUND(impact, sound_missile_explosion),
    DEF_SOUND(fizzle, sound_fizzle),
    DEF_SOUND(gravel, sound_gravel),
    DEF_SOUND(beep_error, sound_beep_error),
    DEF_SOUND(drone_beep, sound_drone_beep),
    DEF_SOUND(typewriter, sound_typewriter),
    DEF_SOUND(footstep1, sound_footstep1),
    DEF_SOUND(footstep2, sound_footstep2),
    DEF_SOUND(footstep3, sound_footstep3),
    DEF_SOUND(ion_cannon, sound_ion_cannon),
    DEF_SOUND(gust1, sound_gust),
    DEF_SOUND(gust2, sound_gust2),
    DEF_SOUND(openbag, sound_openbag),
    DEF_SOUND(tw_bell, sound_tw_bell),
    DEF_SOUND(click, sound_scroll),
    DEF_SOUND(cursor_tick, sound_cursor_click),
    // DEF_SOUND(click_negative, sound_click_negative),
    DEF_SOUND(click_wooden, sound_click_wooden),
    DEF_SOUND(button_wooden, sound_button_wooden),
    DEF_SOUND(click_digital_1, sound_digital_click_1),
    DEF_SOUND(cannon, sound_cannon),
    DEF_SOUND(cling, sound_cling),
    DEF_SOUND(weapon_target, sound_weapon_target),
    DEF_SOUND(transporter, sound_transporter),
    DEF_SOUND(thunder_1, sound_thunder_1),
    DEF_SOUND(thunder_2, sound_thunder_2),
    DEF_SOUND(thunder_close_1, sound_thunder_close_1),
    DEF_SOUND(core_destroyed, sound_core_destroyed),
    DEF_SOUND(pong_blip_1, sound_pong_blip1),
    DEF_SOUND(pong_blip_2, sound_pong_blip2),
    DEF_SOUND(struttin, music_struttin),
    DEF_SOUND(creaking, sound_creaking),
    DEF_SOUND(coin, sound_coin),
    DEF_SOUND(bell, sound_bell),
    DEF_SOUND(archivist, sound_archivist),
    DEF_SOUND(cancel, sound_cancel),
    DEF_SOUND(seagull_1, sound_seagull_1),
    DEF_SOUND(seagull_2, sound_seagull_2),
    DEF_SOUND(msg, sound_msg),
    DEF_SOUND(door, sound_door),
    DEF_SOUND(insert_cart, sound_insert_cart),
    DEF_SOUND(powerdown, sound_powerdown),
    DEF_SOUND(poweron, sound_poweron),
    DEF_SOUND(page_flip, sound_page_flip)};



static const AudioTrack* get_sound(const char* name)
{
    for (auto& sound : sounds) {
        if (str_cmp(name, sound.name_) == 0) {
            return &sound;
        }
    }
    return nullptr;
}



Microseconds Platform::Speaker::track_length(const char* name)
{
    if (const auto music = find_music(name)) {
        return (music->length_ * wordsize) / 0.016f;
    }

    if (const auto sound = get_sound(name)) {
        return sound->length_ / 0.016f;
    }

    return 0;
}



static Optional<ActiveSoundInfo> make_sound(const char* name)
{
    if (auto sound = get_sound(name)) {
        return ActiveSoundInfo{
            0, sound->length_, sound->data_, 0, sound->name_};
    } else {
        return {};
    }
}



// FIXME: comment out of date
// If you're going to edit any of the variables used by the interrupt handler
// for audio playback, you should use this helper function.
template <typename F> auto modify_audio(F&& callback)
{
    // irqDisable(IRQ_TIMER0);
    callback();
    // irqEnable(IRQ_TIMER0);
}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    bool playing = false;

    modify_audio([&] {
        for (const auto& info : snd_ctx.active_sounds) {
            if (str_eq(name, info.name_)) {
                playing = true;
                return;
            }
        }
    });

    return playing;
}



bool Platform::Speaker::is_music_playing(const char* name)
{
    bool playing = false;

    if (auto track = find_music(name)) {
        modify_audio([&] {
            if (track->data_ == snd_ctx.music_track) {
                playing = true;
            }
        });
    }

    return playing;
}



const char* Platform::Speaker::completed_music()
{
    auto ret = ::completed_music;
    ::completed_music = nullptr;
    return ret;
}



Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    while (completed_sounds_lock)
        ;

    Buffer<const char*, 4> result;

    completed_sounds_lock = true;

    result = completed_sounds_buffer;
    completed_sounds_buffer.clear();

    completed_sounds_lock = false;

    return result;
}



void Platform::Speaker::stop_sound(const char* name)
{
    modify_audio([&] {
        for (auto it = snd_ctx.active_sounds.begin();
             it not_eq snd_ctx.active_sounds.end();) {
            if (str_eq(name, it->name_)) {
                it = snd_ctx.active_sounds.erase(it);
            } else {
                ++it;
            }
        }
    });
}



void Platform::Speaker::clear_sounds()
{
    modify_audio([&] { snd_ctx.active_sounds.clear(); });
}


static void add_sound(Buffer<ActiveSoundInfo, 3>& sounds,
                      const ActiveSoundInfo& info)
{
    if (not sounds.full()) {
        sounds.push_back(info);
    } else {
        ActiveSoundInfo* lowest = sounds.begin();
        for (auto it = sounds.begin(); it not_eq sounds.end(); ++it) {
            if (it->priority_ < lowest->priority_) {
                lowest = it;
            }
        }

        if (lowest not_eq sounds.end() and lowest->priority_ < info.priority_) {
            sounds.erase(lowest);
            sounds.push_back(info);
        }
    }
}



static constexpr const unsigned int __snd_rates[13] = {
    0,
    8013, // C
    7566, // C#
    7144, // D
    6742, // D#
    6362, // E
    6005, // F
    5666, // F#
    5346, // G
    5048, // G#
    4766, // A
    4499, // A#
    4246, // B
};


// 131072/(2048-n)Hz
#define SND_RATE(note, oct) (2048 - (__snd_rates[note] >> ((2 + oct))))



static constexpr const struct NoiseFrequencyTableEntry
{
    u8 shift_;
    u8 ratio_;
} noise_frequency_table_[57] = {
    {0, 0},  {1, 0},  {2, 0},  {0, 3},  {3, 0},  {0, 5},  {1, 3},  {0, 7},
    {4, 0},  {1, 5},  {2, 3},  {1, 7},  {5, 0},  {2, 5},  {3, 3},  {2, 7},
    {6, 0},  {3, 5},  {4, 3},  {3, 7},  {7, 0},  {4, 5},  {5, 3},  {4, 7},
    {8, 0},  {5, 5},  {6, 3},  {5, 7},  {9, 0},  {6, 5},  {7, 3},  {6, 7},
    {10, 0}, {7, 5},  {8, 3},  {7, 7},  {11, 0}, {8, 5},  {9, 3},  {8, 7},
    {12, 0}, {9, 5},  {10, 3}, {9, 7},  {13, 0}, {10, 5}, {11, 3}, {10, 7},
    {11, 5}, {12, 3}, {11, 7}, {12, 5}, {13, 3}, {12, 7}, {13, 5}, {13, 7}};



struct AnalogChannel
{
    Platform::Speaker::Note last_note_;
    u8 last_octave_;
    Microseconds effect_timer_;
};



static EWRAM_DATA AnalogChannel analog_channel[4];



void Platform::Speaker::init_chiptune_square_1(ChannelSettings settings)
{
    REG_SND1CNT = SSQR_BUILD(settings.volume_,
                             settings.envelope_direction_,
                             settings.envelope_step_,
                             settings.duty_,
                             settings.length_);
}



void Platform::Speaker::init_chiptune_square_2(ChannelSettings settings)
{
    REG_SND2CNT = SSQR_BUILD(settings.volume_,
                             settings.envelope_direction_,
                             settings.envelope_step_,
                             settings.duty_,
                             settings.length_);
}



void Platform::Speaker::init_chiptune_wave(u16 config)
{
}



void Platform::Speaker::init_chiptune_noise(ChannelSettings settings)
{
    REG_SND4CNT = SSQR_BUILD(settings.volume_,
                             settings.envelope_direction_,
                             settings.envelope_step_,
                             settings.duty_,
                             settings.length_);
}



void Platform::Speaker::stop_chiptune_note(Channel channel)
{
    switch (channel) {
    case Channel::square_1:
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 8);
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xc);
        break;

    case Channel::square_2:
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 9);
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xd);
        break;

    case Channel::noise:
        // FIXME!?
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~SDMG_LNOISE;
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~SDMG_RNOISE;
        break;

    case Channel::wave:
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xb);
        REG_SNDDMGCNT = REG_SNDDMGCNT & ~(1 << 0xf);
        break;

    default:
        // TODO!
        break;
    }

    // Turn directsound back on!
    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 9);
    REG_SOUNDCNT_H = REG_SOUNDCNT_H | (1 << 8);
}



void Platform::Speaker::play_chiptune_note(Channel channel, NoteDesc note_desc)
{
    auto note = note_desc.regular_.note_;
    u8 octave = note_desc.regular_.octave_;

    if (channel == Channel::noise and
        note_desc.noise_freq_.frequency_select_ == 0) {
        return;
    } else if (channel not_eq Channel::noise and
               ((u8)note >= (u8)Note::count or note == Note::invalid)) {
        return;
    }


    // Turn off directsound!
    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 8);
    REG_SOUNDCNT_H = REG_SOUNDCNT_H & ~(1 << 9);

    switch (channel) {
    case Channel::square_1:
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LSQR1;
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RSQR1;
        analog_channel[(int)channel].last_note_ = note;
        analog_channel[(int)channel].last_octave_ = octave;
        analog_channel[(int)channel].effect_timer_ = 0;
        REG_SND1FREQ = SFREQ_RESET | SND_RATE((u8)note, octave);
        break;

    case Channel::square_2:
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LSQR2;
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RSQR2;
        analog_channel[(int)channel].last_note_ = note;
        analog_channel[(int)channel].last_octave_ = octave;
        analog_channel[(int)channel].effect_timer_ = 0;
        REG_SND2FREQ = SFREQ_RESET | SND_RATE((u8)note, octave);
        break;

    case Channel::noise: {
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LNOISE;
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RNOISE;
        analog_channel[(int)channel].last_note_ = note;
        analog_channel[(int)channel].last_octave_ = octave;
        analog_channel[(int)channel].effect_timer_ = 0;
        auto freq = note_desc.noise_freq_.frequency_select_;
        auto entry = noise_frequency_table_[freq];
        REG_SND4FREQ = 0;
        REG_SND4FREQ = SFREQ_RESET | ((0x0f & entry.shift_) << 4) |
                       (0x07 & entry.ratio_) |
                       (note_desc.noise_freq_.wide_mode_ << 3);
        break;
    }

    case Channel::wave:
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_LWAVE;
        REG_SNDDMGCNT = REG_SNDDMGCNT | SDMG_RWAVE;
        analog_channel[(int)channel].last_note_ = note;
        analog_channel[(int)channel].last_octave_ = octave;
        analog_channel[(int)channel].effect_timer_ = 0;
        break;

    default:
        // TODO...
        break;
    }
}



void Platform::Speaker::apply_chiptune_effect(Channel channel,
                                              Effect effect,
                                              u8 argument,
                                              Microseconds delta)
{
    if (channel == Channel::invalid) {
        return;
    }


    const auto ch_num = (int)channel;


    auto apply_vibrato = [&](volatile u16* freq_register) {
        auto amplitude = argument & 0x0f;
        auto freq = (argument & 0xf0) >> 4;

        // We're using freq as a divisor. Zero isn't valid
        freq++;

        analog_channel[ch_num].effect_timer_ += delta;
        auto rate = SND_RATE(analog_channel[ch_num].last_note_,
                             analog_channel[ch_num].last_octave_);


        auto vib = float(cosine(analog_channel[ch_num].effect_timer_ / freq)) /
                   std::numeric_limits<s16>::max();

        vib *= (amplitude << 2);
        rate += vib;
        *freq_register = *freq_register & ~SFREQ_RATE_MASK;
        *freq_register = *freq_register | rate;
    };


    auto apply_duty = [&](volatile u16* ctrl_register) {
        *ctrl_register = *ctrl_register & ~SSQR_DUTY_MASK;
        // (Only four possible duty cycles, hence the mask)
        *ctrl_register = *ctrl_register | SSQR_DUTY((argument >> 4) & 0x03);
    };


    auto apply_envelope = [&](volatile u16* ctrl_register) {
        auto duty = (*ctrl_register & SSQR_DUTY_MASK) >> SSQR_DUTY_SHIFT;
        auto length = (*ctrl_register & SSQR_LEN_MASK) >> SSQR_LEN_SHIFT;

        // To match LSDJ: first nibble: volume, second nibble: 1-7: release with
        // decreasing envelope, 8-f: release with increasing envelope.

        int dir = 0;

        if ((argument & 0x0f) < 8) {
            dir = 0; // decreasing
        } else {
            dir = 1;
        }

        *ctrl_register = SSQR_BUILD(
            (argument & 0xf0) >> 4, dir, (argument & 0x07), duty, length);
    };


    auto cancel_effect = [&](volatile u16* freq_register) {
        *freq_register = *freq_register & ~SFREQ_RATE_MASK;
        *freq_register =
            *freq_register | SND_RATE(analog_channel[ch_num].last_note_,
                                      analog_channel[ch_num].last_octave_);

        analog_channel[ch_num].effect_timer_ = 0;
    };


    switch (channel) {
    case Channel::square_1: {
        switch (effect) {
        case Effect::vibrato:
            apply_vibrato(&REG_SND1FREQ);
            break;

        case Effect::none:
            cancel_effect(&REG_SND1FREQ);
            break;

        case Effect::duty:
            apply_duty(&REG_SND1CNT);
            break;

        case Effect::envelope:
            apply_envelope(&REG_SND1CNT);
            break;
        }
        break;
    }

    case Channel::square_2:
        switch (effect) {
        case Effect::vibrato:
            apply_vibrato(&REG_SND2FREQ);
            break;

        case Effect::none:
            cancel_effect(&REG_SND2FREQ);
            break;

        case Effect::duty:
            apply_duty(&REG_SND2CNT);
            break;

        case Effect::envelope:
            apply_envelope(&REG_SND2CNT);
            break;
        }
        break;

    case Channel::noise:
        switch (effect) {
        case Effect::duty:
            apply_duty(&REG_SND4CNT);
            break;

        case Effect::envelope:
            apply_envelope(&REG_SND4CNT);
            break;

        default:
            break;
        }
        break;

    default:
        // TODO...
        break;
    }
}



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
    (void)position; // We're not using position data, because on the gameboy
                    // advance, we aren't supporting spatial audio.

    if (auto info = make_sound(name)) {
        info->priority_ = priority;

        modify_audio([&] { add_sound(snd_ctx.active_sounds, *info); });
    }
}



static void clear_music()
{
    // The audio interrupt handler can be smaller and simpler if we use a track
    // of empty bytes to represent scenarios where music is not playing, rather
    // than adding another if condition to the audio isr.
    snd_ctx.music_track = reinterpret_cast<const AudioSample*>(null_music);
    snd_ctx.music_track_length = null_music_len - 1;
    snd_ctx.music_track_name = "(null)";
    snd_ctx.music_track_pos = 0;
}



static void stop_music()
{
    modify_audio([] { clear_music(); });
}



void Platform::Speaker::stop_music()
{
    ::stop_music();
}



static void play_music(const char* name, Microseconds offset)
{
    const auto track = find_music(name);
    if (track == nullptr) {
        return;
    }

    const Microseconds sample_offset = offset * 0.016f; // NOTE: because 16kHz

    modify_audio([&] {
        snd_ctx.music_track_length = track->length_;
        snd_ctx.music_track = track->data_;
        snd_ctx.music_track_pos = (sample_offset / 4) % track->length_;
        snd_ctx.music_track_name = name;
    });
}



void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
    // NOTE: The sound sample needs to be mono, and 8-bit signed. To export this
    // format from Audacity, convert the tracks to mono via the Tracks dropdown,
    // and then export as raw, in the format 8-bit signed.
    //
    // Also, important to convert the sound file frequency to 16kHz.

    this->stop_music();

    ::play_music(name, offset);

    // FIXME!!!!!! Mysteriously, there's a weird audio glitch, where the sound
    // effects, but not the music, get all glitched out until two sounds are
    // played consecutively. I've spent hours trying to figure out what's going
    // wrong, and I haven't solved this one yet, so for now, just play a couple
    // quiet sounds. To add further confusion, after adjusting the instruction
    // prefetch and waitstats, I need to play three sounds
    // consecutively... obviously my interrupt service routine for the audio is
    // flawed somehow. Do I need to completely disable the timers and sound
    // chip, as well as the audio interrupts, when playing new sounds? Does
    // disabling the audio interrupts when queueing a new sound effect cause
    // audio artifacts, because the sound chip is not receiving samples?
    play_sound("footstep1", 0);
    play_sound("footstep2", 0);
    play_sound("footstep3", 0);

    // auto tmp = allocate_dynamic<OptDmaBufferData>("test");
    // auto before = __platform__->delta_clock().sample();
    // // audio_update_fast_isr();

    // alignas(4) u16 tmp2[162];

    // win_circle(tmp->data(),
    //            0,
    //            0,
    //            156);

    // auto after = __platform__->delta_clock().sample();
    // Platform::fatal(format("dt %", after - before));
}



Platform::Speaker::Speaker()
{
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
    if (__platform__->network_peer().is_connected()) {
        audio_update_swap(audio_update_fast_cb);
        return;
    }

    switch (speed) {
    default:
    case MusicSpeed::regular:
        audio_update_swap(audio_update_fast_cb);
        break;

    case MusicSpeed::doubled:
        audio_update_swap(audio_update_doublespeed_cb);
        break;

    case MusicSpeed::reversed:
        audio_update_swap(audio_update_rewind_music_cb);
        break;

    case MusicSpeed::reversed4x:
        audio_update_swap(audio_update_rewind4x_music_cb);
        break;

    case MusicSpeed::reversed8x:
        audio_update_swap(audio_update_rewind8x_music_cb);
        break;

    case MusicSpeed::reversed_slow:
        audio_update_swap(audio_update_slow_rewind_music_cb);
        break;

    case MusicSpeed::halved:
        audio_update_swap(audio_update_halfspeed_cb);
        break;
    }
}



void Platform::Speaker::set_music_volume(u8 volume)
{
    if (volume >= volume_scale_LUTs.size()) {
        return;
    }

    if (volume == volume_scale_LUTs.size() - 1) {
        // Ok, so... I'm aware that the platform header would appear to support
        // changing sound effect volume without modifying music volume. Maybe
        // I'll fix it sometime... but for now, sound volume may only be
        // adjusted when music volume is not at maximum. I just never had a
        // reason to fade sounds without fading music as well.
        //
        // Maybe, I'll fix it sometime. But my future plans for this project,
        // after the gba release, just involve targetting other platforms, so
        // eventually, this defficiency won't matter anymore when this file
        // becomes legacy code.
        //
        // Modifying sound/music volume in a timer irq is a heavy operation, so
        // I have no real incentive to make the sound volume behave as expected
        // unless I actually need the behavior for implementing features.
        audio_update_swap(audio_update_fast_cb);
    } else {
        music_volume_lut = &volume_scale_LUTs[volume];
        audio_update_swap(audio_update_music_volume_cb);
    }
}



void Platform::Speaker::set_sounds_volume(u8 volume)
{
    if (volume >= volume_scale_LUTs.size()) {
        return;
    }

    sound_volume_lut = &volume_scale_LUTs[volume];
}



void Platform::Speaker::stash_sounds()
{
    sound_stash.clear();

    modify_audio([&] {
        sound_stash = snd_ctx.active_sounds;
        snd_ctx.active_sounds.clear();
    });
}



void Platform::Speaker::restore_sounds()
{
    modify_audio([&] { snd_ctx.active_sounds = sound_stash; });
}



void audio_start()
{
    clear_music();

    // REG_SOUNDCNT_H =
    //     0x0B0D | SDS_DMG100; //DirectSound A + fifo reset + max volume to L and R
    REG_SOUNDCNT_X = 0x0080; //turn sound chip on

    REG_SOUNDCNT_H = SDS_DMG100 | 1 << 2 | 1 << 3 | 1 << 8 | 1 << 9;


    // Required for stereo, currently unused.
    // // Both direct sound channels, FIFO reset, A is R, B is L.
    // REG_SOUNDCNT_H = 0b1010100100001111;
    // REG_SOUNDCNT_X = 0x0080; //turn sound chip on

    audio_update_fast_isr();


    irqEnable(IRQ_TIMER1);
    irqSet(IRQ_TIMER1, audio_update_fast_isr);
    audio_update_current_freq = audio_update_fast_cb.output_words_per_callback_;

    REG_TM0CNT_L = 0xffff;
    REG_TM1CNT_L = audio_timer_frequency(audio_update_current_freq);

    // While it may look like TM0 is unused, it is in fact used for setting the
    // sample rate for the digital audio chip.
    REG_TM0CNT_H = 0x0083;
    REG_TM1CNT_H = 0x00C3;


    // turn sound on
    REG_SNDSTAT = SSTAT_ENABLE;

    // on left/right ; both full volume
    REG_SNDDMGCNT =
        SDMG_BUILD_LR(SDMG_SQR1 | SDMG_SQR2 | SDMG_WAVE | SDMG_NOISE, 7);

    // no sweep
    REG_SND1SWEEP = SSW_OFF;

    // envelope: vol=12, decay, max step time (7) ; 50% duty
    REG_SND1CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_2;
    REG_SND1FREQ = 0;

    REG_SND2CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_4;
    REG_SND2FREQ = 0;

    REG_SND4CNT = SSQR_ENV_BUILD(12, 0, 7) | SSQR_DUTY1_4;
    REG_SND4FREQ = 0;
}



void Platform::Speaker::start()
{
    audio_start();
    clear_music();
    play_music("unaccompanied_wind", 0);
}
