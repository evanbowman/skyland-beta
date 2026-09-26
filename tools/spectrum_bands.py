#!/usr/bin/env python3
"""
spectrum_bands.py - precompute music-visualizer data for the GBA.

Input:  raw mono signed 8-bit PCM (e.g. 16384 Hz).
Output: raw binary file, one "slice" per `hop` input samples, back to back.

        Default (--bits 4): each band is a level 0..15, two bands per byte,
        low band in the low nibble. With 8 bands a slice is exactly one
        little-endian 32-bit word:

            band i = (word >> (4 * i)) & 0xF        (band 0 = lowest freq)

        With --bits 8: one byte per band, 0..255.

        Slice i describes the audio centred on input sample i * hop, so on
        the GBA:   slice_index = playback_sample_position / hop
        With --hop 256 that is just  pos >> 8.

Requires numpy.

NOTE:
python3 spectrum_bands.py ../scripts/data/music/shadows.raw ../scripts/data/music/shadows.bands --hop 256
"""

import argparse
import sys

import numpy as np


def stft_power(pcm, n_fft, hop, n_slices):
    """Power spectrum of a Hann-windowed frame centred on each sample i*hop."""
    pad = n_fft // 2
    padded = np.concatenate([np.zeros(pad, np.float32), pcm,
                             np.zeros(pad + hop, np.float32)])
    starts = np.arange(n_slices) * hop
    frames = np.lib.stride_tricks.sliding_window_view(padded, n_fft)[starts]
    window = np.hanning(n_fft).astype(np.float32)
    spec = np.fft.rfft(frames * window, axis=1)
    return spec.real ** 2 + spec.imag ** 2


def band_bins(n_bands, f_min, f_max, rate, n_fft):
    """Log-spaced bands, so each band covers roughly the same number of
    octaves (which is how our ears perceive pitch). Returns FFT bin ranges
    [lo, hi), each at least one bin wide."""
    n_bins = n_fft // 2 + 1
    edges = np.geomspace(f_min, f_max, n_bands + 1)
    idx = np.clip(np.round(edges * n_fft / rate).astype(int), 1, n_bins)
    for i in range(1, len(idx)):
        if idx[i] <= idx[i - 1]:
            idx[i] = idx[i - 1] + 1
    if idx[-1] > n_bins:
        sys.exit("error: too many bands for this FFT size; "
                 "increase --fft or raise --fmin")
    return list(zip(idx[:-1], idx[1:]))


def quantisation_noise_db(n_fft):
    """Expected per-bin power (dB) of 8-bit quantisation noise through a
    Hann-windowed FFT, for samples scaled to +-1. Nothing near this level is
    real signal."""
    step = 1.0 / 128.0
    return 10.0 * np.log10(step * step / 12.0 * 0.375 * n_fft)


def band_levels(power, bins, range_db, global_norm, max_boost_db, n_fft,
                decay, bits):
    """Per-slice band levels as integers 0 .. 2**bits - 1."""
    band_power = np.stack([power[:, lo:hi].mean(axis=1) for lo, hi in bins],
                          axis=1)
    db = 10.0 * np.log10(band_power + 1e-12)
    # A high percentile instead of the max, so one spike can't squash the
    # rest of the track.
    track_ref = np.percentile(db, 99.5)
    if global_norm:
        ref = np.full((1, db.shape[1]), track_ref)
    else:
        ref = np.percentile(db, 99.5, axis=0, keepdims=True)
        # Don't boost a nearly empty band (e.g. the treble on a piano track)
        # by more than max_boost_db, or its noise fills the bar.
        ref = np.maximum(ref, track_ref - max_boost_db)
    floor = np.maximum(ref - range_db, quantisation_noise_db(n_fft) + 6.0)
    span = np.maximum(ref - floor, 1e-3)
    frac = np.clip((db - floor) / span, 0.0, 1.0)

    # Quantise into 2**bits equal-width steps (top step includes 1.0).
    steps = 1 << bits
    level = np.minimum(np.floor(frac * steps), steps - 1)
    if decay > 0:
        # Fast attack, slow fall: a bar may drop at most `decay` levels per
        # slice. Done in float so fractional rates work, then floored.
        smooth = frac * steps
        for i in range(1, smooth.shape[0]):
            smooth[i] = np.maximum(smooth[i], smooth[i - 1] - decay)
        level = np.minimum(np.floor(smooth), steps - 1)
    return level.astype(np.uint8)


def pack(levels, bits):
    """Pack levels into bytes: 8-bit = one per byte; 4-bit = two per byte,
    even band in the low nibble."""
    if bits == 8:
        return levels
    return levels[:, 0::2] | (levels[:, 1::2] << 4)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("input", help="raw signed 8-bit mono PCM file")
    ap.add_argument("output", help="output band file")
    ap.add_argument("--rate", type=float, default=16384,
                    help="input sample rate in Hz (default 16384)")
    ap.add_argument("--hop", type=int, default=None,
                    help="input samples per output slice. Default: rate/60 "
                         "rounded. Use 256 at 16384 Hz for 64 slices/s and a "
                         "shift instead of a divide on the GBA.")
    ap.add_argument("--bands", type=int, default=8,
                    help="number of frequency bands (default 8)")
    ap.add_argument("--fft", type=int, default=1024,
                    help="FFT size (default 1024). Larger = better bass "
                         "resolution, blurrier in time.")
    ap.add_argument("--fmin", type=float, default=40.0,
                    help="lowest band edge in Hz (default 40)")
    ap.add_argument("--fmax", type=float, default=None,
                    help="highest band edge in Hz (default: Nyquist)")
    ap.add_argument("--range-db", type=float, default=45.0,
                    help="dynamic range mapped onto 0..255 (default 45 dB)")
    ap.add_argument("--global-norm", action="store_true",
                    help="one shared reference level for all bands instead "
                         "of one per band (true spectral balance; treble "
                         "bars will look small)")
    ap.add_argument("--max-boost-db", type=float, default=30.0,
                    help="per-band normalisation may lift a band at most this "
                         "far toward the loudest band (default 30 dB), so a "
                         "nearly empty band doesn't show its noise")
    ap.add_argument("--bits", type=int, choices=(4, 8), default=4,
                    help="bits per band: 4 = levels 0..15, two bands per byte "
                         "(default); 8 = levels 0..255, one per byte")
    ap.add_argument("--decay", type=float, default=0.0,
                    help="bake in fast-attack/slow-fall smoothing: max drop "
                         "per slice, in levels (fractions allowed). 0 = off "
                         "(default). With 4 bits try 0.5-1.")
    args = ap.parse_args()

    if args.bits == 4 and args.bands % 2:
        sys.exit("error: --bits 4 needs an even number of bands")

    rate = args.rate
    hop = args.hop or int(round(rate / 60.0))
    f_max = args.fmax or rate / 2.0

    pcm = np.fromfile(args.input, dtype=np.int8).astype(np.float32) / 128.0
    if pcm.size == 0:
        sys.exit("error: input file is empty")
    n_slices = (pcm.size + hop - 1) // hop

    power = stft_power(pcm, args.fft, hop, n_slices)
    bins = band_bins(args.bands, args.fmin, f_max, rate, args.fft)
    levels = band_levels(power, bins, args.range_db, args.global_norm,
                         args.max_boost_db, args.fft, args.decay, args.bits)
    out = np.ascontiguousarray(pack(levels, args.bits))
    out.tofile(args.output)

    secs = pcm.size / rate
    print(f"input:   {pcm.size} samples, {secs:.2f} s at {rate:g} Hz")
    print(f"slices:  {n_slices} ({rate / hop:.3f} per second, hop = {hop} samples)")
    print(f"output:  {out.nbytes} bytes, {out.shape[1]} per slice, "
          f"{args.bits}-bit levels "
          f"({out.nbytes / max(secs, 1e-9) * 60 / 1024:.1f} KiB/min)")
    print("bands:")
    for i, (lo, hi) in enumerate(bins):
        print(f"  {i}: {lo * rate / args.fft:7.0f} - {hi * rate / args.fft:7.0f} Hz")


if __name__ == "__main__":
    main()
