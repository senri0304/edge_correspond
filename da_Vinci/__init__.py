#!/usr/bin/env python
# coding: utf-8

import os, pyglet, wave, struct
import numpy as np
from PIL import Image, ImageDraw
from display_info import *

to_dir = 'stereograms'
os.makedirs(to_dir, exist_ok=True)

# Input stereogram size in cm unit
size = 5

# Input line size in cm unit
line_length = 0.7  # 30pix is 42 min of arc on 57cm distance

# Input a number you like to initiate
s = 1

# Input luminance of background
lb = 85  # 215, 84%

# Input fixation point position in cm unit
ecc = 1

# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()

resolution = screens[len(screens) - 1].height

c = (aspect_width ** 2 + aspect_height ** 2) ** 0.5
d_height = 2.54 * (aspect_height / c) * inch

sz = round(resolution * (size / d_height))
ll = round(resolution * line_length / d_height)
f = round(sz * 0.023 / 2)  # % relative size, sz*0.023 equals about 7 min

# Input the disparity at pixel units.
disparity = f*2

#eccentricity = round(1 / np.sqrt(2.0) * ecc / d_height * resolution)
eccentricity = ecc

# Generate stereograms
for i in variation:
    img = Image.new("RGB", (sz, sz), (lb, lb, lb))
    draw = ImageDraw.Draw(img)

    draw.rectangle((int(sz / 2) - int(f*i / 2) - disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) + int(f / 2) - disparity, int(sz / 2) - int(ll / 2)),
                   fill=(0, 0, 0), outline=None)

    draw.rectangle((int(sz / 2) + int(f / 2) + disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) - int(f / 2) + disparity, int(sz / 2) - int(ll / 2)),
                   fill=(lb*2, 0, 0), outline=None)

    # fixation point
    eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
    draw.rectangle((int(sz / 2) - f + eccentricity, int(sz / 2) + f * 3,
                    int(sz / 2) + f + eccentricity, int(sz / 2) - f * 3),
                   fill=(0, 0, 255), outline=None)
    draw.rectangle((int(sz / 2) - f * 3 + eccentricity, int(sz / 2) + f,
                    int(sz / 2) + f * 3 + eccentricity, int(sz / 2) - f),
                   fill=(0, 0, 255), outline=None)

    basename = os.path.basename(str(i) + 'ls.png')
    img.save(os.path.join(to_dir, basename), quality=100)

# ls
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

draw.rectangle((int(sz / 2) - int(f / 2), int(sz / 2) + int(ll / 2),
                int(sz / 2) + int(f / 2), int(sz / 2) - int(ll / 2)),
               fill=(0, 0, 0), outline=None)

# fixation point
eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
draw.rectangle((int(sz / 2) - f + eccentricity, int(sz / 2) + f * 3,
                int(sz / 2) + f + eccentricity, int(sz / 2) - f * 3),
               fill=(0, 0, 255), outline=None)
draw.rectangle((int(sz / 2) - f * 3 + eccentricity, int(sz / 2) + f,
                int(sz / 2) + f * 3 + eccentricity, int(sz / 2) - f),
               fill=(0, 0, 255), outline=None)

basename = os.path.basename('ls.png')
img.save(os.path.join(to_dir, basename), quality=100)


# ls for local
imgt = Image.new("RGB", (sz, sz), (lb, lb, lb))
drawt = ImageDraw.Draw(imgt)

drawt.rectangle((int(sz / 2) - int(f / 2) + disparity, int(sz / 2) + int(ll / 2),
                int(sz / 2) + int(f / 2) + disparity, int(sz / 2) - int(ll / 2)),
                fill=(lb*2, 0, 0), outline=None)

# fixation point
eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
drawt.rectangle((int(sz / 2) - f + eccentricity, int(sz / 2) + f * 3,
                int(sz / 2) + f + eccentricity, int(sz / 2) - f * 3),
                fill=(0, 0, 255), outline=None)
drawt.rectangle((int(sz / 2) - f * 3 + eccentricity, int(sz / 2) + f,
                int(sz / 2) + f * 3 + eccentricity, int(sz / 2) - f),
                fill=(0, 0, 255), outline=None)

basename = os.path.basename('testls.png')
imgt.save(os.path.join(to_dir, basename), quality=100)


# stereogram without stimuli
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

# fixation point
draw.rectangle((int(sz / 2) - f + eccentricity, int(sz / 2) + f * 3,
                int(sz / 2) + f + eccentricity, int(sz / 2) - f * 3),
               fill=(0, 0, 255), outline=None)
draw.rectangle((int(sz / 2) - f * 3 + eccentricity, int(sz / 2) + f,
                int(sz / 2) + f * 3 + eccentricity, int(sz / 2) - f),
               fill=(0, 0, 255), outline=None)

to_dir = 'materials'
os.makedirs(to_dir, exist_ok=True)
basename = os.path.basename('pedestal.png')
img.save(os.path.join(to_dir, basename), quality=100)


# sound files
# special thank: @kinaonao  https://qiita.com/kinaonao/items/c3f2ef224878fbd232f5

# sin波
# --------------------------------------------------------------------------------------------------------------------
def create_wave(A, f0, fs, t, name):  # A:振幅,f0:基本周波数,fs:サンプリング周波数,再生時間[s],n:名前
    # nポイント
    # --------------------------------------------------------------------------------------------------------------------
    point = np.arange(0, fs * t)
    sin_wave = A * np.sin(2 * np.pi * f0 * point / fs)

    sin_wave = [int(x * 32767.0) for x in sin_wave]  # 16bit符号付き整数に変換

    # バイナリ化
    binwave = struct.pack("h" * len(sin_wave), *sin_wave)

    # サイン波をwavファイルとして書き出し
    w = wave.Wave_write(os.path.join(to_dir, str(name) + ".wav"))
    p = (1, 2, fs, len(binwave), 'NONE',
         'not compressed')  # (チャンネル数(1:モノラル,2:ステレオ)、サンプルサイズ(バイト)、サンプリング周波数、フレーム数、圧縮形式(今のところNONEのみ)、圧縮形式を人に判読可能な形にしたもの？通常、 'NONE' に対して 'not compressed' が返されます。)
    w.setparams(p)
    w.writeframes(binwave)
    w.close()


create_wave(1, 640, 44100, 1.0, '640Hz')
create_wave(1, 1000, 44100, 0.1, '1000Hz')



to_dir = 'stereograms2'
os.makedirs(to_dir, exist_ok=True)

# Generate stereograms
for i in variation:
    img = Image.new("RGB", (sz, sz), (lb, lb, lb))
    draw = ImageDraw.Draw(img)

    draw.rectangle((int(sz / 2) + int(f*i / 2) + disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) - int(f / 2) + disparity, int(sz / 2) - int(ll / 2)),
                   fill=(0, 0, 0), outline=None)

    draw.rectangle((int(sz / 2) + int(f / 2) - disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) - int(f / 2) - disparity, int(sz / 2) - int(ll / 2)),
                   fill=(lb*2, 0, 0), outline=None)

    # fixation point
    eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
    draw.rectangle((int(sz / 2) - f - eccentricity, int(sz / 2) + f * 3,
                    int(sz / 2) + f - eccentricity, int(sz / 2) - f * 3),
                   fill=(0, 0, 255), outline=None)
    draw.rectangle((int(sz / 2) - f * 3 - eccentricity, int(sz / 2) + f,
                    int(sz / 2) + f * 3 - eccentricity, int(sz / 2) - f),
                   fill=(0, 0, 255), outline=None)

    basename = os.path.basename(str(i) + 'ls.png')
    img.save(os.path.join(to_dir, basename), quality=100)


# ls
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

draw.rectangle((int(sz / 2) - int(f / 2), int(sz / 2) + int(ll / 2),
                int(sz / 2) + int(f / 2), int(sz / 2) - int(ll / 2)),
               fill=(0, 0, 0), outline=None)

# fixation point
eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
draw.rectangle((int(sz / 2) - f - eccentricity, int(sz / 2) + f * 3,
                int(sz / 2) + f - eccentricity, int(sz / 2) - f * 3),
               fill=(0, 0, 255), outline=None)
draw.rectangle((int(sz / 2) - f * 3 - eccentricity, int(sz / 2) + f,
                int(sz / 2) + f * 3 - eccentricity, int(sz / 2) - f),
               fill=(0, 0, 255), outline=None)

basename = os.path.basename('ls.png')
img.save(os.path.join(to_dir, basename), quality=100)


to_dir = 'stereograms3'
os.makedirs(to_dir, exist_ok=True)

# Generate stereograms
for i in variation:
    img = Image.new("RGB", (sz, sz), (lb, lb, lb))
    draw = ImageDraw.Draw(img)

    draw.rectangle((int(sz / 2) + int(f / 2) - disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) - int(f / 2)*i - disparity, int(sz / 2) - int(ll / 2)),
                   fill=(0, 0, 0), outline=None)

    draw.rectangle((int(sz / 2) - int(f / 2) + disparity, int(sz / 2) + int(ll / 2),
                    int(sz / 2) + int(f / 2) + disparity, int(sz / 2) - int(ll / 2)),
                   fill=(lb*2, 0, 0), outline=None)

    # fixation point
    eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
    draw.rectangle((int(sz / 2) + eccentricity - f, int(sz / 2) + f * 3,
                    int(sz / 2) + eccentricity + f, int(sz / 2) - f * 3),
                   fill=(0, 0, 255), outline=None)
    draw.rectangle((int(sz / 2) + eccentricity - f * 3, int(sz / 2) + f,
                    int(sz / 2) + eccentricity + f * 3, int(sz / 2) - f),
                   fill=(0, 0, 255), outline=None)

    basename = os.path.basename(str(i) + 'ls.png')
    img.save(os.path.join(to_dir, basename), quality=100)


# ls
img = Image.new("RGB", (sz, sz), (lb, lb, lb))
draw = ImageDraw.Draw(img)

draw.rectangle((int(sz / 2) - int(f / 2) - disparity, int(sz / 2) + int(ll / 2),
                int(sz / 2) + int(f / 2) - disparity, int(sz / 2) - int(ll / 2)),
               fill=(0, 0, 0), outline=None)

# fixation point
eccentricity = round(1 / np.sqrt(2.0) / d_height * resolution)
draw.rectangle((int(sz / 2) - f + eccentricity, int(sz / 2) + f * 3,
                int(sz / 2) + f + eccentricity, int(sz / 2) - f * 3),
               fill=(0, 0, 255), outline=None)
draw.rectangle((int(sz / 2) - f * 3 + eccentricity, int(sz / 2) + f,
                int(sz / 2) + f * 3 + eccentricity, int(sz / 2) - f),
               fill=(0, 0, 255), outline=None)

basename = os.path.basename('ls.png')
img.save(os.path.join(to_dir, basename), quality=100)

imgt.save(os.path.join(to_dir, 'testls.png'), quality=100)
