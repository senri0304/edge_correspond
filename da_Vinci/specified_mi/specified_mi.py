# -*- coding: utf-8 -*-
import os, pyglet, time, datetime, random, copy, math
from pyglet.gl import *
from pyglet.image import AbstractImage
from collections import deque
import pandas as pd
import numpy as np
import display_info

# Prefernce
# ------------------------------------------------------------------------
rept = 5
exclude_mousePointer = False
# ------------------------------------------------------------------------

# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()
win = pyglet.window.Window(style=pyglet.window.Window.WINDOW_STYLE_BORDERLESS)
win.set_fullscreen(fullscreen=True, screen=screens[len(screens) - 1])  # Present secondary display
win.set_exclusive_mouse(exclude_mousePointer)  # Exclude mouse pointer
key = pyglet.window.key

# Load variable conditions
deg1 = display_info.deg1
cntx = screens[len(screens) - 1].width / 2  # Store center of screen about x position
cnty = screens[len(screens) - 1].height / 3  # Store center of screen about y position
dat = pd.DataFrame()
iso = 8
draw_objects = []  # 描画対象リスト
end_routine = False  # Routine status to be exitable or not
tcs = []  # Store transients per trials
kud_list = []  # Store durations of key pressed
cdt = []  # Store sum(kud), cumulative reaction time on a trial.
mdt = []
dtstd = []
latencies = []
press_timing = []
release_timing = []
exitance = True
n = 0

# Load resources
p_sound = pyglet.resource.media('materials/1000Hz.wav', streaming=False)
beep_sound = pyglet.resource.media('materials/640Hz.wav', streaming=False)
pedestal: AbstractImage = pyglet.image.load('materials/pedestal.png')
fixr = pyglet.sprite.Sprite(pedestal, x=cntx + iso * deg1 - pedestal.width / 2.0, y=cnty - pedestal.height / 2.0)
fixl = pyglet.sprite.Sprite(pedestal, x=cntx - iso * deg1 - pedestal.width / 2.0, y=cnty - pedestal.height / 2.0)
file_names = copy.copy(display_info.left_eye)*rept
file_names2 = copy.copy(display_info.right_eye)*rept
disparity = copy.copy(display_info.disparity)*rept
test_eye = copy.copy(display_info.test_eye)*rept
r = random.randint(0, math.factorial(len(file_names)))
random.seed(r)
seq1 = random.sample(file_names, len(file_names))
random.seed(r)
seq2 = random.sample(file_names2, len(file_names))
random.seed(r)
seq3 = random.sample(disparity, len(file_names))
random.seed(r)
seq4 = random.sample(test_eye, len(file_names))

print(str(seq1) + '\n' + str(seq2))

# ----------- Core program following ----------------------------

# A getting key response function
class key_resp(object):
    def on_key_press(self, symbol, modifiers):
        global exitance, trial_start
        if exitance == False and symbol == key.DOWN:
            kd.append(time.time())
        if exitance == True and symbol == key.UP:
            p_sound.play()
            exitance = False
            pyglet.clock.schedule_once(delete, 30.0)
            replace()
            trial_start = time.time()
        if symbol == key.ESCAPE:
            win.close()
            pyglet.app.exit()

    def on_key_release(self, symbol, modifiers):
        if exitance == False and symbol == key.DOWN:
            ku.append(time.time())


resp_handler = key_resp()


# Store objects into draw_objects
def fixer():
    draw_objects.append(fixl)
    draw_objects.append(fixr)


def replace():
    del draw_objects[:]
    fixer()
    draw_objects.append(R)
    draw_objects.append(L)


# A end routine function
def exit_routine(dt):
    global exitance
    exitance = True
    beep_sound.play()
    prepare_routine()
    fixer()
    pyglet.app.exit()


@win.event
def on_draw():
    # Refresh window
    win.clear()
    # 描画対象のオブジェクトを描画する
    for draw_object in draw_objects:
        draw_object.draw()


# Remove stimulus
def delete(dt):
    global n, trial_end
    del draw_objects[:]
    p_sound.play()
    n += 1
    pyglet.clock.schedule_once(get_results, 1.0)
    trial_end = time.time()


def get_results(dt):
    global ku, kud, kd, kud_list, mdt, dtstd, n, tcs, trial_end, trial_start, latency, latencies, press_timing, release_timing
    if len(kd) > 0 and len(ku) > 0:
        if kd[0] - ku[0] > 0:
            del ku[0]
    tc = (len(kd) + len(ku))
    if len(ku) != len(kd):
        ku.append(trial_start + 30.0)
    if len(kd) < 1:
        latency = 30.0
    else:
        latency = kd[0] - trial_start
    press_timing.append(str(np.array(kd) - trial_start))
    release_timing.append(str(np.array(ku) - trial_start))
    while len(kd) > 0:
        kud.append(ku.popleft() - kd.popleft() + 0)  # list up key_press_duration
    kud_list.append(str(kud))
    c = sum(kud)
    cdt.append(c)
    tcs.append(tc)
    if len(kud) == 0:
        kud.append(0)
    m = np.mean(kud)
    d = np.std(kud)
    mdt.append(m)
    dtstd.append(d)
    latencies.append(latency)
    string = ('--------------------------------------------------\n'
              'trial: ' + str(n) + '/' + str(len(seq1)) + '\n'
              'start: ' + str(trial_start) + '\n'
              'end: ' + str(trial_end) + '\n'
              'key_pressed: ' + str(kud) + '\n'
              'transient counts: ' + str(tc) + '\n'
              'cdt: ' + str(c) + '\n'
              'mdt: ' + str(m) + '\n'
              'dtstd: ' + str(d) + '\n'
              'latency: ' + str(latency) + '\n'
              'condition: ' + str(seq3[n-1]) + ' ' + str(seq4[n-1]) + '\n'
              '--------------------------------------------------')
    print(string)
    # Check the experiment continue or break
    if n != len(file_names):
        pyglet.clock.schedule_once(exit_routine, 29.0)
    else:
        p_sound.play()
        pyglet.app.exit()


def set_polygon():
    global L, R, n
    # Set up polygon for stimulus
    R = pyglet.resource.image('stereograms/' + str(seq2[n]))
    R = pyglet.sprite.Sprite(R)
    R.x = cntx + deg1 * iso - R.width / 2.0
    R.y = cnty - R.height / 2.0
    L = pyglet.resource.image('stereograms/' + str(seq1[n]))
    L = pyglet.sprite.Sprite(L)
    L.x = cntx - deg1 * iso - L.width / 2.0
    L.y = cnty - L.height / 2.0


def prepare_routine():
    global n, file_names
    if n < len(file_names):
        fixer()
        set_polygon()
    else:
        pass


# Store the start time
start = time.time()
win.push_handlers(resp_handler)

fixer()
set_polygon()

for i in seq1:
    tc = 0  # Count transients
    ku = deque([])  # Store unix time when key up
    kd = deque([])  # Store unix time when key down
    kud = []  # Differences between kd and ku

    pyglet.app.run()

# -------------- End loop -------------------------------

win.close()

# Store the end time
end_time = time.time()
daten = datetime.datetime.now()

# Write results onto csv
results = pd.DataFrame({'trial': list(range(1, len(file_names) + 1)),  # Store variance_A conditions
                        'right_eye': seq1,
                        'left_eye': seq2,
                        'disparity': seq3,
                        'test_eye': seq4,
                        'transient_counts': tcs,  # Store transient_counts
                        'cdt': cdt,  # Store cdt(target values) and input number of trials
                        'mdt': mdt,
                        'dtstd': dtstd,
                        'latency': latencies,
                        'press_timing': press_timing,
                        'release_timing': release_timing,
                        'key_press_list': kud_list})  # Store the key_press_duration list

os.makedirs('data', exist_ok=True)

name = str(daten)
name = name.replace(":", "'")
results.to_csv(path_or_buf='./data/DATE' + name + '.csv', index=False)  # Output experimental data_occ

# Output following to shell, check this experiment
print(u'開始日時: ' + str(start))
print(u'終了日時: ' + str(end_time))
print(u'経過時間: ' + str(end_time - start))
