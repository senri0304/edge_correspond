
import pyglet.canvas

# Input display information
inch = 12#23
aspect_width = 16
aspect_height = 10#9

# Input a variety
variation = [1, -1]

# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()

resolution = screens[len(screens) - 1].height

c = (aspect_width ** 2 + aspect_height ** 2) ** 0.5
d_height = 2.54 * (aspect_height / c) * inch

deg1 = round(resolution * (1 / d_height))

left_eye = ['-1ls.png', '1ls.png', 'ls.png', 'ls.png', 'cntl.png', 'ls.png']
right_eye = ['ls.png', 'ls.png', '1ls.png', '-1ls.png', 'ls.png', 'cntl.png']
disparity = ['uncross', 'cross', 'uncross', 'cross', 'control', 'control']
test_eye = ['left', 'left', 'right', 'right', 'left', 'right']
