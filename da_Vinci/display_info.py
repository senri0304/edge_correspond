
import pyglet.canvas

# Input display information
inch = 23
aspect_width = 16
aspect_height = 9

# Input a variety
variation = [1, 15]
#test_eye = [1, -1]

# Get display information
display = pyglet.canvas.get_display()
screens = display.get_screens()

resolution = screens[len(screens) - 1].height

c = (aspect_width ** 2 + aspect_height ** 2) ** 0.5
d_height = 2.54 * (aspect_height / c) * inch

deg1 = round(resolution * (1 / d_height))
