#!/usr/bin/python3

import collections
import sys

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

from matplotlib import style
from matplotlib.lines import Line2D
from threading import Thread

style.use('dark_background')
#style.use('ggplot')

figure = plt.figure()
timedomain = figure.add_subplot(2,1,1)
spectrum = figure.add_subplot(2,1,2)
tdline = Line2D([], [], color='red', linewidth=2)
fdline = Line2D([], [], color='red', linewidth=2)
timedomain.add_line(tdline);
spectrum.add_line(fdline);

timedomain.set_xlim(1, 2200)
timedomain.set_ylim(-0.05, 0.05)
spectrum.set_xlim(1, 1100)
spectrum.set_ylim(-70, 0)

# Use a Deque with limited size as a FIFO. This implementation drops old data
# on overflow. Exactly what we need.
fifo = collections.deque(maxlen = 10)

def readData():
    while True:
        tmp = sys.stdin.readline().strip().split(" ")
        data = np.array(tmp, dtype=np.double)
        #print("DataSize:", len(data))
        fifo.append(data)

def init():
    return fdline,tdline

def update(frame):
    if len(frame) == 0:
        #print("Nop")
        return tdline,fdline

    frame = frame - np.mean(frame)
    n = len(frame)
    if n != 2200:
        print("python: ", n)
    xs = list(range(1, n + 1))


    windowed = np.hanning(n) * frame
    fd = np.multiply(20, np.log10(np.abs(np.fft.fft(windowed)))) - 30
    n = int(len(fd) / 2)
    fd = fd[0:n]
    fs = list(range(1, n + 1))


    tdline.set_data(xs, frame)
    fdline.set_data(fs, fd)
    return tdline,fdline

def fetch():
    while True:
        if len(fifo) == 0:
            yield []
        else:
            yield fifo.popleft()

reader = Thread(target = readData)
reader.start()

ani = animation.FuncAnimation(fig = figure,
                              func = update,
                              frames = fetch,
                              interval = 80,
                              repeat = False,
                              blit = True)
plt.show()
