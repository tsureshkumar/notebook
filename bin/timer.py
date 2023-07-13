#!/usr/local/bin/python3
import emoji
import argparse
import playsound
import os

from tkinter import *
root = Tk()
W=root.winfo_screenwidth()
H=root.winfo_screenheight()
root.geometry(f"200x100+{W-200}+0")
root.attributes('-alpha', .5)
root.attributes('-topmost', True)

NOT_STARTED = 0
PAUSED = 1
STOPPED = 2
RUNNING = 3

st = NOT_STARTED
def count_down(count=20*60, orig=20*60):
    global st,button
    lblTimer.config(text=f"{count//60:2}:{count%60:02}")
    nxt = count-1
    if st == NOT_STARTED or st == STOPPED:
        nxt = orig
        button.config(text=emoji.emojize(":thumbs_up:"))
    elif st == PAUSED:
        nxt = count
        button.config(text=emoji.emojize(":pause_button:"))
    elif st == RUNNING:
        button.config(text=emoji.emojize(":man_running:"))
    if count > 0:
        pass
    else:
        play_alert()
        count = nxt = orig
    root.lift()
    root.after(1000, lambda: count_down(nxt, orig))

def play_alert():
    alert_bell = os.path.join(os.path.dirname(__file__), 'service-bell-ring-14610.mp3')
    playsound.playsound(alert_bell)

def start():
    global st
    st = RUNNING
def pause():
    global st
    st = PAUSED
def reset():
    global st
    st = NOT_STARTED

def check():
    pass

timerFrame = Frame()
lblTimer = Label(master=timerFrame, text="test", font=("Arial", 50))
lblTimer.pack()
button = Button(master=timerFrame , text = emoji.emojize(":thumbs_up:"), font = "arial 30" , command = check, bd=0, highlightthickness=0)
button.bind("<Enter>", func=lambda e: button.config(background="red"))
button.bind("<Leave>", func=lambda e: button.config(background="blue"))
button.pack()


timerFrame.pack()

def quit(event):
    exit(0)

root.bind("q", quit)
root.bind("s", lambda ev: start())
root.bind("p", lambda ev: pause())
root.bind("r", lambda ev: reset())
args = None
root.after(100, lambda: count_down(count=args.mins*60+args.secs, orig=args.mins*60+args.secs))

parser = argparse.ArgumentParser( prog = 'timer', description = 'timer related functions', epilog = 'help')
parser.add_argument('-m', '--mins', default=20, type=int)
parser.add_argument('-s', '--secs', default=0, type=int)
args = parser.parse_args()
root.mainloop()
