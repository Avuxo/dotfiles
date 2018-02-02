#! /usr/bin/python

import os
import time

starttime = time.time()
counter = 0

while True:
    #uses 'feh' to open the next wallpaper in the directory 
    os.system("feh --bg-fill ~/wallpaper/" + str(counter) + ".png")
    
    #after 10 minutes loop back
    time.sleep(600.0 - ((time.time() - starttime)) % 600.0)
    counter += 1
    
    #checks to see if the directory is out of wallpapers, if it is loopback
    if counter > (len(os.listdir('/home/ben/wallpaper')) - 1):
        counter = 0
