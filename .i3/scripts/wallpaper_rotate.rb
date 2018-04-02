#!/usr/bin/env ruby

counter = 0

loop do
  # feh command to open the next wallpaper
  if !system( "feh --bg-fill ~/wallpaper/" + counter.to_s + ".png" )
    print "Unable to load wallpaper: " + counter.to_s + ".png."
    exit
  end

  # sleep for ~10 minutes
  sleep(600)
  
  # increment wallpaper index
  counter += 1
  
  # loop back to first wallpaper (subtract 1 for 0 index)
  if counter > Dir["**"].length - 1 then counter = 0 end
end
