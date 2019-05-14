import time
import urllib.request
import os
import subprocess

from bs4 import BeautifulSoup

setwallpaper = False

# https://www.dictionary.com/wordoftheday/2018/08/14/
if __name__ == '__main__':
    # Create the wotd link
    wotdLink = "https://www.dictionary.com/wordoftheday/" + time.strftime("%Y/%m/%d")

    # Get HTML
    site = urllib.request.urlopen(wotdLink)
    html = site.read()

    # BeautifulSoup
    soup = BeautifulSoup(html, "html.parser")
    a = soup.find("a", {"class": "uploaded"})

    # If can't find wotd image.
    if not a:
        print("Can't find uploaded a.")
        exit(0)
    word = a.find("img")['alt']
    word = word.replace(' ', '-')

    # Get image in format. Eg. http://static.sfdict.com/static/wotd/tiles/20180814_riant.png
    imageUrl = "http://static.sfdict.com/static/wotd/tiles/{0}_{1}.png".format(time.strftime("%Y%02m%02d"), word)

    # Save wotd image in wotd/yyyymmdd_word.png
    imageFilename = "/home/ryan/Wallpapers/wotd/{0}_{1}.png".format(time.strftime("%Y%02m%02d"), word)

    # Create new wotd directory if it doesn't exist.
    if not os.path.isdir("/home/ryan/Wallpapers/wotd/"):
        os.mkdir("/home/ryan/Wallpapers/wotd")

    # Download image.
    print("Downloaded " + imageFilename + " from " + imageUrl)
    urllib.request.urlretrieve(imageUrl, imageFilename)

    # Only set wallpaper if desired.
    if setwallpaper:
        # Set wotd image as wallpaper.
        resultWallpaper = subprocess.call('xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/last-image --set ' + imageFilename, shell=True)
        print("Set Wallpaper with exit code " + str(resultWallpaper))

        # Set image style as centered
        resultStyle = subprocess.call('xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/image-style --set 1', shell=True)
        print("Set image style with exit code " + str(resultStyle))
