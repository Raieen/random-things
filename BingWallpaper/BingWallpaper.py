import json, time, urllib.request, os, subprocess, sys

if __name__ == '__main__':
	args = sys.argv[1:]

	if len(args) != 2:
		print("Invalid Usage: python file.py save_location y/n")
		sys.exit(1)

	save_location = args[0]
	set_wallpaper = args[1]

	if not os.path.exists(save_location):
		os.mkdir(save_location)

	# Get Wallpaper Image
	request = urllib.request.urlopen("https://www.bing.com/HPImageArchive.aspx?format=js&idx=0&n=1&mkt=en-CA")
	requestJson = json.loads(request.read())
	imageUrl = "https://www.bing.com" + requestJson['images'][0]['url']

	# Download Image
	file = save_location + "/" + time.strftime("%Y-%m-%d") + ".jpg"

	# Retrieve image.
	urllib.request.urlretrieve(imageUrl, file)
	print("Saved wallpaper")

	if set_wallpaper == 'y':
		# Set wallpaper image.
		# xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitoreDP1/workspace0/last-image --set ~/Wallpapers/bing-images/2019-05-10.jpg
		exitWallpaper = subprocess.call("xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitoreDP1/workspace0/last-image --set " + file, shell=True)
		print("Set wallpaper with exit code", exitWallpaper)

		# Center wallpaper
		# xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitoreDP1/workspace0/image-style --set 1
		exitCenter = subprocess.call('xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitoreDP1/workspace0/image-style --set 1', shell=True)
		print("Set image style with exit code", exitCenter)