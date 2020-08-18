from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

import os
import time


class my_handler(FileSystemEventHandler):
    def on_modified(self, event):
        for filename in os.listdir(folder_to_track):
            extn = os.path.splitext(filename)[1]
            if extn != ".txt" and extn != ".pdf" and extn != ".png" and extn != ".jpg":
                print("invalid file type")
            else:
                src = folder_to_track + "/" + filename
                if extn == ".txt" or extn == ".pdf":
                    new_destination = folder_destination_docs + "/" + filename
                elif extn == ".png" or extn == ".jpg":
                    new_destination = folder_destination_images + "/" + filename
                os.rename(src, new_destination)


folder_to_track = r"C:\Users\Kian\Downloads"
folder_destination_images = r"C:\Users\Kian\Pictures\Saved_Pictures"
folder_destination_docs = r"C:\Users\Kian\Documents"

event_handler = my_handler()
observer = Observer()
observer.schedule(event_handler, folder_to_track, recursive=True)
observer.start()

try:
    while True:
        time.sleep(10)
except KeyboardInterrupt:
    observer.stop()
observer.join()
