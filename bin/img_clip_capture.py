#!/usr/bin/env python3
"""
Image Paste Listener for macOS
Listens for image pastes from clipboard and appends them to a growing vertical image.

# pip install Pillow
"""

import os
import time
import hashlib
from PIL import Image, ImageGrab
import threading
from pathlib import Path
from datetime import datetime

class ImagePasteListener:
    def __init__(self, output_filename="combined_image.png", poll_interval=0.5):
        self.output_filename = output_filename
        self.poll_interval = poll_interval
        self.last_clipboard_hash = None
        self.combined_image = None
        self.running = False
        self.current_folder = Path.cwd()
        self.output_path = self.current_folder / self.output_filename

        print(f"Image Paste Listener initialized")
        print(f"Output file: {self.output_path}")
        print(f"Current folder: {self.current_folder}")

    def get_clipboard_image_hash(self):
        """Get hash of current clipboard image to detect changes"""
        try:
            clipboard_image = ImageGrab.grabclipboard()
            if clipboard_image and hasattr(clipboard_image, 'tobytes'):
                # Convert to bytes and hash
                image_bytes = clipboard_image.tobytes()
                return hashlib.md5(image_bytes).hexdigest()
        except Exception as e:
            # Clipboard might not contain an image or might be inaccessible
            pass
        return None

    def get_clipboard_image(self):
        """Get image from clipboard"""
        try:
            clipboard_image = ImageGrab.grabclipboard()
            if clipboard_image:
                # Ensure it's in RGB mode for consistency
                if clipboard_image.mode != 'RGB':
                    clipboard_image = clipboard_image.convert('RGB')
                return clipboard_image
        except Exception as e:
            print(f"Error getting clipboard image: {e}")
        return None

    def load_existing_image(self):
        """Load existing combined image if it exists"""
        if self.output_path.exists():
            try:
                self.combined_image = Image.open(self.output_path)
                print(f"Loaded existing image: {self.combined_image.size}")
            except Exception as e:
                print(f"Error loading existing image: {e}")
                self.combined_image = None

    def append_image(self, new_image):
        """Append new image to the combined image vertically"""
        if self.combined_image is None:
            # First image
            self.combined_image = new_image.copy()
            print(f"Started new combined image with size: {self.combined_image.size}")
        else:
            # Calculate dimensions for the new combined image
            old_width, old_height = self.combined_image.size
            new_width, new_height = new_image.size

            # Use the maximum width
            max_width = max(old_width, new_width)
            total_height = old_height + new_height

            # Create new image with combined dimensions
            combined = Image.new('RGB', (max_width, total_height), 'white')

            # Paste the old image at the top
            old_x_offset = (max_width - old_width) // 2
            combined.paste(self.combined_image, (old_x_offset, 0))

            # Paste the new image below it
            new_x_offset = (max_width - new_width) // 2
            combined.paste(new_image, (new_x_offset, old_height))

            self.combined_image = combined
            print(f"Appended image. New combined size: {self.combined_image.size}")

    def save_combined_image(self):
        """Save the combined image to file"""
        if self.combined_image:
            try:
                self.combined_image.save(self.output_path, 'PNG')
                print(f"Saved combined image to: {self.output_path}")
            except Exception as e:
                print(f"Error saving image: {e}")

    def listen_for_pastes(self):
        """Main listening loop"""
        print("Starting to listen for image pastes...")
        print("Copy an image to clipboard to add it to the combined image.")
        print("Press Ctrl+C to stop.")

        # Load existing image if available
        self.load_existing_image()

        self.running = True
        while self.running:
            try:
                current_hash = self.get_clipboard_image_hash()

                # Check if clipboard content changed and contains an image
                if current_hash and current_hash != self.last_clipboard_hash:
                    clipboard_image = self.get_clipboard_image()

                    if clipboard_image:
                        timestamp = datetime.now().strftime("%H:%M:%S")
                        print(f"[{timestamp}] New image detected in clipboard!")

                        # Append to combined image
                        self.append_image(clipboard_image)

                        # Save the updated combined image
                        self.save_combined_image()

                        # Update last hash
                        self.last_clipboard_hash = current_hash

                        print(f"Image added successfully!")

                time.sleep(self.poll_interval)

            except KeyboardInterrupt:
                print("\nStopping image paste listener...")
                self.running = False
                break
            except Exception as e:
                print(f"Error in main loop: {e}")
                time.sleep(1)  # Wait a bit longer on error

    def start(self):
        """Start the listener"""
        try:
            self.listen_for_pastes()
        except KeyboardInterrupt:
            print("\nShutting down...")
        finally:
            print("Image paste listener stopped.")

def main():
    """Main function"""
    print("=" * 50)
    print("Image Paste Listener for macOS")
    print("=" * 50)

    # You can customize the output filename here
    output_file = "combined_screenshots.png"

    listener = ImagePasteListener(output_filename=output_file)
    listener.start()

if __name__ == "__main__":
    main()
