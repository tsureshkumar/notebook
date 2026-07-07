from PIL import Image, ImageDraw

# 1. Define dimensions in pixels for 200 DPI
# 2.5 cm = 0.984 inches * 200 = 197 pixels
# 3.5 cm = 1.378 inches * 200 = 276 pixels
width, height = 276, 197

# 2. Create a new image with transparent background (RGBA)
img = Image.new("RGBA", (width, height), (0, 0, 0, 0))

# 3. Draw a border outline
draw = ImageDraw.Draw(img)
border_color = (0, 0, 0, 255) # Opaque Black
draw.rectangle([0, 0, width - 1, height - 1], outline=border_color)

# 4. Save the file
img.save("bordered_image.png", "PNG", dpi=(200, 200))

