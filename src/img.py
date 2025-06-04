from PIL import Image, ImageDraw

im = Image.new('RGB',(100,175),(255,255,255))
draw = ImageDraw.Draw(im)
draw.line([(0,174),(100,174)],fill = 0)
draw.line([(0,75),(100,75)],fill = 0)
im.show()
im.save("Rampa.png")