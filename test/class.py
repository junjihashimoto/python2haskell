class RGB:
    r : float
    g : float
    b : float

class YCoCg:
    y : float
    co : float
    cg : float

class Image:
    dat : List[RGB]
    width : int
    height : int

def rgb2ycocg(rgb : RGB) -> YCoCg:
    return (YCoCg ((rgb.r + rgb.g * 2 + rgb.b) / 4,
                   (rgb.r - rgb.b) / 2,
                   (- rgb.r + rgb.g * 2 - rgb.b) / 4))

print (RGB(1,2,3))
