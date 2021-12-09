import numpy

def fill(data, start_coords, fill_value):
    """
    from https://gist.github.com/JDWarner/1158a9515c7f1b1c21f1

    Flood fill algorithm
    
    Parameters
    ----------
    data : (M, N) ndarray of uint8 type
        Image with flood to be filled. Modified inplace.
    start_coords : tuple
        Length-2 tuple of ints defining (row, col) start coordinates.
    fill_value : int
        Value the flooded area will take after the fill.
        
    Returns
    -------
    None, ``data`` is modified inplace.
    """
    new_layer = numpy.zeros(data.shape)
    xsize, ysize = data.shape
    stack = set([start_coords])

    while stack:
        x, y = stack.pop()
        if data[x, y] == 1:
            data[x, y] = fill_value
            if x > 0:
                stack.add((x - 1, y))
            if x < (xsize - 1):
                stack.add((x + 1, y))
            if y > 0:
                stack.add((x, y - 1))
            if y < (ysize - 1):
                stack.add((x, y + 1))
