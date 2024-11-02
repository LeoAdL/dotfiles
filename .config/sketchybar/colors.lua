return {
    black = 0xffeba0ac,
    white = 0xffdc8a78,
    red = 0xffdd7878,
    green = 0xffa6e3a1,
    blue = 0xff89dceb,
    yellow = 0xffe7c664,
    orange = 0xffe2e2e3,
    magenta = 0xfff38ba8,
    grey = 0xfff5e0dc,
    transparent = 0x00000000,

    bar = {
        bg = 0x44000000,
        border = 0xffeba0ac,
    },
    popup = {
        bg = 0xc02c2e34,
        border = 0xfff5e0dc,
    },
    bg1 = 0xff313244,
    bg2 = 0xff81c8be,

    with_alpha = function(color, alpha)
        if alpha > 1.0 or alpha < 0.0 then return color end
        return (color & 0x00ffffff) | (math.floor(alpha * 255.0) << 24)
    end,
}
