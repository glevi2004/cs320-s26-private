
let _ = assert (Lab02.interp "1 - 2" = -1)
let _ = assert (Lab02.interp "2 - 3 - 4" = -5)
let _ = assert (Lab02.interp "((6))" = 6)
let _ = assert (Lab02.interp "1 - (2 - 3 - 4) - ((6))" = 0)
