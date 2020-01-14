# function to draw curly braces
# x, y position where to put the braces
# range is the length of the brace
# position: 1 vertical, 2 horizontal
# direction: 1 left/down, 2 right/up
# depth controls width of the shape
# code obtained at: https://stackoverflow.com/questions/23869526/plot-curly-bracket-for-axes-area
# modified to allow for attached labels
# refactored to have more meaninful parameters

CurlyBraces <- function(x0,
                        x1,
                        y0,
                        y1,
                        position = 1,
                        # using R convention used in par(), it goes: bottom (1), left (2), top (3), right (4)
                        bracesSize = 1,
                        label = NULL,
                        labelDist = 1,
                        labelSize = 1,
                        labelAdj = 0.5) {
  a = c(1, 2, 3, 48, 50)    # set flexion point for spline
  b = c(0, .2, .28, .7, .8) # set depth for spline flexion point
  
  curve = spline(a, b, n = 50, method = "natural")$y * bracesSize
  
  curve = c(curve, rev(curve))
  
  x_sequence = seq(x0, x1, length = 100)
  y_sequence = seq(y0, y1, length = 100)
  
  if (position == 2 | position == 4)
    # vertical
  {
    if (position == 2)
      direction = -1
    else
      direction = 1
    
    x_sequence = x_sequence + direction * curve
  }
  if (position == 1 | position == 3)
    # horizontal
  {
    if (position == 1)
      direction = -1
    else
      direction = 1
    
    y_sequence = y_sequence + direction * curve
  }
  
  lines(x_sequence, y_sequence, lwd = 1.5, xpd = NA)
  
  if (!is.null(label))
    mtext(text = label, side = position, line = labelDist, cex = labelSize, adj = )
}
