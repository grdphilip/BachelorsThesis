
#3DPlot
volcano_long <- data.frame(
  x = as.vector(col(volcano)),
  y = as.vector(row(volcano)),
  z = as.vector(volcano)
)

ggplot(volcano_long, aes(x, y, z = z)) + 
  geom_polygon(aes(fill = stat(level)), alpha = 0.5, stat = "contour") + 
  guides(fill = "legend")

#Barplot
ggplot(mpg) + 
  geom_bar(
    aes(
      x = drv, 
      colour = stage(start = drv, after_scale = alpha(colour, 0.5))
    ), 
    fill = NA, size = 4
  )

