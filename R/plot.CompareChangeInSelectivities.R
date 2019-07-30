#################################################################################################################################
## A function to plot the selectivities
plot.CompareChangeInSelectivities <- function(filenames, path, label = seq(1, length(filenames)), keep = "all"){

  # Load the data
  df <- FromMultipleSelectivityOutputsToDF(filenames, path, label)


  if(keep != "all") df <- subset(df, Type %in% keep)

  print(head(df))

  # Plot
  library(ggplot2)

  p <- ggplot(df, aes(x = age, y = proportion, col = Type)) +
    geom_line( aes(linetype = Model), size = 1) +
    geom_point( size = 2) +
    xlab("Age group") + ylab("Proportion retained") +
    scale_x_continuous(minor_breaks = seq(0, 50, 2)) +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5))


  return(p)
}
