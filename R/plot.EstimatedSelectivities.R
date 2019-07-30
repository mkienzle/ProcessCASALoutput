#################################################################################################################################
## A function to plot the selectivities
plot.EstimatedSelectivities <- function(filenames, path, label = seq(1, length(filenames))){

  # Load the data
  df <- FromMultipleSelectivityOutputsToDF(filenames, path, label)

  # Plot
  library(ggplot2)

  p <- ggplot(df, aes(x = age, y = proportion, col = Model)) +
    geom_line( size = 1) +
    geom_point( size = 2) + facet_wrap(~ Type) +
    xlab("Age group") + ylab("Proportion retained") +
    scale_x_continuous(minor_breaks = seq(0, 50, 2)) +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5))


  return(p)
}
