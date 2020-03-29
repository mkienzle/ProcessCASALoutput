#################################################################################################################################
# A function to plot the trace of an MCMC chain
# ARGUMENT filename contains model quantities obtained (casal -v) from MCMC parameters estimates
#          path to the file
#          var a string defining the variable to be plotted

# NOTE eval(parse(text = var)) allows to convert the var string argument into a variable

plot.MCMC.chain <- function(filename,
                            path = "",
                            var = "NA",
                            y.axis.label = "NA"){
  library(ggplot2)

  # Plot MCMC results in R
  #print(paste(path, "/", filename, sep=""))
  Model.Quantities.from.MCMC <- read.table(paste(path, filename, sep=""), header = TRUE, skip = 8)

  # # Add a counter of samples to the data
  df <- within(Model.Quantities.from.MCMC, {
    x = seq(1, length(eval(parse(text = var))))
  })
  #
  # Plot of the MCMC chain for var

  # Calculate a 90% Confidence interval
  #quantile(with(df, B0), c(5, 95)/100)

  p <- ggplot(df, aes(x = x, y = eval(parse(text = var)))) + geom_line() +
    stat_summary(fun.y=mean, aes(x = 1, yintercept = ..y..), geom = "hline", colour="red", lwd = 1.8, lty = 2) +
    stat_smooth(method = "loess", formula = y ~ x, span = 0.05, size = 1, level = 0, lwd = 2.5, col = "orange") +
    xlab("sample") + ylab(parse(text = y.axis.label)) +
    theme(legend.position = "none",
          axis.title.x = element_text(size = rel(1.8)),
          axis.title.y = element_text(angle = 90, vjust = 0.5, size = rel(1.8)),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5)) + theme_light()

  return(p)
  #return(0)
}
