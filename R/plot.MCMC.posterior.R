#################################################################################################################################
# A function to plot the posterior distribution of some model quantities calculated using MCMC
# ARGUMENT filename contains model quantities obtained (casal -v) from MCMC parameters estimates
#          path to the file
#          var a string defining the variable to be plotted

# NOTE eval(parse(text = var)) allows to convert the var string argument into a variable

plot.MCMC.posterior <- function(filename,
                                path = "",
                                label = "NA",
                                var = "NA",
                                x.axis.label = "NA"){

  df <- data.frame(Model = as.factor(label[1]), read.table(paste(path, filename, sep=""), header = TRUE, skip = 8))
  #
  # Plot the posterior distribution obtained by MCMC

  p <- ggplot(df, aes(x=eval(parse(text = var)), fill = Model)) + geom_density(alpha=.3) +
    xlab(parse(text = x.axis.label)) +
    theme(axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5)) + theme_light()


  return(p)
  #return(0)
}
