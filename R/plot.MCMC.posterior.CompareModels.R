#################################################################################################################################
# A function to plot the posterior distribution of some model quantities calculated using MCMC
# ARGUMENT filename contains model quantities obtained (casal -v) from MCMC parameters estimates
#          path to the file
#          var a string defining the variable to be plotted

# NOTE eval(parse(text = var)) allows to convert the var string argument into a variable

plot.MCMC.posterior.CompareModels <- function(filenames, path, label = seq(1, length(filenames)),
                                              var = "NA",
                                              x.axis.label = "NA"){
  # Load the data
  for(i in 1:length(filenames))
    assign(paste("df", i, sep="."), read.table(paste(path, "/", filenames[i], sep=""), header = TRUE, skip = 8))

  # Find out the variable that are the same in all models output
  common.var <- names(df.1)
  for(i in 2:length(filenames)) common.var <- intersect(common.var, names(get(paste("df", i, sep="."))))

  df <- data.frame(Model = as.factor(label[1]), read.table(paste(path, "/", filenames[1], sep=""), header = TRUE, skip = 8)[, common.var])
  #
  for(i in 2: length(filenames)){
    tmp.df <- read.table(paste(path, "/", filenames[i], sep=""), header = TRUE, skip = 8)[,common.var]

    df <- rbind(df, data.frame(Model = as.factor(label[i]), tmp.df))
  }
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

# Example
# plot.MCMC.posterior.CompareModels(filenames = c(filename1, filename2),
#                                   path = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ\\CASAL",
#                                   var = "SSB.2019./B0")
