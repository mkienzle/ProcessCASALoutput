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
                                x.axis.label = "NA",
                                xlim = c(0,4),
                                x.breaks.major.steps = 0.4,
                                x.breaks.minor.steps = 0.2,
                                lognormal.prior = c(NA,NA)){


  # WARNINGS: there is an ad-hoc number of samples drawn from the theoretical distribution
  for(i in 1:length(filename)){
    if(i == 1){
  df <- data.frame(Distribution = as.factor(label[i]), read.table(paste(path, filename[i], sep=""), header = TRUE, skip = 8))
    } else {
      df = rbind(df,
                 data.frame(Distribution = as.factor(label[i]), read.table(paste(path, filename[i], sep=""), header = TRUE, skip = 8)) )
    }
  }
  # If the user wants to plot the prior
  if(! is.na(lognormal.prior[1])){

    #print(head(df))

    # Generate a random sample from the theoretical distribution
    # WARNINGS: there is an ad-hoc number of samples drawn from the theoretical distribution
    x = rlnorm(1e5, meanlog = lognormal.prior[1], sdlog = lognormal.prior[2])
    tmp.df = data.frame(Distribution = "Prior", x = x)
    dimnames(tmp.df)[[2]] = c("Distribution", var)

    #print(head(tmp.df))
    df = rbind(df[, c("Distribution", var)], tmp.df)

  }

  library(ggplot2)
  p <- ggplot(df, aes(x=eval(parse(text = var)), fill = Distribution)) + geom_density(alpha=.5) +
    xlab(x.axis.label) + scale_x_continuous(breaks = seq(xlim[1], xlim[2], x.breaks.major.steps), minor_breaks = seq(xlim[1], xlim[2], x.breaks.minor.steps), limits = c(xlim[1], xlim[2])) +
    theme_light() +
    theme(axis.title.x = element_text(size = rel(1.8)), axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14))

  p = p + theme(legend.position = "bottom")


  return(p)
}
