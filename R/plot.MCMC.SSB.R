plot.MCMC.SSB <- function(mcmcfilename,
                          path,
                          mgt.ref.points = TRUE,
                          ref.points.label.x.axis = NA,
                          projection.from = NA){

  # ARGUMENTS
  # mcmcfilename
  # path
  # mgt.ref.points: if TRUE, adds horizontal lines representing 3 fraction of Bo
  # ref.points.label.x.axis the location on the x-axis of the label describing the reference point
  # projection.from: a numeric giving the year from when the projections start
  #########################################################################################################################
  # Get all the MCMC data

  library(tidyverse)
  #mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilename, path = path))

  # Load the MCMC file
  #library(casal)
  #mcmc.InputValues = extract.mcmc(samples =  mcmcfilename, path = path)
  mcmc.InputValues = read.table( file = paste(path, mcmcfilename, sep="/"), skip = 8, header = TRUE)

  # Reset the class of the object to matrix to allow manipulation
  #class(mcmc.InputValues) <- "matrix"

  # Get only columns containing SSB
  mcmc.InputValues.subset = mcmc.InputValues[, grep("SSB", dimnames(mcmc.InputValues)[[2]])]
  #rm(mcmc.InputValues)

  # Convert data input from wide to long format
  library(tidyr)
  mcmc.InputValues.long = as_tibble(gather(data = as.data.frame(mcmc.InputValues.subset)))

  # Subset the data for string that look like "SSB" with year in square brackets, for example SSB[1997]
  #mcmc.SSB.data = mcmc.data %>% filter(grepl("SSB\\[[0-9]{4}\\]", key))
  #mcmc.SSB.data = mcmc.InputValues.long %>% filter(grepl("SSB\\[[0-9]{4}\\]", key))
  mcmc.SSB.data = mcmc.InputValues.long %>% filter(grepl("SSB.[0-9]{4}", key))
  #########################################################################################################################

  # Extract year from the label(new.df = mcmc.SSB.data %>% dplyr::mutate(year =  as.numeric(str_extract(key, "[0-9]{4}"))))
  SSB.data = mcmc.SSB.data %>% dplyr::mutate(year =  as.numeric(str_extract(key, "[0-9]{4}")))

  # A function to calculate the quantiles of the distribution
  mean_cl_quantile <- function(x, q = c(0.025, 0.975), na.rm = TRUE){
    dat <- data.frame(y = mean(x, na.rm = na.rm),
                      ymin = quantile(x, probs = q[1], na.rm = na.rm),
                      ymax = quantile(x, probs = q[2], na.rm = na.rm))
    return(dat)
  }

  # Plot
  my.p = ggplot(data = SSB.data, aes(x = year, y = value)) +
    stat_summary(geom = "line", fun.y = median, col = "black", size = 1.2) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.1, lty = 3) +
    xlab("") + ylab("SSB (t)") +
    theme_light() + expand_limits(y = 0) + theme(axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14))

  # Add horizontal lines representing managment reference points
  if(mgt.ref.points){

    # Calculate virgin biomass
    #median.B0 = as.numeric(mcmc.InputValues %>% filter(grepl("B0", key)) %>% summarize(median(value)))
    median.B0 = median(mcmc.InputValues[, grep("^B0", dimnames(mcmc.InputValues)[[2]])])

    # location of the labels on the x-axis
    if(is.na(ref.points.label.x.axis)){lab.x.axis = 1980} else{lab.x.axis = ref.points.label.x.axis}

    # create a data.frame with label info
    lbl.df = data.frame(x = lab.x.axis, y = c(0.1, 0.2, 0.4) * median.B0, lbl = c("10% B0", "20% B0", "40% B0"), lty = c(1, 2, 3))

    # Add lines to plot
    my.p <- my.p +
      geom_hline(aes(yintercept = y), data = lbl.df, lwd = 1, lty = lbl.df$lty, col = "red", show.legend = FALSE) +
      geom_label( mapping = aes(x = x, y = y, label = lbl), data = lbl.df, fill = "white")

  }

  # If data are from projection, colour the range of years corresponding to the projection
  if(!is.na(projection.from)){
    my.p = my.p +
      stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.5, lty = 3,
      data = subset(SSB.data, year >= projection.from), aes(x = year, y = value), colour = "yellow", fill = "yellow") +
      stat_summary(geom = "line", fun.y = median, col = "black", size = 1.2)
  }

  return(my.p)

}


# test
# plot.MCMC.SSB("quant.v1", "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic")
