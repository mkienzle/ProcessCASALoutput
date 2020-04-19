plot.multiple.MCMC.SSB <- function(mcmcfilenames,
                                   path,
                                   labels,
                                   mgt.ref.points = TRUE,
                                   ref.points.label.x.axis = NA,
                                   projection.from = NA){

  # ARGUMENTS
  # mcmcfilenames
  # path
  # labels to be associated with data in each file
  # mgt.ref.points: if TRUE, adds horizontal lines representing 3 fraction of Bo
  # ref.points.label.x.axis the location on the x-axis of the label describing the reference point
  # projection.from: a numeric giving the year from when the projections start
  #########################################################################################################################
  # Get all the MCMC data

  library(tidyverse)
  #mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilename, path = path))

  # Load the MCMC files
  for(file.nb in 1:length(mcmcfilenames)){

    # with first file, initialise the data frame
    if(file.nb == 1){
      mcmc.InputValues = read.table( file = paste(path, mcmcfilenames[file.nb], sep="/"), skip = 8, header = TRUE)

      # Get only columns containing SSB
      mcmc.InputValues.subset = mcmc.InputValues[, grep("SSB", dimnames(mcmc.InputValues)[[2]])]

      # Convert data input from wide to long format
      library(tidyr)
      mcmc.InputValues.long = as_tibble(gather(data = as.data.frame(mcmc.InputValues.subset)))
      mcmc.SSB.data = mcmc.InputValues.long %>% filter(grepl("SSB.[0-9]{4}", key))
      mcmc.SSB.data = data.frame(Projections = labels[file.nb], mcmc.SSB.data)

    }
    # After the first file, add the data to the data frame
    else {

      tmp.mcmc.InputValues = read.table( file = paste(path, mcmcfilenames[file.nb], sep="/"), skip = 8, header = TRUE)

      # Get only columns containing SSB
      tmp.mcmc.InputValues.subset = tmp.mcmc.InputValues[, grep("SSB", dimnames(tmp.mcmc.InputValues)[[2]])]

      # Convert data input from wide to long format
      library(tidyr)
      tmp.mcmc.InputValues.long = as_tibble(gather(data = as.data.frame(tmp.mcmc.InputValues.subset)))
      tmp.mcmc.SSB.data = tmp.mcmc.InputValues.long %>% filter(grepl("SSB.[0-9]{4}", key))
      tmp.mcmc.SSB.data = data.frame(Projections = labels[file.nb], tmp.mcmc.SSB.data)

      #print(head(mcmc.SSB.data))
      #print(head(tmp.mcmc.SSB.data))
      mcmc.SSB.data = rbind(mcmc.SSB.data, tmp.mcmc.SSB.data)

    }

  } # End of loop over files
  #########################################################################################################################

  # Extract year from the label(new.df = mcmc.SSB.data %>% dplyr::mutate(year =  as.numeric(str_extract(key, "[0-9]{4}"))))
  SSB.data = mcmc.SSB.data %>% dplyr::mutate(year =  as.numeric(str_extract(key, "[0-9]{4}")))

  #return(SSB.data)

  # A function to calculate the quantiles of the distribution
  mean_cl_quantile <- function(x, q = c(0.025, 0.975), na.rm = TRUE){
    dat <- data.frame(y = mean(x, na.rm = na.rm),
                      ymin = quantile(x, probs = q[1], na.rm = na.rm),
                      ymax = quantile(x, probs = q[2], na.rm = na.rm))
    return(dat)
  }

  median.by.type = function(df){

    tmp.df = with(df, aggregate(cbind("y"=value), by = list(x = year, labels = labels), median))
    return(tmp.df$y)
  }

  #print(head(median.by.type(SSB.data)))
  print(table(SSB.data$labels))
  # Plot
  my.p = ggplot(data = SSB.data, mapping = aes(x = year, y = value, colour = Projections)) +
    stat_summary(geom = "line", fun.y = median, size = 1.2, col = "black",
                 data = subset(SSB.data, year <= projection.from)) +
    #stat_summary(data = SSB.data, mapping = aes(x = year, y = value, group = labels, col = labels),
    #             geom = "line", fun.y = median.by.type, size = 1.2)
    #stat_summary(geom = "line", fun.y = median.by.type, col = "black", size = 1.2) #+
    stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.1, lty = 3) +
    xlab("") + ylab("SSB (t)") +
    theme_light() + expand_limits(y = 0) + theme(legend.position = "bottom")

  # Add horizontal lines representing managment reference points
  if(mgt.ref.points){

    # Calculate virgin biomass from the first file (implicit is that all files have the same B0)
    #median.B0 = as.numeric(mcmc.InputValues %>% filter(grepl("B0", key)) %>% summarize(median(value)))
    median.B0 = median(mcmc.InputValues[, grep("^B0", dimnames(mcmc.InputValues)[[2]])])

    # location of the labels on the x-axis
    if(is.na(ref.points.label.x.axis)){lab.x.axis = 1980} else{lab.x.axis = ref.points.label.x.axis}

    # create a data.frame with label info
    lbl.df = data.frame(x = lab.x.axis, y = c(0.1, 0.2, 0.4) * median.B0,
                        lbl = c("10% B0", "20% B0", "40% B0"),
                        lty = c(1, 2, 2),
                        my.col = c("red", "orange", "green"))

    # Add lines to plot
    my.p <- my.p +
      geom_hline(aes(yintercept = y), data = lbl.df, lwd = 1, lty = lbl.df$lty, col = lbl.df$my.col, show.legend = FALSE) +
      geom_label( mapping = aes(x = x, y = y, label = lbl), data = lbl.df, fill = "white", col = "black")

  }

  # If data are from projection, colour the range of years corresponding to the projection
  if(!is.na(projection.from)){
    my.p = my.p +
      stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.2, lty = 3,
                   data = subset(SSB.data, year >= projection.from), aes(x = year, colour = Projections, fill = Projections)) +
      stat_summary(geom = "line", fun.y = median, size = 1.2,
                   data = subset(SSB.data, year >= projection.from), aes(x = year, colour = Projections), alpha = 1)
  }

  return(my.p)

}
