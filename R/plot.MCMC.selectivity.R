plot.MCMC.selectivity <- function(mcmcfilename, path, selectivity.label){

  # ARGUMENTS
  # mcmcfilename
  # path

  # Get all the MCMC data
  library(tidyverse)
  mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilename, path = path))

  # extract age
  mcmc.selectivity.data = mcmc.data %>% filter(grepl(paste("selectivity\\[", selectivity.label, "\\]", sep=""), key))

  # Extract year from the label(new.df = mcmc.SSB.data %>% dplyr::mutate(year =  as.numeric(str_extract(key, "[0-9]{4}"))))
  selectivity.data = mcmc.selectivity.data %>% dplyr::mutate(tmp =  str_extract(key, "\\[[0-9]{1,2}\\]"))  %>% dplyr::mutate(age =  as.numeric(str_extract(tmp, "[0-9]{1,2}")))


  # A function to calculate the quantiles of the distribution
  mean_cl_quantile <- function(x, q = c(0.025, 0.975), na.rm = TRUE){
    dat <- data.frame(y = mean(x, na.rm = na.rm),
                      ymin = quantile(x, probs = q[1], na.rm = na.rm),
                      ymax = quantile(x, probs = q[2], na.rm = na.rm))
    return(dat)
  }

  # Plot
  my.p = ggplot(data = selectivity.data, aes(x = age, y = value)) +
    stat_summary(geom = "line", fun.y = median, col = "black", size = 1.2) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.1, lty = 3) +
    xlab("age-group") + ylab("Proportion retained") +
      labs(caption = selectivity.label) +
    theme_light()


  return(my.p)



}


# test
# plot.MCMC.selectivity("quant.v1", "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic")
