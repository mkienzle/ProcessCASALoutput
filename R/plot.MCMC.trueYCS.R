plot.MCMC.trueYCS <- function(mcmcfilenames, path, model.labels){

  # ARGUMENTS
  # mcmcfilenames
  # path
  # model.labels


  # Get all the MCMC data
  library(tidyverse)

  mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[1], path = path))
  #print(mcmc.data)

  # Subset the data for string that look like "true_YCS" with year in square brackets, for example true_YCS[1997]
  mcmc.YCS.data = mcmc.data %>% filter(grepl("^true_YCS\\[[0-9]{4}\\]", key))
  #print(mcmc.YCS.data)

  # Extract year
  YCS.data = mcmc.YCS.data %>% dplyr::mutate(year =  str_extract(key, "[0-9]{4}"))

  # Add the model name
  YCS.data = add_column(YCS.data, model = model.labels[1])

  if(length(mcmcfilenames) > 1) {

    for(i in 2:length(mcmcfilenames)){

      tmp.mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[i], path = path))
      #print(mcmc.data)

      # Subset the data for string that look like "true_YCS" with year in square brackets, for example true_YCS[1997]
      tmp.mcmc.YCS.data = tmp.mcmc.data %>% filter(grepl("^true_YCS\\[[0-9]{4}\\]", key))
      #print(mcmc.YCS.data)

      # Extract year
      tmp.YCS.data = tmp.mcmc.YCS.data %>% dplyr::mutate(year =  str_extract(key, "[0-9]{4}"))

      # Add the model name
      tmp.YCS.data = add_column(tmp.YCS.data, model = model.labels[i])

      # bind all data together
      YCS.data = rbind(YCS.data, tmp.YCS.data)

    }
  }
  # Plot
  my.p = ggplot(data = YCS.data) +
    geom_boxplot(aes(x = year, y = value, col = model, fill = model)) +
    # stat_summary(geom = "line", fun.y = median, col = "black", size = 1.2) +
    # stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.1, lty = 3) +
    xlab("") + ylab("Year Class Strength (true_YCS)") +
    theme_light()

  return(my.p)



}


# test
# (plot.MCMC.trueYCS("quant.v1", "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic", model.labels = "RUN8_ALLsurveyLogistic"))
#(plot.MCMC.trueYCS(mcmcfilenames = c("quant.v1", "quant.v1"), "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic", model.labels = c("RUN8_ALLsurveyLogistic", "test2")))
