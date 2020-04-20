plot.MCMC.trueYCS = function (mcmcfilenames, path, model.labels)
{
  library(tidyverse)
  #mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[1],
  #                                                 path = path))
  mcmc.data = read.table(file = paste(path, mcmcfilenames[1], sep = "/"), skip = 8, header = TRUE)
  mcmc.data.subset = mcmc.data[, grep("true_YCS", dimnames(mcmc.data)[[2]])]
  library(tidyr)
  mcmc.data.long = as_tibble(gather(data = mcmc.data.subset))
  mcmc.YCS.data = mcmc.data.long %>% filter(grepl("^true_YCS",
                                                  key))
  YCS.data = mcmc.YCS.data %>% dplyr::mutate(year = str_extract(key,
                                                                "[0-9]{4}"))
  YCS.data = add_column(YCS.data, model = model.labels[1])

  if (length(mcmcfilenames) > 1) {
    for (i in 2:length(mcmcfilenames)) {
      # tmp.mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[i],
      #                                                      path = path))
      # tmp.mcmc.YCS.data = tmp.mcmc.data %>% filter(grepl("^true_YCS\\[[0-9]{4}\\]",
      #                                                    key))
      # tmp.YCS.data = tmp.mcmc.YCS.data %>% dplyr::mutate(year = str_extract(key,
      #                                                                       "[0-9]{4}"))
      # tmp.YCS.data = add_column(tmp.YCS.data, model = model.labels[i])
      tmp.mcmc.data = read.table(file = paste(path, mcmcfilenames[i], sep = "/"), skip = 8, header = TRUE)
      tmp.mcmc.data.subset = tmp.mcmc.data[, grep("true_YCS", dimnames(tmp.mcmc.data)[[2]])]

      tmp.mcmc.data.long = as_tibble(gather(data = tmp.mcmc.data.subset))
      tmp.mcmc.YCS.data = tmp.mcmc.data.long %>% filter(grepl("^true_YCS",
                                                              key))
      tmp.YCS.data = tmp.mcmc.YCS.data %>% dplyr::mutate(year = str_extract(key,
                                                                            "[0-9]{4}"))
      tmp.YCS.data = add_column(tmp.YCS.data, model = model.labels[i])

      YCS.data = rbind(YCS.data, tmp.YCS.data)
    }
  }
  my.p = ggplot(data = YCS.data) + geom_boxplot(aes(x = year,
                                                    y = value, col = model, fill = model)) + xlab("") +
    ylab("Year Class Strength (true_YCS)") + theme_light() +
    theme(legend.position = "bottom", axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14)) +
    scale_x_discrete(breaks = seq(1970.0, 2020.0, 5.0))
  #+
  #  scale_x_discrete(breaks = seq(xlab[1], xlab[2], by = x.axis.step), limits = c(xlab[1]-1, xlab[2]+1))
  return(my.p)
}

#
# plot.MCMC.trueYCS <- function(mcmcfilenames, path, model.labels){
#
#   # ARGUMENTS
#   # mcmcfilenames
#   # path
#   # model.labels
#
#
#   # Get all the MCMC data
#   library(tidyverse)
#
#   mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[1], path = path))
#   #print(mcmc.data)
#
#   # Subset the data for string that look like "true_YCS" with year in square brackets, for example true_YCS[1997]
#   mcmc.YCS.data = mcmc.data %>% filter(grepl("^true_YCS\\[[0-9]{4}\\]", key))
#   #print(mcmc.YCS.data)
#
#   # Extract year
#   YCS.data = mcmc.YCS.data %>% dplyr::mutate(year =  str_extract(key, "[0-9]{4}"))
#
#   # Add the model name
#   YCS.data = add_column(YCS.data, model = model.labels[1])
#
#   if(length(mcmcfilenames) > 1) {
#
#     for(i in 2:length(mcmcfilenames)){
#
#       tmp.mcmc.data = as_tibble(extract.mcmc.To.LongFormat(mcmcfilename = mcmcfilenames[i], path = path))
#       #print(mcmc.data)
#
#       # Subset the data for string that look like "true_YCS" with year in square brackets, for example true_YCS[1997]
#       tmp.mcmc.YCS.data = tmp.mcmc.data %>% filter(grepl("^true_YCS\\[[0-9]{4}\\]", key))
#       #print(mcmc.YCS.data)
#
#       # Extract year
#       tmp.YCS.data = tmp.mcmc.YCS.data %>% dplyr::mutate(year =  str_extract(key, "[0-9]{4}"))
#
#       # Add the model name
#       tmp.YCS.data = add_column(tmp.YCS.data, model = model.labels[i])
#
#       # bind all data together
#       YCS.data = rbind(YCS.data, tmp.YCS.data)
#
#     }
#   }
#   # Plot
#   my.p = ggplot(data = YCS.data) +
#     geom_boxplot(aes(x = year, y = value, col = model, fill = model)) +
#     # stat_summary(geom = "line", fun.y = median, col = "black", size = 1.2) +
#     # stat_summary(geom = "ribbon", fun.data = mean_cl_quantile, alpha = 0.1, lty = 3) +
#     xlab("") + ylab("Year Class Strength (true_YCS)") +
#     theme_light()
#
#   return(my.p)
#
#
#
# }
#
#
# # test
# # (plot.MCMC.trueYCS("quant.v1", "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic", model.labels = "RUN8_ALLsurveyLogistic"))
# #(plot.MCMC.trueYCS(mcmcfilenames = c("quant.v1", "quant.v1"), "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN8_ALLsurveyLogistic", model.labels = c("RUN8_ALLsurveyLogistic", "test2")))
