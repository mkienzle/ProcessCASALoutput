audit.trail = function(models.path, models.name){
  library(casal)
  library(tidyverse)

  for(i in 1:length(models.path)){

    if(i == 1){
      quantities <- extract.quantities(models.path[i])

      my.df = data.frame(model = models.name[i],
                         year = quantities$SSBs$year,
                         SSB = quantities$"SSBs"$SSB)
    } else {
      quantities <- extract.quantities(models.path[i])

      my.df = rbind(my.df,
                    data.frame(model = models.name[i],
                               year = quantities$SSBs$year,
                               SSB = quantities$"SSBs"$SSB))

    }
  }

  # What is the minimum year (to determine B0) WARNINGS here we assume without checking that it is the same to all models!!!
  min.year = min(my.df$year)

  # What is the largest year common to all models (because we are comparing models biomass in the same year)
  year.for.comp = as_tibble(my.df) %>% group_by(model) %>% summarise(max(year)) %>% ungroup() %>% summarise(min(`max(year)`))

  # Select the data for B0 and Bcurrent
  my.df.sub = as_tibble(my.df) %>% group_by(model) %>% filter(year %in% c(min.year, year.for.comp))

  final.tibble = my.df.sub %>% spread(key = year, value = SSB) %>%
    mutate(!!paste("B", year.for.comp,"/B0", sep="") := round(!!as.symbol(as.character(year.for.comp)) / !!as.symbol(min.year),2))

  return(final.tibble)
}
