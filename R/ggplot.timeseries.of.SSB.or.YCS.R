ggplot.timeseries.of.SSB.or.YCS = function(filenames, models.labels, quant2plot = "SSB"){

  #
  library(casal)

  # Resolve some naming issues
  name.in.input.file = grep(quant2plot, names(extract.quantities(filenames[1])), value = TRUE)
  #print(name.in.input.file)


  my.df = data.frame(with(extract.quantities(filenames[1]), eval(parse(text = name.in.input.file))), model = models.labels[1])

  #print(head(my.df))

  # Deal with more files if pass in arg
  if(length(filenames) > 1){

    for(i in 2:length(filenames)){
      my.df = rbind(my.df,
                    data.frame(with(extract.quantities(filenames[i]), eval(parse(text = name.in.input.file))), model = models.labels[i]))
    }
  }

  #print(head(my.df))

  # Resolve some naming issues again
  #print(names(my.df))
  name.in.df = grep(substr(quant2plot,1,6), names(my.df), value = TRUE)

  # Plot
  library(ggplot2)
  my.p = ggplot(data = my.df, aes(x = year, y = eval(parse(text = name.in.df)), col = model)) + geom_line() +
    xlab("") + ylab(quant2plot) + theme_light()

  return(my.p)
}

# Test
#filename1 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN9_ALLsurveyLogisticSigR08/CASAL-MPDoutput.txt"
#filename2 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN10_ALLsurveyLogisticSingleSelectivity/CASAL-MPDoutput.txt"

#ggplot.timeseries.of.SSB.or.YCS(filenames = c(filename1, filename2), models.labels = c("model 1", "model 2"), quant2plot = "YCS")
#ggplot.timeseries.of.SSB.or.YCS(filenames = c(filename1, filename2), models.labels = c("model 1", "model 2"), quant2plot = "SSB")
