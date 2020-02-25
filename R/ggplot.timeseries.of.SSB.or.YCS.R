ggplot.timeseries.of.SSB.or.YCS = function(filenames, models.labels, quant2plot = "SSB", mgt.ref.points = FALSE){

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
  my.p = ggplot(data = my.df) +
  geom_line(mapping = aes(x = year, y = eval(parse(text = name.in.df)), col = model), size = 1.2) +
    xlab("") + theme_light() + ylab(quant2plot)

  if(quant2plot == "SSB") my.p = my.p + ylab("SSB (t)")
  ######################################################################################################################################
  # Add management reference points

  ## For a single filenames

    B0 = extract.quantities(file = filenames[1])$B0
    #print(B0)

  if(mgt.ref.points){

    # Warn user
    print("Adding Management Reference Points with respect to B0 in the first filename")

    # Add managment reference point
    #print("adding mgt.ref.points")
    my.p <- my.p +
      geom_hline(aes(yintercept = 1 * B0),   lwd = 1, lty = 2, col = "lightgrey", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.4 * B0), lwd = 1, lty = 2, col = "lightgrey", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.2 * B0), lwd = 1, lty = 2, col = "lightgrey", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.1 * B0), lwd = 1, lty = 2, col = "lightgrey", show.legend = FALSE) +
      geom_label(aes(min(my.df$year), 1 * B0, label = "B0"), fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.4 * B0, label = "40% B0"), fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.2 * B0, label = "20% B0"), fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.1 * B0, label = "10% B0"), fill = "white")

    #scale_linetype_manual(name = "limits", values=2)
    #values = c(2,2),
    #                        guide = guide_legend(override.aes = list(color = c("green", "red","red"))))
  }

  return(my.p)
}

# Test
#filename1 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN9_ALLsurveyLogisticSigR08/CASAL-MPDoutput.txt"
#filename2 = "C:/Users/kienzlemj/OneDrive - NIWA/Templates/Assessment XYZ/CASAL/RUN10_ALLsurveyLogisticSingleSelectivity/CASAL-MPDoutput.txt"

# Plot TS of SSB from a single model
# ggplot.timeseries.of.SSB.or.YCS(filenames = filename1, models.labels = "model 1", quant2plot = "SSB", mgt.ref.points = TRUE)

# Comparison between 2 estimates
#ggplot.timeseries.of.SSB.or.YCS(filenames = c(filename1, filename2), models.labels = c("model 1", "model 2"), quant2plot = "YCS")
#ggplot.timeseries.of.SSB.or.YCS(filenames = c(filename1, filename2), models.labels = c("model 1", "model 2"), quant2plot = "SSB", mgt.ref.points = TRUE)
