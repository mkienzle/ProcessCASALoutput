ggplot.timeseries.of.SSB.relativeTo.B0 = function(filenames, models.labels, mgt.ref.points = TRUE, legend.position = "right"){

  #
  library(casal)

  # Resolve some naming issues
  name.in.input.file = grep("SSB", names(extract.quantities(filenames[1])), value = TRUE)
  #print(name.in.input.file)


  my.df = data.frame(with(extract.quantities(filenames[1]), eval(parse(text = name.in.input.file))), model = models.labels[1])
  B0 = extract.quantities(file = filenames[1])$B0
  my.df = within(my.df, {
    SSB.div.B0 = SSB / B0
  })

  #print(head(my.df))

  # Deal with more files if pass in arg
  if(length(filenames) > 1){

    for(i in 2:length(filenames)){
      tmp.df = data.frame(with(extract.quantities(filenames[i]), eval(parse(text = name.in.input.file))), model = models.labels[i])
      B0 = extract.quantities(file = filenames[i])$B0
      tmp.df = within(tmp.df, {
        SSB.div.B0 = SSB / B0
      })

      my.df = rbind(my.df, tmp.df)
    }
  }

  #print(head(my.df))

  # Resolve some naming issues again
  #print(my.df)
  #print(names(my.df))
  name.in.df = grep("SSB", names(my.df), value = TRUE)

  # Plot
  library(ggplot2)
  my.p = ggplot(data = my.df) +
    geom_line(mapping = aes(x = year, y = SSB.div.B0, col = model), size = 1.2) +
    scale_y_continuous(breaks = seq(0,1, 0.2), minor_breaks = seq(0,1, 0.1), labels = scales::percent) +
    xlab("") + theme_light() + ylab(expression(SSB/B[0])) + expand_limits(y = 0) + # modified here
    theme(legend.position = legend.position, axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14))


  #if(quant2plot == "SSB") my.p = my.p + ylab("SSB (t)")
  ######################################################################################################################################
  # Add management reference points

  ## For a single filenames

  B0 = extract.quantities(file = filenames[1])$B0
  #print(B0)

  if(mgt.ref.points){

    # Warn user
    #print("Adding Management Reference Points with respect to B0 in the first filename")

    # Add managment reference point
    #print("adding mgt.ref.points")
    my.p <- my.p +
      #geom_hline(aes(yintercept = 1 * B0),   lwd = 1, lty = 2, col = "lightgrey", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.4), lwd = 1.3, lty = 2, col = "green", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.2), lwd = 1.3, lty = 2, col = "orange", show.legend = FALSE) +
      geom_hline(aes(yintercept = 0.1), lwd = 1.3, lty = 2, col = "red", show.legend = FALSE) +
      #geom_label(aes(min(my.df$year), 1 * B0, label = "B0"), fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.4, label = "40% B0"), col = "green", fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.2, label = "20% B0"), col = "orange", fill = "white") +
      geom_label(aes(min(my.df$year) + 1, 0.1, label = "10% B0"), col = "red", fill = "white")

    #scale_linetype_manual(name = "limits", values=2)
    #values = c(2,2),
    #                        guide = guide_legend(override.aes = list(color = c("green", "red","red"))))
  }

  return(my.p)
}
