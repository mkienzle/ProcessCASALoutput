#

ggplot.TimeseriesOfAbundanceOverlayedWithFit <- function(datafile = NA, path = NA,
                                                         AbundIndex.Name = NA, y.axis.lab = "", xlab = c(2000, 2018),
                                                         ylim = c(0, 1200), increment = 1e2,
                                                         title = "",
                                                         x.axis.step = 5, add.line = TRUE){

  # Useful library
  library(casal)

  # Get the data
  observations <- get(AbundIndex.Name[1], extract.fits(datafile[1], path))

  df <- data.frame( year = observations$year, mean = observations$obs, sd = observations$error.value * observations$obs,
                    model = observations$fits)

  if(length(datafile)>1){
    for(i in 2:length(datafile)){
      observations <- get(AbundIndex.Name[i], extract.fits(datafile[i], path))
      df <- rbind(df, data.frame( year = observations$year, mean = observations$obs, sd = observations$error.value * observations$obs,
                                  model = observations$fits))

    }
  }
  print(df)

  #  df1 <- data.frame(Type = "data", year = df$year, mean = df$mean)
  #  df1 <- rbind(df1, data.frame(Type = "model", year = df$year, mean = df$model))

  df1 <- data.frame(Type = "Observed", year = df$year, mean = df$mean)
  df1 <- rbind(df1, data.frame(Type = "Expected", year = df$year, mean = df$model))

  p <- ggplot(df1, aes(x = year, y = mean, col = Type)) + geom_point(aes(shape = Type), size = 3) +
    geom_errorbar(data = df, mapping = aes(x = year, ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd), width = 0.75, col = "#F8766D") +
    ggtitle(title) + xlab("") + ylab(y.axis.lab) +
    #    xlab("") + ylab(parse(text = expression(paste(y.axis.lab, plain() %+-% plain() , " 95% CI")))) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          legend.title=element_text(size=18),
          legend.text=element_text(size=16),
          panel.grid.major = element_line(size = 1),
          panel.grid.minor = element_line(size = 0.5)) +
    scale_x_continuous(breaks = seq(xlab[1], xlab[2], by = x.axis.step), limits = c(xlab[1]-1, xlab[2]+1), minor_breaks = seq(1990,2020)) +
    scale_y_continuous(breaks = seq(ylim[1], ylim[2], by = increment), limits = ylim)

  if(add.line) p <- p +  geom_line(data = df, aes(x = year, y = model), col = "#00BFC4", lwd = 1)

  p = p + theme_light()
  # Plot
  # p <- ggplot(df, aes(x = year, y = mean)) +
  #   scale_x_continuous(minor_breaks = seq(1990,2020)) + scale_y_continuous(breaks = seq(0, 1.2 * max(df$mean), by = 1e2), limits = c(0, 1.2 * max(df$mean))) +
  #   geom_point(size = 3, col = "#F8766D") +
  #   geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.75, col = "#F8766D") +
  #   xlab("") + ylab(y.axis.lab) +
  #   theme(axis.text=element_text(size=16),
  #         axis.title=element_text(size=18,face="bold"))
  #

  # p <- ggplot() +
  #   geom_point(data = df, aes(x = year, y = mean, group = 1), size = 3, col = "#F8766D") +
  #   geom_line(data = df, aes(x = year, y = model), col = "#00BFC4", lwd = 1) +
  #   geom_point(data = df, aes(x = year, y = model), col = "#00BFC4", size = 3) +
  #   geom_errorbar(data = df, mapping = aes(x = year, ymin = mean - sd, ymax = mean + sd), width = 0.75, col = "#F8766D") +
  #   xlab("") + ylab(y.axis.lab) +
  #   theme(axis.text=element_text(size=16),
  #          axis.title=element_text(size=18,face="bold")) +
  #   scale_x_continuous(minor_breaks = seq(1990,2020)) +
  #   scale_y_continuous(breaks = seq(ylim[1], ylim[2], by = 1e2), limits = ylim) +
  #   scale_color_manual(name = "Legend", values=c("#00BFC4", "#F8766D"), labels = c("Company Last Year", "Overall"))

  #+
  #  scale_x_continuous(minor_breaks = seq(1990,2020)) + scale_y_continuous(breaks = seq(0, 1.2 * max(df$mean), by = 1e2), limits = c(0, 1.2 * max(df$mean)))

  return(p)


}
