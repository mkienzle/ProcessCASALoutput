#################################################################################################################################
# A function to plot the age frequency distribution in samples overlayed with model prediction
# ARGUMENT: obj is an element of the object created from a CASAL standard output file using the function extract.fits in the casal package

plot.PropAtAgeOverlayedWithFit <- function(obj, obj2, obj3, obj4, model.label = 1:4, legend.position = "top"){

  library(ggplot2)
  library(scales)

  if(missing(obj2)){
    # Convert obj into a data.frame
    obj.df <- FromObjToDF(obj)

    # Plot
    p1 <- ggplot(obj.df, aes( x = age.group, y = observations, fill = "Observations")) +
      geom_col(width=.75) +
      geom_line(aes(x = age.group, y = predictions, group = 1, col = "Predictions"), lwd = 1) +
      facet_wrap( ~ year, ncol = 4) +
      xlab("Age group") + ylab("Proportions") +
      scale_fill_manual(name = '  ', guide = 'legend', values="black") +
      scale_x_continuous(minor_breaks = seq(0, 50, 1), breaks = seq(0,50,4)) +
      scale_colour_manual(name = ' ', values=c("Predictions" = hue_pal()(1), "Observations" = "black")) +
      theme(legend.position = "top", axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14),
            panel.grid.major = element_line(size = 1.1),
            panel.grid.minor = element_line(size = 0.5)) + theme_light() + theme(legend.position = legend.position)

    return(p1)
  } # End working only with obj

  if(missing(obj3)){
    # Convert obj into a data.frame
    obj.df <- FromObjToDF(obj)
    obj2.df <- FromObjToDF(obj2)

    obj.df <- rbind(data.frame("Model" = as.factor(model.label[1]), obj.df[c("year","age.group", "observations","predictions")]),
                    data.frame("Model" = as.factor(model.label[2]), year = obj2.df$year,
                               age.group = obj2.df$age.group,
                               observations = 0,
                               predictions = obj2.df$predictions))

    # Set the order of the factors
    # Plot
    p1 <- ggplot(obj.df, aes( x = age.group, y = observations, fill = "Observations")) +
      geom_col(width=.75) +
      geom_line(stat = "identity", aes(x = age.group, y = predictions, group = Model, col = Model, lty = Model), lwd = 1) +
      facet_wrap( ~ year, ncol = 4) +
      xlab("Age group") + ylab("Proportions") +
      scale_fill_manual(name = '  ', guide = 'legend', values="black") +
      scale_x_continuous(minor_breaks = seq(0, 50, 1), breaks = seq(0,50,4)) +
      #scale_colour_manual(name = ' ', values=c("Observations" = "black")) +
      #, as.factor(model.label[1]) = hue_pal()(3)[1],
      #                                         as.factor(model.label[2]) = hue_pal()(3)[2],
      #                                         as.factor(model.label[3]) = hue_pal()(3)[3])) +
      theme(legend.position = "top", axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14),
            panel.grid.major = element_line(size = 1.1),
            panel.grid.minor = element_line(size = 0.5)) + theme_light() + theme(legend.position = legend.position)

    return(p1)
  } # End working only with obj and obj2

  if(missing(obj4)){
    # Convert obj into a data.frame
    obj.df <- FromObjToDF(obj)
    obj2.df <- FromObjToDF(obj2)
    obj3.df <- FromObjToDF(obj3)

    obj.df <- rbind(data.frame("Model" = as.factor(model.label[1]), obj.df[c("year","age.group", "observations","predictions")]),
                    data.frame("Model" = as.factor(model.label[2]), year = obj2.df$year,
                               age.group = obj2.df$age.group,
                               observations = 0,
                               predictions = obj2.df$predictions),
                    data.frame("Model" = as.factor(model.label[3]), year = obj3.df$year,
                               age.group = obj3.df$age.group,
                               observations = 0,
                               predictions = obj3.df$predictions))

    # Set the order of the factors
    # Plot
    p1 <- ggplot(obj.df, aes( x = age.group, y = observations, fill = "Observations")) +
      geom_col(width=.75) +
      geom_line(stat = "identity", aes(x = age.group, y = predictions, group = Model, col = Model, lty = Model), lwd = 1) +
      facet_wrap( ~ year, ncol = 4) +
      xlab("Age group") + ylab("Proportions") +
      scale_fill_manual(name = '  ', guide = 'legend', values="black") +
      scale_x_continuous(minor_breaks = seq(0, 50, 1), breaks = seq(0,50,4)) +
      #scale_colour_manual(name = ' ', values=c("Observations" = "black", "1" = hue_pal()(3)[1], "2" = hue_pal()(3)[2], "3" = hue_pal()(3)[3])) +
      theme(legend.position = "top", axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14),
            panel.grid.major = element_line(size = 1.1),
            panel.grid.minor = element_line(size = 0.5)) + theme_light() + theme(legend.position = legend.position)

    return(p1)
  } # End working only with obj, obj2 and obj3

  # Convert obj into a data.frame
  obj.df <- FromObjToDF(obj)
  obj2.df <- FromObjToDF(obj2)
  obj3.df <- FromObjToDF(obj3)
  obj4.df <- FromObjToDF(obj4)

  obj.df <- rbind(data.frame("Model" = as.factor(model.label[1]), obj.df[c("year","age.group", "observations","predictions")]),
                  data.frame("Model" = as.factor(model.label[2]), year = obj2.df$year,
                             age.group = obj2.df$age.group,
                             observations = 0,
                             predictions = obj2.df$predictions),
                  data.frame("Model" = as.factor(model.label[3]), year = obj3.df$year,
                             age.group = obj3.df$age.group,
                             observations = 0,
                             predictions = obj3.df$predictions),
                  data.frame("Model" = as.factor(model.label[4]), year = obj4.df$year,
                             age.group = obj4.df$age.group,
                             observations = 0,
                             predictions = obj4.df$predictions))

  # Set the order of the factors
  # Plot
  p1 <- ggplot(obj.df, aes( x = age.group, y = observations, fill = "Observations")) +
    geom_col(width=.75) +
    geom_line(stat = "identity", aes(x = age.group, y = predictions, group = Model, col = Model, lty = Model), lwd = 1) +
    facet_wrap( ~ year, ncol = 4) +
    xlab("Age group") + ylab("Proportions") +
    scale_fill_manual(name = '  ', guide = 'legend', values="black") +
    scale_x_continuous(minor_breaks = seq(0, 50, 1), breaks = seq(0,50,4)) +
    #scale_colour_manual(name = ' ', values=c("Observations" = "black", "1" = hue_pal()(3)[1], "2" = hue_pal()(3)[2], "3" = hue_pal()(3)[3])) +
    theme(legend.position = "top", axis.title.y = element_text(size = rel(1.8)), axis.text = element_text(size = 14),
          panel.grid.major = element_line(size = 1.1),
          panel.grid.minor = element_line(size = 0.5))

  return(p1)
  # End working only with obj, obj2, obj3 and obj4
}
