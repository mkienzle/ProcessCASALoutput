# CREATED  7 Jul 2020

# Create a set of reusable theme for plotting with ggplot2


# Theme to plot figures for Fisheries Assessments Reports (FAR) written with Microsoft Word
theme_FAR <- function(){

  theme_light() %+replace% # replace elements we want to change

    theme(

      # text elements
      plot.title = element_text(
        size = 30,
        hjust = 0
      ),

      plot.subtitle = element_text(
        size = 25,
        hjust = 0
      ),

      plot.caption = element_text(
        size = 18
      ),

      axis.title = element_text(
        size = 18
      ),

      axis.text = element_text(
        size = 16
      ),

      legend.title = element_text(
        size = 18,
        hjust = 0
      ),

      legend.text = element_text(
        size = 16
      )

    )
}
