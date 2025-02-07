library(data.table)
library(arrow)
library(dplyr)
library(tidyr)
library(shiny)
library(shinyjs)
library(ggplot2)
library(ggiraph)
library(shinybusy)
library(colorspace)
library(hablar)
library(countrycode)
options(scipen=999999)
colorred="#ca0020"
colorgreen="#1a9641"
colorblue="#0571b0"
#scales::muted("green")

map_theme <- function() {
    theme_bw() +
        theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            strip.text = element_text(size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            panel.border = element_blank(),
            strip.background = element_rect(fill = "white", colour = "white")
  )
}

trend_theme <- function() {
    theme_classic() +
        theme(
            text = element_text(size=16),
            legend.direction = "horizontal",
            legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
            legend.position = "bottom"
            )
}

bar_theme <- function(){
    ggthemes::theme_clean() +
        theme(
            text = element_text(size=16),
            ##text=element_text(size=input$plotlabelsize),
            axis.text.y = element_text(size=16),
            axis.text.x = element_text(angle=90, vjust=.5, hjust=1, size=16),
            legend.position="bottom",
            legend.key.size = unit(0.35, 'cm'),
            plot.margin=unit(c(0.5,2,0.5,0.5), 'cm'),
            plot.caption = element_text(hjust=0))
        }
