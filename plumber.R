#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(pins)
library(dplyr)
library(ggplot2)


#* @apiTitle Fish Mortality API
#* @apiDescription Plumber API for serving fish mortality data.
#* @apiVersion 0.0.3
#* @apiTag Fish

#* @get /
function() {
}

#* Hello World
#* @get /hello
function() {
    print("Hello, World!")
    
}


#### MORTALITY RATES ####

#* Mortality Rates
#* @param zone A string that identifies the zone
#* Could be any of: "All", "PO 1 & 2", "PO 3",  "PO 4", "PO 5",
#* "PO 6", "PO 7", "PO 8", "PO 9", "PO 10", "PO 11", "PO 12 & 13".
#* When selecting a single PO, Norway is always included as a whole for 
#* comparison.
#* @post /mortality_rates

function(zone = "PO 3") {
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/mortality_rates_monthly_data") 
  
  if (zone == "all") {
    
    dat
  }
  
  dat |> 
    filter(sone == zone | sone == "Norge") #Norway is always part of the data
    
  
}

#* Mortality Rates Example Plot
#* @serializer png
#* @param legend_name a string to be used in the legend
#* @get /mortality_rates_plot
function(legend_name = "PA 3") {

  fm_board <- board_connect() 
  dat <-
    pin_read(fm_board, "vi2451/mortality_rates_monthly_data") |>
    filter(sone == "PO 3" |
             sone == "Norge") #Norway is always part of the data
    
    p <- ggplot(data = dat, aes(x = date, y = median, group = sone, color = sone)) +
      geom_ribbon(data = dat, aes(ymin = q1, ymax = q3), fill = "#cccccc", color = NA) +
      geom_line(size = 1) +
      scale_color_manual(values = c("#c4796d", "black"), labels = c("Norway", legend_name)) +
      theme_bw () +
      ylab("Monthly mortality (%)") +
      #coord_cartesian(ylim = c(0, 3)) +
      coord_cartesian(ylim = c(0, 6.5)) +
      scale_y_continuous(breaks=seq(0, 6, by = 1)) +
      scale_x_date(limits = as.Date(c("2019-01-01", "2023-12-01")), expand = c(0,0), date_breaks = "3 months", date_labels = "%b %Y") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=0.5)) +
      theme(legend.position = "top", legend.title = element_blank()) +
      theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())
    print(p)
    
}

#### LOSSES DATA ####

#* Zone Losses Yearly
#* @param year The year, 2019-2022 are available. "All" for all data.
#* @param viz The level at which the data is plotted: "fylke" or "zone".
#* @param species Fish species: "salmon" is available.
#* @post /zone_losses

function(year = "2022",
         viz = "zone",
         species = "salmon") {
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/losses_yearly")
  
  if (year == "All") {
    dat
  }
  
  dat |>
    filter(year == year, viz == viz, species == species)
  
  
}


#* Zone Losses Yearly Example Plot
#* @serializer png
#* @get /zone_losses_yearly_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/losses_yearly") |>
    filter(year == "2022", viz == "zone", species == "salmon")
  
  myPallete <- c('#8dd3c7', '#ffffb3','#bebada','#fb8072')
  
  p <-  ggplot(dat, aes(x = area, y = n, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = myPallete) +
    labs(y = "Antall (millioner)", x = "Område", title = "") +
    theme_minimal()
  
  print(p)
}



#* Zone Mortality Yearly
#* Years 2018-2022 are served.
#* @param viz The level at which the data is plotted: "fylke" or "zone". 
#* "All" for all data
#* @param species Fish species: "salmon" is available.
#* @post /zone_mortality

function(viz = "zone",
         species = "salmon") {
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/losses_yearly_mortality") 
  
  if (viz == "All"){
    dat
  }
  
  dat|>
    filter(viz == viz, species == species)
  
  
}

#* Zone Mortality Example Plot
#* @serializer png
#* @get /zone_mortality_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/losses_yearly_mortality") |>
    filter(viz == "zone", species == "salmon")
  
  p <- ggplot(dat, aes(x = area)) +
    geom_point(aes(y = `2022`, color = "2022"), size = 3) +
    geom_point(aes(y = `2021`, color = "2021"), size = 3) +
    geom_point(aes(y = `2020`, color = "2020"), size = 3) +
    geom_point(aes(y = `2019`, color = "2019"), size = 3) +
    scale_color_manual(values = c(
      "2022" = "#253494",
      "2021" = "#2c7fb8",
      "2020" = "#41b6c4",
      "2019" = "#a1dab4"
    )) +
    labs(
      title = "",
      x = "Område",
      y = "Dødelighet (%)",
      color = "År"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p)
}
