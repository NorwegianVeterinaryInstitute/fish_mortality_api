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
#* @apiVersion 0.0.5
#* @apiTag Fish

#* Hello World
#* @get /hello
function() {
    print("Hello, World!")
    
}

#### YEARLY DATA ####

#### LOSSES AND MORTALITY DATA ####

#* Losses and Mortality Yearly
#* @param year:string The years 2019-2023 are available. Use "All" to get all data in which case the other arguments are ignored.
#* @param viz:string The level at which the data is plotted: "fylke" or "zone".
#* @param species:string Fish species: "salmon" and "rainbowtrout".
#* @serializer json
#* @post /losses_mortality_yearly

function(year = "2022",
         viz = "zone",
         species = "salmon") {
  
  # evaluate the arguments
  .year <- year
  .viz <- viz
  .species <- species
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/losses_yearly_data")
  
  if (.year == "All") {
    dat
  } else {
    dat |>
      filter(year == .year, viz == .viz, species == .species)
  }
  
}


#* Zone Losses Yearly Example Plot
#* @serializer png
#* @get /zone_losses_yearly_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/losses_yearly_data") |>
    filter(year == "2022", viz == "zone", species == "salmon")
  
  myPallete <- c('#8dd3c7', '#ffffb3','#bebada','#fb8072')
  
  p <-  ggplot(dat, aes(x = area, y = n, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = myPallete) +
    labs(y = "Antall (millioner)", x = "Område", title = "") +
    theme_minimal()
  
  print(p)
}

#* Zone Mortality Example Plot
#* Please note: this plot is being build with the same dataset as
#* above - zone_losses, thus another endpoint is not needed. 
#* @serializer png
#* @get /zone_mortality_year_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/mortality_yearly_data") |>
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



#### MONTHLY DATA ####

#### LOSSES ####

#* Zone Losses Monthly
#* @param year_month:string Months for years 2019-2023 are available.  Use "All" to get all data in which case the other arguments are ignored.
#* @param viz:string The level at which the data is plotted: "fylke" or "zone".
#* @param species:string Fish species: "salmon" and "rainbowtrout".
#* @serializer json
#* @post /losses_monthly

function(year_month = "2022-03",
         viz = "zone",
         species = "salmon") {
  
  # evaluate the arguments
  .year_month <- year_month
  .viz <- viz
  .species <- species
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/losses_monthly_data_wrangled")
  
  if (.year_month == "All") {
    dat
  } else {
    dat |>
      filter(year_month == .year_month, viz == .viz, species == .species)
  }
  
}


#* Zone Losses Monthly Example Plot
#* @serializer png
#* @get /zone_losses_monthly_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/losses_monthly_data_wrangled") |>
    filter(year_month == "2022-03", viz == "zone", species == "salmon")
  
  myPallete <- c('#8dd3c7', '#ffffb3','#bebada','#fb8072')
  
  p <-  ggplot(dat, aes(x = area, y = n, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = myPallete) +
    labs(y = "Antall (millioner)", x = "Område", title = "") +
    theme_minimal()
  
  print(p)
}

#### MORTALITY RATES ####

#* Mortality Rates
#* @param zone:string A string that identifies the zone. Could be any of: "All", "1 & 2", "3", "4", "5","6", "7", "8", "9", "10", "11", "12 & 13".When selecting a single production zone, Norway is always included as a whole for comparison.
#* @post /mortality_rates_monthly

function(zone = "3") {
  
  # evaluate the arguments
  .zone <- zone
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/mortality_rates_monthly_data")
  
  if (.zone == "All") {
    dat <- dat |>
      filter(viz %in% c("zone",  "all")) #Norway is always part of the data
    
  } else {
    dat <- dat |>
      filter(area == .zone |
               area == "Norge") #Norway is always part of the data
  }
  
  dat
  
}

#* Mortality Rates Example Plot
#* @serializer png
#* @param legend_name a string to be used in the legend
#* @get /mortality_rates_monthly_plot
function(legend_name = "PA 3") {

  fm_board <- board_connect() 
  dat <-
    pin_read(fm_board, "vi2451/mortality_rates_monthly_data") |>
    filter(area == "PO 3" |
             area == "Norge") #Norway is always part of the data
    
    p <- ggplot(data = dat, aes(x = date, y = median, group = area, color = area)) +
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

#### COHORTS ####

#* Cohorts
#* @param year:string The years 2019-2023 are available. Use "All" to get all data in which case the other arguments are ignored.
#* @param species:string Fish species: "salmon" is available
#* @param viz:string The level at which the data is plotted: "fylke" or "zone".
#* @post /cohorts

function(year = "2022",
         viz = "zone",
         species = "salmon") {
  
  # evaluate the arguments
  .year <- year
  .viz <- viz
  .species <- species
  
  fm_board <- board_connect()
  
  dat <- pin_read(fm_board, "vi2451/mortality_cohorts_data")
  
  if (.year == "All") {
    dat 
    
  } else {
    dat |>
      filter(year == .year, viz == .viz, species == .species)
  }
  

  
}


#* Cohorts Example Plot
#* @serializer png
#* @get /cohorts_plot
function() {
  fm_board <- board_connect()
  dat <- pin_read(fm_board, "vi2451/mortality_cohorts_data") |>
    filter(year == "2022", viz == "zone", species == "salmon")
  
  p <- ggplot(dat) +
    geom_segment(
      aes(color = area, x = area, xend = area, y = q1, yend=q3), size = 10) +
    geom_point(
      aes(x = area, y = mort, group = year),
      pch = 10, size = 9, fill = "black", stroke = 0.2
    )
    
  
  print(p)
}
