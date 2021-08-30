# #########################################################################
# REACH Bangladesh Age and Disability Inclusion Dashboard -----------------
# 2021 - Ben Smith --------------------------------------------------------
# Shiny App ---------------------------------------------------------------
# #########################################################################

# setwd("C:/Users/Ben SMITH/SynDrive/REACH_BGD/Resources/Templates/R Scripts/irq_mcna_dashboard/irq_mcna_dashboard")
# TODO - make sure that colours match between series on differnt plots.



############################ PREAMBLE ##########################################

# Import packages ---------------------------------------------------------

# spatial data
  library(sf)                     # vector data tools
  # library(lwgeom)               # install.packages("lwgeom")
  library(raster)                 # raster data tools
  library(leaflet)                # plot interactive maps
  library(geojsonio)              # deal with geojson layers # install.packages("geojsonio")
  library(spatialEco)             # calculate zonal statistics # install.packages("spatialEco")
  library(rmapshaper)             # tools for simplifying polygons # install.packages("rmapshaper")
  # library(HatchedPolygons)      # hatched polygon patterns with Leaflet

# additional packages
  library(tidyverse)              # data wrangling packages # install.packages("
  library(shiny)                  # App building tools # install.packages("
  library(shinydashboard)         # calculate zonal statistics # install.packages("shinydashboard")
  library(shinyjs)                # javascript plugin for Shiny # install.packages("shinyjs")
  library(widgetframe)            # app widgets # install.packages("widgetframe")
  library(rsconnect)              # connect to online shiny server # install.packages("
  library(highcharter)            # interactive graph packages # install.packages("highcharter")
  library(readxl)                 # import excel files
  library(lubridate)              # smooth date manipulation
  library(htmltools)              # html scripting for R # install.packages("
  library(expss)                  # vlookup for R # install.packages("expss")
  library(htmlwidgets)
  library(plotly)                 # Nice plots
  library(openxlsx)               # Reading in data

# Read in the styling/function for HTML Chars:
source("Chart Setup Function.R") # chart_funct = ...

# REACH Colours ------------------------------------------------------------

colours = data.frame(
  reach_dark_green  = "#637961",
  reach_light_green = "#E6EFE5",
  reach_green       = "#B0CFAC",
  
  reach_dk_red    = "#782c2e",
  reach_dark_red  = "#A73E3E",
  reach_mddk_red  = "#bf4749",
  reach_red       = "#EE5859",
  reach_pink      = "#f5a6a7",
  reach_light_red = "#FAD5D6",
  
  reach_lt_grey  = "#D1D3D4",
  reach_grey     = "#58585A",
  
  white          = "#FFFFFF")
colour_rank = c(3,11,8,10)


# Load Data ---------------------------------------------------------------

# ---- Shapefiles ---------------------------------------------------------

# Camp shapefile layer
BGD_camps <- st_read("spatial_data/200908_rrc_outline_camp_al1/200908_RRC_Outline_Camp_AL1.shp",
                    options = "ENCODING=UTF-8")
 # FYI extent is: 92.1297,20.9185 : 92.2686,21.2229

# Map Data:
Overview_map_data = read.csv("OverviewMapcsv.csv")

# Join the datasets:
BGD_camps_data <- left_join(BGD_camps, Overview_map_data, by = "Camp_Name")


# ---- Numerical Data -----------------------------------------------------

age_dis_sheets = getSheetNames("20210228 - Dataset Edited for Dashboard 2.xlsx")
age_dis = list()
for(s in age_dis_sheets){
  
  temp = as.data.frame(read.xlsx(xlsxFile = "20210228 - Dataset Edited for Dashboard 2.xlsx",
                          sheet = s, colNames = TRUE, rowNames = TRUE))
  temp[is.na(temp)] = 0
  
  assign(x = s, temp)
}

# Construct legend for landing page map -----------------------------------

color_vector <- c(colours$reach_pink, colours$reach_red, colours$reach_mddk_red, 
                  colours$reach_dark_red, colours$reach_lt_grey)

addLegendCustom <- function(map, colors, labels, opacity = 0.8, title) {

  make_shapes <- function(colors) {
    paste0(colors, "; width:20px; top: 0; height:20px; border:1px solid  white; text-align: center; border-radius:0%")
  }

  make_labels <- function(labels) {
    paste0("<div style='display: inline-block; text-align: center; height:20px; line-height: 20px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors)
  legend_labels <- make_labels(labels)

  return(addLegend(map, colors = legend_colors, labels = legend_labels, 
                   opacity = opacity, position = "bottomright", title = title))
}


# Setup functions for creating horizontal Plotly histograms ----------------

# Shared axis; oposing datasets:
plotly_bar_graph = function(data_grid_left, data_grid_right, plot_title){
  
  
  indicators = colnames(data_grid_left)
  indicators = gsub(pattern = "_", replacement = " ", x = indicators)
  
  p = plot_ly()
  for(r in 1:nrow(data_grid_left)){
    p = p %>% add_trace(type = 'bar', name = rownames(data_grid_left)[r], 
                        x = (unlist(data_grid_left[r,])+unlist(data_grid_right[r,])), y = indicators,
                        marker = list(color = colours[1,colour_rank[r]]),
                        base = -unlist(data_grid_left[r,]))
  }
  p = p %>%  layout(title = plot_title, yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                    font = list(family = "arial", size = 12),
                    xaxis = list(tickformat = "%", range=c(-1,1)),
                    paper_bgcolor = 'rgba(0,0,0,0)')
  return(p)
}


# Dual axis; Opposing datasets:
plotly_bar_graph = function(data_grid_left, left_title, data_grid_right, right_title){
  
  # set up the y axis names:
  indicators = colnames(data_grid_left)
  indicators = gsub(pattern = "_", replacement = " ", x = indicators)
  
  # Set up plots:
  lp = plot_ly() ; rp = plot_ly()
  
  # Add data to plots:
  for(r in 1:nrow(data_grid_right)){
    lp = lp %>% add_trace(type = 'bar',
                          name = rownames(data_grid_left)[r], 
                          x = unlist(data_grid_left[r,]),
                          y = indicators,
                          marker = list(color = colours[1,colour_rank[r]]),
                          showlegend = FALSE)
  }
  
  for(r in 1:nrow(data_grid_left)){
    rp = rp %>% add_trace(type = 'bar', 
                          name = rownames(data_grid_right)[r], 
                          x = unlist(data_grid_right[r,]),
                          y = indicators,
                          marker = list(color = colours[1,colour_rank[r]]))
  }
  
  # format the plot layouts:
  lp <- lp %>% layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(title = left_title, range=c(1,0),
                                   tickformat = "%", tick0 = 0, dtick = 0.2),
                      paper_bgcolor = 'rgba(0,0,0,0)')
  
  rp = rp %>%  layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(title = right_title, range=c(0,1),
                                   tickformat = "%", tick0 = 0, dtick = 0.2),
                      legend = list(orientation = 'h', xanchor = "center", x = 0.5,
                                    y=-0.2),
                      paper_bgcolor = 'rgba(0,0,0,0)')
  
  return(subplot(lp, rp, titleX = TRUE, shareY = TRUE))
}

# Single Trace, Dual Axis:
plotly_bar_graph_dual_axis_single_trace = function(data_grid_left, left_col = 1, left_title,
                                                   data_grid_right, right_col = 1, right_title, 
                                                   data_label){
  
  # Input a dataframe with data in a column with row names containing the y axis labels.
  # If the dataset has multiple columns then specify which to use. Default is column 1.
  
  # data_grid_left = navigating_camps_reason
  # data_grid_right = navigating_camps_reason_pwd
  # left_title = "L"
  # right_title = "R"
  
  # set up the y axis names:
  indicators = rownames(data_grid_left)
  indicators = gsub(pattern = "_", replacement = " ", x = indicators)
  
  # Set up plots:
  lp = plot_ly() ; rp = plot_ly()
  
  # Add data to plots:
  lp = lp %>% add_trace(type = 'bar',
                        x = data_grid_left[,left_col],
                        y = indicators,
                        marker = list(color = colours$reach_red),
                        showlegend = FALSE,
                        width = 0.3)
  
  rp = rp %>% add_trace(type = 'bar', 
                        x = data_grid_right[,right_col],
                        y = indicators,
                        marker = list(color =  colours$reach_red),
                        name = data_label,
                        width = 0.3)
  
  # format the plot layouts:
  lp <- lp %>% layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(title = left_title, range=c(1,0),
                                   tickformat = "%", tick0 = 0, dtick = 0.2),
                      bargap = 0.1, paper_bgcolor = 'rgba(0,0,0,0)')
  
  rp = rp %>%  layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(title = right_title, range=c(0,1),
                                   tickformat = "%", tick0 = 0, dtick = 0.2),
                      legend = list(orientation = 'h'),
                      bargap = 0.1, paper_bgcolor = 'rgba(0,0,0,0)')
  
  return(subplot(lp, rp, titleX = TRUE, shareY = TRUE))
}


# Single axis, single dataset:
plotly_bar_graph_single_trace = function(x_data, y_labels){
  
  # Input a dataframe with data in a column with row names containing the y axis labels.
  # If the dataset has multiple columns then specify which to use. Default is column 1.
  
  # y_labels = rownames(bathing_PWD)
  
  # set up the y axis names:
  y_labels = gsub(pattern = "_", replacement = " ", x = y_labels)
  
  # Set up plots:
  rp = plot_ly()
  
  # Add data to plots:
  rp = rp %>% add_trace(type = 'bar', 
                        x = x_data,
                        y = y_labels,
                        marker = list(color = colours$reach_red),
                        width = 0.3)
  
  rp = rp %>%  layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(y_labels)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(tickformat = "%", tick0 = 0, dtick = 0.2, range = c(0,1.1)),
                      # legend = list(orientation = 'h'),
                      bargap = 0.1, paper_bgcolor = 'rgba(0,0,0,0)')
  
  return(rp)
}


# Generate some standard plotly outputs for adding in to the UI -----------

# ---- Barrier_Mobility_pie ----------------------------------------------

Barrier_Mobility_pie = 
  plot_ly() %>% 
  add_pie(values = c(52, 48),
          hoverinfo = 'text',
          text = c(paste('</br><b>52% of persons with disabilities aged 2 and above</br>reportedly face difficulties moving inside shelters</br>without support from others.</b></br></br>Significantly higher proportions of persons with difficulties</br>in functioning in the self-care, upper-body movement,</br>and mobility domains were reported as facing barriers,</br>compared to persons with difficulties in functioning not</br>in those domains.'),
                   ""),
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_red, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0, 0.5), y = c(0.5, 1))) %>% 
  
  add_pie(values = c(76, 24),
          hoverinfo = 'text',
          text = c(paste('</br><b>76% of persons with disabilities agesd 15 and above</br>reportedly face difficulties moving around camps.</br></b></br>Significantly higher proportions of persons with difficulties</br>in functioning in the self-care and mobility domains</br>were reported as facing barriers, compared to persons with</br>difficulties in functioning not in those domains.'), ""),
          # labels = c("a", "b"), 
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_red, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0.5, 1), y = c(0.5, 1))) %>% 
  
  add_pie(values = c(29, 71),
          hoverinfo = 'text',
          text = c(paste('</br><b>29% of persons without disabilities aged 15 and above</br>reportedly face difficulties moving around camps.</b>'), ""),
          # labels = c("a", "b"), 
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_light_green, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0.25, 0.25), y = c(0., 0.5))) %>% 
  
  add_annotations(x = c(0.18,0.85, 0.51),
                  y = c(0.82, 0.82, 0.18),
                  text = c("52%", "76%", "29%"),
                  # xref = "x",
                  # yref = "y",
                  showarrow = FALSE,
                  ax = 0,
                  ay = 0, 
                  font = list(family = "Arial Narrow", size = 40)) %>% 
  
  layout(font = list(family = "Arial Narrow", size = 20), paper_bgcolor = 'rgba(0,0,0,0)')


# ---- Barrier_WASH_pie --------------------------------------------------

Barrier_WASH_pie = 
  plot_ly() %>% 
  add_pie(values = c(34, 66),
          hoverinfo = 'text',
          text = c(paste('</br><b>34% of persons with disabilities aged 2 and above are</br>reportedly not able to showe/bathe without support from others.</b></br></br>Significantly higher proportions of persons with difficulties in functioning</br>in the self-care, upper-body movement, and mobility domains were</br>reported as facing barriers, compared to persons with</br>difficulties in functioning not in those domains.'),
                   ""),
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_red, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0, 0.5), y = c(0.52, 1))) %>% 
  
  add_pie(values = c(30, 70),
          hoverinfo = 'text',
          text = c(paste('</br><b>30% of persons with disabilities aged 2 and above are reportedly not</br>able to use latrines/go to the toilet without support from others.</br></b></br>Significantly higher proportions of persons with difficulties</br>in functioning in the self-care, upper-body movement,</br>and mobility domains were reported as facing barriers,</br>compared to persons with difficulties in functioning not in</br>those domains'), ""),
          # labels = c("a", "b"), 
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_red, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0.5, 1), y = c(0.52, 1))) %>% 
  
  add_pie(values = c(64, 36),
          hoverinfo = 'text',
          text = c(paste('</br><b>64% of persons with disabilities aged 15 and above</br>reportedly face barriers accessing services.</b></br></br>Significantly higher proportions of persons with difficulties</br>in functioning in the self-care,38 and mobility domains</br>were reported as facing barriers, compared to persons with</br>difficulties in functioning not in those domains.'), ""),
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_red, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0, 0.5), y = c(0, 0.48))) %>% 
  
  add_pie(values = c(39, 61),
          hoverinfo = 'text',
          text = c(paste('</br><b>39% of persons without disabilities aged 15 and above</br>reportedly face barriers accessing services.</b>'), ""),
          # labels = c("a", "b"), 
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_light_green, colours$reach_grey), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'none',
          showlegend = FALSE,
          domain = list(x = c(0.5, 1), y = c(0, 0.48))) %>% 
  
  add_annotations(x = c(0.18, 0.85, 0.18, 0.85),
                  y = c(0.82, 0.82, 0.16, 0.16),
                  text = c("34%", "30%", "64%", "39%"),
                  # xref = "x",
                  # yref = "y",
                  showarrow = FALSE,
                  ax = 0,
                  ay = 0, 
                  font = list(family = "Arial Narrow", size = 40)) %>% 
  
  layout(font = list(family = "Arial Narrow", size = 20), paper_bgcolor = 'rgba(0,0,0,0)')


# ---- Barrier Devices Pie -------------------------------------------------

Barrier_devices_pie = 
  
  plot_ly() %>% 
  
  add_pie(values = c(22, 22, 56),
          labels = c("Any", "Didn't need any", "Didn't recieve any despite needing them"),
          hoverinfo = 'text',
          text = "Persons with disabilities aged 2 and above.",
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_green, colours$reach_grey, colours$reach_red), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'percent', 
          textposition = 'inside', 
          insidetextorientation = "horizontal",
          insidetextfont = list(color = '#FFFFFF'),
          showlegend = TRUE,
          
          domain = list(x = c(0, 0.5), y = c(0.5, 1))) %>%
  
  add_pie(values = c(29, 19, 52),
          labels = c("Any", "Didn't need any", "Didn't recieve any despite needing them"),
          hoverinfo = 'text',
          text = "All older persons with and without disabilities.",
          hole = 0.6, sort = FALSE,
          marker = list(colors = c(colours$reach_green, colours$reach_grey, colours$reach_red), 
                        line = list(color = '#FFFFFF', width = 1)),
          textinfo = 'percent', 
          textposition = 'inside', 
          insidetextorientation = "horizontal",
          insidetextfont = list(color = '#FFFFFF'),
          showlegend = TRUE,
          
          domain = list(x = c(0.5, 1), y = c(0.5, 1))) %>%
  
  add_annotations(x = c(0.25, 0.75),
                  y = c(0.75, 0.75),
                  text = c("</br>Persons</br>with disabilities</br>aged 2 and</br>above.",
                           "</br>All</br>older persons</br>with and</br>without</br>disabilities."),
                  # showarrow = FALSE,
                  ax = 0,
                  ay = 0, 
                  font = list(family = "Arial Narrow", size = 12)) %>% 
  
  layout(font = list(family = "Arial Narrow", size = 18), 
         paper_bgcolor = 'rgba(0,0,0,0)',
         legend = list(x = -0.1, y = 0.4, bgcolor = 'rgba(0,0,0,0)'))


# ---- Barrier Devices Gender Bar -----------------------------------------

data <- data.frame(Groups = c("2-99<br>with Disabilities", "18-59<br>with Disabilities",
                            "60+<br>with Disabilities", "Older Persons<br>without Disabilities",
                            "All older<br>persons"), 
                   Female = c(56, 58, 67, 55, 61), 
                   Male = c(56, 58, 51, 42, 46))

Barrier_Devices_Gender_Bar = 
  plot_ly(data, x = ~Groups, y = ~Female, type = 'bar', name = 'Female',
        marker = list(color = c(colours$reach_grey))) %>%
  add_trace(y = ~Male, name = 'Male', 
            marker = list(color = c(colours$reach_lt_grey))) %>% 
  layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
         xaxis = list(title = "", categoryorder = "array", categoryarray = ~Groups),
         font  = list(family = "arial", size = 12),
         barmode = 'group', paper_bgcolor = 'rgba(0,0,0,0)')


# ---- Enrolment  --------------------------------------------------------

# ---- Education 1 --------------------------------------------------------
data <- data.frame("Groups" = c("Persons with<br>Disabilities",
                              "Persons without<br>Disabilities"), 
                   "TLC_5_9" = c(65, 88), "Other_5_9" = c(59, 93),
                   "TLC_10_14" = c(78, 75), "Other_10_14" = c(73, 82))



Education_1 = subplot(nrows = 1, shareY = TRUE, titleX = TRUE,
  
    plot_ly(data, x = ~Groups, y = ~TLC_5_9, type = 'bar', name = 'TLC',
            marker = list(color = c(colours$reach_grey)),
            showlegend = FALSE) %>%
    add_trace(y = ~Other_5_9, name = 'Other', 
              marker = list(color = c(colours$reach_lt_grey))) %>% 
    layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
           xaxis = list(title = "5-9 Years", categoryorder = "array", categoryarray = ~Groups),
           font  = list(family = "arial", size = 12),
           # legend = list(showlegend = FALSE),
           barmode = 'group', paper_bgcolor = 'rgba(0,0,0,0)'),
    
    plot_ly(data, x = ~Groups, y = ~TLC_10_14, type = 'bar', name = 'TLC',
            marker = list(color = c(colours$reach_grey))) %>%
      add_trace(y = ~Other_10_14, name = 'Other', 
                marker = list(color = c(colours$reach_lt_grey))) %>% 
      layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
             xaxis = list(title = "10-14 Years", categoryorder = "array", categoryarray = ~Groups),
             font  = list(family = "arial", size = 12),
             legend = list(showlegend = FALSE),
             barmode = 'group', paper_bgcolor = 'rgba(0,0,0,0)')
    
    # TODO - Add limitations:
    # Limitations
    # . The data represents enrolment rates irrespective of the degree of 
    # participation of children with and without disabilities in their education. 
    # As such, no inferences on the quality of education for those 
    # enrolled or potential differences in the quality experienced or the 
    # inclusion of children with and without disabilities can be made.
    # . Results represent pre-COVID enrolment rates of children with 
    # disabilities as reported at the time of data collection. They may not 
    # exactly reflect children with disabilities' pre-COVID enrolment 
    # rates (refer to annex 1 for more information).
)



# Education Highest levels (Education 2) ----------------------------------

data1 <- data.frame("Groups" = c("Persons<br>with<br>Disabilities", "Persons<br>without<br>Disabilities"),
                   
                   "None_5_9" = c(29,5),      "PrePrimary_5_9" = c(20,32),
                   "Primary_5_9" = c(23,42),  "Secondary_5_9" = c(0,0),
                   "Madrasha_5_9" = c(12,11), "Learning_Center_5_9" = c(16,11),
                   
                   "None_10_17" = c(22,8),      "PrePrimary_10_17" = c(3,2),
                   "Primary_10_17" = c(53,59),  "Secondary_10_17" = c(10,8),
                   "Madrasha_10_17" = c(12,20), "Learning_Center_10_17" = c(2,3))

Education_2 = 
  subplot(nrows = 1, shareY = TRUE, titleX = TRUE, 
  
    plot_ly(data1, x = ~Groups, type = 'bar', showlegend = FALSE,
            y = ~None_5_9, name = 'None', marker = list(color = c(colours$reach_red)),
            text = paste0(data1$None_5_9, "%"), textposition = "auto") %>% 
      
      add_trace(y = ~PrePrimary_5_9, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                text = paste0(data1$PrePrimary_5_9, "%"), textposition = "auto") %>% 
      add_trace(y = ~Primary_5_9, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                text = paste0(data1$Primary_5_9, "%"), textposition = "auto") %>% 
      add_trace(y = ~Secondary_5_9, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                text = paste0(data1$Secondary_5_9, "%"), textposition = "auto") %>% 
      add_trace(y = ~Madrasha_5_9, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                text = paste0(data1$Madrasha_5_9, "%"), textposition = "auto") %>% 
      add_trace(y = ~Learning_Center_5_9, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                text = paste0(data1$Learning_Center_5_9, "%"), textposition = "auto") %>%
      
      layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
             xaxis = list(title = "5-9 Years", categoryorder = "array", categoryarray = ~Groups),
             font  = list(family = "arial", size = 12),
             barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)'#,
             # legend = list(showlegend = FALSE)
             ),
    
    plot_ly(data1, x = ~Groups, type = 'bar', showlegend = FALSE,
            y = ~None_10_17, name = 'None', marker = list(color = c(colours$reach_red)),
            text = paste0(data1$None_10_17, "%"), textposition = "auto") %>%
      add_trace(y = ~PrePrimary_10_17, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                text = paste0(data1$PrePrimary_10_17, "%"), textposition = "auto") %>%
      add_trace(y = ~Primary_10_17, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                text = paste0(data1$Primary_10_17, "%"), textposition = "auto") %>%
      add_trace(y = ~Secondary_10_17, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                text = paste0(data1$Secondary_10_17, "%"), textposition = "auto") %>%
      add_trace(y = ~Madrasha_10_17, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                text = paste0(data1$Madrasha_10_17, "%"), textposition = "auto") %>%
      add_trace(y = ~Learning_Center_10_17, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                text = paste0(data1$Learning_Center_10_17, "%"), textposition = "auto") %>%
  
      layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
             xaxis = list(title = "10-14 Years", categoryorder = "array", categoryarray = ~Groups),
             font  = list(family = "arial", size = 12),
             # legend = list(showlegend = FALSE),
             barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)')
  )

# Education Highest levels (Education 3) ----------------------------------

data2 <- data.frame("Groups" = c("Female", "Male"),
                   
                   "None_5_9" = c(6,5),      "PrePrimary_5_9" = c(32,31),
                   "Primary_5_9" = c(43,40),  "Secondary_5_9" = c(0,0),
                   "Madrasha_5_9" = c(9,12), "Learning_Center_5_9" = c(11,11),
                   
                   "None_10_17" = c(11,6),      "PrePrimary_10_17" = c(2,2),
                   "Primary_10_17" = c(62,56),  "Secondary_10_17" = c(4,12),
                   "Madrasha_10_17" = c(19,21), "Learning_Center_10_17" = c(2,3))

Education_3 = 
  
  subplot(nrows = 1, shareY = TRUE, titleX = TRUE, 
          
          plot_ly(data2, x = ~Groups, type = 'bar', showlegend = FALSE,
                  y = ~None_5_9, name = 'None', marker = list(color = c(colours$reach_red)),
                  text = paste0(data2$None_5_9, "%"), textposition = "auto") %>% 
            
            add_trace(y = ~PrePrimary_5_9, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                      text = paste0(data2$PrePrimary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Primary_5_9, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                      text = paste0(data2$Primary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Secondary_5_9, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                      text = paste0(data2$Secondary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Madrasha_5_9, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                      text = paste0(data2$Madrasha_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Learning_Center_5_9, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                      text = paste0(data2$Learning_Center_5_9, "%"), textposition = "auto") %>%
            
            layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
                   xaxis = list(title = "5-9 Years", categoryorder = "array", categoryarray = ~Groups),
                   font  = list(family = "arial", size = 12),
                   barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)'),
          
          plot_ly(data2, x = ~Groups, type = 'bar', showlegend = FALSE,
                  y = ~None_10_17, name = 'None', marker = list(color = c(colours$reach_red)),
                  text = paste0(data2$None_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~PrePrimary_10_17, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                      text = paste0(data2$PrePrimary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Primary_10_17, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                      text = paste0(data2$Primary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Secondary_10_17, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                      text = paste0(data2$Secondary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Madrasha_10_17, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                      text = paste0(data2$Madrasha_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Learning_Center_10_17, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                      text = paste0(data2$Learning_Center_10_17, "%"), textposition = "auto") %>%
            
            layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
                   xaxis = list(title = "10-14 Years", categoryorder = "array", categoryarray = ~Groups),
                   font  = list(family = "arial", size = 12),
                   barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)')
  )

# Education Highest levels (Education 4) ----------------------------------
data3 <- data.frame("Groups" = c("Female", "Male"),
                   
                   "None_5_9" = c(19,30),      "PrePrimary_5_9" = c(13,8),
                   "Primary_5_9" = c(40,41),  "Secondary_5_9" = c(4,8),
                   "Madrasha_5_9" = c(14,8), "Learning_Center_5_9" = c(11,4),
                   
                   "None_10_17" = c(8,5),      "PrePrimary_10_17" = c(16,16),
                   "Primary_10_17" = c(53,49),  "Secondary_10_17" = c(3,6),
                   "Madrasha_10_17" = c(14,17), "Learning_Center_10_17" = c(6,7))

Education_4 =
  subplot(nrows = 1, shareY = TRUE, titleX = TRUE, 
          
          plot_ly(data3, x = ~Groups, type = 'bar', showlegend = FALSE,
                  y = ~None_5_9, name = 'None', marker = list(color = c(colours$reach_red)),
                  text = paste0(data3$None_5_9, "%"), textposition = "auto") %>% 
            
            add_trace(y = ~PrePrimary_5_9, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                      text = paste0(data3$PrePrimary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Primary_5_9, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                      text = paste0(data3$Primary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Secondary_5_9, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                      text = paste0(data3$Secondary_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Madrasha_5_9, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                      text = paste0(data3$Madrasha_5_9, "%"), textposition = "auto") %>% 
            add_trace(y = ~Learning_Center_5_9, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                      text = paste0(data3$Learning_Center_5_9, "%"), textposition = "auto") %>%
            
            layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
                   xaxis = list(title = HTML("Persons with<br>Disabilities"), categoryorder = "array", categoryarray = ~Groups),
                   font  = list(family = "arial", size = 12), 
                   barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)'),
          
          plot_ly(data3, x = ~Groups, type = 'bar', showlegend = TRUE,
                  y = ~None_10_17, name = 'None', marker = list(color = c(colours$reach_red)),
                  text = paste0(data3$None_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~PrePrimary_10_17, name = 'Pre-Primary', marker = list(color = c(colours$reach_pink)),
                      text = paste0(data3$PrePrimary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Primary_10_17, name = 'Primary', marker = list(color = c(colours$reach_light_green)),
                      text = paste0(data3$Primary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Secondary_10_17, name = 'Secondary', marker = list(color = c(colours$reach_grey)),
                      text = paste0(data3$Secondary_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Madrasha_10_17, name = 'Madrasha', marker = list(color = c(colours$reach_green)),
                      text = paste0(data3$Madrasha_10_17, "%"), textposition = "auto") %>%
            add_trace(y = ~Learning_Center_10_17, name = 'Learning Center', marker = list(color = c(colours$reach_lt_grey)),
                      text = paste0(data3$Learning_Center_10_17, "%"), textposition = "auto") %>%
            
            layout(yaxis = list(title = '% of Persons per Catagory', range = c(0,100)),
                   xaxis = list(title = "Persons without<br>Disabilities", categoryorder = "array", categoryarray = ~Groups),
                   font  = list(family = "arial", size = 12),
                   # legend = list(orientation = 'h', y=-100), # , xanchor = "center", x = 0.5
                   barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)')
  )


# Write some output text for key findings ---------------------------------

Barrier_key_findings = HTML("<ul>
    <li>Barriers related to mobility in shelters or around camps were reported for 52% and 76% of persons with disabilities, respectively. In particular, persons with difficulties in functioning in the self-care or mobility domains reportedly face barriers moving both inside shelters and around camps. In addition, persons with difficulties in functioning in the upper body movement or vision domains reportedly particularly face barriers moving inside shelters. Mobility-related barriers were also increasingly reported with increasing age.</li>

    <li>Especially persons with difficulties in functioning in the self-care, upper body movement or mobility domains were reportedly unable to use latrines or shower without support from others. Age and gender were further found to compound difficulties with self-care, with particularly high proportions of female older persons with disabilities reportedly being unable to shower or use latrines without support from others.

    </li><li>Reported utilisation of public not accessible latrines or public bathing facilities was particularly low among persons with difficulties in functioning in the self-care or upper body movement domains. At the same time, the reported utilisation of private or accessible latrines – while higher than for other individuals – also remained low among those groups.</li>

    <li>A significantly higher proportion of persons with disabilities than persons without disabilities reportedly faces barriers accessing services. In particular, persons with difficulties in functioning in the self-care or mobility domains as well as female older persons with disabilities reportedly face barriers.</li>

    <li>Overall, more than half the persons with disabilities had reportedly not received any assistive devices in the year prior to data collection despite needing them. This proportion was highest among female older persons with disabilities.
    </ul>")

education_key_findings = HTML("<ul>
<li>Among younger age groups, significantly lower proportions of children with disabilities than children without disabilities were found to have been enrolled into both formal and informal learning centres before their closure due to the COVID-19 outbreak (in March 2020). Overall, 65% of children with disabilities aged 5 to 9 had reportedly attended temporary learning centres (TLCs) for at least 4 days a week prior to the COVID-19 outbreak. In comparison, 88% of children without disabilities of the same age group had reportedly attended TLCs.</li>

<li>While among children without disabilities, the proportion of girls, in particular among older children, reportedly not having been enrolled was higher than that of boys, among children with disabilities, the opposite was true. Overall, only 59% of boys with disabilities 
aged 5 to 14 were reported as having been enrolled in TLCs, compared to 82% of girls with disabilities of the same age group.</li>

<li>Similarly, the proportions of children reportedly not having completed any education were higher among children with disabilities than children without disabilities, in particular among younger age groups, and among boys with disabilities compared to girls with disabilities.</li>


    <li>Overall, more than half the persons with disabilities had reportedly not received any assistive devices in the year prior to data collection despite needing them. This proportion was highest among female older persons with disabilities.
    </ul>")

means_key_findings = HTML("<ul>
<li>The proportions of persons with difficulties functioning in the anxiety or depression domains reportedly having been engaged in the informal sector were at least three times higher than those of persons with difficulties in functioning in other domains, both before the COVID-19 outbreak in March 2020 (pre-COVID) and at the time of data collection (post-COVID). At the same time, findings indicate a greater loss of access to self-reliance activities among persons with disabilities than among persons without disabilities.</li>

<li>Slightly higher proportions of households with persons with disabilities reported at least one child as having been engaged in the informal sector both pre- and post-COVID compared to households without persons with disabilities. The proportion of households with persons with disabilities reporting at least one adult as having been engaged in the informal sector was significantly lower than that of households without persons with disabilities.</li>
                              
<li>Average daily per capita incentives received by households engaged in the informal sector and with persons with disabilities were lower than those of households engaged in the informal sector but without persons with disabilities, in particular post-COVID and among less educated households.</li>
</ul>")

participation_key_findings = HTML("<ul>

<li>Participation in meetings or events did not differ significantly between persons with and without disabilities, or across age groups. Participation did differ between male and female individuals, however, with lower reported participation among female individuals.</li>

<li>Most commonly, individuals had reportedly attended NGO meetings, with especially female individuals largely only having attended those types of meetings. Any other types of meetings that were assessed had reportedly been disproportionately attended by male individuals. The overall gender gap was larger among persons without disabilities than among persons with disabilities.</li>

<li>In terms of having been asked for feedback, differences between disability, age and gender groups were small. Only a person's disability and gender appeared to play a small role. Slightly higher proportions of persons without disabilities than persons with disabilities were reportedly asked for feedback. Moreover, slightly larger proportions of female than male individuals among younger age groups, and slightly larger proportions of male than female individuals among older age groups were reportedly asked for feedback.</li>

</ul>")

Education_trends = HTML(
"<ul>
<li> Overall, results seem to indicate a trend of persons with disabilities being enrolled into education at a later stage than persons without disabilities, rather than not being enrolled at all, while potentially also taking longer or being slightly less likely to complete their education.</li>

<li>With disability being an evolving concept, limited conclusions can be drawn as to whether pre-COVID barriers to accessing education may to some degree have disproportionately affected children with disabilities, e.g. among younger children and boys. Moreover, older individuals may not have been affected by the same functional difficulties when they were younger. As such, findings related to educational attainments cannot necessarily be related to disability at the time when education was obtained.</li>

<li>Findings can neither necessarily be directly related to access to different levels of education among persons with or without disabilities specifically in the camp context, with older individuals in particular potentially having received their education in Myanmar. Nevertheless, the most notable difference between boys and girls in terms of completed education is that 11% of girls with disabilities were reported as having completed education at learning centres compared to 4% of boys with disabilities. With learning centres being the primary form of education in camps, this difference might be indicative of gender differences among persons with disabilities in particular in the camp context.</li>
</ul>")

################################## UI ##########################################

ui <- bootstrapPage(

# Styling -----------------------------------------------------------------

  tags$head(
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("styles.css"), HTML(chart_funct)
  ),


# Set up the basic navbar page layout -------------------------------------

  navbarPage(
    id = "nav",
    windowTitle = "Age and Disability Dashboard",
    HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>Age and Disability Inclusion Needs Assessment - Bangladesh 2020-2021</strong></span>'),

# Overview ----------------------------------------------------------------

# ---- Set up the tab: ----------------------------------------------------

    tabPanel(
      strong("Overview"),
      value = "panel1",
      icon = icon("map-marker"),
      div(
        class = "outer",
        # Put the map as the background to the whole tab:
        leafletOutput('mapOverview', width = "100%", height = "100%"),

# ---- Set up the panel containing the main text: -------------------------


        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          draggable = F,
          top = 60,
          left = 10,
          right = "auto",
          width = 800,

          h5(HTML( # Overview Page Box Title:
            sprintf("<span style='color: %s;'><br><strong>CONTEXT</span></strong>", colours$reach_red))),
          p(HTML("Since August 2017, an estimated 715,000 Rohingya refugees have fled to Cox's Bazar District, Bangladesh, where approximately 860,000 refugees are now residing in 34 camps in Ukhiya and Teknaf Upazilas. In response to the refugee influx, national and international organisations have been delivering humanitarian assistance alongside the government of Bangladesh. In this context, the meaningful and dignified inclusion of individuals across all age groups and persons with disabilities has been incorporated into successive Joint Response Plans in 2019 and 2020. However, while the heightened risk of persons with disabilities and older persons is generally recognized by affected populations and humanitarian actors alike, a lack of data on disability prevalence across camps as well as the specific requirements, barriers and preferences of older persons and persons with disabilities complicates evidence-based inclusive programming.")),
          
          p(HTML("Against this background, REACH, with technical support from the Age and Disability Working Group (ADWG), conducted an Age and Disability Inclusion Needs Assessment across Rohingya refugee populations. The assessment aimed to understand disability prevalence, and to support key actors working in Cox's Bazar, including coordination bodies and technical agencies and actors, to consider the nuanced and specific requirements, access to services and assistance, and involvement of persons with disabilities across all age groups, and older persons living in Rohingya camps, within the response programming. The assessment was coordinated through the ADWG, and implemented with technical contributions from an Age and Disability Task Team (ADTT). The ADTT comprised of the United Nations High Commissioner for Refugees (UNHCR), the International Organization for Migration Needs and Population Monitoring (IOM NPM), the Water, Sanitation and Hygiene (WASH) Sector, and REACH. Technical contributions were further made by Humanity & Inclusion (HI), CBM and the Centre for Disability in Development (CDD), and Prottyashi.")),
          
          h5(HTML(sprintf("<span style='color: %s;'><strong>METHODOLOGY</span></strong>", colours$reach_red))),
          
          p(HTML("The assessment comprised a quantitative household survey and a qualitative component consisting of focus group discussions (FGDs). The quantitative component was implemented in all 34 camps in Ukhiya and Teknaf Upazilas. A stratified cluster sampling approach was employed, with the camps as strata and households as clusters. Information related to disability prevalence was collected through the Washington Group Questions (WGQs)4 on all household members in sampled households aged 2 and above. Information on service utilisation, access barriers and enablers, as well as participation and disaster preparedness was collected on sub-samples of those individuals. Information was collected directly from the concerned individuals themselves, if possible. In all other cases, information was collected by proxy from another adult household member. In total, 2,530 household interviews, covering 11,187 individuals aged 2 and above, were carried out between 30 November 2020 and 7 January 2021. Basic descriptive analysis was conducted, complemented by testing for statistically significant differences in outcomes between persons with and without disabilities, overall as well as for different age groups and genders, by types of functional difficulty, and between households with and without persons with disabilities. The achieved level of representativeness of findings differs by the sub-samples addressed for each question. For detailed information on levels of representativeness, as well as challenges and limitations of the assessment, please refer to annex 1.FGDs were conducted to further contextualise quantitative findings and provide more detailed insights into the specific barriers persons with disabilities and older persons face accessing services, participating in community life and in disaster preparedness, as well as potential solutions. A total of 20 FGDs were conducted with older persons with and without disabilities, adults with disabilities, children with disabilities (aged 11 to 17), and caregivers of children with disabilities, between 12 January and 8 February 2021.")),
          
          p(HTML("However, in all IDP camps and in districts where health risks and/or movement or access restrictions prevented face-to-face interviews (38/62), a non-probability purposive quota sampling approach with a minimum target of 60 surveys per population group was adopted. Due to the non-randomized sampling methodology, findings in these strata are not statistically representative with a quantifiable degree of precision and have to be considered as indicative only. All findings for in-camp IDP households are indicative, given the specific COVID-19 risks in camp settings and subsequent decision to survey this population group through phone-based interviews only. Note that the remote setting of phone-based interviews present additional limitations in terms of asking sensitive or technical questions."))
          #       "<strong>The MCNA dataset can be downloaded here: </strong><a href=\"https://www.reachresourcecentre.info/country/iraq/cycle/28380/?toip-group=data&toip=dataset-database#cycle-28380\"   target=\"_blank\"><img class='icon' src='noun_Download_2120764.svg' style = 'width:15px; height:15px;margin-top:5px'></a>"
          #     )
          #   ),
          #   p(
          #     HTML(
          #       "For MCNA related inquiries, please <a href = \"mailto:anne.flake@reach-initiative.org\">reach out!</a>"
          #     )
          #   )
        ),


# ---- Set up the controls for the map input: -----------------------------

        absolutePanel(
          id = "map_desc",
          class = "panel panel-default",
          fixed = FALSE,
          draggable = F,
          top = 60,
          right = 10,
          left = "auto",
          width = 210,
          h5(HTML(sprintf("<span style='color: %s'><strong> Demographics and Coverage Map</strong></span>", colours$reach_red))),
          
          p(HTML(sprintf("<span style='color: %s'><strong>Data sources:</strong></span><br>Administrative boundaries: ISCG, 2020<br>Demographics and Coverage: REACH", colours$reach_red))),

          selectInput(inputId = "coverage", label = "Dataset:", width = 200,
                      choices = c("Survey Coverage",
                                  "Average HH Size",
                                  "% Married HOH",
                                  "% Male HOH",
                                  "% Households with at least 1 PWD",
                                  "% Households with at least 1 PWD (excl anxiety & depression)",
                                  "% Households with at least 1 elderly",
                                  "% Households with at least 1 PWD or elderly")),
          selected = "Survey Coverage"
        )
      )
    ),


# Barriers ----------------------------------------------------------------

# ---- Set up the tab -----------------------------------------------------

tabPanel(
  title = strong("Service Utilisation, Barriers and Enablers"),
  value = "panel2",
  
  div(class = "navbar1",
      
      navbarPage(
        HTML('<span class="navbar-text" style="font-size: 14px; color: #58585A;"><strong>Indicators</strong></span>'),
        selected = "Mobility",
        
# ---- Mobility -----------------------------------------------------------

        tabPanel(title = "Mobility",  value = "Mobility",
                 
                 absolutePanel(
                   id = "controls",
                   class = "panel panel-default",
                   draggable = F,
                   top = 110,
                   left = 10,
                   right = "auto",
                   width = 500,
                   height = 940,
                   
                   # Title:
                   h5(HTML(sprintf(
                     "<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                     colours$reach_red))),
                   
                   # New paragraph of key findings:
                   p(Barrier_key_findings),
                   
                   Barrier_Mobility_pie),
                 
                 absolutePanel(
                   id = "controls",
                    # class = "panel panel-default",
                    # bottom = 900,
                    top = 110,
                    left = 520,
                    right = "auto",
                    width = 600,
                    height = 940,
                    h5(HTML(sprintf("<span style='color: %s;'><br><strong>Dificultly Moving Around Within Shelters</span></strong>", colours$reach_red))),
                   HTML("<br>TOP: % of persons with disabilities reportedly facing dificulties moving inside shelters without support from others.<br>
                        BOTTOM: % of persons reportedly facing dificulties moving inside shelters without support from others, by reason."),
                    plotlyOutput("barrier_1", height = "800px")),
                  
                  absolutePanel(
                    id = "controls",
                    # class = "panel panel-default",
                    bottom = "auto",
                    top = 110,
                    left = 1130,
                    right = "auto",
                    width = 600,
                    height = 940,
                    
                    h5(HTML(sprintf("<span style='color: %s;'><br><strong>Dificultly Moving Around the Camps</span></strong>", colours$reach_red))),
                    HTML("<br>TOP: % of persons reportedly facing dificulties moving around camps, by age group.<br>
                         BOTTOM: % of persons aged 15 and above reportedly facing dificulties moving around camps, by reason."),
                    plotlyOutput("barrier_2", height = "800px"))),

# ---- Self-Care and WASH Infrastructure ----------------------------------

        tabPanel(title = strong("Self-Care and WASH Infrastructure"), value = "Barrier_WASH",
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   draggable = F,
                   top = 110, 
                   left = 10,
                   right = "auto",
                   width = 500,
                   height = 980,
                   
                   # Title:
                   h5(HTML(sprintf(
                     "<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                     colours$reach_red))),
                     
                   # New paragraph of key findings:
                   p(Barrier_key_findings),
                   
                   Barrier_WASH_pie),
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   bottom = "auto",
                   top = 110,
                   left = 520,
                   right = "auto",
                   width = 500,
                   h5(HTML(sprintf("<span style='color: %s;'><br><strong>Access to Bathing</span></strong>",
                                   colours$reach_red))),
                   HTML("% Of persons with disabilities aged 2 and above reportedly unable to shower/bathe without support from others (top 4 reported reasons)."),
                    plotlyOutput("WASH_1")),
                 
                 absolutePanel(
                   id = "controls", # class = "panel panel-default",
                   bottom = "auto",
                   top = 110,
                   left = 1030,
                   right = "auto",
                   width = 500,
                   h5(HTML(sprintf("<span style='color: %s;'><br><strong>Access to Latrines</span></strong>",
                                    colours$reach_red))),
                   HTML("% Of persons with disabilities aged 2 and above reportedly unable to use latrines/go to the toilet without support from others (top 4 reported reasons)."),
                   plotlyOutput("WASH_2")),
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   bottom = "auto",
                   top = 610,
                   left = 520,
                   right = "auto",
                   width = 1010,
                   height = 480,
                   h5(HTML(sprintf("<span style='color: %s;'><br><strong>Publick and Private Facilities</span></strong>",
                                   colours$reach_red))),
                   HTML("% Of persons with and without disabilities aged 15 and above reportedly having used different WASH services in the month prior to data collection."),
                   plotlyOutput("WASH_3")
                   )# Ab'panel
                 ), # Tab panel


# ---- Access to Assistive Devices -----------------------------------------

        tabPanel(title = strong("Access to Assistive Devices"), value = "Barrier_Devices",
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   draggable = F,
                   top = 110, 
                   left = 10,
                   right = "auto",
                   width = 500,
                   height = 950,
                   
                   # Title:
                   h5(HTML(sprintf(
                     "<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                     colours$reach_red))),
                   
                   # New paragraph of key findings:
                   p(Barrier_key_findings),
                   
                   HTML('</br><b>% Of persons reportedly having received assistive devices in the year prior</br>to data collection.</b></br>'),
                   
                   Barrier_devices_pie),
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   bottom = "auto",
                   top = 110,
                   left = 520,
                   right = "auto",
                   width = 900,
                   height = 470,
                   h5(HTML(sprintf("<span style='color: %s;'><br><strong>Devices Recieved</span></strong>",
                                   colours$reach_red))),
                   HTML("% of persons with disabilities aged 15 and above and persons without disabilities over the the age of 60 reportedly having recieved assistive devices in the year prior to data collection."),
                   
                   plotlyOutput("Devices_1")),
                 
                 absolutePanel(
                   id = "controls",
                   # class = "panel panel-default",
                   bottom = "auto",
                   top = 590,
                   left = 520,
                   right = "auto",
                   width = 900,
                   height = 470,
                   h5(HTML(sprintf("<span style='color: %s;'><br><strong>Devices not Recieved</span></strong>",
                                   colours$reach_red))),
                   HTML("% of persons with disabilities aged 2 and above and persons without disabilities over the the age of 60 reportedly not having recieved assistive devices despite needing them in the year prior to data collection."),
                   plotlyOutput("Devices_2")
                 )# Ab'panel
        ) # Tab panel
      ) # Navbar Page
    ) # Div
  ),# Barriers tab panel


# Enrolment --------------------------------------------------------------

  # Tab
tabPanel(title = strong("Enrolement in Education"), value = "Education",
         
         
absolutePanel(
  id = "clear_red_box",
  bottom = "auto", top = 60, left = 620,# right = "auto",
  width = 1050, height = 990),

# Key Findings box
absolutePanel(
  id = "controls", top = 60, 
  left = 10, right = "auto",
  width = 600, height = 990,
  # Title:
  h5(HTML(sprintf("<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                  colours$reach_red))),
  education_key_findings,
  # h5(HTML(sprintf("<span style='color: %s;'><br><strong>Enrolement</span></strong>", colours$reach_red))),
  HTML("<br><br><b>% of children with and without disabilities aged 5 to 14 reportedly having attended a TLC for at least 4 days a week or having attended home-based learning activities, a madrassa or moktab ('Other') prior to the closure of education centres due to COVID-19, by age group.</b><br>"), 
  Education_1),

absolutePanel(
  id = "clear_clear_box",
  bottom = "auto", top = 60, left = 620, right = "auto",
  width = 500, height = 500,
  h5(HTML(sprintf("<span style='color: %s;'><br><strong>Enrolement by Age Group and Gender</span></strong>",
                  colours$reach_red))),
  HTML("% of children with and without disabilities aged 5 to 17 by reported highest level of completed education, by age group and gender."),
  Education_3),
  # plotlyOutput("Education_2_link", height = "800px")),

absolutePanel(
  id = "clear_clear_box",
  bottom = "auto", top = 60, left = 1130, right = "auto",
  width = 520, height = 600,
  h5(HTML(sprintf("<span style='color: %s;'><br><strong>Enrolement by Age Group</span></strong>",
                  colours$reach_red))),
  HTML("% of children aged 5 to 17 by reported highest level of completed education,</br>by age group."), 
  Education_2),
              
absolutePanel(
  id = "clear_clear_box",
  bottom = "auto", top = 540, left = 620, right = "auto",
  width = 640, height = 500,
  h5(HTML(sprintf("<span style='color: %s;'><br><strong>Enrolement by Gender</span></strong>",
                  colours$reach_red))),
  HTML("% of children with and without disabilities aged 5 to 17 by reported highest level of completed<br>education, by gender."),  
  Education_4),

absolutePanel(
  id = "clear_clear_box",
  bottom = "auto", top = 540, left = 1220, right = "auto",
  width = 450, height = 500,
  h5(HTML(sprintf("<span style='color: %s;'><br><strong>Trends and Limitations</span></strong>",
                  colours$reach_red))),
  Education_trends)

), # tab

# Means of Living ---------------------------------------------------------

# Tab - Means of living
tabPanel(title = strong("Means of Living"), value = "Means",
         
         absolutePanel(id = "controls", top = 60, 
                       left = 10, right = "auto",
                       width = 500,# height = 950,
                       # Title:
                       h5(HTML(sprintf("<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                                       colours$reach_red))),
                       
                       means_key_findings
                       )
# key findings
# Percentage of persons engaged bar chart
# Average incentives tables
         ),


# Participation -----------------------------------------------------------

tabPanel(title = strong("Participation"), value = "Participation",
         
         absolutePanel(id = "controls", top = 60, 
                       left = 10, right = "auto",
                       width = 500,# height = 500,
           # Title:
           h5(HTML(sprintf("<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
                           colours$reach_red))),
           
           participation_key_findings
         ) # absolutePanel
         ) # tabPanel


) # navbar
) # UI


################################## SERVER ######################################

server <- function(input, output, session){

# Build landing page map --------------------------------------------------

  output$mapOverview <- renderLeaflet({

    # variable from selected population group
    select_group <- case_when(
      input$coverage == "Survey Coverage" ~ "Coverage",
      input$coverage == "Average HH Size" ~ "hh_size",
      input$coverage == "% Married HOH" ~ "hoh_marital.married",
      input$coverage == "% Male HOH" ~ "i.final_hoh_gender.male",
      input$coverage == "% Households with at least 1 PWD" ~ "i.disabled_hh.yes",
      input$coverage == "% Households with at least 1 PWD (excl anxiety & depression)" ~ "i.disabled_incomplete_hh.yes",
      input$coverage == "% Households with at least 1 elderly" ~ "i.elderly_hh.yes",
      input$coverage == "% Households with at least 1 PWD or elderly" ~ "i.elderly_disabled_hh.yes")#,
      # TRUE ~ "All")


    # put selected group into a value for colours and tooltip
    coverage_value <- BGD_camps_data[[select_group]]


# ---- Map Pop ups --------------------------------------------------------

    # Set the labels for the map:
    labels = c(summary(coverage_value)[c(2,3,5,6)], NA)
    if(select_group == "hh_size"){
      labels = format(round(x = labels, digits = 1), nsmall = 1)
    } else {labels = round(x = labels, digits = 0)}

    # This is making the html popups for the map:
      # Each of the arguments takes the place of a %s in the quotes
    coverage_tooltip <- sprintf(
      '<strong><span style="font-size: 20px; color: %s">%s</span></strong><br>
      <strong><span style="font-size: 15px; color: %s;">%s</span></strong><br>
      <span style ="color: %s;">%s</span><br>
      <span style ="color: %s;">%s</span>',
      colours$reach_red,
      BGD_camps_data$Camp_Name,
      colours$reach_grey,
      BGD_camps_data$Upazila,
      colours$reach_grey,
      paste0(input$coverage, ":"),
      colours$reach_grey,
      coverage_value) %>% 
      
      lapply(htmltools::HTML)


# ---- Render the map -----------------------------------------------------

    # Set up the basic arguments:
    irq_map <- leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        doubleClickZoom = TRUE,
        zoomSnap = 0.01,
        zoomDelta = 0.01,
        attributionControl = FALSE,
        dragging = TRUE,
        scrollWheelZoom = FALSE,
        easeLinearity = 0.35,
        minZoom = 8.0, # zoomed out
        maxZoom = 15) # zoomed in
      ) %>%

      # Add the base map layer from carto DB:
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(opacity = 1)) %>%

      # Add the camp boundary layer
      addPolygons(
        data         = BGD_camps_data,
        color        = colours$white,
        fillColor    = case_when(
          (coverage_value > 0          & coverage_value < labels[1]) ~ colours$reach_pink, # reach_pink
          (coverage_value >= labels[1] & coverage_value < labels[2]) ~ colours$reach_red, # reach_red
          (coverage_value >= labels[2] & coverage_value < labels[3]) ~ colours$reach_mddk_red, # reach_mddk_red
          (coverage_value >= labels[3]) ~ colours$reach_dark_red, # reach_dark_red
          TRUE ~ colours$reach_lt_grey),
        label        =  coverage_tooltip,
        weight       = 0.2,
        smoothFactor = 0.5,
        opacity      = 1,
        fillOpacity  = 0.9,
        options      = list(zIndex = 400),
        highlightOptions = highlightOptions(
          fillColor    = colours$white,
          color        = colours$white,
          weight       = 2,
          opacity      = 0.9,
          fillOpacity  = 0.4,
          bringToFront = T
        ),
        labelOptions = labelOptions(
          noHide      = F,
          opacity     = 0.9,
          direction   = 'left',
          offset      = c(-10, 0),
          textOnly    = F,
          style       = list("padding" = "3px 8px",
                             "font-family" = "Arial Narrow")
        )
      ) %>%

      # Add custom legend and title:
      addLegendCustom(colors = color_vector, labels = labels, title = input$coverage) %>%
      # addLegend(title = input$coverage) %>%

      # Add scale
      addScaleBar(position = "bottomright", scaleBarOptions(imperial = FALSE)) %>%

    # Set initial view coordinates;
    setView(lng = 92.1,
            lat = 21.0707,
            zoom = 11.5)
  })


# Barriers ----------------------------------------------------------------


# ---- Mobility -------------------------------------------------------------
    
  output$barrier_1 <- renderPlotly({
    
    col_index = c(1:5,8,7)
    
    subplot(shareX = TRUE, nrows = 2, heights = c(0.25,0.75),
            
            plotly_bar_graph_dual_axis_single_trace(
              data_grid_left = navigating_inside_shelters,
              left_col = 9,
              data_grid_right = navigating_inside_shelters_pwd, 
              right_col = 9, #grep(x = colnames(navigating_inside_shelters), pattern = "yes")],
              left_title = "Persons without Disabilities", 
              right_title = "Persons with Disabilities",
              data_label = "Persons reporting difficulties"),
            
            plotly_bar_graph(data_grid_left = navigating_inside_shelters[,col_index], 
                             data_grid_right = navigating_inside_shelters_pwd[,col_index],
                             left_title = "Persons without Disabilities", 
                             right_title = "Persons with Disabilities")
                
            )
    
  })
  
  output$barrier_2 <- renderPlotly({
    
    col_index = c(1:6,8,7)
    
    subplot(shareX = TRUE, nrows = 2, heights = c(0.25,0.75),
            
            plotly_bar_graph_dual_axis_single_trace(
              data_grid_left = navigating_camps_reason,
              data_grid_right = navigating_camps_reason_pwd, 
              right_col <- left_col <- grep(x = colnames(navigating_camps_reason), pattern = "yes"),
              left_title = "Persons without Disabilities", 
              right_title = "Persons with Disabilities",
              data_label = "Persons reporting difficulties"),
            
            plotly_bar_graph(data_grid_left = navigating_camps_reason[,col_index],
                             data_grid_right = navigating_camps_reason_pwd[,col_index],
                             left_title = "Persons without Disabilities", 
                             right_title = "Persons with Disabilities")
            )
    })


# ---- Self-Care and WASH Infrastructure ----------------------------------

  output$WASH_1 <- renderPlotly({
    
    plotly_bar_graph_single_trace(x_data = bathing_PWD[,3],
                                          y_labels = rownames(bathing_PWD))
  })
  
  
  output$WASH_2 <- renderPlotly({
    
    plotly_bar_graph_single_trace(x_data = latrines_PWD[c(6,4,7,5),3],
                                  y_labels = rownames(latrines_PWD)[c(6,4,7,5)])
    
  })
  
  output$WASH_3 <- renderPlotly({
    
    plotly_bar_graph(data_grid_left = facilities_public,
                     data_grid_right = facilities_private,
                     left_title = "Public Facilities", 
                     right_title = "Private Facilities")
  })
  

# ---- Devices ------------------------------------------------------------

  output$Devices_1 <- renderPlotly({
    
    plotly_bar_graph(data_grid_left = Assistive_devices, 
                     left_title = "Persons without Disabilities",
                     data_grid_right = Assistive_devices_PWD, 
                     right_title = "Persons with Disabilities")
        
  })
  
  
  output$Devices_2 <- renderPlotly({Barrier_Devices_Gender_Bar})


# Enrolment --------------------------------------------------------------

  output$Education_2_link <- renderPlotly({
    
    subplot(Education_3, 
            Education_2,
            Education_4, 
            plotly_empty(type = 'scatter', mode = 'markers'),
            shareY = T, nrows = 2, margin = 0.15, titleX = TRUE)})
  
}


# RUN APP -----------------------------------------------------------------

# runApp(shinyApp(ui, server), launch.browser = TRUE)

shinyApp(ui, server)
