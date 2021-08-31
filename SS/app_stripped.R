# #########################################################################
# REACH Bangladesh Age and Disability Inclusion Dashboard -----------------
# 2021 - Ben Smith --------------------------------------------------------
# Shiny App ---------------------------------------------------------------
# #########################################################################

# setwd("C:/Users/Ben SMITH/SynDrive/REACH_BGD/Resources/Templates/R Scripts/irq_mcna_dashboard/irq_mcna_dashboard")


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

reach_dark_green  <- "#637961"
reach_light_green <- "#E6EFE5"
reach_green       <- "#B0CFAC"

reach_dk_red    <- "#782c2e"
reach_dark_red  <- "#A73E3E"
reach_mddk_red  <- "#bf4749"
reach_red       <- "#EE5859"
reach_pink      <- "#f5a6a7"
reach_light_red <- "#FAD5D6"

reach_lt_grey  <- "#D1D3D4"
reach_grey     <- "#58585A"

white          <- "#FFFFFF"

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


# Ploty Output ------------------------------------------------------------

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
                                   tickformat = "%", tick0 = 0, dtick = 0.2))
  
  rp = rp %>%  layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(indicators)),
                      font  = list(family = "arial", size = 12),
                      xaxis = list(title = right_title, range=c(0,1),
                                   tickformat = "%", tick0 = 0, dtick = 0.2))
  
  return(subplot(lp, rp, titleX = TRUE, shareY = TRUE))
}

# Load Data ---------------------------------------------------------------

# ---- Numerical Data -----------------------------------------------------

# age_dis_sheets = getSheetNames("20210228 - Dataset Edited for Dashboard.xlsx")[5:8]
# age_dis = list()
# for(s in age_dis_sheets){
#   age_dis[[s]] = read.xlsx(xlsxFile = "20210228 - Dataset Edited for Dashboard.xlsx",
#                            sheet = s, colNames = TRUE, rowNames = TRUE)
# }

age_dis_sheets = getSheetNames("20210228 - Dataset Edited for Dashboard.xlsx")[5:8]
age_dis = list()
for(s in age_dis_sheets){
  assign(x = s, as.data.frame(read.xlsx(xlsxFile = "20210228 - Dataset Edited for Dashboard.xlsx",
                                        sheet = s, colNames = TRUE, rowNames = TRUE)))
}

# Function for creating plots ---------------------------------------------

# This will create two HTML plots with a single header and single footnote.
# If you do not want a footnote then leave out that argument.
bar_plot_layout = function(plot_header,
                           bar_plot_1="plot_1", bar_plot_2="plot_2", plot_footnote=FALSE,
                           bar_plot_1_values, bar_plot_1_labels, bar_plot_1_colour, bp1_value_name,
                           bar_plot_2_values, bar_plot_2_labels, bar_plot_2_colour, bp2_value_name,
                           plot_1_type, plot_2_type){
  
  # Create a function to convert the input data into the right format where the "multiple" chart style is used:
  multiple_format = function(input_data_list){
    
    # This is because we need to specify data labels for the multiple style.
    # e.g. chart('#ChartTitle', [{values:[15, 10]}], ['Bar1_name', 'Bar2_name'],
    #            ['#B0CFAC'], 'single');
    # e.g. chart('#ChartTitle', [{label:'Dataset_1',values:[95, 18, 19, 15]},{label:'Dataset_2',values:[89, 29, 22, 20]}],
    #            ['Food', 'Cash', 'Other NFIs', 'Health services'], ['#D2CBB8', '#58585A'], "multiple");
    
    # Create a lookup for the data labels
    data_names = rownames(input_data_list)
    data_names = gsub(pattern = "_", replacement = " ", x = data_names)
    
    # Add the first value in the list:
    x = paste0("[{label:'", data_names[1], "',values:[",
               paste(input_data_list[1,], collapse = ","), "]}")
    
    # Add the subsequent values
    for(i in 2:nrow(input_data_list)){
      x = paste0(x, ",{label:'", data_names[i], "',values:[",
                 paste(input_data_list[i,], collapse = ","), "]}")
    }
    
    # Conclude the expression:
    x = paste0(x, "]")
    
    return(x)
  }
  
  
  # Arrange the bp1_value_name depending on how many variables there are:
  if(is.data.frame(bar_plot_1_values)){bar_plot_1_values_formatted = multiple_format(bar_plot_1_values)
  }else{bar_plot_1_values_formatted = paste0("[{values:[", paste(bar_plot_1_values, collapse = ","), "]}]")}
  
  # Arrange the bp1_value_name depending on how many variables there are:
  if(is.list(bar_plot_2_values)){bar_plot_2_values_formatted = multiple_format(bar_plot_2_values)
  }else{bar_plot_2_values_formatted = paste0("[{values:[", paste(bar_plot_2_values, collapse = ","), "]}]")}
  
  # Set plot box style:
  # Add header
  # Do something
  # Add chart 1
  # Add chart 2
  # Add footer
  html = sprintf(
    
    '<div class="card" style="background-color:#FFFFFF;text-align:center;margin-bottom:20px">
    <h4 class="card-header" >%s</h4>
      <div class="card-body">
      <svg id="%s" class="chart" style="align-content:center;"></svg>
      <svg id="%s" class="chart" style="align-content:center;"></svg>
      %s
      </div>
    </div>
    <script type="text/javascript">
    chart(%s, %s, %s, %s, %s);
    chart(%s, %s, %s, %s, %s);
    </script>',
    
    # Fill the %s with values:
    # A title for the plot:
    plot_header,
    # An ID for the first plot
    bar_plot_1,
    # An ID for the second plot
    bar_plot_2,
    # The footnote:
    case_when(
      plot_footnote!=FALSE ~ paste0("<div class='card-subtitle2'>", plot_footnote, "</div>"),
      TRUE ~ "<div style='margin-bottom:1.5em'></div>"),
    
    
    # Plot 1
    # The plot ID (with a #)
    paste0("'#", bar_plot_1, "'"),
    # The plot values and legend labels:
    bar_plot_1_values_formatted,
    # Plot labels
    paste0("['", paste(bar_plot_1_labels, collapse = "','"), "']"),
    # Bar colour:
    paste0("['", paste(bar_plot_1_colour, collapse = "','"), "']"), # paste0("['#B0CFAC']"),
    # Whether there are single or multiple colours:
    paste0("'", plot_1_type,"'"),
    
    # Plot 2
    # The plot ID (with a #)
    paste0("'#", bar_plot_2, "'"),
    # The plot values and legend labels:
    bar_plot_2_values_formatted,
    # Plot labels
    paste0("['",paste(bar_plot_2_labels, collapse = "','"), "']"),
    # Bar colour:
    paste0("['", paste(bar_plot_2_colour, collapse = "','"), "']"),
    # Whether there are single or multiple colours:
    paste0("'", plot_2_type,"'")
  )
  
  
  # in the html you have the option of single or multiple. Multiple can be used for showing multiple sets of data on the same y axis. If you use this then you will also generate a legend. For this you need to specify a label prior to the values. If you have multiple labels to add here then seperate them with a ',' inside the "____".
  
  return(HTML(html))
  
}


################################## UI ##########################################

# navbar page with tabs
ui <- bootstrapPage(
  # Set up the basic navbar page layout -------------------------------------
  
    absolutePanel(
      id = "a",
      class = "panel panel-default",
      bottom = "auto",
      top = 110,
      left = 200,
      right = "auto",
      width = 500,
      plotlyOutput(outputId = "barrier_3")
    ),
    
    absolutePanel(
      id = "b",
      class = "panel panel-default",
      bottom = "auto",
      top = 110,
      left = 1030,
      right = "auto",
      width = 500, 
      plotlyOutput(outputId = "barrier_4",
        width = "100%",
        height = "100%",
        inline = TRUE # dont think this matters
        # reportTheme = TRUE
      )
      
      # uiOutput("barrier_4")
    
  ) # navbar
) # UI



################################## SERVER ######################################

server <- function(input, output, session){

  
  output$barrier_3 <- renderPlotly({
    
    # navigating_inside_shelters[is.na(navigating_inside_shelters)] = 0
    # navigating_inside_shelters_pwd[is.na(navigating_inside_shelters_pwd)] = 0
    # plotly_bar_graph(data_grid_left = navigating_inside_shelters, 
    #                  data_grid_right = navigating_inside_shelters_pwd,
    #                  plot_title = "TEST")
    
    navigating_inside_shelters[is.na(navigating_inside_shelters)] = 0
    navigating_inside_shelters_pwd[is.na(navigating_inside_shelters_pwd)] = 0
    plotly_bar_graph(data_grid_left = navigating_inside_shelters, 
                     data_grid_right = navigating_inside_shelters_pwd,
                     left_title = "Test", right_title = "TEST")
  
  })
  
  
  # ---- Set up the disabled camp data --------------------------------------
  
  output$barrier_4 <- renderPlotly({

    # navigating_inside_shelters_pwd[is.na(navigating_inside_shelters_pwd)] = 0
    # plotly_bar_graph(data_grid = navigating_inside_shelters_pwd, plot_title = "TEST")
    
  })
  
}


# RUN APP -----------------------------------------------------------------

# runApp(shinyApp(ui, server), launch.browser = TRUE)

shinyApp(ui, server)
