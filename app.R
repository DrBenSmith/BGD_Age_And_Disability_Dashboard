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

age_dis_sheets = getSheetNames("20210228 - Dataset Edited for Dashboard.xlsx")[5:8]
age_dis = list()
for(s in age_dis_sheets){
  age_dis[[s]] = read.xlsx(xlsxFile = "20210228 - Dataset Edited for Dashboard.xlsx",
                   sheet = s, colNames = TRUE, rowNames = TRUE)
}


# Read in Some functions:
# source("functions.R") # TODO - is this useful for BGD context?

# Construct legend for landing page map -----------------------------------

colors <- c(reach_pink, reach_red, reach_mddk_red, reach_dark_red, reach_lt_grey)

addLegendCustom <- function(map, colors, labels, opacity = 0.8, title) {

  make_shapes <- function(colors) {
    paste0(colors, "; width:20px; top: 0; height:20px; border:1px solid  white; text-align: center; border-radius:0%")
  }

  make_labels <- function(labels) {
    paste0("<div style='display: inline-block; text-align: center; height:20px; line-height: 20px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors)
  legend_labels <- make_labels(labels)

  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position = "bottomright", title = title))
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
            sprintf(
              "<span style='color: %s;'><br><strong>CONTEXT</span></strong>",
              reach_red
            )
          )),
          p(
            HTML(
              "Since August 2017, an estimated 715,000 Rohingya refugees have fled to Cox’s Bazar District, Bangladesh, where approximately 860,000 refugees are now residing in 34 camps in Ukhiya and Teknaf Upazilas. In response to the refugee influx, national and international organisations have been delivering humanitarian assistance alongside the government of Bangladesh. In this context, the meaningful and dignified inclusion of individuals across all age groups and persons with disabilities has been incorporated into successive Joint Response Plans in 2019 and 2020. However, while the heightened risk of persons with disabilities and older persons is generally recognized by affected populations and humanitarian actors alike, a lack of data on disability prevalence across camps as well as the specific requirements, barriers and preferences of older persons and persons with disabilities complicates evidence-based inclusive programming."
                          )
          ),
          p(
            HTML(
              "Against this background, REACH, with technical support from the Age and Disability Working Group (ADWG), conducted an Age and Disability Inclusion Needs Assessment across Rohingya refugee populations. The assessment aimed to understand disability prevalence, and to support key actors working in Cox’s Bazar, including coordination bodies and technical agencies and actors, to consider the nuanced and specific requirements, access to services and assistance, and involvement of persons with disabilities across all age groups, and older persons living in Rohingya camps, within the response programming. The assessment was coordinated through the ADWG, and implemented with technical contributions from an Age and Disability Task Team (ADTT). The ADTT comprised of the United Nations High Commissioner for Refugees (UNHCR), the International Organization for Migration Needs and Population Monitoring (IOM NPM), the Water, Sanitation and Hygiene (WASH) Sector, and REACH. Technical contributions were further made by Humanity & Inclusion (HI), CBM and the Centre for Disability in Development (CDD), and Prottyashi."
            )
          ),
          h5(HTML(
            sprintf(
              "<span style='color: %s;'><strong>METHODOLOGY</span></strong>",
              reach_red
            )
          )),
          p(
            HTML(
              "The assessment comprised a quantitative household survey and a qualitative component consisting of focus group discussions (FGDs). The quantitative component was implemented in all 34 camps in Ukhiya and Teknaf Upazilas. A stratified cluster sampling approach was employed, with the camps as strata and households as clusters. Information related to disability prevalence was collected through the Washington Group Questions (WGQs)4 on all household members in sampled households aged 2 and above. Information on service utilisation, access barriers and enablers, as well as participation and disaster preparedness was collected on sub-samples of those individuals. Information was collected directly from the concerned individuals themselves, if possible. In all other cases, information was collected by proxy from another adult household member. In total, 2,530 household interviews, covering 11,187 individuals aged 2 and above, were carried out between 30 November 2020 and 7 January 2021. Basic descriptive analysis was conducted, complemented by testing for statistically significant differences in outcomes between persons with and without disabilities, overall as well as for different age groups and genders, by types of functional difficulty, and between households with and without persons with disabilities. The achieved level of representativeness of findings differs by the sub-samples addressed for each question. For detailed information on levels of representativeness, as well as challenges and limitations of the assessment, please refer to annex 1.FGDs were conducted to further contextualise quantitative findings and provide more detailed insights into the specific barriers persons with disabilities and older persons face accessing services, participating in community life and in disaster preparedness, as well as potential solutions. A total of 20 FGDs were conducted with older persons with and without disabilities, adults with disabilities, children with disabilities (aged 11 to 17), and caregivers of children with disabilities, between 12 January and 8 February 2021."
            )
          ),
          p(
            HTML(
              "However, in all IDP camps and in districts where health risks and/or movement or access restrictions prevented face-to-face interviews (38/62), a non-probability purposive quota sampling approach with a minimum target of 60 surveys per population group was adopted. Due to the non-randomized sampling methodology, findings in these strata are not statistically representative with a quantifiable degree of precision and have to be considered as indicative only. All findings for in-camp IDP households are indicative, given the specific COVID-19 risks in camp settings and subsequent decision to survey this population group through phone-based interviews only. Note that the remote setting of phone-based interviews present additional limitations in terms of asking sensitive or technical questions."
            )
          )
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
          h5(HTML(
            sprintf(
              # "<img class='icon' src='noun_Map_2009543.svg' style='color: %s;height:20px, width:20px'><span style='color: %s'><strong> Demographics and Coverage Map</strong></span>",
              "<span style='color: %s'><strong> Demographics and Coverage Map</strong></span>",
              # reach_red,
              reach_red
            )
          )),
          p(HTML(
            sprintf(
              "<span style='color: %s'><strong>Data sources:</strong></span><br>Administrative boundaries: ISCG, 2020<br>Demographics and Coverage: REACH",
              reach_red
            )
          )),

          selectInput(inputId = "coverage", label = "Dataset:", width = 150,
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
  title = strong("Barriers to Whatever they were"),
  value = "panel2",
  # )
# ))


# ---- Set up left hand box containing key points: ------------------------

  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    draggable = F,
    top = 60,
    left = 10,
    right = "auto",
    width = 500,

    # Title:
    h5(HTML(sprintf(
      "<span style='color: %s;'><br><strong>KEY FINDINGS</span></strong>",
      reach_red))),

    # New paragraph of key findings:
    p(HTML("<ul>
    <li>Barriers related to mobility in shelters or around camps were reported for 52% and 76% of persons with disabilities, respectively. In particular, persons with difficulties in functioning in the self-care or mobility domains reportedly face barriers moving both inside shelters and around camps. In addition, persons with difficulties in functioning in the upper body movement or vision domains reportedly particularly face barriers moving inside shelters. Mobility-related barriers were also increasingly reported with increasing age.</li>

    <li>Especially persons with difficulties in functioning in the self-care, upper body movement or mobility domains were reportedly unable to use latrines or shower without support from others. Age and gender were further found to compound difficulties with self-care, with particularly high proportions of female older persons with disabilities reportedly being unable to shower or use latrines without support from others.

    </li><li>Reported utilisation of public not accessible latrines or public bathing facilities was particularly low among persons with difficulties in functioning in the self-care or upper body movement domains. At the same time, the reported utilisation of private or accessible latrines – while higher than for other individuals – also remained low among those groups.</li>

    <li>A significantly higher proportion of persons with disabilities than persons without disabilities reportedly faces barriers accessing services. In particular, persons with difficulties in functioning in the self-care or mobility domains as well as female older persons with disabilities reportedly face barriers.</li>

    <li>Overall, more than half the persons with disabilities had reportedly not received any assistive devices in the year prior to data collection despite needing them. This proportion was highest among female older persons with disabilities.
    </ul>")),

# ---- Set up a control panel for the plots: ------------------------------

    selectInput(inputId = "barrier_plots",
                label = "Barrier Datasets:",
                width = 400,
                choices = c("Moving within shelters",
                            "Moving around camps"),
                selected = "Moving within shelters")
  ),


# Set up a box for holding the plots --------------------------------------

absolutePanel(
  id = "barrier_output_age_panel",
  # class = "panel panel-default",
  bottom = "auto",
  top = 60,
  left = 520,
  right = "auto",
  width = 500,
  uiOutput("HTMLplot_barrier_ages")
  ),

absolutePanel(
  id = "barrier_output_rea_panel",
  # class = "panel panel-default",
  bottom = "auto",
  top = 60,
  left = 1030,
  right = "auto",
  width = 500,
  uiOutput("HTMLplot_barrier_reas")
)

  # Plots
  # plotlyOutput("barrier_output")

) # tab panel
) # navbar
) # UI



################################## SERVER ######################################

server <- function(input, output, session){
  # prev_content <-
  #   reactiveValues(indicator = "Average household size",
  #                  sector = "Household Profile",
  #                  popgroup = "Upazila")


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
      reach_red,
      BGD_camps_data$Camp_Name,
      reach_grey,
      BGD_camps_data$Upazila,
      reach_grey,
      paste0(input$coverage, ":"),
      reach_grey,
      coverage_value
      ) %>% lapply(htmltools::HTML)

    # bbox <- st_bbox(irq_gov) %>%
    #   as.vector()

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
        color        = white,
        fillColor    = case_when(
          (coverage_value > 0          & coverage_value < labels[1]) ~ colors[1], # reach_pink
          (coverage_value >= labels[1] & coverage_value < labels[2]) ~ colors[2], # reach_red
          (coverage_value >= labels[2] & coverage_value < labels[3]) ~ colors[3], # reach_mddk_red
          (coverage_value >= labels[3]) ~ colors[4], # reach_dark_red
          TRUE ~ reach_lt_grey),
        label        =  coverage_tooltip,
        weight       = 0.2,
        smoothFactor = 0.5,
        opacity      = 1,
        fillOpacity  = 0.9,
        options      = list(zIndex = 400),
        highlightOptions = highlightOptions(
          fillColor    = white,
          color        = white,
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
      addLegendCustom(colors, labels, title = input$coverage) %>%
      # addLegend(title = input$coverage) %>%

      # Add scale
      addScaleBar(position = "bottomright", scaleBarOptions(imperial = FALSE)) %>%

    # Set initial view coordinates;
    setView(lng = 92.1,
            lat = 21.0707,
            zoom = 11.5)
  })


# Barriers ----------------------------------------------------------------


# ---- HTML Plots ---------------------------------------------------------

  observe({

    # Set up the code for linking the selected variable to the output:

    selected_barrier <-
      case_when(
        input$barrier_plots == "Moving within shelters" ~ "navigating_inside_shelters",
        input$barrier_plots == "Moving around camps" ~ "navigating_camps_reason")
        # TRUE ~ input$popgroup

# ---- Set up the non-disabled age data -----------------------------------

    # Plot 1 (Ages Non - disabled)
    plot_data_ages = age_dis[[selected_barrier]]
    plot_data_ages_labels = rownames(plot_data_ages)
    plot_data_ages = round(plot_data_ages[, grep(x = colnames(plot_data_ages), pattern = "yes")]*100, digits = 0)
    plot_data_ages[is.na(plot_data_ages)]=0

    # Plot 2 (Reasons Non - Disabled)
    bar_reason = age_dis[[selected_barrier]]
    bar_reason = round(bar_reason[, !grepl(x = colnames(bar_reason), pattern = "yes")]*100, digits = 0)
    bar_reason[is.na(bar_reason)]=0

# ---- Set up the disabled data for the reasons plot ----------------------

    selected_barrier_pwd <-
      case_when(
        input$barrier_plots == "Moving within shelters" ~ "navigating_inside_shelters_pwd",
        input$barrier_plots == "Moving around camps" ~ "navigating_camps_reason_pwd")

    # Plot 2 (Ages Disabled)
    # selected_barrier_pwd = paste0(selected_barrier,"_pwd")
    plot_data_ages_pwd = age_dis[[selected_barrier_pwd]]

    plot_data_ages_pwd_labels = rownames(plot_data_ages_pwd)
    plot_data_ages_pwd = plot_data_ages_pwd[, grep(x = colnames(plot_data_ages_pwd), pattern = "yes")]*100
    plot_data_ages_pwd = round(plot_data_ages_pwd, digits = 0)
    plot_data_ages_pwd[is.na(plot_data_ages_pwd)]=0

    # Plot 2 (Reasons Non - Disabled)
    bar_reason_pwd = age_dis[[selected_barrier_pwd]]
    bar_reason_pwd = round(bar_reason_pwd[, !grepl(x = colnames(bar_reason_pwd), pattern = "yes")]*100, digits = 0)
    bar_reason_pwd[is.na(bar_reason_pwd)]=0

# ---- Plot the non-disabled data -----------------------------------------

    output$HTMLplot_barrier_ages <- renderUI({

      bar_plot_layout(
        plot_header = "Persons without Disabilitites", # input$barrier_plots,
        plot_footnote = "I think this needs a note to explain the 0s",

        bar_plot_1_values = plot_data_ages, bar_plot_1_labels = plot_data_ages_labels,
        bp1_value_name = "Persons without disabilities",
        bar_plot_1_colour = reach_red,
        plot_1_type = "single",

        bar_plot_2_values = bar_reason, bar_plot_2_labels = colnames(bar_reason),
        bp2_value_name = names(bar_reason),
        bar_plot_2_colour = c(reach_green, reach_grey, reach_pink, reach_lt_grey),
        plot_2_type = "multiple")
      })


# ---- Plot the disabled data ---------------------------------------------

    output$HTMLplot_barrier_reas <- renderUI({

      bar_plot_layout(
        plot_header = "Persons with Disabilities", #input$barrier_plots,
        plot_footnote = "I think this needs a note to explain the 0s",

        bar_plot_1_values =  plot_data_ages_pwd, bar_plot_1_labels = plot_data_ages_pwd_labels,
        bp1_value_name = "Persons with disabilities",
        bar_plot_1_colour = reach_red,
        plot_1_type = "single",

        bar_plot_2_values = bar_reason_pwd, bar_plot_2_labels = colnames(bar_reason_pwd),
        bp2_value_name = names(bar_reason_pwd),
        bar_plot_2_colour = c(reach_green, reach_grey, reach_pink, reach_lt_grey),
        plot_2_type = "multiple")
    })

})
}


# RUN APP -----------------------------------------------------------------

# runApp(shinyApp(ui, server), launch.browser = TRUE)

shinyApp(ui, server)


# Spare that you shouldn't need -------------------------------------------



# prepare_cards = function(){
#   html = sprintf(
#     ' <div class="card" style="background-color:#FFFFFF;text-align:center;margin-bottom:20px">
#     <h4 class="card-header" >% of households satisfied with regards to their access to hygiene items</h4>
#       <div class="card-body">
#       <svg id="WASH_pop_g98" class="chart"></svg>
#       <svg id="WASH_gender_g98" class="chart"></svg>"
#       <div class="card-subtitle2">Note: Data for this indicator was not collected for...</div>
#       </div></div>
#     <script type="text/javascript">
#     chart(%s, [{values:[93, 96]}], %s, reach_blue, "multiple");
#     chart(%s, [{values:[16, 49]}], %s, reach_blue, "multiple");
#     </script>',
#     "'#WASH_pop_g98'",
#     "['Female-headed', 'Male-headed']",
#     "'#WASH_gender_g98'",
#     "['Female-headed', 'Male-headed']"
#     )
#
#   return(html)
# }

# Set up a template for the general area:
# template_function <- function(selected_sector) {
#   # html <- ""
#
#   html <- sprintf(
#   '<div class="card">
#   <div class="card-title"  style="margin:10px">
#   <h2 style="text-align:center"> %s Sector</h2>
#   <h5 style="text-align:center;margin-top:1rem;" class="card-subtitle mb-2 text-muted"> TITLE </h5>
#   <h5 style="text-align:center;margin-top:1rem;" class="card-subtitle mb-2 text-muted"> Other Stuff.</h5>
#   </div>',
#     selected_sector
#   )
#
#   html <- paste0(
#     html,
#     '<div class="row" style="margin:0px;">
#     <div class="col-sm-12" style ="padding-left:5%;padding-right:5%;">
#     </div>
#     </div>',
#
#     sprintf(
#       '<script type="text/javascript">
#     chart(Title, 10, 20, [\'reach_blue\'], \'single\');
#     chart(tit, 40, 30, [\'reach_red\'], \'single\');
#     </script>')
#   )
#
#   return(html)
# }
