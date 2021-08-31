library(plotly)

age_dis_sheets = getSheetNames("20210228 - Dataset Edited for Dashboard.xlsx")[5:8]
age_dis = list()
for(s in age_dis_sheets){
  assign(x = s, as.data.frame(read.xlsx(xlsxFile = "20210228 - Dataset Edited for Dashboard.xlsx",
                          sheet = s, colNames = TRUE, rowNames = TRUE)))
}

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
                    xaxis = list(tickformat = "%", range=c(-1,1)))
  return(p)
}



# Attempt with seperate axies ---------------------------------------------

plotly_bar_graph_dual_axis = function(data_grid_left, left_title, data_grid_right, right_title){
  
  # data_grid_left = navigating_camps_reason
  # data_grid_right = navigating_camps_reason_pwd
  # left_title = "L"
  # right_title = "R"
  
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
                        marker = list(color = colours[colour_rank[r]]),
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


# Single trace dual axis --------------------------------------------------

plotly_bar_graph_dual_axis = function(data_grid_left, left_col = 1, left_title, 
                                      data_grid_right, right_col = 1, right_title){
  
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
                        showlegend = FALSE)
  
  rp = rp %>% add_trace(type = 'bar', 
                        x = data_grid_right[,right_col],
                        y = indicators,
                        marker = list(color =  colours$reach_red))
  
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

# Other plotly code -------------------------------------------------------

# layout(
#   xaxis = list(title = "", automargin=T, ticksuffix = "%")
#   ,yaxis = list(title = "", automargin=T, ticksuffix = "   ")
# )

# tickvals = list(seq(0,1,0.2))
