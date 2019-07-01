#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(zoo)
library(readr)
library(xts)
library(ggplot2)
library(devtools)
#library(forecastHybrid)
library(forecast)
library(data.table)
library(DT)
library(dygraphs)
library(dplyr)
library(lubridate)
library(stats)
library(tsbox)
library(tidyr)
library(padr)
library(shinythemes)
library(rio)
library(magrittr)
library(shinyjs)
library(rsconnect)

# Increase the maximum file size upload from 5MB
options(shiny.maxRequestSize = 30*1024^2)

# Example Dataset
csvDummy <- 'https://raw.githubusercontent.com/mlombera94/forecast_R-shiny/master/dataset.csv'

# Define UI for application that draws a histogram
shinyUI(
    # Creates navigation bar at the top of the webpage
    navbarPage(
      
      # Title of webpage
      title="Time Series Forecasting",
      # Selects the theme of the webpage
      theme=shinytheme("spacelab"),
      # uses a dark background and light text for the navigation bar
      inverse=TRUE,
      
      # Creates the first tab for the navigation bar at the top of the webpage
      tabPanel(
        "Import and Build Dataset",
        
        ######### DATASET TAB #########      
        
        # Creates a page with fluid layout
        fluidPage(
          # Create a row with fluid layout
          fluidRow( 
            # Creates a sidebar 
            sidebarPanel(
              h3("Build Dataset"),
              # Creates a break in the webpage 
              tags$hr(),
              br(),
              # Creates the input for uploading files 
              h5("Download the example dataset to begin", a("User created CSV", href=csvDummy, target="_blank")),
              fileInput("i_file", 
                        "Upload your CSV file",
                        # A character vector of MIME types; gives the browser a hint of what kind of files the server is expecting.
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              # Creates a break between inputs
              tags$hr(),
              # Outputs a selection input based on the regions in the dataset for the user to select and filter 
              uiOutput("region"),
              # Outputs a selection input based on the markets in the dataset for the user to select and filter
              conditionalPanel(
                condition = "output.region", 
                h6("Filter the dataset based on Region(s)"),
                uiOutput("market"),
                h6("Filter the dataset based on Market(s)"),
                checkboxInput("all", 
                              label = "Select ALL/None", 
                              value = FALSE)),
              # Outputs a selection input based on the products in the dataset for the user to select and filter
              conditionalPanel(
                condition = "output.market", 
                uiOutput("product"),
                h6("Filter the dataset based on Product(s)"),
                checkboxInput("all2", 
                              label = "Select ALL/None", 
                              value = FALSE)),
              # Outputs a selection input based on the SKUs in the dataset for the user to select and filter
              conditionalPanel(
                condition = "output.product", 
                uiOutput("sku"),
                h6("Filter the dataset based on SKU(s)"),
                checkboxInput("all3", 
                              label = "Select ALL/None", 
                              value = FALSE)),
              tags$hr(),
              selectInput(inputId = "observations",
                          label = "Select the minimum # of allowable observations per SKU",
                          choice = c("8" = "8",
                                     "9" = "9",
                                     "10" = "10",
                                     "11" = "11",
                                     "12" = "12",
                                     "13" = "13",
                                     "14" = "14",
                                     "15" = "15"),
                          multiple = FALSE,
                          width = "50%"),
              tags$hr(),
              # selectInput(inputId = "months1",
              #             label = "Select the number of recent periods to observe the number of NA observations",
              #             choice = c("3" = "3",
              #                        "4" = "4",
              #                        "5" = "5",
              #                        "6" = "6", 
              #                        "7" = "7",
              #                        "8" = "8",
              #                        "9" = "9",
              #                        "10" = "10"),
              #             multiple = FALSE, 
              #             width = "50%"),
              # tags$hr(),
              selectInput(inputId = "na_obs",
                          label = "Select the maximum # of NA observations allowable within the six recent periods per SKU",
                          choice = c("1" = "1",
                                     "2" = "2",
                                     "3" = "3",
                                     "4" = "4",
                                     "5" = "5",
                                     "6" = "6", 
                                     "7" = "7",
                                     "8" = "8",
                                     "9" = "9",
                                     "10" = "10"),
                          multiple = FALSE, 
                          width = "50%"),
              tags$hr(),
              # Outputs a checkbox that allows the user to decide whether to aggregate totals across rows and create a new column
              checkboxInput("checkbox", 
                            label = "Aggregate Rows", 
                            value = FALSE), # Sets the checkbox to be empty when the app initiates
              tags$hr(),
              # Outputs an action button that allows the user to build and display the filtered dataset when pressed
              actionButton(inputId="build", "Build Dataset"),
              h6("Display the dataset"),
              # Assigns an id to the sidebar and the length 
              id = "Sidebar Panel 1", width = 3),
            # Creates a main panel to display the final dataset  
            mainPanel(
              h2("Data"),
              br(),
              # Outputs the dataset 
              DT::dataTableOutput("subset_df"),
              # Assigns an id to the main panel and the length
              id = "Main Panel 1", width = 9)
          )
        )
      ),
      tabPanel(
        "Forecast",
        
        ##### FORECAST TAB ##### 
        fluidPage(
          fluidRow(
            sidebarPanel(
              h3("Generate Forecast"),
              # Creates an input for the user to select which column in the data to forecast 
              tags$hr(),
              br(),
              selectInput(inputId="i_task_select", 
                          "Select Series",
                          '',
                          ''),
              br(),
              # Creates an input for the user to select and filter the date range of the data for the column chosen 
              dateRangeInput('dateRange',
                             label = 'Filter Historical Data by Date',
                             start = (Sys.Date() - (3*365)), # Automatically displays the date 3 years past of current date as the start point
                             end = Sys.Date()), # Automatically displays todays current date as the end point 
              br(),
              checkboxInput("checkbox2", 
                            label = "Replace NAs with 0s", 
                            value = FALSE),
              tags$hr(),
              # Allows the user to adjust how may periods ahead to forecast ahead
              sliderInput(inputId = "i_forecast_n",
                          "Forecast Periods",
                          value = 3,
                          min = 2, max = 12, step =  1),
              h6("Select the number of periods to forecast ahead"),
              tags$hr(),
              # Create Select option for confidence interval
              selectInput(inputId = "conf_int",
                          label = "Select Confidence Interval",
                          choice = c("99%" = "99",
                                     "95%" = "95",
                                     "90%" = "90",
                                     "80%" = "80",
                                     "70%" = "70",
                                     "50%" = "50"),
                          multiple = FALSE,
                          width = "50%"),
              # Allows the user to adjust the number of previous periods to observe how the the model would have performed
              sliderInput(inputId = "i_recent_months",
                          "Holdout Period",
                          value = 3,
                          min = 2, max = 12, step =1 ),
              h6("Select the number of periods to include in the Holdout Period"),
              tags$hr(),
              # selectInput(inputId = "error_measurement2",
              #             label = "Select Error Measurement",
              #             choice = c("RMSE",
              #                        "MAE",
              #                        "MAPE"),
              #             multiple = FALSE),
              # Outputs an action button that allows the user to begin forecasting 
              actionButton(inputId="goButton", "Start forecasting!"),
              h6("Click on the tabs at the top right to switch between models"),
              id = "Sidebar Panel 2", width = 3),
            mainPanel(
              h2("Forecast Results"),
              br(),
              tabPanel("Forecast Plot",
                       tabsetPanel(type = "tabs",
                                   # Build forecasting panels
                                   tabPanel("ARIMA", icon = icon("area-chart"), h4("Auto Regressive Integrated Moving Average (ARIMA) Model"), br(), dygraphOutput("p_ARIMA"), value=1),
                                   tabPanel("ARFIMA", icon = icon("area-chart"), h4("Auto Regressive Fractional Integrated Moving Average (ARFIMA) Model"), br(), dygraphOutput("p_ARFIMA"), value=2),
                                   tabPanel("CROSTON", icon = icon("area-chart"), h4("Croston Intermittent Demand Model"), br(), dygraphOutput("p_croston"), value=3),
                                   tabPanel("ETS", icon = icon("line-chart"), h4("Error Trend Seasonal Model"), br(), dygraphOutput("p_ets"), value=4),
                                   tabPanel("HW MULTIPLICATIVE", icon = icon("line-chart"), h4("Holt-Winters Exponential Multiplicative Smoothing Model"), br(), dygraphOutput("p_HW_M"), value=5),
                                   tabPanel("HW MULTIPLICATIVE DAMPED TREND", icon = icon("line-chart"), h4("Holt-Winters Exponential Multiplicative W/ Damped Trend Smoothing Model"), br(), dygraphOutput("p_HW_MD"), value=6),
                                   tabPanel("HW ADDITIVE", icon = icon("line-chart"), h4("Holt-Winters Exponential Additive Smoothing Model"), br(), dygraphOutput("p_HW_A"), value=7),
                                   tabPanel("HW ADDITIVE DAMPED TREND", icon = icon("line-chart"), h4("Holt-Winters Exponential Additive W/ Damped Trend Smoothing Model"), br(), dygraphOutput("p_HW_AD"), value=8),
                                   tabPanel("LOBF", icon = icon("bar-chart"), h4("Line of Best Fit (Linear Regression)"), br(), dygraphOutput("p_LOBF"), value=9),
                                   tabPanel("MA", icon = icon("line-chart"), h4("Moving Average Smoothing Model"), br(), dygraphOutput("p_MA"), value=10),
                                   tabPanel("SES", icon = icon("line-chart"), h4("Simple Exponential Smoothing Model"), br(), dygraphOutput("p_SES"), value=11),
                                   tabPanel("TBATS", icon = icon("area-chart"), h4("TBATS Model: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend & Seasonal components"), br(), dygraphOutput("p_TBATS"), value=12),
                                   id = "timeSeriesTabs"),
                       id = "Tab Panel"),
              id = "Main Panel 2", width = 9)),
          # Creates another row to display on the webpage
          fluidRow(
            column(width=12,
                   h4("Model Performance for Holdout Period"),
                   # Outputs the accuracy measurements of how the model would've performed in previous periods
                   DT::dataTableOutput("model_performance")))
          # fluidRow(
          #   column(width=12,
          #          h4("Top 3 Models"),
          #          # Outputs the accuracy measurements of how the model would've performed in previous periods
          #          DT::dataTableOutput("top_model")))
        )
      ),
      tabPanel(
        "Batch Forecast",
        ##### FORECAST TAB ##### 
        fluidPage(
          fluidRow(
            sidebarPanel(
              h3("Generate Batch Forecast"),
              tabsetPanel(
                tabPanel(
                  h5("Adjust Data"),
                  # Creates an input for the user to select which column in the data to forecast 
                  tags$hr(),
                  br(),
                  br(),
                  # Creates an input for the user to select and filter the date range of the data for the column chosen 
                  dateRangeInput('dateRange2',
                                 label = 'Filter Historical Data by Date',
                                 start = (Sys.Date() - (3*365)), # Automatically displays the date 3 years past of current date as the start point
                                 end = Sys.Date()), # Automatically displays todays current date as the end point 
                  tags$hr(),
                  # h5("Remove SKUs with more than the selected number of observed zeros within the selected number of recent periods"),
                  # br(),
                  # selectInput(inputId = "months",
                  #             label = "Select the number of recent periods to observe the number of zero observations",
                  #             choice = c("15" = "15",
                  #                        "14" = "14",
                  #                        "13" = "13",
                  #                        "12" = "12",
                  #                        "11" = "11",
                  #                        "10" = "10",
                  #                        "9" = "9",
                  #                        "8" = "8",
                  #                        "7" = "7",
                  #                        "6" = "6", 
                  #                        "5" = "5",
                  #                        "4" = "4"),
                  #             multiple = FALSE,
                  #             width = "50%"),
                  # tags$hr(),
                  selectInput(inputId = "zero_obs",
                              label = "Select the number of maximum zero observations within the recent six periods",
                              choice = c("12" = "12",
                                         "11" = "11",
                                         "10" = "10",
                                         "9" = "9",
                                         "8" = "8",
                                         "7" = "7",
                                         "6" = "6", 
                                         "5" = "5",
                                         "4" = "4"),
                              multiple = FALSE,
                              width = "50%")
                ),
                tabPanel(
                  h5("Batch Forecast"),
                  tags$hr(),
                  h5("Select Model(s)"),
                  checkboxInput("ARIMAmodel",
                                label = "ARIMA model",
                                value = TRUE),
                  checkboxInput("ARFIMAmodel",
                                label = "ARFIMA model",
                                value = FALSE),
                  checkboxInput("CROSTONmodel",
                                label = "Croston model",
                                value = FALSE),
                  checkboxInput("ETSmodel",
                                label = "ETS model",
                                value = FALSE),
                  checkboxInput("HW_M_model",
                                label = "HW Multiplicative model",
                                value = FALSE),
                  checkboxInput("HW_MD_model",
                                label = "HW Multiplicative Damped Trend model",
                                value = FALSE),
                  checkboxInput("HW_A_model",
                                label = "HW Additive model",
                                value = FALSE),
                  checkboxInput("HW_AD_model",
                                label = "HW Additive Damped Trend model",
                                value = FALSE),
                  checkboxInput("LOBFmodel",
                                label = "LOBF model",
                                value = FALSE),
                  checkboxInput("MAmodel",
                                label = "MA model",
                                value = FALSE),
                  checkboxInput("SESmodel",
                                label = "SES model",
                                value = FALSE),
                  checkboxInput("TBATSmodel",
                                label = "TBATS model",
                                value = FALSE),
                  tags$hr(),
                  # Allows the user to adjust how may periods ahead to forecast ahead
                  sliderInput(inputId = "forecast_n",
                              "Forecast Periods",
                              value = 3,
                              min = 2, max = 12, step =  1),
                  h6("Select the number of periods to forecast ahead"),
                  tags$hr(),
                  selectInput(inputId = "conf_int2",
                              label = "Select Confidence Interval",
                              choice = c("99%" = "99",
                                         "95%" = "95",
                                         "90%" = "90",
                                         "80%" = "80",
                                         "70%" = "70",
                                         "50%" = "50"),
                              multiple = FALSE,
                              width = "50%"),
                  tags$hr(),
                  # Allows the user to adjust the number of previous periods to observe how the the model would have performed
                  sliderInput(inputId = "i_recent_months2",
                              "Holdout Period",
                              value = 3,
                              min = 2, max = 12, step = 1),
                  h6("Select the number of periods to include in the Holdout Period"),
                  tags$hr(),
                  selectInput(inputId = "error_measurement",
                              label = "Select Error Measurement",
                              choice = c("RMSE",
                                         "MAE",
                                         "MAPE"),
                              multiple = FALSE,
                              width = "50%"),
                  # Outputs an action button that allows the user to begin forecasting 
                  actionButton(inputId="batch_FC", "Start Batch forecasting!")
                )
              ),
              id = "Sidebar Panel 2", width = 3),
            mainPanel(
              h2("Forecast Results"),
              br(),
              tabPanel("Forecast Data Table",
                       tabsetPanel(type = "tabs",
                                   # Build forecasting panels
                                   tabPanel("Batch Forecast", icon = icon("area-chart"), h4("Batch Forecast of Selected SKUs"), br(), DT::dataTableOutput("final_batchdf"), value = 1),
                                   tabPanel("Recommended Forecast", icon = icon("area-chart"), h4("Recommended Forecast of SKUs based on Selected Error Measurement"), br(), DT::dataTableOutput("model_recommend_df"), value = 2),
                                   id = "timeSeriesTabs"),
                       id = "Tab Panel"),
              id = "Main Panel 2", width = 9))
        )
      ),
      ##### STATISTICAL TAB #####
      tabPanel(
        "Statistical Inference",
        fluidPage(
          fluidRow(
            sidebarPanel(
              h3("Generate Forecast"),
              # Creates an input for the user to select which column in the data to forecast 
              tags$hr(),
              br(),
              selectInput(inputId="i_task_select2", 
                          "Select Series",
                          '',
                          ''),
              br(),
              # Creates an input for the user to select and filter the date range of the data for the column chosen 
              dateRangeInput('dateRange3',
                             label = 'Filter Historical Data by Date',
                             start = (Sys.Date() - (3*365)), # Automatically displays the date 3 years past of current date as the start point
                             end = Sys.Date()), # Automatically displays todays current date as the end point 
              br(),
              checkboxInput("checkbox3", 
                            label = "Replace NAs with 0s", 
                            value = FALSE),
              actionButton(inputId="stat_button", "Begin Plots")
            ),
            mainPanel(
              tabPanel(
                "Statistical Information on Data",
                tabsetPanel(type = "tabs",
                            tabPanel("Statistics", h4("Statistics table for selected data"), br(), renderDataTable(""), value = 1),
                            tabPanel("Seasonality Plot", h4("Displays seasonal data by year"), h6("Note: Data must have a minimum of 13 observations for seasonal plot"), br(),
                                     plotOutput("seasonal_plot"), value = 2),
                            tabPanel("ACF Plot", h4("Displays the correlation between the number of periods between observations"), br(),
                                     plotOutput("acf_plot")),
                            tabPanel("Decomposed Plot", h4("Displays the series, trend, and seasonality"), br(),
                                     plotOutput("decomposed_plots")),
                            id = "StatisticalTabs")
              )
            )
          )
        )
      )
  )
)
