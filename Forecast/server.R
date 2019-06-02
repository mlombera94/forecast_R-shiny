#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ###################################################################
    ###################### TIME SERIES FORECASTING ####################
    ###################################################################
    
    # Load any required functions
    source("functions.R", local = TRUE) 
    
    ######################################### READ IN CSV FILE BASED ON SELECTION ############################################################# 
    mySeries_raw <- reactive({
      inFile <- input$i_file
      
      if (is.null(inFile)){return(NULL)}
      df <- read.csv(inFile$datapath,
                     header = T,
                     strip.white=T,
                     stringsAsFactors=F,
                     fill=T)
      
      # Rename columns
      df %>% setnames(old = c("SDATE", "LEVEL0", "LEVEL3", "LEVEL5", "LEVEL6", "EBS_BH_REQ_QTY_RD"),
                      new = c("Date", "SKU", "Product", "Market", "Region", "Booking_AC"))
      
      # Convert Date variable from chr to Date
      df$Date <- as.Date(df$Date, format = "%d-%b-%y")
      
      # Convert any remaining character variables to factors
      df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
      
      # Drop observations containing observations from regions 177899, 234601, 236273, 250900, 29437 and filter observations that exceed current date
      df <- df %>%
        filter(!Region %in% c("177899", "234601", "236273", "250900", "29437"),
               Date < as.Date(Sys.Date() %m-% months(1)),
               !is.na(Region),
               !is.na(Market))
      
      # Recode observations
      df$Region <- df$Region %>%
        recode(CLIN = "CLIN",
               `8851` = "NA",
               `8848` = "EU",
               `8847` = "AS",
               `8846` = "AF",
               `8850` = "ME",
               `8852` = "OC",
               `8849` = "LA")
      
      #Remove "-" and replace with "_" as the "-" causes error 
      df$SKU <- gsub('-', '_', df$SKU)
      
      return(df)
      
    })
    
    ########################################################## BUILD DATAFRAME ################################################################
    # Create Select option for all regions available in the data
    output$region <- renderUI({
      data <- mySeries_raw()
      
      if(is.null(data)){return(NULL)}
      
      selectInput(inputId = "region",
                  label = "Select Region",
                  choice = sort(unique(data$Region)),
                  multiple = TRUE)
    })
    
    # Filter the raw data based on regions selected
    region_df <- reactive({
      data <- mySeries_raw()
      
      if(is.null(data)){return(NULL)}
      
      data %>% 
        filter(Region %in% input$region)
    })
    
    # Create select option for all markets available in the regions selected in previous filter
    output$market <- renderUI({
      data <- region_df()
      
      if(is.null(data)){return(NULL)}
      
      selectInput(inputId = "market",
                  label = "Select Market",
                  choice = sort(unique(data$Market)),
                  multiple = TRUE)
    })
    
    # Create a checkbox option to include all markets in filter 
    observe({
      data <- region_df()
      updateSelectInput(
        session, 
        "market", 
        choices = sort(unique(data$Market)),
        selected = if(input$all) unique(data$Market)
      )
    })
    
    # Filter the previous dataset of selected regions based on markets selected
    market_df <- reactive({
      data <- region_df()
      
      if(is.null(data)){return(NULL)}
      
      data %>% 
        filter(Market %in% input$market)
    })
    
    # Create select option for all products available in the markets selected in previous filter
    output$product <- renderUI({
      data <- market_df()
      
      if(is.null(data)){return(NULL)}
      
      selectInput(inputId = "product",
                  label = "Select Product",
                  choice = sort(unique(data$Product)),
                  multiple = TRUE)
    })
    
    # Create a checkbox option to include all products in filter
    observe({
      data <- market_df()
      updateSelectInput(
        session, 
        "product", 
        choices = sort(unique(data$Product)),
        selected = if(input$all2) unique(data$Product)
      )
    })
    
    # Filter the previous dataset of selected markets based on products selected
    product_df <- reactive({
      data <- market_df()
      
      if(is.null(data)){return(NULL)}
      
      data %>% 
        filter(Product %in% input$product)
    })
    
    # Create select options for all SKUs in the products selected in previous filter
    output$sku <- renderUI({
      data <- product_df()
      
      if(is.null(data)){return(NULL)}
      
      selectInput(inputId = "sku",
                  label = "Select SKU",
                  choice = sort(unique(data$SKU)),
                  multiple = TRUE)
    })
    
    # Create a checkbox option to include all SKUs in filter
    observe({
      data <- product_df()
      updateSelectInput(
        session, 
        "sku", 
        choices = sort(unique(data$SKU)),
        selected = if(input$all3) unique(data$SKU)
      )
    })
    
    # Filter the previous dataset of selected products based on SKUs chosen and build the dataframe based on the action button "Build Dataset"
    final_df <- eventReactive(input$build, {
      data <- product_df()
      
      if(is.null(data)){return(NULL)}
      
      data <- data[, -which(names(data) %in% c("Product"))]
      
      subset_data <- data %>% 
        filter(SKU %in% input$sku)
      
      subset_data <- subset_data %>% 
        my.spread(key = c("Region", "Market", "SKU"), value = c("Booking_AC")) %>% 
        pad(interval = "month")
      
      # Replace the filled in 99999.99 values with NA
      subset_data[subset_data == 99999.99] <- NA
      
      # Remove SKUs that have less than the specified # of observations
      subset_data <- subset_data[, which(as.numeric(colSums(!is.na(subset_data))) >= as.numeric(input$observations))]
      
      # Remove SKUs that have more than the speficied # of NA observation within the last # of specified months
      subset_data <- subset_data[, which(as.numeric(colSums(tail(is.na(subset_data), n = as.numeric(input$months1)))) <= as.numeric(input$na_obs))]
      
      if (input$checkbox) {
        if(ncol(subset_data) < 3) {
          return(subset_data)
        } else {
          subset_data$Row_Total <- rowSums(subset_data[,-1], na.rm = TRUE)
        }
      }
      
      return(subset_data)
    })
    
    # Render the final filtered dataset
    output$subset_df <- renderDataTable(extension = c("FixedColumns", "Scroller"), options = list(deferRender = TRUE,
                                                                                                  scrollX = TRUE, 
                                                                                                  scrollY = 1000,
                                                                                                  scroller = TRUE,
                                                                                                  fixedColumns = list(leftColumns = 2)),{
                                                                                                    final_df() 
                                                                                                  })
    
    #################################################### DYNAMIC DROP DOWN LIST FOR TASK BASED ON INPUT FILE ###############################################
    observeEvent(final_df(), {
      
      mySeries <- final_df()
      
      updateSelectInput(session,
                        'i_task_select',
                        label = 'Select Series',
                        choices = names(select(mySeries, -Date)),
                        names(select(mySeries, -Date))[1])
    })
    
    ############################################################ REACTIVE FILTERED DATAFRAME ##############################################################
    mySeries_filtered <- eventReactive(input$goButton, {
      
      # Dependency on 'start forecasting' button being pressed
      
      #### Input$goButton ####
      if (nrow(final_df())==0) 
        return()
      
      # Reset predictions for model performance
      prediction_arima <- 0 # Set the vector 0 to store predicted values
      
      prediction_arfima <- 0 # Set the vector 0 to store predicted values
      
      prediction_croston <- 0 # Set the vector 0 to store predicted values
      
      prediction_ets <- 0 # Set the vector 0 to store predicted values
      
      prediction_hw_M <- 0 # Set the vector 0 to store predicted values
      
      prediction_hw_M_D <- 0 # Set the vector 0 to store predicted values
      
      prediction_hw_A <- 0 # Set the vector 0 to store predicted values
      
      prediction_hw_A_D <- 0 # Set the vector 0 to store predicted values
      
      prediction_lobf <- 0 # Set the vector 0 to store predicted values
      
      prediction_ma <- 0 # Set the vector 0 to store predicted values
      
      prediction_ses <- 0 # Set the vector 0 to store predicted values
      
      prediction_tbats <- 0 # Set the vector 0 to store predicted values
      
      
      j <- 1 # Sets the first index to 1 to store predicted values
      k <- 1 # Sets the first index to 1 to store predicted values
      l <- 1 # Sets the first index to 1 to store predicted values
      m <- 1 # Sets the first index to 1 to store predicted values
      n <- 1 # Sets the first index to 1 to store predicted values
      o <- 1 # Sets the first index to 1 to store predicted values
      p <- 1 # Sets the first index to 1 to store predicted values
      q <- 1 # Sets the first index to 1 to store predicted values
      r <- 1 # Sets the first index to 1 to store predicted values
      s <- 1 # Sets the first index to 1 to store predicted values
      t <- 1 # Sets the first index to 1 to store predicted values
      u <- 1 # Sets the first index to 1 to store predicted values
      
      # Use existing reactive structures
      mySeries <- as.data.frame(final_df())
      
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries,
                              .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else
        {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Build Dataframe
      mySeries_filtered <- mySeries %>% 
        select_(.dots = list(quote(Date), 
                             task_type)) 
    })  
    
    ########################################################## FORECAST MODELS ########################################################################
    
    ####################################
    ####### AUTO ARIMA DYGRAPH  #######
    ###################################
    
    output$p_ARIMA <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_ARIMA <- mySeries_filtered()
      
      isolate({
        mySeries_ARIMA <- mySeries_ARIMA %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_ARIMA) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries,
                              .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_ARIMA,
                          task_type),
                  order.by=ymd(mySeries_ARIMA$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)]
          break
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     # Forecast n periods using model with 50% and 80% confidence intervals
                     if(nrow(myY) < 3){
                       return(NULL)
                     } else{
                       isolate({
                         TS_mySeries_ARIMA <- forecast(auto.arima(myY,
                                                                  stepwise = FALSE,
                                                                  approximation = TRUE),
                                                       h = forecast_n,
                                                       level = c(as.numeric(input$conf_int)))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_ARIMA_df <- with(TS_mySeries_ARIMA,
                                                 data.frame(Mean=TS_mySeries_ARIMA$mean,
                                                            Upper=TS_mySeries_ARIMA$upper[,1],
                                                            Lower=TS_mySeries_ARIMA$lower[,1]))
                       # Add Date column to the forecasted values data.frame
                       forecast_ARIMA_df$Date <- seq(as.Date(max(mySeries_ARIMA$Date)) %m+% months(1),
                                                     by = "month",
                                                     length.out = forecast_n)
                       
                       
                       forecast_ARIMA_df <- forecast_ARIMA_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Convert xts object to ts object
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       #tsmyY2 <- replace(tsmyY, tsmyY == 0, 1) # Replaces 0s in the ts with .01 to calculate MAPE later on
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1] # Takes the first observation and creates a numerical representation of the date
                       timeProp2 <- tsp(tsmyY)[2] # Takes the last observation and creates a numerical representation of the date
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0 # Set the vector 0 to store predicted values
                       j <- 1 # Sets the first index to 1 to store predicted values
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_arima <- forecast(auto.arima(train, stepwise = FALSE, approximation = TRUE) , h = forecast_n) # Creates the model and forecasts
                             prediction[j] <- FC_arima[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                             j <- j+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       arima_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12) # Converts the predictions into a TS object for plotting and measureing accuracy
                       
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_ARIMA, series = task_type),
                                  select_(mySeries_ARIMA, quote(Date)),
                                  order.by=ymd(mySeries_ARIMA$Date))
                       
                       # Converts prediction into xts object for plotting
                       myPred <- xts(select_(forecast_ARIMA_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_ARIMA_df$Date))
                       
                       # Converts previous forecasts from ts object to xts object
                       xts_arima <- xts(arima_FC,
                                        order.by = as.Date(as.yearmon(time(arima_FC))))
                       
                       myDy <- cbind(myX,
                                     myPred,
                                     xts_arima)
                       
                       
                       # Plots the dygraph
                       d <- dygraph(myDy[,1:5], main=paste0('ARIMA FORECAST OF: ', task_type, ' for ', forecast_n, ' Periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_arima', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ####################################
    ####### AUTO ARFIMA DYGRAPH  #######
    ###################################
    
    output$p_ARFIMA <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_ARFIMA <- mySeries_filtered()
      
      isolate({
        mySeries_ARFIMA <- mySeries_ARFIMA %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_ARFIMA) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries,
                              .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_ARFIMA,
                          task_type),
                  order.by=ymd(mySeries_ARFIMA$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      # Convert xts object to ts object
      y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
      m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
      y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
      m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
      
      tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
      #tsmyY2 <- replace(tsmyY, tsmyY == 0, 1) # Replaces 0s in the ts with .01 to calculate MAPE later on
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(all(myY == 0, na.rm = TRUE) == TRUE){
                       return(NULL)
                     } else{
                       if(sum(is.na(myY)) >= 1){
                         return(NULL)
                       } else{
                         # Forecast n periods using model with 50% and 80% confidence intervals
                         isolate({
                           TS_mySeries_ARFIMA <- forecast(forecast::arfima(tsmyY,
                                                                           drange = c(0,.5),
                                                                           estim = c("mle", "ls"),
                                                                           lambda = "auto"),
                                                          h = forecast_n,
                                                          level = c(as.numeric(input$conf_int)))
                         })
                         
                         # Convert elements of time series FORECAST to dataframe for plotting
                         forecast_ARFIMA_df <- with(TS_mySeries_ARFIMA,
                                                    data.frame(Mean=TS_mySeries_ARFIMA$mean,
                                                               Upper=TS_mySeries_ARFIMA$upper[,1],
                                                               Lower=TS_mySeries_ARFIMA$lower[,1]))
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_ARFIMA_df$Date <- seq(as.Date(max(mySeries_ARFIMA$Date)) %m+% months(1),
                                                        by = "month",
                                                        length.out = forecast_n)
                         
                         
                         forecast_ARFIMA_df <- forecast_ARFIMA_df %>%
                           select(Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Lower, 2))))
                         
                         # Obtain the start and end date of the time series in the form of a ratio
                         timeProp <- tsp(tsmyY)[1] # Takes the first observation and creates a numerical representation of the date
                         timeProp2 <- tsp(tsmyY)[2] # Takes the last observation and creates a numerical representation of the date
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY)-forecast_n
                         prediction <- 0 # Set the vector 0 to store predicted values
                         j <- 1 # Sets the first index to 1 to store predicted values
                         
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(nmonths(train) < 5){
                               prediction[j] <- NA
                               j <- j+1
                             } else{
                               # Creates the model and forecasts
                               FC_arfima <- forecast(forecast::arfima(train,
                                                                      drange = c(0,.5),
                                                                      estim = c("mle", "ls"),
                                                                      lambda = "auto"),
                                                     h = forecast_n) 
                               prediction[j] <- FC_arfima[[2]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                               j <- j+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                         # Replace negative predictions with 0
                         prediction <- replace(prediction, prediction < 0, 0)
                         arfima_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12) # Converts the predictions into a TS object for plotting and measureing accuracy
                         
                         
                         ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                         myX <- xts(select_(mySeries_ARFIMA, series = task_type),
                                    select_(mySeries_ARFIMA, quote(Date)),
                                    order.by=ymd(mySeries_ARFIMA$Date))
                         
                         # Converts prediction into xts object for plotting
                         myPred <- xts(select_(forecast_ARFIMA_df,
                                               quote(Mean),
                                               quote(Upper),
                                               quote(Lower)),
                                       order.by = ymd(forecast_ARFIMA_df$Date))
                         
                         # Converts previous forecasts from ts object to xts object
                         xts_arfima <- xts(arfima_FC,
                                           order.by = as.Date(as.yearmon(time(arfima_FC))))
                         
                         myDy <- cbind(myX,
                                       myPred,
                                       xts_arfima)
                         
                         
                         # Plots the dygraph
                         d <- dygraph(myDy[,1:5], main=paste0('ARFIMA FORECAST OF: ', task_type, ' for ', forecast_n, ' Periods' )) %>%
                           dyAxis("x", drawGrid = FALSE) %>%
                           dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                           dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                           dySeries('xts_arfima', label = "past predictions") %>%
                           dySeries('series') %>%
                           dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                           dyLegend(width = 400) %>%
                           dyRangeSelector()
                         
                         print(d)
                       }
                     }
                   })  
    })
    
    #############################################
    ##### Croston Intermittent Demand Model #####
    #############################################
    
    output$p_croston <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_croston <- mySeries_filtered()
      
      isolate({
        mySeries_croston <- mySeries_croston %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_croston) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_croston, task_type),
                  order.by=ymd(mySeries_croston$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(sum(is.na(myY)) >= 1){
                       return(NULL)
                     } else{
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Forecast n periods using model
                       isolate({
                         forecast_croston <- forecast::croston(tsmyY,
                                                               h=forecast_n)
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_croston_df <- with(forecast_croston,
                                                   data.frame(Mean=forecast_croston$mean,
                                                              Upper=NA,
                                                              Lower=NA))
                       
                       forecast_croston_df$Date <- seq(as.Date(max(mySeries_croston$Date)) %m+% months(1),
                                                       by = "month",
                                                       length.out = forecast_n)
                       
                       forecast_croston_df <- forecast_croston_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2)))
                         )
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else {
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_croston <- forecast::croston(train,
                                                             h=forecast_n)
                             prediction[j] <- FC_croston[[1]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       croston_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_croston, series = task_type),
                                  select_(mySeries_croston, quote(Date)),
                                  order.by=ymd(mySeries_croston$Date))
                       
                       
                       myPred <- xts(select_(forecast_croston_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_croston_df$Date))
                       
                       xts_croston <- xts(croston_FC,
                                          order.by = as.Date(as.yearmon(time(croston_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_croston)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('Croston FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_croston', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ######################################
    ##### Error Trend Seasonal Model #####
    ######################################
    
    output$p_ets <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_ets <- mySeries_filtered()
      
      isolate({
        mySeries_ets <- mySeries_ets %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_ets) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_ets, task_type),
                  order.by=ymd(mySeries_ets$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 3){
                       return(NULL)
                     } else{
                       # Finds the best ets model
                       TS_mySeries_ets <- ets(myY,
                                              allow.multiplicative.trend = TRUE,
                                              opt.crit = c("lik", "amse", "mse", "sigma", "mae"))
                       
                       # Forecast n periods using model
                       isolate({
                         forecast_ets <- forecast(TS_mySeries_ets,
                                                  h=forecast_n,
                                                  level = c(as.numeric(input$conf_int)))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_ets_df <- with(forecast_ets,
                                               data.frame(Mean=forecast_ets$mean,
                                                          Upper=forecast_ets$upper[,1],
                                                          Lower=forecast_ets$lower[,1]))
                       
                       forecast_ets_df$Date <- seq(as.Date(max(mySeries_ets$Date)) %m+% months(1),
                                                   by = "month",
                                                   length.out = forecast_n)
                       
                       forecast_ets_df <- forecast_ets_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_ets <- forecast(ets(train,
                                                    allow.multiplicative.trend = TRUE,
                                                    opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                h = forecast_n)
                             prediction[j] <- FC_ets[[2]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       ets_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_ets, series = task_type),
                                  select_(mySeries_ets, quote(Date)),
                                  order.by=ymd(mySeries_ets$Date))
                       
                       
                       myPred <- xts(select_(forecast_ets_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_ets_df$Date))
                       
                       xts_ets <- xts(ets_FC,
                                      order.by = as.Date(as.yearmon(time(ets_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_ets)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('ETS FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_ets', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
      
    })
    
    #######################################################
    ##### Holt-Winter's Multiplicative Seasonal Model #####
    #######################################################
    
    
    output$p_HW_M <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_HW <- mySeries_filtered()
      
      isolate({
        mySeries_HW <- mySeries_HW %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_HW) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_HW, task_type),
                  order.by=ymd(mySeries_HW$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 15){
                       return(NULL)
                     } else{
                       if(sum(myY == 0, na.rm = TRUE) >= 1) {
                         return(NULL)
                       } else {
                         y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                         m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                         y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                         m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                         tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                         
                         # Forecast n periods using model
                         isolate({
                           forecast_HW <- forecast::hw(tsmyY, 
                                                       h = forecast_n,
                                                       seasonal = "multiplicative",
                                                       damped = FALSE,
                                                       level = as.numeric(input$conf_int))
                         })
                         
                         # Convert elements of time series FORECAST to dataframe for plotting
                         forecast_HW_df <- with(forecast_HW,
                                                data.frame(Mean=forecast_HW$mean,
                                                           Upper=forecast_HW$upper[,1],
                                                           Lower=forecast_HW$lower[,1]))
                         
                         forecast_HW_df$Date <- seq(as.Date(max(mySeries_HW$Date)) %m+% months(1),
                                                    by = "month",
                                                    length.out = forecast_n)
                         
                         forecast_HW_df <- forecast_HW_df %>%
                           select(Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Lower, 2))))
                         
                         # Obtain the start and end date of the time series in the form of a ratio
                         timeProp <- tsp(tsmyY)[1]
                         timeProp2 <- tsp(tsmyY)[2]
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY)-forecast_n
                         prediction <- 0
                         j <- 1
                         
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 15){
                               prediction[j] <- NA
                               j <- j+1
                             } else{
                               FC_HW <- forecast::hw(train, 
                                                     h = forecast_n,
                                                     seasonal = "multiplicative",
                                                     damped = FALSE)
                               prediction[j] <- FC_HW[[2]][[forecast_n]]
                               j <- j+1
                             }
                           }
                         }
                         
                         # Replace negative predictions with 0
                         prediction <- replace(prediction, prediction < 0, 0)
                         HW_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                         myX <- xts(select_(mySeries_HW, series = task_type),
                                    select_(mySeries_HW, quote(Date)),
                                    order.by=ymd(mySeries_HW$Date))
                         
                         
                         myPred <- xts(select_(forecast_HW_df,
                                               quote(Mean),
                                               quote(Upper),
                                               quote(Lower)),
                                       order.by = ymd(forecast_HW_df$Date))
                         
                         xts_HW <- xts(HW_FC,
                                       order.by = as.Date(as.yearmon(time(HW_FC))))
                         
                         myDy <- cbind(myX, myPred, xts_HW)
                         
                         d <- dygraph(myDy[,1:5], main=paste0('HW MULTIPLICATIVE FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                           dyAxis("x", drawGrid = FALSE) %>%
                           dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                           dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                           dySeries('xts_HW', label = "past predictions") %>%
                           dySeries('series') %>%
                           dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                           dyLegend(width = 400) %>%
                           dyRangeSelector()
                         
                         print(d)
                       }
                     }
                   })
    })
    
    
    ##############################################################
    ##### Holt-Winter's Multiplicative Damped Seasonal Model #####
    ##############################################################
    
    
    output$p_HW_MD <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_HW <- mySeries_filtered()
      
      isolate({
        mySeries_HW <- mySeries_HW %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_HW) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_HW, task_type),
                  order.by=ymd(mySeries_HW$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 15){
                       return(NULL)
                     } else{
                       if(sum(myY == 0, na.rm = TRUE) >= 1) {
                         return(NULL)
                       } else {
                         y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                         m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                         y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                         m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                         tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                         
                         # Forecast n periods using model
                         isolate({
                           forecast_HW <- forecast::hw(tsmyY, 
                                                       h = forecast_n,
                                                       seasonal = "multiplicative",
                                                       damped = TRUE,
                                                       level = as.numeric(input$conf_int))
                         })
                         
                         # Convert elements of time series FORECAST to dataframe for plotting
                         forecast_HW_df <- with(forecast_HW,
                                                data.frame(Mean=forecast_HW$mean,
                                                           Upper=forecast_HW$upper[,1],
                                                           Lower=forecast_HW$lower[,1]))
                         
                         forecast_HW_df$Date <- seq(as.Date(max(mySeries_HW$Date)) %m+% months(1),
                                                    by = "month",
                                                    length.out = forecast_n)
                         
                         forecast_HW_df <- forecast_HW_df %>%
                           select(Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Lower, 2))))
                         
                         # Obtain the start and end date of the time series in the form of a ratio
                         timeProp <- tsp(tsmyY)[1]
                         timeProp2 <- tsp(tsmyY)[2]
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY)-forecast_n
                         prediction <- 0
                         j <- 1
                         
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 15){
                               prediction[j] <- NA
                               j <- j+1
                             } else{
                               FC_HW <- forecast::hw(train, 
                                                     h = forecast_n,
                                                     seasonal = "multiplicative",
                                                     damped = TRUE)
                               prediction[j] <- FC_HW[[2]][[forecast_n]]
                               j <- j+1
                             }
                           }
                         }
                         
                         # Replace negative predictions with 0
                         prediction <- replace(prediction, prediction < 0, 0)
                         HW_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                         myX <- xts(select_(mySeries_HW, series = task_type),
                                    select_(mySeries_HW, quote(Date)),
                                    order.by=ymd(mySeries_HW$Date))
                         
                         
                         myPred <- xts(select_(forecast_HW_df,
                                               quote(Mean),
                                               quote(Upper),
                                               quote(Lower)),
                                       order.by = ymd(forecast_HW_df$Date))
                         
                         xts_HW <- xts(HW_FC,
                                       order.by = as.Date(as.yearmon(time(HW_FC))))
                         
                         myDy <- cbind(myX, myPred, xts_HW)
                         
                         d <- dygraph(myDy[,1:5], main=paste0('HW MULTIPLICATIVE DAMPED FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                           dyAxis("x", drawGrid = FALSE) %>%
                           dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                           dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                           dySeries('xts_HW', label = "past predictions") %>%
                           dySeries('series') %>%
                           dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                           dyLegend(width = 400) %>%
                           dyRangeSelector()
                         
                         print(d)
                       }
                     }
                   })
    })
    
    #################################################
    ##### Holt-Winter's Additive Seasonal Model #####
    #################################################
    
    
    output$p_HW_A <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_HW <- mySeries_filtered()
      
      isolate({
        mySeries_HW <- mySeries_HW %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_HW) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_HW, task_type),
                  order.by=ymd(mySeries_HW$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 15){
                       return(NULL)
                     } else{
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Forecast n periods using model
                       isolate({
                         forecast_HW <- forecast::hw(tsmyY, 
                                                     h = forecast_n,
                                                     seasonal = "additive",
                                                     damped = FALSE,
                                                     level = as.numeric(input$conf_int))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_HW_df <- with(forecast_HW,
                                              data.frame(Mean=forecast_HW$mean,
                                                         Upper=forecast_HW$upper[,1],
                                                         Lower=forecast_HW$lower[,1]))
                       
                       forecast_HW_df$Date <- seq(as.Date(max(mySeries_HW$Date)) %m+% months(1),
                                                  by = "month",
                                                  length.out = forecast_n)
                       
                       forecast_HW_df <- forecast_HW_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 15){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_HW <- forecast::hw(train, 
                                                   h = forecast_n,
                                                   seasonal = "additive",
                                                   damped = FALSE)
                             prediction[j] <- FC_HW[[2]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       HW_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_HW, series = task_type),
                                  select_(mySeries_HW, quote(Date)),
                                  order.by=ymd(mySeries_HW$Date))
                       
                       
                       myPred <- xts(select_(forecast_HW_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_HW_df$Date))
                       
                       xts_HW <- xts(HW_FC,
                                     order.by = as.Date(as.yearmon(time(HW_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_HW)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('HW ADDITIVE FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_HW', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    
    ########################################################
    ##### Holt-Winter's Additive Damped Seasonal Model #####
    ########################################################
    
    
    output$p_HW_AD <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_HW <- mySeries_filtered()
      
      isolate({
        mySeries_HW <- mySeries_HW %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_HW) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_HW, task_type),
                  order.by=ymd(mySeries_HW$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 15){
                       return(NULL)
                     } else{
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Forecast n periods using model
                       isolate({
                         forecast_HW <- forecast::hw(tsmyY, 
                                                     h = forecast_n,
                                                     seasonal = "additive",
                                                     damped = TRUE,
                                                     level = as.numeric(input$conf_int))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_HW_df <- with(forecast_HW,
                                              data.frame(Mean=forecast_HW$mean,
                                                         Upper=forecast_HW$upper[,1],
                                                         Lower=forecast_HW$lower[,1]))
                       
                       forecast_HW_df$Date <- seq(as.Date(max(mySeries_HW$Date)) %m+% months(1),
                                                  by = "month",
                                                  length.out = forecast_n)
                       
                       forecast_HW_df <- forecast_HW_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 15){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_HW <- forecast::hw(train, 
                                                   h = forecast_n,
                                                   seasonal = "additive",
                                                   damped = TRUE)
                             prediction[j] <- FC_HW[[2]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       HW_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_HW, series = task_type),
                                  select_(mySeries_HW, quote(Date)),
                                  order.by=ymd(mySeries_HW$Date))
                       
                       
                       myPred <- xts(select_(forecast_HW_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_HW_df$Date))
                       
                       xts_HW <- xts(HW_FC,
                                     order.by = as.Date(as.yearmon(time(HW_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_HW)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('HW ADDITIVE DAMPED FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_HW', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ################################
    ##### Line of Best Fit Model #####
    ################################
    
    
    output$p_LOBF <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_LOBF <- mySeries_filtered()
      
      isolate({
        mySeries_LOBF <- mySeries_LOBF %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_LOBF) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert data to xts
      myY <- xts(select_(mySeries_LOBF, task_type),
                 order.by=ymd(mySeries_LOBF$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph... ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 3){
                       return(NULL)
                     } else{
                       # Create a ts object from the xts object
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Convert xts object to dataframe to create linear model 
                       mySeries_lobf <- data.frame(Date = as.Date(index(myY)), value = coredata(myY)[,]) 
                       
                       # Line of best fit model
                       TS_mySeries_LOBF <- lm(formula = value ~ Date,
                                              data = mySeries_lobf,
                                              na.action = na.exclude)
                       
                       # Convert elements of time series MODEL to dataframe for plotting
                       fit_LOBF_df <- as.data.frame(cbind(as.vector(TS_mySeries_LOBF$fitted.values),
                                                          mySeries_lobf$Date))
                       
                       colnames(fit_LOBF_df) <- c('fitted','Date')
                       
                       # Convert to TS object with monthly frequency
                       myTS <-  xts(fit_LOBF_df$fitted,
                                    order.by = as.Date(fit_LOBF_df$Date))
                       
                       # Forecast n periods using model
                       isolate({
                         TS_mySeries_LOBF <- forecast(myTS,
                                                      h=forecast_n,
                                                      level = c(as.numeric(input$conf_int)))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_LOBF_df <- with(TS_mySeries_LOBF,
                                                data.frame(Mean=TS_mySeries_LOBF$mean,
                                                           Upper=TS_mySeries_LOBF$upper[,1],
                                                           Lower=TS_mySeries_LOBF$lower[,1]))
                       
                       forecast_LOBF_df$Date <- seq(as.Date(max(mySeries_lobf$Date)) %m+% months(1),
                                                    by = "month",
                                                    length.out = forecast_n)
                       
                       forecast_LOBF_df <- forecast_LOBF_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           # Convert ts to xts object as indexing produced wrong dates when using the ts object
                           train2 <- as.xts(train)
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             # Convert train ts object to a dataframe to create a linear model
                             df <- data.frame(time = as.Date(index(train2)), value = coredata(train2)[,])
                             
                             # Create a linear model
                             lobf_series <- lm(formula = value ~ time, 
                                               data = df,
                                               na.action = na.exclude)
                             
                             # Create a dataframe of the fitted values from the linear model and cbind it to the corresponding date from the train dataframe
                             fit_lobf <- as.data.frame(cbind(as.vector(lobf_series$fitted.values),
                                                             df$time))
                             # Convert to a xts object
                             lobf_ts <- xts(fit_lobf$V1,
                                            order.by = as.Date(fit_lobf$V2))
                             
                             # Create forecasts
                             FC_lobf <- forecast(lobf_ts,
                                                 h = forecast_n)
                             
                             # Store forecasts into prediction vector
                             prediction[j] <- FC_lobf$mean[[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       lobf_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_LOBF, series = task_type),
                                  select_(mySeries_LOBF, quote(Date)),
                                  order.by=ymd(mySeries_LOBF$Date))
                       
                       
                       myPred <- xts(select_(forecast_LOBF_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_LOBF_df$Date))
                       
                       xts_lobf <- xts(lobf_FC,
                                       order.by = as.Date(as.yearmon(time(lobf_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_lobf)
                       
                       
                       
                       d <- dygraph(myDy[,1:5], main=paste0('LINE OF BEST FIT of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_lobf', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ################################
    ##### Moving Average Model #####
    ################################
    
    
    output$p_MA <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_MA <- mySeries_filtered()
      
      isolate({
        mySeries_MA <- mySeries_MA %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_MA) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_MA, task_type),
                  order.by=ymd(mySeries_MA$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph... ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 3){
                       return(NULL)
                     } else{
                       # Define moving-average model
                       TS_mySeries_MA <- auto.arima(myY, max.p=0, stationary=TRUE, seasonal=FALSE)
                       
                       # Forecast n periods using model
                       isolate({
                         forecast_MA <- forecast(TS_mySeries_MA,
                                                 h=forecast_n,
                                                 level = c(as.numeric(input$conf_int)))
                       })
                       
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_MA_df <- with(forecast_MA,
                                              data.frame(Mean=forecast_MA$mean,
                                                         Upper=forecast_MA$upper[,1],
                                                         Lower=forecast_MA$lower[,1]))
                       
                       forecast_MA_df$Date <- seq(as.Date(max(mySeries_MA$Date)) %m+% months(1),
                                                  by = "month",
                                                  length.out = forecast_n)
                       
                       forecast_MA_df <- forecast_MA_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_ma <- forecast(auto.arima(train,
                                                          max.p=0,
                                                          stationary=TRUE,
                                                          seasonal=FALSE),
                                               h = forecast_n)
                             prediction[j] <- FC_ma[[4]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       ma_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_MA, series = task_type),
                                  select_(mySeries_MA, quote(Date)),
                                  order.by=ymd(mySeries_MA$Date))
                       
                       
                       myPred <- xts(select_(forecast_MA_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_MA_df$Date))
                       
                       xts_ma <- xts(ma_FC,
                                     order.by = as.Date(as.yearmon(time(ma_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_ma)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('MA FORECAST of: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_ma', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ###################################
    #### Simple Exponential Model #####
    ###################################
    
    
    output$p_SES <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_SES <- mySeries_filtered()
      
      isolate({
        mySeries_SES <- mySeries_SES %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_SES) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_SES, task_type),
                  order.by=ymd(mySeries_SES$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(nrow(myY) < 3){
                       return(NULL)
                     } else{
                       # Forecast n periods using model with selected confidence interval
                       isolate({
                         TS_mySeries_SES <- forecast::ses(myY,
                                                          h=forecast_n,
                                                          initial=c('optimal', 'simple'),
                                                          level = c(as.numeric(input$conf_int)))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_SES_df <- with(TS_mySeries_SES,
                                               data.frame(Mean=TS_mySeries_SES$mean,
                                                          Upper=TS_mySeries_SES$upper[,1],
                                                          Lower=TS_mySeries_SES$lower[,1]))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_SES_df$Date <- seq(as.Date(max(mySeries_SES$Date)) %m+% months(1),
                                                   by = "month",
                                                   length.out = forecast_n)
                       
                       forecast_SES_df <- forecast_SES_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_ses <- forecast::ses(train,
                                                     h=forecast_n,
                                                     initial='optimal')
                             prediction[j] <- FC_ses[[2]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       ses_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_SES, series = task_type),
                                  select_(mySeries_SES, quote(Date)),
                                  order.by=ymd(mySeries_SES$Date))
                       
                       
                       myPred <- xts(select_(forecast_SES_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_SES_df$Date))
                       
                       xts_ses <- xts(ses_FC,
                                      order.by = as.Date(as.yearmon(time(ses_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_ses)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('SES FORECAST OF: ', task_type, ' for ', forecast_n, ' Periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_ses', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    ###################################
    ##### TBATS State Space Model #####
    ###################################
    
    
    output$p_TBATS <- renderDygraph({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_TBATS <- mySeries_filtered()
      
      isolate({
        mySeries_TBATS <- mySeries_TBATS %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      if (nrow(mySeries_TBATS) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_TBATS, task_type),
                  order.by=ymd(mySeries_TBATS$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      withProgress(message = 'Generating Graph... ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if(sum(is.na(myY)) >= 1){
                       return(NULL)
                     } else{
                       #Create a ts object from the xts object
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                       
                       #  Forecast n periods using model
                       isolate({
                         forecast_TBATS <- forecast(tbats(tsmyY), # Create TBATS model
                                                    h=forecast_n,
                                                    level = c(as.numeric(input$conf_int)))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_TBATS_df <- with(forecast_TBATS,
                                                 data.frame(Mean=forecast_TBATS$mean,
                                                            Upper=forecast_TBATS$upper[,1],
                                                            Lower=forecast_TBATS$lower[,1]))
                       
                       forecast_TBATS_df$Date <- seq(as.Date(max(mySeries_TBATS$Date)) %m+% months(1),
                                                     by = "month",
                                                     length.out = forecast_n)
                       
                       forecast_TBATS_df <- forecast_TBATS_df %>%
                         select(Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Obtain the start and end date of the time series in the form of a ratio
                       timeProp <- tsp(tsmyY)[1]
                       timeProp2 <- tsp(tsmyY)[2]
                       # Create start and end point of holdout period
                       holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                       holdout_end <- nrow(myY)-forecast_n
                       prediction <- 0
                       j <- 1
                       
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction[j] <- NA
                             j <- j+1
                           } else{
                             FC_tbats <- forecast(tbats(train),
                                                  h = forecast_n)
                             prediction[j] <- FC_tbats[[2]][[forecast_n]]
                             j <- j+1
                           }
                         }
                       }
                       
                       # Replace negative predictions with 0
                       prediction <- replace(prediction, prediction < 0, 0)
                       tbats_FC <- prediction %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       ##### CONSTRUCT DYGRAPH VISUALIZATION #####
                       myX <- xts(select_(mySeries_TBATS, series = task_type),
                                  select_(mySeries_TBATS, quote(Date)),
                                  order.by=ymd(mySeries_TBATS$Date))
                       
                       
                       myPred <- xts(select_(forecast_TBATS_df,
                                             quote(Mean),
                                             quote(Upper),
                                             quote(Lower)),
                                     order.by = ymd(forecast_TBATS_df$Date))
                       
                       xts_tbats <- xts(tbats_FC,
                                        order.by = as.Date(as.yearmon(time(tbats_FC))))
                       
                       myDy <- cbind(myX, myPred, xts_tbats)
                       
                       d <- dygraph(myDy[,1:5], main=paste0('TBATS: ', task_type, ' for ', forecast_n, ' periods' )) %>%
                         dyAxis("x", drawGrid = FALSE) %>%
                         dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2")) %>%
                         dySeries(c('Upper','Mean','Lower'), label="predicted") %>%
                         dySeries('xts_tbats', label = "past predictions") %>%
                         dySeries('series') %>%
                         dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                         dyLegend(width = 400) %>%
                         dyRangeSelector()
                       
                       print(d)
                     }
                   })
    })
    
    #############################################
    ############ Batch Forecasting ##############
    #############################################
    
    ###### ARIMA ######
    
    # Creates a datatable with arima forecasts when the batch forecast button is pressed
    arima <- eventReactive(input$batch_FC, {
      
      # Load the dataset created by the user 
      mySeries_arima <- final_df()
      
      # Filter the data by date as specified by the user
      mySeries_arima <- mySeries_arima %>% 
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      # Create an xts object of the data chosen
      myY <- xts(mySeries_arima[,-1], 
                 order.by = ymd(mySeries_arima$Date))
      
      # Allow the user to decide how many periods ahead to forecast
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
        
      })
      
      # Creates a loading bar as the forecast is created
      withProgress(message = 'Generating ARIMA Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$ARIMAmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- 0
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         arima_fcast <- forecast(auto.arima(myY2,
                                                            stepwise = FALSE,
                                                            approximation = TRUE),
                                                 h = forecast_n,
                                                 level = as.numeric(input$conf_int2))
                         Mean[i] <- arima_fcast$mean[forecast_n]
                         Upper[i] <- arima_fcast$upper[forecast_n]
                         Lower[i] <- arima_fcast$lower[forecast_n]
                         Parameters[i] <- arima_fcast$method
                       }
                       
                       
                       # rbind the point, upper, and lower forecast
                       arima_df <- as.data.frame(rbind(Mean, Upper, Lower, Parameters))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       arima_df <- as.data.frame(t(arima_df))
                       # Add the product name 
                       arima_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       arima_df$Date <- as.Date(max(mySeries_arima$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe 
                       arima_df$Model <- "ARIMA"
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts 
                       pred_ARIMA <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                       # Create an empty matrix to store the month being forecasted
                       arima_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(tsmyY),i]
                             break
                           }
                         }
                         
                         # Convert new xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_ARIMA[i,k] <- NA
                             arima_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(!is.na(train)) < 3){
                               pred_ARIMA[i,k] <- NA
                               arima_months[i,k] <- NA
                               k <- k+1
                             } else{
                               FC_arima <- forecast(auto.arima(train, 
                                                               stepwise = FALSE, 
                                                               approximation = TRUE) , 
                                                    h = forecast_n) # Creates the model and forecasts
                               pred_ARIMA[i,k] <- FC_arima[[4]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                               arima_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                               k <- k+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       arima_month_df <- arima_months %>%
                         as.data.frame()
                       
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(arima_month_df))
                       for(i in 1:nrow(arima_month_df)){
                         for(j in 1:ncol(arima_month_df)){
                           if(all(is.na(arima_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(arima_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- arima_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                       pred_ARIMA_DF <- pred_ARIMA %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_ARIMA_TS <- pred_ARIMA_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results 
                       my_arima_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function 
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements 
                       for (i in 1:ncol(tsmyY)){
                         my_arima_accuracy[i,] <- pred_ARIMA_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_arima_accuracy_DF <- as.data.frame(my_arima_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_arima_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       arima_df_final <- cbind(arima_df, my_arima_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       arima_df_final$Holdout_Start <- as.yearmon(arima_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       arima_df_final$Product <- gsub("_Booking_AC", " ", arima_df_final$Product)
                       # Remove the date that is displayed in column name in certain ARIMA forecasts
                       arima_df_final$Product <- gsub("\\..*", "", arima_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       arima_df_final$Upper <- as.integer(arima_df_final$Upper)
                       arima_df_final$Mean <- as.integer(arima_df_final$Mean)
                       arima_df_final$Lower <- as.integer(arima_df_final$Lower)
                       
                       arima_df_final <- arima_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       
                       return(arima_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### ARFIMA ######
    
    # Creates a datatable with arima forecasts when the batch forecast button is pressed
    arfima <- eventReactive(input$batch_FC, {
      
      # Load the dataset created by the user 
      mySeries_arfima <- final_df()
      
      # Filter the data by date as specified by the user
      mySeries_arfima <- mySeries_arfima %>% 
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      # Create an xts object of the data chosen
      myY <- xts(mySeries_arfima[,-1], 
                 order.by = ymd(mySeries_arfima$Date))
      
      # Allow the user to decide how many periods ahead to forecast
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      # Creates a loading bar as the forecast is created
      withProgress(message = 'Generating ARFIMA Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$ARFIMAmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- 0
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         if(sum(is.na(myY2)) >= 1) {
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i] <- NA
                         } else{
                           if(sum(!is.na(myY2)) < 5){
                             Mean[i] <- NA
                             Upper[i] <- NA
                             Lower[i] <- NA
                             Parameters[i] <- NA
                           } else{
                             if(all(myY2 == 0, na.rm = TRUE) == TRUE){
                               Mean[i] <- NA
                               Upper[i] <- NA
                               Lower[i] <- NA
                               Parameters[i] <- NA
                             } else {
                               arfima_fcast <- forecast(forecast::arfima(myY2,
                                                                         drange = c(0,.5),
                                                                         estim = c("mle", "ls"),
                                                                         lambda = "auto"),
                                                        h = forecast_n,
                                                        level = as.numeric(input$conf_int2))
                               Mean[i] <- arfima_fcast$mean[forecast_n]
                               Upper[i] <- arfima_fcast$upper[forecast_n]
                               Lower[i] <- arfima_fcast$lower[forecast_n]
                               Parameters[i] <- arfima_fcast$method
                             }
                           }
                         }
                       }
                       
                       # rbind the point, upper, and lower forecast
                       arfima_df <- as.data.frame(rbind(Mean, Upper, Lower, Parameters))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       arfima_df <- as.data.frame(t(arfima_df))
                       # Add the product name 
                       arfima_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       arfima_df$Date <- as.Date(max(mySeries_arfima$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe 
                       arfima_df$Model <- "ARFIMA"
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts 
                       pred_ARFIMA <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                       # Create an empty matrix to store the month being forecasted
                       arfima_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(tsmyY),i]
                             break
                           }
                         }
                         
                         # Convert new xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         if(all(myY2 == 0, na.rm = TRUE) == TRUE){
                           for (j in holdout_start:holdout_end){
                             pred_ARFIMA[i,k] <- NA
                             arfima_months[i,k] <- NA
                             k <- k+1
                           }
                         } else{
                           timeProp <- tsp(tsmyY2)[1]
                           for (j in holdout_start:holdout_end){
                             
                             if(j < 2){
                               pred_ARFIMA[i,k] <- NA
                               arfima_months[i,k] <- NA
                               k <- k+1
                             } else{
                               train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                               
                               if(sum(!is.na(train)) < 5){
                                 pred_ARFIMA[i,k] <- NA
                                 arfima_months[i,k] <- NA
                                 k <- k+1
                               } else{
                                 if(sum(is.na(train)) >=1){
                                   pred_ARFIMA[i,k] <- NA
                                   arfima_months[i,k] <- NA
                                   k <- k+1
                                 } else{
                                   if(all(train == 0, na.rm = TRUE) == TRUE){
                                     pred_ARFIMA[i,k] <- NA
                                     arfima_months[i,k] <- NA
                                     k <- k+1
                                   } else{
                                     FC_arfima <- forecast::forecast(forecast::arfima(y = train, 
                                                                                      drange = c(0,.5),
                                                                                      estim = c("mle", "ls"),
                                                                                      lambda = "auto"), 
                                                                     h = forecast_n) # Creates the model and forecasts
                                     pred_ARFIMA[i,k] <- FC_arfima[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                     arfima_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month that is being forecasted 
                                     k <- k+1 # Creates another index to store the next prediction
                                   }
                                 }
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       arfima_month_df <- arfima_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       holdout_start <- matrix(ncol = 1, nrow = nrow(arfima_month_df)) # Create matrix to store each month
                       for(i in 1:nrow(arfima_month_df)){
                         for(j in 1:ncol(arfima_month_df)){
                           if(all(is.na(arfima_month_df[i,]))){
                             holdout_start[i,1] <- NA
                           } else{
                             if(is.na(arfima_month_df[i,j])){
                               next
                             } else{
                               holdout_start[i,1] <- arfima_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to data frame
                       holdout_start <- as.data.frame(holdout_start)
                       # Change column name
                       colnames(holdout_start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                       pred_ARFIMA_DF <- pred_ARFIMA %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_ARFIMA_TS <- pred_ARFIMA_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results 
                       my_arfima_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function 
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements 
                       for (i in 1:ncol(tsmyY)){
                         my_arfima_accuracy[i,] <- pred_ARFIMA_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_arfima_accuracy_DF <- as.data.frame(my_arfima_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_arfima_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       arfima_df_final <- cbind(arfima_df, my_arfima_accuracy_DF, holdout_start)
                       # Convert holdout_start to yearmon format
                       arfima_df_final$Holdout_Start <- as.yearmon(arfima_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       arfima_df_final$Product <- gsub("_Booking_AC", " ", arfima_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       arfima_df_final$Upper <- as.integer(arfima_df_final$Upper)
                       arfima_df_final$Mean <- as.integer(arfima_df_final$Mean)
                       arfima_df_final$Lower <- as.integer(arfima_df_final$Lower)
                       
                       arfima_df_final <- arfima_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       
                       return(arfima_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    # Creates a datatable with croston forecasts when the batch forecast button is pressed
    croston <- eventReactive(input$batch_FC, {
      
      # Load the dataset created by the user 
      mySeries_croston <- final_df()
      
      # Filter the data by date as specified by the user
      mySeries_croston <- mySeries_croston %>% 
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      # Create an xts object of the data chosen
      myY <- xts(mySeries_croston[,-1], 
                 order.by = ymd(mySeries_croston$Date))
      
      # Allow the user to decide how many periods ahead to forecast
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      # Creates a loading bar as the forecast is created
      withProgress(message = 'Generating CROSTON Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$CROSTONmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(sum(is.na(tsmyY2)) >= 1){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i,] <- NA
                         } else{
                           if(sum(!is.na(tsmyY2)) < 3){
                             Mean[i] <- NA
                             Upper[i] <- NA
                             Lower[i] <- NA
                             Parameters[i,] <- NA
                           } else{
                             if(sum(tsmyY2 != 0) < 2){
                               croston_fcast <- forecast::croston(tsmyY2, 
                                                                  h = forecast_n)
                               Mean[i] <- croston_fcast$mean[forecast_n]
                               Upper[i] <- NA
                               Lower[i] <- NA
                               Parameters[i,] <- NA
                             } else{
                               croston_fcast <- forecast::croston(tsmyY2, 
                                                                  h = forecast_n)
                               Mean[i] <- croston_fcast$mean[forecast_n]
                               Upper[i] <- NA
                               Lower[i] <- NA
                               Parameters[i,] <- croston_fcast$model$demand$model$components
                             }
                           }
                         } 
                       }
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, 
                                          paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       croston_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       croston_df <- as.data.frame(t(croston_df))
                       # Add the Names column 
                       croston_df$Product <- colnames(tsmyY)
                       # Add the Parameters column 
                       croston_df$Parameters <- Parameters
                       # Add the date to the forecasts
                       croston_df$Date <- as.Date(max(mySeries_croston$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe 
                       croston_df$Model <- "CROSTON"
                       
                       # Create an empty matrix to store the recursive forecasts 
                       pred_CROSTON <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       croston_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_CROSTON[i,k] <- NA
                             croston_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(is.na(train)) >= 1){
                               pred_CROSTON[i,k] <- NA
                               croston_months[i,k] <- NA
                               k <- k+1
                             } else{
                               if(nmonths(train) < 3){
                                 pred_CROSTON[i,k] <- NA
                                 croston_months[i,k] <- NA
                                 k <- k+1
                               } else{
                                 FC_croston <- forecast::croston(train, 
                                                                 h = forecast_n) # Creates the model and forecasts
                                 pred_CROSTON[i,k] <- FC_croston[[1]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                 croston_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                 k <- k+1 # Creates another index to store the next prediction
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       croston_month_df <- croston_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(croston_month_df))
                       for(i in 1:nrow(croston_month_df)){
                         for(j in 1:ncol(croston_month_df)){
                           if(all(is.na(croston_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(croston_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- croston_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                       pred_CROSTON_DF <- pred_CROSTON %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_CROSTON_TS <- pred_CROSTON_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results 
                       my_croston_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function 
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements 
                       for (i in 1:ncol(tsmyY)){
                         my_croston_accuracy[i,] <- pred_CROSTON_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_croston_accuracy_DF <- as.data.frame(my_croston_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_croston_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       croston_df_final <- cbind(croston_df, my_croston_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       croston_df_final$Holdout_Start <- as.yearmon(croston_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       croston_df_final$Product <- gsub("_Booking_AC", " ", croston_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       croston_df_final$Upper <- as.integer(croston_df_final$Upper)
                       croston_df_final$Mean <- as.integer(croston_df_final$Mean)
                       croston_df_final$Lower <- as.integer(croston_df_final$Lower)
                       
                       croston_df_final <- croston_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       
                       return(croston_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### ERROR TREND SEASONAL MODEL ######
    
    ets_df <- eventReactive(input$batch_FC, {
      mySeries_ets <- final_df()
      
      mySeries_ets <- mySeries_ets %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_ets[,-1], 
                 order.by = ymd(mySeries_ets$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating ETS Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$ETSmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         if(sum(!is.na(myY2)) < 3){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i,] <- NA
                         } else{
                           ets_fcast <- forecast(ets(myY2,
                                                     allow.multiplicative.trend = TRUE,
                                                     opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                 h = forecast_n,
                                                 level = as.numeric(input$conf_int2))
                           Mean[i] <- ets_fcast$mean[forecast_n]
                           Upper[i] <- ets_fcast$upper[forecast_n]
                           Lower[i] <- ets_fcast$lower[forecast_n]
                           Parameters[i,] <- ets_fcast$model$components
                         }
                       }
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       ets_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       ets_df <- as.data.frame(t(ets_df))
                       # Add the Parameter Column 
                       ets_df$Parameters <- Parameters
                       # Add the product names 
                       ets_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       ets_df$Date <- as.Date(max(mySeries_ets$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       ets_df$Model <- "ETS" 
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_ets <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       ets_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_ets[i,k] <- NA
                             ets_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(!is.na(train)) < 3){
                               pred_ets[i,k] <- NA
                               ets_months[i,k] <- NA
                               k <- k+1
                             } else{
                               FC_ets <- forecast(ets(train,
                                                      allow.multiplicative.trend = TRUE,
                                                      opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                  h = forecast_n)
                               pred_ets[i,k] <- FC_ets[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                               ets_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                               k <- k+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ets_month_df <- ets_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(ets_month_df))
                       for(i in 1:nrow(ets_month_df)){
                         for(j in 1:ncol(ets_month_df)){
                           if(all(is.na(ets_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(ets_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- ets_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_ets_DF <- pred_ets %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_ets_TS <- pred_ets_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_ets_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_ets_accuracy[i,] <- pred_ets_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_ets_accuracy_DF <- as.data.frame(my_ets_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_ets_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       ets_df_final <- cbind(ets_df, my_ets_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       ets_df_final$Holdout_Start <- as.yearmon(ets_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       ets_df_final$Product <- gsub("_Booking_AC", " ", ets_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       ets_df_final$Upper <- as.integer(ets_df_final$Upper)
                       ets_df_final$Mean <- as.integer(ets_df_final$Mean)
                       ets_df_final$Lower <- as.integer(ets_df_final$Lower)
                       
                       ets_df_final <- ets_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper,Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(ets_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### HOLT-WINTERS MULTIPLICATIVE MODEL ######
    
    hw_M_df <- eventReactive(input$batch_FC, {
      mySeries_hw_M <- final_df()
      
      mySeries_hw_M <- mySeries_hw_M %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_hw_M[,-1], 
                 order.by = ymd(mySeries_hw_M$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Holt-Winters Multiplicative Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$HW_M_model){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(nrow(myY2) < 15){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i] <- NA
                         } else{
                           if(sum(tsmyY2 == 0, na.rm = TRUE) >= 1){
                             Mean[i] <- NA
                             Upper[i] <- NA
                             Lower[i] <- NA
                             Parameters[i] <- NA
                           } else{
                             hw_M_fcast <- forecast::hw(tsmyY2, 
                                                        h = forecast_n,
                                                        seasonal = "multiplicative",
                                                        damped = FALSE,
                                                        level = as.numeric(input$conf_int2)
                             )
                             Mean[i] <- hw_M_fcast$mean[forecast_n] 
                             Upper[i] <- hw_M_fcast$upper[forecast_n]
                             Lower[i] <- hw_M_fcast$lower[forecast_n]
                             Parameters[i,] <- hw_M_fcast$model$components
                           }
                         }
                       }
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       hw_M_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       hw_M_df <- as.data.frame(t(hw_M_df))
                       # Add the Parameter Column 
                       hw_M_df$Parameters <- Parameters
                       # Add the product names 
                       hw_M_df$Product <- colnames(tsmyY)
                       # Add the date to the forecasts
                       hw_M_df$Date <- as.Date(max(mySeries_hw_M$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       hw_M_df$Model <- "Holt-Winters Multiplicative Model"
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_hw_M <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       hw_M_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         if(nrow(myY2) < 15){
                           for (j in holdout_start:holdout_end){
                             pred_hw_M[i,k] <- NA
                             hw_M_months[i,k] <- NA
                             k <- k+1
                           }
                         } else{
                           timeProp <- tsp(tsmyY2)[1]
                           for (j in holdout_start:holdout_end){
                             
                             if(j < 2){
                               pred_hw_M[i,k] <- NA
                               hw_M_months[i,k] <- NA
                               k <- k+1
                             } else{
                               train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                               
                               if(nmonths(train) < 15){
                                 pred_hw_M[i,k] <- NA
                                 hw_M_months[i,k] <- NA
                                 k <- k+1
                               } else{ 
                                 if(sum(train == 0, na.rm = TRUE) >=1){
                                   pred_hw_M[i,k] <- NA
                                   hw_M_months[i,k] <- NA
                                   k <- k+1
                                 } else{
                                   FC_HW_M <- forecast::hw(train, 
                                                           h = forecast_n,
                                                           seasonal = "multiplicative",
                                                           damped = FALSE) # Creates the model and forecasts
                                   pred_hw_M[i,k] <- FC_HW_M[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                   hw_M_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                   k <- k+1 # Creates another index to store the next prediction
                                 }
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_M_month_df <- hw_M_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(hw_M_month_df))
                       for(i in 1:nrow(hw_M_month_df)){
                         for(j in 1:ncol(hw_M_month_df)){
                           if(all(is.na(hw_M_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(hw_M_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- hw_M_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_hw_M_DF <- pred_hw_M %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_hw_M_TS <- pred_hw_M_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_hw_M_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_hw_M_accuracy[i,] <- pred_hw_M_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_hw_M_accuracy_DF <- as.data.frame(my_hw_M_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_hw_M_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       hw_M_df_final <- cbind(hw_M_df, my_hw_M_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       hw_M_df_final$Holdout_Start <- as.yearmon(hw_M_df_final$Holdout_Start)
                       # Remove the phrase _actuals from the Product name 
                       hw_M_df_final$Product <- gsub("_Booking_AC", " ", hw_M_df_final$Product)
                       
                       hw_M_df_final <- hw_M_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(hw_M_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### HOLT-WINTERS MULTIPLICATIVE DAMPED TREND MODEL ######
    
    hw_MD_df <- eventReactive(input$batch_FC, {
      mySeries_hw_MD <- final_df()
      
      mySeries_hw_MD <- mySeries_hw_MD %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_hw_MD[,-1], 
                 order.by = ymd(mySeries_hw_MD$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Holt-Winters Multiplicative Damped Trend Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$HW_MD_model){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(nrow(myY2) < 15){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i] <- NA
                         } else{
                           if(sum(tsmyY2 == 0, na.rm = TRUE) >=1){
                             Mean[i] <- NA
                             Upper[i] <- NA
                             Lower[i] <- NA
                             Parameters[i] <- NA
                           } else{
                             hw_MD_fcast <- forecast::hw(tsmyY2, 
                                                         h = forecast_n,
                                                         seasonal = "multiplicative",
                                                         damped = TRUE,
                                                         level = as.numeric(input$conf_int2)
                             )
                             Mean[i] <- hw_MD_fcast$mean[forecast_n] 
                             Upper[i] <- hw_MD_fcast$upper[forecast_n]
                             Lower[i] <- hw_MD_fcast$lower[forecast_n]
                             Parameters[i,] <- hw_MD_fcast$model$components
                           }
                         }
                       }
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       hw_MD_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       hw_MD_df <- as.data.frame(t(hw_MD_df))
                       # Add the Parameter Column 
                       hw_MD_df$Parameters <- Parameters
                       # Add the product names 
                       hw_MD_df$Product <- colnames(tsmyY)
                       # Add the date to the forecasts
                       hw_MD_df$Date <- as.Date(max(mySeries_hw_MD$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       hw_MD_df$Model <- "Holt-Winters Multiplicative Damped Trend Model"
                       
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_hw_MD <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       hw_MD_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         if(nrow(myY2) < 15){
                           for (j in holdout_start:holdout_end){
                             pred_hw_MD[i,k] <- NA
                             hw_MD_months[i,k] <- NA
                             k <- k+1
                           }
                         } else{
                           timeProp <- tsp(tsmyY2)[1]
                           for (j in holdout_start:holdout_end){
                             
                             if(j < 2){
                               pred_hw_MD[i,k] <- NA
                               hw_MD_months[i,k] <- NA
                               k <- k+1
                             } else{
                               train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                               
                               if(nmonths(train) < 15){
                                 pred_hw_MD[i,k] <- NA
                                 hw_MD_months[i,k] <- NA
                                 k <- k+1
                               } else{ 
                                 if(sum(train == 0, na.rm = TRUE) >=1){
                                   pred_hw_MD[i,k] <- NA
                                   hw_MD_months[i,k] <- NA
                                   k <- k+1
                                 } else{
                                   FC_HW_MD <- forecast::hw(train, 
                                                            h = forecast_n,
                                                            seasonal = "multiplicative",
                                                            damped = TRUE) # Creates the model and forecasts
                                   pred_hw_MD[i,k] <- FC_HW_MD[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                   hw_MD_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                   k <- k+1 # Creates another index to store the next prediction
                                 }
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_MD_month_df <- hw_MD_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(hw_MD_month_df))
                       for(i in 1:nrow(hw_MD_month_df)){
                         for(j in 1:ncol(hw_MD_month_df)){
                           if(all(is.na(hw_MD_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(hw_MD_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- hw_MD_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_hw_MD_DF <- pred_hw_MD %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_hw_MD_TS <- pred_hw_MD_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_hw_MD_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_hw_MD_accuracy[i,] <- pred_hw_MD_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_hw_MD_accuracy_DF <- as.data.frame(my_hw_MD_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_hw_MD_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       hw_MD_df_final <- cbind(hw_MD_df, my_hw_MD_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       hw_MD_df_final$Holdout_Start <- as.yearmon(hw_MD_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       hw_MD_df_final$Product <- gsub("_Booking_AC", " ", hw_MD_df_final$Product)
                       
                       hw_MD_df_final <- hw_MD_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(hw_MD_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### HOLT-WINTERS ADDITIVE MODEL ######
    
    hw_A_df <- eventReactive(input$batch_FC, {
      mySeries_hw_A <- final_df()
      
      mySeries_hw_A <- mySeries_hw_A %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_hw_A[,-1], 
                 order.by = ymd(mySeries_hw_A$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Holt-Winters Additive Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$HW_A_model){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(nrow(myY2) < 15){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i,] <- NA
                         } else{
                           hw_A_fcast <- forecast::hw(tsmyY2, 
                                                      h = forecast_n,
                                                      seasonal = "additive",
                                                      damped = FALSE,
                                                      level = as.numeric(input$conf_int2)
                           )
                           Mean[i] <- hw_A_fcast$mean[forecast_n] 
                           Upper[i] <- hw_A_fcast$upper[forecast_n]
                           Lower[i] <- hw_A_fcast$lower[forecast_n]
                           Parameters[i,] <- hw_A_fcast$model$components
                         }
                       }
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       hw_A_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       hw_A_df <- as.data.frame(t(hw_A_df))
                       # Add the Parameter Column 
                       hw_A_df$Parameters <- Parameters
                       # Add the product names 
                       hw_A_df$Product <- colnames(tsmyY)
                       # Add the date to the forecasts
                       hw_A_df$Date <- as.Date(max(mySeries_hw_A$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       hw_A_df$Model <- "Holt-Winters Additive Model"
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_hw_A <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       hw_A_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         if(nrow(myY2) < 15){
                           for (j in holdout_start:holdout_end){
                             pred_hw_A[i,k] <- NA
                             hw_A_months[i,k] <- NA
                             k <- k+1
                           }
                         } else{
                           timeProp <- tsp(tsmyY2)[1]
                           for (j in holdout_start:holdout_end){
                             
                             if(j < 2){
                               pred_hw_A[i,k] <- NA
                               hw_A_months[i,k] <- NA
                               k <- k+1
                             } else{
                               train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                               if(nmonths(train) < 15){
                                 pred_hw_A[i,k] <- NA
                                 hw_A_months[i,k] <- NA
                                 k <- k+1
                               } else{
                                 FC_HW_A <- forecast::hw(train, 
                                                         h = forecast_n,
                                                         seasonal = "additive",
                                                         damped = FALSE) # Creates the model and forecasts
                                 pred_hw_A[i,k] <- FC_HW_A[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                 hw_A_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                 k <- k+1 # Creates another index to store the next prediction
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_A_month_df <- hw_A_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(hw_A_month_df))
                       for(i in 1:nrow(hw_A_month_df)){
                         for(j in 1:ncol(hw_A_month_df)){
                           if(all(is.na(hw_A_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(hw_A_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- hw_A_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_hw_A_DF <- pred_hw_A %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_hw_A_TS <- pred_hw_A_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_hw_A_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_hw_A_accuracy[i,] <- pred_hw_A_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_hw_A_accuracy_DF <- as.data.frame(my_hw_A_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_hw_A_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       hw_A_df_final <- cbind(hw_A_df, my_hw_A_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       hw_A_df_final$Holdout_Start <- as.yearmon(hw_A_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       hw_A_df_final$Product <- gsub("_Booking_AC", " ", hw_A_df_final$Product)
                       
                       hw_A_df_final <- hw_A_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(hw_A_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### HOLT-WINTERS ADDITIVE DAMPED TREND MODEL ######
    
    hw_AD_df <- eventReactive(input$batch_FC, {
      mySeries_hw_AD <- final_df()
      
      mySeries_hw_AD <- mySeries_hw_AD %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_hw_AD[,-1], 
                 order.by = ymd(mySeries_hw_AD$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Holt-Winters Additive Damped Trend Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$HW_AD_model){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(nrow(myY2) < 15){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i,] <- NA
                         } else{
                           hw_AD_fcast <- forecast::hw(tsmyY2, 
                                                       h = forecast_n,
                                                       seasonal = "additive",
                                                       damped = TRUE,
                                                       level = as.numeric(input$conf_int2)
                           )
                           Mean[i] <- hw_AD_fcast$mean[forecast_n] 
                           Upper[i] <- hw_AD_fcast$upper[forecast_n]
                           Lower[i] <- hw_AD_fcast$lower[forecast_n]
                           Parameters[i,] <- hw_AD_fcast$model$components
                         } 
                       }
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       hw_AD_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       hw_AD_df <- as.data.frame(t(hw_AD_df))
                       # Add the Parameter Column 
                       hw_AD_df$Parameters <- Parameters
                       # Add the product names 
                       hw_AD_df$Product <- colnames(tsmyY)
                       # Add the date to the forecasts
                       hw_AD_df$Date <- as.Date(max(mySeries_hw_AD$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       hw_AD_df$Model <- "Holt-Winters Additive Damped Trend Model"
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_hw_AD <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       hw_AD_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast 
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column 
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         if(nrow(myY2) < 15){
                           for (j in holdout_start:holdout_end){
                             pred_hw_AD[i,k] <- NA
                             hw_AD_months[i,k] <- NA
                             k <- k+1
                           }
                         } else{
                           timeProp <- tsp(tsmyY2)[1]
                           for (j in holdout_start:holdout_end){
                             
                             if(j < 2){
                               pred_hw_AD[i,k] <- NA
                               hw_AD_months[i,k] <- NA
                               k <- k+1
                             } else{
                               train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                               if(nmonths(train) < 15){
                                 pred_hw_AD[i,k] <- NA
                                 hw_AD_months[i,k] <- NA
                                 k <- k+1
                               } else{
                                 FC_HW_AD <- forecast::hw(train, 
                                                          h = forecast_n,
                                                          seasonal = "additive",
                                                          damped = TRUE) # Creates the model and forecasts
                                 pred_hw_AD[i,k] <- FC_HW_AD[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                 hw_AD_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                 k <- k+1 # Creates another index to store the next prediction
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_AD_month_df <- hw_AD_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(hw_AD_month_df))
                       for(i in 1:nrow(hw_AD_month_df)){
                         for(j in 1:ncol(hw_AD_month_df)){
                           if(all(is.na(hw_AD_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(hw_AD_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- hw_AD_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_hw_AD_DF <- pred_hw_AD %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_hw_AD_TS <- pred_hw_AD_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_hw_AD_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_hw_AD_accuracy[i,] <- pred_hw_AD_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_hw_AD_accuracy_DF <- as.data.frame(my_hw_AD_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_hw_AD_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       hw_AD_df_final <- cbind(hw_AD_df, my_hw_AD_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       hw_AD_df_final$Holdout_Start <- as.yearmon(hw_AD_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       hw_AD_df_final$Product <- gsub("_Booking_AC", " ", hw_AD_df_final$Product)
                       
                       hw_AD_df_final <- hw_AD_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(hw_AD_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### LINE OF BEST FIT MODEL ######
    
    lobf <- eventReactive(input$batch_FC, {
      mySeries_lobf <- final_df()
      
      mySeries_lobf <- mySeries_lobf %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_lobf[,-1], 
                 order.by = ymd(mySeries_lobf$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        mySeries_lobf <- mySeries_lobf[, which(as.numeric(colSums(tail(mySeries_lobf == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Line of Best Fit Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$LOBFmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- 0
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to dataframe to create linear model
                         lobf_series <- data.frame(time = as.Date(index(myY2)), value = coredata(myY2)[,])
                         
                         # Line of Best fit model
                         lm_lobf <- lm(formula = value ~ time,
                                       data = lobf_series,
                                       na.action = na.exclude)
                         
                         # Convert to dataframe
                         fit_lobf_df <- as.data.frame(cbind(as.vector(lm_lobf$fitted.values),
                                                            lobf_series$time))
                         colnames(fit_lobf_df) <- c("fitted", "Date")
                         
                         # Convert to xts with monthly frequency
                         xts_lobf <- xts(fit_lobf_df$fitted, 
                                         order.by = as.Date(fit_lobf_df$Date))
                         
                         # Forecast n periods
                         lobf_fcast <- forecast(xts_lobf,
                                                h = forecast_n,
                                                level = as.numeric(input$conf_int2))
                         
                         # Store values in vectors
                         Mean[i] <- lobf_fcast$mean[forecast_n]
                         Upper[i] <- lobf_fcast$upper[forecast_n]
                         Lower[i] <- lobf_fcast$lower[forecast_n]
                         Parameters[i] <- NA
                       }
                       
                       # rbind the point, upper, and lower forecast
                       lobf_df <- as.data.frame(rbind(Mean, Upper, Lower, Parameters))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       lobf_df <- as.data.frame(t(lobf_df))
                       # Add the product name 
                       lobf_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       lobf_df$Date <- as.Date(max(mySeries_lobf$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       lobf_df$Model <- "Line_of_Best_Fit"
                       # Remove rownames
                       rownames(lobf_df) <- NULL
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_lobf <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       lobf_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(tsmyY),i]
                             break
                           }
                         }
                         
                         # Convert new xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_lobf[i,k] <- NA
                             lobf_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             # Convert ts to xts object as indexing produced wrong dates when using the ts object
                             train2 <- as.xts(train)
                             
                             if(nmonths(train) < 3){
                               pred_lobf[i,k] <- NA
                               lobf_months[i,k] <- NA
                               k <- k+1
                             } else{
                               # Convert train ts object to a dataframe to create a linear model
                               df <- data.frame(time = as.Date(index(train2)), value = coredata(train2)[,])
                               
                               # Create a linear model
                               lobf_series <- lm(formula = value ~ time, 
                                                 data = df,
                                                 na.action = na.exclude)
                               
                               # Create a dataframe of the fitted values from the linear model and cbind it to the corresponding date from the train dataframe
                               fit_lobf <- as.data.frame(cbind(as.vector(lobf_series$fitted.values),
                                                               df$time))
                               # Convert to a xts object
                               lobf_ts <- xts(fit_lobf$V1,
                                              order.by = as.Date(fit_lobf$V2))
                               
                               # Create forecasts
                               FC_lobf <- forecast(lobf_ts,
                                                   h = forecast_n)
                               pred_lobf[i,k] <- FC_lobf$mean[[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                               lobf_months[i,k] <- tsp(train)[2] + ((forecast_n)/12)
                               k <- k+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                       }
                       
                       # Convert the month lobftrix to a dataframe
                       lobf_month_df <- lobf_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(lobf_month_df))
                       for(i in 1:nrow(lobf_month_df)){
                         for(j in 1:ncol(lobf_month_df)){
                           if(all(is.na(lobf_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(lobf_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- lobf_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert lobftrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_lobf_DF <- pred_lobf %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2]
                       pred_lobf_TS <- pred_lobf_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_lobf_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_lobf_accuracy[i,] <- pred_lobf_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_lobf_accuracy_DF <- as.data.frame(my_lobf_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_lobf_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       lobf_df_final <- cbind(lobf_df, my_lobf_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       lobf_df_final$Holdout_Start <- as.yearmon(lobf_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name
                       lobf_df_final$Product <- gsub("_Booking_AC", " ", lobf_df_final$Product)
                       
                       lobf_df_final <- lobf_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(lobf_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### MOVING AVERAGE ######
    
    ma_df <- eventReactive(input$batch_FC, {
      
      mySeries_ma <- final_df()
      
      mySeries_ma <- mySeries_ma %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_ma[,-1], 
                 order.by = ymd(mySeries_ma$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating Moving Average Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$MAmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- 0
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         if(nrow(myY2) < 3){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i] <- NA
                         } else{
                           ma_fcast <- forecast(auto.arima(myY2,
                                                           max.p = 0,
                                                           stationary = TRUE,
                                                           seasonal = FALSE),
                                                h = forecast_n,
                                                level = as.numeric(input$conf_int2))
                           Mean[i] <- ma_fcast$mean[forecast_n]
                           Upper[i] <- ma_fcast$upper[forecast_n]
                           Lower[i] <- ma_fcast$lower[forecast_n]
                           Parameters[i] <- ma_fcast$method
                         }
                       }
                       
                       # rbind the point, upper, and lower forecast
                       ma_df <- as.data.frame(rbind(Mean, Upper, Lower, Parameters))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       ma_df <- as.data.frame(t(ma_df))
                       # Add the product name 
                       ma_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       ma_df$Date <- as.Date(max(mySeries_ma$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       ma_df$Model <- "MA"
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_ma <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                       # Create an empty matrix to store the month being forecasted
                       ma_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       for (i in 1:ncol(tsmyY)){
                         k <- 1
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(tsmyY),i]
                             break
                           }
                         }
                         
                         # Convert new xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_ma[i,k] <- NA
                             ma_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(!is.na(train)) < 3) {
                               pred_ma[i,k] <- NA
                               ma_months[i,k] <- NA
                               k <- k+1
                             } else{
                               FC_ma <- forecast(auto.arima(train, 
                                                            max.p = 0, 
                                                            stationary = TRUE, 
                                                            seasonal = FALSE) , 
                                                 h = forecast_n) # Creates the model and forecasts
                               pred_ma[i,k] <- FC_ma[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                               ma_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                               k <- k+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ma_month_df <- ma_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(ma_month_df))
                       for(i in 1:nrow(ma_month_df)){
                         for(j in 1:ncol(ma_month_df)){
                           if(all(is.na(ma_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(ma_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- ma_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                       pred_ma_DF <- pred_ma %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_ma_TS <- pred_ma_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_ma_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_ma_accuracy[i,] <- pred_ma_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_ma_accuracy_DF <- as.data.frame(my_ma_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_ma_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       ma_df_final <- cbind(ma_df, my_ma_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       ma_df_final$Holdout_Start <- as.yearmon(ma_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       ma_df_final$Product <- gsub("_Booking_AC", " ", ma_df_final$Product)
                       # Remove the date that is displayed in column name in certain ARIMA forecasts
                       ma_df_final$Product <- gsub("\\..*", "", ma_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       ma_df_final$Upper <- as.integer(ma_df_final$Upper)
                       ma_df_final$Mean <- as.integer(ma_df_final$Mean)
                       ma_df_final$Lower <- as.integer(ma_df_final$Lower)
                       
                       ma_df_final <- ma_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(ma_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### SIMPLE EXPONENTIAL SMOOTHING ######
    
    ses <- eventReactive(input$batch_FC, {
      
      mySeries_ses <- final_df()
      
      mySeries_ses <- mySeries_ses %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_ses[,-1], 
                 order.by = ymd(mySeries_ses$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating SES Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$SESmodel){
                       Mean <- 0
                       Upper <- 0
                       Lower <- 0
                       Parameters <- matrix(nrow=ncol(myY), ncol = 4)
                       for(i in 1:ncol(myY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         if(sum(!is.na(myY2)) < 3){
                           Mean[i] <- NA
                           Upper[i] <- NA
                           Lower[i] <- NA
                           Parameters[i,] <- NA
                         } else{
                           ses_fcast <- forecast::ses(myY2,
                                                      initial = "optimal",
                                                      h = forecast_n,
                                                      level = as.numeric(input$conf_int2))
                           Mean[i] <- ses_fcast$mean[forecast_n]
                           Upper[i] <- ses_fcast$upper[forecast_n]
                           Lower[i] <- ses_fcast$lower[forecast_n]
                           Parameters[i,] <- ses_fcast$model$components
                         }
                       }
                       
                       # Convert Parameters to a data frame
                       Parameters <- as.data.frame(Parameters)
                       # Create a single row with each parameter as its own column
                       Parameters <- with(Parameters, paste0(Parameters[,1], ", ", Parameters[,2], ", ", Parameters[,3],", ", Parameters[,4]))
                       # rbind the point, upper, and lower forecast
                       ses_df <- as.data.frame(rbind(Mean, Upper, Lower))
                       # Transpose the dataset to have the Mean, upper and lower as columns and each product its own row
                       ses_df <- as.data.frame(t(ses_df))
                       # Add the Parameter Column 
                       ses_df$Parameters <- Parameters
                       # Add the product names 
                       ses_df$Product <- colnames(myY)
                       # Add the date to the forecasts
                       ses_df$Date <- as.Date(max(mySeries_ses$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       ses_df$Model <- "SES"
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Convert xts object to a ts object
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_ses <- matrix(nrow = ncol(tsmyY), ncol = recent_months) 
                       # Create an empty matrix to store the month being forecasted
                       ses_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast
                       for (i in 1:ncol(tsmyY)){
                         k <- 1
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_ses[i,k] <- NA
                             ses_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(!is.na(train)) < 3){
                               pred_ses[i,k] <- NA
                               ses_months[i,k] <- NA
                               k <- k+1
                             } else{
                               FC_ses <- forecast::ses(train,
                                                       h=forecast_n,
                                                       initial=c('optimal', 'simple'))
                               pred_ses[i,k] <- FC_ses[[2]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                               ses_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                               k <- k+1 # Creates another index to store the next prediction
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ses_month_df <- ses_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(ses_month_df))
                       for(i in 1:nrow(ses_month_df)){
                         for(j in 1:ncol(ses_month_df)){
                           if(all(is.na(ses_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(ses_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- ses_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0 
                       pred_ses_DF <- pred_ses %>% 
                         t() %>%
                         as.data.frame() %>% 
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>% 
                         apply(2, function(x) round(x, 2)) %>% 
                         apply(2, function(x) as.integer(x)) %>% 
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2] 
                       pred_ses_TS <- pred_ses_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_ses_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_ses_accuracy[i,] <- pred_ses_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_ses_accuracy_DF <- as.data.frame(my_ses_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_ses_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       ses_df_final <- cbind(ses_df, my_ses_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       ses_df_final$Holdout_Start <- as.yearmon(ses_df_final$Holdout_Start)
                       # Remove the phrase _actuals.mean from the Product name 
                       ses_df_final$Product <- gsub("_Booking_AC", " ", ses_df_final$Product)
                       # Convert to Mean, Upper and Lower to integers
                       ses_df_final$Upper <- as.integer(ses_df_final$Upper)
                       ses_df_final$Mean <- as.integer(ses_df_final$Mean)
                       ses_df_final$Lower <- as.integer(ses_df_final$Lower)
                       
                       ses_df_final <- ses_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(Mean < 0,
                                              0,
                                              round(Mean, 2)),
                                Upper = ifelse(Upper < 0,
                                               0,
                                               round(Upper, 2)),
                                Lower = ifelse(Lower < 0,
                                               0,
                                               round(Lower, 2)),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(ses_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ####################################################################################################################################
    
    ###### TBATS MODEL ######
    
    tbats_df <- eventReactive(input$batch_FC, {
      mySeries_tbats <- final_df()
      
      mySeries_tbats <- mySeries_tbats %>%
        filter(Date >= input$dateRange2[1] &
                 Date <= input$dateRange2[2])
      
      myY <- xts(mySeries_tbats[,-1], 
                 order.by = ymd(mySeries_tbats$Date))
      
      isolate({
        forecast_n <- input$forecast_n
        recent_months <- input$i_recent_months2
        
        # Remove SKUs that contain more than the specified number of zero observation within the specified number of recent months
        myY <- myY[, which(as.numeric(colSums(tail(myY == 0, n = input$months), na.rm = TRUE)) <= as.numeric(input$zero_obs))]
      })
      
      withProgress(message = 'Generating TBATS Forecasts... ',
                   detail = 'this may take several minutes to hours depending on the number of SKUs',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     if (input$TBATSmodel){
                       
                       y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                       m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                       y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                       m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                       
                       tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12)
                       
                       # Create empty matrices to store mean, upper and lower forecast
                       Mean <- matrix(nrow = ncol(tsmyY), ncol = 1)
                       Upper <- matrix(nrow = ncol(tsmyY), ncol = 1)
                       Lower <- matrix(nrow = ncol(tsmyY), ncol = 1)
                       Parameters <- matrix(nrow=ncol(myY), ncol = 1)
                       for (i in 1:ncol(tsmyY)){
                         
                         # Set the start of the xts object to the first non-NA observation
                         for(j in 1:nrow(myY)){
                           if(is.na(myY[j,i])){
                             next
                           } else{
                             myY2 <- myY[j:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         if(sum(is.na(tsmyY2)) >= 1){
                           Mean[i,] <- NA
                           Upper[i,] <- NA
                           Lower[i,] <- NA
                           Parameters[i] <- NA
                         } else{
                           if(nrow(myY2) < 3){
                             Mean[i,] <- NA
                             Upper[i,] <- NA
                             Lower[i,] <- NA
                             Parameters[i] <- NA
                           } else{
                             tbats_fcast <- forecast(tbats(tsmyY2),
                                                     h = forecast_n,
                                                     level = as.numeric(input$conf_int2))
                             Mean[i,] <- tbats_fcast$mean[[forecast_n]] # Store the mean forecast
                             Upper[i,] <- tbats_fcast$upper[[forecast_n]] # Store the upper forecast
                             Lower[i,] <- tbats_fcast$lower[[forecast_n]] # Store the lower forecast
                             Parameters[i] <- tbats_fcast$method # Store the Parameters 
                           }
                         }
                       }
                       # Column bind the Mean, Upper and lower forecast and convert to data frame
                       tbats_df <- as.data.frame(cbind(Mean, Upper, Lower, Parameters))
                       # Convert Mean, Upper, and Lower to numeric, otherwise it will be factor and cause issues later
                       tbats_df[,1:3] <- as.data.frame(apply(tbats_df[,1:3], 2, as.numeric))
                       # Obtain the SKU names
                       SKU_names <- as.vector(colnames(tsmyY))
                       # Bind the matrices by columns and convert to a dataframe
                       tbats_df <- as.data.frame(cbind(SKU_names, tbats_df))
                       # Give the columns their appropiate names 
                       colnames(tbats_df) <- c("Product", "Mean", "Upper", "Lower", "Parameters")
                       # Add the date to the forecasts
                       tbats_df$Date <- as.Date(max(mySeries_tbats$Date)) %m+% months(forecast_n)
                       # Add the model name to the dataframe
                       tbats_df$Model <- "TBATS"
                       
                       # Create an empty matrix to store the recursive forecasts
                       pred_tbats <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       # Create an empty matrix to store the month being forecasted
                       tbats_months <- matrix(nrow = ncol(tsmyY), ncol = recent_months)
                       
                       # Recursive Forecast
                       for (i in 1:ncol(tsmyY)){
                         k <- 1 # Set the index to 1 for each recursive forecast done on each column
                         
                         # Subset each SKU to start at the first non-NA observation
                         for(z in 1:nrow(myY)){
                           if(is.na(myY[z,i])){
                             next
                           } else{
                             myY2 <- myY[z:nrow(myY),i]
                             break
                           }
                         }
                         
                         # Convert xts object to ts object
                         tsmyY2 <- xts.to.ts(myY2, freq = 12L)
                         
                         # Create start and end point of holdout period
                         holdout_start <- nrow(myY2) - (recent_months + (forecast_n - 1))
                         holdout_end <- nrow(myY2)-forecast_n
                         
                         timeProp <- tsp(tsmyY2)[1]
                         for (j in holdout_start:holdout_end){
                           
                           if(j < 2){
                             pred_tbats[i,k] <- NA
                             tbats_months[i,k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY2, end = timeProp + ((j-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(sum(is.na(train)) >= 1){
                               pred_tbats[i,k] <- NA
                               tbats_months[i,k] <- NA
                               k <- k+1
                             } else{
                               if(nmonths(train) < 3){
                                 pred_tbats[i,k] <- NA
                                 tbats_months[i,k] <- NA
                                 k <- k+1
                               } else{
                                 FC_tbats <- forecast(tbats(train),
                                                      h = forecast_n)
                                 pred_tbats[i,k] <- FC_tbats[[2]][[forecast_n]] # Store the predicted forecast in the vector prediction with index j
                                 tbats_months[i,k] <- tsp(train)[2] + ((forecast_n)/12) # Store the month being forecasted
                                 k <- k+1 # Creates another index to store the next prediction
                               }
                             }
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       tbats_month_df <- tbats_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       Holdout_Start <- matrix(ncol = 1, nrow = nrow(tbats_month_df))
                       for(i in 1:nrow(tbats_month_df)){
                         for(j in 1:ncol(tbats_month_df)){
                           if(all(is.na(tbats_month_df[i,]))){
                             Holdout_Start[i,1] <- NA
                           } else{
                             if(is.na(tbats_month_df[i,j])){
                               next
                             } else{
                               Holdout_Start[i,1] <- tbats_month_df[i,j]
                               break
                             }
                           }
                         }
                       }
                       # Convert matrix to dataframe 
                       Holdout_Start <- as.data.frame(Holdout_Start)
                       # Rename column to Holdout_Start
                       colnames(Holdout_Start) <- "Holdout_Start"
                       
                       # Transpose the recursive forecasts matrix and convert to a dataframe and convert negative forecasts to 0
                       pred_tbats_DF <- pred_tbats %>%
                         t() %>%
                         as.data.frame() %>%
                         apply(2, function(x) ifelse(x < 0, 0, x)) %>%
                         apply(2, function(x) round(x, 2)) %>%
                         apply(2, function(x) as.integer(x)) %>%
                         as.data.frame()
                       # Convert the dataframe to a ts object to use for measuring accuracy of the recursive forecast
                       timeProp2 <- tsp(tsmyY)[2]
                       pred_tbats_TS <- pred_tbats_DF %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Create an empty matrix to store accuracy measurement results
                       my_tbats_accuracy <- matrix(nrow = ncol(tsmyY), ncol = 7) #ncol = 7 since there are 7 measurements taken when using the accuracy() function
                       
                       # Use a forloop to take recrusive forecast and compare with actuals to obtain accuracy measurements
                       for (i in 1:ncol(tsmyY)){
                         my_tbats_accuracy[i,] <- pred_tbats_TS[, i] %>% accuracy(tsmyY[,i])
                       }
                       
                       # Subset the RMSE, MAE, and MAPE measurements from the matrix and convert to a dataframe
                       my_tbats_accuracy_DF <- as.data.frame(my_tbats_accuracy[, c(2,3,5)])
                       # Rename the columns to their appropiate measurement names
                       colnames(my_tbats_accuracy_DF) <- c("RMSE", "MAE", "MAPE")
                       
                       # Column bind the accuracy measurement dataframe and the forecasts from earlier to create a single dataframe
                       tbats_df_final <- cbind(tbats_df, my_tbats_accuracy_DF, Holdout_Start)
                       # Convert holdout_start to yearmon format
                       tbats_df_final$Holdout_Start <- as.yearmon(tbats_df_final$Holdout_Start)
                       # Remove the phrase Booking_AC from the Product name
                       tbats_df_final$Product <- gsub("_Booking_AC", " ", tbats_df_final$Product)
                       
                       tbats_df_final <- tbats_df_final %>%
                         select(Product, Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))),
                                RMSE = round(RMSE, 2),
                                MAE = round(MAE, 2),
                                MAPE = round(MAPE, 2),
                                Holdout_Start = as.character(Holdout_Start))
                       return(tbats_df_final)
                     } else {
                       return(NULL)
                     }
                   })
    })
    
    ###############################################################################################################################################################################
    
    output$final_batchdf <- DT::renderDataTable(extensions = "Scroller", 
                                                options = list(deferRender = TRUE,
                                                               scrollX = TRUE,
                                                               scrollY = 700,
                                                               scroller = TRUE),{
                                                                 arima <- arima()
                                                                 arfima <- arfima()
                                                                 croston <- croston()
                                                                 ets <- ets_df()
                                                                 hw_M <- hw_M_df()
                                                                 hw_MD <- hw_MD_df()
                                                                 hw_A <- hw_A_df()
                                                                 hw_AD <- hw_AD_df()
                                                                 lobf <- lobf()
                                                                 ma <- ma_df()
                                                                 ses <- ses()
                                                                 tbats <- tbats_df()
                                                                 final_batchdf <- as.data.frame(rbind(arima, arfima, croston, ets, hw_M, hw_MD, hw_A, hw_AD, lobf, ma, ses, tbats)) %>% 
                                                                   arrange(Product, Model)
                                                                 
                                                                 final_batchdf <- data.table(final_batchdf) #%>% 
                                                                 # formatStyle(
                                                                 #   'MAPE',
                                                                 #   fontWeight = 'bold', # Makes the font for the MAPE column bold
                                                                 #   backgroundColor = styleInterval(c(15,45), c('#8DDA77', '#F3EC58', '#E75943')) # Formats the cells based on the value, <15~green, >15&<45~yellow, >45 red
                                                                 # )
                                                                 return(final_batchdf)
                                                               })
    
    model_recommend <- reactive({
      arima <- arima()
      arfima <- arfima()
      croston <- croston()
      ets <- ets_df()
      hw_M <- hw_M_df()
      hw_MD <- hw_MD_df()
      hw_A <- hw_A_df()
      hw_AD <- hw_AD_df()
      lobf <- lobf()
      ma <- ma_df()
      ses <- ses()
      tbats <- tbats_df()
      recommend_df <- as.data.frame(rbind(arima, arfima, croston, ets, hw_M, hw_MD, hw_A, hw_AD, lobf, ma, ses, tbats)) %>%
        arrange(Product, Model)
      
      if(input$error_measurement == "RMSE"){
        recommend_df <- recommend_df %>%
          group_by(Product) %>%
          filter(RMSE == min(RMSE, na.rm = TRUE))
      } else if(input$error_measurement == "MAE"){
        recommend_df <- recommend_df %>%
          group_by(Product) %>%
          filter(MAE == min(MAE, na.rm = TRUE))
      } else {
        recommend_df <- recommend_df %>%
          group_by(Product) %>%
          filter(is.finite(MAPE) == TRUE,
                 MAPE == min(MAPE, na.rm = TRUE))
      }
      return(data.table(recommend_df))
    })
    
    output$model_recommend_df <- DT::renderDataTable(extensions = "Scroller",
                                                     options = list(deferRender = TRUE,
                                                                    scrollY = 700,
                                                                    scroller = TRUE),{
                                                                      model_recommend()
                                                                    })
    
    ###############################################################################################################################################################################
    ###############################################################################################################################################################################
    
    output$model_performance <- DT::renderDataTable({
      
      # Use existing reactive structures
      mySeries <- final_df()
      mySeries_filtered <- mySeries_filtered()
      
      isolate({
        mySeries_filtered <- mySeries_filtered %>%
          filter(Date >= input$dateRange[1] &
                   Date <= input$dateRange[2])
      })
      
      ################################################################################################# 
      
      if (nrow(mySeries_filtered) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      ################################################################################################# 
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries,
                              .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
        forecast_n <- input$i_forecast_n
        recent_months <- input$i_recent_months
      })
      
      #################################################################################################
      
      # Convert to TS object with monthly frequency
      myY <-  xts(select_(mySeries_filtered,
                          task_type),
                  order.by=ymd(mySeries_filtered$Date))
      
      # Set the start of the TS object until at the start of the first numeric observation
      for(i in 1:nrow(myY)){ # loops through each index to check for the first non-NA value
        if(is.na(myY[i])){
          next
        } else{
          myY <- myY[i:nrow(myY)] # Once the first numeric observation is found, it subsets the TS Object at this point on forward 
          break # Breaks the for loop once the first numeric observation is found
        }
      }
      
      # Replace any NAs that follow with 0s optionally
      isolate({
        if (input$checkbox2){
          myY <- myY %>% 
            tidyr::replace_na(0)
        }
      })
      
      #################################################################################################
      
      withProgress(message = 'Generating Forecasts...  ',
                   detail = 'this may take a few seconds',
                   value = 0.1,
                   min = 0,
                   max = 1, {
                     
                     # Convert xts object to ts object
                     y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
                     m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
                     y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
                     m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
                     
                     #################################################################################################
                     
                     tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
                     #tsmyY2 <- replace(tsmyY, tsmyY == 0, 1) # Replaces 0s in the ts with .01 to calculate MAPE later on
                     
                     # Create start and end point of holdout period
                     holdout_start <- nrow(myY) - (recent_months + (forecast_n - 1))
                     holdout_end <- nrow(myY)-forecast_n
                     
                     timeProp <- tsp(tsmyY)[1] # Takes the first observation and creates a numerical representation of the date
                     timeProp2 <- tsp(tsmyY)[2] # Takes the last observation and creates a numerical representation of the date
                     
                     #################################################################################################
                     
                     prediction_arima <- 0 # Set the vector 0 to store predicted values
                     prediction_arfima <- 0 # Set the vector 0 to store predicted values
                     prediction_croston <- 0 # Set the vector 0 to store predicted values
                     prediction_ets <- 0 # Set the vector 0 to store predicted values
                     prediction_hw_M <- 0 # Set the vector 0 to store predicted values
                     prediction_hw_M_D <- 0 # Set the vector 0 to store predicted values
                     prediction_hw_A <- 0 # Set the vector 0 to store predicted values
                     prediction_hw_A_D <- 0 # Set the vector 0 to store predicted values
                     prediction_lobf <- 0 # Set the vector 0 to store predicted values
                     prediction_ma <- 0 # Set the vector 0 to store predicted values
                     prediction_ses <- 0 # Set the vector 0 to store predicted values
                     prediction_tbats <- 0 # Set the vector 0 to store predicted values
                     
                     arima_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     arfima_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     croston_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     ets_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     hw_M_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     hw_MD_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     hw_A_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     hw_AD_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     lobf_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     ma_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     ses_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     tbats_months <- 0 # Set the vector 0 to store months forecasted in the recursive forecasts
                     
                     j <- 1 # Sets the first index to 1 to store predicted values
                     k <- 1 # Sets the first index to 1 to store predicted values
                     l <- 1 # Sets the first index to 1 to store predicted values
                     m <- 1 # Sets the first index to 1 to store predicted values
                     n <- 1 # Sets the first index to 1 to store predicted values
                     o <- 1 # Sets the first index to 1 to store predicted values
                     p <- 1 # Sets the first index to 1 to store predicted values
                     q <- 1 # Sets the first index to 1 to store predicted values
                     r <- 1 # Sets the first index to 1 to store predicted values
                     s <- 1 # Sets the first index to 1 to store predicted values
                     t <- 1 # Sets the first index to 1 to store predicted values
                     u <- 1 # Sets the first index to 1 to store predicted values
                     
                     #################################################################################################
                     
                     ### ARIMA MODEL ###
                     if(nrow(myY) < 3){
                       isolate({
                         forecast_Arima_df <- NULL
                         my_arima_Accuracy <- NULL
                       })
                     } else{
                       isolate({
                         # Forecast n periods using model with selected confidence intervals
                         TS_mySeries_ARIMA <- forecast(auto.arima(myY,
                                                                  stepwise = FALSE,
                                                                  approximation = TRUE),
                                                       h = forecast_n,
                                                       level = as.numeric(input$conf_int)
                         )
                       })
                       # Convert elements of time series FORECAST to dataframe
                       forecast_ARIMA_df <- with(TS_mySeries_ARIMA,
                                                 data.frame(Mean=TS_mySeries_ARIMA$mean[forecast_n],
                                                            Upper=TS_mySeries_ARIMA$upper[forecast_n, 1],
                                                            Lower=TS_mySeries_ARIMA$lower[forecast_n, 1],
                                                            Parameters=TS_mySeries_ARIMA$method))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_ARIMA_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_ARIMA_df$Model <- "ARIMA"
                       
                       # Adjust dataframe to replace negative forecasted values with 0
                       forecast_ARIMA_df <- forecast_ARIMA_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Recursive forecast for ARIMA model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_arima[j] <- NA
                           arima_months[j] <- NA
                           j <- j+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                           
                           if(nmonths(train) < 3){
                             prediction_arima[j] <- NA
                             arima_months[j] <- NA
                             j <- j+1
                           } else{
                             FC_arima <- forecast(auto.arima(train, 
                                                             stepwise = FALSE, 
                                                             approximation = TRUE) , 
                                                  h = forecast_n) # Creates the model and forecasts
                             prediction_arima[j] <- FC_arima[[4]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                             arima_months[j] <- tsp(train)[2] + ((forecast_n)/12)
                             j <- j+1 # Creates another index to store the next prediction
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       arima_month_df <- arima_months %>%
                         as.data.frame()
                       # Extract the first non-NA month from each SKU using forloop
                       arima_start <- 0
                       for(i in 1:nrow(arima_month_df)){
                         if(all(is.na(arima_month_df))){
                           arima_start <- NA 
                         } else{
                           if(is.na(arima_month_df[i,])){
                             next
                           } else{
                             arima_start <- arima_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       arima_start <- as.yearmon(arima_start)
                       
                       # Replace negative predictions with 0
                       prediction_arima <- replace(prediction_arima, prediction_arima < 0, 0) %>% 
                         round(2) %>%
                         as.integer()
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       arima_FC <- prediction_arima %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_arima_Accuracy <- arima_FC %>%
                         accuracy(tsmyY) 
                       
                       my_arima_Accuracy <- my_arima_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                         as.data.frame() %>% # Converts accuracy measurements to a dataframe
                         t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                       rownames(my_arima_Accuracy) <- NULL # Removes the rownames
                       
                       # Add Holdout_Start to the df
                       my_arima_Accuracy$Holdout_Start <- arima_start # Coercess to list instead of dataframe
                       
                       # Convert back to dataframe
                       my_arima_Accuracy <- as.data.frame(my_arima_Accuracy)
                       
                       # Rename columns
                       colnames(my_arima_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     
                     ############################################################################################################################
                     
                     ### ARFIMA Model ###
                     if(sum(is.na(myY)) >= 1){
                       isolate({
                         forecast_ARFIMA_df <- NULL
                         my_arfima_Accuracy <- NULL
                       })
                     } else{
                       if(all(tsmyY == 0, na.rm = TRUE) == TRUE){
                         isolate({
                           forecast_ARFIMA_df <- NULL
                           my_arfima_Accuracy <- NULL
                         })
                       } else{
                         isolate({
                           # Forecast n periods using ARFIMA model
                           TS_mySeries_ARFIMA <- forecast(forecast::arfima(tsmyY,
                                                                           drange = c(0,.5),
                                                                           estim = c("mle", "ls"),
                                                                           lambda = "auto"),
                                                          h = forecast_n,
                                                          level = as.numeric(input$conf_int)
                           )
                         })
                         
                         # Convert elements of time series FORECAST to dataframe 
                         forecast_ARFIMA_df <- with(TS_mySeries_ARFIMA,
                                                    data.frame(Mean=TS_mySeries_ARFIMA$mean[forecast_n],
                                                               Upper=TS_mySeries_ARFIMA$upper[forecast_n, 1],
                                                               Lower=TS_mySeries_ARFIMA$lower[forecast_n, 1],
                                                               Parameters=TS_mySeries_ARFIMA$method))
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_ARFIMA_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                         
                         # Add column for model name
                         forecast_ARFIMA_df$Model <- "ARFIMA"
                         
                         # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                         forecast_ARFIMA_df <- forecast_ARFIMA_df %>%
                           select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Lower, 2)))
                           )
                         
                         # Recursive forecast for ARFIMA model
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction_arfima[k] <- NA
                             arfima_months[k] <- NA
                             k <- k+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12)) # Creates the first training dataset to use for forecasting
                             
                             if(nmonths(train) < 5){
                               prediction_arfima[k] <- NA
                               arfima_months[k] <- NA
                               k <- k+1
                             } else{
                               if(all(train == 0, na.rm = TRUE) == TRUE){
                                 prediction_arfima[k] <- NA
                                 arfima_months[k] <- NA
                                 k <- k+1
                               } else{
                                 FC_arfima <- forecast(forecast::arfima(train,
                                                                        drange = c(0,.5),
                                                                        estim = c("mle", "ls"),
                                                                        lambda = "auto"),
                                                       h = forecast_n) # Creates the model and forecasts
                                 prediction_arfima[k] <- FC_arfima[[2]][[forecast_n]] #Store the predicted forecast in the vector prediction with index j
                                 arfima_months[k] <- tsp(train)[2] + ((forecast_n)/12)
                                 k <- k+1 # Creates another index to store the next prediction
                               }
                             }
                           }
                         }
                         
                         # Convert the month matrix to a dataframe
                         arfima_month_df <- arfima_months %>%
                           as.data.frame()
                         # Extract first non-NA months and store it in vector
                         for(i in 1:nrow(arfima_month_df)){
                           if(all(is.na(arfima_month_df))){
                             arfima_start <- NA 
                           } else{
                             if(is.na(arfima_month_df[i,])){
                               next
                             } else{
                               arfima_start <- arfima_month_df[i,]
                               break
                             }
                           }
                         }
                         # Convert to yearmon format
                         arfima_start <- as.yearmon(arfima_start)
                         
                         # Replace negative predictions with 0
                         prediction_arfima <- replace(prediction_arfima, prediction_arfima < 0, 0) %>% 
                           round(2) %>%
                           as.integer() 
                         
                         # Converts the predictions into a TS object for plotting and measuring accuracy
                         arfima_FC <- prediction_arfima %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                         my_arfima_Accuracy <- arfima_FC %>%
                           accuracy(tsmyY)
                         
                         my_arfima_Accuracy <- my_arfima_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                           as.data.frame() %>% # Converts accuracy measurements to a dataframe
                           t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                         rownames(my_arfima_Accuracy) <- NULL # Removes the rownames
                         
                         # Add Holdout_Start to the df
                         my_arfima_Accuracy$Holdout_Start <- arfima_start
                         
                         # Convert back to dataframe
                         my_arfima_Accuracy <- as.data.frame(my_arfima_Accuracy)
                         
                         # Rename columns
                         colnames(my_arfima_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                       }
                     }
                     
                     ############################################################################################################################
                     
                     ### CROSTON Model ###
                     if(nrow(myY) < 3){
                       forecast_croston_df <- NULL
                       my_croston_Accuracy <- NULL
                     } else{
                       if(sum(is.na(myY)) >= 1){
                         isolate({
                           forecast_croston_df <- NULL
                           my_croston_Accuracy <- NULL
                         })
                       } else{
                         isolate({
                           TS_mySeries_croston <- forecast::croston(tsmyY,
                                                                    h=forecast_n)
                         })
                         
                         # Convert elements of time series FORECAST to dataframe
                         forecast_croston_df <- with(TS_mySeries_croston,
                                                     data.frame(Mean=TS_mySeries_croston$mean[forecast_n],
                                                                Upper=NA,
                                                                Lower=NA))
                         if(sum(myY != 0) < 2){
                           forecast_croston_df$Parameters <- with(forecast_croston_df, 
                                                                  paste0("ETS(", NA, ",", NA, ",", NA, ",", NA, ")")) # Combine with DF under the Parameter column
                         } else{
                           para_croston <- TS_mySeries_croston$model$demand$model$components # Extract the parameters of the model 
                           forecast_croston_df$Parameters <- with(forecast_croston_df, 
                                                                  paste0("ETS(", para_croston[1], ",", para_croston[2], ",", para_croston[3], ",", para_croston[4], ")")) # Combine with DF under the Parameter column
                         }
                         
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_croston_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                         
                         # Add column for model name
                         forecast_croston_df$Model <- "CROSTON"
                         
                         # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                         forecast_croston_df <- forecast_croston_df %>%
                           select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2)))
                           )
                         
                         # Recursive forecast for Croston model
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction_croston[l] <- NA
                             croston_months[l] <- NA
                             l <- l+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 3){
                               prediction_croston[l] <- NA
                               croston_months[l] <- NA
                               l <- l+1
                             } else{
                               FC_croston <- forecast::croston(train,
                                                               h=forecast_n)
                               prediction_croston[l] <- FC_croston[[1]][[forecast_n]]
                               croston_months[l] <- tsp(train)[2] + ((forecast_n)/12)
                               l <- l+1
                             }
                           }
                         }
                         
                         # Convert the month matrix to a dataframe
                         croston_month_df <- croston_months %>%
                           as.data.frame()
                         # Extract first non-NA month
                         for(i in 1:nrow(croston_month_df)){
                           if(all(is.na(croston_month_df))){
                             croston_start <- NA 
                           } else{
                             if(is.na(croston_month_df[i,])){
                               next
                             } else{
                               croston_start <- croston_month_df[i,]
                               break
                             }
                           }
                         }
                         # Convert to yearmon format
                         croston_start <- as.yearmon(croston_start)
                         
                         # Replace negative predictions with 0
                         prediction_croston <- replace(prediction_croston, prediction_croston < 0, 0) %>% 
                           round(2) %>%
                           as.integer() 
                         
                         # Converts the predictions into a TS object for plotting and measuring accuracy
                         croston_FC <- prediction_croston %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                         my_croston_Accuracy <- croston_FC %>%
                           accuracy(tsmyY)
                         
                         my_croston_Accuracy <- my_croston_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                           as.data.frame() %>% # Converts accuracy measurements to a dataframe
                           t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                         rownames(my_croston_Accuracy) <- NULL # Removes the rownames
                         
                         # Add Holdout_Start to the df
                         my_croston_Accuracy$Holdout_Start <- croston_start
                         
                         # Convert back to dataframe
                         my_croston_Accuracy <- as.data.frame(my_croston_Accuracy)
                         
                         # Rename columns
                         colnames(my_croston_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                       }
                     }
                     
                     
                     ############################################################################################################################
                     
                     ### ETS Model ### 
                     if(nrow(myY) < 3){
                       forecast_ets_df <- NULL
                       my_ets_Accuracy <- NULL
                     } else{
                       isolate({
                         TS_mySeries_ets <- forecast(ets(myY,
                                                         allow.multiplicative.trend = TRUE,
                                                         opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                     h = forecast_n,
                                                     level = as.numeric(input$conf_int)
                         )
                       })
                       
                       # Convert elements of time series FORECAST to dataframe 
                       forecast_ets_df <- with(TS_mySeries_ets,
                                               data.frame(Mean=TS_mySeries_ets$mean[forecast_n],
                                                          Upper=TS_mySeries_ets$upper[forecast_n, 1],
                                                          Lower=TS_mySeries_ets$lower[forecast_n, 1]))
                       para_ets <- TS_mySeries_ets$model$components
                       forecast_ets_df$Parameters <- with(forecast_ets_df, paste0("ETS(", para_ets[1], ",", para_ets[2], ",", para_ets[3], ",", para_ets[4], ")"))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_ets_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_ets_df$Model <- "ETS"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_ets_df <- forecast_ets_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2)))
                         )
                       
                       # Recursive forecast for ETS model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_ets[m] <- NA
                           ets_months[m] <- NA
                           m <- m+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction_ets[m] <- NA
                             ets_months[m] <- NA
                             m <- m+1
                           } else{
                             FC_ets <- forecast(ets(train,
                                                    allow.multiplicative.trend = TRUE,
                                                    opt.crit = c("lik", "amse", "mse", "sigma", "mae")),
                                                h = forecast_n)
                             prediction_ets[m] <- FC_ets[[2]][[forecast_n]]
                             ets_months[m] <- tsp(train)[2] + ((forecast_n)/12)
                             m <- m+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ets_month_df <- ets_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(ets_month_df)){
                         if(all(is.na(ets_month_df))){
                           ets_start <- NA 
                         } else{
                           if(is.na(ets_month_df[i,])){
                             next
                           } else{
                             ets_start <- ets_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       ets_start <- as.yearmon(ets_start)
                       
                       # Replace negative predictions with 0
                       prediction_ets <- replace(prediction_ets, prediction_ets < 0, 0) %>% 
                         round(2) %>%
                         as.integer() 
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       ets_FC <- prediction_ets %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_ets_Accuracy <- ets_FC %>%
                         accuracy(tsmyY)
                       
                       my_ets_Accuracy <- my_ets_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                         as.data.frame() %>% # Converts accuracy measurements to a dataframe
                         t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                       rownames(my_ets_Accuracy) <- NULL # Removes the rownames
                       
                       # Add Holdout_Start to the df
                       my_ets_Accuracy$Holdout_Start <- ets_start
                       
                       # Convert back to dataframe
                       my_ets_Accuracy <- as.data.frame(my_ets_Accuracy)
                       
                       # Rename columns
                       colnames(my_ets_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     
                     ############################################################################################################################
                     
                     ### HW Multiplicative Model ###
                     if(nrow(myY) < 15){
                       isolate({
                         forecast_HW_M_df <- NULL
                         my_hw_M_Accuracy <- NULL
                       })
                     } else{
                       if(sum(tsmyY == 0, na.rm = TRUE) >= 1){
                         isolate({
                           forecast_HW_M_df <- NULL
                           my_hw_M_Accuracy <- NULL
                         })
                       } else{
                         isolate({
                           # Forecast Holt-Winters Multiplicative Model
                           TS_mySeries_HW_M <- forecast::hw(tsmyY, 
                                                            h = forecast_n,
                                                            seasonal = "multiplicative",
                                                            damped = FALSE,
                                                            level = as.numeric(input$conf_int)
                           )
                         })
                         
                         # Convert elements of time series FORECAST to dataframe
                         forecast_HW_M_df <- with(TS_mySeries_HW_M,
                                                  data.frame(Mean=TS_mySeries_HW_M$mean[forecast_n],
                                                             Upper=TS_mySeries_HW_M$upper[forecast_n, 1],
                                                             Lower=TS_mySeries_HW_M$lower[forecast_n, 1]))
                         para_HW_M <- TS_mySeries_HW_M$model$components
                         forecast_HW_M_df$Parameters <- with(forecast_HW_M_df, 
                                                             paste0("ETS(", para_HW_M[1], ",", para_HW_M[2], ",", para_HW_M[3], ",", para_HW_M[4], ")"))
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_HW_M_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                         
                         # Add column for model name
                         forecast_HW_M_df$Model <- "HOLT_WINTERS_MULTIPLICATIVE"
                         
                         # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                         forecast_HW_M_df <- forecast_HW_M_df %>%
                           select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Mean, 2)))
                           )
                         
                         # Recursive forecast Holt-Winters Multiplicative model
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction_hw_M[n] <- NA
                             hw_M_months[n] <- NA
                             n <- n+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 15){
                               prediction_hw_M[n] <- NA
                               hw_M_months[n] <- NA
                               n <- n+1
                             } else{
                               FC_HW_M <- forecast::hw(train, 
                                                       h = forecast_n,
                                                       seasonal = "multiplicative",
                                                       damped = FALSE)
                               prediction_hw_M[n] <- FC_HW_M[[2]][[forecast_n]]
                               hw_M_months[n] <- tsp(train)[2] + ((forecast_n)/12)
                               n <- n+1
                             }
                           }
                         }
                         
                         # Convert the month matrix to a dataframe
                         hw_M_month_df <- hw_M_months %>%
                           as.data.frame()
                         # Extract first non-NA month
                         for(i in 1:nrow(hw_M_month_df)){
                           if(all(is.na(hw_M_month_df))){
                             hw_M_start <- NA 
                           } else{
                             if(is.na(hw_M_month_df[i,])){
                               next
                             } else{
                               hw_M_start <- hw_M_month_df[i,]
                               break
                             }
                           }
                         }
                         # Convert to yearmon format
                         hw_M_start <- as.yearmon(hw_M_start)
                         
                         # Replace negative predictions with 0
                         prediction_hw_M <- replace(prediction_hw_M, prediction_hw_M < 0, 0) %>% 
                           round(2) %>%
                           as.integer()
                         
                         # Converts the predictions into a TS object for plotting and measuring accuracy
                         HW_FC_M <- prediction_hw_M %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                         my_hw_M_Accuracy <- HW_FC_M %>%
                           accuracy(tsmyY)
                         
                         my_hw_M_Accuracy <- my_hw_M_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                           as.data.frame() %>% # Converts accuracy measurements to a dataframe
                           t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                         rownames(my_hw_M_Accuracy) <- NULL # Removes the rownames
                         
                         # Add Holdout_Start to the df
                         my_hw_M_Accuracy$Holdout_Start <- hw_M_start
                         
                         # Convert back to dataframe
                         my_hw_M_Accuracy <- as.data.frame(my_hw_M_Accuracy)
                         
                         # Rename columns
                         colnames(my_hw_M_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                       }
                     }
                     
                     ############################################################################################################################
                     
                     ### HW Multiplicative Damped Model ###
                     if(nrow(myY) < 15){
                       isolate({
                         forecast_HW_MD_df <- NULL
                         my_hw_MD_Accuracy <- NULL
                       })
                     } else{
                       if(sum(tsmyY == 0, na.rm = TRUE) >= 1){
                         isolate({
                           forecast_HW_MD_df <- NULL
                           my_hw_MD_Accuracy <- NULL
                         })
                       } else{
                         # Forecast Holt-Winters Multiplicative Model with Damped Trend
                         TS_mySeries_HW_MD <- forecast::hw(tsmyY, 
                                                           h = forecast_n,
                                                           seasonal = "multiplicative",
                                                           damped = TRUE,
                                                           level = as.numeric(input$conf_int)
                         )
                         
                         # Convert elements of time series FORECAST to dataframe
                         forecast_HW_MD_df <- with(TS_mySeries_HW_MD,
                                                   data.frame(Mean=TS_mySeries_HW_MD$mean[forecast_n],
                                                              Upper=TS_mySeries_HW_MD$upper[forecast_n, 1],
                                                              Lower=TS_mySeries_HW_MD$lower[forecast_n, 1]))
                         para_HW_MD <- TS_mySeries_HW_MD$model$components
                         forecast_HW_MD_df$Parameters <- with(forecast_HW_MD_df, 
                                                              paste0("ETS(", para_HW_MD[1], ",", para_HW_MD[2], ",", para_HW_MD[3], ",", para_HW_MD[4], ")"))
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_HW_MD_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                         
                         # Add column for model name
                         forecast_HW_MD_df$Model <- "HOLT_WINTERS_MULTIPLICATIVE_DAMPED_TREND"
                         
                         # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                         forecast_HW_MD_df <- forecast_HW_MD_df %>%
                           select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Mean, 2))))
                         
                         # Recursive forecast Holt-Winters Multiplicative Damped Trend model
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction_hw_M_D[o] <- NA
                             hw_MD_months[o] <- NA
                             o <- o+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 15){
                               prediction_hw_M_D[o] <- NA
                               hw_MD_months[o] <- NA
                               o <- o+1
                             } else{
                               FC_HW_MD <- forecast::hw(train, 
                                                        h = forecast_n,
                                                        seasonal = "multiplicative",
                                                        damped = TRUE)
                               prediction_hw_M_D[o] <- FC_HW_MD[[2]][[forecast_n]]
                               hw_MD_months[o] <- tsp(train)[2] + ((forecast_n)/12)
                               o <- o+1
                             }
                           }
                         }
                         
                         # Convert the month matrix to a dataframe
                         hw_MD_month_df <- hw_MD_months %>%
                           as.data.frame()
                         # Extract first non-NA month
                         for(i in 1:nrow(hw_MD_month_df)){
                           if(all(is.na(hw_MD_month_df))){
                             hw_MD_start <- NA 
                           } else{
                             if(is.na(hw_MD_month_df[i,])){
                               next
                             } else{
                               hw_MD_start <- hw_MD_month_df[i,]
                               break
                             }
                           }
                         }
                         # Convert to yearmon format
                         hw_MD_start <- as.yearmon(hw_MD_start)
                         
                         # Replace negative predictions with 0
                         prediction_hw_M_D <- replace(prediction_hw_M_D, prediction_hw_M_D < 0, 0) %>% 
                           round(2) %>%
                           as.integer()
                         
                         # Converts the predictions into a TS object for plotting and measuring accuracy
                         HW_FC_MD <- prediction_hw_M_D %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                         my_hw_MD_Accuracy <- HW_FC_MD %>%
                           accuracy(tsmyY)
                         
                         
                         my_hw_MD_Accuracy <- my_hw_MD_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                           as.data.frame() %>% # Converts accuracy measurements to a dataframe
                           t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                         rownames(my_hw_MD_Accuracy) <- NULL # Removes the rownames
                         
                         # Add Holdout_Start to the df
                         my_hw_MD_Accuracy$Holdout_Start <- hw_MD_start
                         
                         # Convert back to dataframe
                         my_hw_MD_Accuracy <- as.data.frame(my_hw_MD_Accuracy)
                         
                         # Rename columns
                         colnames(my_hw_MD_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                       }
                     }
                     
                     ############################################################################################################################
                     
                     ### HW Additive Model ###
                     if(nrow(myY) < 15){
                       isolate({
                         forecast_HW_A_df <- NULL
                         my_hw_A_Accuracy <- NULL
                       })
                     } else{
                       isolate({
                         # Forecast n periods Holt-Winters Additive Model
                         TS_mySeries_HW_A <- forecast::hw(tsmyY, 
                                                          h = forecast_n,
                                                          seasonal = "additive",
                                                          damped = FALSE,
                                                          level = as.numeric(input$conf_int))
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_HW_A_df <- with(TS_mySeries_HW_A,
                                                data.frame(Mean=TS_mySeries_HW_A$mean[forecast_n],
                                                           Upper=TS_mySeries_HW_A$upper[forecast_n, 1],
                                                           Lower=TS_mySeries_HW_A$lower[forecast_n, 1]))
                       para_HW_A <- TS_mySeries_HW_A$model$components
                       forecast_HW_A_df$Parameters <- with(forecast_HW_A_df, 
                                                           paste0("ETS(", para_HW_A[1], ",", para_HW_A[2], ",", para_HW_A[3], ",", para_HW_A[4], ")"))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_HW_A_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_HW_A_df$Model <- "HOLT_WINTERS_ADDITIVE"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_HW_A_df <- forecast_HW_A_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Mean, 2)))
                         )
                       
                       # Recursive forecast Holt-Winters Additive model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_hw_A[p] <- NA
                           hw_A_months[p] <- NA
                           p <- p+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 15){
                             prediction_hw_A[p] <- NA
                             hw_A_months[p] <- NA
                             p <- p+1
                           } else{
                             FC_HW_A <- forecast::hw(train, 
                                                     h = forecast_n,
                                                     seasonal = "additive",
                                                     damped = FALSE)
                             prediction_hw_A[p] <- FC_HW_A[[2]][[forecast_n]]
                             hw_A_months[p] <- tsp(train)[2] + ((forecast_n)/12)
                             p <- p+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_A_month_df <- hw_A_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(hw_A_month_df)){
                         if(all(is.na(hw_A_month_df))){
                           hw_A_start <- NA 
                         } else{
                           if(is.na(hw_A_month_df[i,])){
                             next
                           } else{
                             hw_A_start <- hw_A_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       hw_A_start <- as.yearmon(hw_A_start)
                       
                       # Replace negative predictions with 0
                       prediction_hw_A <- replace(prediction_hw_A, prediction_hw_A < 0, 0) %>% 
                         round(2) %>%
                         as.integer() 
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       HW_FC_A <- prediction_hw_A %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_hw_A_Accuracy <- HW_FC_A %>%
                         accuracy(tsmyY)
                       
                       my_hw_A_Accuracy <- my_hw_A_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                         as.data.frame() %>% # Converts accuracy measurements to a dataframe
                         t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                       rownames(my_hw_A_Accuracy) <- NULL # Removes the rownames
                       
                       # Add Holdout_Start to the df
                       my_hw_A_Accuracy$Holdout_Start <- hw_A_start
                       
                       # Convert back to dataframe
                       my_hw_A_Accuracy <- as.data.frame(my_hw_A_Accuracy)
                       
                       # Rename columns
                       colnames(my_hw_A_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     ############################################################################################################################
                     
                     ### HW Additive Damped Model ###
                     if(nrow(myY) < 15){
                       isolate({
                         forecast_HW_AD_df <- NULL
                         my_hw_AD_Accuracy <- NULL
                       })
                     } else{
                       isolate({
                         # Forecast n periods Holt-Winters Additive Model with Damped Trend
                         TS_mySeries_HW_AD <- forecast::hw(tsmyY, 
                                                           h = forecast_n,
                                                           seasonal = "additive",
                                                           damped = TRUE,
                                                           level = as.numeric(input$conf_int)
                         )
                       })
                       
                       # Convert elements of time series FORECAST to dataframe
                       forecast_HW_AD_df <- with(TS_mySeries_HW_AD,
                                                 data.frame(Mean=TS_mySeries_HW_AD$mean[forecast_n],
                                                            Upper=TS_mySeries_HW_AD$upper[forecast_n, 1],
                                                            Lower=TS_mySeries_HW_AD$lower[forecast_n, 1]))
                       para_HW_AD <- TS_mySeries_HW_AD$model$components
                       forecast_HW_AD_df$Parameters <- with(forecast_HW_AD_df, 
                                                            paste0("ETS(", para_HW_AD[1], ",", para_HW_AD[2], ",", para_HW_AD[3], ",", para_HW_AD[4], ")"))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_HW_AD_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_HW_AD_df$Model <- "HOLT_WINTERS_ADDITIVE_DAMPED_TREND"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_HW_AD_df <- forecast_HW_AD_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Mean, 2)))
                         )
                       
                       # Recursive forecast Holt-Winters Additive Damped Trend model 
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_hw_A_D[q] <- NA
                           hw_AD_months[q] <- NA
                           q <- q+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 15){
                             prediction_hw_A_D[q] <- NA
                             hw_AD_months[q] <- NA
                             q <- q+1
                           } else{
                             FC_HW_AD <- forecast::hw(train, 
                                                      h = forecast_n,
                                                      seasonal = "additive",
                                                      damped = TRUE)
                             prediction_hw_A_D[q] <- FC_HW_AD[[2]][[forecast_n]]
                             hw_AD_months[q] <- tsp(train)[2] + ((forecast_n)/12)
                             q <- q+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       hw_AD_month_df <- hw_AD_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(hw_AD_month_df)){
                         if(all(is.na(hw_AD_month_df))){
                           hw_AD_start <- NA 
                         } else{
                           if(is.na(hw_AD_month_df[i,])){
                             next
                           } else{
                             hw_AD_start <- hw_AD_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       hw_AD_start <- as.yearmon(hw_AD_start)
                       
                       # Replace negative predictions with 0
                       prediction_hw_A_D <- replace(prediction_hw_A_D, prediction_hw_A_D < 0, 0) %>% 
                         round(2) %>%
                         as.integer()
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       HW_FC_AD <- prediction_hw_A_D %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_hw_AD_Accuracy <- HW_FC_AD %>%
                         accuracy(tsmyY)
                       
                       my_hw_AD_Accuracy <- my_hw_AD_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                         as.data.frame() %>% # Converts accuracy measurements to a dataframe
                         t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                       rownames(my_hw_AD_Accuracy) <- NULL # Removes the rownames
                       
                       # Add Holdout_Start to the df
                       my_hw_AD_Accuracy$Holdout_Start <- hw_AD_start
                       
                       # Convert back to dataframe
                       my_hw_AD_Accuracy <- as.data.frame(my_hw_AD_Accuracy)
                       
                       # Rename columns
                       colnames(my_hw_AD_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     ############################################################################################################################
                     
                     ### LOBF Model ###
                     if(nrow(myY) < 3){
                       forecast_LOBF_df <- NULL
                       my_lobf_Accuracy <- NULL
                     } else{
                       isolate({
                         # Convert to dataframe
                         mySeries_filtered2 <- data.frame(Date = as.Date(index(myY)), value = coredata(myY)[,])
                         
                         TS_mySeries_LOBF <- lm(value ~ Date,
                                                data = mySeries_filtered2,
                                                na.action = na.exclude)
                         
                         # Convert elements of time series MODEL to dataframe for plotting
                         fit_LOBF_df <- as.data.frame(cbind(as.vector(TS_mySeries_LOBF$fitted.values),
                                                            mySeries_filtered2$Date))
                         
                         colnames(fit_LOBF_df) <- c('fitted','Date')
                         
                         # Convert to TS object with monthly frequency
                         myTS <-  xts(fit_LOBF_df$fitted,
                                      order.by = as.Date(fit_LOBF_df$Date)
                         )
                         
                         # Forecast n periods using LOBF model
                         TS_mySeries_LOBF <- forecast(myTS,
                                                      h=forecast_n,
                                                      level = as.numeric(input$conf_int)
                         )
                       })
                       
                       # Convert elements of time series FORECAST to dataframe
                       forecast_LOBF_df <- with(TS_mySeries_LOBF,
                                                data.frame(Mean=TS_mySeries_LOBF$mean[forecast_n],
                                                           Upper=TS_mySeries_LOBF$upper[forecast_n, 1],
                                                           Lower=TS_mySeries_LOBF$lower[forecast_n, 1],
                                                           Parameters=NA))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_LOBF_df$Date <- as.Date(max(mySeries_filtered2$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_LOBF_df$Model <- "Line_of_Best_Fit"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_LOBF_df <- forecast_LOBF_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2)))
                         )
                       
                       # Recursive forecast for LOBF model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_lobf[r] <- NA
                           lobf_months[r] <- NA
                           r <- r+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           # Convert ts to xts object as indexing produced wrong dates when using the ts object
                           train2 <- as.xts(train)
                           
                           if(nmonths(train2) < 3){
                             prediction_lobf[r] <- NA
                             lobf_months[r] <- NA
                             r <- r+1
                           } else{
                             # Convert train ts object to a dataframe to create a linear model
                             df <- data.frame(time = as.Date(index(train2)), value = coredata(train2)[,])
                             
                             # Create a linear model
                             lobf_series <- lm(formula = value ~ time, 
                                               data = df,
                                               na.action = na.exclude)
                             
                             # Create a dataframe of the fitted values from the linear model and cbind it to the corresponding date from the train dataframe
                             fit_lobf <- as.data.frame(cbind(as.vector(lobf_series$fitted.values),
                                                             df$time))
                             # Convert to a xts object
                             lobf_ts <- xts(fit_lobf$V1,
                                            order.by = as.Date(fit_lobf$V2))
                             
                             # Create forecasts
                             FC_lobf <- forecast(lobf_ts,
                                                 h = forecast_n)
                             
                             # Store forecasts into prediction vector
                             prediction_lobf[r] <- FC_lobf$mean[[forecast_n]]
                             lobf_months[r] <- tsp(train)[2] + ((forecast_n)/12)
                             r <- r+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       lobf_month_df <- lobf_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(lobf_month_df)){
                         if(all(is.na(lobf_month_df))){
                           lobf_start <- NA 
                         } else{
                           if(is.na(lobf_month_df[i,])){
                             next
                           } else{
                             lobf_start <- lobf_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       lobf_start <- as.yearmon(lobf_start)
                       
                       # Replace negative predictions with 0
                       prediction_lobf <- replace(prediction_lobf, prediction_lobf < 0, 0) %>% 
                         round(2) %>%
                         as.integer()
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       lobf_FC <- prediction_lobf %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_lobf_Accuracy <- lobf_FC %>%
                         accuracy(tsmyY)
                       
                       my_lobf_Accuracy <- my_lobf_Accuracy[,c(2,3,5)] %>% # Subsets RMSE, MAE, and MAPE accuracy measurements
                         as.data.frame() %>% # Converts accuracy measurements to a dataframe
                         t() # Transposes the dataset to have RMSE, MAE, and MAPE as the column names
                       rownames(my_lobf_Accuracy) <- NULL # Removes the rownames
                       
                       # Add Holdout_Start to the df
                       my_lobf_Accuracy$Holdout_Start <- lobf_start
                       
                       # Convert back to dataframe
                       my_lobf_Accuracy <- as.data.frame(my_lobf_Accuracy)
                       
                       # Rename columns
                       colnames(my_lobf_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     ############################################################################################################################
                     
                     ### Moving Average Model ###
                     if(nrow(myY) < 3){
                       forecast_MA_df <- NULL
                       my_ma_Accuracy <- NULL
                     } else{
                       isolate({
                         TS_mySeries_MA <- forecast(auto.arima(myY, 
                                                               max.p=0, 
                                                               stationary=TRUE, 
                                                               seasonal=FALSE),
                                                    h = forecast_n,
                                                    level = as.numeric(input$conf_int)
                         )
                       })
                       
                       # Convert elements of time series FORECAST to dataframe for plotting
                       forecast_MA_df <- with(TS_mySeries_MA,
                                              data.frame(Mean=TS_mySeries_MA$mean[forecast_n],
                                                         Upper=TS_mySeries_MA$upper[forecast_n, 1],
                                                         Lower=TS_mySeries_MA$lower[forecast_n, 1],
                                                         Parameters=TS_mySeries_MA$method))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_MA_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_MA_df$Model <- "MOVING_AVERAGE"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_MA_df <- forecast_MA_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Recursive forecast for MA model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_ma[s] <- NA
                           ma_months[s] <- NA
                           s <- s+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction_ma[s] <- NA
                             ma_months[s] <- NA
                             s <- s+1
                           } else{
                             FC_ma <- forecast(auto.arima(train,
                                                          max.p=0,
                                                          stationary=TRUE,
                                                          seasonal=FALSE),
                                               h = forecast_n)
                             prediction_ma[s] <- FC_ma[[4]][[forecast_n]]
                             ma_months[s] <- tsp(train)[2] + ((forecast_n)/12)
                             s <- s+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ma_month_df <- ma_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(ma_month_df)){
                         if(all(is.na(ma_month_df))){
                           ma_start <- NA 
                         } else{
                           if(is.na(ma_month_df[i,])){
                             next
                           } else{
                             ma_start <- ma_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       ma_start <- as.yearmon(ma_start)
                       
                       # Replace negative predictions with 0
                       prediction_ma <- replace(prediction_ma, prediction_ma < 0, 0) %>% 
                         round(2) %>%
                         as.integer()
                       
                       # Converts the predictions into a TS object for plotting and measuring accuracy
                       ma_FC <- prediction_ma %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_ma_Accuracy <- ma_FC %>%
                         accuracy(tsmyY)
                       
                       my_ma_Accuracy <- my_ma_Accuracy[,c(2,3,5)] %>%
                         as.data.frame() %>% 
                         t()
                       rownames(my_ma_Accuracy) <- NULL
                       
                       # Add Holdout_Start to the df
                       my_ma_Accuracy$Holdout_Start <- ma_start
                       
                       # Convert back to dataframe
                       my_ma_Accuracy <- as.data.frame(my_ma_Accuracy)
                       
                       # Rename columns
                       colnames(my_ma_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     
                     ############################################################################################################################
                     
                     ### SES ###
                     if(nrow(myY) < 3){
                       forecast_SES_df <- NULL
                       my_ses_Accuracy <- NULL
                     } else{
                       isolate({
                         # Forecast SES model n periods 
                         TS_mySeries_SES <- forecast::ses(myY,
                                                          h=forecast_n,
                                                          initial=c('optimal', 'simple'),
                                                          level = as.numeric(input$conf_int)
                         )
                       })
                       
                       # Convert elements of time series FORECAST to dataframe
                       forecast_SES_df <- with(TS_mySeries_SES,
                                               data.frame(Mean=TS_mySeries_SES$mean[forecast_n],
                                                          Upper=TS_mySeries_SES$upper[forecast_n, 1],
                                                          Lower=TS_mySeries_SES$lower[forecast_n, 1]))
                       para_SES <- TS_mySeries_SES$model$components
                       forecast_SES_df$Parameters <- with(forecast_SES_df, paste0("ETS(", para_SES[1], ",", para_SES[2], ",", para_SES[3], ",", para_SES[4], ")"))
                       
                       # Add Date column to the forecasted values data.frame
                       forecast_SES_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                       
                       # Add column for model name
                       forecast_SES_df$Model <- "SES"
                       
                       # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                       forecast_SES_df <- forecast_SES_df %>%
                         select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                         mutate(Mean = ifelse(as.integer(Mean) < 0,
                                              0,
                                              as.integer(round(Mean, 2))),
                                Upper = ifelse(as.integer(Upper) < 0,
                                               0,
                                               as.integer(round(Upper, 2))),
                                Lower = ifelse(as.integer(Lower) < 0,
                                               0,
                                               as.integer(round(Lower, 2))))
                       
                       # Recursive forecast for SES model
                       for (i in holdout_start:holdout_end){
                         
                         if(i < 2){
                           prediction_ses[t] <- NA
                           ses_months[t] <- NA
                           t <- t+1
                         } else{
                           train <- window(tsmyY, end = timeProp + ((i-1)/12))
                           
                           if(nmonths(train) < 3){
                             prediction_ses[t] <- NA
                             ses_months[t] <- NA
                             t <- t+1
                           } else{
                             FC_ses <- forecast::ses(train,
                                                     h=forecast_n,
                                                     initial=c('optimal', 'simple')
                             )
                             prediction_ses[t] <- FC_ses[[2]][[forecast_n]]
                             ses_months[t] <- tsp(train)[2] + ((forecast_n)/12)
                             t <- t+1
                           }
                         }
                       }
                       
                       # Convert the month matrix to a dataframe
                       ses_month_df <- ses_months %>%
                         as.data.frame()
                       # Extract first non-NA month
                       for(i in 1:nrow(ses_month_df)){
                         if(all(is.na(ses_month_df))){
                           ses_start <- NA 
                         } else{
                           if(is.na(ses_month_df[i,])){
                             next
                           } else{
                             ses_start <- ses_month_df[i,]
                             break
                           }
                         }
                       }
                       # Convert to yearmon format
                       ses_start <- as.yearmon(ses_start)
                       
                       # Replace negative predictions with 0
                       prediction_ses <- replace(prediction_ses, prediction_ses < 0, 0) %>% 
                         round(2) %>%
                         as.integer() 
                       
                       # Converts the predictions into a TS object for measuring accuracy
                       ses_FC <- prediction_ses %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                       
                       # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                       my_ses_Accuracy <- ses_FC %>%
                         accuracy(tsmyY)
                       
                       my_ses_Accuracy <- my_ses_Accuracy[,c(2,3,5)] %>%
                         as.data.frame() %>% 
                         t()
                       rownames(my_ses_Accuracy) <- NULL
                       
                       # Add Holdout_Start to the df
                       my_ses_Accuracy$Holdout_Start <- ses_start
                       
                       # Convert back to dataframe
                       my_ses_Accuracy <- as.data.frame(my_ses_Accuracy)
                       
                       # Rename columns
                       colnames(my_ses_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                     }
                     
                     
                     ############################################################################################################################
                     
                     if(nrow(myY) < 3){
                       forecast_TBATS_df <- NULL
                       my_tbats_Accuracy <- NULL
                     } else{
                       if(sum(is.na(myY)) >= 1){
                         isolate({
                           forecast_TBATS_df <- NULL
                           my_tbats_Accuracy <- NULL
                         })
                       } else{
                         isolate({
                           forecast_TBATS <- forecast(tbats(tsmyY),
                                                      h=forecast_n,
                                                      level = as.numeric(input$conf_int)
                           )
                         })
                         
                         # Convert elements of time series FORECAST to dataframe 
                         forecast_TBATS_df <- with(forecast_TBATS,
                                                   data.frame(Mean=forecast_TBATS$mean[forecast_n],
                                                              Upper=forecast_TBATS$upper[forecast_n, 1],
                                                              Lower=forecast_TBATS$lower[forecast_n, 1],
                                                              Parameters=forecast_TBATS$method))
                         
                         # Add Date column to the forecasted values data.frame
                         forecast_TBATS_df$Date <- as.Date(max(mySeries_filtered$Date)) %m+% months(forecast_n)
                         
                         # Add column for model name
                         forecast_TBATS_df$Model <- "TBATS"
                         
                         # Adjust dataframe to replace negative forecasted values with 0 and round Mean, Upper, Lower by two decimal places
                         forecast_TBATS_df <- forecast_TBATS_df %>%
                           select(Model, Parameters, Date, Mean, Upper, Lower) %>%
                           mutate(Mean = ifelse(as.integer(Mean) < 0,
                                                0,
                                                as.integer(round(Mean, 2))),
                                  Upper = ifelse(as.integer(Upper) < 0,
                                                 0,
                                                 as.integer(round(Upper, 2))),
                                  Lower = ifelse(as.integer(Lower) < 0,
                                                 0,
                                                 as.integer(round(Lower, 2))))
                         
                         # Recursive forecast for TBATS model
                         for (i in holdout_start:holdout_end){
                           
                           if(i < 2){
                             prediction_tbats[u] <- NA
                             tbats_months[u] <- NA
                             u <- u+1
                           } else{
                             train <- window(tsmyY, end = timeProp + ((i-1)/12))
                             
                             if(nmonths(train) < 3){
                               prediction_tbats[u] <- NA
                               tbats_months[u] <- NA
                               u <- u+1
                             } else{
                               FC_tbats <- forecast(tbats(train),
                                                    h = forecast_n)
                               prediction_tbats[u] <- FC_tbats[[2]][[forecast_n]]
                               tbats_months[u] <- tsp(train)[2] + ((forecast_n)/12)
                               u <- u+1
                             }
                           }
                         }
                         
                         # Convert the month matrix to a dataframe
                         tbats_month_df <- tbats_months %>%
                           as.data.frame()
                         # Extract first non-NA month
                         for(i in 1:nrow(tbats_month_df)){
                           if(all(is.na(tbats_month_df))){
                             tbats_start <- NA 
                           } else{
                             if(is.na(tbats_month_df[i,])){
                               next
                             } else{
                               tbats_start <- tbats_month_df[i,]
                               break
                             }
                           }
                         }
                         # Convert to yearmon format
                         tbats_start <- as.yearmon(tbats_start)
                         
                         # Replace negative predictions with 0
                         prediction_tbats <- replace(prediction_tbats, prediction_tbats < 0, 0) %>% 
                           round(2) %>%
                           as.integer() 
                         
                         # Converts the predictions into a TS object for measuring accuracy
                         tbats_FC <- prediction_tbats %>% ts(start = (timeProp2 - (recent_months-1)/12), frequency = 12)
                         
                         # Generates accuracy measurements for predicted values from recursive forecast against actual shipments
                         my_tbats_Accuracy <- tbats_FC %>%
                           accuracy(tsmyY)
                         
                         my_tbats_Accuracy <- my_tbats_Accuracy[,c(2,3,5)] %>%
                           as.data.frame() %>% 
                           t()
                         rownames(my_tbats_Accuracy) <- NULL
                         
                         # Add Holdout_Start to the df
                         my_tbats_Accuracy$Holdout_Start <- tbats_start
                         
                         # Convert back to dataframe
                         my_tbats_Accuracy <- as.data.frame(my_tbats_Accuracy)
                         
                         # Rename columns
                         colnames(my_tbats_Accuracy) <- c("RMSE", "MAE", "MAPE", "Holdout_Start")
                       }
                     }
                     
                     
                     isolate({
                       combined_df <- cbind(rbind(forecast_ARIMA_df,
                                                  forecast_ARFIMA_df,
                                                  forecast_croston_df,
                                                  forecast_ets_df,
                                                  forecast_HW_M_df,
                                                  forecast_HW_MD_df,
                                                  forecast_HW_A_df,
                                                  forecast_HW_AD_df,
                                                  forecast_LOBF_df,
                                                  forecast_MA_df,
                                                  forecast_SES_df,
                                                  forecast_TBATS_df),
                                            rbind(my_arima_Accuracy,
                                                  my_arfima_Accuracy,
                                                  my_croston_Accuracy,
                                                  my_ets_Accuracy,
                                                  my_hw_M_Accuracy,
                                                  my_hw_MD_Accuracy,
                                                  my_hw_A_Accuracy,
                                                  my_hw_AD_Accuracy,
                                                  my_lobf_Accuracy,
                                                  my_ma_Accuracy,
                                                  my_ses_Accuracy,
                                                  my_tbats_Accuracy))
                       return(combined_df %>%
                                select(Model, Parameters, Date, Mean, Upper, Lower, Holdout_Start, RMSE, MAE, MAPE) %>%
                                mutate(RMSE = round(RMSE, 2), MAE = round(MAE, 2), MAPE = round(MAPE, 2), Holdout_Start = as.character(Holdout_Start)) %>%
                                datatable(rownames = FALSE, options = list(pageLength = 12, dom = "t", ordering = F)) %>%
                                formatStyle(
                                  'MAPE',
                                  fontWeight = 'bold', # Makes the font for the MAPE column bold
                                  backgroundColor = styleInterval(c(15,45), c('#8DDA77', '#F3EC58', '#E75943')) # Formats the cells based on the value, <15~green, >15&<45~yellow, >45 red
                                ))
                     })
                   })
    })
    ###############################################################################################################################################################################
    ###############################################################################################################################################################################
    
    
    #############################################
    ############# Decomposed Plot ###############
    #############################################
    
    output$decomposed_plots <- shiny::renderPlot({
      
      #use existing reactive structures
      mySeries <- mySeries_raw()
      mySeries_XTS <- mySeries_filtered()
      
      if (nrow(mySeries_XTS) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'
          ))
        )
      }
      
      #make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else
        {
          task_type = input$i_task_select
        }
      })
      
      myY <-  xts(select_(mySeries_XTS, task_type),
                  order.by=ymd(mySeries_XTS$Date))
      #monthly frequency
      
      #Create a ts object from the xts object
      y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
      m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
      y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
      m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
      tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
      
      #Plot Decomposed plot
      decomp_plot <- autoplot(stats::decompose(tsmyY))
      
      print(decomp_plot)
    })
    
    #############################################
    ############# Seasonal Plot #################
    #############################################
    
    output$seasonal_plot <- shiny::renderPlot({
      
      # Use existing reactive structures
      mySeries <- mySeries_raw()
      mySeries_XTS <- mySeries_filtered()
      
      if (nrow(mySeries_XTS) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
      })
      
      if (input$checkbox2) {
        mySeries_XTS[,2] <- mySeries_XTS[,2] %>%
          tidyr::replace_na(0)
      }
      
      myY <-  xts(select_(mySeries_XTS, task_type),
                  order.by=ymd(mySeries_XTS$Date))
      
      #Create a ts object from the xts object
      y1 <- as.numeric(format(start(myY), "%Y")) # Takes the the year of the first observation
      m1 <- as.numeric(format(start(myY), "%m")) # Takes the month of the first observation
      y2 <- as.numeric(format(end(myY), "%Y")) # Takes the the year of the last observation
      m2 <- as.numeric(format(end(myY), "%m")) # Takes the month of the last observation
      tsmyY <- ts(myY, start = c(y1, m1), end = c(y2, m2), frequency = 12) # Creates a ts object
      
      # Plot seasonality of data
      seasonplot <- ggseasonplot(tsmyY,
                                 year.labels = TRUE,
                                 year.labels.left = TRUE) +
        ggtitle("Seasonality Plot")
      
      print(seasonplot)
    })
    
    ########################################
    ############# ACF Plot #################
    ########################################
    
    output$acf_plot <- shiny::renderPlot({
      
      # Use existing reactive structures
      mySeries <- mySeries_raw()
      mySeries_XTS <- mySeries_filtered()
      
      if (nrow(mySeries_XTS) == 0){
        stop(
          showModal(modalDialog(
            title = "Important message",
            'Please hit "start forecasting"!',
            easyClose = TRUE,
            size = 's'))
        )
      }
      
      # Make inputs dependent on users hitting 'start forecasting' button
      isolate({
        if(input$i_task_select ==""){
          task_type = select_(mySeries, .dots = list(quote(-Date)))
          task_type = names(task_type[1])
        } else {
          task_type = input$i_task_select
        }
      })
      
      myY <-  xts(select_(mySeries_XTS, task_type),
                  order.by=ymd(mySeries_XTS$Date))
      
      # Plot seasonality of data
      acfplot <- autoplot(acf(myY)) +
        ggtitle(paste(task_type))
      
      print(acfplot)
    })
    
    # Automatically stop the shiny app when closing the browser tab
    session$onSessionEnded(stopApp)
  
  # Run the application 
  #shinyApp(ui = ui, server = server)
  
  # Checks the last error
  #rlang::last_error()
})
