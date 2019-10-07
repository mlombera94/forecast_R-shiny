# Forecast Application with R-shiny
## <a href="https://mlombera.shinyapps.io/forecast_r-shiny/" target="_blank">Launch the Shiny app!</a>

## Summary
This shiny app allows users to forecast monthly demand by applying various time series models and comparing the performance of each model. There are four main functions of this application including:
- Uploading raw demand data in a csv format and filtering data

![gif1](https://user-images.githubusercontent.com/20471627/66239497-dd1eec00-e6ae-11e9-9777-9fa4f9550538.gif)

- Single SKU forecasts and visualization

![gif2](https://user-images.githubusercontent.com/20471627/66339136-5872de80-e8f7-11e9-9f05-650156aff007.gif)

- Batch SKU forecasts

![gif3](https://user-images.githubusercontent.com/20471627/66340022-35492e80-e8f9-11e9-86a7-f851bf52f2c0.gif)

- Statistical analysis/visualization

## Packages Utilized
- shiny
- zoo
- xts
- ggplot2
- devtools
- forecast
- data.table
- DT
- dygraphs
- dplyr
- lubridate
- stats
- tsbox
- tidyr
- padr
- shinythemes
- rio
- magrittr
- shinyjs
- rsconnect

## Features
- Users can filter data based on region, country, product, and SKU number. Users can also filter time series data that doesn't have the specified number of minimum observations or too many recent zero observations. The time series data can also be aggregated. 
- Users have the option to specify the parameters of the forecast such as how many months ahead to forecast, which models to apply, the size of the confidence interval, and how to split the time series data into training and test data sets. 
- Aside from individually forecasting time series data for individual SKUs, users have the option to batch forecast for multiple SKUs at once. This allows multiple forecasts to be generated quickly despite possibly applying all 12 models on multiple SKUs. Just like the individual forecasts, users can specify certain parameters of the forecast. 
- There is also the option to visualize the time series data for individual SKUs such as seasonality plots, ACF plots, and decomposed plots. 

## File Format
There is currently no file format validation built in. Files should have format as per the table below. A data set example is provided <a href="https://raw.githubusercontent.com/mlombera94/forecast_R-shiny/master/dataset.csv" target="_blank">here</a>. 
  
|```Date```| ```SKU #```| ```Product```| ```Country```| ```Region```| ```Demand```|
|:---------------|:---------------|:---------------|:--------------|:---------------|:------------------|
|```dd/mm/yyyy```| ```SKU # 1```	|	```Product A```|```Country A```| ```Region A```	|	```Demand Value```|
|```dd/mm/yyyy```| ```SKU # 2```	|	```Product B```|```Country B```| ```Region B```	|	```Demand Value```|
|```dd/mm/yyyy```| ```SKU # 3```	|	```Product C```|```Country C```| ```Region C```	|	```Demand Value```|

## Current Bugs
- If the user clicks on "Build Dataset" without having filled in all the sections, the application will crash, requiring the user to restart the application. 
- If the user clicks on "Build Dataset" and none of SKUs selected satisfy the condition for having the minimum number of observations or exceeding the maximum number of NA observations, the application will crash as there are no SKUs to create a dataset
- If all SKUs are filtered out by exceeding the maximum number of zeros within the last six observations, clicking on "Start Batch Forecasting!" will result in an error "subscripts out of bounds" as there are no SKUs being forecasted. 
- ACF plots and Decomposition plots will not work if NAs are present in the data, therefore the user must replace NA values with zero values. 
