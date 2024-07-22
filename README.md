# Discharge planning suite

This project simulates expected discharges and pathway requirements for patients over a 10-day horizon. This can be helpful for planning for upcoming discharge activity within local community settings.

# Features

- Simulates patient journeys for currently admitted population using models calibrated on historic data.
- Simulates new arrivals, and their journeys.
- Aggregates to a 10-day forecast.
- Displays model outputs on a Shiny dashboard.

# Limitations

Currently, this project is designed for a specific Docker instance with a customised environment. It requires:

- R version 4.2.2
- Specific SQL tables pre-populated with relevant patient data
- Specific environment variables configured

Due to these dependencies, running the code directly (without modifications) on other machines is not feasible.


# Package dependencies
- `tidyverse`: For data manipulation and visualization.
- `tidymodels`: For building and applying statistical models.
- `RODBC`: For connecting to SQL databases.
- `fitdistr` & `fitdistrplus`: For fitting probability distributions.
- `tsibble`: For time series data manipulation.
- `fable` & `fabletools`: For building and evaluating ARIMA model.
- `actuar` & `extraDistr`: For additional probability distributions.
- `ggiraph` & `ggh4x`: For creating advanced visualisations.
- `shiny` & `shinydashboard`: For building interactive web applications 

# Code overview

The core functionalities are implemented in separate R scripts:

- `code_main.R`: This script runs the main simulation model for predicting discharges and pathways.

- `code_los_model.R` & `code_pathway_model.R`: These scripts calibrate the models used for predicting length-of-stay and discharge pathways, respectively. Calibration should be done before running the main model.