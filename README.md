Public Housing Development Analysis Dashboard

Live Dashboard: https://edefang.shinyapps.io/House/

Project Overview

This project analyzes federally assisted public housing across the United States using data from the U.S. Department of Housing and Urban Development (HUD). I built an interactive dashboard to examine housing availability, rent structure, household income, and resident demographics at both the national and state levels.

The focus of the project is to understand how public housing operates in practice and how rent, income, and household characteristics interact within federally subsidized systems. The dataset represents outcomes for nearly 900,000 households nationwide.

Project Motivation

Housing affordability is often discussed using private market data, while public housing is treated as a black box. I wanted to look directly at how public housing works, using real administrative data, and evaluate whether rent and allocation patterns reflect the policy goals behind HUD programs.

This project also reflects my interest in applying data analysis to real social systems, especially in areas like housing equity, public policy, and resource allocation.

Dashboard Components

The dashboard is organized into four sections:

National Overview
A high-level view of public housing across all 50 states, including total housing units, occupancy rates (approximately 95.6%), and vacancy trends.

Financial Analysis
Examines the relationship between Median Household Income ($18,254) and Average Monthly Rent ($412), with a focus on how federal subsidies influence affordability.

Demographic Insights
Provides a breakdown of household characteristics, showing that:

73.9% of households are female-headed

33.8% include a person with a disability

Statistical Modeling
Uses linear regression to identify the key factors that influence monthly rent in public housing.

Statistical Findings

The regression model produced the following results:

Model Fit:
The model explains 85.9% of the variance in rent (R² = 0.859)

Statistical Significance:
The overall model is statistically significant (p-value < 0.001)

Primary Driver of Rent:
Household income is the strongest predictor of rent, consistent with HUD’s income-based rent structure.

Bedroom Count:
The number of bedrooms was not a statistically significant factor in determining rent, which differs from private housing market behavior.

Tools and Technologies

Language: R

Framework: Shiny

Visualization: ggplot2, Leaflet

Analysis: Linear Regression

Data Source: U.S. Department of Housing and Urban Development (HUD)

Repository Structure
├── app.R                # Main Shiny application
├── data/                # Processed HUD datasets
├── R/                   # Data cleaning and analysis scripts
├── www/                 # Styling and image assets
└── README.md            # Project documentation

Key Takeaways

Household Structure:
Approximately 91% of households with children are single-parent households, indicating a strong need for complementary social support services.

Unit Allocation:
The presence of single occupants in multi-bedroom units suggests potential inefficiencies that could be addressed to reduce waitlists.

Running the Project Locally

To run the dashboard locally, install the required packages and use:

install.packages(c("shiny", "tidyverse", "leaflet", "ggplot2"))
library(shiny)
runGitHub("Your-Repo-Name", "Your-Username")

Author

Eyong Defang
Data Science Student
Interested in housing, public policy, and applied analytics

Data Update Cycle: HUD quarterly release
