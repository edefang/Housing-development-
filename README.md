Here it is rewritten cleanly in **proper GitHub README.md format** — concise, structured, and professional, with no AI tone and no filler. You can paste this directly into `README.md`.

````md
# Public Housing Development Analysis Dashboard

**Live Dashboard:** https://edefang.shinyapps.io/House/

## Project Overview

This project analyzes federally assisted public housing across the United States using data from the U.S. Department of Housing and Urban Development (HUD). The interactive dashboard examines housing availability, rent structure, household income, and resident demographics at both national and state levels.

The goal is to better understand how public housing operates in practice and how policy-driven rent structures align with household income. The dataset represents nearly **900,000 households nationwide**.

## Project Motivation

Public housing is often discussed using summaries or private market comparisons, but rarely explored directly through data. This project focuses on public housing itself, using administrative HUD data to evaluate affordability, allocation patterns, and demographic outcomes.

The project also reflects my interest in applying data science to real-world systems, particularly in housing, public policy, and equity-focused analysis.

## Dashboard Components

- **National Overview**  
  A high-level view of public housing across all 50 states, including total units, occupancy rates (approximately **95.6%**), and vacancy trends.

- **Financial Analysis**  
  Examines the relationship between **Median Household Income ($18,254)** and **Average Monthly Rent ($412)**, highlighting the role of federal subsidies.

- **Demographic Insights**  
  Household-level characteristics show that:
  - **73.9% of households are female-headed**
  - **33.8% include a person with a disability**

- **Statistical Modeling**  
  Linear regression is used to identify the primary factors influencing monthly rent in public housing.

## Statistical Findings

- **Model Fit:**  
  Explains **85.9% of the variance in rent (R² = 0.859)**

- **Statistical Significance:**  
  Overall model significance (**p-value < 0.001**)

- **Primary Predictor:**  
  Household income is the strongest determinant of rent.

- **Bedroom Count:**  
  Number of bedrooms is not a statistically significant predictor of rent, unlike in private housing markets.

## Tools and Technologies

- **Language:** R  
- **Framework:** Shiny  
- **Visualization:** ggplot2, Leaflet  
- **Analysis:** Linear Regression  
- **Data Source:** U.S. Department of Housing and Urban Development (HUD)

## Repository Structure

```text
├── app.R                # Main Shiny application
├── data/                # Processed HUD datasets
├── R/                   # Data cleaning and analysis scripts
├── www/                 # Styling and image assets
└── README.md            # Project documentation
````

## Key Takeaways

* **Household Structure:**
  Approximately **91% of households with children are single-parent households**, indicating a need for additional support services.

* **Unit Allocation:**
  Single occupants in multi-bedroom units suggest potential inefficiencies that could be addressed to reduce waitlists.

## Running the Project Locally

```r
install.packages(c("shiny", "tidyverse", "leaflet", "ggplot2"))
library(shiny)
runGitHub("Your-Repo-Name", "Your-Username")
```

## Author

**Eyong Defang**
Data Science Student
```

