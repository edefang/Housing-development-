library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

if (file.exists("modules/mod_scatter_ui.R")) source("modules/mod_scatter_ui.R")
if (file.exists("modules/mod_scatter_server.R")) source("modules/mod_scatter_server.R")
if (file.exists("modules/mod_demographics_map_ui.R")) source("modules/mod_demographics_map_ui.R")
if (file.exists("modules/mod_demographics_map_server.R")) source("modules/mod_demographics_map_server.R")


data_file <- "Public_Housing_Developments_cleaned.csv"

housing_data <- NULL
tryCatch({
  housing_data <- read_csv(data_file, show_col_types = FALSE)
  cat("Ã¢Å“â€œ Loaded data from:", data_file, "\n")
}, error = function(e) {
  cat("Ã¢Å“â€” Failed to load:", data_file, "\n")
  stop("Data file not found:", data_file)
})

if (is.null(housing_data) || nrow(housing_data) == 0) {
  stop("Could not load housing data. Please ensure data file exists.")
}

state_fips_to_name <- c(
  "01" = "Alabama", "02" = "Alaska", "04" = "Arizona", "05" = "Arkansas",
  "06" = "California", "08" = "Colorado", "09" = "Connecticut", "10" = "Delaware",
  "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia", "15" = "Hawaii",
  "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa",
  "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine",
  "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan", "27" = "Minnesota",
  "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska",
  "32" = "Nevada", "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico",
  "36" = "New York", "37" = "North Carolina", "38" = "North Dakota", "39" = "Ohio",
  "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island",
  "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas",
  "49" = "Utah", "50" = "Vermont", "51" = "Virginia", "53" = "Washington",
  "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming"
)

housing_data <- housing_data %>%
  filter(
    !is.na(rent_per_month), rent_per_month > 0,
    !is.na(total_dwelling_units), total_dwelling_units > 0,
    !is.na(hh_income), hh_income > 0
  ) %>%
  mutate(
    state_fips = sprintf("%02d", state2kx),
    state_name = ifelse(!is.na(std_st), std_st, 
                        ifelse(state_fips %in% names(state_fips_to_name),
                               state_fips_to_name[state_fips], 
                               paste("State", state_fips))),
    county_name = ifelse(!is.na(cnty_nm2kx), cnty_nm2kx,
                         ifelse(!is.na(curcnty_nm), curcnty_nm, "Unknown")),
    city_name = ifelse(!is.na(std_city), std_city, "Unknown"),
    
    vacant_units = ifelse(!is.na(regular_vacant), regular_vacant, 0),
    vacancy_rate = ifelse(!is.na(pct_occupied), 100 - pct_occupied, 
                          ifelse(total_dwelling_units > 0, 
                                 (vacant_units / total_dwelling_units) * 100, 0)),
    vacancy_category = case_when(
      vacancy_rate < 5 ~ "Low (0-5%)",
      vacancy_rate < 10 ~ "Moderate (5-10%)",
      vacancy_rate < 20 ~ "High (10-20%)",
      TRUE ~ "Very High (20%+)"
    ),
    occupancy_rate = ifelse(!is.na(pct_occupied), pct_occupied, 100 - vacancy_rate),
    
    annual_rent = rent_per_month * 12,
    rent_to_income_ratio = annual_rent / hh_income,
    affordability_status = ifelse(rent_to_income_ratio < 0.30, 
                                  "Affordable (<30%)", "Unaffordable (Ã¢â€°Â¥30%)"),
    rent_category = ntile(rent_per_month, 4),
    rent_category = case_when(
      rent_category == 1 ~ "Very Low",
      rent_category == 2 ~ "Low",
      rent_category == 3 ~ "Medium",
      TRUE ~ "High"
    ),
    
    income_category = cut(hh_income,
                          breaks = c(0, 30000, 50000, 75000, 100000, Inf),
                          labels = c("0-30k", "30-50k", "50-75k", "75-100k", "100k+"),
                          include.lowest = TRUE),
    
    pct_male_head = ifelse(!is.na(pct_female_head), 100 - pct_female_head, NA),
    pct_female_head = ifelse(!is.na(pct_female_head), pct_female_head, NA),
    gender_head_category = case_when(
      !is.na(pct_female_head) & pct_female_head >= 50 ~ "Female Majority",
      !is.na(pct_female_head) & pct_female_head < 50 ~ "Male Majority",
      TRUE ~ "Unknown"
    ),
    
    pct_black = ifelse(!is.na(pct_black), pct_black, 0),
    pct_hispanic = ifelse(!is.na(pct_hispanic), pct_hispanic, 0),
    pct_asian = ifelse(!is.na(pct_asian), pct_asian, 0),
    pct_native_american = ifelse(!is.na(pct_native_american), pct_native_american, 0),
    pct_white = ifelse(!is.na(pct_minority), 100 - pct_minority, NA),
    pct_minority = ifelse(!is.na(pct_minority), pct_minority, 0),
    
    dominant_race = case_when(
      pct_black >= pct_hispanic & pct_black >= pct_asian & pct_black >= pct_native_american & pct_black >= ifelse(!is.na(pct_white), pct_white, 0) ~ "Black",
      pct_hispanic >= pct_asian & pct_hispanic >= pct_native_american & pct_hispanic >= ifelse(!is.na(pct_white), pct_white, 0) ~ "Hispanic",
      pct_asian >= pct_native_american & pct_asian >= ifelse(!is.na(pct_white), pct_white, 0) ~ "Asian",
      pct_native_american >= ifelse(!is.na(pct_white), pct_white, 0) ~ "Native American",
      !is.na(pct_white) & pct_white >= 50 ~ "White",
      TRUE ~ "Mixed/Other"
    ),
    
    minority_category = cut(ifelse(!is.na(pct_minority), pct_minority, 0),
                            breaks = c(0, 25, 50, 75, 100),
                            labels = c("Low (0-25%)", "Moderate (25-50%)", 
                                       "High (50-75%)", "Very High (75-100%)"),
                            include.lowest = TRUE),
    
    low_income_concentration = cut(ifelse(!is.na(pct_lt30_median), pct_lt30_median, 0),
                                   breaks = c(0, 25, 50, 75, 100),
                                   labels = c("Low", "Moderate", "High", "Very High"),
                                   include.lowest = TRUE),
    
    size_category = cut(total_dwelling_units,
                        breaks = c(0, 50, 200, 500, Inf),
                        labels = c("Small (1-50)", "Medium (51-200)", 
                                   "Large (201-500)", "Very Large (500+)"),
                        include.lowest = TRUE),
    
    vulnerability_index = rowMeans(
      cbind(
        ifelse(!is.na(pct_lt30_median), pct_lt30_median, 0),
        ifelse(!is.na(pct_minority), pct_minority, 0),
        ifelse(!is.na(pct_disabled_all), pct_disabled_all, 0),
        ifelse(!is.na(pct_female_head_child), pct_female_head_child, 0)
      ),
      na.rm = TRUE
    )
  )

state_choices <- sort(unique(housing_data$state_name))
cat("âœ“ States:", length(state_choices), "\n")

header <- dashboardHeader(
  title = tagList(
    tags$i(class = "fa fa-home", style = "margin-right: 10px;"),
    "Public Housing Dashboard"
  ),
  titleWidth = 300,
  tags$li(class = "dropdown",
    tags$a(href = "https://www.linkedin.com/in/defang-ako-eyong-809168294/", 
           target = "_blank",
           style = "font-weight: bold; font-size: 14px;",
           tags$i(class = "fa fa-linkedin", style = "margin-right: 5px;"),
           "Defang Ako Eyong")
  )
)

sidebar <- dashboardSidebar(
  width = 280,
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Housing Development", tabName = "housing_development", icon = icon("building")),
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Finance", tabName = "rent", icon = icon("dollar-sign")),
    menuItem("Demographics", tabName = "demographics", icon = icon("users")),
    
    menuItem("Statistics", tabName = "states", icon = icon("chart-line")),
    
    
    menuItem("Data Explorer", tabName = "data_table", icon = icon("table")),
    br(),
    
    selectInput(
      "state_filter",
      "Filter by State:",
      choices = c("All States" = "all", state_choices),
      selected = "all",
      multiple = FALSE
    ),
    br()
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      /* Modern Professional Styling - Inspired by Appsilon Templates */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
      
      body, .content-wrapper, .main-sidebar, .sidebar {
        font-family: 'Inter', 'Segoe UI', sans-serif;
      }
      
      /* Header Styling */
      .main-header .logo {
        font-weight: 700;
        font-size: 18px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-bottom: 3px solid #5568d3;
      }
      
      .main-header .navbar {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      }
      
      /* Sidebar Styling */
      .main-sidebar {
        background: #2c3e50;
        box-shadow: 2px 0 10px rgba(0,0,0,0.1);
      }
      
      .sidebar-menu > li > a {
        border-left: 3px solid transparent;
        transition: all 0.3s ease;
      }
      
      .sidebar-menu > li.active > a,
      .sidebar-menu > li:hover > a {
        background: #34495e;
        border-left: 3px solid #667eea;
      }
      
      /* Content Area */
      .content-wrapper {
        background: #f8f9fa;
      }
      
      /* Box Styling - disable hover transforms to avoid visual shaking */
        NULL
        background: linear-gradient(to right, #f8f9fa 0%, #ffffff 100%);
        border-bottom: 1px solid #e9ecef;
      }
      
      .box-header h3 {
        font-size: 16px;
        font-weight: 600;
        color: #2c3e50;
        margin: 0;
      }
      
      /* Value Boxes */
      .small-box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        transition: none;
      }
      
      .small-box h3 {
        font-weight: 700;
        font-size: 32px;
      }
      
      /* Info Boxes */
      .info-box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        transition: none;
      }
      
      /* Buttons */
      .btn {
        border-radius: 6px;
        font-weight: 600;
        transition: none;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-size: 12px;
      }
      
      .btn-warning {
        background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%);
        border: none;
      }
      
      .btn-warning:hover {
        background: linear-gradient(135deg, #e67e22 0%, #d35400 100%);
        box-shadow: 0 4px 12px rgba(230, 126, 34, 0.4);
      }
      
      /* Slider Inputs */
      .irs-bar {
        background: linear-gradient(to right, #667eea, #764ba2);
      }
      
      .irs-from, .irs-to, .irs-single {
        background: #667eea;
      }
      
      /* Tab Headers */
      h2 {
        color: #2c3e50;
        font-weight: 700;
        font-size: 28px;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 3px solid #667eea;
        display: inline-block;
      }
      
      /* Select Inputs */
      .selectize-input {
        border-radius: 6px;
        border: 2px solid #e9ecef;
        transition: all 0.3s ease;
      }
      
      .selectize-input:focus {
        border-color: #667eea;
        box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
      }
      
      /* Custom scrollbar */
      ::-webkit-scrollbar {
        width: 10px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #667eea;
        border-radius: 5px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #5568d3;
      }

      /* Market Overview Value Boxes - unique teal gradient (compact) */
        background: linear-gradient(135deg, #1abc9c 0%, #16a085 100%) !important;
        border-top: 3px solid #138f75 !important;
        color: #ffffff !important;
        min-height: 72px;
        padding: 8px 10px;
      }
        color: #ffffff !important;
      }
        font-size: 20px;
        margin: 6px 0 4px;
      }
        color: rgba(255,255,255,0.95) !important;
        font-size: 30px;
      }
    "))
  ),
  
  tabItems(
    tabItem(
      tabName = "housing_development",
      h2("Housing Development Data"),
      
      fluidRow(
        column(width = 12,
          box(
            title = "Project Background",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            div(style = "padding: 10px; font-size: 16px; line-height: 1.6;",
              HTML("The <b>HUD Public Housing Developments dataset</b> provides a national snapshot of federally assisted housing across the United States. Each development is represented by the building with the largest number of units, ensuring consistency while protecting privacy. The dataset combines <b>geographic coordinates</b> with detailed information on housing units, occupancy, household demographics, income, and federal spending. It tracks characteristics such as the number of occupied and vacant units, average household size, age distribution, disability status, and racial/ethnic composition. Financial measures include household income, rent contributions, utility allowances, and federal subsidies. Poverty levels are also recorded at the census tract level, offering insight into the broader community context. Updated quarterly, the dataset supports research, planning, and policy development by highlighting patterns of affordability, inequality, and neighborhood conditions. By making this information publicly available, HUD enables policymakers, researchers, and advocates to better understand and improve public housing nationwide."),
              br(), br(),
              tags$a(href = "https://hudgis-hud.opendata.arcgis.com/datasets/HUD::public-housing-developments-1/about", 
                     target = "_blank", 
                     style = "color: #3c8dbc; font-weight: bold;",
                     icon("external-link-alt"), " View Dataset & Documentation")
            )
          )
        )
      ),
      
      h3("Key Dataset Features", style = "margin-top: 0; margin-bottom: 20px; border-bottom: 2px solid #d2d6de; padding-bottom: 10px;"),
      fluidRow(
        column(width = 3,
          div(class = "small-box bg-light-blue",
            div(class = "inner",
              h4("National Scope"),
              p("Federally assisted housing across the US, with geographic coordinates for mapping.")
            ),
            div(class = "icon", icon("map-marked-alt"))
          )
        ),
        column(width = 3,
          div(class = "small-box bg-teal",
            div(class = "inner",
              h4("Demographics"),
              p("Tracks occupancy, age, disability, race/ethnicity, and household size.")
            ),
            div(class = "icon", icon("users"))
          )
        ),
        column(width = 3,
          div(class = "small-box bg-olive",
            div(class = "inner",
              h4("Financials"),
              p("Income, rent contributions, utility allowances, and federal subsidies.")
            ),
            div(class = "icon", icon("hand-holding-usd"))
          )
        ),
        column(width = 3,
          div(class = "small-box bg-navy",
            div(class = "inner",
              h4("Impact"),
              p("Updated quarterly to support policy, research, and community improvement.")
            ),
            div(class = "icon", icon("chart-line"))
          )
        )
      ),
      
      fluidRow(
        column(width = 12,
          box(
            title = "Dashboard Objectives",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            div(style = "padding: 10px; text-align: center; margin-bottom: 20px;",
                h4(style = "font-weight: bold;", "Main Goal"),
                p(style = "font-size: 16px;", "To provide a comprehensive, data-driven analysis of public housing developments, enabling stakeholders to understand financial accessibility, demographic composition, and key statistical relationships affecting housing equity.")
            ),
            HTML("
              <div style='display: flex; justify-content: space-around; flex-wrap: wrap; border-top: 1px solid #eee; padding-top: 20px;'>
                <div style='flex: 1; min-width: 200px; margin: 10px; text-align: center;'>
                  <i class='fa fa-hand-holding-usd fa-3x' style='color: #00c0ef;'></i>
                  <h4 style='margin-top: 15px; font-weight: bold;'>Financial Accessibility</h4>
                  <p>Analyze rent burdens, income distributions, and affordability metrics across states.</p>
                </div>
                <div style='flex: 1; min-width: 200px; margin: 10px; text-align: center;'>
                  <i class='fa fa-users fa-3x' style='color: #00a65a;'></i>
                  <h4 style='margin-top: 15px; font-weight: bold;'>Demographic Insights</h4>
                  <p>Examine household structures, racial composition, and age demographics to ensure equitable housing distribution.</p>
                </div>
                <div style='flex: 1; min-width: 200px; margin: 10px; text-align: center;'>
                  <i class='fa fa-project-diagram fa-3x' style='color: #f39c12;'></i>
                  <h4 style='margin-top: 15px; font-weight: bold;'>Statistical Drivers</h4>
                  <p>Identify significant correlations between economic factors and housing characteristics using rigorous statistical modeling.</p>
                </div>
              </div>
            ")
          )
        )
      )
    ),
    tabItem(
      tabName = "overview",
      
      fluidRow(
        column(width = 12,
          div(style = "background-color: #ecf0f5; border-left: 5px solid #3c8dbc; padding: 15px; margin-bottom: 20px; border-radius: 3px; box-shadow: 0 1px 1px rgba(0,0,0,0.1);",
            h4(style = "margin-top: 0; color: #3c8dbc;", "Summary"),
            uiOutput("overview_summary_report")
          )
        )
      ),

      fluidRow(
        column(width = 3,
          valueBoxOutput("overview_units_box", width = NULL)
        ),
        column(width = 3,
          valueBoxOutput("overview_vacancy_box", width = NULL)
        ),
        column(width = 3,
          box(width = NULL, solidHeader = TRUE, status = "primary", height = 150,
              valueBoxOutput("overview_rent_box", width = NULL),
              div(style = "margin-top: -10px;", selectInput("rent_view", label = NULL,
                  choices = c("Average" = "avg", "Highest" = "max", "Lowest" = "min"),
                  selected = "avg", width = "100%"))
          )
        ),
        column(width = 3,
          box(width = NULL, solidHeader = TRUE, status = "success", height = 150,
              valueBoxOutput("overview_income_box", width = NULL),
              div(style = "margin-top: -10px;", selectInput("income_view", label = NULL,
                  choices = c("Median" = "median", "Highest" = "max", "Lowest" = "min"),
                  selected = "median", width = "100%"))
          )
        )
      ),

      fluidRow(
        column(width = 8,
          box(
            title = "Top 10 States - Metric Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = 550,
            fluidRow(
              column(width = 12,
                selectInput("overview_metric", "Metric:",
                  choices = c(
                    "Average household rent contribution" = "rent",
                    "Vacancy Rate (avg %)" = "vacancy",
                    "Total Units" = "units",
                    "Median household income per year" = "income",
                    "Total Vacant Units" = "vacant_total",
                    "Total number of people" = "people_total"
                  ),
                  selected = "rent", width = "100%")
              )
            ),
            plotlyOutput("overview_top5_states", height = 430)
          )
        ),
        column(width = 4,
          box(width = NULL, solidHeader = TRUE, status = "warning", title = "Minority Representation", height = 180,
              valueBoxOutput("overview_minority_box", width = NULL),
              div(style = "margin-top: -10px;", selectInput("minority_view", label = NULL,
                choices = c("Minority", "Black", "Asian", "Hispanic", "Native American"),
                selected = "Minority", width = "100%"))
          ),
          box(
            title = "About the Data",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            p("This dashboard visualizes public housing data across US states. Use the filters in the sidebar to narrow down the analysis by state."),
            p("The 'Top 10 States' chart on the left allows you to compare different metrics across the highest ranking states.")
          )
        )
      )

      

    ),
    
    tabItem(
      tabName = "rent",
      h2("Finance"),
      tags$head(includeCSS("www/finance.css")),
      
      fluidRow(
        column(width = 12,
          div(style = "background-color: #ecf0f5; border-left: 5px solid #3c8dbc; padding: 15px; margin-bottom: 20px; border-radius: 3px; box-shadow: 0 1px 1px rgba(0,0,0,0.1);",
            h4(style = "margin-top: 0; color: #3c8dbc;", "Summary"),
            uiOutput("finance_summary_report")
          )
        )
      ),

      fluidRow(
        valueBoxOutput("finance_avg_income_box", width = 6),
        valueBoxOutput("finance_household_count_box", width = 6)
      ),

      fluidRow(
        valueBoxOutput("income_source_wage_box", width = 4),
        valueBoxOutput("income_source_welfare_box", width = 4),
        valueBoxOutput("income_source_other_box", width = 4)
      ),

      fluidRow(
        column(width = 8,
          box(
            title = "Household Income by Bedroom",
            status = "success",
            solidHeader = TRUE,
            width = NULL,
            height = 520,
            plotlyOutput("finance_income_by_bedroom_bracket", height = 420)
          )
        ),
        column(width = 4,
          box(
            title = "Bedroom Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = 520,
            uiOutput("finance_bedroom_dist_ui")
          )
        )
      ),

      fluidRow(
        column(width = 12,
          box(
            title = "Top 10 States - Income Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            div(style = "margin-bottom: 10px;",
              selectInput("finance_segment_by", NULL, 
                          choices = c("Breakdown by: Income Bracket" = "bracket", "Breakdown by: Income Source" = "source"),
                          selected = "bracket", width = "250px")
            ),
            plotlyOutput("finance_income_bracket_top10", height = 400)
          )
        )
      )
    ),
    
    
    
    tabItem(
      tabName = "demographics",
      h2("Demographic Analysis"),
      
      fluidRow(
        box(
          title = "Demographic Snapshot",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          uiOutput("demographics_summary_report")
        )
      ),

      fluidRow(
        column(width = 6,
          box(
            title = "Households with Children",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            plotlyOutput("demographics_household_donut", height = "350px")
          )
        ),
        column(width = 6,
          box(
            title = "Average Household Head Age Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            plotlyOutput("demographics_head_age_dist", height = "350px")
          )
        )
      ),
      
      fluidRow(
        column(width = 12,
          box(
            title = "Household Composition Analysis (1 vs 2 Adults)",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            fluidRow(
              column(width = 4,
                selectInput("pyramid_category", "Compare Household Structure by:",
                            choices = c("Income Level" = "income", 
                                        "Unit Size" = "unit_size",
                                        "Predominant Race" = "race",
                                        "Head of Household Age" = "age"),
                            selected = "income")
              )
            ),
            plotlyOutput("demographics_pyramid", height = "500px"),
            footer = "Compares the percentage of households with 1 Adult vs 2 Adults across different categories."
          )
        )
      ),
      
      fluidRow(
        column(width = 12,
          box(
            title = "Age Distribution by Predominant Race",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            plotlyOutput("demographics_age_by_race", height = "600px")
          )
        )
      )
    ),
    
    
    
    
    tabItem(
      tabName = "states",
      h2("Statistical Analysis & Models"),
      
      # Row 1: Key Performance Indicators (Top Level)
      fluidRow(
        valueBoxOutput("stats_r2_box", width = 4),
        valueBoxOutput("stats_p_box", width = 4),
        valueBoxOutput("stats_driver_box", width = 4)
      ),
      
      # Row 2: Main Content Area
      fluidRow(
        # Left Column: Controls & Guide
        column(width = 3,
          box(
            title = "Analysis Settings",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            background = "navy",
            selectInput("stats_model_select", "Select Analysis Question:",
                        choices = c(
                          "What drives Monthly Rent?" = "R2",
                          "What determines Household Income?" = "I1",
                          "What affects Occupancy Rates?" = "O1",
                          "Does Bedroom Size affect Rent?" = "R1",
                          "What influences Project Spending?" = "L1",
                          "What predicts Wait Times?" = "L2"
                        ),
                        selected = "R2"),
            p(style = "font-size: 13px; color: #bdc3c7; margin-top: 10px;",
              "Choose a question above to update the statistical model and insights.")
          ),
          
          box(
            title = "Statistical Guide",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            collapsible = TRUE,
            collapsed = FALSE,
            div(style = "font-size: 13px; line-height: 1.6; color: #333;",
              tags$ul(style = "padding-left: 15px; margin-bottom: 0;",
                tags$li(style="margin-bottom: 8px;", tags$strong("R-Squared (R²):"), br(), "Model Strength (0-100%). Higher is better."),
                tags$li(style="margin-bottom: 8px;", tags$strong("P-Value:"), br(), "Significance check. < 0.05 is good."),
                tags$li(style="margin-bottom: 8px;", tags$strong("Estimate:"), br(), "Impact per unit change."),
                tags$li(tags$strong("Std. Error:"), br(), "Margin of error.")
              )
            )
          )
        ),
        
        # Right Column: Insights & Data
        column(width = 9,
          box(
            title = "Model Insights",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            uiOutput("stats_model_interpretation")
          ),
          
          box(
            title = "Variable Impact Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            DTOutput("stats_coef_table")
          )
        )
      )
    ),
    
    
    
    tabItem(
      tabName = "data_table",
      h2("Data Explorer"),
      
      fluidRow(
        box(
          title = "Filtered Project Data", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          p(
            "Data Source: ",
            tags$a(href = "https://hudgis-hud.opendata.arcgis.com/datasets/HUD::public-housing-developments-1/about", 
                   target = "_blank", 
                   "HUD Public Housing Developments Dataset")
          ),
          DTOutput("data_table_output"),
          br(),
          downloadButton("download_csv", "Download CSV", class = "btn-success")
        )
      )
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)


server <- function(input, output, session) {
  
  
  filtered_data <- reactive({
    data <- housing_data
    if (input$state_filter != "all") {
      data <- data %>% filter(state_name == input$state_filter)
    }
    return(data)
  })
  
  
  

  



  output$overview_summary_report <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(HTML("<em>No data available.</em>"))
    
    total_units <- if ("total_dwelling_units" %in% names(data)) sum(data$total_dwelling_units, na.rm = TRUE) else NA
    
    vac_candidates <- c("regular_vacant", "vacant_units", "vacant_total", "total_vacant", "pha_vacant", "vacancies")
    col <- intersect(vac_candidates, names(data))
    vac_total <- NA_real_
    if (length(col) > 0) {
      vac_total <- sum(as.numeric(data[[col[1]]]), na.rm = TRUE)
    } else if ("total_dwelling_units" %in% names(data) && "total_occupied" %in% names(data)) {
      vac_total <- sum(as.numeric(data$total_dwelling_units) - as.numeric(data$total_occupied), na.rm = TRUE)
    }
    
    vacancy_rate <- if (!is.na(total_units) && total_units > 0 && !is.na(vac_total)) (vac_total / total_units) * 100 else NA
    
    avg_rent <- if ("rent_per_month" %in% names(data)) mean(data$rent_per_month, na.rm = TRUE) else NA
    median_income <- if ("hh_income" %in% names(data)) median(data$hh_income, na.rm = TRUE) else NA
    
    fmt_num <- function(x) if (is.na(x)) "N/A" else scales::comma(round(x))
    fmt_money <- function(x) if (is.na(x)) "N/A" else paste0("$", scales::comma(round(x)))
    fmt_pct <- function(x) if (is.na(x)) "N/A" else paste0(round(x, 1), "%")
    
    state_context <- if (input$state_filter == "all") "across all states" else paste("in", input$state_filter)
    
    html <- paste0(
      "<div style='font-size: 16px; line-height: 1.6; color: #2c3e50;'>",
      "This dashboard provides a comprehensive analysis of public housing developments ", state_context, ". ",
      "Currently tracking <strong>", fmt_num(total_units), "</strong> total dwelling units with a vacancy rate of <strong>", fmt_pct(vacancy_rate), "</strong>. ",
      "The median household income is <strong>", fmt_money(median_income), "</strong> per year, with an average monthly rent contribution of <strong>", fmt_money(avg_rent), "</strong>.",
      "</div>"
    )
    
    HTML(html)
  })

  output$overview_units_box <- renderValueBox({
    data <- filtered_data()
    if ("total_dwelling_units" %in% names(data)) {
      total_units <- sum(data$total_dwelling_units, na.rm = TRUE)
      valueBox(formatC(total_units, format = "d", big.mark = ","), "Total Units", icon = icon("warehouse"), color = "teal")
    } else {
      valueBox("N/A", "Total Units", icon = icon("warehouse"), color = "teal")
    }
  })

  output$overview_vacancy_box <- renderValueBox({
    data <- filtered_data()

    vac_candidates <- c("regular_vacant", "vacant_units", "vacant_total", "total_vacant", "pha_vacant", "vacancies")
    col <- intersect(vac_candidates, names(data))

    vac_total <- NA_real_
    if (length(col) > 0) {
      vac_total <- sum(as.numeric(data[[col[1]]]), na.rm = TRUE)
    } else if ("total_units" %in% names(data) && "total_occupied" %in% names(data)) {
      vac_total <- sum(as.numeric(data$total_units) - as.numeric(data$total_occupied), na.rm = TRUE)
    } else if ("total_dwelling_units" %in% names(data) && "total_occupied" %in% names(data)) {
      vac_total <- sum(as.numeric(data$total_dwelling_units) - as.numeric(data$total_occupied), na.rm = TRUE)
    }

    if (is.na(vac_total)) {
      valueBox("N/A", "Vacant Units", icon = icon("door-open"), color = "teal")
    } else {
      valueBox(formatC(round(vac_total), format = "d", big.mark = ","), "Vacant Units", icon = icon("door-open"), color = "teal")
    }
  })

  output$overview_rent_box <- renderValueBox({
    data <- filtered_data()
    rents <- data$rent_per_month
    rents <- rents[!is.na(rents)]
    if (length(rents) == 0) {
      val <- "N/A"
    } else {
      val <- switch(input$rent_view,
                    "avg" = paste0("$", format(round(mean(rents), 0), big.mark = ",")),
                    "max" = paste0("$", format(round(max(rents), 0), big.mark = ",")),
                    "min" = paste0("$", format(round(min(rents), 0), big.mark = ",")))
    }
    subtitle <- switch(input$rent_view, "avg" = "Average Rent", "max" = "Highest Rent", "min" = "Lowest Rent")
    valueBox(value = val, subtitle = subtitle, icon = icon("dollar-sign"), color = "teal")
  })

  output$overview_income_box <- renderValueBox({
    data <- filtered_data()
    inc <- data$hh_income
    inc <- inc[!is.na(inc)]
    if (length(inc) == 0) {
      val <- "N/A"
    } else {
      val <- switch(input$income_view,
                    "median" = paste0("$", format(round(median(inc), 0), big.mark = ",")),
                    "max" = paste0("$", format(round(max(inc), 0), big.mark = ",")),
                    "min" = paste0("$", format(round(min(inc), 0), big.mark = ",")))
    }
    subtitle <- switch(input$income_view, "median" = "Median Income", "max" = "Highest Income", "min" = "Lowest Income")
    valueBox(value = val, subtitle = subtitle, icon = icon("users"), color = "teal")
  })

  output$overview_minority_box <- renderValueBox({
    df <- filtered_data()
    req(df)
    sel <- input$minority_view
    if (is.null(sel) || sel == "") return(valueBox("N/A", "Minority", icon = icon("users"), color = "teal"))

    nm <- names(df)

    find_col <- function(candidates) {
      col <- intersect(candidates, nm)
      if (length(col) > 0) return(col[1])
      return(NULL)
    }

    pct_col <- switch(sel,
                      "Black" = find_col(c("pct_black","black","percent_black","black_pct")),
                      "Asian" = find_col(c("pct_asian","asian","asian_pct")),
                      "Hispanic" = find_col(c("pct_hispanic","hispanic","latinx","hispanic_pct")),
                      "Native American" = find_col(c("pct_native_american","pct_nativeamerican","native_american","nativeamerican")),
                      "Minority" = find_col(c("pct_minority","minority","tminority")),
                      NULL)

    pct_val <- NA_real_
    est_count <- NA_real_

    if (!is.null(pct_col)) {
      v <- suppressWarnings(as.numeric(df[[pct_col]]))
      if (!all(is.na(v))) pct_val <- mean(v, na.rm = TRUE)
    }

    if (!is.na(pct_val) && "total_units" %in% nm) {
      total_units_vec <- as.numeric(df[[which(nm=="total_units")]])
      est_count <- sum((pct_val/100) * total_units_vec, na.rm = TRUE)
    }

    if (is.na(pct_val)) {
      count_col <- switch(sel,
                          "Black" = find_col(c("black","black_count","n_black")),
                          "Asian" = find_col(c("asian","asian_count","n_asian")),
                          "Hispanic" = find_col(c("hispanic","hispanic_count","n_hispanic")),
                          "Native American" = find_col(c("native_american","nativeamerican","n_native")),
                          "Minority" = find_col(c("tminority","minority","n_minority")),
                          NULL)
      if (!is.null(count_col)) est_count <- sum(as.numeric(df[[count_col]]), na.rm = TRUE)
    }

    if (!is.na(est_count) && est_count > 0) {
      display <- format(round(est_count), big.mark = ",")
      subtitle <- if (!is.na(pct_val)) paste0(round(pct_val,1), "% of units") else sel
    } else if (!is.na(pct_val)) {
      display <- paste0(round(pct_val,1), "%")
      subtitle <- sel
    } else {
      display <- "N/A"
      subtitle <- sel
    }

    valueBox(value = display, subtitle = subtitle, icon = icon("users"), color = "teal")
  })

  output$overview_demographics <- renderUI({
    df <- filtered_data()

    people_cols <- c("total_residents", "total_people", "population", "pop_total")
    popcol <- intersect(people_cols, names(df))
    popcol <- if (length(popcol) > 0) popcol[1] else NULL

    race_candidates <- list(
      Black = c("black", "pct_black", "percent_black", "black_pct", "african_american"),
      Asian = c("asian", "pct_asian", "asian_pct"),
      Hispanic = c("hispanic", "latinx", "pct_hispanic", "hispanic_pct"),
      White = c("white", "pct_white", "white_pct"),
      Other = c("other", "pct_other", "other_pct", "race_other")
    )

    found <- list()
    for (nm in names(race_candidates)) {
      col <- intersect(race_candidates[[nm]], names(df))
      if (length(col) > 0) found[[nm]] <- col[1]
    }

    if (length(found) == 0) {
      return(tags$div("No demographic columns found in the dataset."))
    }

    vals <- sapply(names(found), function(nm) {
      col <- found[[nm]]
      v <- suppressWarnings(as.numeric(df[[col]]))
      if (all(is.na(v))) return(NA_real_)

      maxv <- max(v, na.rm = TRUE)
      if (!is.na(maxv) && maxv <= 1 && !is.null(popcol)) {
        return(sum(v * df[[popcol]], na.rm = TRUE))
      }
      if (!is.na(maxv) && maxv <= 100 && !is.null(popcol)) {
        return(sum((v / 100) * df[[popcol]], na.rm = TRUE))
      }
      return(sum(v, na.rm = TRUE))
    }, USE.NAMES = TRUE)

    vals[is.na(vals)] <- 0
    total <- sum(vals)
    if (total == 0) {
      vals_pct <- sapply(names(found), function(nm) {
        col <- found[[nm]]
        v <- suppressWarnings(as.numeric(df[[col]]))
        if (all(is.na(v))) return(0)
        mean(v, na.rm = TRUE)
      }, USE.NAMES = TRUE)
      if (all(vals_pct == 0)) return(tags$div("Demographic data present but not in a recognizable numeric form."))
      vals <- vals_pct / sum(vals_pct, na.rm = TRUE)
      pct <- vals * 100
    } else {
      pct <- vals / total * 100
    }

    colors <- c(Black = "#34495e", Asian = "#2980b9", Hispanic = "#e67e22", White = "#95a5a6", Other = "#8e44ad")

    bars <- lapply(names(pct), function(nm) {
      p <- round(pct[[nm]], 1)
      col <- if (!is.null(colors[[nm]])) colors[[nm]] else "#999999"
      tags$div(style = "margin-bottom:8px;",
               tags$div(style = "display:flex; justify-content:space-between; align-items:center;",
                        tags$span(nm),
                        tags$span(paste0(p, "%"))
               ),
               tags$div(class = "progress", style = "height:18px;",
                        tags$div(class = "progress-bar", role = "progressbar",
                                 style = paste0("width:", p, "%; background-color:", col, ";"),
                                 `aria-valuenow` = p, `aria-valuemin` = 0, `aria-valuemax` = 100)
               )
      )
    })

    return(tags$div(bars))
  })

    

  
  output$overview_top5_states <- renderPlotly({
    src <- housing_data
    metric <- ifelse(is.null(input$overview_metric), "rent", input$overview_metric)

    src <- src %>% filter(!is.na(state_name))

    if (metric == "rent") {
      df <- src %>% group_by(state_name) %>% summarise(val = mean(rent_per_month, na.rm = TRUE), .groups = "drop")
      ytitle <- "Rent ($)"
      yformat <- "currency"
    } else if (metric == "vacancy") {
      df <- src %>% group_by(state_name) %>% summarise(val = mean(vacancy_rate, na.rm = TRUE), .groups = "drop")
      ytitle <- "Vacancy Rate (%)"
      yformat <- "percent"
    } else if (metric == "units") {
      df <- src %>% group_by(state_name) %>% summarise(val = sum(total_dwelling_units, na.rm = TRUE), .groups = "drop")
      ytitle <- "Units"
      yformat <- "count"
    } else if (metric == "income") {
      df <- src %>% group_by(state_name) %>% summarise(val = median(hh_income, na.rm = TRUE), .groups = "drop")
      ytitle <- "Median Income ($)"
      yformat <- "currency"
    } else if (metric == "vacant_total") {
      if ("vacant_units" %in% names(src)) {
        df <- src %>% group_by(state_name) %>% summarise(val = sum(vacant_units, na.rm = TRUE), .groups = "drop")
      } else {
        if (all(c("vacancy_rate", "total_dwelling_units") %in% names(src))) {
          df <- src %>% mutate(est_vacant = ifelse(is.na(vacant_units), round((vacancy_rate/100) * total_dwelling_units), NA)) %>%
            group_by(state_name) %>% summarise(val = sum(est_vacant, na.rm = TRUE), .groups = "drop")
        } else {
          df <- src %>% group_by(state_name) %>% summarise(val = NA_real_, .groups = "drop")
        }
      }
      ytitle <- "Total Vacant Units"
      yformat <- "count"
    } else if (metric == "people_total") {
      people_cols <- c("total_residents", "total_people", "population", "pop_total")
      found_people <- intersect(people_cols, names(src))
      if (length(found_people) > 0) {
        col <- found_people[1]
        df <- src %>% group_by(state_name) %>% summarise(val = sum(.data[[col]], na.rm = TRUE), .groups = "drop")
        ytitle <- "Total People"
        yformat <- "count"
      } else {
        df <- src %>% group_by(state_name) %>% summarise(val = sum(total_dwelling_units, na.rm = TRUE), .groups = "drop")
        ytitle <- "Total People (proxy: units)"
        yformat <- "count"
      }
    } else {
      return(plotly_empty())
    }

    df <- df %>% filter(!is.na(val))

    hh_by_state <- src %>%
      group_by(state_name) %>%
      summarise(hh_income_state = mean(hh_income, na.rm = TRUE), .groups = "drop")

    df <- df %>%
      left_join(hh_by_state, by = "state_name") %>%
      arrange(desc(ifelse(is.na(hh_income_state), val, hh_income_state))) %>%
      slice_head(n = 10)
    if (nrow(df) == 0) return(plotly_empty())

    df <- df %>% mutate(state_name = factor(state_name, levels = df$state_name))

    if (yformat == 'count') {
      df <- df %>% mutate(val_plot = ifelse(is.na(val) | val <= 0, 1, val + 1))
      vals_for_ticks <- df$val[!is.na(df$val) & df$val > 0]
      if (length(vals_for_ticks) == 0) {
        tickvals <- c(1,10,100,1000)
      } else {
        minv <- min(vals_for_ticks, na.rm = TRUE)
        maxv <- max(vals_for_ticks, na.rm = TRUE)
        min_exp <- floor(log10(minv))
        max_exp <- ceiling(log10(maxv))
        exps <- seq(min_exp, max_exp)
        tickvals <- 10 ^ exps
      }
      fmt_tick <- function(x) {
        if (x >= 1e6) return(paste0(round(x / 1e6, 1), "M"))
        if (x >= 1e3) return(paste0(round(x / 1e3, 1), "k"))
        return(as.character(round(x)))
      }
      ticktext <- vapply(tickvals, fmt_tick, FUN.VALUE = "")

      p <- plot_ly(df, x = ~state_name, y = ~val_plot, type = 'bar', marker = list(color = '#16a085'),
                   customdata = ~val,
                   hovertemplate = "<b>%{x}</b><br>Value: %{customdata:,.0f}<extra></extra>") %>%
        layout(yaxis = list(title = ytitle, type = 'log', tickmode = 'array', tickvals = tickvals, ticktext = ticktext, showgrid = TRUE),
               xaxis = list(title = ''), margin = list(t = 40, b = 80))
    } else {
      p <- plot_ly(df, x = ~state_name, y = ~val, type = 'bar', marker = list(color = '#16a085')) %>%
        layout(yaxis = list(title = ytitle), xaxis = list(title = ''), margin = list(t = 40, b = 80))
    }

    if (yformat == 'currency') {
      p <- p %>% layout(yaxis = list(title = ytitle, tickprefix = '$'))
    } else if (yformat == 'percent') {
      p <- p %>% layout(yaxis = list(title = ytitle, ticksuffix = '%'))
    }

    p %>% config(displayModeBar = TRUE)
  })

  output$demographics_age_counts <- renderPlotly({
    plotly_empty()
  })

    output$finance_income_by_bedroom_bracket <- renderPlotly({
      df <- filtered_data()
      req(df)

      bracket_cols <- c('pct_lt5k','pct_5k_lt10k','pct_10k_lt15k','pct_15k_lt20k','pct_ge20k')
      brackets <- bracket_cols[bracket_cols %in% names(df)]
      bed_cols <- c('pct_bed1','pct_bed2','pct_bed3','pct_overhoused')
      beds <- bed_cols[bed_cols %in% names(df)]
      if (length(brackets) == 0 || length(beds) == 0) return(plotly_empty())

      units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA

      df2 <- df %>% mutate(across(all_of(union(brackets, beds)), ~ as.numeric(.x)))

      results <- list()
      for (b in brackets) {
        for (bd in beds) {
          if (!is.na(units_col)) {
            val <- sum((df2[[b]]/100) * as.numeric(df2[[units_col]]) * (df2[[bd]]/100), na.rm = TRUE)
          } else {
            val <- sum((df2[[b]]/100) * (df2[[bd]]/100), na.rm = TRUE)
          }
          results[[length(results) + 1]] <- tibble::tibble(bracket = b, bedroom = bd, count = val)
        }
      }

      long <- bind_rows(results)
      bracket_labels <- c(pct_lt5k = '< $5k', pct_5k_lt10k = '$5k-$10k', pct_10k_lt15k = '$10k-$15k', pct_15k_lt20k = '$15k-$20k', pct_ge20k = '>= $20k')
      bed_labels <- c(pct_bed1 = '1 Bedroom Units', pct_bed2 = '2 Bedroom Units', pct_bed3 = '3 Bedroom Units', pct_overhoused = '3+ Bedroom Units')
      long <- long %>% mutate(bracket_label = unname(bracket_labels[bracket]), bedroom_label = unname(bed_labels[bedroom]))

      bedroom_order <- unique(bed_labels[intersect(bed_cols, beds)])
      long$bedroom_label <- factor(long$bedroom_label, levels = bedroom_order)

      palette <- c('< $5k' = '#eff3ff', '$5k-$10k' = '#bdd7e7', '$10k-$15k' = '#6baed6', '$15k-$20k' = '#3182bd', '>= $20k' = '#08519c')

      p <- plot_ly(long, x = ~bedroom_label, y = ~count, color = ~bracket_label, colors = palette, type = 'bar', customdata = ~count,
                   hovertemplate = '<b>%{x}</b><br>%{fullData.name}: %{customdata:,.0f}<extra></extra>') %>%
        layout(barmode = 'stack', xaxis = list(title = 'Bedroom size'), yaxis = list(title = 'Number of Projects'), margin = list(b = 120))

      p %>% config(displayModeBar = TRUE)
    })

  output$finance_bedroom_dist_ui <- renderUI({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) return(tags$div("No data available"))

    pct_cols <- c('pct_bed1','pct_bed2','pct_bed3','pct_overhoused')
    existing <- pct_cols[pct_cols %in% names(df)]
    if (length(existing) == 0) return(tags$div("No bedroom percentage columns present"))

    units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA

    df2 <- df %>% mutate(across(all_of(existing), ~ as.numeric(.x)))
    if (!is.na(units_col)) {
      est <- df2 %>% mutate(across(all_of(existing), ~ (.x/100) * as.numeric(.data[[units_col]]))) %>%
        summarise(across(all_of(existing), ~ sum(.x, na.rm = TRUE)))
      totals <- as.numeric(est[1, ])
      pct_of_total <- totals / sum(totals, na.rm = TRUE) * 100
    } else {
      means <- df2 %>% summarise(across(all_of(existing), ~ mean(.x, na.rm = TRUE)))
      totals <- as.numeric(means[1, ])
      pct_of_total <- totals / sum(totals, na.rm = TRUE) * 100
    }

    labels <- c(pct_bed1 = '1 Bedroom Units', pct_bed2 = '2 Bedroom Units', pct_bed3 = '3 Bedroom Units', pct_overhoused = '3+ Bedroom Units')
    items <- lapply(seq_along(existing), function(i) {
      key <- existing[i]
      lbl <- labels[[key]]
      cnt <- totals[i]
      pct <- round(pct_of_total[i], 1)
      tags$div(style = 'margin-bottom:12px;',
               tags$div(style = 'display:flex; justify-content:space-between; align-items:center;',
                        tags$strong(lbl), tags$span(paste0(scales::comma(round(cnt)), ' (', pct, '%)'))
               ),
               tags$div(class = 'progress', style = 'height:18px;',
                        tags$div(class = 'progress-bar progress-bar-striped progress-bar-animated', role = 'progressbar',
                                 style = paste0('width:', pct, '%; background-color:#2c7fb8;'),
                                 `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100)
               )
      )
    })

    tags$div(items)
  })

  


  compute_income_source_counts <- reactive({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    pct_cols <- c("pct_wage_major", "pct_welfare_major", "pct_other_major")
    existing <- intersect(pct_cols, names(df))
    if (length(existing) == 0) return(NULL)

    total_col <- if ("total_units" %in% names(df)) "total_units" else if ("total_dwelling_units" %in% names(df)) "total_dwelling_units" else NA

    est_vals <- sapply(existing, function(col) {
      if (!is.na(total_col)) {
        pct <- as.numeric(df[[col]])
        units <- as.numeric(df[[total_col]])
        sum(units * (pct/100), na.rm = TRUE)
      } else {
        pct_mean <- mean(as.numeric(df[[col]]), na.rm = TRUE)
        round(pct_mean/100 * nrow(df))
      }
    })

    label_map <- list(
      pct_wage_major = "Wage",
      pct_welfare_major = "Welfare",
      pct_other_major = "Other"
    )

    tib <- tibble::tibble(
      source = unname(sapply(existing, function(x) label_map[[x]])),
      count = as.numeric(est_vals),
      count_bucket = as.numeric(est_vals)
    )

    tib
  })


  compute_finance_aggregates <- reactive({
    df <- filtered_data()
    req(!is.null(df) && nrow(df) > 0)

    safe_num <- function(x) as.numeric(x)

    if ("total_units" %in% names(df)) {
      agg <- df %>%
        dplyr::filter(!is.na(state_name)) %>%
        dplyr::group_by(state_name) %>%
        dplyr::summarise(
          total_units = sum(safe_num(total_units), na.rm = TRUE),
          est_wage = sum(safe_num(total_units) * safe_num(pct_wage_major) / 100, na.rm = TRUE),
          est_welfare = sum(safe_num(total_units) * safe_num(pct_welfare_major) / 100, na.rm = TRUE),
          est_other = sum(safe_num(total_units) * safe_num(pct_other_major) / 100, na.rm = TRUE),
          avg_income = ifelse(total_units > 0, sum(safe_num(total_units) * safe_num(hh_income), na.rm = TRUE) / sum(safe_num(total_units), na.rm = TRUE), NA_real_),
          .groups = "drop"
        )
    } else {
      agg <- df %>%
        dplyr::filter(!is.na(state_name)) %>%
        dplyr::group_by(state_name) %>%
        dplyr::summarise(
          total_units = dplyr::n(),
          est_wage = round(mean(safe_num(pct_wage_major), na.rm = TRUE) / 100 * dplyr::n()),
          est_welfare = round(mean(safe_num(pct_welfare_major), na.rm = TRUE) / 100 * dplyr::n()),
          est_other = round(mean(safe_num(pct_other_major), na.rm = TRUE) / 100 * dplyr::n()),
          avg_income = mean(safe_num(hh_income), na.rm = TRUE),
          .groups = "drop"
        )
    }

    agg <- agg %>% dplyr::mutate(total_est = est_wage + est_welfare + est_other)
    agg
  })
  
  output$income_source_wage_box <- renderValueBox({
    tib <- compute_income_source_counts()
    val <- NA_real_
    if (!is.null(tib) && any(tib$source == "Wage")) val <- tib$count_bucket[tib$source == "Wage"]
    display <- if (is.na(val)) "N/A" else scales::comma(round(val))
    valueBox(display, "Wage Income (Major Source)", icon = icon("briefcase"), color = "olive")
  })

  output$income_source_welfare_box <- renderValueBox({
    tib <- compute_income_source_counts()
    val <- NA_real_
    if (!is.null(tib) && any(tib$source == "Welfare")) val <- tib$count_bucket[tib$source == "Welfare"]
    display <- if (is.na(val)) "N/A" else scales::comma(round(val))
    valueBox(display, "Welfare Income (Major Source)", icon = icon("hand-holding-heart"), color = "teal")
  })

  output$income_source_other_box <- renderValueBox({
    tib <- compute_income_source_counts()
    val <- NA_real_
    if (!is.null(tib) && any(tib$source == "Other")) val <- tib$count_bucket[tib$source == "Other"]
    display <- if (is.na(val)) "N/A" else scales::comma(round(val))
    valueBox(display, "Other Income (Major Source)", icon = icon("users"), color = "maroon")
  })
  output$finance_state_select_ui <- renderUI({
    NULL
  })

  compute_income_summary <- reactive({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA

    hh_count_total <- if (!is.na(units_col)) sum(as.numeric(df[[units_col]]), na.rm = TRUE) else nrow(df)

    hh_income_num <- suppressWarnings(as.numeric(df$hh_income))
    median_income <- NA_real_
    if (!is.na(units_col) && hh_count_total > 0) {
      w <- as.numeric(df[[units_col]])
      ok <- !is.na(hh_income_num) & !is.na(w) & w > 0
      if (sum(ok) > 0) {
        xw <- hh_income_num[ok]
        ww <- w[ok]
        ord <- order(xw)
        x_ord <- xw[ord]
        w_ord <- ww[ord]
        cumw <- cumsum(w_ord) / sum(w_ord)
        idx <- which(cumw >= 0.5)[1]
        median_income <- x_ord[idx]
      } else {
        median_income <- NA_real_
      }
    } else {
      median_income <- median(hh_income_num, na.rm = TRUE)
    }

    is_19k <- !is.na(hh_income_num) & (round(hh_income_num, -3) == 19000)
    if (!is.na(units_col)) {
      hh_count_19k <- sum(as.numeric(df[[units_col]]) * as.numeric(is_19k), na.rm = TRUE)
    } else {
      hh_count_19k <- sum(is_19k, na.rm = TRUE)
    }

    is_ge_18k <- !is.na(hh_income_num) & (hh_income_num >= 18000)
    if (!is.na(units_col)) {
      hh_count_ge_18k <- sum(as.numeric(df[[units_col]]) * as.numeric(is_ge_18k), na.rm = TRUE)
    } else {
      hh_count_ge_18k <- sum(is_ge_18k, na.rm = TRUE)
    }

    list(
      median_income = ifelse(is.nan(median_income), NA_real_, median_income),
      household_count = hh_count_total,
      household_count_19k = hh_count_19k,
      household_count_ge_18k = hh_count_ge_18k
    )
  })

  output$finance_avg_income_box <- renderValueBox({
    s <- compute_income_summary()
    if (is.null(s) || is.na(s$median_income)) {
      valueBox("N/A", "Average household income per year", icon = icon("wallet"), color = "olive")
    } else {
      valueBox(paste0("$", scales::comma(round(s$median_income, 0))), "Average household income per year", icon = icon("wallet"), color = "olive")
    }
  })

  output$finance_household_count_box <- renderValueBox({
    s <- compute_income_summary()
    if (is.null(s) || is.na(s$household_count_ge_18k)) {
      valueBox("N/A", "Households >= $18k", icon = icon("users"), color = "teal")
    } else {
      valueBox(scales::comma(round(s$household_count_ge_18k)), "Households >= $18k", icon = icon("users"), color = "teal")
    }
  })

  output$finance_summary_report <- renderUI({
    s <- compute_income_summary()
    src_tib <- compute_income_source_counts()
    agg <- compute_finance_aggregates()
    df <- filtered_data()

    median_income <- if (!is.null(s) && !is.na(s$median_income)) paste0("$", scales::comma(round(s$median_income, 0))) else "N/A"
    hh_ge_18k <- if (!is.null(s) && !is.na(s$household_count_ge_18k)) scales::comma(round(s$household_count_ge_18k)) else "N/A"

    top_source_text <- "No income source data"
    if (!is.null(src_tib) && nrow(src_tib) > 0) {
      best_idx <- which.max(src_tib$count)
      if (length(best_idx) > 0 && !is.na(src_tib$count[best_idx]) && src_tib$count[best_idx] > 0) {
        src_name <- src_tib$source[best_idx]
        src_count <- scales::comma(round(src_tib$count[best_idx]))
        top_source_text <- paste0(src_name, " (", src_count, " households)")
      }
    }

    top_state_text <- "N/A"
    if (!is.null(agg) && nrow(agg) > 0) {
      ok <- agg %>% filter(!is.na(avg_income))
      if (nrow(ok) > 0) {
        top <- ok %>% arrange(desc(avg_income)) %>% slice_head(n = 1)
        top_state_text <- paste0(top$state_name[1], " ($", scales::comma(round(top$avg_income[1],0)), ")")
      }
    }
    
    state_context <- if (input$state_filter == "all") "across all states" else paste("in", input$state_filter)

    html <- paste0(
      "<div style='font-size: 16px; line-height: 1.6; color: #2c3e50;'>",
      "Financial analysis of public housing ", state_context, " reveals a median household income of <strong>", median_income, "</strong> per year. ",
      "Approximately <strong>", hh_ge_18k, "</strong> households earn $18,000 or more annually. ",
      "The primary source of income for the majority of households is <strong>", top_source_text, "</strong>. ",
      "Among the states analyzed, <strong>", top_state_text, "</strong> has the highest average household income.",
      "</div>"
    )

    HTML(html)
  })

  output$finance_income_bracket_top10 <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(state_name))
    if (nrow(df) == 0) return(plotly_empty())

    seg <- ifelse(is.null(input$finance_segment_by), 'bracket', input$finance_segment_by)

    if (seg == 'source') {
      units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA
      sources <- c('pct_wage_major', 'pct_welfare_major', 'pct_other_major')
      existing_src <- sources[sources %in% names(df)]
      if (length(existing_src) == 0) return(plotly_empty())

      if (!is.na(units_col)) {
        src_agg <- df %>% mutate(across(all_of(existing_src), ~ as.numeric(.x))) %>%
          group_by(state_name) %>%
          summarise(across(all_of(existing_src), ~ sum((.x/100) * as.numeric(.data[[units_col]]), na.rm = TRUE), .names = '{.col}'), .groups = 'drop')
      } else {
        src_agg <- df %>% mutate(across(all_of(existing_src), ~ as.numeric(.x))) %>%
          group_by(state_name) %>%
          summarise(across(all_of(existing_src), ~ sum(.x/100, na.rm = TRUE), .names = '{.col}'), .groups = 'drop')
      }

      hh_by_state <- df %>% group_by(state_name) %>% summarise(hh_income_state = mean(hh_income, na.rm = TRUE), .groups = 'drop')
      topn <- src_agg %>% left_join(hh_by_state, by = 'state_name') %>% arrange(desc(ifelse(is.na(hh_income_state), rowSums(select(., all_of(existing_src)), na.rm = TRUE), hh_income_state))) %>% slice_head(n = 10)
      if (nrow(topn) == 0) return(plotly_empty())

      long <- topn %>% pivot_longer(cols = all_of(existing_src), names_to = 'bracket', values_to = 'count')
      state_levels <- topn$state_name
      long$state_name <- factor(long$state_name, levels = state_levels)

      src_labels <- c(pct_wage_major = 'Wage', pct_welfare_major = 'Welfare', pct_other_major = 'Other')
      bracket_totals <- long %>% group_by(bracket) %>% summarise(total = sum(count, na.rm = TRUE), .groups = 'drop') %>% arrange(desc(total))
      ordered_brackets <- bracket_totals$bracket[bracket_totals$bracket %in% existing_src]
      long$bracket <- factor(long$bracket, levels = ordered_brackets, labels = src_labels[ordered_brackets])

      palette <- c('#2980b9', '#16a085', '#2c3e50')
      colors <- palette[seq_len(length(unique(long$bracket)))]

      p <- plot_ly(long, x = ~state_name, y = ~count, color = ~bracket, colors = colors, type = 'bar', customdata = ~count,
                   hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{customdata:,.0f}<extra></extra>") %>%
        layout(barmode = 'stack', xaxis = list(title = 'State', tickangle = -45),
               yaxis = list(title = 'Number of Projects (Log Scale)', type = 'log'),
               legend = list(title = list(text = 'Income Source')), margin = list(b = 140))

      return(p %>% config(displayModeBar = TRUE))
    }

    pct_cols <- c('pct_lt5k','pct_5k_lt10k','pct_10k_lt15k','pct_15k_lt20k','pct_ge20k')
    existing <- pct_cols[pct_cols %in% names(df)]
    if (length(existing) == 0) return(plotly_empty())

    units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA
    if (!is.na(units_col)) {
      agg <- df %>% mutate(across(all_of(existing), ~ as.numeric(.x))) %>% group_by(state_name) %>% summarise(across(all_of(existing), ~ sum((.x/100) * as.numeric(.data[[units_col]]), na.rm = TRUE), .names = '{.col}'), .groups = 'drop')
    } else {
      agg <- df %>% mutate(across(all_of(existing), ~ as.numeric(.x))) %>% group_by(state_name) %>% summarise(across(all_of(existing), ~ sum(.x/100, na.rm = TRUE), .names = '{.col}'), .groups = 'drop')
    }

    order_col <- if ('pct_ge20k' %in% colnames(agg)) 'pct_ge20k' else existing[1]
    agg2 <- agg %>% filter(!is.na(.data[[order_col]]))
    if (nrow(agg2) == 0) agg2 <- agg
    topn <- agg2 %>% arrange(desc(.data[[order_col]])) %>% slice_head(n = 10)
    if (nrow(topn) == 0) return(plotly_empty())

    long <- topn %>% pivot_longer(cols = all_of(existing), names_to = 'bracket', values_to = 'count')
    state_levels <- topn$state_name
    long$state_name <- factor(long$state_name, levels = state_levels)

    bracket_labels <- c(pct_lt5k = '< $5k', pct_5k_lt10k = '$5k-$10k', pct_10k_lt15k = '$10k-$15k', pct_15k_lt20k = '$15k-$20k', pct_ge20k = '>= $20k')
    bracket_totals <- long %>% group_by(bracket) %>% summarise(total = sum(count, na.rm = TRUE), .groups = 'drop') %>% arrange(desc(total))
    ordered_brackets <- bracket_totals$bracket[bracket_totals$bracket %in% existing]
    long$bracket <- factor(long$bracket, levels = ordered_brackets, labels = bracket_labels[ordered_brackets])

    palette <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#006d2c')
    colors <- palette[seq_len(length(unique(long$bracket)))]

    p <- plot_ly(long, x = ~state_name, y = ~count, color = ~bracket, colors = colors, type = 'bar', customdata = ~count,
                 hovertemplate = '<b>%{x}</b><br>%{fullData.name}: %{customdata:,.0f}<extra></extra>') %>%
      layout(barmode = 'stack', xaxis = list(title = 'State', tickangle = -45),
             yaxis = list(title = 'Number of Projects (Log Scale)', type = 'log'),
             legend = list(title = list(text = 'Income Bracket')), margin = list(b = 140))

    p %>% config(displayModeBar = TRUE)
  })

  
  
  output$rent_histogram <- renderPlotly({
    p <- plot_ly(filtered_data(), x = ~rent_per_month, type = "histogram",
                 nbinsx = 50, marker = list(color = "#3498db", line = list(color = "white", width = 1)),
                 hovertemplate = "<b>Rent:</b> $%{x:,.0f}<br><b>Count:</b> %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Average household rent contribution ($)"),
             yaxis = list(title = "Number of Projects"),
             title = "",
             hovermode = "closest")
    p %>% config(displayModeBar = TRUE, modeBarButtonsToAdd = list("select2d", "lasso2d"))
  })
  
  output$income_histogram <- renderPlotly({
    p <- plot_ly(filtered_data(), x = ~hh_income, type = "histogram",
                 nbinsx = 50, marker = list(color = "#2ecc71", line = list(color = "white", width = 1)),
                 hovertemplate = "<b>Income:</b> $%{x:,.0f}<br><b>Count:</b> %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Average household income per year ($)"),
             yaxis = list(title = "Number of Projects"),
             title = "",
             hovermode = "closest")
    p %>% config(displayModeBar = TRUE, modeBarButtonsToAdd = list("select2d", "lasso2d"))
  })
  
  output$rent_income_scatter <- renderPlotly({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) return(HTML("<em>No data</em>"))

    container_style <- "font-size:12px; line-height:1.15; max-height:360px; overflow-y:auto; padding-right:8px"

    total_units <- if ('total_dwelling_units' %in% names(df)) sum(as.numeric(df$total_dwelling_units), na.rm = TRUE) else nrow(df)
    ssum <- compute_income_summary()
    med_income <- if (!is.null(ssum) && !is.na(ssum$median_income)) round(ssum$median_income, 0) else NA

    src_tib <- compute_income_source_counts()
    if (!is.null(src_tib) && nrow(src_tib) > 0) {
      src_rows <- paste0("<li style='margin-bottom:4px;'><strong>", src_tib$source, ":</strong> ", scales::comma(round(src_tib$count)), "</li>", collapse = "")
      src_html <- paste0("<ul style='margin:6px 0 6px 12px; padding-left:0;'>", src_rows, "</ul>")
    } else {
      src_html <- "<div style='margin:6px 0 6px 0; color:#777;'>No income source data</div>"
    }

    bracket_cols <- c('pct_lt5k','pct_5k_lt10k','pct_10k_lt15k','pct_15k_lt20k','pct_ge20k')
    brackets <- bracket_cols[bracket_cols %in% names(df)]
    bed_cols <- c('pct_bed1','pct_bed2','pct_bed3','pct_overhoused')
    beds <- bed_cols[bed_cols %in% names(df)]

    if (length(brackets) > 0 && length(beds) > 0) {
      units_col <- if ('total_dwelling_units' %in% names(df)) 'total_dwelling_units' else if ('total_units' %in% names(df)) 'total_units' else NA
      df2 <- df %>% mutate(across(all_of(union(brackets, beds)), ~ as.numeric(.x)))

      rows <- lapply(brackets, function(b) {
        if (!is.na(units_col)) {
          hh_in_bracket <- (df2[[b]]/100) * as.numeric(df2[[units_col]])
        } else {
          hh_in_bracket <- (df2[[b]]/100)
        }
        p1 <- if ('pct_bed1' %in% names(df2)) df2[['pct_bed1']]/100 else 0
        p2 <- if ('pct_bed2' %in% names(df2)) df2[['pct_bed2']]/100 else 0
        p3 <- if ('pct_bed3' %in% names(df2)) df2[['pct_bed3']]/100 else 0
        p4 <- if ('pct_overhoused' %in% names(df2)) df2[['pct_overhoused']]/100 else 0
        est_beds <- hh_in_bracket * (1*p1 + 2*p2 + 3*p3 + 4*p4)

        total_hh <- sum(hh_in_bracket, na.rm = TRUE)
        total_beds <- sum(est_beds, na.rm = TRUE)
        avg_beds <- if (total_hh > 0) round(total_beds / total_hh, 2) else NA
        est_hh <- round(total_hh)
        list(bracket = b, est_households = est_hh, avg_beds = avg_beds)
      })

      rows_df <- bind_rows(lapply(rows, as.data.frame))
      label_map <- c(pct_lt5k = '< $5k', pct_5k_lt10k = '$5k-$10k', pct_10k_lt15k = '$10k-$15k', pct_15k_lt20k = '$15k-$20k', pct_ge20k = '>= $20k')
      rows_df$bracket_label <- unname(label_map[rows_df$bracket])

      items_html <- paste0(apply(rows_df, 1, function(r) {
        paste0("<li style='margin-bottom:6px;'><span style='display:inline-block;width:110px;'>", r['bracket_label'], "</span>",
               "<strong>", scales::comma(as.numeric(r['est_households'])), "</strong>",
               " &nbsp; avg beds: <strong>", ifelse(is.na(as.numeric(r['avg_beds'])), 'N/A', as.character(r['avg_beds'])), "</strong></li>")
      }), collapse = "")
      bed_results_html <- paste0("<ul style='margin:6px 0 6px 12px; padding-left:0;'>", items_html, "</ul>")
    } else {
      bed_results_html <- "<div style='color:#777; margin:6px 0;'>No income bracket / bedroom columns available</div>"
    }

    html <- paste0(
      "<div style='", container_style, "'>",
      "<div style='margin-bottom:6px;'><strong>Total units (approx):</strong> ", scales::comma(total_units), "</div>",
      "<div style='margin-bottom:6px;'><strong>Average household income per year:</strong> $", scales::comma(med_income), "</div>",
      "<hr style='margin:8px 0;' />",
      "<div style='margin-bottom:6px;'><strong>Income sources:</strong>", src_html, "</div>",
      "<div style='margin-top:8px;'><strong>By income bracket (est. households & avg bedrooms):</strong>", bed_results_html, "</div>",
      "</div>"
    )

    HTML(html)
  })

  output$demographics_household_by_race <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    req_cols <- c("pct_2adults", "pct_1adult", "pct_black", "pct_hispanic", "pct_asian", "pct_native_american", "pct_minority")
    if (!all(req_cols %in% names(df))) return(NULL)

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    df$units <- as.numeric(units)
    
    df <- df %>% filter(!is.na(units), units > 0)
    if (nrow(df) == 0) return(NULL)

    plot_df <- df %>%
      mutate(
        predominant_race = case_when(
          pct_black >= 50 ~ "Black",
          pct_hispanic >= 50 ~ "Hispanic",
          pct_asian >= 50 ~ "Asian",
          pct_native_american >= 50 ~ "Native Am.",
          TRUE ~ "White"
        )
      ) %>%
      group_by(predominant_race) %>%
      summarise(
        avg_2adults = weighted.mean(pct_2adults, w = units, na.rm = TRUE),
        avg_1adult = weighted.mean(pct_1adult, w = units, na.rm = TRUE),
        count = n()
      ) %>%
      filter(count > 0) %>%
      arrange(predominant_race)

    max_val <- max(c(plot_df$avg_2adults, plot_df$avg_1adult), na.rm = TRUE)
    limit <- ceiling(max_val / 10) * 10 
    if(limit < 10) limit <- 10
    
    plot_ly(plot_df) %>%
      add_trace(x = ~ -avg_1adult, y = ~predominant_race, name = '1 Adult w/ Children', 
                type = 'bar', orientation = 'h', marker = list(color = '#764ba2'),
                text = ~paste0(round(avg_1adult, 1), "%"), hoverinfo = 'text+y+name') %>%
      add_trace(x = ~ avg_2adults, y = ~predominant_race, name = '2 Adults w/ Children', 
                type = 'bar', orientation = 'h', marker = list(color = '#667eea'),
                text = ~paste0(round(avg_2adults, 1), "%"), hoverinfo = 'text+y+name') %>%
      layout(
        barmode = 'relative',
        xaxis = list(
          title = "Average % of Households",
          tickmode = 'array',
          tickvals = seq(-limit, limit, length.out = 5),
          ticktext = abs(seq(-limit, limit, length.out = 5)),
          range = c(-limit * 1.1, limit * 1.1)
        ),
        yaxis = list(title = "", categoryorder = "category ascending"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
        margin = list(t = 10) # Reduce top margin since title is gone
      )
  })

  output$demographics_age_by_race <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    age_cols <- c("pct_lt24_head", "pct_age25_50", "pct_age51_61", "pct_age62plus", "pct_age85plus")
    race_cols <- c("pct_black", "pct_hispanic", "pct_asian", "pct_native_american", "pct_minority")
    if (!all(c(age_cols, race_cols) %in% names(df))) return(NULL)

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    df$units <- as.numeric(units)
    
    df <- df %>% filter(!is.na(units), units > 0)
    if (nrow(df) == 0) return(NULL)

    plot_df <- df %>%
      mutate(
        predominant_race = case_when(
          pct_black >= 50 ~ "Black",
          pct_hispanic >= 50 ~ "Hispanic",
          pct_asian >= 50 ~ "Asian",
          pct_native_american >= 50 ~ "Native Am.",
          TRUE ~ "White"
        )
      ) %>%
      group_by(predominant_race) %>%
      summarise(
        est_lt24 = sum((pct_lt24_head/100) * units, na.rm = TRUE),
        est_25_50 = sum((pct_age25_50/100) * units, na.rm = TRUE),
        est_51_61 = sum((pct_age51_61/100) * units, na.rm = TRUE),
        est_62plus = sum((pct_age62plus/100) * units, na.rm = TRUE),
        est_85plus = sum((pct_age85plus/100) * units, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = starts_with("est_"), names_to = "Age_Group", values_to = "Households") %>%
      mutate(
        Age_Group = case_when(
          Age_Group == "est_lt24" ~ "<= 24",
          Age_Group == "est_25_50" ~ "25-50",
          Age_Group == "est_51_61" ~ "51-61",
          Age_Group == "est_62plus" ~ "62+",
          Age_Group == "est_85plus" ~ "85+",
          TRUE ~ Age_Group
        )
      )

    plot_df <- plot_df %>% mutate(log_val = log10(Households + 1))

    race_colors <- c(
      "Black" = "#2E4053",       # Dark Blue-Grey
      "Hispanic" = "#2874A6",    # Strong Blue
      "Asian" = "#117864",       # Strong Teal
      "Native Am." = "#7D3C98",  # Purple
      "White" = "#1E8449"        # Green
    )
    
    age_colors <- c(
      "<= 24" = "#39cccc",       # Teal
      "25-50" = "#3c8dbc",       # Light Blue
      "51-61" = "#3d9970",       # Olive
      "62+" = "#605ca8",         # Purple
      "85+" = "#001f3f"          # Navy
    )

    leaves <- plot_df %>%
      mutate(
        labels = Age_Group,
        parents = predominant_race,
        ids = paste(predominant_race, Age_Group, sep = " - "),
        value_for_size = log_val,
        real_value = Households,
        color_hex = age_colors[Age_Group] # Use Age Group color
      ) %>%
      select(labels, parents, ids, value_for_size, real_value, color_hex)
      
    parents_agg <- leaves %>%
      group_by(parents) %>%
      summarise(
        value_for_size = sum(value_for_size), 
        real_value = sum(real_value)
      ) %>%
      mutate(
        color_hex = race_colors[parents], # Use Race color
        labels = parents,
        parents = "",
        ids = labels
      )
      
    treemap_data <- bind_rows(parents_agg, leaves)
    
    treemap_data <- treemap_data %>% mutate(
       hover_text = ifelse(parents == "",
           paste0("<b>Race: ", labels, "</b><br>Count: ", scales::comma(round(real_value))),
           paste0("<b>Age: ", labels, "</b><br>Count: ", scales::comma(round(real_value)))
       )
    )

    p <- plot_ly(
      treemap_data,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~value_for_size,
      branchvalues = "total",
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        colors = ~color_hex
      )
    ) 
    
    for(age in names(age_colors)) {
      p <- p %>% add_trace(
        type = 'scatter',
        mode = 'markers',
        x = c(NA), 
        y = c(NA),
        marker = list(color = age_colors[[age]], size = 10),
        name = age,
        showlegend = TRUE,
        hoverinfo = "none",
        inherit = FALSE,
        visible = TRUE # Show in legend active
      )
    }
    
    p %>% layout(
      margin = list(t = 0, b = 0, l = 0, r = 0),
      uniformtext = list(minsize = 10, mode = "hide"),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      legend = list(title = list(text = "Age Group"), orientation = "v")
    )
  })

  output$demographics_pyramid <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    req_cols <- c("pct_1adult", "pct_2adults", "total_dwelling_units")
    if (!all(req_cols %in% names(df))) return(NULL)
    
    category <- input$pyramid_category
    plot_df <- NULL
    y_label <- ""
    
    if (category == "income") {
      if (!"hh_income" %in% names(df)) return(NULL)
      y_label <- "Household Income"
      
      plot_df <- df %>%
        filter(!is.na(hh_income), !is.na(pct_1adult), !is.na(pct_2adults)) %>%
        mutate(
          group_bin = cut(hh_income, 
                           breaks = c(0, 10000, 20000, 30000, 40000, 50000, Inf),
                           labels = c("< $10k", "$10k-20k", "$20k-30k", "$30k-40k", "$40k-50k", "> $50k"),
                           include.lowest = TRUE)
        )
        
    } else if (category == "unit_size") {
      if (!all(c("pct_bed1", "pct_bed2", "pct_bed3") %in% names(df))) return(NULL)
      y_label <- "Predominant Unit Size"
      
      plot_df <- df %>%
        filter(!is.na(pct_bed1), !is.na(pct_bed2), !is.na(pct_bed3)) %>%
        mutate(
          group_bin = case_when(
            pct_bed1 >= pct_bed2 & pct_bed1 >= pct_bed3 ~ "1 Bedroom",
            pct_bed2 >= pct_bed1 & pct_bed2 >= pct_bed3 ~ "2 Bedrooms",
            TRUE ~ "3+ Bedrooms"
          )
        )
        
    } else if (category == "race") {
      race_cols <- c("pct_black", "pct_hispanic", "pct_asian", "pct_native_american")
      if (!all(race_cols %in% names(df))) return(NULL)
      y_label <- "Predominant Race"
      
      plot_df <- df %>%
        filter(!is.na(pct_black)) %>%
        mutate(
          group_bin = case_when(
            pct_black >= 50 ~ "Black",
            pct_hispanic >= 50 ~ "Hispanic",
            pct_asian >= 50 ~ "Asian",
            pct_native_american >= 50 ~ "Native Am.",
            TRUE ~ "White/Other"
          )
        )
        
    } else if (category == "age") {
      age_cols <- c("pct_lt24_head", "pct_age25_50", "pct_age51_61", "pct_age62plus")
      if (!all(age_cols %in% names(df))) return(NULL)
      y_label <- "Head of Household Age"
      
      plot_df <- df %>%
        filter(!is.na(pct_lt24_head)) %>%
        mutate(
          group_bin = case_when(
            pct_lt24_head >= 50 ~ "< 24 Years",
            pct_age25_50 >= 50 ~ "25-50 Years",
            pct_age51_61 >= 50 ~ "51-61 Years",
            pct_age62plus >= 50 ~ "62+ Years",
            TRUE ~ "Mixed Ages"
          )
        )
    }
    
    if (is.null(plot_df)) return(NULL)
    
    summary_df <- plot_df %>%
      group_by(group_bin) %>%
      summarise(
        pct_1_adult = weighted.mean(pct_1adult, w = total_dwelling_units, na.rm = TRUE),
        pct_2_adults = weighted.mean(pct_2adults, w = total_dwelling_units, na.rm = TRUE)
      ) %>%
      filter(!is.na(group_bin))
    
    summary_df$pct_1_adult_neg <- -1 * summary_df$pct_1_adult
    
    max_val <- max(c(summary_df$pct_1_adult, summary_df$pct_2_adults), na.rm = TRUE)
    limit <- ceiling(max_val / 10) * 10 
    if(limit < 10) limit <- 10
    
    color_1_adult <- '#9b59b6' # Purple
    color_2_adults <- '#1abc9c' # Teal
    
    plot_ly(summary_df) %>%
      add_trace(x = ~pct_1_adult_neg, y = ~group_bin, type = 'bar', orientation = 'h', 
                name = '1 Adult', marker = list(color = color_1_adult),
                text = ~paste0(round(pct_1_adult, 1), "%"), textposition = "auto",
                hoverinfo = "text", hovertext = ~paste(y_label, ":", group_bin, "<br>1 Adult:", round(pct_1_adult, 1), "%")) %>%
      add_trace(x = ~pct_2_adults, y = ~group_bin, type = 'bar', orientation = 'h', 
                name = '2 Adults', marker = list(color = color_2_adults),
                text = ~paste0(round(pct_2_adults, 1), "%"), textposition = "auto",
                hoverinfo = "text", hovertext = ~paste(y_label, ":", group_bin, "<br>2 Adults:", round(pct_2_adults, 1), "%")) %>%
      layout(
        barmode = 'overlay',
        xaxis = list(
          title = "Percentage of Households",
          tickmode = 'array',
          tickvals = seq(-limit, limit, length.out = 5),
          ticktext = abs(seq(-limit, limit, length.out = 5)),
          range = c(-limit * 1.1, limit * 1.1)
        ),
        yaxis = list(title = y_label),
        legend = list(x = 0.5, y = 1.1, orientation = 'h', xanchor = "center"),
        margin = list(l = 100, r = 20, t = 30, b = 50)
      )
  })

  output$demographics_household_by_age <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    req_cols <- c("pct_2adults", "pct_1adult", "pct_lt24_head", "pct_age25_50", "pct_age51_61", "pct_age62plus")
    if (!all(req_cols %in% names(df))) return(NULL)

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    df$units <- as.numeric(units)
    
    df <- df %>% filter(!is.na(units), units > 0)
    if (nrow(df) == 0) return(NULL)

    plot_df <- df %>%
      mutate(
        predominant_age = case_when(
          pct_lt24_head >= 50 ~ "Head Age < 24",
          pct_age25_50 >= 50 ~ "Head Age 25-50",
          pct_age51_61 >= 50 ~ "Head Age 51-61",
          pct_age62plus >= 50 ~ "Head Age 62+",
          TRUE ~ "Mixed/No Majority"
        )
      ) %>%
      group_by(predominant_age) %>%
      summarise(
        avg_2adults = weighted.mean(pct_2adults, w = units, na.rm = TRUE),
        avg_1adult = weighted.mean(pct_1adult, w = units, na.rm = TRUE),
        count = n()
      ) %>%
      filter(count > 0) %>%
      arrange(predominant_age)

    max_val <- max(c(plot_df$avg_2adults, plot_df$avg_1adult), na.rm = TRUE)
    limit <- ceiling(max_val / 10) * 10 
    if(limit < 10) limit <- 10
    
    plot_ly(plot_df) %>%
      add_trace(x = ~ -avg_1adult, y = ~predominant_age, name = '% 1 adult with children', 
                type = 'bar', orientation = 'h', marker = list(color = '#764ba2'),
                text = ~paste0(round(avg_1adult, 1), "%"), hoverinfo = 'text+y+name') %>%
      add_trace(x = ~ avg_2adults, y = ~predominant_age, name = '% 2+ adults with children', 
                type = 'bar', orientation = 'h', marker = list(color = '#667eea'),
                text = ~paste0(round(avg_2adults, 1), "%"), hoverinfo = 'text+y+name') %>%
      layout(
        barmode = 'relative',
        xaxis = list(
          title = "Average % of Households",
          tickmode = 'array',
          tickvals = seq(-limit, limit, length.out = 5),
          ticktext = abs(seq(-limit, limit, length.out = 5)),
          range = c(-limit * 1.1, limit * 1.1)
        ),
        yaxis = list(title = "", categoryorder = "category ascending"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1),
        margin = list(t = 10)
      )
  })

  output$demographics_head_age_dist <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    req_cols <- c("pct_lt24_head", "pct_age25_50", "pct_age51_61", "pct_age62plus")
    if (!all(req_cols %in% names(df))) return(NULL)

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    df$units <- as.numeric(units)
    
    df <- df %>% filter(!is.na(units), units > 0)
    if (nrow(df) == 0) return(NULL)
    
    avg_lt24 <- weighted.mean(df$pct_lt24_head, w = df$units, na.rm = TRUE)
    avg_25_50 <- weighted.mean(df$pct_age25_50, w = df$units, na.rm = TRUE)
    avg_51_61 <- weighted.mean(df$pct_age51_61, w = df$units, na.rm = TRUE)
    avg_62plus <- weighted.mean(df$pct_age62plus, w = df$units, na.rm = TRUE)
    
    plot_data <- data.frame(
      Age_Group = factor(c("< 24", "25-50", "51-61", "62+"), levels = c("< 24", "25-50", "51-61", "62+")),
      Percentage = c(avg_lt24, avg_25_50, avg_51_61, avg_62plus)
    )
    
    bar_colors <- c('#3498db', '#2980b9', '#16a085', '#27ae60')
    
    plot_ly(plot_data, x = ~Age_Group, y = ~Percentage, type = 'bar',
            marker = list(color = bar_colors),
            text = ~paste0(round(Percentage, 1), "%"),
            textposition = 'auto',
            hoverinfo = 'x+y') %>%
      layout(
        xaxis = list(title = "Age Bracket"),
        yaxis = list(title = "Average % of Households"),
        margin = list(t = 10)
      )
  })

  output$demographics_summary_report <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(HTML("<em>No data available for the selected filters.</em>"))

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    units <- as.numeric(units)
    valid <- !is.na(units) & units > 0
    df <- df[valid, ]
    units <- units[valid]
    
    if (nrow(df) == 0) return(HTML("<em>No valid unit data available.</em>"))

    w_mean <- function(col) {
      if (!col %in% names(df)) return(NA)
      weighted.mean(as.numeric(df[[col]]), w = units, na.rm = TRUE)
    }

    total_hh <- sum(units, na.rm = TRUE)
    pct_minority <- w_mean("pct_minority")
    pct_black <- w_mean("pct_black")
    pct_hispanic <- w_mean("pct_hispanic")
    pct_female_head <- w_mean("pct_female_head")
    pct_female_head_child <- w_mean("pct_female_head_child")
    pct_disabled_lt62 <- w_mean("pct_disabled_lt62")
    pct_disabled_ge62 <- w_mean("pct_disabled_ge62")
    pct_elderly <- w_mean("eldly_prcnt") # Assuming this column exists based on previous reads
    avg_hh_size <- w_mean("people_per_unit")
    
    fmt_pct <- function(x) if (is.na(x)) "N/A" else paste0(round(x, 1), "%")
    fmt_num <- function(x) if (is.na(x)) "N/A" else scales::comma(round(x))
    fmt_dec <- function(x) if (is.na(x)) "N/A" else round(x, 2)

    html <- paste0(
      "<div style='font-size: 15px; line-height: 1.6; color: #2c3e50;'>",
      
      "<div style='display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 20px;'>",
        "<div style='flex: 1; min-width: 200px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #667eea;'>",
          "<h4 style='margin-top:0; color: #667eea;'>Population Scale</h4>",
          "<strong>Total Households:</strong> ", fmt_num(total_hh), "<br>",
          "<strong>Avg Household Size:</strong> ", fmt_dec(avg_hh_size), " people",
        "</div>",
        
        "<div style='flex: 1; min-width: 200px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #e67e22;'>",
          "<h4 style='margin-top:0; color: #e67e22;'>Racial Composition</h4>",
          "<strong>Minority Households:</strong> ", fmt_pct(pct_minority), "<br>",
          "<span style='color:#7f8c8d; font-size:0.9em;'>(Black: ", fmt_pct(pct_black), ", Hispanic: ", fmt_pct(pct_hispanic), ")</span>",
        "</div>",
        
        "<div style='flex: 1; min-width: 200px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #16a085;'>",
          "<h4 style='margin-top:0; color: #16a085;'>Vulnerable Groups</h4>",
          "<strong>Female Headed:</strong> ", fmt_pct(pct_female_head), " <span style='font-size:0.9em;'>(w/ children: ", fmt_pct(pct_female_head_child), ")</span><br>",
          "<strong>Elderly Members:</strong> ", fmt_pct(pct_elderly), "<br>",
          "<strong>Disabled (<62):</strong> ", fmt_pct(pct_disabled_lt62),
        "</div>",
      "</div>",
      
      "<p><strong>Summary Analysis:</strong> Across the selected area, the public housing population is predominantly <b>", 
      ifelse(pct_minority > 50, "minority", "non-minority"), "</b> (", fmt_pct(pct_minority), "). ",
      "A significant portion of households are headed by women (", fmt_pct(pct_female_head), "), ",
      "and ", fmt_pct(pct_female_head_child), " of all households are female-headed families with children. ",
      "Disability prevalence among non-elderly residents is ", fmt_pct(pct_disabled_lt62), ", highlighting the need for accessible housing units and supportive services.</p>",
      
      "</div>"
    )
    
    HTML(html)
  })

  output$demographics_household_donut <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    if (!all(c("pct_2adults", "pct_1adult") %in% names(df))) {
      return(NULL)
    }

    units <- if("total_occupied" %in% names(df)) df$total_occupied else df$total_dwelling_units
    units <- as.numeric(units)
    
    count_2adults <- sum((as.numeric(df$pct_2adults) / 100) * units, na.rm = TRUE)
    count_1adult <- sum((as.numeric(df$pct_1adult) / 100) * units, na.rm = TRUE)
    
    total_visible <- count_2adults + count_1adult
    
    if (total_visible == 0) return(NULL)

    pct_2a <- count_2adults / total_visible
    pct_1a <- count_1adult / total_visible
    
    labels <- c("% 2+ adults with children", "% 1 adult with children", "")
    values <- c(count_2adults, count_1adult, total_visible)
    
    colors <- c('#1abc9c', '#9b59b6', 'rgba(255,255,255,0)') 
    
    text_labels <- c(
      paste0("2 Adults<br>", scales::percent(pct_2a, accuracy = 1)),
      paste0("1 Adult<br>", scales::percent(pct_1a, accuracy = 1)),
      ""
    )
    
    plot_ly(
      labels = labels, 
      values = values, 
      type = 'pie', 
      hole = 0.6,
      rotation = 270,       # Start at 9 o'clock
      direction = "clockwise",
      sort = FALSE,         # Preserve order
      text = text_labels,
      textinfo = 'text',
      textposition = 'inside',
      hoverinfo = 'label+value',
      marker = list(colors = colors, line = list(color = 'white', width = 2))
    ) %>%
      layout(
        title = list(text = "Households with Children", y = 0.05, font = list(size = 14, color = "#7f8c8d")),
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  
  # --- Statistics Page Logic ---
  
  # Variable Labels Helper
  var_labels <- c(
    "rent_per_month" = "Monthly Rent ($)",
    "hh_income" = "Household Income ($)",
    "pct_lt30_median" = "Poverty Rate (%)",
    "pct_bed1" = "1-Bedroom Units (%)",
    "pct_bed2" = "2-Bedroom Units (%)",
    "pct_bed3" = "3+ Bedroom Units (%)",
    "pct_overhoused" = "Overhoused Units (%)",
    "pct_occupied" = "Occupancy Rate (%)",
    "pct_minority" = "Minority Population (%)",
    "people_per_unit" = "Avg Household Size",
    "pct_wage_major" = "Income from Wages (%)",
    "pct_welfare_major" = "Income from Welfare (%)",
    "pct_other_major" = "Other Income Sources (%)",
    "pct_female_head_child" = "Female Head w/ Child (%)",
    "spending_per_month" = "Fed Spending/Unit ($)",
    "total_dwelling_units" = "Total Units",
    "pct_disabled_all" = "Disabled Population (%)",
    "annl_expns_amnt_prev_yr" = "Annual Expenses (Prev Yr)",
    "months_waiting" = "Wait Time (Months)",
    "place_level" = "Location Index"
  )
  
  get_label <- function(var) {
    if (var %in% names(var_labels)) return(var_labels[[var]])
    return(var)
  }

  # Reactive dataset for statistics
  stats_data <- reactive({
    df <- filtered_data()
    req(nrow(df) > 10)
    
    # Ensure columns exist and are numeric
    cols_needed <- c(
      'rent_per_month', 'pct_bed1', 'pct_bed2', 'pct_bed3', 'pct_overhoused', 'hh_income',
      'pct_lt30_median', 'pct_occupied', 'pct_minority', 'people_per_unit',
      'pct_wage_major', 'pct_welfare_major', 'pct_other_major',
      'pct_female_head_child', 'spending_per_month', 'total_dwelling_units',
      'pct_disabled_all', 'annl_expns_amnt_prev_yr', 'months_waiting',
      'place_level'
    )
    
    # Check which columns are actually in the dataset
    available_cols <- intersect(cols_needed, names(df))
    
    df_clean <- df %>%
      select(all_of(available_cols)) %>%
      mutate(across(everything(), ~suppressWarnings(as.numeric(.)))) %>%
      filter(complete.cases(.))
      
    df_clean
  })

  # Selected Model Logic
  selected_model <- reactive({
    df <- stats_data()
    req(nrow(df) > 0)
    model_id <- input$stats_model_select
    
    fit <- NULL
    formula_str <- ""
    
    if (model_id == "R1") {
      # Added pct_overhoused to be more comprehensive about unit usage
      formula_str <- "rent_per_month ~ pct_bed1 + pct_bed2 + pct_bed3 + pct_overhoused"
    } else if (model_id == "R2") {
      formula_str <- "rent_per_month ~ hh_income + pct_lt30_median + pct_bed1 + pct_bed2 + pct_bed3 + pct_overhoused"
    } else if (model_id == "O1") {
      formula_str <- "pct_occupied ~ pct_minority + rent_per_month + pct_bed1 + pct_bed2 + pct_bed3"
    } else if (model_id == "I1") {
      formula_str <- "hh_income ~ people_per_unit + pct_wage_major + pct_welfare_major + pct_other_major + pct_female_head_child"
    } else if (model_id == "L1") {
      formula_str <- "spending_per_month ~ total_dwelling_units + pct_occupied + pct_disabled_all + annl_expns_amnt_prev_yr"
    } else if (model_id == "L2") {
      formula_str <- "months_waiting ~ rent_per_month + pct_occupied + hh_income + place_level"
    }
    
    # Check if all vars in formula are in df
    vars <- all.vars(as.formula(formula_str))
    if (!all(vars %in% names(df))) {
      return(list(fit = NULL, error = paste("Missing columns for model:", paste(setdiff(vars, names(df)), collapse=", "))))
    }
    
    fit <- lm(as.formula(formula_str), data = df)
    list(fit = fit, formula = formula_str, error = NULL)
  })

  # Render Coefficient Table (Replaces Plot)
  output$stats_coef_table <- renderDT({
    mod <- selected_model()
    req(mod$fit)
    
    # Extract coefficients
    coefs <- summary(mod$fit)$coefficients
    df_coefs <- as.data.frame(coefs)
    df_coefs$Variable <- rownames(coefs)
    
    # Clean variable names
    df_coefs$Variable <- sapply(df_coefs$Variable, get_label)
    
    # Select and rename columns
    df_coefs <- df_coefs %>%
      select(Variable, Estimate, `Pr(>|t|)`) %>%
      rename(
        `Impact (Estimate)` = Estimate,
        `Significance (P-Value)` = `Pr(>|t|)`
      )
    
    # Add interpretation column
    df_coefs$Conclusion <- case_when(
      df_coefs$`Significance (P-Value)` < 0.001 ~ "Highly Significant",
      df_coefs$`Significance (P-Value)` < 0.05 ~ "Significant",
      df_coefs$`Significance (P-Value)` < 0.1 ~ "Marginal",
      TRUE ~ "Not Significant"
    )
    
    datatable(df_coefs, 
              options = list(
                dom = 't', 
                pageLength = 15, 
                scrollX = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
                  "}"
                )
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover') %>%
      formatRound(columns = c('Impact (Estimate)'), digits = 2) %>%
      formatSignif(columns = c('Significance (P-Value)'), digits = 3) %>%
      formatStyle(
        'Conclusion',
        color = styleEqual(
          c("Highly Significant", "Significant", "Marginal", "Not Significant"), 
          c("green", "green", "orange", "gray")
        ),
        fontWeight = styleEqual(
          c("Highly Significant", "Significant"), 
          c("bold", "bold")
        )
      )
  })

  # Value Box: R-Squared
  output$stats_r2_box <- renderValueBox({
    mod <- selected_model()
    if (is.null(mod$fit)) return(valueBox("N/A", "Model Fit", icon = icon("chart-bar"), color = "red"))
    
    r2 <- summary(mod$fit)$r.squared
    color <- if (r2 > 0.5) "green" else if (r2 > 0.3) "orange" else "red"
    
    valueBox(
      paste0(round(r2 * 100, 1), "%"),
      "Explained Variance (R²)",
      icon = icon("percentage"),
      color = color
    )
  })

  # Value Box: P-Value
  output$stats_p_box <- renderValueBox({
    mod <- selected_model()
    if (is.null(mod$fit)) return(valueBox("N/A", "Significance", icon = icon("question"), color = "red"))
    
    f <- summary(mod$fit)$fstatistic
    p_val <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    
    label <- if (p_val < 0.001) "< 0.001" else round(p_val, 3)
    color <- if (p_val < 0.05) "green" else "red"
    
    valueBox(
      label,
      "Model Significance (P-Value)",
      icon = icon("check-circle"),
      color = color
    )
  })

  # Value Box: Key Driver
  output$stats_driver_box <- renderValueBox({
    mod <- selected_model()
    if (is.null(mod$fit)) return(valueBox("N/A", "Key Driver", icon = icon("key"), color = "blue"))
    
    coefs <- summary(mod$fit)$coefficients
    # Exclude intercept
    if (nrow(coefs) > 1) {
      coefs <- coefs[-1, , drop = FALSE]
      # Find max absolute t-value
      top_idx <- which.max(abs(coefs[, "t value"]))
      top_var <- rownames(coefs)[top_idx]
      # Clean variable name (remove backticks if any)
      top_var <- gsub("`", "", top_var)
      label <- get_label(top_var)
    } else {
      label <- "None"
    }
    
    valueBox(
      label,
      "Strongest Predictor",
      icon = icon("arrow-up"),
      color = "blue"
    )
  })

  # Model Summary Output
  output$stats_model_summary <- renderPrint({
    mod <- selected_model()
    req(mod$fit)
    summary(mod$fit)
  })
  
  # Model Interpretation
  output$stats_model_interpretation <- renderUI({
    mod <- selected_model()
    req(mod$fit)
    
    s <- summary(mod$fit)
    r2 <- s$r.squared
    
    model_id <- input$stats_model_select
    
    title_text <- ""
    body_text <- ""
    
    if (model_id == "R2") {
      title_text <- "Strong Predictor of Rent"
      body_text <- "Our analysis shows that <b>Household Income</b> and <b>Poverty Levels</b> are the most important factors in determining rent. This confirms that public housing rent is heavily tied to how much a family earns."
    } else if (model_id == "I1") {
      title_text <- "Income Drivers Identified"
      body_text <- "We found that <b>Household Size</b> and the <b>Source of Income</b> (Wages vs. Welfare) strongly predict total household income. Larger families and those relying on wages tend to have higher reported incomes."
    } else if (model_id == "O1") {
      title_text <- "Occupancy is Complex"
      body_text <- "Only about 30% of the variation in occupancy rates can be explained by rent and demographics. This suggests that <b>Location</b> and <b>Building Condition</b> (which are not in this model) likely play a huge role in whether units are full."
    } else if (model_id == "R1") {
      title_text <- "Bedroom Size Matters, but..."
      body_text <- "While larger apartments do rent for more, bedroom size alone only explains about 40% of the rent price. Income is a much stronger predictor than just the size of the unit."
    } else if (model_id %in% c("L1", "L2")) {
      title_text <- "Weak Relationship Found"
      body_text <- "The data does <b>not</b> show a strong link here. Factors like 'Wait Times' and 'Project Spending' are likely driven by complex administrative decisions or local policies that aren't captured in this dataset."
    }
    
    HTML(paste0(
      "<div style='font-size: 15px; line-height: 1.6;'>",
      "<h4 style='margin-top: 0; color: #2c3e50;'>", title_text, "</h4>",
      "<p>", body_text, "</p>",
      "</div>"
    ))
  })

  output$data_table_output <- renderDT({
    display_data <- filtered_data() %>%
      select(
        state_name, county_name, city_name, project_name, total_dwelling_units, vacancy_rate,
        people_per_unit, people_total, pct_2adults, pct_1adult, pct_female_head, pct_female_head_child,
        pct_disabled_lt62, pct_disabled_ge62, pct_disabled_all,
        pct_lt24_head, pct_age25_50, pct_age51_61, pct_age62plus, pct_age85plus,
        pct_minority, pct_black, pct_native_american, pct_asian, pct_hispanic,
        chldrn_mbr_cnt, eldly_prcnt,
        hh_income, person_income, pct_lt5k, pct_5k_lt10k, pct_10k_lt15k, pct_15k_lt20k, pct_ge20k,
        pct_wage_major, pct_welfare_major, pct_other_major,
        pct_median, pct_lt50_median, pct_lt30_median, pct_lt80_median, median_inc_amnt,
        pct_bed1, pct_bed2, pct_bed3, pct_overhoused,
        rent_per_month, spending_per_month, spending_per_month_prev_yr, ave_util_allow, pct_utility_allow
      ) %>%
      rename(
        State = state_name,
        County = county_name,
        City = city_name,
        Project = project_name,
        Units = total_dwelling_units,
        Vacancy_Rate = vacancy_rate,
        
        Avg_HH_Size = people_per_unit,
        Total_People = people_total,
        Pct_2_Parents = pct_2adults,
        Pct_1_Parent = pct_1adult,
        Pct_Female_Head = pct_female_head,
        Pct_Fem_Head_Child = pct_female_head_child,
        Pct_Dis_Lt62 = pct_disabled_lt62,
        Pct_Dis_Ge62 = pct_disabled_ge62,
        Pct_Dis_All = pct_disabled_all,
        Pct_Head_Lt24 = pct_lt24_head,
        Pct_Head_25_50 = pct_age25_50,
        Pct_Head_51_61 = pct_age51_61,
        Pct_Head_62Plus = pct_age62plus,
        Pct_Head_85Plus = pct_age85plus,
        Pct_Minority = pct_minority,
        Pct_Black = pct_black,
        Pct_Native_Am = pct_native_american,
        Pct_Asian = pct_asian,
        Pct_Hispanic = pct_hispanic,
        Total_Children = chldrn_mbr_cnt,
        Pct_Elderly_Mbrs = eldly_prcnt,
        
        Avg_HH_Income = hh_income,
        Avg_Person_Inc = person_income,
        Pct_Inc_Lt5k = pct_lt5k,
        Pct_Inc_5_10k = pct_5k_lt10k,
        Pct_Inc_10_15k = pct_10k_lt15k,
        Pct_Inc_15_20k = pct_15k_lt20k,
        Pct_Inc_Ge20k = pct_ge20k,
        Pct_Wage_Major = pct_wage_major,
        Pct_Welfare_Major = pct_welfare_major,
        Pct_Other_Major = pct_other_major,
        Pct_Median_Inc = pct_median,
        Pct_Lt50_Median = pct_lt50_median,
        Pct_Lt30_Median = pct_lt30_median,
        Pct_Lt80_Median = pct_lt80_median,
        Median_Inc_Amt = median_inc_amnt,
        
        Pct_0_1_Bed = pct_bed1,
        Pct_2_Bed = pct_bed2,
        Pct_3Plus_Bed = pct_bed3,
        Pct_Overhoused = pct_overhoused,
        
        Avg_Rent = rent_per_month,
        Fed_Spend_Unit = spending_per_month,
        Fed_Spend_Prev = spending_per_month_prev_yr,
        Avg_Util_Allow = ave_util_allow,
        Pct_Util_Allow = pct_utility_allow
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      filter = "top"
    ) %>%
      formatCurrency(c("Avg_Rent", "Avg_HH_Income", "Avg_Person_Inc", "Median_Inc_Amt", "Fed_Spend_Unit", "Fed_Spend_Prev", "Avg_Util_Allow"), digits = 0) %>%
      formatRound(c("Vacancy_Rate", "Avg_HH_Size", "Pct_2_Parents", "Pct_1_Parent", "Pct_Female_Head", "Pct_Fem_Head_Child",
                    "Pct_Dis_Lt62", "Pct_Dis_Ge62", "Pct_Dis_All", "Pct_Head_Lt24", "Pct_Head_25_50", "Pct_Head_51_61", "Pct_Head_62Plus", "Pct_Head_85Plus",
                    "Pct_Minority", "Pct_Black", "Pct_Native_Am", "Pct_Asian", "Pct_Hispanic", "Pct_Elderly_Mbrs",
                    "Pct_Inc_Lt5k", "Pct_Inc_5_10k", "Pct_Inc_10_15k", "Pct_Inc_15_20k", "Pct_Inc_Ge20k",
                    "Pct_Wage_Major", "Pct_Welfare_Major", "Pct_Other_Major", "Pct_Median_Inc", "Pct_Lt50_Median", "Pct_Lt30_Median", "Pct_Lt80_Median",
                    "Pct_0_1_Bed", "Pct_2_Bed", "Pct_3Plus_Bed", "Pct_Overhoused", "Pct_Util_Allow"), digits = 1) %>%
      formatRound(c("Units", "Total_People", "Total_Children"), digits = 0)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("housing_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}


shinyApp(ui, server)

