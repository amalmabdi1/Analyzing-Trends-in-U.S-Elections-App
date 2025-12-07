library(reader)
library(dplyr)

anes_df <- read_csv("../data_raw/anes_timeseries_cdf_csv_20220916.csv")


# subset df with relevant variables
anes_subset <- anes_df |>
  select(
    year = VCF0004,
    respondent_id = VCF0006,
    cross_year_id = VCF0006a,
    state = VCF0901b,
    age_group = VCF0102,
    gender = VCF0104,
    race = VCF0105a,
    education = VCF0110,
    income = VCF0114,
    urban_rural = VCF0111,
    census_region = VCF0112,
    party_id = VCF0303,
    turnout = VCF0706,
    pres_vote = VCF0705
  )


## Re-coding values into labels 

# mapping state abb. to full state names

state_names <- data.frame(
  state_abb = state.abb,
  state_name = state.name
)

anes_clean_subset <- anes_subset |>
  filter(year >= 1974) |>
  left_join(state_names, by = c("state" = "state_abb")) |>
  mutate(
    party_id = factor(
      case_when(
        party_id == 1 ~ "Strong Democrat",
        party_id == 2 ~ "Weak Democrat",
        party_id == 3 ~ "Ind-Democrat",
        party_id == 4 ~ "Independent",
        party_id == 5 ~ "Ind-Republican",
        party_id == 6 ~ "Weak Republican",
        party_id == 7 ~ "Strong Republican")),
    
    census_region = factor(
      case_when(
        census_region == 1 ~ "Northeast",
        census_region == 2 ~ "North Central",
        census_region == 3 ~ "South",
        census_region == 4 ~ "West")),
    
    urban_rural = factor(
      case_when(
        urban_rural == 1 ~ "City",
        urban_rural == 2 ~ "Suburban area",
        urban_rural == 3 ~ "Rural area")),
    
    age_group = factor(
      case_when(
        age_group == 1 ~ "17-24",
        age_group == 2 ~ "25-34",
        age_group == 3 ~ "35-44",
        age_group == 4 ~ "45-54",
        age_group == 5 ~ "55-64",
        age_group == 6 ~ "65-74",
        age_group == 7 ~ "75+"),
      ordered = TRUE,
      levels = c("17-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")),
    
    gender = factor(
      case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        gender == 3 ~ "Other")),
    
    race = factor(
      case_when(
        race == 1 ~ "White",
        race == 2 ~ "Black",
        race == 3 ~ "Asian/Pacific lander",
        race == 4 ~ "American Indian/Alaska Native",
        race == 5 ~ "Hispanic",
        race == 6 ~ "Other",
        race == 7 ~ "Non-white Non-black")),
    
    education = factor(
      case_when(
        education == 1 ~ "Less than high school",
        education == 2 ~ "High school",
        education == 3 ~ "Some college",
        education == 4 ~ "College or more"),
      ordered = TRUE,
      levels = c("Less than high school", "High school", "Some college", "College or more")),
    
    income = factor(
      case_when(
        income == 1 ~ "0-16 percentile",
        income == 2 ~ "17-33 percentile",
        income == 3 ~ "34-67 percentile",
        income == 4 ~ "68-95 percentile",
        income == 5 ~ "96-100 percentile"),
      ordered = TRUE,
      levels = c("0-16 percentile", "17-33 percentile", "34-67 percentile", "68-95 percentile","96-100 percentile")),
    
    turnout = factor(
      case_when(
        turnout == 1 ~ "Voted",
        turnout == 0 ~ "Did not vote")),
    
    pres_vote = factor(
      case_when(
        pres_vote == 1 ~ "Democrat",
        pres_vote == 2 ~ "Republican",
        pres_vote == 3 ~ "Other"))) |>
  
  select(-state)

# Read in data 

govener_df <- read_excel("../data_raw/StateElections_Gub_2012_09_06_Public_Version.xlsx")

# subset data 
govener_subset <- govener_df |>
  select(
    state,
    year,
    govparty_a,        
    years_served,      
    term_length,
    femgov,            
    gub_election,     
    gub_election_regime
  )

head(govener_subset)

# Clean and recode 
govener_clean_sub <- govener_subset |>
  filter(year >= 1974) |>
  mutate(
    govparty = factor(case_when(
      govparty_a == 0 ~ "Republican",
      govparty_a == 1 ~ "Democrat",
      govparty_a == .5 ~ "Non-major party",
      TRUE ~ NA_character_)),
    
    femgov = case_when(
      is.na(femgov) ~ NA_character_,
      femgov == 0   ~ "Male",
      femgov > 0    ~ "Female"
    ),
    
    term_length = factor(term_length,
                         levels = c(2, 4),
                         labels = c("2-year term", "4-year term")),
    gub_election = factor(gub_election,
                          levels = c(0, 1),
                          labels = c("No election", "Election held"))
  ) |>
  rename(
    governor_party = govparty,
    next_gub_election_year = gub_election_regime
  )

head(govener_clean_sub)

govener_clean_sub |>
  group_by(state,year) |>
  summarize(n = n())


# these will need to be played with once other is cleaned 

year_choices <- unique(govener_clean_sub$year) |> 
  sort()

party_choices <- c("Democrat", "Republican", "None-major party")

## Begin User Interface Section ----------------
  
  # ===== TAB 1: U.S. Election Map ===================================
ui <- fluidPage(
  titlePanel("Progress Report Analyzing Trends in U.S Elections (1976-2020))",
             tabsetPanel(
               tabPanel("National Map Explorer",
                        sidebarLayout(
                            sidebarPanel(
                              h4("Map Controls"),
                              
                              selectInput(
                                "year",
                                "Select Year: ",
                                choices = year_choices),
                              
                              
                              
                              selectInput(
                                "demo_overlay",
                                "Demographic Info:",
                                choices = c("urban_rural", "age_group", "gender", "race", "education","income")),
                              
                            ),   
                            
                            mainPanel(
                              plotOutput("us_map", height = "500px"),
                              tableOutput("map_summary")
                            )
                          )
                        ),
  
  # ===== TAB 2: Multivariable Analysis ===============================
  #leaving space for the other datasets by using filler dummy variables
  tabPanel(
    "Multivariable Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Multivariable Controls"),
        
        selectInput(
          "multi_level",
          "Election level:",
          choices = election_level_choices
        ),
        
        sliderInput(
          "multi_year_range",
          "Year range:",
          min = min(year_choices),
          max = max(year_choices),
          value = c(2000, max(year_choices)),
          step = 1,
          sep = ""
        ),
        
        selectInput(
          "multi_states",
          "Filter states (optional):",
          choices = state_choices,
          multiple = TRUE
        ),
        
        selectInput(
          "multi_outcome",
          "Outcome variable:",
          choices = outcome_choices,
        ),
        
        selectInput(
          "multi_predictors",
          "Explanatory variable(s):",
          choices = demographic_choices,
          multiple = TRUE
        ),
        
        radioButtons(
          "multi_plot_type",
          "Plot type:",
          choices = c(
            "Scatterplot" = "scatter",
            "Faceted scatter by region" = "facet_scatter", #region is dummy for now
            "Grouped bar chart" = "grouped_bar"
          ),
          selected = "scatter"
        ),
        
        checkboxInput(
          "multi_add_lm",
          "Fit linear model (lm)",
          value = FALSE
        )
      ),
      
      mainPanel(
        plotOutput("multi_plot", height = "500px"),
        br(),
        h4("Model summary"),
        verbatimTextOutput("multi_model_summary")
      )
    )
  ),
  
#=======TAB 3: Single Var Analysis=======
  tabPanel(
    "Single-Variable Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Single-variable Controls"),
        
        selectInput(
          "single_level",
          "Election level:",
          choices = election_level_choices
        ),
        
        sliderInput(
          "single_year_range",
          "Year range:",
          min = min(year_choices),
          max = max(year_choices),
          value = c(2000, max(year_choices)),
          step = 1,
          sep = ""
        ),
        
        selectInput(
          "single_states",
          "Filter states (optional):",
          choices = state_choices,
          multiple = TRUE,
          selected = NULL
        ),
        
        selectInput(
          "single_var",
          "Variable:",
          choices = single_var_choices
        ),
        
        radioButtons(
          "single_plot_type",
          "Plot type:",
          choices = c(
            "Histogram / density"    = "dist",
            "Bar chart (categorical)"= "bar",
            "Time series (by year)"  = "time"
          ),
          selected = "dist"
        )
      ),
      
      mainPanel(
        plotOutput("single_plot", height = "500px"),
        br(),
        h4("Summary statistics"),
        verbatimTextOutput("single_summary")
      )
    )
  ))))

  
### End User Interface Section ----------------
##
### Begin Server Section ----------------
server <- function(input, output, session) {
  ###
  ### Enter Server Code After this line
  ###
  ##add in helpers here 
  
  ###create US election map
  
  
  ## 4. Create Single Variable Plot
  ##
  
  ### Clean data and create the base plot
  ## Check for other inputs and adjust base plot
  
  
  
  
  
  
  ## 6. Create 2 Variable Plots
  ### Clean the data and create the base plot
  ###
  ### Create flag variables for is.numeric for x and y
  ### Replace ... with appropriate variables
  ### Use flag variables to check what type of data has been selected
  ### and then add the correct geoms, log scales, and labels.
  # Are both x and y numeric
  # log transform y Axis?
  # Add linear smoother?
  # end if both numeric
  # if x is numeric and y is not
  #
  # Is x logged
  #
  # if y is numeric and x is not
  #
  # neither x or y are numeric
  #
  
  
  
  
  
  
  
  ## 7. Create Linear Model Output
  ### If linear model is selected clean the data
  ## Check if either variable needs to be transformed and
  ## then transform the data as required for the Linear Model and create output.
  
  
  
  
  ## 8. Create data table for all data with page length 20
  
  
  ##
  ### Enter Server code above this line
} # server
### End Server Section ----------------
shinyApp(ui, server)
