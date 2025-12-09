library(reader)
library(dplyr)
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(scales)


anes_df <- read_csv("data_raw/anes_timeseries_cdf_csv_20220916.csv")
us_states_map <- ggplot2::map_data("state")

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

govener_df <- read_excel("data_raw/StateElections_Gub_2012_09_06_Public_Version.xlsx")

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


# these will need to be played with once other is cleaned 

year_choices <- unique(govener_clean_sub$year) |> 
  sort()

party_choices <- c("Democrat", "Republican", "None-major party")

#president/house/senate data
president_df <- read_csv("president_data_clean.csv")
senate_df    <- read_csv("data_raw/cleaned_senate_data.csv")
house_df     <- read_csv("data_raw/house_data_cleaned.csv")

# --- NEW: standardize state + state_po here -----------------------
standardize_state_vars <- function(df) {
  df |>
    mutate(
      # make sure state_po is uppercase two-letter code
      state_po = toupper(state_po),
      # derive canonical full state name from state_po
      state    = state.name[match(state_po, state.abb)]
    )
}

president_df <- standardize_state_vars(president_df)
senate_df <- standardize_state_vars(senate_df)
house_df <- standardize_state_vars(house_df)
# -----------------------------------------------------------

# Make House party column roughly comparable to party_simplified
house_df <- house_df |>
  mutate(
    party_simplified = case_when(
      party %in% c("DEMOCRAT", "Democrat", "DEMOCRATIC") ~ "DEMOCRAT",
      party %in% c("REPUBLICAN", "Republican") ~ "REPUBLICAN",
      TRUE ~ "OTHER"
    )
  )

# Stack into one long elections df with a "level" column
elections_all_dummy <- bind_rows(
  president_df |>
    mutate(level = "President") |>
    select(
      year, state, state_po,
      office,
      candidate,
      party_simplified,
      candidatevotes,
      totalvotes,
      level
    ),
  senate_df |>
    mutate(level = "Senate") |>
    select(
      year, state, state_po,
      office,
      candidate,
      party_simplified,
      candidatevotes,
      totalvotes,
      level
    ),
  house_df |>
    mutate(level = "House") |>
    select(
      year, state, state_po,
      office,
      district, #only exists for House
      candidate,
      party_simplified,
      candidatevotes,
      totalvotes,
      level
    )
)

gub_dummy <- govener_clean_sub |>
  mutate(
    office = "GOVERNOR",
    # convert full state names to postal abbreviations for state_po
    state_po = state.abb[match(state, state.name)],
    candidate = NA_character_,
    party_simplified = governor_party,
    candidatevotes = NA_real_,
    totalvotes = NA_real_,
    level = "Governor"
  ) |>
  select(
    year, state, state_po,
    office,
    candidate,
    party_simplified,
    candidatevotes,
    totalvotes,
    level
  )

elections_all_dummy <- bind_rows(elections_all_dummy, gub_dummy)

elections_all_dummy <- elections_all_dummy |>
  left_join(
    govener_clean_sub |>
      select(
        state,
        year,
        governor_party,
        femgov,
        term_length,
        gub_election
      ),
    by = c("state", "year")
  )

anes_state_year <- anes_clean_subset |>
  group_by(year, state_name) |>
  summarise(
    prop_dem_party_id = mean(
      party_id %in% c("Strong Democrat", "Weak Democrat", "Ind-Democrat"),
      na.rm = TRUE
    ),
    prop_rep_party_id = mean(
      party_id %in% c("Strong Republican", "Weak Republican", "Ind-Republican"),
      na.rm = TRUE
    ),
    prop_voted = mean(turnout == "Voted", na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(state = state_name)

elections_all_dummy <- elections_all_dummy |>
  left_join(anes_state_year, by = c("state", "year"))

## ======

# Use all election years in your combined dataset
year_choices <- elections_all_dummy$year |>
  unique() |>
  sort()

# Election level selector
election_level_choices <- elections_all_dummy$level |>
  unique() |>
  sort()

#states (using postal abbreviations; switch to `state` if you prefer full names)
state_choices <- elections_all_dummy |>
  arrange(state) |>
  pull(state) |>
  unique()

# Outcome variables for the multivariable tab
outcome_choices <- c(
  "Total votes" = "totalvotes",
  "Candidate votes" = "candidatevotes"
)

# "Demographic" / explanatory variables available in these files for now
# (you can add more once you join ANES or other data)
demographic_choices <- c(
  "Party (simplified)" = "party_simplified",
  "Office" = "office",
  "State (full name)" = "state"
)

# Variables allowed on the single-variable tab
single_var_choices <- c(
  "Total votes" = "totalvotes",
  "Candidate votes" = "candidatevotes",
  "Party (simplified)" = "party_simplified"
)
map_overlay_choices <- c(
  "ANES: share Democrat ID" = "prop_dem_party_id",
  "ANES: share Republican ID" = "prop_rep_party_id",
  "ANES: turnout rate" = "prop_voted"
)

state_abb_choices <- setNames(state.abb, state.name)

## Begin User Interface Section ----------------

ui <- fluidPage(
  titlePanel("Analyzing Trends in U.S. Elections (1976–2020)"),
  
  tabsetPanel(
    # ===== TAB 1: U.S. Election Map =================================
    tabPanel(
      "National Map Explorer",
      sidebarLayout(
        sidebarPanel(
          h4("Map Controls"),
          
          selectInput(
            "map_level",
            "Election level:",
            choices = c("President", "Senate", "House"),
            selected = "President"
          ),
          
          selectInput(
            "map_year",
            "Select year:",
            choices = year_choices,
            selected = max(year_choices)
          ),
          
          selectInput(
            "map_state",
            "Filter to state (optional):",
            choices  = c("All states" = "", state_choices),
            selected = ""
          ),
          
          selectInput(
            "map_info_vars",
            "Extra columns in summary table:",
            choices = c(
              "totalvotes",
              "candidatevotes",
              "dem_votes",
              "rep_votes",
              "other_votes",
              "rep_share",
              "dem_share",
              "rep_margin"
            ),
            multiple = TRUE
          )
        ),
        mainPanel(
          h4(textOutput("map_overall_winner")),
          plotlyOutput("us_map", height = "500px"),
          tableOutput("map_summary")
        )
      )
    ),
    
    # ===== TAB 2: Multivariable Analysis ============================
    tabPanel(
      "Multi-Variable Analysis",
      sidebarLayout(
        sidebarPanel(
          h4("Multivariable Controls"),
          
          # Which level of election to analyze
          selectInput(
            "multi_level",
            "Election level:",
            choices  = election_level_choices,
            selected = "President"
          ),
          
          # Year range
          sliderInput(
            "multi_year_range",
            "Year range:",
            min = min(year_choices),
            max = max(year_choices),
            value = c(1990, max(year_choices)),
            step = 1,
            sep  = ""
          ),
          
          # Optional state filter
          selectInput(
            "multi_states",
            "Filter states (optional):",
            choices  = state_choices,
            multiple = TRUE
          ),
          
          # Outcome (Y-axis)
          selectInput(
            "multi_outcome",
            "Outcome variable (Y-axis):",
            choices = c(
              "Total votes" = "totalvotes",
              "Republican vote share (R / total)" = "rep_share",
              "Democratic vote share (D / total)" = "dem_share",
              "Republican margin (R - D)" = "rep_margin"
            ),
            selected = "rep_margin"
          ),
          
          # Explanatory variables (first one will be X-axis)
          selectInput(
            "multi_predictors",
            "Explanatory variable(s) (first is X-axis):",
            choices = c(
              "ANES: turnout rate" = "prop_voted",
              "ANES: share Democrat ID" = "prop_dem_party_id",
              "ANES: share Republican ID" = "prop_rep_party_id",
              "Governor party" = "governor_party",
              "State" = "state",
              "Year" = "year"
            ),
            multiple = TRUE,
            selected = c("prop_dem_party_id", "prop_voted")
          ),
          
          # Color aesthetic
          selectInput(
            "multi_color_by",
            "Color by (optional):",
            choices = c(
              "None" = "",
              "Governor party" = "governor_party",
              "State" = "state"
            ),
            selected = "governor_party"
          ),
          
          # Plot style
          radioButtons(
            "multi_plot_type",
            "Plot type:",
            choices = c(
              "Scatterplot" = "scatter",
              "Scatter, faceted by state" = "facet_state",
              "Grouped boxplot" = "box"
            ),
            selected = "scatter"
          ),
          
          checkboxInput(
            "multi_add_lm",
            "Add linear model fit",
            value = TRUE
          )
        ),
        
        mainPanel(
          plotOutput("multi_plot", height = "500px"),
          br(),
          h4("Model summary (lm)"),
          verbatimTextOutput("multi_model_summary"),
          br(),
          h4("First 10 rows of aggregated data"),
          tableOutput("multi_data_head")
        )
      )
    ),
    
    
    # ===== TAB 3: Single-Variable Analysis ==========================
    tabPanel(
      "Single-Variable Analysis",
      
      sidebarLayout(
        
        sidebarPanel(
          # Data source switch
          radioButtons(
            "single_analysis_data_source",
            "What do you want to explore?",
            choices = c(
              "Public Opinion (ANES)" = "anes",
              "Election Results" = "results"
            ),
            selected = "anes"
          ),
          
          # ANES-only inputs
          conditionalPanel(
            condition = "input.single_analysis_data_source == 'anes'",
            
            selectInput(
              "anes_variable",
              "Public Opinion Metric:",
              choices = map_overlay_choices,
              selected = "prop_voted"
            ),
            selectInput(
              "anes_filter",
              "Filter voters by:",
              choices = c(
                "All voters" = "all",
                "Black voters" = "black",
                "White voters" = "white",
                "Hispanic voters" = "hispanic",
                "Men" = "men",
                "Women" = "women",
                "High school or less" = "lowedu",
                "Some college or more"= "highedu"
              ),
              selected = "all"
            )
          ),
          
          # Always show year range
          sliderInput(
            "single_analysis_year_range",
            "Year range:",
            min = min(year_choices),
            max = max(year_choices),
            value = c(min(year_choices), max(year_choices)),
            step = 1
          ),
          
          
          # Results-only inputs
          conditionalPanel(
            condition = "input.single_analysis_data_source == 'results'",
            
            selectInput(
              "single_analysis_level",
              "Election level:",
              choices = election_level_choices,
              selected = "President"
            ),
            
            radioButtons(
              "results_metric_type",
              "What do you want to show?",
              choices = c(
                "Vote Share" = "share",
                "Party Winner" = "winner"
              ),
              selected = "share"
            ),
            
            selectInput(
              "single_analysis_states",
              "Filter by states (max 6):",
              choices = state_abb_choices,
              multiple = TRUE
            ),
            
            # Variable selector ONLY for Winner mode
            conditionalPanel(
              condition = "input.results_metric_type == 'winner'",
              selectInput(
                "single_analysis_variable",
                "Variable (winner mode only):",
                choices = single_var_choices
              )
            ),
            
            radioButtons(
              "single_analysis_plot_type",
              "Plot type:",
              choices = c(
                "Time Series" = "time",
                "Bar Graph"   = "bar"
              ),
              selected = "time"
            )
          ),
          
          
          checkboxInput(
            "single_analysis_show_narrative",
            "Show narrative summary",
            FALSE
          )
        ), # END sidebarPanel
        
        mainPanel(
          plotOutput("single_analysis_plot", height = "500px"),
          verbatimTextOutput("single_analysis_summary"),
          
          conditionalPanel(
            condition = "input.single_analysis_level == 'Governor' &&
                 (input.single_analysis_variable == 'totalvotes')",
            div(
              style = "background-color:#ffdddd; border:1px solid #cc0000;
              padding:10px; margin-top:10px;",
              HTML("<strong>Error:</strong> Governor vote counts not available.")
            )
          ),
          
          conditionalPanel(
            condition = "input.single_analysis_show_narrative == true",
            div(
              style = "background-color:#f0f8ff; border:1px solid #4682b4;
              padding:10px;",
              textOutput("single_analysis_narrative")
              
            )
          )
        )
      )
    ),
    #---------stat test UI-------------
    tabPanel(
      "Statistical Tests",
      sidebarLayout(
        sidebarPanel(
          h4("Statistical Test Controls"),
          
          radioButtons(
            "test_type",
            "Choose a test:",
            choices = c(
              "Turnout by governor party (t-test)" =
                "ttest_turnout_govparty",
              "Governor vs president party alignment (chi-square)" =
                "chisq_gov_pres",
              "ANES Dem ID vs Dem vote share (correlation)" =
                "cor_demID_demShare"
            ),
            selected = "ttest_turnout_govparty"
          ),
          
          sliderInput(
            "test_year_range",
            "Year range:",
            min   = min(year_choices),
            max   = max(year_choices),
            value = c(1980, max(year_choices)),
            step  = 1,
            sep   = ""
          )
        ),
        
        mainPanel(
          h4("Test results"),
          verbatimTextOutput("stat_test_result"),
          br(),
          h4("Data used in test (first 10 rows)"),
          tableOutput("stat_test_data")
        )
      )
    )
  )
)
## End User Interface Section ----------------

### Begin Server Section ----------------

server <- function(input, output, session) {
  
  ## Helper: pick the right data frame based on election level
  get_level_df <- function(level) {
    dplyr::filter(elections_all_dummy, level == !!level)
  }
  
  ## Helper: filter by year range and optional state list (state_po)
  filter_elections <- function(df, year_range, states = NULL) {
    
    df2 <- df |>
      dplyr::filter(
        year >= year_range[1],
        year <= year_range[2]
      )
    
    if (!is.null(states) && length(states) > 0) {
      if ("state_po" %in% names(df2)) {
        df2 <- df2 |>
          dplyr::filter(state_po %in% states)
      } 
      
      else if ("state" %in% names(df2)) {
        df2 <- df2 |>
          dplyr::filter(state %in% states)
      }
    }
    
    df2
  }
  
  # ---- 1. National Map tab ----------------------------------------
  observeEvent(input$map_level, {
    df_level <- get_level_df(input$map_level)
    yrs <- sort(unique(df_level$year))
    
    updateSelectInput(
      session,
      "map_year",
      choices  = yrs,
      selected = max(yrs)
    )
  })
  
  map_data <- reactive({
    df_level <- get_level_df(input$map_level)
    
    df_year <- df_level |>
      dplyr::filter(year == input$map_year)
    
    # For ALL maps, optionally restrict to selected state
    if (!is.null(input$map_state) && nzchar(input$map_state)) {
      df_year <- df_year |>
        dplyr::filter(state == input$map_state)
    }
    
    df_year |>
      dplyr::group_by(state, state_po) |>
      dplyr::summarise(
        year = dplyr::first(year),
        
        # use candidatevotes to define total state votes
        totalvotes = sum(candidatevotes, na.rm = TRUE),
        # explicit sums by party so we don't accidentally double-count
        dem_votes = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
        rep_votes = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
        other_votes = sum(candidatevotes[!(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"))],
                          na.rm = TRUE)
      ) |>
      dplyr::mutate(
        # vote shares
        rep_share  = dplyr::if_else(totalvotes > 0, rep_votes / totalvotes, NA_real_),
        dem_share  = dplyr::if_else(totalvotes > 0, dem_votes / totalvotes, NA_real_),
        rep_margin = rep_share - dem_share,   # R - D
        
        # winning party (used for President coloring)
        winning_party = dplyr::case_when(
          dem_votes >  rep_votes & dem_votes >= other_votes ~ "DEMOCRAT",
          rep_votes >  dem_votes & rep_votes >= other_votes ~ "REPUBLICAN",
          TRUE ~ "OTHER"
        ),
        winning_party = factor(
          winning_party,
          levels = c("DEMOCRAT", "REPUBLICAN", "OTHER")
        ),
        region = tolower(state)   # for join with map polygons
      )
  })
  
  # Text showing the winning president (name + party) for President maps
  output$map_overall_winner <- renderText({
    #only for pres level
    if (input$map_level != "President") {
      return("")
    }
    
    df_level <- get_level_df("President")
    
    df_year <- df_level |>
      dplyr::filter(year == input$map_year)
    
    # Apply same optional state filter as map
    if (!is.null(input$map_state) && nzchar(input$map_state)) {
      df_year <- df_year |>
        dplyr::filter(state == input$map_state)
    }
    
    if (nrow(df_year) == 0) {
      return("No presidential data available for this selection.")
    }
    
    winner_df <- df_year |>
      dplyr::group_by(candidate, party_simplified) |>
      dplyr::summarise(
        votes = sum(candidatevotes, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(votes))
    
    if (nrow(winner_df) == 0 || is.na(winner_df$candidate[1])) {
      return("No presidential winner could be determined for this selection.")
    }
    
    winner <- winner_df$candidate[1]
    winner_party <- winner_df$party_simplified[1]
    
    party_label <- dplyr::case_when(
      winner_party == "DEMOCRAT"   ~ "Democrat",
      winner_party == "REPUBLICAN" ~ "Republican",
      TRUE                         ~ winner_party
    )
    
    state_suffix <- if (!is.null(input$map_state) && nzchar(input$map_state)) {
      paste0(" in ", input$map_state)
    } else {
      ""
    }
    
    paste0(
      "Winning president in ", input$map_year, state_suffix,
      ": ", winner, " (", party_label, ")"
    )
  })
  
  output$us_map <- renderPlotly({
    df <- map_data()
    req(nrow(df) > 0)
    
    # Join election data to base map polygons
    plot_df <- us_states_map |>
      dplyr::left_join(df, by = "region")
    
    # Different map behavior for President vs others
    if (input$map_level == "President") {
      
      # COLORED BY WINNING PARTY (President only)
      p <- ggplot(
        plot_df,
        aes(
          x = long,
          y = lat,
          group = group,
          fill  = winning_party,
          text  = paste0(
            "State: ", state, "<br>",
            "Winning party: ", winning_party, "<br>",
            "Dem votes: ", dem_votes, "<br>",
            "Rep votes: ", rep_votes, "<br>",
            "Total votes: ", totalvotes
          )
        )
      ) +
        geom_polygon(color = "white", linewidth = 0.2) +
        coord_fixed(1.3) +
        scale_fill_manual(
          values = c(
            "DEMOCRAT" = "blue",  # blue
            "REPUBLICAN" = "red",  # red
            "OTHER" = "grey70"
          ),
          na.value = "grey90",
          name = "Winning party"
        ) +
        labs(
          title = paste0(
            input$map_year, " ",
            input$map_level, " – State winning party"
          )
        ) +
        theme_void()
      
    } else if (input$map_level %in% c("Senate", "House")) {
      
      # RED↔BLUE CONTINUOUS SCALE FOR SENATE / HOUSE
      # rep_margin = rep_share - dem_share
      p <- ggplot(
        plot_df,
        aes(
          x = long,
          y = lat,
          group = group,
          fill = rep_margin,
          text = paste0(
            "State: ", state, "<br>",
            "Rep share: ", round(rep_share, 3), "<br>",
            "Dem share: ", round(dem_share, 3), "<br>",
            "Rep margin (R - D): ", round(rep_margin, 3), "<br>",
            "Total votes: ", totalvotes
          )
        )
      ) +
        geom_polygon(color = "white", linewidth = 0.2) +
        coord_fixed(1.3) +
        scale_fill_gradient2(
          low = "blue",  # blue = more Dem
          mid = "purple",
          high = "red",  # red  = more Rep
          midpoint = 0,
          name = "Rep margin\n(R - D)"
        ) +
        labs(
          title = paste0(
            input$map_year, " ",
            input$map_level, " – Republican vs Democrat vote share"
          )
        ) +
        theme_void()
      
    } else {
      # Fallback neutral map
      p <- ggplot(
        plot_df,
        aes(
          x = long,
          y = lat,
          group = group,
          text = paste0(
            "State: ", state, "<br>",
            "Total votes: ", totalvotes
          )
        )
      ) +
        geom_polygon(color = "white", fill = "grey80", linewidth = 0.2) +
        coord_fixed(1.3) +
        labs(
          title = paste0(
            input$map_year, " ",
            input$map_level, " – U.S. map"
          )
        ) +
        theme_void() +
        guides(fill = "none")
    }
    
    ggplotly(p, tooltip = "text") |>
      layout(
        margin = list(l = 0, r = 0, t = 50, b = 0)
      )
  })
  
  output$map_summary <- renderTable({
    df <- map_data()
    
    vars <- input$map_info_vars
    if (is.null(vars) || length(vars) == 0) {
      vars <- c("totalvotes")
    }
    
    # For ALL maps, if a state is chosen, only show that state
    if (!is.null(input$map_state) && nzchar(input$map_state)) {
      df <- df |>
        dplyr::filter(state == input$map_state)
    }
    
    df |>
      dplyr::select(state, state_po, year, dplyr::any_of(vars)) |>
      dplyr::arrange(state_po) |>
      head(20)
  })
  
  
  
  # ---- 2. Single Variable Analysis---------------------------------------- 
  
  # ================= SINGLE VARIABLE ANALYSIS =======================
  
  observeEvent(input$results_metric_type, {
    if (input$results_metric_type == "share") {
      updateSelectInput(session, "single_analysis_variable",
                        selected = "candidatevotes")
    }
  })
  
  
  # --- 1) Reactively filter ANES data ---
  anes_filtered <- reactive({
    req(input$single_analysis_data_source == "anes")
    
    df <- anes_clean_subset
    
    # ----- Demographic filters -----
    if (input$anes_filter == "black")    
      df <- df |> filter(race == "Black")
    if (input$anes_filter == "white")   
      df <- df |> filter(race == "White")
    if (input$anes_filter == "hispanic") 
      df <- df |> filter(race == "Hispanic")
    if (input$anes_filter == "men")     
      df <- df |> filter(gender == "Male")
    if (input$anes_filter == "women")    
      df <- df |> filter(gender == "Female")
    if (input$anes_filter == "lowedu")   
      df <- df |> filter(education %in% c("Less than high school", "High school"))
    if (input$anes_filter == "highedu")  
      df <- df |> filter(education %in% c("Some college", "College or more"))
    
    # ----- Filter by year -----
    df <- df |>
      filter(
        year >= input$single_analysis_year_range[1],
        year <= input$single_analysis_year_range[2]
      )
    
    # ----- Filter by state (if selected) -----
    if (!is.null(input$single_analysis_states) &&
        length(input$single_analysis_states) > 0) {
      df <- df |>
        filter(state_name %in% state.name[match(input$single_analysis_states, state.abb)])
    }
    
    df
  })
  
  
  # --- 2) Reactively filter Election data ---
  
  compute_anes_metric <- function(df, var) {
    if (var == "prop_voted") {
      as.numeric(df$turnout == "Voted")
    } else if (var == "prop_dem_party_id") {
      as.numeric(df$party_id %in% c("Strong Democrat", "Weak Democrat", "Ind-Democrat"))
    } else if (var == "prop_rep_party_id") {
      as.numeric(df$party_id %in% c("Strong Republican", "Weak Republican", "Ind-Republican"))
    } else {
      rep(NA_real_, nrow(df))
    }
  }
  
  single_analysis_data <- reactive({
    req(input$single_analysis_data_source == "results")
    
    df_level <- get_level_df(input$single_analysis_level)
    
    selected_states <- input$single_analysis_states
    if (is.null(selected_states) || length(selected_states) == 0) {
      selected_states <- NULL
    }
    
    filter_elections(
      df_level,
      input$single_analysis_year_range,
      selected_states
    )
    
  }
  
  )
  
  
  ## --- Plotting ---
  ## --- Plotting ---
  output$single_analysis_plot <- renderPlot({
    
    # ---------- ANES MODE ----------
    if (input$single_analysis_data_source == "anes") {
      df <- anes_filtered()
      req(nrow(df) > 0)
      
      var <- input$anes_variable
      
      df <- df |>
        mutate(metric = compute_anes_metric(df, var))
      
      df_agg <- df |>
        group_by(year, state_name) |>
        summarise(
          val = mean(metric, na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(df_agg, aes(year, val, color = state_name)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          x = "Election Year",
          y = "Share of voters",
          color = "State",
          title = names(map_overlay_choices)[map_overlay_choices == var]
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "bottom"
        )
      
      return(p)
    }
    
    # ---------- ELECTION RESULTS MODE ----------
    df <- single_analysis_data()
    req(nrow(df) > 0)
    
    validate(
      need(is.null(input$single_analysis_states) ||
             length(input$single_analysis_states) <= 6,
           "Please select 6 or fewer states for comparison.")
    )
    
    metric_type <- input$results_metric_type
    plot_type <- input$single_analysis_plot_type
    level <- input$single_analysis_level
    
    # Governor: only Party Winner works
    if (level == "Governor" && metric_type == "share") {
      validate(
        need(FALSE, "Governor vote counts unavailable — switch to Party Winner.")
      )
    }
    
    # --- Aggregate and calculate winners ---
    df_metric <- df |>
      group_by(year, state_po) |>
      summarise(
        dem   = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
        rep   = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
        other = sum(candidatevotes[!(party_simplified %in% c("DEMOCRAT","REPUBLICAN"))],
                    na.rm = TRUE),
        total = sum(candidatevotes, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        dem_share = if_else(total > 0, dem / total, NA_real_),
        rep_share = if_else(total > 0, rep / total, NA_real_),
        winner = case_when(
          dem > rep & dem >= other ~ "DEMOCRAT",
          rep > dem & rep >= other ~ "REPUBLICAN",
          TRUE                     ~ "OTHER"
        )
      )
    
    validate(need(nrow(df_metric) > 0, "No data available for this combination."))
    
    # --- Plotting logic ---
    if (metric_type == "share") {
      
      df_long <- df_metric |>
        select(year, state_po, dem_share, rep_share) |>
        tidyr::pivot_longer(
          cols = c(dem_share, rep_share),
          names_to = "party",
          values_to = "share"
        ) |>
        mutate(
          party = recode(
            party,
            "dem_share" = "DEMOCRAT",
            "rep_share" = "REPUBLICAN"
          )
        )
      
      if (plot_type == "time") {
        
        p <- ggplot(df_long,
                    aes(x = year, y = share, color = party)) +
          geom_line(linewidth = 1.1) +
          geom_point(size = 2) +
          facet_wrap(~ state_po, scales = "free_y") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_color_manual(values = c(
            "DEMOCRAT" = "#0059ff",
            "REPUBLICAN" = "#ff0000"
          )) +
          labs(
            title = "Vote Share by Party and State",
            x = "Election Year", y = "Vote Share (%)",
            color = "Party"
          )
        
      } else if (plot_type == "bar") {
        
        p <- ggplot(df_long,
                    aes(x = factor(year), y = share, fill = party)) +
          geom_col(position = "dodge") +
          facet_wrap(~ state_po, scales = "free_y") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_fill_manual(values = c(
            "DEMOCRAT"   = "#0059ff",
            "REPUBLICAN" = "#ff0000"
          )) +
          labs(
            title = "Year-wise Comparison of Vote Share",
            x = "Election Year", y = "Vote Share (%)",
            fill = "Party"
          )
      }
      
    } else if (metric_type == "winner") {
      
      if (plot_type == "time") {
        
        p <- ggplot(df_metric,
                    aes(x = year, y = 1, fill = winner)) +
          geom_tile(color = "white") +
          facet_wrap(~ state_po) +
          scale_fill_manual(values = c(
            "DEMOCRAT"   = "#0059ff",
            "REPUBLICAN" = "#ff0000",
            "OTHER"      = "#800080"
          )) +
          labs(
            title = "Winning Party by Year and State",
            x = "Election Year", y = "",
            fill = "Winner"
          ) +
          theme(
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank()
          )
        
      } else if (plot_type == "bar") {
        
        p <- ggplot(df_metric,
                    aes(x = winner, fill = winner)) +
          geom_bar() +
          labs(
            title = "Party Winner Count",
            x = "Winning Party",
            y = "Number of Elections",
            fill = "Party"
          ) +
          scale_fill_manual(values = c(
            "DEMOCRAT"   = "#0059ff",
            "REPUBLICAN" = "#ff0000",
            "OTHER"      = "#800080"
          ))
      }
    }
    
    # --- Final display ---
    p +
      theme_minimal(base_size = 15) +
      theme(
        plot.title  = element_text(face = "bold", hjust = 0.5),
        strip.text  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
  })
  
  # --- 4) Summary Output ---
  output$single_analysis_summary <- renderPrint({
    
    if (input$single_analysis_data_source == "anes") {
      df  <- anes_filtered()
      var <- input$anes_variable
      
      df <- df |>
        mutate(metric = compute_anes_metric(df, var))
      
      df_agg <- df |>
        group_by(year) |>
        summarise(val = mean(metric, na.rm = TRUE), .groups = "drop")
      
      print(df_agg)
      return()
    }
    
    # Only show summary in winner mode
    if (input$results_metric_type == "winner") {
      df <- single_analysis_data()
      var_name <- input$single_analysis_variable
      x <- df[[var_name]]
      
      if (is.numeric(x)) summary(x, na.rm = TRUE)
      else print(table(x))
    } else {
      cat("Summary statistics available in Party Winner mode only.\n")
    }
  })
  
  
  
  # --- 5) Narrative Output ---
  output$single_analysis_narrative <- renderText({
    req(input$single_analysis_show_narrative)
    
    if (input$single_analysis_data_source == "anes") {
      df  <- anes_filtered()
      var <- input$anes_variable
      
      df <- df |>
        mutate(metric = compute_anes_metric(df, var))
      
      df_agg <- df |>
        group_by(year) |>
        summarise(val = mean(metric, na.rm = TRUE), .groups = "drop")
      
      change <- round((dplyr::last(df_agg$val) - dplyr::first(df_agg$val)) * 100, 1)
      
      return(
        paste0(
          names(map_overlay_choices)[map_overlay_choices == var],
          " changed by ", change,
          " percentage points from ", dplyr::first(df_agg$year),
          " to ", dplyr::last(df_agg$year), "."
        )
      )
    }
    
    if (input$results_metric_type == "winner") {
      paste("Summary narrative for", input$single_analysis_variable)
    } else {
      paste("Narrative summary available in Party Winner mode only.")
    }
  })
  
  
  
  # ---- 3. Multivariable tab ---------------------------------------
  
  multi_data <- reactive({
    df_level <- get_level_df(input$multi_level)
    
    # Filter by year + (optionally) states
    df <- filter_elections(
      df_level,
      year_range = input$multi_year_range,
      states     = input$multi_states
    )
    req(nrow(df) > 0)
    
    # Aggregate to STATE–YEAR level and bring in ANES + governor info
    df_state <- df |>
      dplyr::group_by(year, state, state_po) |>
      dplyr::summarise(
        totalvotes  = sum(candidatevotes, na.rm = TRUE),
        dem_votes   = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
        rep_votes   = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
        other_votes = sum(candidatevotes[!(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"))],
                          na.rm = TRUE),
        governor_party     = dplyr::first(governor_party),
        prop_dem_party_id  = dplyr::first(prop_dem_party_id),
        prop_rep_party_id  = dplyr::first(prop_rep_party_id),
        prop_voted         = dplyr::first(prop_voted),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        rep_share  = dplyr::if_else(totalvotes > 0, rep_votes / totalvotes, NA_real_),
        dem_share  = dplyr::if_else(totalvotes > 0, dem_votes / totalvotes, NA_real_),
        rep_margin = rep_share - dem_share,
        # alias so "candidatevotes" still exists as an outcome if needed
        candidatevotes = totalvotes
      )
    
    df_state
  })
  
  output$multi_plot <- renderPlot({
    df <- multi_data()
    req(nrow(df) > 0)
    
    outcome <- input$multi_outcome
    preds <- input$multi_predictors
    req(length(preds) >= 1)
    
    x_var <- preds[1]
    color_var  <- input$multi_color_by
    plot_type  <- input$multi_plot_type
    
    # Base ggplot
    p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[outcome]]))
    
    # Optional color/fill aesthetic
    if (!is.null(color_var) && nzchar(color_var)) {
      p <- p + aes(color = .data[[color_var]])
    }
    
    if (plot_type %in% c("scatter", "facet_state")) {
      # numeric–numeric -> scatter; otherwise boxplot-style
      if (is.numeric(df[[x_var]]) && is.numeric(df[[outcome]])) {
        p <- p + geom_point(alpha = 0.7)
        if (input$multi_add_lm) {
          p <- p + geom_smooth(method = "lm", se = FALSE)
        }
      } else {
        p <- p + geom_boxplot()
      }
      
      if (plot_type == "facet_state") {
        p <- p + facet_wrap(~ state)
      }
      
    } else if (plot_type == "box") {
      # Grouped boxplot of outcome by (categorical) x
      p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[outcome]]))
      if (!is.null(color_var) && nzchar(color_var)) {
        p <- p + aes(fill = .data[[color_var]])
      }
      p <- p + geom_boxplot()
    }
    
    # Nice axis labels
    outcome_labels <- c(
      totalvotes = "Total votes (all candidates)",
      rep_share  = "Republican vote share",
      dem_share  = "Democratic vote share",
      rep_margin = "Republican minus Democratic vote share"
    )
    
    predictor_labels <- c(
      prop_voted = "ANES turnout rate",
      prop_dem_party_id = "ANES share Democrat ID",
      prop_rep_party_id = "ANES share Republican ID",
      governor_party = "Governor party",
      state = "State",
      year = "Year"
    )
    
    y_lab <- outcome_labels[[outcome]]
    if (is.null(y_lab)) y_lab <- outcome
    
    x_lab <- predictor_labels[[x_var]]
    if (is.null(x_lab)) x_lab <- x_var
    
    p +
      labs(
        x = x_lab,
        y = y_lab,
        color = if (!is.null(color_var) && nzchar(color_var)) color_var else NULL,
        fill = if (!is.null(color_var) && nzchar(color_var)) color_var else NULL
      ) +
      theme_minimal()
  })
  
  output$multi_model_summary <- renderPrint({
    df <- multi_data()
    req(nrow(df) > 5)
    
    outcome <- input$multi_outcome
    preds <- input$multi_predictors
    req(length(preds) >= 1)
    
    # Simple check: need numeric outcome for lm
    if (!is.numeric(df[[outcome]])) {
      cat("Linear model summary only computed when the outcome is numeric.\n")
      return()
    }
    
    formula_str <- paste(
      outcome,
      "~",
      paste(preds, collapse = " + ")
    )
    
    model <- lm(as.formula(formula_str), data = df)
    summary(model)
  })
  
  output$multi_data_head <- renderTable({
    head(multi_data(), 10)
  })
  
  # ----- 4. Statistical Test Tab -------
  
  test_data <- reactive({
    req(input$test_type, input$test_year_range)
    yr <- input$test_year_range
    
    if (input$test_type == "ttest_turnout_govparty") {
      # t-test: ANES turnout (prop_voted) by governor party
      df <- elections_all_dummy |>
        dplyr::filter(
          level == "Governor",
          year >= yr[1],
          year <= yr[2],
          !is.na(governor_party),
          !is.na(prop_voted)
        ) |>
        dplyr::distinct(state, year, governor_party, prop_voted)
      
    } else if (input$test_type == "chisq_gov_pres") {
      
      pres_state_year <- elections_all_dummy |>
        dplyr::filter(
          level == "President",
          year >= yr[1],
          year <= yr[2]
        ) |>
        dplyr::group_by(state, year) |>
        dplyr::summarize(
          totalvotes = sum(candidatevotes, na.rm = TRUE),
          dem_votes = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
          rep_votes = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
          other_votes = sum(candidatevotes[!(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"))],
                            na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          winning_party = dplyr::case_when(
            dem_votes >  rep_votes & dem_votes >= other_votes ~ "DEMOCRAT",
            rep_votes >  dem_votes & rep_votes >= other_votes ~ "REPUBLICAN",
            TRUE ~ "OTHER"
          )
        )
      
      df <- pres_state_year |>
        dplyr::left_join(
          govener_clean_sub |>
            dplyr::select(state, year, governor_party),
          by = c("state", "year")
        ) |>
        dplyr::filter(
          !is.na(governor_party),
          !is.na(winning_party)
        )
      
    } else if (input$test_type == "cor_demID_demShare") {
      
      pres_state_year <- elections_all_dummy |>
        dplyr::filter(
          level == "President",
          year >= yr[1],
          year <= yr[2]
        ) |>
        dplyr::group_by(state, year) |>
        dplyr::summarize(
          totalvotes = sum(candidatevotes, na.rm = TRUE),
          dem_votes  = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
          rep_votes  = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          dem_share = dplyr::if_else(
            totalvotes > 0,
            dem_votes / totalvotes,
            NA_real_
          )
        )
      
      df <- pres_state_year |>
        dplyr::left_join(
          anes_state_year |>
            dplyr::select(year, state, prop_dem_party_id),
          by = c("state", "year")
        ) |>
        dplyr::filter(
          !is.na(dem_share),
          !is.na(prop_dem_party_id)
        )
      
    } else {
      df <- tibble::tibble()
    }
    
    df
  })
  
  output$stat_test_result <- renderPrint({
    df <- test_data()
    req(nrow(df) > 0)
    
    if (input$test_type == "ttest_turnout_govparty") {
      cat("Two-sample t-test:\n",
          "Mean ANES turnout (prop_voted) by governor party\n\n")
      print(t.test(prop_voted ~ governor_party, data = df))
      
    } else if (input$test_type == "chisq_gov_pres") {
      cat("Chi-square test of independence:\n",
          "Governor party vs. presidential winning party (state-year)\n\n")
      tab <- table(df$governor_party, df$winning_party)
      print(tab)
      cat("\n")
      print(chisq.test(tab))
      
    } else if (input$test_type == "cor_demID_demShare") {
      cat("Correlation test:\n",
          "ANES Dem ID (prop_dem_party_id) vs Democratic vote share (dem_share)\n\n")
      print(cor.test(df$prop_dem_party_id, df$dem_share, method = "pearson"))
    }
  })
  
  output$stat_test_data <- renderTable({
    head(test_data(), 10)
  })
}

shinyApp(ui, server)