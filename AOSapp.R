library(reader)
library(dplyr)
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)

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

govener_clean_sub |>
  group_by(state,year) |>
  summarize(n = n())


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
senate_df    <- standardize_state_vars(senate_df)
house_df     <- standardize_state_vars(house_df)
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
      district,         # only exists for House
      candidate,
      party_simplified,
      candidatevotes,
      totalvotes,
      level
    )
)

gub_dummy <- govener_clean_sub |>
  mutate(
    office          = "GOVERNOR",
    # convert full state names to postal abbreviations for state_po
    state_po        = state.abb[match(state, state.name)],
    candidate       = NA_character_,
    party_simplified = governor_party,   # rough stand-in
    candidatevotes  = NA_real_,
    totalvotes      = NA_real_,
    level           = "Governor"
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

## === Choice vectors that were dummies before ===

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
  "ANES: share Democrat ID"   = "prop_dem_party_id",
  "ANES: share Republican ID" = "prop_rep_party_id",
  "ANES: turnout rate"        = "prop_voted"
)

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
            choices  = election_level_choices,
            selected = "President"
          ),
          
          selectInput(
            "map_year",
            "Select year:",
            choices  = year_choices,
            selected = max(year_choices)
          ),
          
          # NEW: state filter for non-presidential maps
          selectInput(
            "map_state",
            "Filter to state (for non-presidential maps):",
            choices  = c("All states" = "", state_choices),
            selected = ""
          ),
          
          selectInput(
            "map_info_vars",
            "Extra columns in summary table:",
            choices  = c(
              "totalvotes",
              "candidatevotes",
              "governor_party",
              "femgov",
              "term_length",
              "gub_election",
              "prop_dem_party_id",
              "prop_rep_party_id",
              "prop_voted"
            ),
            multiple = TRUE
          )
        ),
        mainPanel(
          plotlyOutput("us_map", height = "500px"),
          tableOutput("map_summary")
        )
      )
    ),
    
    # ===== TAB 2: Multivariable Analysis ============================
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
            min   = min(year_choices),
            max   = max(year_choices),
            value = c(2000, max(year_choices)),
            step  = 1,
            sep   = ""
          ),
          
          selectInput(
            "multi_states",
            "Filter states (optional):",
            choices  = state_choices,
            multiple = TRUE
          ),
          
          selectInput(
            "multi_outcome",
            "Outcome variable:",
            choices = outcome_choices
          ),
          
          selectInput(
            "multi_predictors",
            "Explanatory variable(s):",
            choices  = demographic_choices,
            multiple = TRUE
          ),
          
          radioButtons(
            "multi_plot_type",
            "Plot type:",
            choices = c(
              "Scatterplot"       = "scatter",
              "Faceted scatter"   = "facet_scatter",
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
    
    # ===== TAB 3: Single-Variable Analysis ==========================
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
            min   = min(year_choices),
            max   = max(year_choices),
            value = c(2000, max(year_choices)),
            step  = 1,
            sep   = ""
          ),
          
          selectInput(
            "single_states",
            "Filter states (optional):",
            choices  = state_choices,
            multiple = TRUE
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
              "Histogram / density"     = "dist",
              "Bar chart (categorical)" = "bar",
              "Time series (by year)"   = "time"
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
    )
  )
)

## End User Interface Section ----------------
##
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
      } else if ("state" %in% names(df2)) {
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
    
    # For non-presidential maps, optionally restrict to selected state
    if (input$map_level != "President" &&
        !is.null(input$map_state) &&
        nzchar(input$map_state)) {
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
        dem_votes   = sum(candidatevotes[party_simplified == "DEMOCRAT"],   na.rm = TRUE),
        rep_votes   = sum(candidatevotes[party_simplified == "REPUBLICAN"], na.rm = TRUE),
        other_votes = sum(candidatevotes[!(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"))],
                          na.rm = TRUE),
        
        # pull any governor / ANES columns if present
        dplyr::across(
          dplyr::any_of(c(
            "governor_party",
            "femgov",
            "term_length",
            "gub_election",
            "prop_dem_party_id",
            "prop_rep_party_id",
            "prop_voted"
          )),
          ~ dplyr::first(.x)
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        winning_party = dplyr::case_when(
          dem_votes >  rep_votes & dem_votes >= other_votes ~ "DEMOCRAT",
          rep_votes >  dem_votes & rep_votes >= other_votes ~ "REPUBLICAN",
          TRUE                                              ~ "OTHER"
        ),
        winning_party = factor(
          winning_party,
          levels = c("DEMOCRAT", "REPUBLICAN", "OTHER")
        ),
        region = tolower(state)   # for join with map polygons
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
          x     = long,
          y     = lat,
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
            "DEMOCRAT"   = "#3182bd",  # blue
            "REPUBLICAN" = "#de2d26",  # red
            "OTHER"      = "grey70"
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
      
    } else {
      
      # NEUTRAL MAP (no party colors) for House / Senate / Governor
      # Only the selected state (if any) will have non-NA tooltip data
      p <- ggplot(
        plot_df,
        aes(
          x     = long,
          y     = lat,
          group = group,
          text  = paste0(
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
      vars <- c("totalvotes", "prop_dem_party_id", "prop_rep_party_id", "prop_voted")
    }
    
    # For non-presidential maps, if a state is chosen, only show that state
    if (input$map_level != "President" &&
        !is.null(input$map_state) &&
        nzchar(input$map_state)) {
      df <- df |>
        dplyr::filter(state == input$map_state)
    }
    
    df |>
      dplyr::select(state, state_po, year, dplyr::any_of(vars)) |>
      dplyr::arrange(state_po) |>
      head(20)
  })
  
  # ---- 2. Single-variable tab -------------------------------------
  
  single_data <- reactive({
    df_level <- get_level_df(input$single_level)
    filter_elections(
      df_level,
      year_range = input$single_year_range,
      states     = input$single_states
    )
  })
  
  output$single_plot <- renderPlot({
    df <- single_data()
    req(nrow(df) > 0)
    
    var_name  <- input$single_var
    plot_type <- input$single_plot_type
    
    x <- df[[var_name]]
    
    if (plot_type == "dist") {
      if (is.numeric(x)) {
        # histogram + density for numeric
        p <- ggplot(df, aes(x = .data[[var_name]])) +
          geom_histogram(bins = 15, alpha = 0.7) +
          geom_density(aes(y = after_stat(..count..))) +
          labs(x = var_name, y = "Count")
      } else {
        # basic bar chart for categorical
        p <- ggplot(df, aes(x = .data[[var_name]])) +
          geom_bar() +
          labs(x = var_name, y = "Count")
      }
      
    } else if (plot_type == "bar") {
      df_counts <- df |>
        dplyr::count(.data[[var_name]], name = "n")
      
      p <- ggplot(df_counts, aes(x = .data[[var_name]], y = n)) +
        geom_col() +
        labs(x = var_name, y = "Count")
      
    } else if (plot_type == "time") {
      df_agg <- df |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          value = mean(.data[[var_name]], na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(df_agg, aes(x = year, y = value)) +
        geom_line() +
        geom_point() +
        labs(x = "Year", y = paste("Mean", var_name))
    }
    
    p + theme_minimal()
  })
  
  output$single_summary <- renderPrint({
    df <- single_data()
    req(nrow(df) > 0)
    
    var_name <- input$single_var
    x <- df[[var_name]]
    
    if (is.numeric(x)) {
      stats <- c(
        n      = sum(!is.na(x)),
        mean   = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd     = sd(x, na.rm = TRUE),
        min    = min(x, na.rm = TRUE),
        max    = max(x, na.rm = TRUE)
      )
      print(stats)
    } else {
      tab <- prop.table(table(x))
      print(round(tab, 3))
    }
  })
  
  # ---- 3. Multivariable tab ---------------------------------------
  
  multi_data <- reactive({
    df_level <- get_level_df(input$multi_level)
    filter_elections(
      df_level,
      year_range = input$multi_year_range,
      states     = input$multi_states
    )
  })
  
  output$multi_plot <- renderPlot({
    df <- multi_data()
    req(nrow(df) > 0)
    
    outcome    <- input$multi_outcome
    predictors <- input$multi_predictors
    req(length(predictors) >= 1)
    
    x_var <- predictors[1]
    
    p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[outcome]])) +
      geom_point(alpha = 0.7) +
      labs(
        x = names(demographic_choices)[demographic_choices == x_var],
        y = names(outcome_choices)[outcome_choices == outcome]
      )
    
    if (input$multi_add_lm) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    if (input$multi_plot_type == "facet_scatter") {
      # Example facet by governor party if present
      if ("governor_party" %in% names(df)) {
        p <- p + facet_wrap(~ governor_party)
      }
    } else if (input$multi_plot_type == "grouped_bar") {
      df <- df |>
        dplyr::mutate(x_bin = cut(.data[[x_var]], breaks = 4))
      
      p <- ggplot(df, aes(x = x_bin, y = .data[[outcome]])) +
        stat_summary(fun = mean, geom = "bar") +
        labs(
          x = paste(
            "Binned",
            names(demographic_choices)[demographic_choices == x_var]
          ),
          y = names(outcome_choices)[outcome_choices == outcome]
        )
    }
    
    p + theme_minimal()
  })
  
  output$multi_model_summary <- renderPrint({
    req(input$multi_add_lm)
    
    df <- multi_data()
    outcome    <- input$multi_outcome
    predictors <- input$multi_predictors
    req(length(predictors) >= 1)
    
    formula_str <- paste(
      outcome,
      "~",
      paste(predictors, collapse = " + ")
    )
    
    model <- lm(as.formula(formula_str), data = df)
    summary(model)
  })
}
### End Server Section ----------------

shinyApp(ui, server)