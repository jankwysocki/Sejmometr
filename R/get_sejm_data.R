## R/get_sejm_data.R ##
library(httr) # for HTTP requests
library(jsonlite) # for JSON parsing
library(dplyr) # data manipulation package, e.g. sorting, filtering, summarizing
library(purrr) # functional programming, loops
library(tidyr) # data tidying
library(lubridate) # date manipulation

# Helper function to fetch API data safely
fetch_api <- function(endpoint) {
  base_url <- "https://api.sejm.gov.pl"
  url <- paste0(base_url, endpoint) # construct full URL
  
  # tryCatch to handle potential errors if API is down or endpoint is invalid
  tryCatch({
    response <- GET(url) # make GET request
    stop_for_status(response) # check if status is OK, if not, stop
    fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE) # convert nested JSON to flat data frame
  }, error = function(e) {
    message(paste("Error fetching", url, ":", e$message))
    return(NULL) # return NULL on error so the script knows something went wrong
  })
}

# Main function to update data
# this is called by the app when the Refresh button is clicked
update_sejm_data <- function(progress_callback = NULL) {
  
  # 1. Get Current Term Info
  # ---------------------------------------------------------
  if (!is.null(progress_callback)) progress_callback(0.1, "Fetching term info...")
  terms <- fetch_api("/sejm/term")
  current_term_info <- terms %>% filter(current == TRUE) # filter to find current term
  
  current_term <- current_term_info$num # extract term number
  term_start_date <- as.Date(current_term_info$from) # extract term start date
  
  if (length(current_term) == 0) stop("Could not determine current term.") # safety check
  message(paste("Current Sejm Term:", current_term, "started on", term_start_date))
  
  # 2. Get MPs List
  # ---------------------------------------------------------
  if (!is.null(progress_callback)) progress_callback(0.2, "Fetching list of MPs...")
  all_mps <- fetch_api(paste0("/sejm/term", current_term, "/MP")) # get all MPs for current term
  
  # filter only active MPs and select relevant columns
  active_mps <- all_mps %>% 
    filter(active == TRUE) %>%
    select(id, firstName, lastName, club, districtNum, birthDate) %>%
    mutate( # Calculate full name and age
      fullName = paste(firstName, lastName),
      age = floor(as.numeric(difftime(Sys.Date(), as.Date(birthDate), units = "weeks")) / 52.25) # today - birthDate in years
    )
  
  message(paste("Found", nrow(active_mps), "active MPs.")) # log number of active MPs for the status box
  
  # 3. Get Committee Data (For Salary Calculation)
  # ---------------------------------------------------------
  if (!is.null(progress_callback)) progress_callback(0.3, "Fetching committee memberships...")
  
  committees_list <- fetch_api(paste0("/sejm/term", current_term, "/committees")) # get all committees for current term
  mp_bonuses <- data.frame(id = integer(), bonus_percent = numeric()) # initialize empty data frame for bonuses
  
  # this loop goes through each committee to find members and their roles
  if (!is.null(committees_list)) {
    for (code in committees_list$code) {
      comm_details <- fetch_api(paste0("/sejm/term", current_term, "/committees/", code)) # get committee details
      
      if (!is.null(comm_details$members) && "function" %in% names(comm_details$members)) { # if members exist, check roles
        members <- comm_details$members %>%
          mutate(
            bonus = case_when( # assign bonus based on role, case_when is like a if/else ladder
              # if role contains "przewodniczący" but not "zastępca", assign 20%
              grepl("przewodniczący", `function`, ignore.case = TRUE) & !grepl("zastępca", `function`, ignore.case = TRUE) ~ 0.20, 
              # if role contains "zastępca przewodniczącego", assign 15%
              grepl("zastępca przewodniczącego", `function`, ignore.case = TRUE) ~ 0.15,
              # otherwise, no bonus
              TRUE ~ 0
            )
          ) %>%
          select(id, bonus)
        
        # append these numbers to our main bonuses data frame using row binding
        mp_bonuses <- rbind(mp_bonuses, members)
      }
    }
  }

  # Aggregate bonus per MP (some MPs may be in multiple committees, so sum their bonuses)
  mp_bonuses_final <- mp_bonuses %>%
    group_by(id) %>% # group by MP id
    summarise(salary_bonus_factor = sum(bonus, na.rm = TRUE)) # sum bonuses, remove NAs
  
  # 4. Get Voting Stats (Iterate over every MP)
  # ---------------------------------------------------------
  message("Fetching voting stats for all MPs (this takes time)...")
  
  fetch_voting_stats <- function(mp_id, idx, total) { # helper function to fetch voting stats for a single MP
    if (!is.null(progress_callback) && idx %% 10 == 0) { # update progress every 10 MPs
      progress_callback(0.4 + (0.5 * (idx / total)), paste("Fetching stats:", idx, "/", total)) # progress from 0.4 to 0.9
    }
    
    endpoint <- paste0("/sejm/term", current_term, "/MP/", mp_id, "/votings/stats") # construct endpoint
    stats <- fetch_api(endpoint) # fetch voting stats
    
    if (is.null(stats) || length(stats) == 0) return(NULL) # if no stats, return NULL
    
    # return raw rows for this MP with necessary columns
    stats %>%
      mutate(id = mp_id,
             absenceExcuse = as.numeric(absenceExcuse)) %>%
      select(id, date, numVotings, numVoted, numMissed, absenceExcuse) # voting date used for earliest vote calculation
  }
  
  ids <- active_mps$id # list of active MP ids
  # this function uses imap_dfr to iterate over ids with index, fetching voting stats and combining into one data frame
  raw_voting_data <- imap_dfr(ids, ~fetch_voting_stats(.x, .y, length(ids)))
  
  # 5. AGGREGATION STEP
  # ---------------------------------------------------------
  # there are multiple rows per MP (one per voting date), so we need to aggregate into one row per MP
  aggregated_voting_data <- raw_voting_data %>%
    group_by(id) %>%
    summarise( # summarize voting stats per MP, removing NAs
      totalVotings = sum(numVotings, na.rm = TRUE),
      totalVoted = sum(numVoted, na.rm = TRUE),
      totalMissed = sum(numMissed, na.rm = TRUE),
      totalExcused = sum(absenceExcuse, na.rm = TRUE),
      # find the earliest voting date for this MP (to calculate their start date later)
      first_vote_date = min(as.Date(date), na.rm = TRUE)
    )
  
  # 6. Merge and Calculate Final Metrics
  # ---------------------------------------------------------
  if (!is.null(progress_callback)) progress_callback(0.95, "Finalizing calculations...") # final progress update
  
  # Salary Constants
  # used to estimate total earnings, can be quickly updated if rates change
  BASE_SALARY_GROSS <- 13467.92
  PARLIAMENTARY_ALLOWANCE <- 4208.73
  
  # final merge of all data and calculation of metrics
  final_df <- active_mps %>%
    left_join(aggregated_voting_data, by = "id") %>%
    left_join(mp_bonuses_final, by = "id") %>%
    # replace NAs with 0 for calculations
    replace_na(list(salary_bonus_factor = 0, totalVotings = 0, totalVoted = 0, totalMissed = 0, totalExcused = 0)) %>%
    mutate(
      # calculate attendance rate (as fraction of total votings)
      attendance_rate = ifelse(totalVotings > 0, (totalVoted + totalExcused) / totalVotings, 0),
      
      # determine individualized start date
      # if no votes found (NA), default to term start, otherwise use their first vote date
      effective_start_date = if_else(is.na(first_vote_date) | is.infinite(first_vote_date), 
                                     term_start_date, 
                                     first_vote_date),
      
      # calculate INDIVIDUAL months in office (today - start date)
      months_in_office = as.numeric(difftime(Sys.Date(), effective_start_date, units = "days")) / 30.44,
      
      # ensure months is at least 0.5 to avoid division errors for brand new MPs
      months_in_office = pmax(months_in_office, 0.5),
      
      # estimated monthly salary (base + bonuses + allowance)
      monthly_salary = (BASE_SALARY_GROSS * (1 + salary_bonus_factor)) + PARLIAMENTARY_ALLOWANCE,
      
      # total earnings (monthly salary * months in office)
      total_earnings_est = monthly_salary * months_in_office,
      
      # cost per vote calculated only if they voted at least once (total earnings / total votes)
      cost_per_vote = ifelse(totalVoted > 0, total_earnings_est / totalVoted, total_earnings_est)
    )
  
  # save to CSV
  write.csv(final_df, "data/mps_processed.csv", row.names = FALSE)
  message(paste("Data saved to data/mps_processed.csv. Total rows:", nrow(final_df)))
  
  return(final_df)
}