library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

fetch_api <- function(endpoint) {
  base_url <- "https://api.sejm.gov.pl"
  url <- paste0(base_url, endpoint)
  tryCatch({
    response <- GET(url)
    stop_for_status(response)
    fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
  }, error = function(e) {
    message(paste("Error fetching", url, ":", e$message))
    return(NULL)
  })
}

update_sejm_data <- function(progress_callback = NULL) {
  
  # 1. Get Current Term Info
  if (!is.null(progress_callback)) progress_callback(0.1, "Fetching term info...")
  terms <- fetch_api("/sejm/term")
  current_term_info <- terms %>% filter(current == TRUE)
  current_term <- current_term_info$num
  term_start_date <- as.Date(current_term_info$from)
  months_in_office <- as.numeric(difftime(Sys.Date(), term_start_date, units = "days")) / 30.44
  
  # 2. Get MPs List (Added birthDate here)
  if (!is.null(progress_callback)) progress_callback(0.2, "Fetching list of MPs...")
  all_mps <- fetch_api(paste0("/sejm/term", current_term, "/MP"))
  
  active_mps <- all_mps %>% 
    filter(active == TRUE) %>%
    select(id, firstName, lastName, club, districtNum, birthDate) %>%  # Added birthDate
    mutate(
      fullName = paste(firstName, lastName),
      # Calculate Age
      age = floor(as.numeric(difftime(Sys.Date(), as.Date(birthDate), units = "weeks")) / 52.25)
    )
  
  message(paste("Found", nrow(active_mps), "active MPs."))
  
  # 3. Get Committee Data
  if (!is.null(progress_callback)) progress_callback(0.3, "Fetching committee memberships...")
  committees_list <- fetch_api(paste0("/sejm/term", current_term, "/committees"))
  mp_bonuses <- data.frame(id = integer(), bonus_percent = numeric())
  
  if (!is.null(committees_list)) {
    for (code in committees_list$code) {
      comm_details <- fetch_api(paste0("/sejm/term", current_term, "/committees/", code))
      if (!is.null(comm_details$members) && "function" %in% names(comm_details$members)) {
        members <- comm_details$members %>%
          mutate(bonus = case_when(
              grepl("przewodniczący", `function`, ignore.case = TRUE) & !grepl("zastępca", `function`, ignore.case = TRUE) ~ 0.20,
              grepl("zastępca przewodniczącego", `function`, ignore.case = TRUE) ~ 0.15,
              TRUE ~ 0
          )) %>% select(id, bonus)
        mp_bonuses <- rbind(mp_bonuses, members)
      }
    }
  }
  
  mp_bonuses_final <- mp_bonuses %>%
    group_by(id) %>% summarise(salary_bonus_factor = sum(bonus, na.rm = TRUE))
  
  # 4. Get Voting Stats
  message("Fetching voting stats for all MPs...")
  
  fetch_voting_stats <- function(mp_id, idx, total) {
    if (!is.null(progress_callback) && idx %% 10 == 0) {
      progress_callback(0.4 + (0.5 * (idx / total)), paste("Fetching stats:", idx, "/", total))
    }
    endpoint <- paste0("/sejm/term", current_term, "/MP/", mp_id, "/votings/stats")
    stats <- fetch_api(endpoint)
    if (is.null(stats) || length(stats) == 0) return(NULL)
    
    stats %>%
      mutate(id = mp_id, absenceExcuse = as.numeric(absenceExcuse)) %>%
      select(id, numVotings, numVoted, numMissed, absenceExcuse)
  }
  
  ids <- active_mps$id
  raw_voting_data <- imap_dfr(ids, ~fetch_voting_stats(.x, .y, length(ids)))
  
  # 5. Aggregation
  aggregated_voting_data <- raw_voting_data %>%
    group_by(id) %>%
    summarise(
      totalVotings = sum(numVotings, na.rm = TRUE),
      totalVoted = sum(numVoted, na.rm = TRUE),
      totalMissed = sum(numMissed, na.rm = TRUE),
      totalExcused = sum(absenceExcuse, na.rm = TRUE)
    )
  
  # 6. Final Calculation
  if (!is.null(progress_callback)) progress_callback(0.95, "Finalizing calculations...")
  BASE_SALARY_MONTHLY <- 17676.65
  PARLIAMENTARY_ALLOWANCE <- 4208.73
  
  final_df <- active_mps %>%
    left_join(aggregated_voting_data, by = "id") %>%
    left_join(mp_bonuses_final, by = "id") %>%
    replace_na(list(salary_bonus_factor = 0, totalVotings = 0, totalVoted = 0, totalMissed = 0, totalExcused = 0)) %>%
    mutate(
      attendance_rate = ifelse(totalVotings > 0, (totalVoted + totalExcused) / totalVotings, 0),
      monthly_salary = BASE_SALARY_MONTHLY * (1 + salary_bonus_factor) + PARLIAMENTARY_ALLOWANCE,
      total_earnings_est = monthly_salary * months_in_office,
      cost_per_vote = ifelse(totalVoted > 0, total_earnings_est / totalVoted, total_earnings_est)
    )
  
  write.csv(final_df, "data/mps_processed.csv", row.names = FALSE)
  message("Data saved to data/mps_processed.csv")
  return(final_df)
}