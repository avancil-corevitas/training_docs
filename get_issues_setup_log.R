library(tidyverse)

owner <- "avancil-corevitas"  
repo  <- "training_docs" 

#open issues
raw <- gh::gh(
  "/repos/{owner}/{repo}/issues",
  owner = owner, 
  repo = repo,
  state = "open",
  per_page = 100,
  .limit = 5000   
)

is_issue <- map_lgl(raw, ~ is.null(.x$pull_request))
raw <- raw[is_issue]


extract_section <- function(body, heading) {
  pattern <- paste0(
    "^###\\s*", str_replace_all(heading, "([.?()])", "\\\\\\1"),
    "\\s*\\n+(.*?)(?=^\\s*###\\s|\\Z)"
  )
  
  match <- str_match(body, regex(pattern, multiline = TRUE, dotall = TRUE))
  
  if (is.na(match[1,2])) {
    return(NA_character_)
  }
  
  value <- str_trim(match[1,2])
  
  if (tolower(value) %in% c("_no response_", "no response", "")) {
    return(NA_character_)
  }
  
  return(value)
}

parse_issue_variables <- function(body) {
   extract_section(body, "What variables are affected?")
}

parse_issue_subjects <- function(body) {
  extract_section(body, "What subjects are affected?")
}

parse_issue_dataset <- function(body) {
  extract_section(body, "Relevant Dataset")
}

parse_issue_dataset_other <- function(body) {
  extract_section(body, "Other/specify:")
}

issues <- tibble(
  number = map_int(raw, ~ .x$number),
  issue_status = map_chr(raw, ~ .x$state),
  resolution_status = map(raw, \(top){
    map(top$labels, pluck, "name", .default = NA_character_)
  }),
  title = map_chr(raw, ~ .x$title %||% ""),
  date_reported = map_chr(raw, ~ .x$created_at %||% NA_character_),
  date_updated = map_chr(raw, ~ .x$updated_at %||% NA_character_),
  date_closed = map_chr(raw, ~ .x$closed_at %||% NA_character_),
  reported_by = map_chr(raw, ~ .x$user$login %||% ""),
  assignees = map(raw, \(top){
    map(top$assignees, pluck, "login", .default = NA_character_)
  }),
  dataset = map_chr(raw, ~ parse_issue_dataset(.x$body %||% "")),
  dataset_other = map_chr(raw, ~ parse_issue_dataset_other(.x$body %||% "")),
  description = map_chr(raw, ~ .x$body %||% ""),
  variables = map_chr(raw, ~ parse_issue_variables(.x$body %||% "")),
  subjects = map_chr(raw, ~ parse_issue_subjects(.x$body %||% "")),
  url = map_chr(raw, ~ .x$html_url %||% "")
) |>
  arrange(desc(number)) %>%
  mutate(date_updated = as.character(as.Date(as.POSIXct(date_updated, tz="UTC", format = "%Y-%m-%d"))))  %>%
  mutate(date_reported = as.character(as.Date(as.POSIXct(date_reported, tz="UTC", format = "%Y-%m-%d")))) %>%
  mutate(date_closed = as.character(as.Date(as.POSIXct(date_closed, tz="UTC", format = "%Y-%m-%d"))))

