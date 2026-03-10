library(tidyverse)

owner <- "avancil-corevitas"  
repo  <- "training_docs" 

#open issues
raw_open <- gh::gh(
  "/repos/{owner}/{repo}/issues",
  owner = owner, 
  repo = repo,
  state = "open",
  per_page = 100,
  .limit = 5000,
  overwrite = TRUE
)

is_issue <- map_lgl(raw_open, ~ is.null(.x$pull_request))
raw_open <- raw_open[is_issue]

#get closed
raw_closed <- gh::gh(
  "/repos/{owner}/{repo}/issues",
  owner = owner, 
  repo = repo,
  state = "closed",
  per_page = 100,
  .limit = 5000)

is_issue <- map_lgl(raw_closed, ~ is.null(.x$pull_request))
raw_closed <- raw_closed[is_issue]

raw <- c(raw_open, raw_closed)


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

parse_issue_description <- function(body) {
  extract_section(body, "Description")
}

parse_issue_dataset <- function(body) {
  extract_section(body, "Analytic Dataset")
}

parse_issue_dataset_other <- function(body) {
  extract_section(body, "Other/specify")
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
  description = map_chr(raw, ~ parse_issue_description(.x$body %||% "")),
  variables = map_chr(raw, ~ parse_issue_variables(.x$body %||% "")),
  subjects = map_chr(raw, ~ parse_issue_subjects(.x$body %||% "")),
  num_comments = map_int(raw, ~ .x$comments),
  url = map_chr(raw, ~ .x$html_url %||% "")
) |>
  arrange(desc(number)) %>%
  mutate(date_updated = as.character(as.Date(as.POSIXct(date_updated, tz="UTC", format = "%Y-%m-%d"))))  %>%
  mutate(date_reported = as.character(as.Date(as.POSIXct(date_reported, tz="UTC", format = "%Y-%m-%d")))) %>%
  mutate(date_closed = as.character(as.Date(as.POSIXct(date_closed, tz="UTC", format = "%Y-%m-%d"))))


### ----

#put in excel; write to sharepoint----
library(openxlsx)

{
  issue_log <- openxlsx::createWorkbook()
  
  # title page -----
  addWorksheet(issue_log,
               "Title Page")
  
  title_style <- createStyle(
    fontSize = 20,
    textDecoration = "bold",
    halign = "center"
  )
  
  date_style <- createStyle(
    fontSize = 12,
    halign = "center"
  )
  
  # ---- Write Content ----
  
  # Merge cells for centered layout
  mergeCells(issue_log, "Title Page", cols = 1:6, rows = 4)
  mergeCells(issue_log, "Title Page", cols = 1:6, rows = 5)
  mergeCells(issue_log, "Title Page", cols = 1:6, rows = 7)
  
  # Write title
  writeData(issue_log, "Title Page", repo, startCol = 1, startRow = 4)
  writeData(issue_log, "Title Page", "Setup Code Issue Log", startCol = 1, startRow = 5)
  addStyle(issue_log, "Title Page", title_style, rows = 4:7, cols = 1, gridExpand = TRUE)
  
  # Write date
  writeData(issue_log, "Title Page", 
            paste("Date Rendered:", format(Sys.Date(), "%B %d, %Y")),
            startCol = 1, startRow = 7)
  addStyle(issue_log, "Title Page", date_style, rows = 6, cols = 1, gridExpand = TRUE)
  
  # Optional: adjust column widths
  setColWidths(issue_log, "Title Page", cols = 1:6, widths = 15)
  
  # Optional: hide gridlines for cleaner look
  showGridLines(issue_log, "Title Page", showGridLines = FALSE)
  
  
  # body ----
  
  table_body_style <- 
    createStyle(fontSize = 10, 
                halign = "left",
                valign = "center",
                wrapText = T,
    )
  
  header_row_style <- createStyle(
    fontSize = 12,
    border = "bottom",
    borderStyle = "medium",
    halign = "center",
    textDecoration = "bold",
    fgFill = "#CAE2F6"
  )
  
  addWorksheet(issue_log, "Issue Log")
  
  writeData(issue_log, 
            "Issue Log",
            issues)
  
  setColWidths(issue_log, sheet = "Issue Log", cols=1:ncol(issues), widths = c(10, 15, 30, 60, 15, 15, 15, 20, 25, 20, 20, 60, 20, 20, 20, 60))
  setRowHeights(issue_log, sheet = "Issue Log", rows = 2:nrow(issues), heights = "auto")
  setRowHeights(issue_log, sheet = "Issue Log", rows = 1, heights = 20)
  
  addStyle(issue_log, 
           sheet = "Issue Log",
           cols = 1:ncol(issues),
           rows = 2:(nrow(issues)+1),
           style = table_body_style, 
           gridExpand = TRUE)
  
  addStyle(issue_log, 
           sheet = "Issue Log",
           cols = 1:ncol(issues),
           rows = 1,
           style = header_row_style)
  
  for (i in seq_len(nrow(issues))) {
    writeFormula(
      issue_log,
      sheet = "Issue Log",
      x = sprintf('HYPERLINK("%s", "%s")', issues$url[i], issues$url[i]),
      startCol = 16,
      startRow = i + 1  
    )
  }
  
  freezePane(issue_log, sheet = "Issue Log", firstRow = TRUE)
  
  
  
  saveWorkbook(issue_log, "Test Setup Code Issue Log.xlsx", overwrite = T)
}
