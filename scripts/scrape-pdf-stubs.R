library(tidyverse)
library(lubridate)
library(pdftools)

read_lines_from_stub_pdf <- function(stub_pdf) {
  pdf_text(stub_pdf) %>%
    read_lines() %>%
    str_squish() %>%
    enframe(name = NULL, value = "text") %>%
    filter(! str_detect(text, "^View Phoenix Paycheque|^Return .* Print$")) %>%
    mutate(section = case_when(
      str_detect(text, "^NON NEGOTIABLE") ~ "stub-meta",
      str_detect(text, "^Pay$") ~ "summary",
      str_detect(text, "^Statutory Deductions$") ~ "deductions-statutory",
      str_detect(text, "^Voluntary Deductions$") ~ "deductions-voluntary",
      str_detect(text, "^Other Deductions$") ~ "deductions-other",
      str_detect(text, "^Entitlements and Deductions$") ~ "detailed-entitlements-deductiond",
      str_detect(text, "^Date Modified") ~ "end-matter",
      TRUE ~ NA_character_
    )) %>%
    fill(section)
}

extract_line_from_section <- function(stub, section_to_search, search_pattern, remove_search_pattern = TRUE) {
  line_to_return <- stub %>%
    filter(section == section_to_search) %>%
    filter(str_detect(text, search_pattern)) %>%
    pull(text)
  
  if (! remove_search_pattern) {
    return(line_to_return)
  }
  
  line_to_return %>% str_remove(search_pattern)
}

extract_amounts_from_line <- function(line) {
  amounts <- line %>%
    str_squish %>%
    str_split(fixed("$"), simplify = TRUE) %>%
    str_squish %>%
    str_remove_all("[^\\d\\.]") %>% # remove non-numeric characters
    as.double
  
  has_current_amount <- length(amounts) == 3
  
  if(has_current_amount) {
    return(tibble(
      amount = amounts[2],
      year_to_date = amounts[3]
    ))
  }
  
  return(tibble(
    amount = NA,
    year_to_date = amounts[2]
  ))
}

extract_summary_details <- function(stub) {
  extract_line_from_current_stub_section <- function(section, line_identifier, stub_to_check = stub) {
    stub_to_check %>%
      extract_line_from_section(section, line_identifier)
  }
  
  pay <- tribble(
    ~line, ~line_details,
    "gross", extract_line_from_current_stub_section("summary", "^Gross"),
    "taxable_gross", extract_line_from_current_stub_section("summary", "^Taxable Gross"),
    "total_taxes_and_deductions", extract_line_from_current_stub_section("summary", "^Total Taxes and Deductions"),
    "net", extract_line_from_current_stub_section("summary", "^Net")
  ) %>%
    mutate(line_details = map(line_details, extract_amounts_from_line)) %>%
    unnest_wider(line_details) %>%
    mutate(section = "pay")
  
  deductions_statutory <- tribble(
    ~line, ~line_details,
    "tax_federal", extract_line_from_current_stub_section("deductions-statutory", "^Federal Tax"),
    "tax_provincial", extract_line_from_current_stub_section("deductions-statutory", "^Provincial Tax"),
    "pension", extract_line_from_current_stub_section("deductions-statutory", "^Superannuation"),
    "cpp_qpp", extract_line_from_current_stub_section("deductions-statutory", "^CPP / QPP"),
    "ei", extract_line_from_current_stub_section("deductions-statutory", "^Employment Insurance \\(EI\\)"),
    "ppip", extract_line_from_current_stub_section("deductions-statutory", "^PPIP")
  ) %>%
    mutate(line_details = map(line_details, extract_amounts_from_line)) %>%
    unnest_wider(line_details) %>%
    mutate(section = "deductions_statutory")
  
  deductions_voluntary <- tribble(
    ~line, ~line_details,
    "canada_savings_bonds", extract_line_from_current_stub_section("deductions-voluntary", "^Canada Savings Bonds"),
    "charitable_donations", extract_line_from_current_stub_section("deductions-voluntary", "^Charitable Donations"),
    "credit_union", extract_line_from_current_stub_section("deductions-voluntary", "^Credit Union")
  ) %>%
    mutate(line_details = map(line_details, extract_amounts_from_line)) %>%
    unnest_wider(line_details) %>%
    mutate(section = "deductions_voluntary")
  
  deductions_other <- tribble(
    ~line, ~line_details,
    "association_dues", extract_line_from_current_stub_section("deductions-other", "^Association Dues"),
    "death_benefits", extract_line_from_current_stub_section("deductions-other", "^Death Benefits"),
    "disability_insurance", extract_line_from_current_stub_section("deductions-other", "^Disability Insurance"),
    "health_insurance", extract_line_from_current_stub_section("deductions-other", "^Health Insurance"),
    "group_medical_insurance", extract_line_from_current_stub_section("deductions-other", "^Group Medical Insurance"),
    "taxable_allowance_benefits", extract_line_from_current_stub_section("deductions-other", "^Taxable Allowance Benefits")
  ) %>%
    mutate(line_details = map(line_details, extract_amounts_from_line)) %>%
    unnest_wider(line_details) %>%
    mutate(section = "deductions_other")
  
  
  bind_rows(pay, deductions_statutory, deductions_voluntary, deductions_other) %>%
    mutate(
      date = extract_line_from_current_stub_section("stub-meta", "^Dated: ") %>%
        ymd(),
      paycheque_number = extract_line_from_current_stub_section("stub-meta", "^Paycheque Number: ") %>%
        as.integer
    ) %>%
    select(date, paycheque_number, section, everything())
}

stubs <- tibble(filename = fs::dir_ls("data/source/stubs/", glob = "*.pdf")) %>%
  filter(! str_detect(filename, "mgp|phx|damages")) %>%
  mutate(contents = map(filename, read_lines_from_stub_pdf)) %>%
  mutate(summary = map(contents, extract_summary_details))

stub_summary_details <- stubs %>%
  select(-contents) %>%
  unnest_longer(summary) %>%
  unnest(summary)

# what's the percent deductions, based on net vs gross, by year?
stub_summary_details %>%
  filter(section == "pay", line %in% c("gross", "net")) %>%
  group_by(year = year(date), line) %>%
  summarize(amount = sum(amount, na.rm = TRUE)) %>%
  pivot_wider(id_cols = year, names_from = line, values_from = amount) %>%
  mutate(pct_deduction = 1 - net / gross)
