library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

stubs <- read_excel("data/source/Pay stubs.xlsx", sheet = "summary") %>%
  clean_names() %>%
  mutate(date = as_date(date)) %>%
  mutate_at(vars(total_taxes_and_deductions, tax:disability_insurance), ~ .x * -1)

entitlements_deductions <- read_excel("data/source/Pay stubs.xlsx", sheet = "entitlements_deductions") %>%
  clean_names() %>%
  mutate_at(vars(contains("date")), dmy) %>%
  mutate(dates = date_start %--% date_end)
