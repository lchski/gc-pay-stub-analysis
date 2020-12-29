entitlements_deductions %>% filter(str_detect(type, "pay")) %>% filter(! str_detect(type, "lieu|previous|delayed implement|payment under")) %>% filter(date_start > "2020-07-01") %>% arrange(date_start) %>% ggplot(aes(x = date_start, y = amount)) + geom_point() + geom_line()

entitlements_deductions %>% filter(! str_detect(type, "pay")) %>% filter(date_start > "2020-07-01") %>% left_join(stubs %>% select(filename, stub_date = date)) %>% arrange(date_start, stub_date) %>% select(filename, stub_date, everything(), -rate, -is_pay:-dates) %>% mutate(hrly = round(amount / hours, 2)) %>% View()

entitlements_deductions %>% filter(str_detect(type, "pay")) %>% filter(! str_detect(type, "lieu|previous|delayed implement|payment under")) %>% filter(date_start > "2020-07-01") %>% left_join(stubs %>% select(filename, stub_date = date)) %>% arrange(date_start, stub_date) %>% select(filename, stub_date, everything(), -rate, -is_pay:-dates) %>% mutate(hrly = round(amount / hours, 2)) %>% View()

## pension percent
stubs %>%
  select(filename, date, taxable_gross, pension) %>%
  mutate(pension_pct = (pension * -1) / taxable_gross) %>%
  View()

stubs %>%
  select(filename, date, gross, total_taxes_and_deductions) %>%
  mutate(deduction_pct = (total_taxes_and_deductions * -1) / gross) %>%
  filter(deduction_pct > 0, deduction_pct < 1) %>%
  ggplot(aes(x = date, y = deduction_pct)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_smooth(method = "lm")

deductions_by_stub <- stubs %>% 
  select(filename, date, gross = taxable_gross, tax:disability_insurance) %>%
  pivot_longer(cols = tax:disability_insurance, names_to = "deduction", values_to = "deduction_value") %>%
  mutate(deduction_value = deduction_value * -1) %>%
  mutate(deduction_pct = (deduction_value / gross) * 100)

deductions_by_stub %>%
  ggplot(aes(x = date, y = deduction_pct, color = deduction)) +
  geom_point() +
  facet_wrap(vars(deduction)) +
  scale_y_continuous(limits = c(0, 1))

deductions_by_stub %>%
  filter(gross > 100) %>%
  ggplot(aes(x = deduction_pct, fill = deduction)) +
  geom_histogram(bins = 7) +
  facet_wrap(vars(deduction))

deductions_by_stub %>%
  filter(gross > 100) %>%
  mutate(deduction_bin = cut(deduction_pct, breaks = 7)) %>%
  count_group(deduction, deduction_bin) %>%
  arrange(deduction, deduction_bin) %>%
  View()

