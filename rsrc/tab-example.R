library(dplyr)
dat_raw = read.csv(here::here("data/who-life-expectancy-data.csv"))

countries = c("Germany", "United States of America", "Sweden", "South Africa", "Ethiopia")
country_codes = c("GER", "USA", "SWE", "ZAF", "ETH")
names(country_codes) = countries

dat = dat_raw %>%
  filter(Country %in% countries) %>%
  select("Life.expectancy", "Country", "Year", "BMI", "Adult.Mortality") %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

set.seed(31415)
idx = sample(nrow(dat), 6)
lines = dat[idx, ] %>%
  arrange(Country, Year) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, row.names = FALSE) %>%
  kableExtra::kable_styling(latex_options = "striped")

file = here::here("tex/tab-example.tex")
writeLines(lines, file)
lines = readLines(file)

#lines[grepl("begin[{]tabular[}]", lines)] = "\\begin{tabular}[t]{r|lrrr}"
lines = lines[! grepl("addlinespace", lines)]
lines[grepl("Adult[.]Mortality", lines)] = paste0(paste(
  sprintf("\\textbf{%s}", names(dat)), collapse = " & "), "\\\\")
writeLines(lines, file)
