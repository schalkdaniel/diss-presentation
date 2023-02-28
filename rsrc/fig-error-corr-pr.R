library(dplyr)
library(ggplot2)

ITER_MAX = 150

devtools::load_all("~/repos/compboost")

## Data: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
dat_raw = read.csv(here::here("data/who-life-expectancy-data.csv"))

countries = c("Germany", "United States of America", "Sweden", "South Africa", "Ethiopia")
country_codes = c("GER", "USA", "SWE", "ZAF", "ETH")
names(country_codes) = countries

dat = dat_raw %>%
  filter(Country %in% countries) %>%
  select("Country", "Year", "Life.expectancy", "BMI", "Adult.Mortality") %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

target = "Life.expectancy"


ll_pr = list()
resp = cbind(dat[[target]])
for (iter in seq_len(ITER_MAX)) {
  cboost0 = boostSplines(dat, target, learning_rate = 0.05, df = 4, df_cat = 3,
    iterations = iter)
  pred0 = cboost0$response$getPrediction()
  pr0 = cboost0$loss$calculatePseudoResiduals(resp, pred0)
  df0 = data.frame(pred = pred0, pr = pr0, pr_ec = NA)

  opt = OptimizerAGBM$new()
  cboost = boostSplines(dat, target, learning_rate = 0.05, df = 4, df_cat = 3,
    iterations = iter, optimizer = opt)
  pred = cboost$response$getPrediction()
  if (iter == 1) {
    pr = cboost$loss$calculatePseudoResiduals(resp, pred)
    pr_ec = pr
  } else {
    pr_ec1 = cboost$optimizer$getErrorCorrectedPseudoResiduals()
    pr = pr_ec1 - pr_ec
    pr_ec = pr_ec1
  }
  df_pr = data.frame(pred = pred, pr = pr, pr_ec = pr_ec)

  ll_pr = c(ll_pr, list(rbind(cbind(df0, method = "CWB"),
    cbind(df_pr, method = "ACWB"))))
}

sapply(ll_pr, function(df) {
  sqrt(sum(df$pr_ec[df$method == "ACWB"]^2))
})

gg0 = plotRisk(cboost0)
gg = plotRisk(cboost)

dfp = rbind(cbind(gg0$data, method = "CWB"), cbind(gg$data, method = "ACWB"))

ggplot(dfp, aes(x = iter, y = risk, color = method)) + geom_line()
