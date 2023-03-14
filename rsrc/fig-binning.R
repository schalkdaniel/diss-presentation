library(dplyr)
library(ggplot2)
library(ggsci)
library(patchwork)

devtools::load_all("~/repos/compboost")

REBUILD = TRUE
FIGURES = TRUE
ITER_MAX = 150L
RM_OLD   = TRUE
FIG_DIR  = "figures"

dir.create(FIG_DIR)

dat_raw = read.csv(here::here("data/who-life-expectancy-data.csv"))

countries = c("Germany", "United States of America", "Sweden", "South Africa", "Ethiopia")
country_codes = c("GER", "USA", "SWE", "ZAF", "ETH")
names(country_codes) = countries

dat = dat_raw %>%
  filter(Country %in% countries) %>%
  select("Life.expectancy", "Country", "Year", "BMI", "Adult.Mortality") %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

target = "Life.expectancy"

## Spline base learner:
## ===================================================================

FIG_DIR_BS = sprintf("%s/%s", FIG_DIR, "bs-base")
dir.create(FIG_DIR_BS, recursive = TRUE)

xn = "Adult.Mortality"

cboost = Compboost$new(data = dat, target = target, learning_rate = 0.03)
cboost$addBaselearner(xn, "spline", BaselearnerPSpline, df = 4, n_knots = 6)
cboost$train(ITER_MAX)

dfage = plotPEUni(cboost, xn)$layers[[1]]$data
ggage = ggplot(dfage, aes(x, y)) +
  geom_line(linewidth = 1.2) +
  xlab(expression(x[k])) +
  ylab(expression(b[k])) +
  theme_minimal()

ggsave(ggage, filename = here::here("figures/binning/fig-fe.png"), width = 2.2, height = 1.8)

## Binning figure:
## ===================================================================

x = dat[[xn]]
z = seq(min(x), max(x), length.out = 5)
xb = vapply(x, FUN.VALUE = numeric(1), FUN = function(x) z[which.min(abs(x - z))])
dfpl = data.frame(x = x, z = xb)
dfd = data.frame(x = z)

getBinPlot = function(i) {
  if (i >= nrow(dfd))
    xfilter = dfd$x[i]
  else
    xfilter = (dfd$x[i] + dfd$x[i + 1]) / 2

  xl = sprintf("expression(x[k] == x[\"%s\"])", xn)
  dfplt = dfpl %>% filter(x <= xfilter)
  dfdt = dfd %>% filter(x <= xfilter)
  gg = ggplot() +
    geom_segment(data = dfplt, aes(x = x, xend = z, y = 1, yend = 0), alpha = 0.5) +
    geom_point(data = dfplt, aes(x = x, y = 1)) +
    geom_point(data = dfplt, aes(x = z, y = 0)) +
    geom_point(data = dfdt, aes(x = x, y = 0)) +
    theme_minimal() +
    xlab(eval(parse(text = xl))) + ylab("") +
    scale_y_continuous(limits = c(-0.2, 1.2),
      breaks = c(0, 1),
      labels = expression(paste("Design points ", z[k]), paste("Feature values ", x[k]))) +
    xlim(min(dfpl[["x"]]), max(dfpl[["x"]]))

  return(gg)
}

ggsave(getBinPlot(5), filename = here::here("figures/binning/fig-xbin.png"), width = 5, height = 1)

ndat = cboost$prepareData(data.frame(Adult.Mortality = dfd$x))
Zbin = round(as.matrix(cboost$baselearner_list$Adult.Mortality_spline$factory$transformData(ndat)$design), 2)
latexRow = function(x) {
  zidx = x < 0.006
  out = as.character(x)
  out[zidx] = sprintf("\\color{lightgray}%s", "0.00")
  out[! zidx] = sprintf("\\color{black}%s", x[!zidx])
  paste0(paste(out, collapse = " & "), "\\\\\n")
}
cat(apply(Zbin, 1, latexRow))


memExample = function(n, p, dk) {
  mem = c(n, sqrt(n)) * p * dk * 8
  data.frame(n = n, p = p, dk = dk, mem = mem / 1024^2,
    mode = c("None", "Binning"), unit = "MB")
}

#memExample(1000000, 100, 24)
#memExample(1000000, 500, 24)

#dat %>%
  #group_by(Country) %>%
  #filter(row_number() == 1 | row_number() == n()) %>%
  #arrange(Country, Year)

