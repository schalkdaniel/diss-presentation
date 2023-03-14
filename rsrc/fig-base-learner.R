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
  select("Country", "Year", "Life.expectancy", "Alcohol", "Adult.Mortality") %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

target = "Life.expectancy"

#dat %>% group_by(Country) %>% summarize(mean(Life.expectancy))
#tst = data.frame(Country = dat$Country, abb = country_codes[dat$Country])


## Spline base learner:
## ===================================================================

FIG_DIR_BS = sprintf("%s/%s", FIG_DIR, "bs-base")
dir.create(FIG_DIR_BS, recursive = TRUE)

xn = "Adult.Mortality"

cboost = Compboost$new(data = dat, target = target, learning_rate = 0.03)
cboost$addBaselearner(xn, "spline", BaselearnerPSpline, df = 4, n_knots = 6)
cboost$train(ITER_MAX)

x = dat[[xn]]
bln = paste0(xn, "_spline")
xval = seq(min(x), max(x), len = 1000L)

xdat = data.frame(x = xval)
names(xdat) = xn
ndat = cboost$prepareData(xdat)
bsbase = cboost$baselearner_list[[bln]]$factory$transformData(ndat)$design
params = as.numeric(cboost$getCoef()[[bln]])
ivec = seq_len(ncol(bsbase))
bvec = sprintf("B_%s(x)", ivec)
Xbase = do.call(rbind, lapply(ivec, function(i) {
  data.frame(y = bsbase[, i], x = xval, base = bvec[i], ystretch = bsbase[, i] * params[i])
}))
dpred = data.frame(x = xval, y = as.numeric(bsbase %*% params))
idxs_add = round(seq(1, 1000, len = 100))

dp = do.call(rbind, lapply(idxs_add, function(idx) {
  dp = data.frame(y = bsbase[idx, ] * params, b = bvec, x = xval[idx])
  dp = dp %>%
    filter(y != 0) %>%
    mutate(ycs = cumsum(y)) %>%
    mutate(ycs0 = c(0, ycs[-length(y)]))
}))


dlab = Xbase %>%
  group_by(base) %>%
  filter(y == max(y)) %>%
  mutate(vjust = ifelse(ystretch < 0, 1, 0))

## Colors:
vcols_raw = viridis::viridis(10)
vcols = substr(vcols_raw, 2, 7)
colorcode = data.frame(base = bvec, color = vcols_raw)

Xbase = Xbase %>%
  left_join(colorcode, by = "base")

#plotBaselearner(cboost, "Adult.Mortality_spline") +
gg_bs_add = ggplot() +
  geom_line(data = Xbase, aes(x = x, y = y)) +
  geom_line(data = Xbase, aes(x = x, y = ystretch, color = base)) +
  geom_segment(data = dp, aes(x = x, xend = x, y = ycs0, yend = ycs, color = b),
    linewidth = 2) +
  geom_text(data = dlab, aes(x = x, y = ystretch, label = base, vjust = vjust)) +
  geom_line(data = dpred, aes(x = x, y = y)) +
  geom_point(data = dat, aes(x = Adult.Mortality, y = Life.expectancy - mean(Life.expectancy), alpha = 0.5)) +
  geom_rug(data = dat, aes(x = Adult.Mortality, y = 0)) +
  #viridis::scale_color_viridis(discrete = TRUE)
  scale_color_manual(values = vcols_raw)


## Design matrix:
Xdat = t(cboost$baselearner_list[[bln]]$factory$getData())
idxs = c(1, 59, 40, 70, 5, 10)

## Base functions:
dlab$label = sprintf("B[%s]", seq_len(nrow(dlab)))

# Without label:
Xbase$base = as.factor(Xbase$base)
Xbase$base = factor(Xbase$base, levels = bvec)
ggbs = ggplot() +
  geom_line(data = Xbase, aes(x = x, y = y, color = base), show.legend = FALSE, linewidth = 1.2) +
  geom_text(data = dlab, aes(x = x, y = y, label = label), parse = TRUE, vjust = -0.5) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  #viridis::scale_fill_viridis(discrete = TRUE) +
  scale_color_manual(labels = bvec, values = vcols_raw) +
  scale_fill_manual(values = vcols_raw) +
  xlab(xn) +
  ylab(eval(parse(text = sprintf("expression(B[k](%s))", xn)))) +
  ylim(0, 0.8) +
  geom_rug(data = dat, aes(x = Adult.Mortality, y = 0)) +
  theme_minimal()

ggsave(ggbs, filename = here::here(FIG_DIR_BS, "fig-bs0.png"), width = 7, height = 2.5)

getBasePlot = function(i, save = TRUE, ilab = NULL) {
  if (is.null(ilab)) ilab = i
  di = data.frame(bval = as.vector(Xdat[i,]), b = bvec, x = x[i], fill = vcols_raw, k = seq_along(bvec)) %>%
    filter(bval > 0.006) %>%
    mutate(label = format(round(bval, 2), nsmall = 2), alpha = 1) %>%
    mutate(label_b = sprintf("B[%s](%s) == %s", k, sprintf("x^{(%s)}", ilab), label))

  Xbase$alpha = 0.4
  Xbase$alpha[Xbase$base %in% di$b] = 0.8

  dsegment = di %>% filter(bval == max(bval))
  lhjust = -0.5
  if (dsegment$x > 470) lhjust = 1.5

  gg = ggplot() +
    geom_line(data = Xbase, aes(x = x, y = y, color = base, alpha = alpha), show.legend = FALSE, linewidth = 1.2) +
    geom_text(data = dlab, aes(x = x, y = y, label = label), parse = TRUE, vjust = -0.5) +
    #viridis::scale_color_viridis(discrete = TRUE) +
    #viridis::scale_fill_viridis(discrete = TRUE) +
    scale_color_manual(values = vcols_raw) +
    scale_fill_manual(values = vcols_raw) +
    xlab(xn) +
    ylab(eval(parse(text = sprintf("expression(B[k](%s))", xn)))) +
    ylim(0, 0.8) +
    geom_segment(data = dsegment, aes(x = x, xend = x, y = 0, yend = bval)) +
    #geom_label(data = dsegment, aes(x = x, y = 0, label = paste("x = ", x)), vjust = -0.5, hjust = 1.5, fill = "black", color = "white", fontface = "bold") +
    geom_label(data = dsegment, aes(x = x, y = 0, label = paste0(sprintf("x^{(%s)} == ", ilab), x)),
      hjust = 0.5, fill = "black", color = "white", fontface = "bold", parse = TRUE) +
    geom_rug(data = dat, aes(x = Adult.Mortality, y = 0)) +
    geom_point(data = di, aes(x = x, y = bval, color = b), show.legend = FALSE,
      size = 6) +
    geom_label(data = di, aes(x = x, y = bval, label = label_b), show.legend = FALSE,
      hjust = lhjust, color = "white", fontface = "bold", fill = di$fill, parse = TRUE) +
    theme_minimal()

  if (save) {
    ggsave(gg, filename = here::here(sprintf("%s/fig-bs%s.png", FIG_DIR_BS, i)), width = 7, height = 2.5)
  } else {
    return(gg)
  }
}

ilabs = c(1, 2, 3, 73, 74, 75)
for (i in seq_along(idxs)) {
  getBasePlot(idxs[i], save = TRUE, ilabs[i])
}
## Build design:

getRow = function(i, color = TRUE, empty_col = FALSE) {
  di = data.frame(bval = as.vector(Xdat[i,]), b = bvec, x = x[i])

  zcol = "lightgray"
  ctemp = "gray"
  if (empty_col) {
    ctemp = "white"
    zcol = "white"
  }
  ctype = ""
  if (color) {
    ctemp = ifelse(di$bval > 0.006, vcols, "lightgray")
    ctype = ifelse(di$bval > 0.006, "[HTML]", "")
  } else {
    ctemp = ifelse(di$bval > 0.006, ctemp, zcol)
  }
  di = di %>%
    mutate(mlabel = sprintf("\\color%s{%s}%s", ctype, ctemp, format(round(bval, 2), nsmall = 2)))

  oline = paste(di$mlabel, collapse = " & ")
  paste0(oline, "\\color{black}")
}

pasteBlock = function(idxs, add_colors = TRUE, dots_after = NA, empty_after = NA) {
  if (is.na(empty_after)) empty_after = length(idxs) + 1

  algs = paste(rep("c", length(vcols)), collapse = "")
  hn = paste(sprintf("\\color[HTML]{%s}B_{%s}", vcols, seq_along(vcols)), collapse = " & ")
  header = sprintf("\\begin{blockarray}{%s}\n%s \\\\", algs, hn)
  blockstart = sprintf("\\begin{block}{(%s)}\n\\phantom{x}\\\\", algs)
  blockend = "\\end{blockarray}"
  lines = c(header, blockstart)
  lines_middle = c()

  if (length(add_colors) == 1) add_colors = rep(add_colors, length(idxs))
  for (i in seq_along(idxs)) {
    empty_col = FALSE
    if (i >= empty_after) empty_col = TRUE
    lines_middle = c(lines_middle, getRow(idxs[i], color = add_colors[i], empty_col = empty_col))
    if (i == dots_after) {
      dots = paste(rep(sprintf("\\color{%s}\\vdots", ifelse(empty_col, "white", "black")), length(vcols)), collapse = " & ")
      lines_middle = c(lines_middle, dots)
    }
  }
  ladd = 0
  if (empty_after >= dots_after) ladd = 1
  lend = rep("\\\\\n  ", length(lines_middle))
  lend[length(lend)] = paste0(lend[length(lend)], "\n\\phantom{x}")
  #lend = character(length(lines_middle))
  #lend[-unique(length(lines_middle))] = "\\\\\n  "
  lines_middle = paste0(lines_middle, lend)
  lines_middle = append(lines_middle, "\n\\end{block}\n", after = empty_after + ladd)
  lines_middle = paste(lines_middle, collapse = "")
  paste0(paste(c(paste0("\n\\[\n\\design_k = \\tiny", header), blockstart, lines_middle, blockend), collapse = "\n"), "\n\\]\n\\normalsize")
}

template = "
\\begin{frame}{B/P-spline base learner}
  \\vspace{-0.3cm}\\[g_k(x) = (B_{k,1}(x), \\dots, B_{k,d_k}(x))^\\tran\\] B-spline basis $B$ of a pre-defined degree~\\citep{eilers1996flexible}.
  \\begin{center}
    \\begin{figure}
      \\includegraphics[width=0.7\\textwidth]{%s/fig-bs%s.png}
    \\end{figure}
  \\end{center}
  \\vspace{-0.3cm}
  \\input{tex/tex-bmat%s.tex}
  %s
\\end{frame}
"

ofile = c()
for (i in seq_along(idxs)) {
  add_colors = rep(FALSE, length(idxs))
  add_colors[i] = TRUE
  lines = pasteBlock(idxs, add_colors = add_colors, dots_after = 3, empty_after = i)
  writeLines(lines, here::here(sprintf("tex/tex-bmat%s.tex", idxs[i])))
  if (i == 1) {
    counter_string = ""
  } else {
    counter_string = "\\addtocounter{framenumber}{-1}"
  }
  ofile = c(ofile, sprintf(template, FIG_DIR_BS, idxs[i], idxs[i], counter_string))
}
writeLines(ofile, here::here(sprintf("tex/tex-bmat-anim.tex")))


## Categorical base learner:
## ===================================================================

FIG_DIR_CAT = sprintf("%s/%s", FIG_DIR, "bs-cat")
dir.create(FIG_DIR_CAT, recursive = TRUE)

xn = "Country"

cboost = Compboost$new(data = dat, target = target)
cboost$addBaselearner(xn, "cat", BaselearnerCategoricalRidge)
cboost$train(ITER_MAX)

vcols_raw = ggsci::pal_aaas()(length(country_codes))
vcols = substr(vcols_raw, 2, 7)

ccode = seq_along(country_codes)
names(ccode) = country_codes
margin = 0.1
df_base = data.frame(country = country_codes,
  xlow = ccode - 0.5 + margin / 2,
  xup = ccode + 0.5 - margin / 2,
  x = ccode,
  y = 1,
  ypred = cboost$getCoef()[["Country_cat"]][country_codes, ],
  color = vcols_raw,
  hexc = vcols,
  label = sprintf("g[paste(k, \",\", %s)]", ccode),
  xlabel = sprintf("x = %s", country_codes)
)

ggCat = function(x = NULL, i = "i") {
  if (is.null(x)) {
    alpha = 1
  } else {
    alpha = rep(0.3, nrow(df_base))
    alpha[df_base$country == x] = 1
  }
  gg = ggplot(df_base) +
    geom_segment(aes(x = xlow, xend = xup, y = y, yend = y, color = country),
      linewidth = 2, alpha = alpha, show.legend = FALSE) +
    geom_text(aes(x = x, y = y, label = label), parse = TRUE, vjust = -0.5) +
    labs(color = "", x = "Country", y = "Basis") +
    ylim(0, 2) +
    scale_color_manual(values = vcols_raw) +
    scale_x_continuous(breaks = df_base$x, labels = df_base$country)

  if (! is.null(x)) {
    dfx = df_base[df_base$country == x, ]
    dfx$xlabel = sprintf("paste(x^{(%s)} == %s)", i, x)
    gg = gg +
      geom_label(data = dfx, mapping = aes(x = x, y = 0.5, label = xlabel),
        fill = "black", color = "white", fontface = "bold", parse = TRUE)
  }
  return(gg + theme_minimal())
}
ggCat("GER")

ggsave(ggCat(), filename = here::here(FIG_DIR_CAT, "fig-cat0.png"), width = 7, height = 2.5)

idx = c(67, 1, 23, 56, 40, 56)
is = c(1, 2, 3, 73, 74, 75)
dat$Country[idx]
for (i in seq_along(idx)) {
  ggsave(ggCat(dat$Country[idx[i]], is[i]),
    filename = here::here(sprintf("%s/fig-cat%s.png", FIG_DIR_CAT, i)),
    width = 7, height = 2.5)
}

## Tensor base learner:
## ===================================================================

dat = dat_raw %>%
  filter(Country %in% countries) %>%
  select(Country, BMI, Alcohol, Life.expectancy) %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

cboost = Compboost$new(dat, target, optimizer = OptimizerCoordinateDescent$new(4),
  learning_rate = 0.2)
cboost$addTensor("Country", "BMI", df1 = 5, df2 = 3)
cboost$addTensor("BMI", "Alcohol", df = 4)
cboost$train(500)

tab = names(table(cboost$getSelectedBaselearner()))
tab

library(patchwork)

gg1 = plotTensor(cboost, tab[1])
gg2 = plotTensor(cboost, tab[2])

vcols_raw = ggsci::pal_aaas()(length(country_codes))
colorcode = data.frame(base = sort(country_codes), color = vcols_raw)

tmp = lapply(country_codes, function(cc) {
  dtmp = gg2$data %>% filter(Country == cc)
  clr = colorcode$color[colorcode$base == cc]
  yl = sprintf("expression(b[\"l, %s\"])", cc)
  gg = ggplot(dtmp, aes(x = BMI, y = y)) +
    geom_line(color = clr, linewidth = 1.2) +
    ggtitle(cc) +
    ylim(range(gg2$data$y)) +
    ylab(eval(parse(text = yl))) +
    theme_minimal() +
    theme(plot.title = element_text(color = clr, face = "bold"))
  ggsave(gg, filename = here::here("figures/bs-tensor", sprintf("fig-tensor-%s.png", cc)),
    width = 2, height = 1.4)
})


gg1 = plotTensor(cboost, tab[1]) + theme_minimal() + theme(legend.position = "bottom") +
  ylab("Alcohol") + theme(legend.text = element_text(size = 6), legend.key.size = unit(0.4,"line"))
ggsave(gg1, filename = here::here("figures/bs-tensor/fig-num-num.png"), width = 4, height = 2)

gg2 = plotTensor(cboost, tab[2]) + theme_minimal() + ggsci::scale_color_aaas() + ylab(expression(b[l]))
ggsave(gg2, filename = here::here("figures/bs-tensor/fig-cat-num.png"), width = 4, height = 2)
