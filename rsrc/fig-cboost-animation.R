library(ggplot2)
library(ggsci)
library(patchwork)

devtools::load_all("~/repos/compboost")

iter_max = 150L
cboost = boostSplines(iris, "Sepal.Length", learning_rate = 0.1, df = 3, iterations = iter_max)

getPlot = function(iter) {
  cboost$train(iter_max)

  bltrace = cboost$getSelectedBaselearner()
  blnames = sort(unique(bltrace))
  bltint  = vapply(bltrace, function(bn) which(bn == blnames), integer(1), USE.NAMES = FALSE)
  df_bl = data.frame(iter = seq_along(bltint), blsel = bltint, bln = bltrace)
  df_bl = df_bl[seq_len(iter), ]

  cols = pal_aaas()(length(blnames))
  df_cols = data.frame(bln = blnames, color = cols[c(1,3,2,4)])

  ilab = iter
  if (iter == 1) ilab = 0
  ggt = ggplot(df_bl, aes(xmin = iter - 1, xmax = iter, ymin = blsel - 0.4, ymax = blsel + 0.4, fill = bln)) +
    geom_rect(color = "white", show.legend = FALSE, linewidth = 0.05) +
    #scale_color_aaas() +
    #scale_fill_aaas() +
    scale_fill_manual(values = cols) +
    scale_y_continuous(breaks = seq_along(blnames), labels = blnames, limits = c(0.5, length(blnames) + 0.5)) +
    xlim(c(0.5, iter_max + 0.5)) +
    xlab("Iteration") +
    ylab("") +
    ggtitle(sprintf("Model after %s iterations", ilab))

  cboost$train(iter)

  pbl = function(bln, cboost, npoints = 100L) {
    gg = try(plotBaselearner(cboost, bln), silent = TRUE)
    if (inherits(gg, "try-error")) {
      itmp = cboost$getCurrentIteration()
      cboost$train(iter_max)
      dat = plotBaselearner(cboost, bln)$data
      dat$y = 0
      cboost$train(itmp)
      alpha = 0.3
    } else {
      dat = gg$data
      alpha = 1
      if (iter == 1) alpha = 0.3
    }
    if (iter == 1) dat$y = 0

    cl = df_cols$color[df_cols$bln == bln]
    f = cboost$baselearner_list[[bln]]$factory
    #yl = sprintf("expression(paste(b[%s]))", f$getFeatureName())

    if (is.numeric(dat$x)) {
      gg = ggplot(data = dat, aes(x = x, y = y, color = bln)) +
        geom_line(show.legend = FALSE, alpha = alpha, linewidth = 1.5) +
        scale_color_manual(values = cl)
    } else {
      dat$i = seq_len(nrow(dat))
      gg = ggplot(data = dat, aes(x = i - 0.4, xend = i + 0.4, y = y, yend = y, color = bln)) +
        geom_segment(show.legend = FALSE, alpha = alpha, linewidth = 1.2) +
        scale_color_manual(values = cl) +
        scale_x_continuous(breaks = dat$i, labels = dat$x)
    }

    return(gg + ggtitle(bln) + xlab(f$getFeatureName()) + ylab(""))
  }

  ggs = lapply(blnames, pbl, cboost = cboost)
  ggbls = (ggs[[1]] + ylim(-1, 2)) +
    (ggs[[2]] + ylim(-0.125, 0.12)) +
    (ggs[[3]] + ylim(-0.375, 0.675)) +
    (ggs[[4]] + ylim(-0.05, 0.05))

  gg = ggt / ggbls + plot_layout(heights = c(1, 5))

  cboost$train(iter_max)

  return(gg)
}

iters = c(1, 5, 10, 15, 20, 30, 50, 70, 90, 110, 130, 140, 145, 150)#, 150, 200, 250, 300, 400, 500, 750, 800, 850, 900, 950, 980, 990, 995, 1000)
slt = character()
for (i in iters) {
  gg = getPlot(i) & theme_minimal()
  fn = sprintf(here::here("figures/fig-iter-%s.png"), stringr::str_pad(i, 4, pad = 0))
  suppressWarnings(ggsave(plot = gg, filename = fn, width = 7, height = 5))

  if (i == iters[1]) {
    pc = "\\addtocounter{framenumber}{0}"
  } else {
    pc = "\\addtocounter{framenumber}{-1}"
  }
  message(sprintf("Save figure %s", fn))
  slt = c(slt, sprintf("\n\\begin{frame}{Component-wise gradient boosting -- Example}\n\t\\begin{figure}\n\t\t\\centering\n\t\t\\includegraphics[width=\\textwidth]{%s}\n\t\\end{figure}\n\t%s\n\\end{frame}\n", fn, pc))
  writeLines(slt, con = here::here("figures/fig-cwb-anim.tex"))
}
