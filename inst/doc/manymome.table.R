## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = ""
)

## ----dataset_me---------------------------------------------------------------
library(manymome)
dat <- data_serial
print(head(dat), digits = 3)

## -----------------------------------------------------------------------------
library(lavaan)
mod_med <- "
m1 ~ x
m2 ~ m1 + x
y ~ m2 + m1 + x
"
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)

## -----------------------------------------------------------------------------
all_paths <- all_indirect_paths(fit = fit_med,
                                x = "x",
                                y = "y")
all_paths

## -----------------------------------------------------------------------------
# R set to 100 just for illustration.
# Use 5000 or 10000 and set parallel to TRUE in real research.
out_all <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE,
                                 boot_ci = TRUE,
                                 R = 100,
                                 seed = 12345,
                                 parallel = FALSE,
                                 progress = FALSE)
out_all

## -----------------------------------------------------------------------------
library(manymome.table)
ft_all <- as_flextable(out_all)
ft_all

## ----dataset------------------------------------------------------------------
dat <- data_med_mod_ab
print(head(dat), digits = 3)

## -----------------------------------------------------------------------------
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
lm_m <- lm(m ~ x*w1, dat)
lm_y <- lm(y ~ m*w2 + x, dat)
lm_out <- lm2list(lm_m, lm_y)

## ----cond_indirect------------------------------------------------------------
# R set to 100 just for illustration.
# Use 5000 or 10000 and set parallel to TRUE in real research.
out_cond <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                  x = "x",
                                  y = "y",
                                  m = "m",
                                  fit = lm_out,
                                  standardized_x = TRUE,
                                  standardized_y = TRUE,
                                  boot_ci = TRUE,
                                  R = 100,
                                  seed = 12345,
                                  parallel = FALSE,
                                  progress = FALSE)
out_cond

## -----------------------------------------------------------------------------
library(manymome.table)
ft_cond <- as_flextable(out_cond)
ft_cond

## -----------------------------------------------------------------------------
library(flextable)
ft_cond2 <- ft_cond |>
              bold(part = "header") |>
              bg(i = c(1, 2), bg = "lightblue", part = "body") |>
              bg(i = c(3, 4), bg = "lightgreen", part = "body")
ft_cond2

