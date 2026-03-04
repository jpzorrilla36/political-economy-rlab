# title: "RDD"
# author: "Eduardo Zago, Manuel Quintero and Juan Pablo Zorrilla"
# date: "`r Sys.Date()`"
# output: html_document

# Packages — rdd and rddtools removed (defunct)
list.of.packages <- c("stargazer", "tidyverse", "rdrobust", "haven", "fixest",
                      "varhandle", "lfe", "rddensity")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

set.seed(4)

# Generate data
x <- runif(100, -2, 2)
y <- 1 + x + 5*(x >= 0) + rnorm(100, 0, 1)
z <- ifelse(x > 0, 1, 0)
rd_data <- cbind.data.frame(y, x, z)

# ── McCrary / density test ──────────────────────────────────────────────────
# rddensity() is the modern equivalent (Calonico, Cattaneo, Titiunik 2018).

rdbwdensity(rd_data$x, c = 0, p = 1)
rdd2 <- rddensity(rd_data$x, c=0, p=1)
summary(rdd2)

# ── Density plot ────────────────────────────────────────────────────────────
# rdplotdensity() is the direct replacement.

set.seed(42)
plot2 <- rdplotdensity(rdd2, rd_data$x,
                       plotRange = c(-2, 2),
                       plotN = 25,
                       CIuniform = TRUE)

# Bandwidth selection 
rdbwdensity(rd_data$x, c = 0, p = 1)

# ── Manual RD with felm  ─────────────────────────────────────────
rd_data_filter <- rd_data |> filter(x < 1 & x > -1)
rd1 <- felm(y ~ z + z*x, data = rd_data_filter)
rd1$coefficients

ggplot(rd_data_filter, aes(x, y, color = factor(z))) +
  geom_point() +
  geom_smooth(linewidth = 1.5) +  
  geom_vline(xintercept = 0, linetype = "longdash") +
  theme_bw() +
  xlab("x") + ylab("y") +
  scale_color_discrete(name = "T")


# ── Calonico et al. optimal bandwidth  ────────────────────────────
rdbwselect(rd_data$y, rd_data$x, c = 0,
           kernel = "triangular", bwselect = "mserd", vce = "nn")

reg_cct <- rdrobust(rd_data$y, rd_data$x, all = TRUE)
summary(reg_cct)

rdplot(rd_data$y, rd_data$x,
       x.lab = "Running Variable (x)",
       y.lab = "Dependent Variable (y)", title = "")

# ── Pre/Post comparison  ──────────────────────────────────────────
set.seed(5)
x <- runif(100, -2, 2)
y <- 1 + x + 8*(x >= 0) + rnorm(100, 0, 1)
z <- ifelse(x > 0, 1, 0)

rd_data_post <- cbind.data.frame(y, x, z) |> mutate(post = 1)
rd_data      <- rd_data |> mutate(post = 0)
final        <- rbind(rd_data, rd_data_post)

final_pre  <- final |> filter(post == 0)
final_post <- final |> filter(post == 1)

reg_pre <- rdrobust(final_pre$y, final_pre$x, all = TRUE)
summary(reg_pre)

reg_post <- rdrobust(final_post$y, final_post$x, all = TRUE)
summary(reg_post)
