# File:       A0_Params_Init.R
# Finalized:  2023-12-01
# Author:     Hans-Aloys Wischmann

# create a clean slate
rm(list = ls())

# load libraries
library(tidyverse)
library(flextable)
library(doParallel)
library(foreach)
library(doRNG)
library(pROC)

# configurable parameters: sample sizes, number of simulations
n_simulation_runs    <- 10000L  # number of simulation runs
n_sample_development <-  5000L  # sub-sample of the target population, for development
n_sample_validation  <- 50000L  # sub-sample of the target population, for validation

# configurable parameters: cutoff working points (typical, do not change without reason)
cutoff_spec_high <- pnorm(2.0)  # 97.7 %
cutoff_spec_low  <- pnorm(1.0)  # 84.1 %
cutoff_sens      <- pnorm(1.0)  # 84.1 %

# realistic test performance and analysis cutoffs (extracted from literature, do not change without reason)
score_offset_mci    <-  -5.1   # realistic value for test performance offset for MCI patients compared to healthy individuals
score_offset_dem    <- -10.7   # realistic value for test performance offset for dementia patients compared to healthy individuals
score_individual_SD <-   2.9   # realistic value for random individual-specific variability in test performance

# available census data, choice of age range for target population (do not change without reason)
pop_census  <- 2022L  # census date (population data per Jan 1st of following year, compiled in census year)
pop_age_min <-   55L  # minimum age at end of census year (target population)
pop_age_max <-   89L  # maximum age at end of census year (target population)

# minimum and maximum age to use for extracting education by age and sex
edu_age_min <- 30L    # ignore younger cohorts, as 25-29 year olds may not have not completed their education yet
edu_age_max <- 64L    # ignore oldest cohort, as 65+ integrates over too much of our target population

# derived parameters (do not edit)
pop_yob_min <- pop_census - pop_age_max # YOB for oldest people in census data
pop_yob_max <- pop_census - pop_age_min # YOB for youngest people in census data

# document (configurable and derived) parameters
params_used <- unlist(mget(ls()))

# derived 5-year brackets for YOB (do not edit)
yob_table <- data.frame(yob_from = as.integer(seq(pop_yob_min, pop_yob_max, 5))) %>%
  mutate(yob_to = yob_from + 4, yob_midp = yob_from + 2, yob_range = sprintf("%d-%d", yob_from, yob_to))

# derived table of specificity and sensitivity cutoffs to explore (do not edit)
sens_spec_cutoffs <- data.frame(sens_or_spec = c("Specificity", "Specificity", "Sensitivity"), value = c(cutoff_spec_high, cutoff_spec_low, cutoff_sens)) %>%
  mutate(label = sprintf("%s = %.1f %%", str_sub(sens_or_spec, 1, 4), 100.0 * value))

# ensure consistency across systems, define cutoffs, set default figure size
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")
knitr::opts_chunk$set(echo = FALSE, dpi = 1200)
knitr::opts_knit$set(root.dir = getwd())

# use all but two logical cores when processing in parallel
registerDoParallel(cores = detectCores(logical = TRUE) - 2)

# read/write csv files in "UTF-8" encoding with semi-colon as separator, without row numbers
load_csv <- function(file) {
  return(read.csv(file, sep = ";", encoding = "UTF-8"))
}
save_csv <- function(object, file) {
  return(write.table(object, file, sep = ";", fileEncoding = "UTF-8", quote = FALSE, row.names = FALSE))
}

# plot to *.pdf file and inline (indirectly via png), set default size to 5.25 in x 5.25 in
ggplot_font_size = 10 # font size for all texts except for geom_text, in points
update_geom_defaults("text", list(size = ggplot_font_size * 0.35)) # font size for geom_text, in mm
plot_pdf <- function(object, file, width = 5.25, height = 5.25) {
  themed_object <- object + theme(text = element_text(size = ggplot_font_size), plot.title = element_text(size = ggplot_font_size))
  pdf(file, width, height, paper = "a4")
  print(themed_object)
  dev.off()
  png_file <- str_replace(file, ".pdf", ".png")
  png(png_file, width, height, units = "in", res = 1200)
  print(themed_object)
  dev.off()
  knitr::include_graphics(png_file, dpi = 1200)
}

# document parameters
document_params <- function() {
  data.frame(Parameter = names(params_used), Value = gsub("[.]0*$", "", round(params_used, 3))) %>%
    flextable() %>%
      align(., i = NULL, j = 2, "right") %>%
      autofit(.)
}
