# ============================================================
# Regression implementation for the thesis
# ============================================================
#
# This script documents the R implementation of the baseline
# difference-in-differences, event-study, heterogeneity, and
# robustness specifications used in the thesis.
#
# The final analysis dataset is:
# data/panel_hts8_month_analysis_final_r_ready.csv
# ============================================================

# ============================================================
# Code excerpt G2. Regression implementation
# ============================================================

library(readr)
library(dplyr)
library(fixest)

# ------------------------------------------------------------
# 1. Import final analysis dataset
# ------------------------------------------------------------

df <- read_csv(
  "data/panel_hts8_month_analysis_final_r_ready.csv",
  col_types = cols(
    month = col_date(),
    hts8 = col_character(),
    treat = col_integer(),
    is_control_removed = col_integer(),
    interim = col_integer(),
    post = col_integer(),
    total_value = col_double(),
    china_value = col_double(),
    nonchina_value = col_double(),
    china_share = col_double(),
    ln_china_value = col_double(),
    ln_nonchina_value = col_double(),
    nsuppliers_nonchina_pre = col_double(),
    hhi_nonchina_pre = col_double(),
    limited_options_count = col_integer(),
    limited_options_hhi = col_integer(),
    nonchina_pre_total = col_double(),
    no_nonchina_pre = col_integer(),
    drop_july2018 = col_integer(),
    period_bucket = col_character()
  )
)

# Month identifier used for fixed effects and event-study interactions
df <- df %>%
  mutate(
    month_id = format(month, "%Y-%m")
  )

# ------------------------------------------------------------
# 2. Baseline difference-in-differences variables
# ------------------------------------------------------------

df <- df %>%
  mutate(
    treat_interim = treat * interim,
    treat_post = treat * post
  )

# ------------------------------------------------------------
# 3. Baseline difference-in-differences models
# ------------------------------------------------------------

did_share <- feols(
  china_share ~ treat_interim + treat_post | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

did_ln_china <- feols(
  ln_china_value ~ treat_interim + treat_post | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

did_ln_nonchina <- feols(
  ln_nonchina_value ~ treat_interim + treat_post | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

# ------------------------------------------------------------
# 4. Event-study models
# ------------------------------------------------------------

es_share <- feols(
  china_share ~ i(month_id, treat, ref = "2018-03") | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

es_ln_china <- feols(
  ln_china_value ~ i(month_id, treat, ref = "2018-03") | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

es_ln_nonchina <- feols(
  ln_nonchina_value ~ i(month_id, treat, ref = "2018-03") | hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

# ------------------------------------------------------------
# 5. Heterogeneity by pre-treatment non-China source-country breadth
# ------------------------------------------------------------

df <- df %>%
  mutate(
    lo_count = limited_options_count,
    lo_interim = lo_count * interim,
    lo_post = lo_count * post,
    treat_lo_interim = treat * lo_count * interim,
    treat_lo_post = treat * lo_count * post
  )

ddd_share_count <- feols(
  china_share ~
    treat_interim + treat_post +
    lo_interim + lo_post +
    treat_lo_interim + treat_lo_post |
    hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

ddd_ln_china_count <- feols(
  ln_china_value ~
    treat_interim + treat_post +
    lo_interim + lo_post +
    treat_lo_interim + treat_lo_post |
    hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

ddd_ln_nonchina_count <- feols(
  ln_nonchina_value ~
    treat_interim + treat_post +
    lo_interim + lo_post +
    treat_lo_interim + treat_lo_post |
    hts8 + month_id,
  data = df,
  cluster = ~ hts8
)

# ------------------------------------------------------------
# 6. HHI-based heterogeneity robustness
# ------------------------------------------------------------

df <- df %>%
  mutate(
    lo_hhi = limited_options_hhi,
    lo_hhi_interim = lo_hhi * interim,
    lo_hhi_post = lo_hhi * post,
    treat_lo_hhi_interim = treat * lo_hhi * interim,
    treat_lo_hhi_post = treat * lo_hhi * post
  )

ddd_share_hhi <- feols(
  china_share ~
    treat_interim + treat_post +
    lo_hhi_interim + lo_hhi_post +
    treat_lo_hhi_interim + treat_lo_hhi_post |
    hts8 + month_id,
  data = df,
  cluster = ~ hts8
)
