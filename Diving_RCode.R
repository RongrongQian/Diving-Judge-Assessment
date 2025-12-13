
# From Scores to Trust: A Statistical Quality Assessment for Diving Judges
# ---Quantifying Rater Effects, Bias, and Reliability using Summary Data
# Author: 
# - "Rongrong Qian (qianr5@miamioh.edu)"
# - "Jing Jing (jingj4@miamioh.edu)"
# - "Charntel Kazingizi (kazingcr@miamioh.edu)"
# - "Sagar Pangeni (pangens@miamioh.edu)"
# - "Advisor: Michael Hughes (hughesmr@miamioh.edu)"
# Date: "2025-12-05"



## ----setup----------------------------------------------------------------------
# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(purrr)
library(kableExtra)
library(lme4)
library(tidyr)
library(shiny)
library(fmsb)
library(tibble)


## ----clean_data-----------------------------------------------------------------
# please set your own file path first
fp <- "/Users/..."

# read data
diving_2024_raw <- read_csv(file.path(fp, "diving_data_2024.csv"), show_col_types = FALSE) 
diving_2025_raw <- read_csv(file.path(fp, "diving_data_2025.csv"), show_col_types = FALSE) 

# filter NA judge_id and add a column year
diving_2024 <- diving_2024_raw %>%
  mutate(year = 2024)

diving_2025 <- diving_2025_raw %>%
  mutate(year = 2025)

# combine two years data
diving_data <- bind_rows(diving_2024, diving_2025)


## ----remove_duplicate-----------------------------------------------------------
# remove exact duplicate rows across ALL columns
n_before <- nrow(diving_data)
diving_data <- dplyr::distinct(diving_data)  
n_after  <- nrow(diving_data)

message(sprintf("Removed %d duplicate rows; %d rows remain.",
                n_before - n_after, n_after))

# check for repeated judge–dive combinations
dup_judge_dive <- diving_data %>%
  filter(!is.na(judge_id)) %>%          
  count(year, meet_id, event_id, judge_id, submitted_dive_id) %>%
  filter(n > 1)

## ----flag_anon_judges-----------------------------------------------------------
# create is_anonymous_judge flag
diving_data <- diving_data %>%
  mutate(
    is_anonymous_judge = is.na(judge_id)
  )


## ----Table1: Summary of Data---------------------------------------------------
# Total number of observations: each row is one judge–dive score
n_total <- nrow(diving_data)

# Number of observations by year
obs_by_year <- diving_data %>%
  count(year, name = "n_obs")%>%
  arrange(year)
# Count unique identified judges (non-anonymous, has judge_id)
n_identified_judges <- diving_data %>%
  filter(!is_anonymous_judge, !is.na(judge_id)) %>%
  summarise(n_judges = n_distinct(judge_id)) %>%
  pull(n_judges)

# Summary of anonymous vs identified judge scores
anon_summary <- diving_data %>%
  count(is_anonymous_judge, name = "n") %>%
  mutate(
    label = ifelse(is_anonymous_judge, "Anonymous", "Identified"),
    prop  = n / sum(n)
  )

# Count unique divers
n_dives <- diving_data %>%
  summarise(n_dives = n_distinct(diver_id)) %>%
  pull(n_dives)

# Count unique divers by gender (standardize gender labels)
diver_counts <- diving_data %>%
  filter(!is.na(diver_id), !is.na(diver_gender)) %>%        # keep valid IDs and genders
  mutate(
    diver_gender = case_when(
      tolower(as.character(diver_gender)) == "female" ~ "Female",
      tolower(as.character(diver_gender)) == "male"   ~ "Male",
      TRUE ~ as.character(diver_gender)
    )
  ) %>%
  distinct(diver_id, diver_gender) %>%                      # one row per diver
  count(diver_gender, name = "n_divers")                    # count divers in each gender

# Number of distinct meets by meet type (each meet_id counted once)
meets_by_type <- diving_data %>%
  filter(!is.na(meet_id), !is.na(meet_type)) %>%
  distinct(meet_id, meet_type) %>%
  count(meet_type, name = "n_meets") %>%
  arrange(desc(n_meets))

# Count unique teams (diver_team_id)
n_teams <- diving_data %>%
  summarise(n_teams = n_distinct(diver_team_id)) %>%
  pull(n_teams)

# Count unique events (event_id)
n_events <- diving_data %>%
  summarise(n_events = n_distinct(event_id)) %>%
  pull(n_events)

# table 1
# Observations section 
table_obs <- tibble::tibble(
  Category = c("Observations", rep("", nrow(obs_by_year))),
  Metric   = c(
    "Total judge–dive scores",
    paste0("Judge–dive scores (", obs_by_year$year, ")")
  ),
  Value    = c(
    n_total,
    obs_by_year$n_obs
  )
)

# Judges section 
n_identified_scores <- anon_summary %>%
  dplyr::filter(label == "Identified") %>%
  dplyr::pull(n)

n_anonymous_scores <- anon_summary %>%
  dplyr::filter(label == "Anonymous") %>%
  dplyr::pull(n)

table_judges <- tibble::tibble(
  Category = c("Judges", "", ""),
  Metric   = c(
    "Total identified judges",
    "Identified score records",
    "Anonymous score records"
  ),
  Value    = c(
    n_identified_judges,
    n_identified_scores,
    n_anonymous_scores
  )
)

# Divers section
diver_metric_labels <- ifelse(
  diver_counts$diver_gender %in% c("Female", "Male"),
  paste0(diver_counts$diver_gender, " divers"),
  paste0("Divers (", diver_counts$diver_gender, ")")
)

table_divers <- tibble::tibble(
  Category = c("Divers", rep("", length(diver_metric_labels))),
  Metric   = c("Total distinct divers", diver_metric_labels),
  Value    = c(n_dives, diver_counts$n_divers)
)

# Meets section
n_meets_total <- sum(meets_by_type$n_meets)

meet_metric_labels <- paste0(meets_by_type$meet_type, " meets")

table_meets <- tibble::tibble(
  Category = c("Meets", rep("", length(meet_metric_labels))),
  Metric   = c("Total distinct meets", meet_metric_labels),
  Value    = c(n_meets_total, meets_by_type$n_meets)
)

# Events & Teams
table_events <- tibble::tibble(
  Category = "Events",
  Metric   = "Total distinct events",
  Value    = n_events
)

table_teams <- tibble::tibble(
  Category = "Teams",
  Metric   = "Total distinct teams",
  Value    = n_teams
)

# Combine all pieces into one table
table1 <- dplyr::bind_rows(
  table_obs,
  table_judges,
  table_divers,
  table_meets,
  table_events,
  table_teams
) %>%
  dplyr::mutate(
    # add thousands separator
    Value = format(Value, big.mark = ","),
    # bold totals (rows where Metric starts with "Total")
    Value = ifelse(grepl("^Total", Metric),
                   paste0("**", Value, "**"),
                   Value)
  )

knitr::kable(
  table1,
  col.names = c("Category", "Metric", "Value"),
  caption   = "Table1: Summary of Data Used in the Judge–dive Analysis",
  escape    = FALSE
)


## ----Table 2: Distribution of Panel Sizes---------------------------------------
# For each event × diver: how many distinct judges 
diver_event_panel <- diving_data %>%
  group_by(year, meet_id, event_id, submitted_dive_id) %>%
  summarise(
    n_judges = n_distinct(judge_number, na.rm = TRUE),
    .groups = "drop"
  )

# Distribution of panel sizes at the event–diver level
panel_size_summary <- diver_event_panel %>%
  count(n_judges, name = "n_dives") %>%
  arrange(n_judges) %>%
  mutate(
    prop = n_dives / sum(n_dives),
    dives_label = paste0(
      n_dives, " (", scales::percent(prop, accuracy = 0.1), ")"
    )
  )

panel_size_summary %>%
  select(n_judges, dives_label) %>%
  kable(
    col.names = c("Number of Judges", "Number of Dives"),
    caption   = "Table 2: Distribution of Panel Sizes (Judges per Dive).",
    digits    = 2
  )



## ----Figure 1: Distribution of Raw Awards (2024-2025)---------------------------
# Overall raw score distribution (histogram)
ggplot(diving_data, aes(x = raw_award)) +
  geom_histogram(
    binwidth = 0.5,          
    boundary = 0,            
    closed = "left",
    aes(y = after_stat(count)),
    fill = "grey30",
    color = "black",
    alpha = 0.8
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 0.5)  
  ) +
  labs(
    x = "Raw Award (Judge Score)",
    y = "Count",
    title = "Figure 1: Distribution of Raw Awards (2024-2025)"
  )



## --------------Figure 2: Raw Awards by Meet Type--------------------------------
ggplot(diving_data, aes(x = meet_type, y = raw_award)) +
  geom_boxplot() +
  stat_summary(
    fun  = mean,
    geom = "point",
    shape = 23,       
    size  = 3,
    fill  = "white",  
    color = "black"   
  ) +
  labs(
    x = "Meet type",
    y = "Raw award (judge score)",
    title = "Figure 2: Raw Awards by Meet Type"
  )



## ----Figure 3: Raw Awards vs. Degree of Difficulty------------------------------
ggplot(diving_data, aes(x = dd, y = raw_award)) +
  # lighter, smaller points
  geom_point(alpha = 0.15, size = 0.7, colour = "grey40") +
  # smooth trend line
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  labs(
    x = "Degree of Difficulty (dd)",
    y = "Raw award (judge score)",
    title = "Figure 3: Raw Awards vs. Degree of Difficulty"
  ) +
  # cleaner theme
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust = 0.5)  
  )



## ------------Figure 4: Score Distribution by Diver Gender-----------------------
# clean diver gender before plotting
diving_gender_clean <- diving_data %>%
  mutate(
    diver_gender = stringr::str_to_lower(diver_gender),
    diver_gender = dplyr::recode(
      diver_gender,
      "female" = "Female",
      "male"   = "Male"
    )
  ) %>%
  filter(diver_gender %in% c("Female", "Male"))  # drop deleted / not specified

## Score distribution by diver gender (boxplot + mean diamond)
ggplot(diving_gender_clean, aes(x = diver_gender, y = raw_award)) +
  geom_boxplot() +
  stat_summary(
    fun  = mean,
    geom = "point",
    shape = 23,      
    size  = 3,
    fill  = "white",
    color = "black"
  ) +
  labs(
    x = "Diver Gender",
    y = "Raw award (Judge Score)",
    title = "Figure 4: Score Distribution by Diver Gender"
  )



## ----------Table 3: Top 10 Judges by SD of Scores-------------------------------
# restrict to identified judges only
judge_summary <- diving_data %>%
  filter(!is_anonymous_judge, !is.na(judge_id)) %>%   # keep non-anonymous judges
  group_by(judge_id, judge_user_fullname) %>%        # judge-level group
  summarise(
    n_scores   = n(),
    mean_score = mean(raw_award, na.rm = TRUE),
    sd_score   = sd(raw_award,   na.rm = TRUE),
    .groups    = "drop"
  )

# small table (top 10 by number of scores)
judge_summary %>%
  arrange(desc(sd_score)) %>%
  slice_head(n = 10) %>%
  kable(
    digits  = 2,
    caption = "Table 3: Top 10 Judges by SD of Scores"
  )


## -------------Figure 5: Per-judge Severity and Stability------------------------
# scatter: x = mean score, y = variability
ggplot(judge_summary, aes(x = mean_score, y = sd_score)) +
  geom_point(aes(size = n_scores), alpha = 0.6) +   # each point = one judge
  scale_size_continuous(name = "Number of Scores") +
  labs(
    x = "Judge Mean Score (Severity / Leniency)",
    y = "Judge SD of Scores (Stability)",
    title = "Figure 5: Per-judge Severity and Stability"
  ) +
  theme_minimal(base_size = 12)


## ----Helper_function------------------------------------------------------------
# This subsection applies all metrics to every dive in the cleaned dataset. For each judge–dive–panel combination, it computes a leave-one-out baseline, residual, panel volatility, standardized residual, and extremeness indicators, while retaining anonymous judges so that all available scores inform the panel structure.

# Epsilon to stabilize SD when panel variation is near zero
eps_sd <- 0.10

# Compute per-panel metrics from a vector of scores
compute_panel_metrics <- function(scores) {
  n           <- length(scores)
  n_nonmissing <- sum(!is.na(scores))

  # Initialize with NA
  baseline_loo  <- rep(NA_real_, n)
  sd_others_raw <- rep(NA_real_, n)
  is_max        <- rep(NA_integer_, n)
  is_min        <- rep(NA_integer_, n)
  gap_next      <- rep(NA_real_, n)

  # Require at least 3 non-missing scores
  if (n_nonmissing < 3) {
    return(list(
      baseline_loo  = baseline_loo,
      sd_others_raw = sd_others_raw,
      is_max        = is_max,
      is_min        = is_min,
      gap_next      = gap_next
    ))
  }

  # Work only with non-missing values
  idx_nonmiss <- which(!is.na(scores))
  s_nonmiss   <- scores[idx_nonmiss]

  # unique max / min and edge gap 
  max_score <- max(s_nonmiss)
  min_score <- min(s_nonmiss)
  which_max <- idx_nonmiss[s_nonmiss == max_score]
  which_min <- idx_nonmiss[s_nonmiss == min_score]

  # Unique maximum
  if (length(which_max) == 1) {
    is_max[which_max] <- 1L
    oth <- scores[-which_max]
    oth <- oth[!is.na(oth)]
    gap_next[which_max] <- scores[which_max] - max(oth)
  }

  # Unique minimum
  if (length(which_min) == 1) {
    is_min[which_min] <- 1L
    oth <- scores[-which_min]
    oth <- oth[!is.na(oth)]
    gap_next[which_min] <- min(oth) - scores[which_min]
  }

  #  LOO baseline and SD of others 
  for (i in idx_nonmiss) {
    others <- scores[-i]
    others <- others[!is.na(others)]

    baseline_loo[i]  <- mean(others)
    sd_others_raw[i] <- sd(others)
  }

  list(
    baseline_loo  = baseline_loo,
    sd_others_raw = sd_others_raw,
    is_max        = is_max,
    is_min        = is_min,
    gap_next      = gap_next
  )
}


## ----Per-dive_metrics-----------------------------------------------------------
# This subsection applies the metrics to every dive in the cleaned dataset. For each judge–dive–panel combination, it computes a leave-one-out baseline, residual, panel volatility, standardized residual, and extremeness indicators, while retaining anonymous judges so that all available scores inform the panel structure.

# Per-dive metrics: baselines, residuals, volatility, extremeness
dive_level <- diving_data %>%
  group_by(submitted_dive_id) %>%
  group_modify(~ {
    df     <- .
    scores <- df$raw_award

    # Panel-level metrics for this dive
    pm <- compute_panel_metrics(scores)

    df$BL_loo        <- pm$baseline_loo
    df$SD_others_raw <- pm$sd_others_raw
    df$is_max        <- pm$is_max
    df$is_min        <- pm$is_min
    df$gap_next      <- pm$gap_next

    # Stabilized panel SD (add epsilon floor)
    df$SD_others <- ifelse(
      !is.na(df$SD_others_raw),
      pmax(df$SD_others_raw, eps_sd),
      NA_real_
    )

    # Residuals and standardized residuals
    df$residual <- df$raw_award - df$BL_loo
    df$z_score  <- df$residual / df$SD_others

    df
  }) %>%
  ungroup()


## ----Judge-level-Metrics--------------------------------------------------------
# Here, the per-dive quantities are aggregated to identifiable judges using judge id. For each judge, summary statistics describe average severity, consistency, agreement with the panel, range of scores, relative spread, and extremeness across all meets and events.
# In the earlier EDA summary step, the count of 105 judges simply reflects all judges who had an ID and were not marked as anonymous in the data.However, for the judge-level metrics, only judges who have at least one dive with a valid residual are retained. A valid residual requires that the corresponding panel satisfy conditions such as having at least three non-missing scores. If a judge only appears in dives where these conditions are not met, all of that judge’s residuals remain missing, making it impossible to compute severity, inconsistency, or extremeness metrics. There are 5 such judges, so they are excluded from the judge-level summary, leaving 100 judges in the final judge-level analysis.

# Judge-level metrics across panels
judge_level <- dive_level %>%
  # Use only records with defined residuals
  filter(!is.na(residual)) %>%
  # Aggregate only identifiable judges (non-missing judge_id)
  filter(!is.na(judge_id)) %>%
  group_by(judge_id, judge_user_fullname) %>%
  summarise(
    n_dives_used = n(),                      # Number of dives used

    # Severity / leniency
    SEV = mean(residual, na.rm = TRUE),

    # Inconsistency
    INC = sd(residual, na.rm = TRUE),

    # Range utilization: own spread vs panel spread
    SPREAD_j         = sd(raw_award, na.rm = TRUE),
    panel_spread_mean = mean(SD_others, na.rm = TRUE),
    RR = ifelse(
      !is.na(SPREAD_j) & !is.na(panel_spread_mean) & panel_spread_mean > 0,
      SPREAD_j / panel_spread_mean,
      NA_real_
    ),

    # Agreement with panel (Spearman correlation)
    CORR = suppressWarnings(
      cor(raw_award, BL_loo,
          method = "spearman",
          use    = "complete.obs")
    ),

    # Extremeness: frequency and average gap when extreme
    ER_max_raw = mean(is_max == 1L, na.rm = TRUE),
    ER_min_raw = mean(is_min == 1L, na.rm = TRUE),

    ER_max = ifelse(is.nan(ER_max_raw), 0, ER_max_raw),
    ER_min = ifelse(is.nan(ER_min_raw), 0, ER_min_raw),
    ER     = ER_max + ER_min,


    n_extreme = sum(is_max == 1L | is_min == 1L, na.rm = TRUE),
    EGI = ifelse(
      n_extreme > 0,
      sum(gap_next * (is_max == 1L | is_min == 1L), na.rm = TRUE) / n_extreme,
      NA_real_
    ),
    .groups = "drop"
  )



## ----Judge_ranking--------------------------------------------------------------
# In this final step, judge-level statistics are converted into five penalty scores and a composite rank. Smaller penalty values represent better performance, and the combined ranking highlights judges whose scoring is most reliable and least biased with respect to the project goals.

# Integrated performance metrics and composite ranking
judge_performance <- judge_level %>%
  mutate(
    # Five penalty scores (lower = better)
    B1_SEV = abs(SEV),          # Severity penalty: |SEV_j|
    B2_INC = INC,               # Inconsistency penalty
    B3_AGR = 1 - CORR,          # Agreement penalty: 1 - CORR_j

    # Extremeness penalty
    B4_EGI = dplyr::case_when(
      is.na(ER) ~ NA_real_,
      ER == 0   ~ 0,            # Never extreme
      ER  > 0   ~ EGI           # Uses mean edge gap when extreme
    ),

    # Range-use penalty
    B5_RR = dplyr::case_when(
      is.na(RR) ~ NA_real_,
      RR >= 1   ~ RR,
      RR < 1    ~ 1 / RR
    )
  ) %>%
  # Per-metric ranks (smaller penalty = better rank)
  mutate(
    rank_SEV = min_rank(B1_SEV),
    rank_INC = min_rank(B2_INC),
    rank_AGR = min_rank(B3_AGR),
    rank_EGI = min_rank(B4_EGI),
    rank_RR  = min_rank(B5_RR),

    # Composite rank score across five dimensions
    overall_rank_score =
      rank_SEV + rank_INC + rank_AGR + rank_EGI + rank_RR
  ) %>%
  arrange(overall_rank_score, judge_user_fullname)



## -------Composite_Judge_Ranking-------------------------------------------------
judge_performance <- judge_level %>%
  mutate(
    # Five penalty scores (lower = better)
    B1_SEV = abs(SEV),
    B2_INC = INC,
    B3_AGR = 1 - CORR,
    B4_EGI = dplyr::case_when(
      is.na(ER) ~ NA_real_,
      ER == 0   ~ 0,
      ER  > 0   ~ EGI
    ),
    B5_RR = dplyr::case_when(
      is.na(RR) ~ NA_real_,
      RR >= 1   ~ RR,
      RR < 1    ~ 1 / RR
    )
  ) %>%
  mutate(
    rank_SEV = min_rank(B1_SEV),
    rank_INC = min_rank(B2_INC),
    rank_AGR = min_rank(B3_AGR),
    rank_EGI = min_rank(B4_EGI),
    rank_RR  = min_rank(B5_RR),
    overall_rank_score =
      rank_SEV + rank_INC + rank_AGR + rank_EGI + rank_RR
  ) %>%
  mutate(
    overall_rank = min_rank(overall_rank_score)  
  ) %>%
  arrange(overall_rank, judge_user_fullname)



## ------------Table 4: Top 10 Judges by Composite Performance Score--------------
top10_judges <- judge_performance %>%
  # Add a column with the total number of judges
  # (same value for every row, e.g. 100)
  mutate(n_judges = n()) %>%        # n() = total number of rows in judge_performance

  # Order by composite overall rank, then by name
  arrange(overall_rank, judge_user_fullname) %>%
  # Keep only the top 10 judges
  slice_head(n = 10) %>%
  # Build the output table, adding "(rank/total)" for each metric
  transmute(
    `Judge ID`        = judge_id,
    Judge             = judge_user_fullname,
    `Number of dives` = n_dives_used,

    # Severity with its rank: e.g. "0.016 (3/100)"
    Severity          = sprintf(
      "%.3f (%d/%d)",
      SEV, rank_SEV, n_judges
    ),

    # Inconsistency with its rank
    Inconsistency     = sprintf(
      "%.3f (%d/%d)",
      INC, rank_INC, n_judges
    ),

    # Agreement with panel with its rank
    `Agreement with panel` = sprintf(
      "%.3f (%d/%d)",
      CORR, rank_AGR, n_judges
    ),

    # Extremeness penalty with its rank
    `Extremeness` = sprintf(
      "%.3f (%d/%d)",
      B4_EGI, rank_EGI, n_judges
    ),

    # Range-use ratio with its rank
    `Range-use ratio` = sprintf(
      "%.3f (%d/%d)",
      RR, rank_RR, n_judges
    ),

    # Overall composite rank (integer)
    `Overall rank`    = overall_rank
  )


knitr::kable(
  top10_judges,
  caption = "Table 4: Top 10 Judges by Composite Performance Score"
)


## ------------Table 5: Bottom 10 Judges by Composite Performance Score-----------
bottom10_judges <- judge_performance %>%
  # Add the total number of judges as a column (same for every row)
  # so we can display ranks like "(95/100)"
  mutate(n_judges = n()) %>%
  
  # Start from the worst overall ranks
  arrange(desc(overall_rank), judge_user_fullname) %>%
  # Keep only the bottom 10 judges
  slice_head(n = 10) %>%
  # For display, sort them back in ascending overall rank (91–100)
  arrange(overall_rank, judge_user_fullname) %>%
  
  # Build the output table and attach "(rank/total)" to each metric
  transmute(
    `Judge ID`        = judge_id,
    Judge             = judge_user_fullname,
    `Number of dives` = n_dives_used,

    # Severity with its rank, e.g. "-0.120 (95/100)"
    Severity          = sprintf(
      "%.3f (%d/%d)",
      SEV, rank_SEV, n_judges
    ),

    # Inconsistency with its rank
    Inconsistency     = sprintf(
      "%.3f (%d/%d)",
      INC, rank_INC, n_judges
    ),

    # Agreement with panel with its rank
    `Agreement with panel` = sprintf(
      "%.3f (%d/%d)",
      CORR, rank_AGR, n_judges
    ),

    # Extremeness penalty with its rank
    `Extremeness` = sprintf(
      "%.3f (%d/%d)",
      B4_EGI, rank_EGI, n_judges
    ),

    # Range-use ratio with its rank
    `Range-use ratio` = sprintf(
      "%.3f (%d/%d)",
      RR, rank_RR, n_judges
    ),

    # Overall composite rank (integer)
    `Overall rank`    = overall_rank
  )

knitr::kable(
  bottom10_judges,
  caption = "Table 5: Bottom 10 Judges by Composite Performance Score"
)




## ----Figure 6: Relationship Between Overall Rank and Judge-level Metrics--------
# Prepare data for scatterplots: one row per judge–metric
scatter_df <- judge_performance %>%
  # Keep overall rank and all judge-level metrics
  select(
    overall_rank,
    SEV,
    INC,
    CORR,
    EGI,
    RR
  ) %>%
  # Convert from wide (one row per judge) to long (one row per judge–metric)
  pivot_longer(
    cols      = c(SEV, INC, CORR, EGI, RR),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  # Set facet order and display full metric names on the plot
  # Order (left to right): Severity, Inconsistency, Agreement with panel,
  # Extremeness, Range-use ratio
  mutate(
    metric = factor(
      metric,
      levels = c("SEV", "INC", "CORR", "EGI", "RR"),  # facet order
      labels = c(
        "Severity (SEV)",
        "Inconsistency (INC)",
        "Agreement with Panel (CORR)",
        "Extremeness Rate (EGI)",
        "Range-use Ratio (RR)"
      )
    )
  )

# Scatterplots: overall rank vs each metric
ggplot(scatter_df, aes(x = overall_rank, y = value)) +
  # Vertical dashed lines for Top 10 and Bottom 10 cutoffs
  geom_vline(
    xintercept = c(10, 90),
    linetype   = "dashed",
    color      = "grey60"
  ) +
  geom_point(alpha = 0.6) +                   # Judges as points
  geom_smooth(se = FALSE, method = "loess") + # LOESS trend per metric
  facet_wrap(~ metric, scales = "free_y") +   # One panel per metric (with full names)
  labs(
    x = "Overall Rank (1 = best)",
    y = "Metric Value",
    title = "Figure 6: Relationship Between Overall Rank and Judge-level Metrics"
  ) +
  theme_minimal()



## --------Figure 7: Comparison of Key Metrics for Top vs Bottom Judges-----------
# Define top and bottom 10 judges based on overall rank
rank_summary <- judge_performance %>%
  summarise(max_rank = max(overall_rank, na.rm = TRUE))

max_rank <- rank_summary$max_rank

judge_groups <- judge_performance %>%
  mutate(
    group = case_when(
      overall_rank <= 10 ~ "Top 10",              # Best 10 judges
      overall_rank >= max_rank - 9 ~ "Bottom 10", # Worst 10 judges
      TRUE ~ "Middle"                             # All remaining judges
    )
  )

# Keep only top and bottom 10 for the boxplot
box_df <- judge_groups %>%
  filter(group %in% c("Top 10", "Bottom 10")) %>%
  select(
    group,
    SEV,
    INC,
    CORR,
    EGI,
    RR
  ) %>%
  # Convert from wide (one row per judge) to long (one row per judge–metric)
  pivot_longer(
    cols      = c(SEV, INC, CORR, EGI, RR),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  # Set facet order and display full metric names on the plot
  # Order (left to right): Severity, Inconsistency, Agreement with panel,
  # Extremeness, Range-use ratio
  mutate(
    metric = factor(
      metric,
      levels = c("SEV", "INC", "CORR", "EGI", "RR"),  # facet order
      labels = c(
        "Severity (SEV)",
        "Inconsistency (INC)",
        "Agreement with Panel (CORR)",
        "Extremeness Rate (EGI)",
        "Range-use Ratio (RR)"
      )
    )
  )

# Boxplots comparing Top 10 vs Bottom 10 judges
ggplot(box_df, aes(x = group, y = value)) +
  geom_boxplot() +
  facet_wrap(~ metric, scales = "free_y") +   # One panel per metric (with full names)
  labs(
    x = NULL,
    y = "Metric Value",
    title = "Figure 7: Comparison of Key Metrics for Top vs Bottom Judges"
  ) +
  theme_minimal()



## -----Figure 8: Correlation Between Overall Rank and Judge-level Metrics--------
# Select overall rank and judge-level metrics
cor_df <- judge_performance %>%
  select(
    overall_rank,
    SEV,
    INC,
    CORR,
    EGI,
    RR
  )

# Compute Spearman correlation of each metric with overall_rank
cor_with_rank <- cor_df %>%
  summarise(
    across(
      c(SEV, INC, CORR, EGI, RR),
      ~ cor(.x, overall_rank,
            method = "spearman",
            use    = "pairwise.complete.obs")
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to  = "metric",
    values_to = "corr"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("SEV", "INC", "CORR", "EGI", "RR"),  
      labels = c(
        "Severity",
        "Inconsistency",
        "Agreement with Panel",
        "Extremeness Rate",
        "Range-use Ratio"
      )
    )
  )


# Bar plot for correlations with overall_rank
ggplot(cor_with_rank, aes(x = metric, y = corr)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = NULL,
    y = "Spearman Correlation with Overall Rank",
    title = "Figure 8: Correlation Between Overall Rank and Judge-level Metrics"
  ) +
  theme_minimal()



## ----------Table 6: Summary Statistics for Judge Inconsistency------------------
# all judge inconsistency spread and REPORT min Q1 MEDIAN Q2 and MAX
judge_ids_all <- judge_performance %>%
  select(judge_id, judge_user_fullname, INC)

# Summary statistics
inc_summary <- judge_ids_all %>%
  summarise(
    Min    = min(INC, na.rm = TRUE),
    Q1     = quantile(INC, 0.25, na.rm = TRUE),
    Median = median(INC, na.rm = TRUE),
    Q3     = quantile(INC, 0.75, na.rm = TRUE),
    Max    = max(INC, na.rm = TRUE)
  )
knitr::kable(
  inc_summary,
  caption = "Table 6: Summary Statistics for Judge Inconsistency (INC)",
  digits = 3
)


## -------Figure 9: Distribution of Judge-level Gender Bias-----------------------
gender_bias <- dive_level %>%
  # Keep dives with defined residual, judge ID, and diver gender
  filter(
    !is.na(residual),
    !is.na(judge_id),
    !is.na(diver_gender)
  ) %>%
  # For each judge and gender, summarize sample size and mean residual
  group_by(judge_id, judge_user_fullname, diver_gender) %>%
  summarise(
    n_dives_gender = n(),
    mean_resid     = mean(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Reshape to wide format: separate columns for girls and boys
  pivot_wider(
    names_from  = diver_gender,
    values_from = c(n_dives_gender, mean_resid),
    names_glue  = "{diver_gender}_{.value}"
  ) %>%
  mutate(
    # Require a minimum number of dives for each gender
    enough_girls = !is.na(female_n_dives_gender) & female_n_dives_gender >= 5,
    enough_boys  = !is.na(male_n_dives_gender)   & male_n_dives_gender   >= 5,
    has_both     = enough_girls & enough_boys,
    # Gender bias index: girls minus boys mean residual
    gender_diff  = ifelse(
      has_both,
      female_mean_resid - male_mean_resid,
      NA_real_
    ),
    abs_gender_diff = abs(gender_diff)
  )

# Attach gender-bias metrics back to the judge-level performance table
judge_performance <- judge_performance %>%
  left_join(gender_bias, by = c("judge_id", "judge_user_fullname"))

# plot histogram of judge gender bias
gender_bias_plot <- gender_bias %>%
  filter(has_both, !is.na(gender_diff))

ggplot(gender_bias_plot, aes(x = gender_diff)) +
  geom_histogram(bins = 30, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 9: Distribution of Judge-level Gender Bias",
    x = "Gender Bias Index (girls minus boys mean residual)",
    y = "Number of Judges"
  )



## ---Table 7: Top 10 Judges Showing the Strongest Gender-related Scoring Bias----
# Top 10 judges by gender-related bias (absolute difference)
gender_top10 <- gender_bias %>%
  filter(has_both, !is.na(gender_diff)) %>%
  mutate(abs_bias = abs(gender_diff)) %>%
  arrange(desc(abs_bias)) %>%
  slice_head(n = 10) %>%
  select(
    judge_id,
    Judge = judge_user_fullname,
    `Girls dives` = female_n_dives_gender,
    `Boys dives`  = male_n_dives_gender,
    `Bias (Girls - Boys)` = gender_diff
  ) %>%
  mutate(`Bias (Girls - Boys)` = round(`Bias (Girls - Boys)`, 3))

knitr::kable(
  gender_top10,
  caption = "Table 7: Top 10 Judges Showing the Strongest Gender-related Scoring Bias",
  digits = 3
)


## -----Figure 10: Distribution of Judge-level Difficulty Bias--------------------
# Difficulty-Related Bias (Easy vs Hard Dives)
# 1) Define difficulty groups using the global median
dd_median <- dive_level %>%
  filter(!is.na(dd)) %>%
  summarise(med_dd = median(dd, na.rm = TRUE)) %>%
  pull(med_dd)

difficulty_bias <- dive_level %>%
  filter(
    !is.na(residual),
    !is.na(judge_id),
    !is.na(dd)
  ) %>%
  mutate(
    dd_group = if_else(dd >= dd_median, "High DD", "Low DD")
  ) %>%
  group_by(judge_id, judge_user_fullname, dd_group) %>%
  summarise(
    n_dives_group = n(),
    mean_resid    = mean(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = dd_group,
    values_from = c(n_dives_group, mean_resid),
    names_glue  = "{dd_group}_{.value}"
  ) %>%
  mutate(
    enough_low  = !is.na(`Low DD_n_dives_group`)  & `Low DD_n_dives_group`  >= 5,
    enough_high = !is.na(`High DD_n_dives_group`) & `High DD_n_dives_group` >= 5,
    has_both    = enough_low & enough_high,
    diff_DD     = ifelse(
      has_both,
      `High DD_mean_resid` - `Low DD_mean_resid`,
      NA_real_
    ),
    abs_diff_DD = abs(diff_DD)
  )


# Histogram of judge-level difficulty bias
difficulty_bias_plot <- difficulty_bias %>%
  filter(has_both, !is.na(diff_DD))

ggplot(difficulty_bias_plot, aes(x = diff_DD)) +
  geom_histogram(bins = 30, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 10: Distribution of Judge-level Difficulty Bias",
    x = "High DD - Low DD Mean Residual",
    y = "Number of Judges"
  )


## --Table 8: Top 10 Judges Showing the Largest Difficulty-related Scoring Bias---
# Top 10 judges by difficulty-related bias (absolute difference)
difficulty_top10 <- difficulty_bias %>%
  filter(has_both, !is.na(diff_DD)) %>%
  mutate(abs_bias = abs(diff_DD)) %>%
  arrange(desc(abs_bias)) %>%
  slice_head(n = 10) %>%  # Top 10 most biased
  select(
    judge_id,
    Judge = judge_user_fullname,
    `Low DD dives`  = `Low DD_n_dives_group`,
    `High DD dives` = `High DD_n_dives_group`,
    `Bias (High - Low DD)` = diff_DD
  ) %>%
  mutate(`Bias (High - Low DD)` = round(`Bias (High - Low DD)`, 3))

knitr::kable(
  difficulty_top10,
  caption = "Table 8: Top 10 Judges Showing the Largest Difficulty-related Scoring Bias",
  digits = 3
)


## ---Figure 11: Judge-level Spread in Mean Residuals Across Meet Types-----------
# Meet-Type Effects on Judge Scoring

meet_bias <- dive_level %>%
  filter(
    !is.na(residual),
    !is.na(judge_id),
    !is.na(meet_type)
  ) %>%
  group_by(judge_id, judge_user_fullname, meet_type) %>%
  summarise(
    n_dives_meet_type = n(),
    mean_resid        = mean(residual, na.rm = TRUE),
    .groups = "drop"
  )

# Judge-level spread across meet types (only types with >= 5 dives)
meet_bias_judge <- meet_bias %>%
  group_by(judge_id, judge_user_fullname) %>%
  summarise(
    n_types_used = sum(n_dives_meet_type >= 5),
    min_mean     = ifelse(
      n_types_used >= 2,
      min(mean_resid[n_dives_meet_type >= 5], na.rm = TRUE),
      NA_real_
    ),
    max_mean     = ifelse(
      n_types_used >= 2,
      max(mean_resid[n_dives_meet_type >= 5], na.rm = TRUE),
      NA_real_
    ),
    diff_meet    = max_mean - min_mean,
    abs_diff_meet = abs(diff_meet),
    .groups = "drop"
  )

# Distribution of meet-type effects
ggplot(
  meet_bias_judge %>% filter(!is.na(diff_meet)),
  aes(x = diff_meet)
) +
  geom_histogram(bins = 30, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 11: Judge-level Spread in Mean Residuals Across Meet Types",
    x = "Max(mean residual) - Min(mean residual) over Meet Types",
    y = "Number of Judges"
  )


## ---Table 9: Top 10 Judges Showing the Largest Meet-type Scoring Shifts---------
# Top 10 judges by meet-type bias (absolute spread)
meet_top10 <- meet_bias_judge %>%
  filter(!is.na(diff_meet)) %>%
  mutate(abs_bias = abs_diff_meet) %>%
  arrange(desc(abs_bias)) %>%
  slice_head(n = 10) %>%
  select(
    judge_id,
    Judge = judge_user_fullname,
    `Meet types used` = n_types_used,
    `Bias spread (max-min)` = diff_meet
  ) %>%
  mutate(`Bias spread (max-min)` = round(`Bias spread (max-min)`, 3))

knitr::kable(
  meet_top10,
  caption = " Table 9: Top 10 Judges Showing the Largest Meet-type Scoring Shifts",
  digits = 3
)


## ------Figure 12: Distribution of Judge-level Team Bias-------------------------
team_bias <- dive_level %>%
  # Keep dives with defined residual, judge ID, and team ID
  filter(
    !is.na(residual),
    !is.na(judge_id),
    !is.na(diver_team_id)
  ) %>%
  # Summarize per judge × team
  group_by(judge_id, judge_user_fullname, diver_team_id, team_name, team_abbreviation) %>%
  summarise(
    n_dives_team = n(),
    mean_resid_team = mean(residual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # For each judge, compute "other-team" residual average
  group_by(judge_id, judge_user_fullname) %>%
  mutate(
    total_dives = sum(n_dives_team),
    dives_other = total_dives - n_dives_team,
    mean_resid_other = (sum(n_dives_team * mean_resid_team) - n_dives_team * mean_resid_team) /
                       pmax(dives_other, 1)
  ) %>%
  ungroup() %>%
  mutate(
    enough_team  = n_dives_team >= 5,
    enough_other = dives_other >= 5,
    has_both     = enough_team & enough_other,
    # Team-bias index: team-specific score minus all-other-teams score
    team_diff    = ifelse(
      has_both,
      mean_resid_team - mean_resid_other,
      NA_real_
    ),
    abs_team_diff = abs(team_diff)
  )

team_bias_plot <- team_bias %>%
  filter(has_both, !is.na(team_diff))

# Histogram of judge–team bias
ggplot(team_bias_plot, aes(x = team_diff)) +
  geom_histogram(bins = 30, boundary = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 12: Distribution of Judge-level Team Bias",
    x = "Team Bias Index (team mean residual minus all-other-teams mean residual)",
    y = "Number of Judge–team Combinations"
  )



## ---Table 10: Top 10 Judge–team Combinations Showing the Strongest Team-related Scoring Bias------
# Top 10 judge–team combinations by absolute team bias
team_top10 <- team_bias %>%
  filter(has_both, !is.na(team_diff)) %>%
  mutate(abs_bias = abs(team_diff)) %>%
  arrange(desc(abs_bias)) %>%
  slice_head(n = 10) %>%   # Top 10 strongest team bias relationships
  select(
    judge_id,
    Judge = judge_user_fullname,
    Team = team_name,
    `Team dives` = n_dives_team,
    `Other dives` = dives_other,
    `Bias (Team - Others)` = team_diff
  ) %>%
  mutate(`Bias (Team - Others)` = round(`Bias (Team - Others)`, 3))

knitr::kable(
  team_top10,
  caption = "Table 10: Top 10 Judge–team Combinations Showing the Strongest Team-related Scoring Bias",
  digits = 3
)




## ----Shiny_APP-------------------------------------------------------------
# Shiny app for judge performance dashboard

# 0. Penalty and range data (using existing judge_performance)

# Names of the five penalty variables for the radar plot
penalty_vars <- c("B1_SEV", "B2_INC", "B3_AGR", "B4_EGI", "B5_RR")

# Per-judge score range based on raw_award (from dive_level data)
judge_range <- dive_level %>%
  filter(!is.na(judge_id), !is.na(raw_award)) %>%
  group_by(judge_id) %>%
  summarise(
    score_min   = min(raw_award, na.rm = TRUE),
    score_max   = max(raw_award, na.rm = TRUE),
    score_range = score_max - score_min,
    .groups     = "drop"
  )

# Attach score_range to the judge_performance table
judge_penalty <- judge_performance %>%
  left_join(judge_range, by = "judge_id")

# Global max and min for each penalty (needed by fmsb::radarchart)
penalty_max <- judge_penalty %>%
  summarise(across(all_of(penalty_vars), ~ max(.x, na.rm = TRUE)))

penalty_min <- judge_penalty %>%
  summarise(across(all_of(penalty_vars), ~ min(.x, na.rm = TRUE)))

# Dropdown choices for the UI: "Name (ID xxxx)"
judge_choices <- judge_penalty %>%
  mutate(label = paste0(judge_user_fullname, " (ID ", judge_id, ")")) %>%
  arrange(judge_user_fullname) %>%
  { setNames(.$judge_id, .$label) }

# 1. UI

ui <- fluidPage(
  titlePanel("Judge performance dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId  = "judge_id",
        label    = "Select a judge:",
        choices  = judge_choices,
        selected = judge_choices[1]
      ),
      br(),
      selectInput(
        inputId  = "bias_type",
        label    = "Select bias type:",
        choices  = c(
          "Gender bias",
          "Difficulty bias",
          "Meet-type bias",
          "Team bias"
        ),
        selected = "Gender bias"
      )
    ),

    mainPanel(
      h4("Summary for selected judge"),
      tableOutput("judge_table"),
      br(),
      h4("Radar plot of penalty scores (higher = worse)"),
      plotOutput("radar_plot", height = "350px"),
      tags$p(
        tags$em(
          "Each axis shows a penalty score (higher is worse). ",
          "For example, the agreement penalty is defined as 1 − CORR, ",
          "so judges with low agreement receive higher penalties ",
          "and longer spokes."
        )
      ),
      hr(),
      h4("Bias diagnostics for selected judge"),
      textOutput("bias_text"),
      tableOutput("bias_table"),
      plotOutput("bias_plot", height = "300px")
    )
  )
)

# 2. Server

server <- function(input, output, session) {

  # Row in judge_penalty for the currently selected judge
  sel_judge <- reactive({
    req(input$judge_id)
    judge_penalty %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  # Per-judge bias data (four types), filtered to the selected judge

  gender_judge <- reactive({
    req(input$judge_id)
    gender_bias %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  difficulty_judge <- reactive({
    req(input$judge_id)
    difficulty_bias %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  meet_judge_detail <- reactive({
    req(input$judge_id)
    meet_bias %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  meet_judge_summary <- reactive({
    req(input$judge_id)
    meet_bias_judge %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  team_judge <- reactive({
    req(input$judge_id)
    team_bias %>%
      filter(judge_id == as.numeric(input$judge_id))
  })

  # Summary table for the selected judge (values + ranks)

  output$judge_table <- renderTable({
    df <- sel_judge()
    req(nrow(df) == 1)

    # Total number of judges used in the ranking
    n_judges <- nrow(judge_penalty)

    # First row: actual metric values
    value_row <- df %>%
      transmute(
        Row            = "Value",
        `Judge ID`     = as.integer(judge_id),
        Judge          = judge_user_fullname,
        Dives          = n_dives_used,
        Severity       = round(SEV, 3),
        Inconsistency  = round(INC, 3),
        Agreement      = round(CORR, 3),
        Extremeness    = round(EGI, 3),
        `Range use`    = round(RR, 3),
        `Overall rank` = overall_rank
      ) %>%
      mutate(across(everything(), as.character))

    # Second row: ranks for each metric (1 = best)
    rank_row <- df %>%
      transmute(
        Row            = "Rank (1 = best; of all judges)",
        `Judge ID`     = "",
        Judge          = "",
        Dives          = "",
        Severity       = paste0(rank_SEV,  " / ", n_judges),
        Inconsistency  = paste0(rank_INC,  " / ", n_judges),
        Agreement      = paste0(rank_AGR,  " / ", n_judges),
        Extremeness    = paste0(rank_EGI,  " / ", n_judges),
        `Range use`    = paste0(rank_RR,   " / ", n_judges),
        `Overall rank` = paste0(overall_rank, " / ", n_judges)
      ) %>%
      mutate(across(everything(), as.character))

    bind_rows(value_row, rank_row)
  })

  # Radar plot of penalty scores for the selected judge

  output$radar_plot <- renderPlot({
    df <- sel_judge()
    req(nrow(df) == 1)

    judge_pen <- df %>%
      select(all_of(penalty_vars))

    # fmsb expects: first row = max, second row = min, third row = data
    radar_data <- rbind(penalty_max, penalty_min, judge_pen)
    rownames(radar_data) <- c("max", "min", "judge")

    colnames(radar_data) <- c(
      "Severity\npenalty",
      "Inconsistency\npenalty",
      "Agreement\npenalty",
      "Extremeness\npenalty",
      "Range-use\npenalty"
    )

    old_par <- par(mar = c(2, 2, 6, 2))
    on.exit(par(old_par))

    radarchart(
      radar_data,
      axistype   = 1,
      pcol       = "black",
      pfcol      = rgb(0.2, 0.4, 0.8, 0.3),
      plwd       = 2,
      cglcol     = "grey80",
      cglty      = 1,
      axislabcol = "grey30",
      vlcex      = 0.8
    )

    title(
      main = paste0(
        "Penalty profile for ",
        df$judge_user_fullname, " (ID ",
        as.integer(df$judge_id), ")"
      )
    )
  })

  # Short text explaining which bias view is being shown

  output$bias_text <- renderText({
    type  <- input$bias_type
    jname <- sel_judge()$judge_user_fullname

    paste0(
      "Bias view for ", jname, ": ",
      dplyr::case_when(
        type == "Gender bias" ~
          "positive values mean this judge scores girls higher than boys (relative to the panel baseline).",
        type == "Difficulty bias" ~
          "positive values mean this judge scores high-DD dives higher than low-DD dives.",
        type == "Meet-type bias" ~
          "positive mean residuals indicate more generous scoring in that meet type.",
        type == "Team bias" ~
          "positive values mean this judge scores that team higher than all other teams.",
        TRUE ~ ""
      )
    )
  })

  # Bias summary table for the selected judge

  output$bias_table <- renderTable({
    type <- input$bias_type

    if (type == "Gender bias") {

      df <- gender_judge()
      validate(need(nrow(df) == 1 && df$has_both,
                    "Not enough dives for both girls and boys for this judge."))

      tibble(
        Group           = c("Girls", "Boys", "Girls - Boys"),
        `Mean residual` = c(df$female_mean_resid,
                            df$male_mean_resid,
                            df$gender_diff),
        Interpretation  = c(
          "Girls: >0 = more generous to girls overall",
          "Boys: >0 = more generous to boys overall",
          "Girls - Boys: >0 = favors girls; <0 = favors boys"
        )
      )

    } else if (type == "Difficulty bias") {

      df <- difficulty_judge()
      validate(need(nrow(df) == 1 && df$has_both,
                    "Not enough high- and low-DD dives for this judge."))

      tibble(
        Group           = c("Low DD", "High DD", "High - Low"),
        `Mean residual` = c(df$`Low DD_mean_resid`,
                            df$`High DD_mean_resid`,
                            df$diff_DD),
        Interpretation  = c(
          "Low DD: >0 = more generous on easy dives",
          "High DD: >0 = more generous on hard dives",
          "High - Low: >0 = favors high DD; <0 = favors low DD"
        )
      )

    } else if (type == "Meet-type bias") {

      df_det  <- meet_judge_detail()
      summary <- meet_judge_summary()

      validate(need(nrow(summary) == 1 && summary$n_types_used >= 2,
                    "Not enough meet types (with >=5 dives) for this judge."))

      df_det %>%
        filter(n_dives_meet_type >= 5) %>%
        transmute(
          `Meet type`     = meet_type,
          `Dives`         = n_dives_meet_type,
          `Mean residual` = round(mean_resid, 3)
        )

    } else if (type == "Team bias") {

      df <- team_judge() %>%
        filter(has_both, !is.na(team_diff))

      validate(need(nrow(df) > 0,
                    "Not enough dives for any single team vs others for this judge."))

      df %>%
        transmute(
          Team                = team_name,
          `Dives team`        = n_dives_team,
          `Dives others`      = dives_other,
          `Mean resid team`   = round(mean_resid_team, 3),
          `Mean resid others` = round(mean_resid_other, 3),
          `Team - others`     = round(team_diff, 3)
        )
    }
  })

  # Bias plots for the selected judge

  output$bias_plot <- renderPlot({
    type <- input$bias_type

    if (type == "Gender bias") {

      df <- gender_judge()
      validate(need(nrow(df) == 1 && df$has_both, NULL))

      plot_df <- tibble(
        group      = c("Girls", "Boys"),
        mean_resid = c(df$female_mean_resid, df$male_mean_resid)
      )

      ggplot(plot_df, aes(x = group, y = mean_resid)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          x     = NULL,
          y     = "Mean residual",
          title = "Girls vs Boys mean residual"
        )

    } else if (type == "Difficulty bias") {

      df <- difficulty_judge()
      validate(need(nrow(df) == 1 && df$has_both, NULL))

      plot_df <- tibble(
        group      = c("Low DD", "High DD"),
        mean_resid = c(df$`Low DD_mean_resid`, df$`High DD_mean_resid`)
      )

      ggplot(plot_df, aes(x = group, y = mean_resid)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          x     = NULL,
          y     = "Mean residual",
          title = "High-DD vs Low-DD mean residual"
        )

    } else if (type == "Meet-type bias") {

      summary <- meet_judge_summary()
      if (!(nrow(summary) == 1 && summary$n_types_used >= 2)) return(NULL)

      df <- meet_judge_detail() %>%
        filter(n_dives_meet_type >= 5)
      validate(need(nrow(df) > 0, NULL))

      ggplot(df, aes(x = meet_type, y = mean_resid)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          x     = "Meet type",
          y     = "Mean residual",
          title = "Mean residual by meet type"
        )

    } else if (type == "Team bias") {

      df <- team_judge() %>%
        filter(has_both, !is.na(team_diff))
      validate(need(nrow(df) > 0, NULL))

      ggplot(df, aes(x = reorder(team_abbreviation, team_diff),
                     y = team_diff)) +
        geom_col() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          x     = "Team",
          y     = "Team bias index (team - others)",
          title = "Team-level bias for selected judge"
        ) +
        coord_flip()
    }
  })
}

# 3. Run app

shinyApp(ui, server)



## ------------All Judges: Full Performance Summary Table-------------------------
all_judges <- judge_performance %>%
  # Add total number of judges so we can display "(rank/total)"
  mutate(n_judges = n()) %>%
  
  # Order by composite overall rank, then alphabetically by judge name
  arrange(overall_rank, judge_user_fullname) %>%
  
  # Build the output table using the same metrics as Top 10 / Bottom 10
  transmute(
    `Judge ID`        = judge_id,
    Judge             = judge_user_fullname,
    `Number of dives` = n_dives_used,

    # Severity with its rank (lower = better)
    Severity = sprintf(
      "%.3f (%d/%d)",
      SEV, rank_SEV, n_judges
    ),

    # Inconsistency with its rank
    Inconsistency = sprintf(
      "%.3f (%d/%d)",
      INC, rank_INC, n_judges
    ),

    # Agreement with panel with its rank
    `Agreement with panel` = sprintf(
      "%.3f (%d/%d)",
      CORR, rank_AGR, n_judges
    ),

    # Extremeness penalty (B4_EGI), same method used in Top 10 / Bottom 10 tables
    # Rank_EGI corresponds to the extremeness penalty ranking
    `Extremeness` = sprintf(
      "%.3f (%d/%d)",
      B4_EGI, rank_EGI, n_judges
    ),

    # Range-use ratio with its rank
    `Range-use ratio` = sprintf(
      "%.3f (%d/%d)",
      RR, rank_RR, n_judges
    ),

    # Final composite overall rank
    `Overall rank` = overall_rank
  )

knitr::kable(all_judges)



