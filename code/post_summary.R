

beta_cols <- grep("^beta\\[", colnames(draws), value = TRUE)
beta_tbl <- summ(draws[, beta_cols]) %>%
  mutate(
    parameter = colnames(X),                  # readable names from the design matrix
    odds_ratio = exp(mean),                   # exp of the posterior mean, on the odds scale
    rhat = rhat[beta_cols],
    ess  = ess[beta_cols],
    .before = 1
  )

sd_cols <- c("sd.a", "sd.b", "sd.gamma")
sd_tbl <- summ(draws[, sd_cols]) %>%
  mutate(parameter = c("SD of case effects", "SD of justice effects", "SD of yearly drift in gamma"),
         odds_ratio = NA_real_,
         rhat = rhat[sd_cols], 
         ess = ess[sd_cols], .before = 1)

bind_rows(beta_tbl, sd_tbl) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kable(caption = "Posterior summaries: case-level effects and variance parameters")


# ----

g <- draws[, grep("^gamma\\[", colnames(draws))]   # draws x terms

# Era boundaries by term year (edit to taste)
term_tbl <- term_tbl %>%
  mutate(era2 = case_when(term_year < 1969 ~ "1946-1968 (Vinson/Warren)",
                          term_year < 1986 ~ "1969-1985 (Burger)",
                          term_year < 2005 ~ "1986-2004 (Rehnquist)",
                          TRUE             ~ "2005+ (Roberts)"))

era_draws <- sapply(unique(term_tbl$era2), function(e) {
  rowMeans(g[, term_tbl$term[term_tbl$era2 == e], drop = FALSE])
})

era_tbl <- summ(era_draws) %>%
  mutate(era = colnames(era_draws), .before = 1)

# Change between eras, e.g. latest era minus earliest
diff_draws <- cbind(
  "Roberts minus Burger" = era_draws[, "2005+ (Roberts)"] - era_draws[, "1969-1985 (Burger)"]
)
diff_tbl <- summ(diff_draws) %>% mutate(era = colnames(diff_draws), .before = 1)

bind_rows(era_tbl, diff_tbl) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kable(caption = "Effect of appointing party (gamma) by era, with era contrast")

# ----

term_summary <- summ(g) %>%
  mutate(term = 1:ncol(g)) %>%
  left_join(term_tbl, by = "term") %>%
  select(term_year, mean, sd, lo95, hi95, p_pos)

term_summary %>% arrange(desc(mean)) %>% slice(c(1:5, (n() - 4):n())) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kable(caption = "Five highest and five lowest terms for gamma")

















