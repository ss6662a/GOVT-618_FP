

# gamma[t] trajectory: posterior mean and 95% interval by term

draws <- as.matrix(post)
g <- draws[, grep("^gamma\\[", colnames(draws))]

gamma_tbl <- tibble(
  term      = 1:ncol(g),
  mean      = colMeans(g),
  lo        = apply(g, 2, quantile, 0.025),
  hi        = apply(g, 2, quantile, 0.975)
) %>% left_join(term_tbl, by = "term")

ggplot(gamma_tbl, aes(term_year, mean)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.25) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Term", y = expression(gamma[t]),
       title = "Estimated effect of appointing party on conservative votes")


# ----

draws <- as.matrix(post)   # rows = posterior draws, columns = parameters

# Helper: summarize one or more columns of draws
summ <- function(mat) {
  tibble(
    mean   = colMeans(mat),
    sd     = apply(mat, 2, sd),               # posterior SD (the "error")
    lo95   = apply(mat, 2, quantile, 0.025),
    hi95   = apply(mat, 2, quantile, 0.975),
    p_pos  = colMeans(mat > 0)                # P(parameter > 0)
  )
}

# Convergence diagnostics for every monitored parameter
rhat <- gelman.diag(post, multivariate = FALSE)$psrf[, "Point est."]
ess  <- effectiveSize(post)
