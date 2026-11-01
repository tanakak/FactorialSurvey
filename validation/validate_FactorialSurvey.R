## =============================================================================
## Validation of the FactorialSurvey package
##
## Checks the package's results against reference values computed by hand,
## independently of the package code, and checks the statistical behaviour of
## the estimators by Monte Carlo simulation (coverage of confidence intervals,
## size of tests). It complements the unit tests in tests/testthat.
##
## Run from the package root:   Rscript validation/validate_FactorialSurvey.R
## Set FS_VALIDATION_QUICK=true for a faster run with fewer replications.
## =============================================================================

suppressPackageStartupMessages({
  library(FactorialSurvey)
  library(betareg)
})

quick <- identical(Sys.getenv("FS_VALIDATION_QUICK"), "true")
R_MC  <- if (quick) 100L else 400L   # Monte Carlo replications
t_start <- Sys.time()

cat("FactorialSurvey validation\n")
cat("  package version :", as.character(packageVersion("FactorialSurvey")), "\n")
cat("  R version       :", R.version.string, "\n")
cat("  replications    :", R_MC, "\n\n")

## ---- A tiny check framework -------------------------------------------------
results <- data.frame(id = character(), area = character(), check = character(),
                      result = character(), detail = character(),
                      stringsAsFactors = FALSE)
check <- function(id, area, desc, code) {
  out <- tryCatch(code, error = function(e) list(ok = FALSE,
                    detail = paste("ERROR:", conditionMessage(e))))
  if (is.logical(out)) out <- list(ok = out, detail = "")
  res <- if (isTRUE(out$ok)) "PASS" else "FAIL"
  results[nrow(results) + 1L, ] <<- list(id, area, desc, res, out$detail)
  cat(sprintf("[%s] %-5s %s%s\n", res, id, desc,
              if (nzchar(out$detail)) paste0("  -- ", out$detail) else ""))
  invisible(out)
}
close_to <- function(a, b, tol = 1e-8) {
  isTRUE(all.equal(unname(a), unname(b), tolerance = tol))
}
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

## Independent reference: D-efficiency with an orthonormal coding built from
## Helmert contrasts (a different basis from the package's polynomial one;
## a correct D-efficiency must not depend on the basis).
ref_deff <- function(df, factors) {
  df <- df[names(factors)]
  for (v in names(factors)) df[[v]] <- factor(df[[v]], levels = factors[[v]])
  ctr <- lapply(factors, function(l) {
    k <- length(l); H <- contr.helmert(k)
    H <- sweep(H, 2, sqrt(colSums(H^2)), "/")   # orthonormal columns
    H * sqrt(k)
  })
  X <- model.matrix(~ ., df, contrasts.arg = ctr)
  M <- crossprod(X) / nrow(X)
  if (qr(X)$rank < ncol(X)) return(0)
  100 * det(M)^(1 / ncol(X))
}

## Independent reference: cluster-robust covariance of a logit, by hand.
ref_vcov_cl_logit <- function(X, y, beta, cl) {
  p <- plogis(drop(X %*% beta))
  B <- solve(crossprod(X * sqrt(p * (1 - p))))
  S <- X * (y - p)
  Sg <- rowsum(S, cl)
  G <- nrow(Sg)
  G / (G - 1) * B %*% crossprod(Sg) %*% B
}

## =============================================================================
## A. Experimental design
## =============================================================================
cat("A. Experimental design\n")

f34 <- list(a = 1:3, b = 1:3, c = 1:3, d = 1:3)
check("A1", "design", "3^4 in 9 vignettes reaches 100% (an L9 orthogonal array)", {
  d <- fs_defficient(f34, n_vignettes = 9, seed = 1, verbose = FALSE)
  e <- attr(d, "d_efficiency")
  list(ok = close_to(e, 100, 1e-6) && close_to(ref_deff(d, f34), 100, 1e-6),
       detail = paste0("reported ", fmt(e, 1), "%"))
})

f27 <- setNames(rep(list(c("no", "yes")), 7), letters[1:7])
check("A2", "design", "2^7 in 8 vignettes reaches 100% (an L8 orthogonal array)", {
  d <- fs_defficient(f27, n_vignettes = 8, seed = 1, verbose = FALSE)
  list(ok = close_to(attr(d, "d_efficiency"), 100, 1e-6),
       detail = paste0("reported ", fmt(attr(d, "d_efficiency"), 1), "%"))
})

fb <- list(main = 0:2, side = 0:1, info = 0:1, price = c(700, 900, 1100, 1300))
check("A3", "design", "Numerically coded levels: every level appears (bug fixed)", {
  d <- fs_defficient(fb, n_vignettes = 16, seed = 1, verbose = FALSE)
  counts <- c(table(d$main), table(d$price))
  ok <- setequal(unique(d$main), 0:2) &&
    setequal(unique(d$price), fb$price) && is.numeric(d$price)
  list(ok = ok, detail = paste("main:", paste(table(d$main), collapse = "/"),
                               " price:", paste(table(d$price), collapse = "/")))
})

check("A4", "design", "Reported D-efficiency equals an independent calculation", {
  set.seed(5)
  diffs <- vapply(1:20, function(i) {
    f <- list(x = c("p", "q", "r", "s"), y = c("u", "v", "w"),
              z = c("m", "n"), w = c(1, 2, 3))
    n <- sample(c(12, 15, 18, 24), 1)
    d <- fs_defficient(f, n_vignettes = n, seed = i, verbose = FALSE)
    attr(d, "d_efficiency") - ref_deff(d, f)
  }, numeric(1))
  list(ok = max(abs(diffs)) < 1e-8,
       detail = paste("max |difference| over 20 designs:", signif(max(abs(diffs)), 2)))
})

check("A5", "design", "fs_efficiency flags a design that drops a level", {
  l9 <- data.frame(a = c(1,1,1,2,2,2,3,3,3), b = c(1,2,3,1,2,3,1,2,3),
                   c = c(1,2,3,2,3,1,3,1,2), d = c(1,2,3,3,1,2,2,3,1))
  bad <- l9; bad$a[bad$a == 2] <- 3
  e <- fs_efficiency(bad, factors = f34)
  list(ok = !e$estimable && e$d_efficiency == 0 &&
         identical(e$missing_levels, list(a = "2")),
       detail = "level a = 2 detected as missing; D-efficiency 0")
})

check("A6", "design", "Restrictions are honoured", {
  f <- list(damage = c("minor", "moderate", "severe"), cost = c(100, 500, 1000))
  d <- fs_defficient(f, n_vignettes = 6, restrictions = list(
    ~ damage == "minor" & cost == 1000, ~ damage == "severe" & cost < 200),
    seed = 1, verbose = FALSE)
  bad <- (d$damage == "minor" & d$cost == 1000) | (d$damage == "severe" & d$cost < 200)
  list(ok = !any(bad) && attr(d, "n_candidates") == 7L,
       detail = paste(attr(d, "n_candidates"), "of 9 combinations allowed"))
})

check("A7", "design", "Blocks are equal-sized and cover each vignette once", {
  d <- fs_defficient(fb, n_vignettes = 16, n_blocks = 4, seed = 2, verbose = FALSE)
  key <- do.call(paste, as.data.frame(d)[names(fb)])
  list(ok = all(table(d$block) == 4) && !anyDuplicated(key),
       detail = paste("block sizes", paste(table(d$block), collapse = "/")))
})

check("A8", "design", "Williams orders: each vignette once per position, neighbours balanced", {
  oks <- vapply(c(3, 4, 5, 6, 8), function(k) {
    W <- fs_williams(k)
    pos_ok <- all(apply(W, 2, function(col) length(unique(col)) == k))
    pairs <- table(factor(paste(W[, -ncol(W)], W[, -1])))
    nb_ok <- length(unique(as.vector(pairs))) == 1 &&
      length(pairs) == k * (k - 1)
    pos_ok && nb_ok
  }, logical(1))
  list(ok = all(oks), detail = "checked k = 3, 4, 5, 6, 8")
})

check("A9", "design", "fs_assign deals blocks out evenly", {
  d <- fs_defficient(fb, n_vignettes = 16, n_blocks = 4, seed = 2, verbose = FALSE)
  s <- suppressMessages(fs_assign(d, n_respondents = 200, seed = 1))
  per_block <- tapply(s$respondent, s$block, function(x) length(unique(x)))
  per_resp <- table(s$respondent)
  list(ok = all(per_block == 50) && all(per_resp == 4),
       detail = "50 respondents per block, 4 vignettes each")
})

check("A10", "design", "Export round-trips (CSV, Qualtrics, xlsx) with Japanese text", {
  d <- fs_defficient(list(food = c("焼き魚", "チキン"),
                          price = c(500, 700)),
                     n_vignettes = 4, n_blocks = 2, seed = 1, verbose = FALSE)
  d <- fs_vignette_text(d, "{{food}}弁当、{{price}}円")
  tmp <- tempfile(fileext = ".csv")
  fs_export(d, tmp)
  back <- read.csv(tmp, fileEncoding = "UTF-8-BOM")
  ok_csv <- identical(back$vignette_text, d$vignette_text) &&
    identical(as.numeric(back$price), d$price)
  fs_export(d, tempfile(fileext = ".csv"), format = "qualtrics")
  ok_x <- TRUE
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    tx <- tempfile(fileext = ".xlsx")
    fs_export(d, tx, format = "xlsx", by_block = TRUE)
    ok_x <- length(openxlsx::getSheetNames(tx)) == 2
  }
  list(ok = ok_csv && ok_x, detail = "text and numbers identical after re-reading")
})

## =============================================================================
## B. Estimation
## =============================================================================
cat("\nB. Estimation\n")

check("B1", "estimation", "fs_betareg reproduces betareg on the transformed scale", {
  y01 <- bento$rating / 10
  n <- length(y01)
  bento2 <- transform(bento, y = (y01 * (n - 1) + 0.5) / n)
  ref <- suppressWarnings(betareg(y ~ main + side + info + price, data = bento2))
  fit <- fs_betareg(rating ~ main + side + info + price, data = bento,
                    cluster = ~ respondent, compute_me = FALSE)
  list(ok = close_to(coef(fit), coef(ref), 1e-6),
       detail = "coefficients identical")
})

check("B2", "estimation", "Clustered covariance (betareg) equals a manual sandwich", {
  fit <- fs_betareg(rating ~ main + side + info + price, data = bento,
                    cluster = ~ respondent, compute_me = FALSE)
  m <- fit$model
  S <- sandwich::estfun(m)
  Sg <- rowsum(S, bento$respondent)
  G <- nrow(Sg); n <- nrow(S)
  Bm <- sandwich::bread(m) / n
  V <- G / (G - 1) * Bm %*% crossprod(Sg) %*% Bm
  list(ok = close_to(vcov(fit), V, 1e-6),
       detail = paste("SE ratio clustered / naive for price:",
                      fmt(sqrt(vcov(fit)["price", "price"]) /
                            sqrt(vcov(m)["price", "price"]), 2)))
})

set.seed(11)
nl <- 400
dl <- data.frame(id = rep(1:100, each = 4), q = rbinom(nl, 1, 0.5),
                 type = factor(sample(c("a", "b", "c"), nl, TRUE)),
                 cost = sample(c(500, 1000, 2000), nl, TRUE))
dl$y <- rbinom(nl, 1, plogis(1 + 0.8 * dl$q + 0.3 * (dl$type == "b") -
                               0.001 * dl$cost + rnorm(100, sd = 0.5)[dl$id]))
check("B3", "estimation", "fs_logit coefficients and clustered SE equal hand calculations", {
  fit <- fs_logit(y ~ q + type + cost, data = dl, cluster = ~ id, compute_me = FALSE)
  X <- model.matrix(~ q + type + cost, dl)
  ref <- glm(y ~ q + type + cost, binomial, dl)
  V <- ref_vcov_cl_logit(X, dl$y, coef(ref), dl$id)
  list(ok = close_to(coef(fit), coef(ref)) && close_to(vcov(fit), V, 1e-6),
       detail = "covariance matches B (sum_g s_g s_g') B x G/(G-1)")
})

check("B4", "estimation", "fs_logit McFadden R2 and share correct match definitions", {
  fit <- fs_logit(y ~ q + type + cost, data = dl, cluster = ~ id, compute_me = FALSE)
  ref <- glm(y ~ q + type + cost, binomial, dl)
  r2 <- 1 - as.numeric(logLik(ref)) / as.numeric(logLik(glm(y ~ 1, binomial, dl)))
  pc <- 100 * mean((fitted(ref) >= 0.5) == (dl$y == 1))
  list(ok = close_to(fit$gof$pseudo_r2_mcfadden, r2) &&
         close_to(fit$gof$percent_correct, pc),
       detail = paste0("R2 = ", fmt(r2), ", correct = ", fmt(pc, 1), "%"))
})

## =============================================================================
## C. Willingness to pay
## =============================================================================
cat("\nC. Willingness to pay\n")

fit_b <- fs_betareg(rating ~ main + side + info + price, data = bento,
                    cluster = ~ respondent, compute_me = FALSE)

check("C1", "WTP", "MWTP and delta-method SE equal hand calculations (bento)", {
  w <- as.data.frame(fs_mwtp(fit_b, price = "price"))
  b <- coef(fit_b); V <- vcov(fit_b)
  ok <- TRUE
  for (v in w$variable) {
    est <- -b[[v]] / b[["price"]]
    g <- c(-1 / b[["price"]], b[[v]] / b[["price"]]^2)
    se <- sqrt(drop(t(g) %*% V[c(v, "price"), c(v, "price")] %*% g))
    ok <- ok && close_to(w$mwtp[w$variable == v], est) &&
      close_to(w$std_error[w$variable == v], se, 1e-6)
  }
  list(ok = ok, detail = paste0("chicken ", fmt(w$mwtp[1], 0), " yen, side ",
                                fmt(w$mwtp[w$variable == "side"], 0), " yen"))
})

check("C2", "WTP", "Fieller and Krinsky-Robb intervals bracket the estimate", {
  wf <- as.data.frame(fs_mwtp(fit_b, price = "price", method = "fieller"))
  wk <- as.data.frame(fs_mwtp(fit_b, price = "price", method = "krinsky-robb",
                              n_sim = 4000, seed = 1))
  ok <- all(wf$conf_low < wf$mwtp & wf$mwtp < wf$conf_high) &&
    all(wk$conf_low < wk$mwtp & wk$mwtp < wk$conf_high)
  list(ok = ok, detail = paste0("chicken: Fieller [", fmt(wf$conf_low[1], 0), ", ",
                                fmt(wf$conf_high[1], 0), "], KR [",
                                fmt(wk$conf_low[1], 0), ", ", fmt(wk$conf_high[1], 0), "]"))
})

check("C3", "WTP", "EWTP equals the numerical integral of the acceptance curve", {
  sc <- data.frame(main = c("grilled fish", "teriyaki chicken", "tofu hamburger"),
                   side = c(0, 1, 1), info = c(0, 1, 0))
  e <- fs_ewtp(fit_b, sc, price = "price")
  b <- coef(fit_b); gam <- -b[["price"]]
  num <- vapply(e$estimates$index, function(a)
    integrate(function(r) plogis(a - gam * r), 0, Inf)$value, numeric(1))
  list(ok = close_to(e$estimates$ewtp, num, 1e-6),
       detail = paste("EWTP (yen):", paste(fmt(e$estimates$ewtp, 0), collapse = ", ")))
})

check("C4", "WTP", "EWTP delta-method SE equals a numerical-gradient calculation", {
  sc <- data.frame(main = "teriyaki chicken", side = 1, info = 1)
  e <- fs_ewtp(fit_b, sc, price = "price")
  b <- coef(fit_b); V <- vcov(fit_b)
  keep <- c("(Intercept)", "mainteriyaki chicken", "maintofu hamburger",
            "side", "info", "price")
  x <- c(1, 1, 0, 1, 1, 0)
  f <- function(bb) log1p(exp(sum(x * bb))) / -bb[6]
  h <- 1e-6
  g <- vapply(1:6, function(j) { up <- dn <- b[keep]; up[j] <- up[j] + h
    dn[j] <- dn[j] - h; (f(up) - f(dn)) / (2 * h) }, numeric(1))
  se <- sqrt(drop(t(g) %*% V[keep, keep] %*% g))
  list(ok = close_to(e$estimates$std_error, se, 1e-5),
       detail = paste("SE =", fmt(se, 2)))
})

## Monte Carlo: coverage of 95% intervals with respondent-level correlation.
## True MWTP = 0.8 / 0.004 = 200. The respondent shift is on the logit scale,
## so ratios of coefficients are preserved closely.
mc_design <- fs_defficient(list(q = c("no", "yes"), s = c("no", "yes"),
                                cost = c(100, 200, 300, 400)),
                           n_vignettes = 8, seed = 1, verbose = FALSE)
true_b <- c("(Intercept)" = 0.5, qyes = 0.8, syes = 0.3, cost = -0.004)

mc_coverage <- function(response, sd_resp, n_resp, reps, seed) {
  set.seed(seed)
  cov_cl <- cov_naive <- est <- numeric(reps)
  for (r in seq_len(reps)) {
    sim <- fs_simulate_ratings(mc_design, n_respondents = n_resp, coef = true_b,
                               formula = ~ q + s + cost, response = response,
                               phi = 8, sd_respondent = sd_resp, name = "y")
    sim <- as.data.frame(sim)
    if (response == "rating") {
      fc <- fs_betareg(y ~ q + s + cost, sim, cluster = ~ respondent, compute_me = FALSE)
      fn <- fs_betareg(y ~ q + s + cost, sim, cluster = FALSE, compute_me = FALSE)
    } else {
      fc <- fs_logit(y ~ q + s + cost, sim, cluster = ~ respondent, compute_me = FALSE)
      fn <- fs_logit(y ~ q + s + cost, sim, cluster = FALSE, compute_me = FALSE)
    }
    wc <- as.data.frame(fs_mwtp(fc, variable = "qyes", price = "cost"))
    wn <- as.data.frame(fs_mwtp(fn, variable = "qyes", price = "cost"))
    est[r] <- wc$mwtp
    cov_cl[r] <- wc$conf_low <= 200 && 200 <= wc$conf_high
    cov_naive[r] <- wn$conf_low <= 200 && 200 <= wn$conf_high
  }
  list(cl = mean(cov_cl), naive = mean(cov_naive), median_est = median(est))
}
mc_band <- function(p, reps) 0.95 + c(-1, 1) * 2.5 * sqrt(0.95 * 0.05 / reps)

check("C5", "WTP", "MWTP 95% CI coverage, ratings, clustered SE (beta regression)", {
  mc <- mc_coverage("rating", sd_resp = 0.6, n_resp = 150, reps = R_MC, seed = 21)
  band <- mc_band(0.95, R_MC)
  .GlobalEnv$mc_rating <- mc
  list(ok = mc$cl >= band[1] && mc$cl <= band[2],
       detail = paste0("clustered ", fmt(mc$cl, 3), " vs naive ", fmt(mc$naive, 3),
                       "; median MWTP ", fmt(mc$median_est, 1), " (true 200)"))
})

check("C6", "WTP", "MWTP 95% CI coverage, yes/no, clustered SE (logit)", {
  mc <- mc_coverage("binary", sd_resp = 0.6, n_resp = 300, reps = R_MC, seed = 22)
  band <- mc_band(0.95, R_MC)
  .GlobalEnv$mc_binary <- mc
  list(ok = mc$cl >= band[1] && mc$cl <= band[2],
       detail = paste0("clustered ", fmt(mc$cl, 3), " vs naive ", fmt(mc$naive, 3),
                       "; median MWTP ", fmt(mc$median_est, 1), " (true 200)"))
})

check("C7", "WTP", "EWTP 95% CI coverage (logit, no respondent effect)", {
  set.seed(23)
  true_ewtp <- log1p(exp(0.5 + 0.8 + 0.3)) / 0.004
  hit <- numeric(R_MC)
  for (r in seq_len(R_MC)) {
    sim <- as.data.frame(fs_simulate_ratings(
      mc_design, n_respondents = 300, coef = true_b, formula = ~ q + s + cost,
      response = "binary", sd_respondent = 0, name = "y"))
    f <- fs_logit(y ~ q + s + cost, sim, cluster = ~ respondent, compute_me = FALSE)
    e <- fs_ewtp(f, data.frame(q = "yes", s = "yes"), price = "cost")$estimates
    hit[r] <- e$conf_low <= true_ewtp && true_ewtp <= e$conf_high
  }
  band <- mc_band(0.95, R_MC)
  list(ok = mean(hit) >= band[1] && mean(hit) <= band[2],
       detail = paste0("coverage ", fmt(mean(hit), 3), " (true EWTP ",
                       fmt(true_ewtp, 1), ")"))
})

## Where clustering matters: characteristics of the respondent (constant
## within a respondent), such as gender. Data are generated with a respondent
## shift on the logit scale; the pseudo-true coefficient of the
## population-averaged logit is taken from one very large sample.
check("C8", "WTP", "Respondent characteristic: clustered SE keep 95% coverage, naive SE do not", {
  gen <- function(n, k = 8) {
    d <- data.frame(id = rep(seq_len(n), each = k), q = rep(c(0, 1), n * k / 2),
                    cost = rep(c(100, 200, 300, 400), n * k / 4))
    d$female <- rep(rbinom(n, 1, 0.5), each = k)
    d$y <- rbinom(nrow(d), 1, plogis(0.5 + 0.8 * d$q + 0.3 * d$female -
                                       0.004 * d$cost + rnorm(n)[d$id]))
    d
  }
  set.seed(40)
  b_star <- coef(glm(y ~ q + female + cost, binomial, gen(200000)))
  hc <- hn <- qc <- qn <- numeric(R_MC)
  for (r in seq_len(R_MC)) {
    d <- gen(200)
    f <- fs_logit(y ~ q + female + cost, d, cluster = ~ id, compute_me = FALSE)
    b <- coef(f); Vc <- vcov(f); Vn <- vcov(f$model)
    hc[r] <- abs(b[["female"]] - b_star[["female"]]) <= 1.96 * sqrt(Vc["female", "female"])
    hn[r] <- abs(b[["female"]] - b_star[["female"]]) <= 1.96 * sqrt(Vn["female", "female"])
    qc[r] <- abs(b[["q"]] - b_star[["q"]]) <= 1.96 * sqrt(Vc["q", "q"])
    qn[r] <- abs(b[["q"]] - b_star[["q"]]) <= 1.96 * sqrt(Vn["q", "q"])
  }
  band <- mc_band(0.95, R_MC)
  list(ok = mean(hc) >= band[1] && mean(hc) <= band[2] && mean(hn) < band[1],
       detail = paste0("gender coverage: clustered ", fmt(mean(hc), 3),
                       ", naive ", fmt(mean(hn), 3),
                       " | within-respondent attribute: clustered ",
                       fmt(mean(qc), 3), ", naive ", fmt(mean(qn), 3)))
})

## =============================================================================
## D. Simulation and power
## =============================================================================
cat("\nD. Simulation and power\n")

check("D1", "simulation", "Simulated ratings have the theoretical mean", {
  sim <- fs_simulate_ratings(mc_design, n_respondents = 20000, coef = true_b,
                             formula = ~ q + s + cost, phi = 8,
                             sd_respondent = 0, round = FALSE, seed = 31)
  X <- model.matrix(~ q + s + cost, as.data.frame(sim))
  mu <- 10 * plogis(drop(X %*% true_b))
  diff <- mean(sim$rating) - mean(mu)
  list(ok = abs(diff) < 0.02, detail = paste("mean difference", fmt(diff, 4)))
})

check("D2", "simulation", "Simulated yes/no responses have the theoretical rate", {
  sim <- fs_simulate_ratings(mc_design, n_respondents = 20000, coef = true_b,
                             formula = ~ q + s + cost, response = "binary",
                             sd_respondent = 0, seed = 32)
  X <- model.matrix(~ q + s + cost, as.data.frame(sim))
  diff <- mean(sim$response) - mean(plogis(drop(X %*% true_b)))
  list(ok = abs(diff) < 0.005, detail = paste("rate difference", fmt(diff, 4)))
})

check("D3", "power", "Size: a null effect is rejected at about 5%", {
  co <- c("(Intercept)" = 0.5, qyes = 0, syes = 0.3, cost = -0.004)
  pw <- fs_power(mc_design, 150, coef = co, formula = ~ q + s + cost,
                 sd_respondent = 0.6, n_sim = R_MC, seed = 33, verbose = FALSE)
  s <- pw$power[pw$term == "qyes"]
  band <- 0.05 + c(-1, 1) * 2.5 * sqrt(0.05 * 0.95 / R_MC)
  list(ok = s >= band[1] && s <= band[2], detail = paste("rejection rate", fmt(s, 3)))
})

check("D4", "power", "Power rises and MWTP precision improves with sample size", {
  pw <- fs_power(mc_design, c(50, 200, 800), coef = true_b,
                 formula = ~ q + s + cost, price = "cost", sd_respondent = 0.6,
                 n_sim = max(50, R_MC / 4), seed = 34, verbose = FALSE)
  s <- pw[pw$term == "syes", ]
  list(ok = all(diff(s$power) >= 0) && all(diff(s$mwtp_halfwidth) < 0),
       detail = paste0("power ", paste(fmt(s$power, 2), collapse = " -> "),
                       "; half-width ", paste(fmt(s$mwtp_halfwidth, 0), collapse = " -> ")))
})

## =============================================================================
## E. Graphics and housekeeping
## =============================================================================
cat("\nE. Graphics and housekeeping\n")

check("E1", "graphics", "plot() methods draw the estimates they are given", {
  w <- fs_mwtp(fit_b, price = "price")
  b <- ggplot2::ggplot_build(plot(w))
  ok1 <- close_to(sort(b$data[[3]]$x), sort(w$estimates$mwtp))
  e <- fs_ewtp(fit_b, data.frame(main = "grilled fish", side = 1, info = 0),
               price = "price")
  ok2 <- inherits(plot(e), "ggplot") && inherits(plot(fit_b), "ggplot")
  pw <- fs_power(mc_design, c(30, 60), coef = true_b, formula = ~ q + s + cost,
                 n_sim = 5, seed = 1, verbose = FALSE)
  ok3 <- inherits(plot(pw), "ggplot")
  f <- tempfile(fileext = ".png")
  ggplot2::ggsave(f, plot(w), width = 5, height = 3, dpi = 72)
  list(ok = ok1 && ok2 && ok3 && file.size(f) > 0,
       detail = "MWTP points match; all plots render")
})

check("E2", "housekeeping", "Seeded functions leave the caller's RNG untouched", {
  set.seed(99); before <- .Random.seed
  invisible(fs_defficient(f34, 9, seed = 1, verbose = FALSE))
  invisible(fs_mwtp(fit_b, price = "price", method = "krinsky-robb", n_sim = 100, seed = 1))
  invisible(fs_ewtp(fit_b, data.frame(main = "grilled fish", side = 0, info = 0),
                    price = "price", method = "krinsky-robb", n_sim = 100, seed = 1))
  invisible(fs_simulate_ratings(mc_design, 10, true_b, ~ q + s + cost, seed = 1))
  invisible(fs_power(mc_design, 20, true_b, ~ q + s + cost, n_sim = 2, seed = 1,
                     verbose = FALSE))
  list(ok = identical(.Random.seed, before), detail = "")
})

check("E3", "housekeeping", "Missing cluster triggers a warning; FALSE is silent", {
  w1 <- tryCatch({ fs_betareg(rating ~ price, bento, compute_me = FALSE); FALSE },
                 warning = function(w) grepl("independent", conditionMessage(w)))
  w2 <- tryCatch({ fs_betareg(rating ~ price, bento, cluster = FALSE,
                              compute_me = FALSE); TRUE },
                 warning = function(w) FALSE)
  list(ok = isTRUE(w1) && isTRUE(w2), detail = "")
})

## =============================================================================
## Summary
## =============================================================================
elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
n_pass <- sum(results$result == "PASS")
cat("\n==========================================================\n")
cat(sprintf("%d of %d checks passed (%.1f minutes)\n", n_pass, nrow(results), elapsed))
if (any(results$result == "FAIL")) {
  cat("FAILED:\n"); print(results[results$result == "FAIL", c("id", "check", "detail")],
                          row.names = FALSE)
}
out <- file.path(tempdir(), "FactorialSurvey_validation.csv")
if (dir.exists("validation")) {
  out <- file.path("validation", if (quick) "validation_results_quick.csv"
                   else "validation_results.csv")
}
write.csv(results, out, row.names = FALSE, fileEncoding = "UTF-8")
cat("Results written to", out, "\n")
