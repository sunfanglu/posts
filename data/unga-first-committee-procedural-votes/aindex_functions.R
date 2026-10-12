## Alignment index (A-index) and clique partitioning, following
## Benati & Capurri (2026), Quality & Quantity 60:16075-16107, doi:10.1007/s11135-026-02814-x
## Votes coded X = 1 (Yes), 2 (Abstain), 3 (No).  S-index, Eq. (1): s_ij = 1 - mean_k |x_ik - x_jk| over K_ij.
## Expected S under independent voting, Eq. (4): s0_ij = 1 - sum_{u,v} p_iu p_jv |u - v|.
## A-index, Eq. (5): a_ij = s_ij - s0_ij.  Generalised, Eq. (15): a^lambda_ij = s_ij - lambda * s0_ij.
code3 <- function(v) unname(c(Yes = 1, Abstain = 2, No = 3)[as.character(v)])
D <- abs(outer(1:3, 1:3, "-"))                     # penalty matrix of Table 1(a)
marg <- function(x) tabulate(x, 3) / length(x)
s0_from <- function(pi, pj) 1 - sum(outer(pi, pj) * D)

## Pairwise indices for two vote vectors (NA = absent/missing).
## marginals = "joint": p estimated on K_ij (Table 1 / worked example, K_i = K_j = K_ij);
## marginals = "own":   p estimated on K_i and K_j separately (literal Eq. 3).
pair_index <- function(a, b, marginals = c("joint","own"), lambda = 1) {
  marginals <- match.arg(marginals); ok <- !is.na(a) & !is.na(b); n <- sum(ok)
  if (n == 0) return(c(n = 0, lijphart = NA, S = NA, S0 = NA, A = NA))
  xa <- a[ok]; xb <- b[ok]; d <- abs(xa - xb)
  S <- 1 - mean(d)
  pa <- if (marginals == "joint") marg(xa) else marg(a[!is.na(a)])
  pb <- if (marginals == "joint") marg(xb) else marg(b[!is.na(b)])
  S0 <- s0_from(pa, pb)
  c(n = n, lijphart = mean(c(1, .5, 0)[d + 1]), S = S, S0 = S0, A = S - lambda * S0)
}

## Full matrices from a vote matrix X (rows = votes, cols = countries; 1/2/3/NA). Vectorised via indicator matrices.
amatrix <- function(X, marginals = "joint", lambda = 1, min_n = 1) {
  Y <- (X == 1) * 1; Y[is.na(Y)] <- 0; Ab <- (X == 2) * 1; Ab[is.na(Ab)] <- 0; N <- (X == 3) * 1; N[is.na(N)] <- 0
  P <- (!is.na(X)) * 1
  n <- crossprod(P)
  # observed mean distance on K_ij: |1-2|=1, |2-3|=1, |1-3|=2
  dist <- crossprod(Y, Ab) + crossprod(Ab, Y) + crossprod(Ab, N) + crossprod(N, Ab) + 2 * (crossprod(Y, N) + crossprod(N, Y))
  S <- 1 - dist / n
  if (marginals == "joint") {      # marginal of i on K_ij: counts of i's votes on votes where j present / n_ij
    pY <- crossprod(Y, P) / n; pA <- crossprod(Ab, P) / n; pN <- crossprod(N, P) / n   # pX[i,j] = share of i on K_ij
    qY <- t(pY); qA <- t(pA); qN <- t(pN)                                                # share of j on K_ij
  } else {
    k <- colSums(P); oy <- colSums(Y) / k; oa <- colSums(Ab) / k; on <- colSums(N) / k
    pY <- matrix(oy, length(k), length(k)); pA <- matrix(oa, length(k), length(k)); pN <- matrix(on, length(k), length(k))
    qY <- t(pY); qA <- t(pA); qN <- t(pN)
  }
  S0 <- 1 - (pY * qA + pA * qY + pA * qN + pN * qA) - 2 * (pY * qN + pN * qY)
  A <- S - lambda * S0
  A[n < min_n] <- NA; S[n < min_n] <- NA; diag(A) <- NA; diag(S) <- NA
  list(A = A, S = S, S0 = S0, n = n)
}

## ---- Clique partitioning: maximise sum_{i<j same cluster} w_ij (Eq. 8-12) ----
cp_objective <- function(W, lab) { same <- outer(lab, lab, "=="); sum(W[same & upper.tri(W)]) }

## Heuristic: Louvain-style local moving + cluster aggregation for signed weights, many random restarts.
cp_local <- function(W, lab, rng_order) {      # fast local moving with incremental gain matrix G = W %*% Z
  lab <- as.integer(factor(lab)); k <- max(lab); n <- nrow(W)
  G <- W %*% sapply(seq_len(k), function(c) (lab == c) * 1); if (is.null(dim(G))) G <- matrix(G, n)
  size <- tabulate(lab, k)
  repeat {
    moved <- FALSE
    for (i in rng_order) {
      c <- lab[i]; g <- G[i, ]; cur <- g[c]; g[c] <- -Inf
      empty <- which(size == 0); g[empty] <- -Inf
      b <- which.max(g); best <- if (length(b) && is.finite(g[b])) g[b] else -Inf
      tgt <- NA
      if (best > cur + 1e-12 && best >= 0 - 1e-12) tgt <- b
      else if (cur < -1e-12 && size[c] > 1) {          # better alone (gain 0)
        if (best > 1e-12) tgt <- b else { if (length(empty)) tgt <- empty[1] else { G <- cbind(G, 0); size <- c(size, 0); tgt <- ncol(G) } }
      } else if (best > cur + 1e-12) tgt <- b
      if (!is.na(tgt)) { G[, c] <- G[, c] - W[, i]; G[, tgt] <- G[, tgt] + W[, i]; size[c] <- size[c] - 1; size[tgt] <- size[tgt] + 1; lab[i] <- tgt; moved <- TRUE }
    }
    if (!moved) break
  }
  as.integer(factor(lab))
}
cp_climb <- function(W, lab) {                 # local moving + aggregation until no improvement
  n <- nrow(W); lab <- cp_local(W, lab, sample(n))
  repeat {
    k <- max(lab); if (k < 2) break
    Z <- sapply(seq_len(k), function(c) (lab == c) * 1); M <- t(Z) %*% W %*% Z; diag(M) <- 0
    if (max(M) <= 1e-12) break
    sup <- cp_local(M, seq_len(k), sample(k)); if (max(sup) == k) break
    lab <- cp_local(W, sup[lab], sample(n))
  }
  lab
}
## Heuristic: random-restart + iterated local search (perturb 10% of nodes, re-climb, accept if better).
cp_heuristic <- function(W, restarts = 100, ils = 30, seed = 20261012) {
  set.seed(seed); W[is.na(W)] <- 0; diag(W) <- 0; n <- nrow(W); best <- NULL; bestv <- -Inf; objs <- numeric(restarts); sols <- list()
  for (r in seq_len(restarts)) {
    lab <- cp_climb(W, seq_len(n)); v <- cp_objective(W, lab)
    for (t in seq_len(ils)) {
      l2 <- lab; m <- max(1, round(n / 10)); pick <- sample(n, m); l2[pick] <- sample(c(unique(lab), max(lab) + 1), m, TRUE)
      l2 <- cp_climb(W, as.integer(factor(l2))); v2 <- cp_objective(W, l2)
      if (v2 > v + 1e-9) { lab <- l2; v <- v2 }
    }
    objs[r] <- v; sols[[r]] <- lab
    if (v > bestv + 1e-9) { bestv <- v; best <- lab }
  }
  list(labels = setNames(best, rownames(W)), objective = bestv, restarts = restarts,
       share_best = mean(abs(objs - bestv) < 1e-6), all = sols, objs = objs)
}

## Exact ILP (Eq. 8-12) with lpSolve, for small n only (used to validate the heuristic).
cp_exact <- function(W) {
  W[is.na(W)] <- 0; n <- nrow(W); pr <- which(upper.tri(W), arr.ind = TRUE); pr <- pr[order(pr[,1], pr[,2]), , drop = FALSE]
  idx <- matrix(0, n, n); idx[pr] <- seq_len(nrow(pr)); idx <- idx + t(idx)
  tri <- t(combn(n, 3)); rows <- list()
  for (t in seq_len(nrow(tri))) { i <- tri[t,1]; j <- tri[t,2]; k <- tri[t,3]
    rows[[length(rows)+1]] <- c(idx[i,j], idx[j,k], idx[i,k], 1, 1, -1)
    rows[[length(rows)+1]] <- c(idx[i,j], idx[j,k], idx[i,k], 1, -1, 1)
    rows[[length(rows)+1]] <- c(idx[i,j], idx[j,k], idx[i,k], -1, 1, 1) }
  m <- length(rows); dense <- matrix(0, m, nrow(pr))
  for (r in seq_len(m)) dense[r, rows[[r]][1:3]] <- rows[[r]][4:6]
  sol <- lpSolve::lp("max", W[pr], dense, rep("<=", m), rep(1, m), all.bin = TRUE)
  x <- sol$solution; same <- matrix(FALSE, n, n); same[pr[x > .5, , drop = FALSE]] <- TRUE; same <- same | t(same); diag(same) <- TRUE
  lab <- rep(NA, n); c <- 0; for (i in 1:n) if (is.na(lab[i])) { c <- c + 1; lab[same[i, ]] <- c }
  list(labels = setNames(lab, rownames(W)), objective = sol$objval, status = sol$status)
}
ari <- function(a, b) mclust::adjustedRandIndex(a, b)
