normalise <- function(x) {
  x / sum(x)
}

as_stochastic <- function(x) {
  for (i in 1:nrow(x)) {
    x[i, ] <- x[i, ] / sum(x[i, ])
  }
  x
}

matrix_pow <- function(A, n) {
  if (n == 1) return(A)
  if (n %% 2 == 0) return(matrix_pow(A %*% A, n / 2))
  if (n %% 2 == 1) return(A %*% matrix_pow(A %*% A, (n-1) / 2))
}

# markov_chain <- function(row_x0, P, iter) {
#   decomp <- eigen(P)
#   U <- decomp$vector
#   Dn <- diag(decomp$value ^ iter)
#   ((row_x0 %*% U) %*% Dn) %*% solve(U)
# }

markov_chain <- function(row_x0, P, iter) {
  row_x0 %*% matrix_pow(P, iter)
}

# 1 state, multiple steps
sample_markov_chain <- function(s0, P, iter) {
  n <- ncol(P)
  track <- s0
  for (i in 1:iter) {
    s0 <- sample(n, 1, prob = P[s0, ])
    track <- c(track, s0)
  }
  track
}

# 1 state, 1 step, multiple replicates
one_step <- function(s0, P, n) {
  sample(ncol(P), n, prob = P[s0, ], replace = TRUE)
}
