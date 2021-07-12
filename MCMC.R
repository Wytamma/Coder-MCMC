#https://nicercode.github.io/guides/mcmc/

p <- 0.4
mu <- c(-1, 2)
sd <- c(.5, 2)
f <- function(x)
  p     * dnorm(x, mu[1], sd[1]) +
  (1-p) * dnorm(x, mu[2], sd[2])


curve(f(x), col="red", -4, 8, n=301, las=1)

q <- function(x) rnorm(1, x, 4)


step <- function(x, f, q) {
  ## Pick new point
  xp <- q(x)
  ## Acceptance probability:
  alpha <- min(1, f(xp) / f(x))
  ## Accept new point with probability alpha:
  if (runif(1) < alpha)
    x <- xp
  ## Returning the point:
  x
}


run <- function(x, f, q, nsteps) {
  res <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps))
    res[i,] <- x <- step(x, f, q)
  drop(res)
}

res <- run(-10, f, q, 1000)


layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(res, type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1)
usr <- par("usr")
xx <- seq(usr[3], usr[4], length=301)
plot(f(xx), xx, type="l", yaxs="i", axes=FALSE, xlab="")


hist(res, 50, freq=FALSE, main="", ylim=c(0, .4), las=1,
     xlab="x", ylab="Probability density")
z <- integrate(f, -Inf, Inf)$value
curve(f(x) / z, add=TRUE, col="red", n=200)
