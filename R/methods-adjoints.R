adjs <- list()

defadj <- function(f, ...) {
  ajs <- list(...)
  h <- list()
  for (i in seq(ajs)) {
    h[[i]] <- ajs[[i]]
  }
  adjs[[f]] <<- h
}


f1 <- function(x, y) 1
f2 <- function(x, y) 1
defadj("+", f1, f2)

f1 <- function(x, y) 1
f2 <- function(x, y) -1
defadj("-", f1, f2)

f1 <- function(x, y) y
f2 <- function(x, y) x
defadj("*", f1, f2)

f1 <- function(x, y) 1 / y
f2 <- function(x, y) x / y**2
defadj("/", f1, f2)

f1 <- function(x, y) {
  ps <- ifelse(y != 0, y - 1, 1)
  y * x ^ ps
}
f2 <- function(x, y) {
  ps <- ifelse(x == 0, 1, x)
  log(ps * x ^ y)
}
defadj("^", f1, f2)
