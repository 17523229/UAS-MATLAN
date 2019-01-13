
#UAS SEMESTER 3 - MATEMATIKA LANJUT - HARRY AKBAR A - 17523229

# NO 1 - JAWABAN (D)

x <- c(50, 51, 52, 53, 54)
y <- c(40, 46, 44, 55, 49)

relation <- lm(y~x)
print(relation)

# No 2 - JAWABAN (B)

x <- c(50, 51, 52, 53, 54)
y <- c(40, 46, 44, 55, 49)

relation <- lm(y~x)
a <- data.frame(x = 55)
result <- predict(relation,a)
print(result)

relation <- lm(y~x)
print(relation)

#No 3 - JAWABAN(C)

x <- c(0, 1, 2, 3, 4)
y <- c(1, 2.25, 3.75, 4.25, 5.65)

b <- poly.calc(x, y)
b

#No 4 - JAWABAN(B)

x <- c(0, 1, 2, 3, 4)
y <- c(1, 2.25, 3.75, 4.25, 5.65)

relation <- poly.calc(x, y)
d <- data.frame(x = 2.75)
result <- predict(relation, d)
result

#No 5 - JAWABAN(C)

f1 <- function(x){
  result <- 1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4
  return(result)
}
plot(x, y)
curve(f1, add = TRUE)

#No 6 - JAWABAN(C)

bi <- function(a, b){
  re <- 3
  pn <- (a + b) / 2
  while(re >= 0.0001){
    print(paste(a, b, pn, fx(pn), fx(a), re, sep = " "))
    p <- pn
    if (sign(g(p)) == sign(g(a))){
      a <- p
    } else {
      b <- p
    }
    pn <- (a + b) / 2
    re <- abs(pn - p) / abs(pn)
  }
}

#NO 7 - JAWABAN(D)
#NO 8 - JAWABAN(D)
#NO 9 - JAWABAN(A)
#nO 10 - JAWABAN(D)

#NO 11 - JAWABAN(A)

tes1<-function(x){
  return(x^2 - 6)
  
}
trapzfun(tes1,1,0)
  
#NO 12 - JAWABAN (A)

tes2<-function(x){
  return(x^3+(4*x^2)-10)
  
}
trapzfun(tes2,2,1)

#NO 13 - JAWABAN(C)

h <- 0.1
x <- seq(0.1, by = h)
f2 <- function(x){
  return(x^2)
}
f0 <- F(X[1])
fi <- sapply(x[2:10], f2)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  return(h/2 * f0 + 2 * sum(fi)-1 + fn)
}
trap(f0, fi, fn, h)

#NO 14 - JAWABAN (B)

h <- 0.2
x <- seq(0.1, by = h)
f2 <- function(x){
  return(x^2)
}
f0 <- F(X[1])
fi <- sapply(x[2:10], f2)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L <- h/2 * f0 + 2 * sum(fi)-1 + fn
  return(L)
}
trap(f0, fi, fn, h)

#NO 15 - JAWABAN (B)




