#fcalc.R

#Here are cosine and sine functions that oscillate m times in N months
data <- as.numeric(AirPassengers);data

myCos <- function(m,N) cos((1:N)*m*2*pi/N)
mySin <- function(m,N) sin((1:N)*m*2*pi/N)

#These functions evaluate Euler's formula for the coefficients $a_n$ and #$b_n$ by computing Riemann sums.
coeffA <- function(m,N,v){
  sum(2*v*myCos(m,N)/N)
}
coeffB <- function(m,N,v){
  sum(2*v*mySin(m,N)/N)
}

fa.makeCoefficients <- function(data){
  N <- length(data)
  FourierA <- sapply(1:(N/2),FUN = coeffA, N = length(data), v = data)
  FourierB <- sapply(1:(N/2),FUN = coeffB, N = length(data), v = data)
  Fourier <- sqrt(FourierA^2+FourierB^2)
  return(list(cos = FourierA, sin = FourierB, abs =Fourier, mean = mean(data)))
}
cf <- fa.makeCoefficients(data);cf

cf$mean

fa.reconstruct <- function(mean, fcos, fsin, n){
  N <- 2*length(fcos)
  v <- mean
  for (i in 1:n) {
    v <- v + fcos[i]*myCos(i,N)+ fsin[i]*mySin(i,N)
  }
  return (v)
}

dset <<- as.numeric(AirPassengers);dset

dset <<- read.csv("flu2016.csv")$influ
dset <<- as.numeric(sunspots)

AirPassengers
dset <<- read.csv("amzn.csv");dset

dset <<- read.csv("amzn.csv")$close
dset
