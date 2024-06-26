% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alg_mcompare.R
\name{alg_mcompare}
\alias{alg_mcompare}
\title{Comparison of the performances of multiple optimization algorithms with user-defined discrepancies}
\usage{
alg_mcompare(
  x,
  alpha = 0.05,
  bootstrapping = FALSE,
  measure = "mean",
  nsample = 10000,
  severityRequirement = 0.8,
  deltaPS = 1e-04,
  budget = 50000
)
}
\arguments{
\item{x}{a \code{m}-dimensional dataframe of the achieved objective values obtained for \code{n} runs by various algorithms}

\item{alpha}{is the significance level (\code{0< alpha <1}, default is 0.05)}

\item{bootstrapping}{indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE",
accepts "TRUE" or "FALSE", choose "FALSE" for normal data-set and "TRUE" in-case of non-normal data-set}

\item{measure}{indicates which measure to be compared among algorithms in the hypothesis testing, can take "mean", "median", "mode".}

\item{nsample}{the number of bootstrap samples, if needed}

\item{severityRequirement}{ranges from 0 to 1. Desired post data power.}

\item{deltaPS}{practically significant delta}
}
\value{
returns a list of performance metrics
\itemize{
\item Main Algorithm- The algorithm which is tested to be outperforming
\item Other Compared Algorithm- The algorithm which is compared
\item Adjusted P-Value- The P-value of the test adjusted
\item Decision-\code{H0} either as not-rejected or rejected based on the P-value
\item Discrepancy Supported- Until which delta is the severity requirement achieved
\item Practically Significant Delta- practically significant delta
}
}
\description{
The alg_mcompare function performs multiple pairwise hypothesis testing. The important feature of this function is the severity measure, a key statistic that stringently validates the performance in presence of small discrepancies. This discrepancy accounts for the practically insignificant improvement, which could have occurred due to several factors such as computer accuracy (i.e., floating points), variable types (4-byte float, 8-byte float, 10-byte float), or even the stopping criteria that is the error threshold when the algorithms are stopped.
}
\examples{
## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
 set.seed(123)
A <-abs(rnorm(20,mean=.06,sd=.02))
set.seed(123)
B <- abs(rnorm(20,mean=.0002,sd=.01))
set.seed(123)
C <- abs(rnorm(20,mean=.01,sd=.02))
data <- data.frame(A,B,C)
## Example 1: Compare A and B to check if B outperforms A.
## H0: B does not outperform A vs Ha: B outperforms A
result_example1 <- alg_mcompare(data,bootstrapping=FALSE,measure="mean")
## Prerequisite 2: Create sphere function and optimize it with Nelder-Mead,BFGS and SANN for 10 runs
## Sphere function
sphere <- function(xx)
{
sum <- sum(xx^2)
y <- sum
return(y)
}
 ## Initialization of variables
nm_optim <- NULL
sann_optim <-NULL
bfgs_optim <- NULL
runs <- 10
set.seed(123)
## Optimizing Sphere function with NM and SANN
for(i in 1:runs){
 start <- (runif(2,min=(-5),max=(5)))# random initial start at each run
 nm <-optim(start, sphere, method = "Nelder-Mead")
 nm_mean <- (nm$value)
 nm_optim <- c(nm_optim,nm_mean)# Complete Results for NM Algorithm
 sann <- optim(start, sphere, method = "SANN")
 sann_mean <- (sann$value)
 sann_optim <- c(sann_optim,sann_mean)# Complete Results for SANN Algorithm
 bfgs <- optim(start, sphere, method = "BFGS")
 bfgs_mean <- (bfgs$value)
 bfgs_optim <- c(bfgs_optim,bfgs_mean)# Complete Results for BFGS Algorithm
}
data <- data.frame(nm_optim,sann_optim,bfgs_optim)
result_example2 <- alg_mcompare(data,bootstrapping=FALSE,measure="mean")
}
\author{
Sowmya Chandrasekaran
}
