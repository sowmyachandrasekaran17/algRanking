# algRanking
The algRanking package is a statistical benchmarking tool for stringent comparison and ranking of optimization algorithms. 

## Installation
The package can be installed directly from R 
```R
install.packages("algRanking")
```


## Prerequisite
The results of several optimization methods to be compared is needed. 

## Usage
The main function of the algRanking package is. 



The alg_mcompare function performs multiple pairwise hypothesis testing.

```R
alg_mcompare(x,alpha=0.05,bootstrapping=FALSE)
```

Where 


#' @return returns a list of performance metrics
#' \itemize{
#' \item Main Algorithm- The algorithm which is tested to be outperforming
#' \item Other Compared Algorithm- The algorithm which is compared

#' }

x- a m-dimensional dataframe of the achieved objective values obtained for n runs by various algorithms

alpha	- is the significance level (0< alpha <1, default is 0.05)


bootstrapping	- indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE", accepts "TRUE" or "FALSE", choose "FALSE" for normal data-set and "TRUE" in-case of non-normal data-set

nsample -the number of bootstrap samples, if needed
 severityRequirement- ranges from 0 to 1. Desired post data power.
deltaPS- practically significant delta
The function returns a list of performance metrics

Main Algorithm- The algorithm which is tested to be outperforming

Other Compared Algorithm- The algorithm which is compared

Adjusted P-Value- The P-value of the test adjusted
Decision-H0 either as not-rejected or rejected based on the P-value
 Discrepancy Supported- Until which delta is the severity requirement achieved
Practically Significant Delta- practically significant delta


## Examples
```R

 ## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
  set.seed(123)
A <-abs(rnorm(20,mean=.06,sd=.02))
 set.seed(123)
B <- abs(rnorm(20,mean=.02,sd=.01))
set.seed(123)
C <- abs(rnorm(20,mean=.01,sd=.02))
data <- data.frame(A,B,C)
## Example 1: Compare A and B to check if B outperforms A.
## H0: B does not outperform A vs Ha: B outperforms A
 result_example1 <- alg_mcompare(data)
```

The alg_ranking function ranks the algorithms based on the results from  multiple pairwise hypothesis testing and its corresponding severity results. Each algorithm is given a score and GD  and then ranked.


```R
alg_ranking(df)
```
Where

df- result of the alg_mcompare function, which is a data frame

It returns the algorithm ranked based on the score obtained

Algorithm- The algorithm which is tested to be outperforming

Score- Score obtained by each algorithm

Goal_Differences -Goal_Difference- Goal_Difference obtained by each algorithm

## Examples
```R
 ## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
  set.seed(123)
A <-abs(rnorm(20,mean=.06,sd=.02))
 set.seed(123)
B <- abs(rnorm(20,mean=.02,sd=.01))
set.seed(123)
C <- abs(rnorm(20,mean=.01,sd=.02))
data <- data.frame(A,B,C)
 ## Example 1: Compare A and B to check if B outperforms A.
## H0: B does not outperform A vs Ha: B outperforms A
 result_example1 <- alg_mcompare(data,bootstrapping=FALSE)
 ranking_example <-alg_ranking(result_example1)
```
