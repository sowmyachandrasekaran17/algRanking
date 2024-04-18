#' @title Comparison of the performances of multiple optimization algorithms with user-defined discrepancies
#' @description The alg_mcompare function performs multiple pairwise hypothesis testing. The important feature of this function is the severity measure, a key statistic that stringently validates the performance in presence of small discrepancies. This discrepancy accounts for the practically insignificant improvement, which could have occurred due to several factors such as computer accuracy (i.e., floating points), variable types (4-byte float, 8-byte float, 10-byte float), or even the stopping criteria that is the error threshold when the algorithms are stopped.
#' @author Sowmya Chandrasekaran
#' @param x a \code{m}-dimensional dataframe of the achieved objective values obtained for \code{n} runs by various algorithms
#' @param alpha is the significance level (\code{0< alpha <1}, default is 0.05)
#' @param bootstrapping indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE",
#' accepts "TRUE" or "FALSE", choose "FALSE" for normal data-set and "TRUE" in-case of non-normal data-set
#' @param measure indicates which measure to be compared among algorithms in the hypothesis testing, can take "mean", "median", "mode".
#' @param nsample the number of bootstrap samples, if needed
#' @param severityRequirement ranges from 0 to 1. Desired post data power.
#' @param deltaPS practically significant delta
#' @import stats
#' @importFrom DescTools AUC
#' @export
#' @return returns a list of performance metrics
#' \itemize{
#' \item Main Algorithm- The algorithm which is tested to be outperforming
#' \item Other Compared Algorithm- The algorithm which is compared
#' \item Adjusted P-Value- The P-value of the test adjusted
#' \item Decision-\code{H0} either as not-rejected or rejected based on the P-value
#' \item Discrepancy Supported- Until which delta is the severity requirement achieved
#' \item Practically Significant Delta- practically significant delta
#' }
#' @examples
#' ## Prerequisite 1: Create two vectors A and B. Let A and B be the optimum achieved.
#'  set.seed(123)
#'A <-abs(rnorm(20,mean=.06,sd=.02))
#' set.seed(123)
#'B <- abs(rnorm(20,mean=.0002,sd=.01))
#'set.seed(123)
#'C <- abs(rnorm(20,mean=.01,sd=.02))
#'data <- data.frame(A,B,C)
#' ## Example 1: Compare A and B to check if B outperforms A.
#'## H0: B does not outperform A vs Ha: B outperforms A
#' result_example1 <- alg_mcompare(data,bootstrapping=FALSE,measure="mean")
#' ## Prerequisite 2: Create sphere function and optimize it with Nelder-Mead,BFGS and SANN for 10 runs
#' ## Sphere function
#' sphere <- function(xx)
#' {
#' sum <- sum(xx^2)
#' y <- sum
#' return(y)
#' }
#'  ## Initialization of variables
#'nm_optim <- NULL
#'sann_optim <-NULL
#'bfgs_optim <- NULL
#'runs <- 10
#'set.seed(123)
#' ## Optimizing Sphere function with NM and SANN
#'for(i in 1:runs){
#'  start <- (runif(2,min=(-5),max=(5)))# random initial start at each run
#'  nm <-optim(start, sphere, method = "Nelder-Mead")
#'  nm_mean <- (nm$value)
#'  nm_optim <- c(nm_optim,nm_mean)# Complete Results for NM Algorithm
#'  sann <- optim(start, sphere, method = "SANN")
#'  sann_mean <- (sann$value)
#'  sann_optim <- c(sann_optim,sann_mean)# Complete Results for SANN Algorithm
#'  bfgs <- optim(start, sphere, method = "BFGS")
#'  bfgs_mean <- (bfgs$value)
#'  bfgs_optim <- c(bfgs_optim,bfgs_mean)# Complete Results for BFGS Algorithm
#'}
#'data <- data.frame(nm_optim,sann_optim,bfgs_optim)
#'result_example2 <- alg_mcompare(data,bootstrapping=FALSE,measure="mean")

alg_mcompare <-  function(x,alpha=0.05,bootstrapping=FALSE,measure="mean",nsample=10000,severityRequirement=0.8,deltaPS=0.0001,budget=50000)
{

  ## Input Validation

  if(is.null(x)){
    stop("alg_mcompare: no data was specified for variable x")
  }
  if(!is.data.frame(x)){
    stop("alg_mcompare: x has to be a data.frame")
  }
  if(any(is.nan(unlist(x)))){
    stop("alg_mcompare: The specified data for x contained NaNs")
  }
  if(!all(apply(x,2,is.numeric))){
    stop("alg_mcompare: one or more columns in x contain non-numeric data")
  }
  if(measure!="mean" &&  measure!="median"){
    stop("Please choose appropriate measure. It must be mean or median")
  }
  alpha <-  as.numeric(alpha)


  severityRequirement <- as.numeric(severityRequirement)
  if(class(severityRequirement) != "numeric")
  {
    stop("severityRequirement should be numeric")
  }

  budget <- as.numeric(budget)
  if(class(budget) != "numeric")
  {
    stop("budget should be numeric")
  }

  if(!isFALSE(bootstrapping) && !isTRUE(bootstrapping)){
    stop("bootstrapping parameter not correctly chosen. It must be either TRUE or FALSE")
  }



  #######Pre-processing

  preprocess_result <-alg_preprocess(x,alpha,measure,bootstrapping,nsample,severityRequirement,budget)
  matchcount=1
  ### Multiple pairwise comparison
result <- data.frame(matrix(ncol = 6, nrow = 0))

  for (mi in colnames(x)){
Alg_new <- (x[mi])
Alg_new_unbiased <- (x[mi])
comparison_df <-x[,!names(x) %in% names(x[mi])]
for (mc in 1:length(comparison_df)){
Alg_bm <-(comparison_df[mc])
Alg_new <- Alg_new_unbiased


  ### Hypothesis Testing  Formulation for Normal Data sets###
Adjusted_pValue <-preprocess_result[matchcount,6]
discrepancy_supported_sr <-preprocess_result[matchcount,4]
discrepancy_supported_snr <- preprocess_result[matchcount,5]
  if(isFALSE(bootstrapping)){


    if(Adjusted_pValue >alpha) # not-reject H0
    {
      decision <-  "notReject H0" # reject null hypothesis
      discrepancy_supported <- discrepancy_supported_snr
      result <-    rbind(result,list(colnames(Alg_new),colnames(Alg_bm),Adjusted_pValue,decision,discrepancy_supported,deltaPS))


    }
    if(Adjusted_pValue<= alpha) # Reject H0
    {
      decision <-  "Reject H0" # reject null hypothesis
      discrepancy_supported <- discrepancy_supported_sr
      result <-    rbind(result,list(colnames(Alg_new),colnames(Alg_bm),Adjusted_pValue,decision,discrepancy_supported,deltaPS))

    }
}



  ######### Non-normal Data-sets bootstrapping with Hypothesis testing
  if(isTRUE(bootstrapping)){

    if( Adjusted_pValue<=alpha){
      decision <-  "Reject H0" # reject null hypothesis
      discrepancy_supported <- discrepancy_supported_sr


      # for( loop_sev in 1:length(sev_list))
      # {
      #   ctr_loop=1
      #   while(ctr_loop <=12){
      #     ##  target function for optimization:
      #     f <- function(delta){
      #       (delta) #discrepancy
      #     }
      #     contr <- list()  #control list
      #     #opts <- list("xtol_rel"=1.0e-16)
      #     #opts <- list()
      #     contr$eval_g_ineq <- function(delta) (-sev_list[loop_sev]+pt(sigma_x_inv * (d_measure - (mu0+delta)) , df=n-1, lower.tail=TRUE))
      #     bound<-1e-6*10^ctr_loop
      #
      #     res <- optimNLOPTR(,fun=f,lower=-bound,upper=bound,control=contr)
      #     #res <- optimNLOPTR(,fun=f,lower=-bound,upper=bound,control=contr,opts=opts)
      #     if(res$ybest!=Inf)
      #       break
      #     else
      #       ctr_loop=ctr_loop+1
      #   }
      #   discrepancy_supported <- c(discrepancy_supported,res$xbest)
      #   discrepancy_supported <- round(discrepancy_supported,digits = 2)
      # }


      result <-     rbind(result,list(colnames(Alg_new),colnames(Alg_bm),Adjusted_pValue,decision,discrepancy_supported,deltaPS))
    }
    if(Adjusted_pValue>alpha){
      decision <-  "notReject H0" # not reject null hypothesis

      discrepancy_supported <- discrepancy_supported_snr
      result <-     rbind(result,list(colnames(Alg_new),colnames(Alg_bm),Adjusted_pValue,decision,discrepancy_supported,deltaPS))
    }}
matchcount=matchcount+1
}}

names(result) <- c("Main Algorithm","Other Compared Algorithm","Adjusted P-Value","Decision","Discrepancy Supported","Practically Significant Delta")
return(result)
}
