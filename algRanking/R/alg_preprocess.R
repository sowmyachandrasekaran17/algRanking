#' @title Preprocessing for multiple pairwise comparisons
#' @description The alg_preprocess is to obtain the p-value corrections for multiple pairwise comparisions
#' @author Sowmya Chandrasekaran
#' @param x a \code{m}-dimensional dataframe of the achieved objective values obtained for \code{n} runs by various algorithms
#' @param alpha is the significance level (\code{0< alpha <1}, default is 0.05)
#' @param measure indicates which measure to be compared among algorithms in the hypothesis testing, can take "mean", "median".
#' @param bootstrapping indicates if bootstrapping needs to be performed with hypothesis testing, default "FALSE",
#' accepts "TRUE" or "FALSE", choose "FALSE" for normal data-set and "TRUE" in-case of non-normal data-set
#' @param nsample the number of bootstrap samples, if needed
#' @import stats
#' @export
#' @return returns a list of performance metrics
#' \itemize{
#' \item Main Algorithm- The algorithm which is tested to be outperforming
#' \item Other Compared Algorithm- The algorithm which is compared
#' \item P-Value- The P-value of the test
#' \item Adjusted P-Value- The Adjusted P-value of the test according to the methods of Benjamini, Hochberg, and Yekutieli to control the false discovery rate
#' }

alg_preprocess <-  function(x,alpha,measure,bootstrapping,nsample,severityRequirement,budget)
{

  ### Multiple pairwise comparison
  pre_result <- data.frame(matrix(ncol = 5, nrow = 0))

  for (mi in colnames(x)){
    Alg_new <- (x[mi])
    Alg_new_unbiased <- (x[mi])
    comparison_df <-x[,!names(x) %in% names(x[mi])]
    for (mc in 1:length(comparison_df)){
      Alg_bm <-(comparison_df[mc])
      Alg_new <- Alg_new_unbiased


      ##### Handling negative values if observed for Alg_bm, Alg_new with bias factor
      bias <- FALSE
      min_Algbm <- 0
      min_Algnew <- 0


      Alg_bm[is.na(Alg_bm)] <- budget
      Alg_new[is.na(Alg_new)] <- budget
#
#       if(any(is.na(Alg_bm)))
#       {
# missing_pointer <- which(is.na(Alg_bm))
# Alg_bm[missing_pointer] <-max(Alg_bm,na.rm = TRUE)
#       }
#
#
#       if(any(is.na(Alg_new)))
#       {
#         missing_pointer <- which(is.na(Alg_new))
#         Alg_new[missing_pointer] <-max(Alg_new,na.rm = TRUE)
#       }

      if(any(Alg_bm<0)){
        bias <- TRUE
        min_Algbm <- min(Alg_bm)
      }

      if(any(Alg_new<0)){
        bias <- TRUE
        min_Algnew <- min(Alg_new)
      }

      if(bias==TRUE){

        bias_value <- min(min_Algbm,min_Algnew)
        Alg_bm <- Alg_bm+abs(bias_value)
        Alg_new <- Alg_new+abs(bias_value)
        # warning("Negative values were observed for Alg_bm, Alg_new or both. Hence the data is appropriately biased for testing. However this does not affect the comparison.")

      }
      ### Hypothesis Testing ###


      mu0<- 0 # null hypothesis is that the difference d between the two population means should be zero
      d <- (Alg_bm[[1]]-Alg_new[[1]]) # difference between both algorithms
      n <- length(d) # total number of data points

      sev_list <- severityRequirement
      l=length(sev_list)
      discrepancy_supported_sr <- NULL
      discrepancy_supported_snr <- NULL

      discrepancy_supported_bsr <- NULL
      discrepancy_supported_bsnr <- NULL

      if(measure=="mean"){
        d_measure <- mean(d) # mean value of the differences
      }
      else if (measure=="median")
      {
        d_measure <- median(d) # median value of the differences
      }

      d_sigma <- sd(d)/ sqrt(n) # standard deviation of the differences
      sigma_x_inv <-  1 / d_sigma

### Discrepancy supported for required severity level

      discrepancy_init_sr=qt(sev_list,(n-1))
      discrepancy_supported_sr=rep(d_measure,l)-(discrepancy_init_sr/sigma_x_inv)
      discrepancy_supported_sr <- round(discrepancy_supported_sr,digits = 2)


      discrepancy_init_snr=qt(sev_list,(n-1),lower.tail = FALSE)
      discrepancy_supported_snr=rep(d_measure,l)-(discrepancy_init_snr/sigma_x_inv)
      discrepancy_supported_snr <- round(discrepancy_supported_snr,digits = 2)

#print("discrepancy_supported_sr")

#print(discrepancy_supported_sr)

#print("discrepancy_supported_snr")

#print(discrepancy_supported_snr)

      if(isFALSE(bootstrapping)){

        t_alpha <-  qt((1-alpha), df=n-1) # cut-off region for t-distribution
        t <-  sigma_x_inv * (d_measure - mu0) # test statistic, t
        p <-  pt(t, df=n-1, lower.tail=FALSE) # p-value

          pre_result <- rbind(pre_result,list(colnames(Alg_new),colnames(Alg_bm),p,discrepancy_supported_sr,discrepancy_supported_snr))

      }


      if(isTRUE(bootstrapping)){

        test_statistic <-(mean(Alg_bm[[1]]-Alg_new[[1]]))

        Bootstrapping_data <- c(Alg_bm[[1]],Alg_new[[1]])
        # Perform bootstrapping with replacement


n_Alg_bm<-length(Alg_bm[[1]])




        n_obs <- length(Bootstrapping_data)  # the number of observations to sample

        B <- nsample  # the number of bootstrap samples.
        Boot_test_stat <- rep(0,B)
        Boot_test_stat_sev_a <- matrix(nrow=l,ncol=B) # For severity calculation
        Boot_test_stat_sev_r <- matrix(nrow=l,ncol=B) # For severity calculation
        if(n_Alg_bm>30)
        {
        # Form each Boot-sample as a column
        set.seed(1000)   # seed for reproducibility
        Bootstrapped_data <- matrix( sample(Bootstrapping_data, size= B*n_obs,replace=TRUE), ncol=B, nrow=n_obs)
        }
        else
        {

          #  permutation data
          Bootstrapped_data <- matrix(0, nrow=n_obs, ncol=B)
          set.seed(1000)
          for(i in 1:B){
            Bootstrapped_data[,i] <- sample(Bootstrapping_data, size= n_obs, replace=FALSE)
          }}


        for (i in 1:B){
          # calculate the bootstrapping-test-statistic
          Boot_test_stat[i] <-(mean(Bootstrapped_data[(1:n_Alg_bm),i])-mean(Bootstrapped_data[(n_Alg_bm+1):nrow(Bootstrapped_data),i]))
        }

        #calculate the p-value
        p <- mean( Boot_test_stat >= test_statistic)
       # print("p")
      #  print(p)

#
# sev_r_est <-mean(abs( Boot_test_stat - discrepancy_supported ) >= test_statistic)
# print("sev_r_est")
# print(sev_r_est)
# sev_nr_est <-mean( (Boot_test_stat + discrepancy_supported ) >= test_statistic)
# print("sev_nr_est")
# print(sev_nr_est)


        #### For severity to RejectH0

for( loop_sev in 1:length(sev_list))
{
  ctr_loop=1
  while(ctr_loop <=12){

    ##  target function for optimization:
    f <- function(delta){
      (delta) #discrepancy
    }
    contr <- list()  #control list
    #opts <- list("xtol_rel"=1.0e-16)
    #opts <- list()
    contr$eval_g_ineq <- function(delta) (mean(abs( Boot_test_stat - delta ) <=abs(test_statistic))-severityRequirement)
  bound<-bound<-1e-6*10^ctr_loop

    res <- optimNLOPTR(,fun=f,lower=0,upper=bound,control=contr)

        if(res$ybest!=Inf)
          break
        else
          ctr_loop=ctr_loop+1
      }

   discrepancy_supported_bsr <- c(discrepancy_supported_bsr,res$xbest)
   discrepancy_supported_bsr <- round(discrepancy_supported_bsr,digits = 2)
 #  print("discrepancy_supported_bsr")
  # print(discrepancy_supported_bsr)
}



        #### For severity to notRejectH0

        for( loop_sev in 1:length(sev_list))
        {
          ctr_loop=1
          while(ctr_loop <=12){

            ##  target function for optimization:
            f <- function(delta){
              (-delta) #discrepancy
            }
            contr <- list()  #control list
            #opts <- list("xtol_rel"=1.0e-16)
            #opts <- list()
            contr$eval_g_ineq <- function(delta) (mean(( Boot_test_stat + delta ) > (test_statistic))-severityRequirement)
            bound<-bound<-1e-6*10^ctr_loop

            res <- optimNLOPTR(,fun=f,lower=-bound,upper=0,control=contr)

            if(res$ybest!=Inf)
              break
            else
              ctr_loop=ctr_loop+1
          }

          discrepancy_supported_bsnr <- c(discrepancy_supported_bsnr,res$xbest)
          discrepancy_supported_bsnr <- round(discrepancy_supported_bsnr,digits = 2)
         # print("discrepancy_supported_bsnr")
          #print(discrepancy_supported_bsnr)
          #print("............")
        }


        pre_result <-    rbind(pre_result,list(colnames(Alg_new),colnames(Alg_bm),p,discrepancy_supported_bsr,discrepancy_supported_bsnr))
        }
  }}
input_p <-pre_result[,3]
Adjusted_pValue <-p.adjust(input_p, method = "BH")
preprocess_result <-cbind(pre_result,Adjusted_pValue)
names(preprocess_result) <- c("Main Algorithm","Other Compared Algorithm","P-Value", "Discrepancy Supported-Reject","Discrepancy Supported-notReject", "Adjusted P-Value")
 return(preprocess_result)
  }

