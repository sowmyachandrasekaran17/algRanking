#' @title Ranking of the optimization algorithms based on mc
#' @param df result of the alg_mcompare function, which is a data frame
#' @import stats
#' @export
#' @return returns the algorithm ranked based on the score obtained
#' \itemize{
#' \item Algorithm- The algorithm which is tested to be outperforming
#' \item Score- Score obtained by each algorithm
#' \item Goal_Difference- Goal_Difference obtained by each algorithm
#' \item Ranking - Rank obtained by each algorithm
#' }
alg_ranking <-  function(df)
{

  ## Input Validation

  if(is.null(df)){
    stop("ranking_mc: no data was specified")
  }
  if(!is.data.frame(df)){
    stop("ranking_mc: df has to be a data.frame")
  }
  if(!all(names(df)==c("Main Algorithm","Other Compared Algorithm","Adjusted P-Value","Decision","Discrepancy Supported","Practically Significant Delta"))){
    stop("ranking_mc: df is inappropriate")
  }
  result <- data.frame(matrix(ncol = 5, nrow = 0))

  alg_list <- unique(df[,1])
  for ( i in 1:length(alg_list))
  {
    main_alg <-alg_list[i]
    comparison_df <-df[which(df[,1]==main_alg),]
    score <- NULL
    decision_score <- NULL
    Goal_Difference <- NULL

    # score_sd <- NULL
    for (j in 1:nrow(comparison_df)){
      if(comparison_df[j,4]==c("notReject H0"))
      {
        decision_score <- 0

      score <- c(score,(decision_score))
        Goal_Difference <- c(Goal_Difference,as.integer(comparison_df[j,5])/(comparison_df[j,6]))
      }

      if(comparison_df[j,4]==c("Reject H0") & (comparison_df[j,5] < comparison_df[j,6]))
      {
        decision_score <- 1

      score <- c(score,(decision_score))
      Goal_Difference <- c(Goal_Difference,as.integer(comparison_df[j,5])/(comparison_df[j,6]))
      }

      if(comparison_df[j,4]==c("Reject H0") & (comparison_df[j,5] >= comparison_df[j,6]))
      {
        decision_score <- 3

        score <- c(score,(decision_score))
        Goal_Difference <- c(Goal_Difference,as.integer(comparison_df[j,5])/(comparison_df[j,6]))
      }
    }
    result <- rbind(result,list(main_alg,sum(score),sum(Goal_Difference)))
  }
  names(result) <- c("Algorithm", "Score","Goal_Difference")
  result <- result[order(-result$Score,-result$Goal_Difference),]
  # Format score
  #
  result$Score <-  as.numeric(formatC(result$Score,
                                      format = 'f', digits = 2))
  # # Normalize the score
  # normalize <- function(x) {
  #   return ((x - min(x)) / (max(x) - min(x)))
  # }
  # result$Score <- normalize(result$Score)

  Ranking <-rank(-result$Score,ties.method = "min")
  #Adjusted_Ranking <-rank(-result$Score,-result$Goal_Difference,na.last = "keep")
  #Adjusted_Ranking <- Adjusted_Ranking[complete.cases(Adjusted_Ranking)]
  combined_result <- cbind(result,Ranking)

  #print(combined_result)
 # custom_palette <-colorRampPalette(brewer.pal(9, "Set1"))(nrow(result))
 # print(ggplot(data=combined_result, aes(x=(reorder(Algorithm, Score)), y=Score,fill=Algorithm)) +geom_bar(stat="identity")+coord_flip()+labs(x ="Algorithm")+labs(y="Score")+ scale_fill_manual(values=custom_palette)+ggtitle("Statistical test based ranking")+theme(axis.title=element_text(size=18))+theme(title = element_text( size=18))+theme(text = element_text(size=18))+theme(plot.title = element_text(hjust = 0.5)))+theme(legend.title=element_blank())
  return(combined_result)


}
