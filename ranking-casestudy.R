
# Wrapper function to call the alg_mcompare and alg_ranking functions that perform multiple algorithm comparison and ranking.
ranking_wrapperfn <-  function(data)
{
  result <- data.frame(matrix(ncol = 6, nrow = 0))

  functions <- unique(data$funcId)
  dimensions <- unique(data$DIM)
  algorithms <- unique(data$ID)
  for(i in 1:length(dimensions))
  {
    dimension <- dimensions[i]
    for(k in 1:length(functions))
    {
      current_function <-functions[k]

      sub_data <-subset(data,DIM== dimension & funcId == current_function,select =-c(DIM,funcId,target))


      restructured_data <- sub_data %>%
        gather(variable, value, -ID) %>%
        spread(ID, value)
      restructured_data <- data.frame(restructured_data[,c(-1)])
      sub_result <- alg_mcompare(restructured_data,bootstrapping = TRUE,deltaPS = deltaPS,severityRequirement = severityRequirement)
      sub_rank <- alg_ranking_mcttest(sub_result)
      sub_rank <- sub_rank[,-4]
      sub_rank$Function <-current_function
      result <- rbind(result,sub_rank)

    }
  }
  return(result)
}

library(tidyr)
library(tidyverse)
delta_values <- c(50,100,250,500)
sev_values <- c(0.5,0.65,0.8,0.95)
data <- read.csv("Data.csv")

for (j in 1:length(delta_values)){
  deltaPS=delta_values[j]

  for (kk in 1:length(sev_values)){
    severityRequirement=sev_values[kk]

result <- ranking_wrapperfn(data,deltaPS,severityRequirement)

summarized_result <- result %>%
  group_by(Algorithm) %>%
  summarise(across(c(Score, Goal_Difference), sum))


mean_result <- result %>%
  group_by(Algorithm) %>%
  summarise(across(c(Score), mean))

colnames(mean_result)[colnames(mean_result) == "Score"] <- "Score Mean"

median_result <- result %>%
  group_by(Algorithm) %>%
  summarise(across(c(Score), median))

colnames(median_result)[colnames(median_result) == "Score"] <- "Score Median"


sd_result <- result %>%
  group_by(Algorithm) %>%
  summarise(across(c(Score), sd))

colnames(sd_result)[colnames(sd_result) == "Score"] <- "Score SD"

final_result <- merge(merge(merge( summarized_result, mean_result, by = "Algorithm", all = TRUE), median_result, by = "Algorithm", all = TRUE),sd_result,by = "Algorithm", all = TRUE)
final_result <- final_result[order(-final_result$Score),]
# Format score
#
final_result$Score <-  as.numeric(formatC(final_result$Score,
                                          format = 'f', digits = 2))

final_result$`Score SD` <-  as.numeric(formatC(final_result$`Score SD` ,
                                               format = 'f', digits = 2))

final_result$`Score Mean` <-  as.numeric(formatC(final_result$`Score Mean` ,
                                                 format = 'f', digits = 2))

final_result$`Score Median` <-  as.numeric(formatC(final_result$`Score Median` ,
                                                   format = 'f', digits = 2))

final_result$Goal_Difference <-  floor(final_result$Goal_Difference)


Ranking <-rank(-final_result$Score,ties.method = "min")
ranking_result <- cbind(final_result,Ranking)

}}

