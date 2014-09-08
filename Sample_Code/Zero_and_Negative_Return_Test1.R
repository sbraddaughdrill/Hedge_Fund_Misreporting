
x_flag_cols <- c("flag_pos","flag_zero","flag_neg")

x <- rnorm(1000, mean=1, sd=.5)
x_df <- data.frame(ret_per=x,
                   ret_zeroed=NA,
                   ret_dec=NA, 
                   matrix(NA, ncol=length(x_flag_cols), nrow=length(x), dimnames=list(c(), x_flag_cols)),
                   stringsAsFactors=FALSE)








###### ADJUST NEGATIVE RETURNS ######

x_sort <- sort(x)
x_neg <- head(x_sort,5)

x_df[,"ret_zeroed"] <- ifelse(x_df[,"ret_per"] %in% x_neg,0,x_df[,"ret_per"])

#####################################

x_df[,"ret_dec"] <- x_df[,"ret_zeroed"]/100

x_df[,"flag_pos"] <- ifelse(x_df[,"ret_dec"]>0,1,0)
x_df[,"flag_zero"] <- ifelse(x_df[,"ret_dec"]==0,1,0)
x_df[,"flag_neg"] <- ifelse(x_df[,"ret_dec"]<0,1,0)


x_sum_cols <- c("sum_pos","sum_zero","sum_neg")
x_prob_ind_cols <- c("prob_ind_pos","prob_ind_zero","prob_ind_neg")
x_prob_cum_cols <- c("prob_cum_pos","prob_cum_zero","prob_cum_neg")
x_num_cols <- c("num_pos","num_zero","num_neg")

x_freq <- data.frame(temp_id=1,sum_total=NA,
                     matrix(NA, ncol=length(c(x_sum_cols,x_prob_ind_cols,x_prob_cum_cols,x_num_cols)), nrow=1,
                            dimnames=list(c(), c(x_sum_cols,x_prob_ind_cols,x_prob_cum_cols,x_num_cols))),
                     stringsAsFactors=FALSE)


x_freq[1,x_sum_cols] <- colSums(x_df[,x_flag_cols])
x_freq[1,c("sum_total")]  <- nrow(x_df)


### Find Overall Probabilities

x_type <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("type","lower_tail"))), stringsAsFactors=FALSE)
x_type[1,] <- c("sum_pos",FALSE)
x_type[2,] <- c("sum_zero",FALSE)
x_type[3,] <- c("sum_neg",TRUE)

x_ret_overall_trim <- x
x_freq_overall_trim <- x_freq[1,]

total_overall <- x_freq_overall_trim[,"sum_total"]

total_overall_prob <- data.frame(matrix(NA, ncol=4*nrow(x_type), nrow=1, 
                                        dimnames=list(c(), c(paste(x_type[,1],"prob_pop",sep="_"),
                                                             paste(x_type[,1],"prob_pop_binomial",sep="_"),
                                                             paste(x_type[,1],"prob_n",sep="_"),
                                                             paste(x_type[,1],"prob_n_binomial",sep="_")))), stringsAsFactors=FALSE)

mean_overall <- mean(x_ret_overall_trim, na.rm = FALSE)
sd_overall <- sd(x_ret_overall_trim, na.rm = FALSE)

total_overall_prob[,"sum_pos_prob_n"] <- pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)

total_overall_prob[,"sum_zero_prob_n"] <- pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)-pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)
#total_overall_prob[,"sum_zero_prob_n"] <- pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)-pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)

total_overall_prob[,"sum_neg_prob_n"] <- pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)

for (i in 1:nrow(x_type))
{
  # i <- 1
  # i <- 2
  
  lower.tail_flag <- as.logical(x_type[i,2])
  
  cutoff_overall <- x_freq_overall_trim[,x_type[i,1]]
  
  prob_pop_col <- paste(x_type[i,1],"prob_pop",sep="_")
  prob_pop_binomial_col <- paste(x_type[i,1],"prob_pop_binomial",sep="_")
  prob_n_col <- paste(x_type[i,1],"prob_n",sep="_")
  prob_n_binomial <- paste(x_type[i,1],"prob_n_binomial",sep="_")
  
  total_overall_prob[,prob_pop_col] <- cutoff_overall/total_overall
  total_overall_prob[,prob_pop_binomial_col] <- pbinom(q=cutoff_overall,size=total_overall, prob=total_overall_prob[,prob_pop_col],
                                                       lower.tail=lower.tail_flag, log.p = FALSE)
  
  total_overall_prob[,prob_n_binomial] <- pbinom(q=cutoff_overall, size=total_overall, prob=total_overall_prob[,prob_n_col],
                                                 lower.tail=lower.tail_flag, log.p = FALSE)
  
  rm(lower.tail_flag,cutoff_overall)
  rm(prob_pop_col,prob_pop_binomial_col,prob_n_col,prob_n_binomial)
  
}
rm(i)

x_freq[1,c(x_prob_ind_cols,x_prob_cum_cols)] <- total_overall_prob[,c(paste(x_type[,1],"prob_n",sep="_"),paste(x_type[,1],"prob_n_binomial",sep="_"))]
