
########################################################################################################
# XXXX
########################################################################################################

generate_data <-  function(x,command_col){
  
  #cat(x[,"draw_id"], "\n")
  
  num_obs <-x[,"num_obs"]
  location <- x[,"location"]
  scale <- x[,"scale"]
  skewness <- x[,"skewness"]
  df <- x[,"df"]
  command <- x[,command_col]
  
  ret_temp <- data.frame(overall_id=NA,sim_id=NA,sim_ob_id=NA,calc_ret=as.numeric(eval(parse(text = paste0(command)))),
                         calc_ret_lag=NA,other_ret=NA,other_ret_lag=NA,stringsAsFactors=FALSE)
  
  ret_temp[,"calc_ret_lag"] <- c(NA,ret_temp[1:(nrow(ret_temp)-1),"calc_ret"])  
  
  return(ret_temp[2:nrow(ret_temp),])
}


########################################################################################################
# XXXX
########################################################################################################

#Trigger if fails at any point in time series
misreport_cumm_any <- function(data,id_col,pct_cols,suffix){
  
  require(data.table)
  require(plyr)
  
  # data <- data_s5_1_full[,c(identifier,zero_and_neg_percentiles)]
  # id_col <- identifier
  # pct_cols <- zero_and_neg_percentiles
  # suffix <- "any"
  
  data_any0 <- ddply(.data=data[,c(id_col,pct_cols)],.variables=id_col,.fun=function(x,pct_cols){
    for (i in which(colnames(x) %in% pct_cols)){ x[[i]] <- sum(x[[i]],na.rm=T) }
    return(unique(x))
  },pct_cols=pct_cols)
  data_any <- data_any0
  for (i in which(!(colnames(data_any) %in% id_col)))
  {
    data_any[[i]] <- ifelse(is.na(data_any[[i]]),NA,ifelse(data_any[[i]]>0,1,0))
  }
  setnames(data_any,old=pct_cols,new=paste(pct_cols,suffix,sep="_"))
  return(data_any)
}


########################################################################################################
# XXXX
########################################################################################################

#Trigger if fails more than half
misreport_cumm_avg <- function(data,id_col,pct_cols,suffix){
  
  require(data.table)
  require(plyr)
  
  # data <- data_s5_1_full[,c(identifier,zero_and_neg_percentiles)]
  # id_col <- identifier
  # pct_cols <- zero_and_neg_percentiles
  # suffix <- "avg"
  
  data_avg0 <- ddply(.data=data[,c(id_col,pct_cols)],.variables=id_col,.fun=function(x,pct_cols){
    for (i in which(colnames(x) %in% pct_cols)){ x[[i]] <- mean(x[[i]],na.rm=T) }
    return(unique(x))
  },pct_cols=pct_cols)
  
  data_avg <- data_avg0
  for (i in which(!(colnames(data_avg) %in% id_col)))
  {
    data_avg[[i]] <- ifelse(is.na(data_avg[[i]]),NA,ifelse(data_avg[[i]]>0.5,1,0))
  }
  
  setnames(data_avg,old=pct_cols,new=paste(pct_cols,suffix,sep="_"))
  return(data_avg)
}


########################################################################################################
# XXXX
########################################################################################################

kink_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  data_s1_bins <- kink_screen_bins(data=data,ret_col=ret_col)
  
  data_s1_ratios <- kink_screen_ratios(bins=data_s1_bins)
  
  rm(data_s1_bins)
  
  return(data_s1_ratios)
}


########################################################################################################
# XXXX
########################################################################################################

kink_screen_bins <- function(data,ret_col){
  
  # data <- data
  # ret_col <- ret_col
  
  require(plyr)
  require(stats)
  
  data_s1_ret_min <- min(data[,ret_col],na.rm=T)
  data_s1_ret_max <- max(data[,ret_col],na.rm=T)
  
  data_s1_mean <- mean(data[,ret_col],na.rm=T)
  data_s1_sd <- sd(data[,ret_col],na.rm=T)
  data_s1_iqr <- IQR(data[,ret_col],na.rm=T,type=7)
  data_s1_n <- nrow(data[!is.na(data[,ret_col]),])
  
  ## OLD
  
  #   #data_s1_width <- 0.25
  #   data_s1_width <- 0.0025
  #   #data_s1_width <- 0.00025
  
  #   data_s1_extremes <- round_any(max(abs(data[,ret_col]),na.rm=T),0.005,ceiling)
  #   data_s1_breaks <- seq(-(data_s1_extremes+data_s1_width*2),(data_s1_extremes+data_s1_width*2),data_s1_width)
  #   data_s1_breaks <- sort(unique(round2(data_s1_breaks,digits=5)))
  
  
  ## NEW
  
  data_s1_sigma <- min(data_s1_sd,data_s1_iqr/1.34)
  
  if(data_s1_sigma==0){ data_s1_sigma <- 1/data_s1_n } # Do incase the min is 0
  
  # data_s1_width <- round2(1.05846*data_s1_sigma*data_s1_n^(-0.2),digits=5)
  data_s1_width <- round2(0.776*1.364*data_s1_sigma*data_s1_n^(-0.2),digits=5)
  
  data_s1_extremes <- round_any(max(abs(data[,ret_col]),na.rm=T),0.005,ceiling)
  data_s1_cuts <- round2((data_s1_extremes+data_s1_width*2)/data_s1_width,digits=0)
  data_s1_breaks <- seq(-data_s1_cuts*data_s1_width,data_s1_cuts*data_s1_width,data_s1_width)
  data_s1_breaks <- sort(unique(round2(data_s1_breaks,digits=5)))
  
  
  ## Compute Bins
  
  data_s1_breaks_keep3 <- which(data_s1_breaks==0.0)
  data_s1_breaks_keep2 <- data_s1_breaks_keep3-1
  data_s1_breaks_keep1 <- data_s1_breaks_keep3-2
  
  bins_temp <- hist(data[,ret_col],breaks=data_s1_breaks,right=T,plot=F)
  bins_cols <- c("breaks_start","breaks_end","counts","density","mids","good_bin","prob")
  bins_out <- data.frame(matrix(NA,ncol=length(bins_cols),nrow=length(data_s1_breaks)-1,dimnames=list(c(),bins_cols)),stringsAsFactors=F)
  
  bins_out[,"breaks_start"] <- head(bins_temp[["breaks"]],length(data_s1_breaks)-1)
  bins_out[,"breaks_end"] <-  tail(bins_temp[["breaks"]],length(data_s1_breaks)-1)
  bins_out[,"counts"] <- bins_temp[["counts"]]
  bins_out[,"density"] <- bins_temp[["density"]]
  bins_out[,"mids"] <- bins_temp[["mids"]]
  bins_out[,"good_bin"] <- ifelse(bins_out[,"breaks_start"] %in% data_s1_breaks[c(data_s1_breaks_keep1,data_s1_breaks_keep2,data_s1_breaks_keep3)],1,0)
  bins_out[,"prob"] <- bins_temp[["counts"]]/sum(bins_temp[["counts"]],na.rm=T) ## Same as Density * Width
  
  return(bins_out)
}


########################################################################################################
# XXXX
########################################################################################################

kink_screen_ratios <- function(bins){
  
  # bins <- data_s1_bins
  
  require(reshape2)
  
  data_s1_bins_keep <-  data.frame(id=NA,bin_id=NA,bins[bins[,"good_bin"]==1,])
  data_s1_bins_keep[,"id"] <- 99
  
  data_s1_bins_keep <- data_s1_bins_keep[order(data_s1_bins_keep[,"id"],data_s1_bins_keep[,"breaks_start"]),]
  data_s1_bins_keep[,"bin_id"] <- seq(1,nrow(data_s1_bins_keep))
  row.names(data_s1_bins_keep) <- seq(nrow(data_s1_bins_keep))
  
  data_s1_bins_keep[,"bin_id"] <- paste("bin_count",data_s1_bins_keep[,"bin_id"],sep="")
  
  
  
  data_s1_bins_outside_avg <- 0.5*(data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count1","counts"]+data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count3","counts"])
  
  data_s1_bins_kink_ratio <- data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count2","counts"]/data_s1_bins_outside_avg
  data_s1_bins_kink_ratio <- ifelse(data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count2","counts"]==data_s1_bins_outside_avg,1,data_s1_bins_kink_ratio)
  data_s1_bins_kink_ratio <- ifelse(is.infinite(data_s1_bins_kink_ratio),0,data_s1_bins_kink_ratio)
  
  data_s1_bins_diff <- data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count2","counts"]-data_s1_bins_outside_avg
  
  n <- sum(bins[,"counts"])
  p1 <- data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count1","prob"]
  p2 <- data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count2","prob"]
  p3 <- data_s1_bins_keep[data_s1_bins_keep[,"bin_id"]=="bin_count3","prob"]
  data_s1_bins_diff_sd <- (n*(p2-p2^2)+0.25*n*(p1-p1^2+p3-p3^2)+n*p2*(p1+p3)-0.5*n*p1*p3)^0.5
  
  data_s1_bins_t_stat <- data_s1_bins_diff/data_s1_bins_diff_sd
    
  data_s1_bins_discontinuity <- data.frame(dcast(data_s1_bins_keep[,c("id","bin_id","counts")],id~bin_id,value.var=c("counts")),
                                           matrix(NA,ncol=6,nrow=1,dimnames=list(c(),c("n","outside_bin_avg","kink_ratio","diff","diff_sd","t_stat"))),stringsAsFactors=F)
    
  #     data_s1_bins_discontinuity[,"outside_bin_avg"] <- rowMeans(data_s1_bins_discontinuity[,c("bin_count1","bin_count3")],na.rm=T)
  #     data_s1_bins_discontinuity[,"kink_ratio"] <- data_s1_bins_discontinuity[,"bin_count2"]/data_s1_bins_discontinuity[,"outside_bin_avg"]
  #     data_s1_bins_discontinuity[,"kink_ratio"] <- ifelse(data_s1_bins_discontinuity[,"bin_count2"]==data_s1_bins_discontinuity[,"outside_bin_avg"],1,data_s1_bins_discontinuity[,"kink_ratio"])
  #     data_s1_bins_discontinuity[,"kink_ratio"] <- ifelse(is.infinite(data_s1_bins_discontinuity[,"kink_ratio"]),0,data_s1_bins_discontinuity[,"kink_ratio"])
  
  data_s1_bins_discontinuity[,"n"] <- n
  data_s1_bins_discontinuity[,"outside_bin_avg"] <- data_s1_bins_outside_avg
  data_s1_bins_discontinuity[,"kink_ratio"] <- data_s1_bins_kink_ratio
  data_s1_bins_discontinuity[,"diff"] <- data_s1_bins_diff
  data_s1_bins_discontinuity[,"diff_sd"] <- data_s1_bins_diff_sd
  data_s1_bins_discontinuity[,"t_stat"] <- data_s1_bins_t_stat

  return(data_s1_bins_discontinuity[,!(colnames(data_s1_bins_discontinuity) %in% "id")])
}


########################################################################################################
# XXXX
########################################################################################################

indexrsq_screen_execute <- function(data_style,id,ret_col,id_col,date_col){
  
  # data_style <- data_style
  # id <- x
  # ret_col <- analysis_col
  
  data_s2_3_reg_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  indexrsq_screen_rets <- indexrsq_screen_returns(data_style=data_style,id=id,ret_col=ret_col,id_col=id_col,date_col=date_col)
  indexrsq_screen_temp <- indexrsq_screen_model(data_ret=indexrsq_screen_rets,fund_ret_col=ret_col,index_ret_col=paste("avg",ret_col,sep="_"),model_cols=data_s2_3_reg_cols)
  
  if (nrow(indexrsq_screen_temp) == 0) {
    indexrsq_screen_temp <-  as.data.frame(matrix(NA,ncol=ncol(indexrsq_screen_temp),nrow=1,dimnames=list(c(),colnames(indexrsq_screen_temp))),stringsAsFactors=FALSE)
  }
  return(indexrsq_screen_temp)
}


########################################################################################################
# XXXX
########################################################################################################

indexrsq_screen_returns <- function(data_style,id,ret_col,id_col,date_col){
  
  # data_style <- data_style
  # id <- id
  
  id_date_range <- data_style[data_style[,id_col]==id,date_col]
  
  data_style_good_dates <- data_style[data_style[,date_col] %in% id_date_range,]
  
  data_style_good_dates_id <- data_style_good_dates[data_style_good_dates[,id_col] %in% id,]
  
  data_style_good_dates_noid <- data_style_good_dates[!(data_style_good_dates[,id_col] %in% id),]
  
  #rm(data_style_good_dates)
  
  data_style_good_dates_noid_avg <- ddply(.data=data_style_good_dates_noid, .variables=date_col, .fun = function(w,col){
    
    mean(w[,col], trim = 0, na.rm = TRUE)
  },col=ret_col)
  #rm(data_style_good_dates_noid)
  
  colnames(data_style_good_dates_noid_avg) <- c(date_col,paste("avg",ret_col,sep="_"))
  
  data_style_good_rets <- data.frame(temp_date=id_date_range,temp_y=NA,temp_x=NA,stringsAsFactors=FALSE)
  colnames(data_style_good_rets)[match("temp_date",names(data_style_good_rets))] <- date_col
  colnames(data_style_good_rets)[match("temp_y",names(data_style_good_rets))] <- ret_col
  colnames(data_style_good_rets)[match("temp_x",names(data_style_good_rets))] <- paste("avg",ret_col,sep="_")
  
  #rm(id_date_range)
  
  for(i in 1:nrow(data_style_good_dates_id)){ 
    data_style_good_rets[data_style_good_rets[,date_col]==data_style_good_dates_id[i,date_col],ret_col] <- data_style_good_dates_id[i,ret_col] 
  }
  #rm(i,data_style_good_dates_id)
  
  for(i in 1:nrow(data_style_good_dates_noid_avg)){ 
    data_style_good_rets[data_style_good_rets[,date_col]==data_style_good_dates_noid_avg[i,date_col],paste("avg",ret_col,sep="_")] <- data_style_good_dates_noid_avg[i,paste("avg",ret_col,sep="_")] 
  }
  #rm(i,data_style_good_dates_noid_avg)
  
  return(data_style_good_rets)
}


########################################################################################################
# XXXX
########################################################################################################

indexrsq_screen_model <- function(data_ret,fund_ret_col,index_ret_col,model_cols){
  
  # data_ret <- indexrsq_screen_rets
  # fund_ret_col <- ret_col
  # index_ret_col <- paste("avg",ret_col,sep="_")
  # model_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  colnames(data_ret)[match(fund_ret_col,names(data_ret))] <- "y"
  colnames(data_ret)[match(index_ret_col,names(data_ret))] <- "x1"
  
  data_s2_3_group_coef <- data.frame(var=NA,coefficients(suppressWarnings(summary(lm(y ~ x1, data=data_ret)))), stringsAsFactors=FALSE)
  colnames(data_s2_3_group_coef) <- c("var",model_cols)
  
  data_s2_3_group_coef[,"var"] <- row.names(data_s2_3_group_coef)
  row.names(data_s2_3_group_coef) <- seq(nrow(data_s2_3_group_coef))
  
  return(data_s2_3_group_coef[data_s2_3_group_coef[,"var"]=="x1",model_cols])
}


########################################################################################################
# XXXX
########################################################################################################

ar_screen_execute <- function(data,ret_col,lag_ret_col){
  
  # data <- w
  # ret_col <- analysis_col
  # lag_ret_col <- data_trim_cols_lagged_trim
  
  data_s3_1_reg_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  ar_screen_temp <- ar_screen_ar_model(data=data,ret_col=ret_col,lag_ret_col=lag_ret_col,model_cols=data_s3_1_reg_cols)
  return(ar_screen_temp)
}


########################################################################################################
# XXXX
########################################################################################################

ar_screen_ar_model <- function(data,ret_col,lag_ret_col,model_cols){
  
  # data <- data
  # ret_col <- ret_col
  # lag_ret_col <- lag_ret_col
  # model_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  colnames(data)[match(ret_col,names(data))] <- "y"
  colnames(data)[match(lag_ret_col,names(data))] <- "x1"
  
  data_s3_1_group_coef <- data.frame(var=NA,coefficients(suppressWarnings(summary(lm(y ~ x1, data=data)))), stringsAsFactors=FALSE)
  colnames(data_s3_1_group_coef) <- c("var",model_cols)
  
  data_s3_1_group_coef[,"var"] <- row.names(data_s3_1_group_coef)
  row.names(data_s3_1_group_coef) <- seq(nrow(data_s3_1_group_coef))
  
  #return(data_s3_1_group_coef[data_s3_1_group_coef[,"var"]=="x1",model_cols])
  return(data_s3_1_group_coef)
}


########################################################################################################
# XXXX
########################################################################################################

zero_neg_screen_execute <- function(data,ret_col,prob_type){
  
  # data <- x
  # ret_col <- analysis_col
  # prob_type <- "normal"
  
  data_trim <- data[!is.na(data[,ret_col]),]
  # data_trim <- data
  
  data_s5_1_sum_cols <- c("sum_pos","sum_zero","sum_neg")
  data_s5_1_prob_ind_cols <- c("prob_ind_pos","prob_ind_zero","prob_ind_neg")
  data_s5_1_prob_cum_cols <- c("prob_cum_pos","prob_cum_zero","prob_cum_neg")
  
  data_s5_1_freq <- data.frame(sum_total=NA,matrix(NA,ncol=length(c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)),nrow=1,
                                                   dimnames=list(c(),c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols))),stringsAsFactors=FALSE)
  
  data_s5_temp <- zero_neg_screen_ret_flags(z=data_trim[,analysis_col],analysis_col=analysis_col,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_freq[,c("sum_total",data_s5_1_sum_cols)] <- zero_neg_screen_totals(y=data_s5_temp,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_type <- data.frame(matrix(NA,ncol=2,nrow=3,dimnames=list(c(),c("type","lower_tail"))),stringsAsFactors=FALSE)
  data_s5_1_type[1,] <- c("sum_pos",FALSE)
  data_s5_1_type[2,] <- c("sum_zero",FALSE)
  data_s5_1_type[3,] <- c("sum_neg",TRUE)
  
  data_s5_1_prob_ind_cum_cols <- c(paste(data_s5_1_type[,1],"ind_prop",sep="_"),paste(data_s5_1_type[,1],"cum_prop",sep="_"))
  
  data_s5_1_prob <- data.frame(prob_type=NA,total=NA, matrix(NA,ncol=2*nrow(data_s5_1_type),nrow=2,dimnames=list(c(),data_s5_1_prob_ind_cum_cols)),
                               stringsAsFactors=FALSE)
  data_s5_1_prob[,"prob_type"] <- c("population","normal")
  
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="population",] <- zero_neg_screen_pop_prob(data_ret=data_s5_temp,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="normal",] <- zero_neg_screen_norm_prob(data_ret=data_s5_temp,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  
  data_s5_1_freq[,c(data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)] <- data_s5_1_prob[data_s5_1_prob[,"prob_type"]==prob_type,data_s5_1_prob_ind_cum_cols]
  
  rm(data_s5_1_prob,data_s5_1_type,data_s5_temp)
  rm(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_prob_ind_cum_cols)
  
  return(data_s5_1_freq)
}


########################################################################################################
# XXXX
########################################################################################################

zero_neg_screen_ret_flags <- function(z,analysis_col,flags){
  
  # z <- data[,analysis_col]
  # analysis_col <- analysis_col
  # flags <- c("flag_pos","flag_zero","flag_neg")
  
  z_out <- data.frame(temp_ret=z, matrix(NA, ncol=length(flags), nrow=length(z), dimnames=list(c(), flags)),stringsAsFactors=FALSE)
  colnames(z_out)[match("temp_ret",names(z_out))] <- analysis_col
  
  z_out[,flags[grep("pos",flags)]] <- ifelse(z_out[,analysis_col] > 0.00005,1,0)
  z_out[,flags[grep("zero",flags)]] <- ifelse(z_out[,analysis_col] <= 0.00005 & z_out[,analysis_col] >= -0.00005,1,0)
  z_out[,flags[grep("neg",flags)]] <- ifelse(z_out[,analysis_col] < -0.00005,1,0)
  
  return(z_out)
}


########################################################################################################
# XXXX
########################################################################################################

zero_neg_screen_totals <- function(y,flags){
  
  # y <- data_s5
  # flags <- c("flag_pos","flag_zero","flag_neg")
  
  totals_out <- c(nrow(y),colSums(y[,flags],na.rm=TRUE))
}


########################################################################################################
# XXXX
########################################################################################################

zero_neg_screen_pop_prob <- function(data_ret,data_freq,type,analysis_col){
  
  # data_ret <- data_s5
  # data_freq <- data_s5_1_freq
  # type <- data_s5_1_type
  # analysis_col <- analysis_col
  
  prob_type <- "population"
  
  total_group <- data.frame(prob_type=NA,total=NA,
                            matrix(NA, ncol=2*nrow(type), nrow=1, 
                                   dimnames=list(c(), c(paste(type[,1],"ind_prop",sep="_"),paste(type[,1],"cum_prop",sep="_")))), 
                            stringsAsFactors=FALSE)
  
  total_group[,"prob_type"] <- prob_type
  #total_group[,"total"] <- data_freq[,"sum_total"]
  total_group[,"total"] <- nrow(data_ret)
  
  total_group[total_group[,"prob_type"]==prob_type,"sum_pos_ind_prop"] <- data_freq[,"sum_pos"]/nrow(data_ret)
  total_group[total_group[,"prob_type"]==prob_type,"sum_zero_ind_prop"] <- data_freq[,"sum_zero"]/nrow(data_ret)
  total_group[total_group[,"prob_type"]==prob_type,"sum_neg_ind_prop"] <- data_freq[,"sum_neg"]/nrow(data_ret)
  
  #  # P(X <= 48) - P(X <= 47)  => P(X > 47) - P(X > 48) => P(X=48)
  #  pbinom(q=48,size=100,prob=0.50,lower.tail=T,log.p=F)-pbinom(q=47,size=100,prob=0.50,lower.tail=T,log.p=F)
  #  pbinom(q=47,size=100,prob=0.50,lower.tail=F,log.p=F)-pbinom(q=48,size=100,prob=0.50,lower.tail=F,log.p=F)
  #  dbinom(x=48,size=100,prob=0.50,log=F)
  #     
  #  # 1 - P(X <= 47) => P(X=48) + P(X > 48) => P(X >= 48)
  #  1-pbinom(q=47,size=100,prob=0.50,lower.tail=T,log.p=F)
  #  dbinom(x=48,size=100,prob=0.50,log=F) + pbinom(q=48,size=100,prob=0.50,lower.tail=F,log.p=F)
  
  # Positive: P(X=x) + P(X>x) (i.e., prob of seeing this many positive returns or more)
  total_group[,"sum_pos_cum_prop"] <- 0
  total_group[,"sum_pos_cum_prop"] <- total_group[,"sum_pos_cum_prop"] + dbinom(x=data_freq[,"sum_pos"],size=total_group[,"total"],prob=total_group[,paste("sum_pos","ind_prop",sep="_")])
  total_group[,"sum_pos_cum_prop"] <- total_group[,"sum_pos_cum_prop"] + pbinom(q=data_freq[,"sum_pos"],size=total_group[,"total"],prob=total_group[,paste("sum_pos","ind_prop",sep="_")],lower.tail=F)
  
  # Zero:     P(X=x) + P(X>x) (i.e., prob of seeing this many zero returns or more)
  total_group[,"sum_zero_cum_prop"] <- 0
  total_group[,"sum_zero_cum_prop"] <- total_group[,"sum_zero_cum_prop"] + dbinom(x=data_freq[,"sum_zero"],size=total_group[,"total"],prob=total_group[,paste("sum_zero","ind_prop",sep="_")])
  total_group[,"sum_zero_cum_prop"] <- total_group[,"sum_zero_cum_prop"] + pbinom(q=data_freq[,"sum_zero"],size=total_group[,"total"],prob=total_group[,paste("sum_zero","ind_prop",sep="_")],lower.tail=F)
  
  # Negative: P(X<=x) or P(X=x) + P(X<=x-1) (i.e., prob of seeing this many negative returns or less)
  #total_group[,"sum_neg_cum_prop"] <- pbinom(q=data_freq[,"sum_neg"],size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")],lower.tail=T)
  total_group[,"sum_neg_cum_prop"] <- 0
  total_group[,"sum_neg_cum_prop"] <- total_group[,"sum_neg_cum_prop"] + dbinom(x=data_freq[,"sum_neg"],size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")])
  total_group[,"sum_neg_cum_prop"] <- total_group[,"sum_neg_cum_prop"] + pbinom(q=data_freq[,"sum_neg"]-1,size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")],lower.tail=T)

  return(total_group)
}

########################################################################################################
# XXXX
########################################################################################################

zero_neg_screen_norm_prob <- function(data_ret,data_freq,type,analysis_col){
  
  # data_ret <- data_s5_temp
  # data_freq <- data_s5_1_freq
  # type <- data_s5_1_type
  # analysis_col <- analysis_col
  
  prob_type <- "normal"
  
  total_group <- data.frame(prob_type=NA,total=NA,
                            matrix(NA, ncol=2*nrow(type), nrow=1, 
                                   dimnames=list(c(), c(paste(type[,1],"ind_prop",sep="_"),paste(type[,1],"cum_prop",sep="_")))), 
                            stringsAsFactors=F)
  
  total_group[,"prob_type"] <- prob_type
  total_group[,"total"] <- nrow(data_ret)
  
  mean_group <- mean(data_ret[,analysis_col], na.rm=T)
  sd_group <- sd(data_ret[,analysis_col], na.rm=T)

  total_group[total_group[,"prob_type"]==prob_type,"sum_pos_ind_prop"] <- pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=F)
  total_group[total_group[,"prob_type"]==prob_type,"sum_zero_ind_prop"] <- pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=T)-pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=T)
  #total_group[total_group[,"prob_type"]==prob_type,"sum_zero_ind_prop"] <- pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=F)-pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=F)
  total_group[total_group[,"prob_type"]==prob_type,"sum_neg_ind_prop"] <- pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=T)
  
  #  # P(X <= 48) - P(X <= 47)  => P(X > 47) - P(X > 48) => P(X=48)
  #  pbinom(q=48,size=100,prob=0.50,lower.tail=T,log.p=F)-pbinom(q=47,size=100,prob=0.50,lower.tail=T,log.p=F)
  #  pbinom(q=47,size=100,prob=0.50,lower.tail=F,log.p=F)-pbinom(q=48,size=100,prob=0.50,lower.tail=F,log.p=F)
  #  dbinom(x=48,size=100,prob=0.50,log=F)
  #     
  #  # 1 - P(X <= 47) => P(X=48) + P(X > 48) => P(X >= 48)
  #  1-pbinom(q=47,size=100,prob=0.50,lower.tail=T,log.p=F)
  #  dbinom(x=48,size=100,prob=0.50,log=F) + pbinom(q=48,size=100,prob=0.50,lower.tail=F,log.p=F)
  
  # Positive: P(X=x) + P(X>x) (i.e., prob of seeing this many positive returns or more)
  total_group[,"sum_pos_cum_prop"] <- 0
  total_group[,"sum_pos_cum_prop"] <- total_group[,"sum_pos_cum_prop"] + dbinom(x=data_freq[,"sum_pos"],size=total_group[,"total"],prob=total_group[,paste("sum_pos","ind_prop",sep="_")])
  total_group[,"sum_pos_cum_prop"] <- total_group[,"sum_pos_cum_prop"] + pbinom(q=data_freq[,"sum_pos"],size=total_group[,"total"],prob=total_group[,paste("sum_pos","ind_prop",sep="_")],lower.tail=F)
  
  # Zero:     P(X=x) + P(X>x) (i.e., prob of seeing this many zero returns or more)
  total_group[,"sum_zero_cum_prop"] <- 0
  total_group[,"sum_zero_cum_prop"] <- total_group[,"sum_zero_cum_prop"] + dbinom(x=data_freq[,"sum_zero"],size=total_group[,"total"],prob=total_group[,paste("sum_zero","ind_prop",sep="_")])
  total_group[,"sum_zero_cum_prop"] <- total_group[,"sum_zero_cum_prop"] + pbinom(q=data_freq[,"sum_zero"],size=total_group[,"total"],prob=total_group[,paste("sum_zero","ind_prop",sep="_")],lower.tail=F)
  
  # Negative: P(X<=x) or P(X=x) + P(X<=x-1) (i.e., prob of seeing this many negative returns or less)
  #total_group[,"sum_neg_cum_prop"] <- pbinom(q=data_freq[,"sum_neg"],size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")],lower.tail=T)
  total_group[,"sum_neg_cum_prop"] <- 0
  total_group[,"sum_neg_cum_prop"] <- total_group[,"sum_neg_cum_prop"] + dbinom(x=data_freq[,"sum_neg"],size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")])
  total_group[,"sum_neg_cum_prop"] <- total_group[,"sum_neg_cum_prop"] + pbinom(q=data_freq[,"sum_neg"]-1,size=total_group[,"total"],prob=total_group[,paste("sum_neg","ind_prop",sep="_")],lower.tail=T)
  
  return(total_group)
}


########################################################################################################
# XXXX
########################################################################################################

per_repeat_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  data_trim <- data[!is.na(data[,ret_col]),]
  # data_trim <- data
  
  repeat_num_data_count <- per_repeat_screen_counts(ret=data_trim[,ret_col],ret_col=ret_col)
  
  repeat_num_data_sum <- per_repeat_screen_sum(data=repeat_num_data_count,data_col="freq")
  
  rm(repeat_num_data_count)
  
  return(repeat_num_data_sum)
}


########################################################################################################
# XXXX
########################################################################################################

per_repeat_screen_counts <- function(ret,ret_col){
  
  require(plyr)
  
  # ret <- data2[,ret_col]
  # ret_col <- ret_col
  
  repeat_num_data_trim <- ret[!is.na(ret)]
  repeat_num_data_trim <- as.data.frame(repeat_num_data_trim,stringsAsFactors=FALSE)
  row.names(repeat_num_data_trim) <- seq(nrow(repeat_num_data_trim))
  colnames(repeat_num_data_trim) <- ret_col
  
  #repeat_num_data_count <- ddply(.data=repeat_num_data_trim, .variables=c(ret_col), .fun = function(x){unique(data.frame(x,freq=nrow(x),stringsAsFactors=FALSE))})
  repeat_num_data_count <- count(repeat_num_data_trim,ret_col)
  
  repeat_num_data_count <- repeat_num_data_count[order(repeat_num_data_count[,"freq"]),]
  row.names(repeat_num_data_count) <- seq(nrow(repeat_num_data_count))
  
  return(repeat_num_data_count)
}


########################################################################################################
# XXXX
########################################################################################################

per_repeat_screen_sum <- function(data,data_col){
  
  # data <- repeat_num_data_count
  # data_col <- "freq"
  
  sum_temp <- data.frame(Count_1s=NA,Count_u=NA,Total=NA,Prop_1s=NA,Prop_u=NA,
                         Prop_1s_one_minus=NA,Prop_u_one_minus=NA,stringsAsFactors=FALSE)
  sum_temp[,"Count_1s"] <- length(which(data[,data_col]==1))
  sum_temp[,"Count_u"] <- nrow(data)
  sum_temp[,"Total"] <- sum(data[,data_col])
  sum_temp[,"Prop_1s"] <- sum_temp[,"Count_1s"]/sum_temp[,"Total"] 
  sum_temp[,"Prop_u"] <- sum_temp[,"Count_u"]/sum_temp[,"Total"] 
  sum_temp[,"Prop_1s_u"] <- sum_temp[,"Count_1s"]/sum_temp[,"Count_u"]
  sum_temp[,"Prop_1s_one_minus"] <- 1 - sum_temp[,"Prop_1s"]
  sum_temp[,"Prop_u_one_minus"] <- 1 - sum_temp[,"Prop_u"]
  sum_temp[,"Prop_1s_u_one_minus"] <- 1 - sum_temp[,"Prop_1s_u"]
  
  return(sum_temp)
}

########################################################################################################
# XXXX
########################################################################################################

string_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  data_trim <- data[!is.na(data[,ret_col]),]
  # data_trim <- data
  
  string_screen_counts <- string_screen_rle(data=data_trim,ret_col=ret_col)
  
  string_screen_temp <- data.frame(Max_Length=NA,Total=NA,Prop_u=NA,stringsAsFactors=FALSE)
  
  string_screen_temp[,"Max_Length"] <- tail(string_screen_counts,1)
  string_screen_temp[,"Total"] <- nrow(data_trim)
  string_screen_temp[,"Prop_u"] <- string_screen_temp[,"Max_Length"]/string_screen_temp[,"Total"] 
  
  return(string_screen_temp)
}


########################################################################################################
# XXXX
########################################################################################################

string_screen_rle <- function(data,ret_col){
  
  # data <- data
  # ret_col <- ret_col
  
  rle_temp <- data.frame(do.call(cbind,rle(data[,ret_col])),stringsAsFactors=FALSE)
  rle_temp <- rle_temp[,c("values","lengths")]

  return(data.frame(lengths=sort(unique(rle_temp[,c("lengths")])),stringsAsFactors=FALSE))
}

########################################################################################################
# XXXX
########################################################################################################

num_pairs_screen_execute <- function(data,ret_col,lag_ret_col,both_ways){
  
  # data <- x
  # ret_col <- analysis_col
  # lag_ret_col <- paste(analysis_col,"lag1",sep="_")
  # both_ways <- TRUE
  
  num_pair_data <- num_pairs_screen_pairs(data=data,ret_col=ret_col,lag_ret_col=lag_ret_col)

  if (both_ways) {
    pair_col <- "Pair_ID"
  } else {
    pair_col <- "Pair_Reg"
  }
  
  num_pairs_counts <- count(num_pair_data[num_pair_data[,"Repeat"]==0,],pair_col)
  num_pairs_counts <- num_pairs_counts[order(num_pairs_counts[,"freq"]),]

  num_pairs_sum <- data.frame(Max_Pairs=NA,Max_Pairs_Adj=NA,Total=NA,Prop=NA,Prop_Adj=NA,stringsAsFactors=FALSE)
  
  num_pairs_sum[,"Max_Pairs"] <- tail(num_pairs_counts,1)[,"freq"]
  num_pairs_sum[,"Max_Pairs_Adj"] <- num_pairs_sum[,"Max_Pairs"]-1
  num_pairs_sum[,"Total"] <- nrow(num_pairs_counts)
  num_pairs_sum[,"Prop"] <- num_pairs_sum[,"Max_Pairs"]/num_pairs_sum[,"Total"] 
  num_pairs_sum[,"Prop_Adj"] <- num_pairs_sum[,"Max_Pairs_Adj"]/num_pairs_sum[,"Total"] 
  
  return(num_pairs_sum)
}


########################################################################################################
# XXXX
########################################################################################################

# num_pairs_screen_pairs <- function(data,ret_col,lag_ret_col){
#   
#   # data <- data
#   # ret_col <- ret_col
#   # lag_ret_col <- lag_ret_col
#   
#   num_pair_data <- data.frame(data[,c(ret_col,lag_ret_col)],Repeat=NA,Pair=NA,stringsAsFactors=FALSE)
#   
#   num_pair_data_trim <- num_pair_data[!(is.na(num_pair_data[,ret_col]) | is.na(num_pair_data[,lag_ret_col])),]
#   row.names(num_pair_data_trim) <- seq(nrow(num_pair_data_trim))
#   
#   rm(num_pair_data)
#   
#   num_pair_data_trim[,"Repeat"] <- ifelse(num_pair_data_trim[,ret_col]==num_pair_data_trim[,lag_ret_col],1,0)
#   
#   num_pair_data_trim[,"Pair"] <- paste(formatC(num_pair_data_trim[,ret_col], digits = 4, format = "f", flag = "0"),
#                                        formatC(num_pair_data_trim[,lag_ret_col], digits = 4, format = "f", flag = "0"),sep="_")
#   
#   num_pair_data_trim[,"Pair"] <- ifelse(num_pair_data_trim[,"Repeat"]==1,NA,num_pair_data_trim[,"Pair"])
#   
#   #num_pair_data_trim2 <- num_pair_data_trim
#   num_pair_data_trim2 <- num_pair_data_trim[!is.na(num_pair_data_trim[,"Pair"]),]
#   #row.names(num_pair_data_trim2) <- seq(nrow(num_pair_data_trim2))
#   
#   rm(num_pair_data_trim)
#   
#   return(num_pair_data_trim2)
# }

num_pairs_screen_pairs <- function(data,ret_col,lag_ret_col){
  
  # data <- w
  # data <- x
  # ret_col <- ret_col
  # lag_ret_col <- lag_ret_col
  
  num_pair_data <- data.frame(data[,c(ret_col,lag_ret_col)],Missing=NA,Repeat=NA,Pair_Reg=NA,Pair_Reverse=NA,Pair_ID=NA,stringsAsFactors=FALSE)
  
  num_pair_data[,"Missing"] <- ifelse(is.na(num_pair_data[,ret_col]),1,0)
  num_pair_data[,ret_col] <- ifelse(is.na(num_pair_data[,ret_col]),9999,num_pair_data[,ret_col])
  
  num_pair_data[,"Missing"] <- ifelse(is.na(num_pair_data[,lag_ret_col]),1,0)
  num_pair_data[,lag_ret_col] <- ifelse(is.na(num_pair_data[,lag_ret_col]),9999,num_pair_data[,lag_ret_col])
  
  #num_pair_data <- num_pair_data[!(is.na(num_pair_data[,ret_col]) | is.na(num_pair_data[,lag_ret_col])),]
  #row.names(num_pair_data) <- seq(nrow(num_pair_data))

  num_pair_data[,"Repeat"] <- ifelse(num_pair_data[,ret_col]==num_pair_data[,lag_ret_col],1,0)
  
  num_pair_data[,"Pair_Reg"] <- paste(formatC(num_pair_data[,ret_col],digits=4,format="f",flag="0"),
                                       formatC(num_pair_data[,lag_ret_col],digits=4,format="f",flag="0"),sep="_")
  #num_pair_data[,"Pair_Reg"] <- ifelse(num_pair_data[,"Repeat"]==1,NA,num_pair_data[,"Pair_Reg"])
  
  num_pair_data[,"Pair_Reverse"] <- paste(formatC(num_pair_data[,lag_ret_col],digits=4,format="f",flag="0"),
                                               formatC(num_pair_data[,ret_col],digits=4,format="f",flag="0"),sep="_")
  #num_pair_data[,"Pair_Reverse"] <- ifelse(num_pair_data[,"Repeat"]==1,NA,num_pair_data[,"Pair_Reverse"])

  num_pair_data[,"Pair_ID"] <- ifelse(num_pair_data[,ret_col]==num_pair_data[,lag_ret_col],num_pair_data[,"Pair_Reg"],
                                    ifelse(num_pair_data[,ret_col]<num_pair_data[,lag_ret_col],num_pair_data[,"Pair_Reg"],num_pair_data[,"Pair_Reverse"]))
  
  return(num_pair_data)
}

########################################################################################################
# XXXX
########################################################################################################

# num_pairs_screen_counts <- function(pairs,pair_col){
#   
#   require(plyr)
#   
#   # pairs <- num_pair_data
#   # pair_col <- "Pair"
#   
#   pairs_trim <- pairs[,c("Pair")]
#   pairs_trim <- as.data.frame(pairs_trim,stringsAsFactors=FALSE)
#   colnames(pairs_trim) <- pair_col
#   
#   num_pair_data_sum <- count(pairs_trim,pair_col)
#   
#   num_pair_data_sum <- num_pair_data_sum[order(num_pair_data_sum[,"freq"]),]
#   row.names(num_pair_data_sum) <- seq(nrow(num_pair_data_sum))
#   
#   return(num_pair_data_sum)
# }



########################################################################################################
# XXXX
########################################################################################################

# num_pairs_screen_sum <- function(data,data_col){
#   
#   # data <- num_pairs_counts
#   # data_col <- "freq"
#   
#   sum_temp <- data.frame(Max_Pairs=NA,Max_Pairs_Adj=NA,Total=NA,Prop=NA,Prop_Adj=NA,stringsAsFactors=FALSE)
#   
#   sum_temp[,"Max_Pairs"] <- tail(data[,"freq"],1)
#   sum_temp[,"Max_Pairs_Adj"] <- sum_temp[,"Max_Pairs"]-1
#   sum_temp[,"Total"] <- nrow(data)
#   sum_temp[,"Prop"] <- sum_temp[,"Max_Pairs"]/sum_temp[,"Total"] 
#   sum_temp[,"Prop_Adj"] <- sum_temp[,"Max_Pairs_Adj"]/sum_temp[,"Total"] 
#   
#   return(sum_temp)
# }


########################################################################################################
# XXXX
########################################################################################################

uniform_screen_execute <- function(data,ret_col,graph,rounding_digit){
  
  # data <- x
  # ret_col <- analysis_col
  # graph <- FALSE
  # rounding_digit <- 4
  
  screen_uniform <- data.frame(data,Ret_Digits=NA,stringsAsFactors=FALSE)
  
  #suppressWarnings(screen_uniform[,"Ret_Digits"] <- uniform_screen_LHS_digits(screen_uniform[,analysis_col],1))
  suppressWarnings(screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,analysis_col],1,rounding_digit))
  #suppressWarnings(screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,"Monthly_Ret_Percent"],1,2))
  
  ### Expand Digits
  screen_uniform_all <- uniform_screen_expand_digits(z=screen_uniform,test_col="Ret_Digits")
  #rm(screen_uniform)
  
  ### Compute Stats
  screen_uniform_all_stats <- uniform_screen_probs(y=screen_uniform_all,test_col="Ret_Digits")
  #rm(screen_uniform_all)
  
  ### Goodness of Fit test
  screen_uniform_gof <- uniform_screen_gof(w=screen_uniform_all_stats,test_col="Ret_Digits")
  
  ### Graph
  if(graph){
    screen_uniform_graph_temp  <- uniform_screen_graph(g=screen_uniform_all_stats,test_col="Ret_Digits")
    
    #ggplot(screen_uniform_graph_temp,aes(x=fd,y=prob_actual)) + geom_bar(stat="identity",fill="blue") + 
    #  geom_line(aes(x=fd,y=prob_theoretical,size=0.1)) + geom_point(aes(x=fd,y=prob_theoretical,color="red",size=1)) + 
    #  theme_bw() + scale_x_continuous(breaks=seq(min(screen_uniform_graph_temp[,"fd"]):max(screen_uniform_graph_temp[,"fd"])))
    
    plot_temp <- ggplot(screen_uniform_graph_temp,aes(x=fd,y=prob_actual))
    plot_temp <- plot_temp + geom_bar(stat="identity",fill="blue")
    plot_temp <- plot_temp + geom_line(aes(x=fd,y=prob_theoretical,size=0.1))
    plot_temp <- plot_temp + geom_point(aes(x=fd,y=prob_theoretical,color="red",size=1))
    plot_temp <- plot_temp + theme_bw()  
    plot_temp <- plot_temp + scale_x_continuous(breaks=seq(min(screen_uniform_graph_temp[,"fd"]):max(screen_uniform_graph_temp[,"fd"])))
    plot_temp  
  }
  rm(screen_uniform_all_stats)
  return(screen_uniform_gof)
}


########################################################################################################
# XXXX
########################################################################################################

uniform_screen_LHS_digits <- function(x,digits){
  
  # x <- uniform_data[,analysis_col]
  # digits <- 1
  
  require(plyr)
  
  x_out <- ldply(.data=x, .fun = function(y,digits){
    
    # y <- screen_uniform[1,analysis_col]
    y_out0 <- formatC(abs(y), width = digits, format = "d", flag = "0")
    y_out1 <- head(strsplit(y_out0,'')[[1]],n=digits)
    y_out2 <- paste(y_out1,sep="",collapse="")
    return(y_out2) 
    
  },digits=digits)
  
  x_out <- as.numeric(x_out[,1])
  
  return(x_out)
}

########################################################################################################
# XXXX
########################################################################################################

uniform_screen_RHS_digits <- function(x,n,digits){
  
  # x <- screen_uniform[,analysis_col]
  # n <- 1
  # digits <- 6
  
  #x_out <- round(abs(screen_uniform[,analysis_col]) * (10^digits)) %% 10
  #x_out<-  substr(formatC(x, digits = digits, format = "f", flag = "0"), 
  #                nchar(formatC(x, digits = digits, format = "f", flag = "0"))-n+1, 
  #                nchar(formatC(x, digits = digits, format = "f", flag = "0")))
  #x_out <- as.numeric(x_out)
  
  x_out <- formatC(x, digits = digits, format = "f", flag = "0")
  return(as.numeric(substr(x_out, nchar(x_out)-n+1,nchar(x_out))))
}


########################################################################################################
# XXXX
########################################################################################################

uniform_screen_expand_digits <- function(z,test_col){
  
  # z <- screen_uniform
  # test_col <- screen_uniform_test_col
  
  screen_uniform_all_temp_full <- data.frame(temp_col=seq(0,9,1),stringsAsFactors=FALSE)
  
  colnames(screen_uniform_all_temp_full)[match("temp_col",names(screen_uniform_all_temp_full))] <- test_col
  
  screen_uniform_all <- merge(screen_uniform_all_temp_full, 
                              data.frame(count(z, test_col),stringsAsFactors=FALSE), 
                              by.x=c(test_col), by.y=c(test_col), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  screen_uniform_all <- screen_uniform_all[order(screen_uniform_all[,test_col]),]
  row.names(screen_uniform_all) <- seq(nrow(screen_uniform_all))
  
  screen_uniform_all <- screen_uniform_all[,c(test_col,"freq", colnames(screen_uniform_all[,!(colnames(screen_uniform_all) %in% c(test_col,"freq"))]))]
  
  screen_uniform_all[,"freq"] <- ifelse(is.na(screen_uniform_all[,"freq"]),0,screen_uniform_all[,"freq"])
  
  screen_uniform_all[,test_col] <- as.numeric(screen_uniform_all[,test_col])
  screen_uniform_all[,"freq"] <- as.integer(screen_uniform_all[,"freq"])
  
  return(screen_uniform_all)
}

########################################################################################################
# XXXX
########################################################################################################

uniform_screen_probs <- function(y,test_col){

  # y <- screen_uniform_all
  # test_col <- screen_uniform_test_col
  
  ### NOTE: logarithmic (Benford Test) doesn't include the 0 digit
  ### NOTE: Benford'a Law is applied to the first digit so not really applicable here
  
  screen_uniform_all <- data.frame(y,prob_logarithmic_actual=NA,prob_logarithmic_theoretical=NA,
                                   prob_uniform_actual=NA,prob_uniform_theoretical=NA,
                                   stringsAsFactors=FALSE)
  
  screen_uniform_all[,"prob_uniform_actual"] <- prop.table(screen_uniform_all[,"freq"])
  screen_uniform_all[,"prob_uniform_theoretical"] <- 0.10
  
  screen_uniform_all[,"prob_uniform_actual"] <- ifelse(is.na(screen_uniform_all[,"prob_uniform_actual"]),0,screen_uniform_all[,"prob_uniform_actual"])
  screen_uniform_all[,"prob_uniform_theoretical"] <- ifelse(is.na(screen_uniform_all[,"prob_uniform_theoretical"]),0,screen_uniform_all[,"prob_uniform_theoretical"])
  
  screen_uniform_all_trim <- screen_uniform_all[!(screen_uniform_all[,test_col]==0),]
  
  screen_uniform_all[,"prob_logarithmic_actual"] <- c(NA,prop.table(screen_uniform_all_trim[!(screen_uniform_all_trim[,test_col]==0),"freq"]))
  screen_uniform_all[,"prob_logarithmic_theoretical"] <- c(NA,log10(screen_uniform_all_trim[,test_col]+ 1) - log10(screen_uniform_all_trim[,test_col] + 0))
  
  screen_uniform_all[,"prob_logarithmic_actual"] <- ifelse(is.na(screen_uniform_all[,"prob_logarithmic_actual"]),0,screen_uniform_all[,"prob_logarithmic_actual"])
  screen_uniform_all[,"prob_logarithmic_theoretical"] <- ifelse(is.na(screen_uniform_all[,"prob_logarithmic_theoretical"]),0,screen_uniform_all[,"prob_logarithmic_theoretical"])
  
  screen_uniform_all[1,c("prob_logarithmic_actual","prob_logarithmic_theoretical")] <- c(NA,NA)
  
  #sum(screen_uniform_all[,"prob_logarithmic_actual"],na.rm=TRUE)
  #sum(screen_uniform_all[,"prob_logarithmic_theoretical"],na.rm=TRUE)
  #sum(screen_uniform_all[,"prob_uniform_actual"],na.rm=TRUE)
  #sum(screen_uniform_all[,"prob_uniform_theoretical"],na.rm=TRUE)
  
 return(screen_uniform_all)
}


########################################################################################################
# XXXX
########################################################################################################

uniform_screen_chisq.test <- function(data,freq_col,prob_t_col){
  screen_uniform_all_out_temp0 <- tryCatch({
    #message("This is the 'try' part")
    suppressWarnings(chisq.test(data[,freq_col],p=data[,prob_t_col]))
  },error=function(cond) {
    #message(paste("There was an error: ","\n","Here's the original error message:","\n",cond,sep=""))
    return(NULL)
  },warning=function(cond) {
    #message(paste("There was an warnings: ","\n","Here's the original warnings message:","\n",cond,sep=""))
    #return(chisq.test(x[,"freq"],p=x[,"prob_uniform_theoretical"]))
  },finally={
    #message(paste("Good: ","\n","Some other message at the end",sep=""))
  })
  screen_uniform_all_out_temp <- data.frame(x_squared=NA,df=NA,pval=NA,stringsAsFactors=FALSE)
  if(length(screen_uniform_all_out_temp0)>0){
    screen_uniform_all_out_temp[,"x_squared"] <- screen_uniform_all_out_temp0$statistic
    screen_uniform_all_out_temp[,"df"] <- screen_uniform_all_out_temp0$parameter
    screen_uniform_all_out_temp[,"pval"] <- screen_uniform_all_out_temp0$p.value
  }
  return(screen_uniform_all_out_temp)
}

########################################################################################################
# XXXX
########################################################################################################

uniform_screen_gof <- function(w,test_col){
  
  # w <- screen_uniform_all_stats
  # test_col <- screen_uniform_test_col
  
  screen_uniform_gof_log_cols <- c("x_squared_logarithmic","df_logarithmic","pval_logarithmic")
  screen_uniform_gof_uni_cols <- c("x_squared_uniform","df_uniform","pval_uniform")
  screen_uniform_gof_all_cols <- c(screen_uniform_gof_log_cols,screen_uniform_gof_uni_cols)
  
  screen_uniform_gof <- data.frame(matrix(NA, ncol=length(c(screen_uniform_gof_log_cols,screen_uniform_gof_uni_cols)), nrow=1, 
                                          dimnames=list(c(), c(screen_uniform_gof_log_cols,screen_uniform_gof_uni_cols))), stringsAsFactors=FALSE)
  
  screen_uniform_gof[,screen_uniform_gof_uni_cols] <- uniform_screen_chisq.test(data=w,freq_col="freq", prob_t_col="prob_uniform_theoretical")

  screen_uniform_gof[,screen_uniform_gof_log_cols] <- uniform_screen_chisq.test(data=w[!(w[,test_col]==0),],freq_col="freq", prob_t_col="prob_logarithmic_theoretical")
  
  return(screen_uniform_gof)
}

########################################################################################################
# XXXX
########################################################################################################

uniform_screen_graph <- function(g,test_col){
  
  # g <- screen_uniform_all_stats
  # test_col <- screen_uniform_test_col
  
  require(ggplot2)
  
  screen_uniform_graph_temp_prob_actual <- "prob_uniform_actual"
  screen_uniform_graph_temp_prob_theoretical <- "prob_uniform_theoretical"
  
  screen_uniform_graph_temp <- g
  screen_uniform_graph_temp <- screen_uniform_graph_temp[!(rowSums(is.na(screen_uniform_graph_temp[,1:ncol(screen_uniform_graph_temp)]))>0),]
  row.names(screen_uniform_graph_temp) <- seq(nrow(screen_uniform_graph_temp))
  
  colnames(screen_uniform_graph_temp)[match(test_col,names(screen_uniform_graph_temp))] <- "fd"
  colnames(screen_uniform_graph_temp)[match(screen_uniform_graph_temp_prob_actual,names(screen_uniform_graph_temp))] <- "prob_actual"
  colnames(screen_uniform_graph_temp)[match(screen_uniform_graph_temp_prob_theoretical,names(screen_uniform_graph_temp))] <- "prob_theoretical"

  #rm(screen_uniform_graph_temp,screen_uniform_graph_temp_prob_actual,screen_uniform_graph_temp_prob_theoretical)
 
 return(screen_uniform_graph_temp)
}
