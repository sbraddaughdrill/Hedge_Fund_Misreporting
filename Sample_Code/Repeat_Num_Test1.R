library(plyr)

repeat_num_data1 <- data.frame(fund_id=0,
                               Num=c(NA,-10.0,seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,10,1)),
                               stringsAsFactors=FALSE)

repeat_num_data2 <- data.frame(fund_id=1,
                               Num=c(NA,-1.99,seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,7,1)),
                               stringsAsFactors=FALSE)

repeat_num_data <- rbind(repeat_num_data1,repeat_num_data2)

rm(repeat_num_data1,repeat_num_data2)

repeat_num_data_trim <- repeat_num_data[!is.na(repeat_num_data[,"Num"]),]
row.names(repeat_num_data_trim) <- seq(nrow(repeat_num_data_trim))

rm(repeat_num_data)

#############




repeat_num_data_count <- ddply(.data=repeat_num_data_trim, .variables=c("fund_id","Num"), .fun = function(x,id_col){
  
  # x <- repeat_num_data_trim[repeat_num_data_trim[,"fund_id"]==0,]
  # id_col <- "fund_id"
  
  count_temp <- unique(data.frame(x,Freq=nrow(x),stringsAsFactors=FALSE))
  
  return(count_temp)
  
},id_col="fund_id",.progress = "none")

repeat_num_data_count <- repeat_num_data_count[order(repeat_num_data_count[,"fund_id"], repeat_num_data_count[,"Freq"]),]
row.names(repeat_num_data_count) <- seq(nrow(repeat_num_data_count))

rm(repeat_num_data_trim)


repeat_num_data_sum <- ddply(.data=repeat_num_data_count[,c("fund_id","Freq")], .variables="fund_id", .fun = function(x,id_col,data_col){
  
  # x <- repeat_num_data_count[repeat_num_data_count[,"fund_id"]==0,c("fund_id","Freq")]
  # id_col <- "fund_id"
  # data_col <- "Freq"
  
  fund_id <- unique(x[,id_col])
  
  sum_temp <- data.frame(tail(x,1),
                         Count_u=NA,
                         Total=NA,
                         Prop_u=NA,
                         Flag=NA,
                         stringsAsFactors=FALSE)
  
  sum_temp[,"Count_u"] <- nrow(x)
  sum_temp[,"Total"] <- sum(x[,data_col])
  sum_temp[,"Prop_u"] <- sum_temp[,"Count_u"]/sum_temp[,"Total"] 
  
  return(sum_temp)
  
},id_col="fund_id",data_col="Freq",.progress = "none")

rm(repeat_num_data_count)

