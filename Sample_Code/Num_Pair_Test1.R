library(plyr)

num_pair_data1 <- data.frame(fund_id=0,
                           Freq=c(NA,-10.0,seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,10,1)),
                           Freq_Lag=c(1,NA,-10.0,seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,9,1)),
                           Repeat=NA,
                           Pair=NA,
                           stringsAsFactors=FALSE)

num_pair_data2 <- data.frame(fund_id=1,
                           Freq=c(NA,-1.99,seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,7,1)),
                           Freq_Lag=c(1,NA,-1.99,seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,6,1)),
                           Repeat=NA,
                           Pair=NA,
                           stringsAsFactors=FALSE)

num_pair_data <- rbind(num_pair_data1,num_pair_data2)

rm(num_pair_data1,num_pair_data2)

########################

num_pair_data_trim <- num_pair_data[!(is.na(num_pair_data[,"Freq"]) | is.na(num_pair_data[,"Freq_Lag"])),]
row.names(num_pair_data_trim) <- seq(nrow(num_pair_data_trim))

rm(num_pair_data)

num_pair_data_trim[,"Repeat"] <- ifelse(num_pair_data_trim[,"Freq"]==num_pair_data_trim[,"Freq_Lag"],1,0)

num_pair_data_trim[,"Pair"] <- paste(formatC(num_pair_data_trim[,"Freq"], digits = 4, format = "f", flag = "0"),
                                     formatC(num_pair_data_trim[,"Freq_Lag"], digits = 4, format = "f", flag = "0"),sep="_")

num_pair_data_trim[,"Pair"] <- ifelse(num_pair_data_trim[,"Repeat"]==1,NA,num_pair_data_trim[,"Pair"])

num_pair_data_trim2 <- num_pair_data_trim[!is.na(num_pair_data_trim[,"Pair"]),]
row.names(num_pair_data_trim2) <- seq(nrow(num_pair_data_trim2))

rm(num_pair_data_trim)

num_pair_data_sum <- ddply(.data=num_pair_data_trim2[,c("fund_id","Pair")], .variables=c("fund_id","Pair"), .fun = function(x,id_col,data_col){
  
  # x <- num_pair_data_trim2[num_pair_data_trim2[,"fund_id"]==0,c("fund_id","Pair")]
  # id_col <- "fund_id"
  # data_col <- "Pair"
  
  rle_temp <- unique(data.frame(x,Freq=nrow(x),stringsAsFactors=FALSE))
  
  return(rle_temp)
  
},id_col="fund_id", data_col="Pair",.progress = "none")

num_pair_data_sum <- num_pair_data_sum[order(num_pair_data_sum[,"fund_id"], num_pair_data_sum[,"Freq"]),]
row.names(num_pair_data_sum) <- seq(nrow(num_pair_data_sum))

rm(num_pair_data_trim2)

num_pair_data_max <- ddply(.data=num_pair_data_sum[,c("fund_id","Freq")], .variables="fund_id", .fun = function(x,id_col){
  
  # x <- num_pair_data_sum[num_pair_data_sum[,"fund_id"]==0,]
  # id_col <- "fund_id"
  
  fund_id <- unique(x[,id_col])
  
  max_temp <- data.frame(tail(x,1),
                         flag=NA,
                         stringsAsFactors=FALSE)
  return(max_temp)
  
},id_col="fund_id",.progress = "none")

rm(num_pair_data_sum)

