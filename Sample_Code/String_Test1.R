library(plyr)

string_data1 <- data.frame(fund_id=0,
                           Freq=c(seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,10,1)),
                           stringsAsFactors=FALSE)

string_data2 <- data.frame(fund_id=1,
                           Freq=c(seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,7,1)),
                           stringsAsFactors=FALSE)

string_data <- rbind(string_data1,string_data2)

rm(string_data1,string_data2)



string_data_rle <- ddply(.data=string_data, .variables="fund_id", .fun = function(x,id_col,data_col){
  
  # x <- string_data[string_data[,"fund_id"]==0,]
  # id_col <- "fund_id"
  # data_col <- "Freq"
  
  fund_id <- unique(x[,id_col])
  
  rle_temp <- data.frame(temp_id=NA,
                         do.call(cbind,rle(x[,data_col])),
                         stringsAsFactors=FALSE)
  colnames(rle_temp)[match("temp_id",names(rle_temp))] <- id_col
  
  rle_temp[,id_col] <- fund_id
  rle_temp <- rle_temp[,c(id_col,"values","lengths")]
  
  return(rle_temp)
  
},id_col="fund_id", data_col="Freq",.progress = "none")

rm(string_data)

string_data_rle_sum <- data.frame(unique(string_data_rle[,c("fund_id","lengths")]),
                                  stringsAsFactors=FALSE)

rm(string_data_rle)

string_data_rle_sum <- string_data_rle_sum[order(string_data_rle_sum[,"fund_id"], string_data_rle_sum[,"lengths"]),]
row.names(string_data_rle_sum) <- seq(nrow(string_data_rle_sum))


string_data_rle_max <- ddply(.data=string_data_rle_sum, .variables="fund_id", .fun = function(x,id_col){
  
  # x <- string_data_rle_sum[string_data_rle_sum[,"fund_id"]==0,]
  # id_col <- "fund_id"
  
  fund_id <- unique(x[,id_col])
  
  max_temp <- data.frame(tail(x,1),
                         flag=NA,
                         stringsAsFactors=FALSE)
  return(max_temp)
  
},id_col="fund_id",.progress = "none")


rm(string_data_rle_sum)


