library(data.table,plyr,reshape2)


###############################################################################
cat("SECTION: IMPORT DATA", "\n")
###############################################################################

cols <- c("Item_Order","SubItem_Order","MRet_Percent","MRet_Dec","ARet_Percent","ARet_Dec")
obs <- 100000
groups <- 5

test_data <- data.frame(matrix(NA, ncol=length(cols), nrow=obs, dimnames=list(c(), cols)), 
                        stringsAsFactors=FALSE)


test_data[,"Item_Order"] <- unlist(lapply(seq(1,groups,1), rep, obs/groups))
test_data[,"SubItem_Order"] <- rep.int(seq(1,obs/groups,1), groups)
test_data[,"MRet_Percent"] <- rnorm(n=obs, m=0, sd=0.5)
test_data[,"MRet_Dec"] <- test_data[,"MRet_Percent"]/100
test_data[,"ARet_Dec"] <- ((1+test_data[,"MRet_Dec"])^12)-1
test_data[,"ARet_Percent"] <- test_data[,"ARet_Dec"]*100


###############################################################################
cat("SECTION: CHECK FOR DICONTINUITY", "\n")
###############################################################################

#analysis_col <- "MRet_Percent"
analysis_col <- "MRet_Dec"
#analysis_col <- "ARet_Dec"

ret_min <- min(test_data[,analysis_col])
ret_max <- max(test_data[,analysis_col])

#rounding_level <- 0.5
rounding_level <- 0.005

extremes <- round_any(max(abs(test_data[,analysis_col])),rounding_level,ceiling)

#interval <- 0.25
#interval <- 0.0025
interval <- 0.00025

breaks <- seq(-extremes,extremes,interval)

bins <- ddply(.data=test_data, .variables="Item_Order", .fun = function(x,id_col,analysis_col,breaks,plot_flag){
  
  #  x <- test_data[test_data[,"Item_Order"]==1,]
  #  id_col <- "Item_Order"
  #  analysis_col <- analysis_col
  #  breaks <- breaks
  #  plot_flag <- FALSE
  #  plot_flag <- TRUE
  
  id_temp <- unique(x[,id_col])
  
  bins_temp <- hist(x[,analysis_col], breaks=breaks, plot=plot_flag)
  
  bins_out <- data.frame(matrix(NA, ncol=5, nrow=length(breaks), dimnames=list(c(), c(id_col,"breaks","counts","density","mids"))), 
                         stringsAsFactors=FALSE)
  
  bins_out[,id_col] <- id_temp
  bins_out[,"breaks"] <- bins_temp$breaks
  bins_out[,"counts"] <- c(bins_temp$counts,NA)
  bins_out[,"density"] <- c(bins_temp$density,NA)
  bins_out[,"mids"] <- c(bins_temp$mids,NA)
  
  return(bins_out)
  
}, id_col="Item_Order", analysis_col=analysis_col,breaks=breaks, plot_flag=FALSE, 
.progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


break_keep3 <- which(breaks==0.0)
break_keep2 <- break_keep3-1
break_keep1 <- break_keep3-2

bins_keep <- bins[bins[,"breaks"] %in% breaks[c(break_keep1,break_keep2,break_keep3)],]

rm(break_keep1,break_keep2,break_keep3)

bins_discontinuity0 <- dcast(bins_keep[,c("Item_Order","breaks","counts")], Item_Order ~ breaks, 
                            value.var = c("counts"))

colnames(bins_discontinuity0) <- c("Item_Order","bin1_count","bin2_count","bin3_count")


bins_discontinuity <- data.frame(bins_discontinuity0, 
                                 outside_bin_avg=NA, 
                                 kink_ratio=NA, 
                                 kink_percent_95=NA, 
                                 kink_percent_90=NA, 
                                 kink_percent_75=NA,
                                 kink_percent_66=NA,
                                 kink_percent_50=NA,
                                 stringsAsFactors=FALSE)
bins_discontinuity[,"outside_bin_avg"] <- rowMeans(bins_discontinuity[,c("bin1_count","bin3_count")], na.rm = TRUE)
bins_discontinuity[,"kink_ratio"] <- bins_discontinuity[,"bin2_count"]/bins_discontinuity[,"outside_bin_avg"]

bins_discontinuity[,"kink_percent_95"] <- ifelse(bins_discontinuity[,"kink_ratio"]<=0.95,1,0)
bins_discontinuity[,"kink_percent_90"] <- ifelse(bins_discontinuity[,"kink_ratio"]<=0.90,1,0)
bins_discontinuity[,"kink_percent_75"] <- ifelse(bins_discontinuity[,"kink_ratio"]<=0.75,1,0)
bins_discontinuity[,"kink_percent_66"] <- ifelse(bins_discontinuity[,"kink_ratio"]<=0.66,1,0)
bins_discontinuity[,"kink_percent_50"] <- ifelse(bins_discontinuity[,"kink_ratio"]<=0.50,1,0)