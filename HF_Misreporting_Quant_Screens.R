# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Quant_Screens.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: This is the Performance Screens from Bollen_Pool (2012)
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))
rm(list = ls(all.names = TRUE))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
#unknowns_strings <- c("",".",NA,"na","n/a","n\a","NA","N/A","N\\A","<NA>","null","NULL",NULL,"nan","NaN",NaN,
#                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",NA_character_,
#                      "NA_character_",NA_real_,"NA_real_")
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)    
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)  
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE)   
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else {
  
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)

source(file=paste(function_directory,"functions_misreporting_screens.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","ggplot2","MASS","plyr","quantmod",
                       "reshape2","RSQLite","stringr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="\\")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="\\")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="\\")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="\\")
#descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="\\")
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="\\")


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

#analysis_col <- "mktadjret"
analysis_col <- "monthly_ret"

#beg_year <- 1994
#end_year <- 2011

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_prescreen <- read.csv(file=paste(output_directory,"data_prescreen.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

cutoffs_60 <- read.csv(file=paste(output_directory,"cutoff_simulation","Cutoff_Simulation_60.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
cutoffs_120 <- read.csv(file=paste(output_directory,"cutoff_simulation","Cutoff_Simulation_120.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("SECTION: DATA CLEANING", "\n")
###############################################################################

### Preallocate Data 

data_trim_lag_count <- 4
data_trim_unlagged_cols <- c("monthly_ret","mktadjret")

#data_trim_lagged_cols <- c(paste(data_trim_unlagged_cols[1],"_lag",seq(1,data_trim_lag_count),sep=""),
#                           paste(data_trim_unlagged_cols[2],"_lag",seq(1,data_trim_lag_count),sep=""))
data_trim_lagged_cols <- unlist(lapply(data_trim_unlagged_cols,function(x,lag_length){ paste(x,"_lag",seq(1,lag_length),sep="") },
                                       lag_length=data_trim_lag_count))

data_trim_lagged_full_cols <- c(data_trim_unlagged_cols,data_trim_lagged_cols)
data_trim_lagged_full_cols <- sort(data_trim_lagged_full_cols)

data_trim_lagged_trim_cols <- data_trim_lagged_full_cols[grepl(analysis_col, data_trim_lagged_full_cols)]

data_trim_id_cols <- c(identifier,"yr","month","yr_month")

data_trim_id_full_cols <- c("return_id",data_trim_id_cols)
data_trim_id_full_cols <- c(identifier,data_trim_id_full_cols[!data_trim_id_full_cols %in% c(identifier)])

data_trim0 <- data.frame(data_prescreen[,c(data_trim_id_cols,data_trim_unlagged_cols)],
                         return_id=NA,
                         matrix(NA, ncol=length(data_trim_unlagged_cols)*data_trim_lag_count, nrow=nrow(data_prescreen), 
                                dimnames=list(c(), data_trim_lagged_cols)),
                         stringsAsFactors=FALSE)

#data_trim0 <- data_trim0[,c(data_trim_id_full_cols, colnames(data_trim0[,!(colnames(data_trim0) %in% c(data_trim_id_full_cols))]))]
data_trim0 <- data_trim0[,c(data_trim_id_full_cols,data_trim_lagged_full_cols)]

### Make sure funds have atleast 24 months of returns
data_trim0_firm_counts <- count(data_trim0, c(identifier))
data_trim0_firm_keep <- data_trim0_firm_counts[data_trim0_firm_counts[,"freq"]>=24,]
data_trim0_firm_keep <- data_trim0_firm_keep[!is.na(data_trim0_firm_keep[,c(identifier)]),]
row.names(data_trim0_firm_keep) <- seq(nrow(data_trim0_firm_keep))

data_trim <- data_trim0[(data_trim0[,c(identifier)] %in% data_trim0_firm_keep[,c(identifier)]),]
row.names(data_trim) <- seq(nrow(data_trim))

rm(data_trim0,data_trim0_firm_counts,data_trim0_firm_keep)

#max(data_trim[,"monthly_ret"])
#min(data_trim[,"monthly_ret"])
#mean(data_trim[,"monthly_ret"])
#median(data_trim[,"monthly_ret"])

#max(count(data_trim,identifier)[,"freq"])
#min(count(data_trim,identifier)[,"freq"])
#mean(count(data_trim,identifier)[,"freq"])
#median(count(data_trim,identifier)[,"freq"])

### Create Return ID
data_trim <- data_trim[order(data_trim[,identifier],
                             data_trim[,"yr_month"]),]
row.names(data_trim) <- seq(nrow(data_trim))

data_trim <- ddply(.data=data_trim, .variables=c(identifier), .fun = function(x){ 
  
  x[,"return_id"] <- seq(1,nrow(x))
  
  return(x)
  
},.drop = FALSE)

data_trim <- data_trim[order(data_trim[,identifier],
                             data_trim[,"return_id"]),]
row.names(data_trim) <- seq(nrow(data_trim))

### Populate Lags
for (i in 1:length(data_trim_unlagged_cols))
{
  #i <- 1
  #i <- 2
  #i <- 3
  
  data_trim[,paste(data_trim_unlagged_cols[i],"_lag",seq(1,data_trim_lag_count),sep="")] <- 
    create_lags2(data_trim,data_trim_unlagged_cols[i],identifier,data_trim_lag_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(data_trim_unlagged_cols), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 
rm(i)

rm(data_trim_lag_count,data_trim_unlagged_cols,data_trim_lagged_cols,data_trim_lagged_full_cols)

### Create Overall Data

data_trim_overall <- data_trim

data_trim_overall[,identifier] <- 0
data_trim_overall[,"return_id"] <- seq(nrow(data_trim_overall))

### Combine Overall and Group Data

data_trim_full <- rbind(data_trim_overall,data_trim)

rm(data_trim_overall,data_trim)
rm(data_trim_id_cols)


###############################################################################
cat("SECTION: CREATE CUTOFF TABLE", "\n")
###############################################################################

cutoffs_comb <- rbind(data.frame(Type="60-Month History",cutoffs_60[1:6,],stringsAsFactors=FALSE),
                      data.frame(Type="120-Month History",cutoffs_120[1:6,],stringsAsFactors=FALSE))

rm(cutoffs_60,cutoffs_120)

cutoff_type <- "60-Month History"


###############################################################################
cat("SECTION: SCREENS", "\n")
###############################################################################

###############################################################################
##
cat("SECTION: SCREEN 1 - DISCONTINUITY AT ZERO", "\n")
##
###############################################################################

data_s1 <- data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)]


###############################################################################
cat("S1 - (1) KINK", "\n")
###############################################################################

kink_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  data_s1_bins <- kink_screen_bins(data=data,ret_col=ret_col)
  
  data_s1_ratios <- kink_screen_ratios(bins=data_s1_bins)
  
  rm(data_s1_bins)
  
  return(data_s1_ratios)
  
}


data_s1_1 <- data_s1[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)]
data_s1_1[,data_trim_lagged_trim_cols] <- round(data_s1_1[,data_trim_lagged_trim_cols],digits=4)

data_s1_bins <- ddply(.data=data_s1_1, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s1_1[data_s1_1[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  kink_screen <- kink_screen_execute(data=x,ret_col=analysis_col)
  
  return(kink_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s1_1)


### Create Flags

data_s1_bins_full <- data.frame(data_s1_bins,
                                kink_percent_99=NA, kink_percent_95=NA, kink_percent_90=NA, 
                                kink_percent_75=NA,kink_percent_66=NA,kink_percent_50=NA,
                                stringsAsFactors=FALSE)

rm2(data_s1_bins)

data_s1_bins_full[,"kink_percent_99"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.99,1,0)
data_s1_bins_full[,"kink_percent_95"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.95,1,0)
data_s1_bins_full[,"kink_percent_90"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.90,1,0)
data_s1_bins_full[,"kink_percent_75"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.75,1,0)
data_s1_bins_full[,"kink_percent_66"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.66,1,0)
data_s1_bins_full[,"kink_percent_50"] <- ifelse(data_s1_bins_full[,"kink_ratio"]<=0.50,1,0)

data_s1_final <- data_s1_bins_full[,c(identifier,"kink_percent_99","kink_percent_95","kink_percent_90","kink_percent_75","kink_percent_66","kink_percent_50")]

rm2(data_s1_bins_full)

rm2(data_s1)


###############################################################################
##
cat("SECTION: SCREEN 2 - LOW CORRELATION WITH OTHER ASSETS", "\n")
##
###############################################################################

data_s2_temp <- data.frame(data_trim_full[,c(data_trim_id_full_cols,analysis_col)],stringsAsFactors=FALSE)

data_s2_style_temp0 <- read.csv(file=paste(output_directory,"EurekahedgeHF_Excel_aca.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
data_s2_style_temp1 <- data_s2_style_temp0[,c("Fund.ID","Main.Investment.Strategy")]

rm(data_s2_style_temp0)

colnames(data_s2_style_temp1)[match("Fund.ID",names(data_s2_style_temp1))] <- identifier
colnames(data_s2_style_temp1)[match("Main.Investment.Strategy",names(data_s2_style_temp1))] <- "Main_Investment_Strategy"

data_s2_style_temp1[,"Main_Investment_Strategy"] <- ifelse(data_s2_style_temp1[,"Main_Investment_Strategy"]=="NA",NA, data_s2_style_temp1[,"Main_Investment_Strategy"])

data_s2_merge <- merge(data_s2_temp, data_s2_style_temp1, 
                       by.x=c(identifier), by.y=c(identifier), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(data_s2_temp,data_s2_style_temp1)

data_s2_merge[,"Main_Investment_Strategy"] <- ifelse(data_s2_merge[,identifier]==0,"ALL",data_s2_merge[,"Main_Investment_Strategy"])

data_s2_merge <- data_s2_merge[order(data_s2_merge[,identifier], data_s2_merge[,"return_id"]),]
row.names(data_s2_merge) <- seq(nrow(data_s2_merge))

data_s2_merge_trim0 <- data_s2_merge

rm(data_s2_merge)

data_s2_merge_trim1 <- data_s2_merge_trim0[!(data_s2_merge_trim0[,identifier]==0),]

rm(data_s2_merge_trim0)

data_s2_merge_trim2 <- data_s2_merge_trim1[!is.na(data_s2_merge_trim1[,"Main_Investment_Strategy"]),]

rm(data_s2_merge_trim1)

data_s2 <- data_s2_merge_trim2

rm2(data_s2_merge_trim2)


###############################################################################
cat("S2 - (1) MAXRSQ ", "\n")
###############################################################################


###############################################################################
cat("S2 - (2) SWITCHRSQ", "\n")
###############################################################################


###############################################################################
cat("S2 - (3) INDEXRSQ", "\n")
###############################################################################

indexrsq_screen_execute <- function(data_style,id,ret_col,id_col,date_col){
  
  # data_style <- data_style
  # id <- x
  # ret_col <- analysis_col
  
  data_s2_3_reg_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  indexrsq_screen_rets <- indexrsq_screen_returns(data_style=data_style,id=id,ret_col=ret_col,id_col=id_col,date_col=date_col)
  indexrsq_screen_temp <- indexrsq_screen_model(data_ret=indexrsq_screen_rets,fund_ret_col=ret_col,index_ret_col=paste("avg",ret_col,sep="_"),model_cols=data_s2_3_reg_cols)
  
  return(indexrsq_screen_temp)
  
}


data_s2_3 <- data_s2
data_s2_3[,analysis_col] <- round(data_s2_3[,analysis_col],digits=4)

data_s2_3_style_counts <- data.frame(style_id=NA,count(data_s2_3,"Main_Investment_Strategy"),stringsAsFactors=FALSE)
data_s2_3_style_counts[,"style_id"] <- seq(1,nrow(data_s2_3_style_counts))

data_s2_3_prob <- ddply(.data=data_s2_3_style_counts, .variables="style_id", .fun = function(y,data,style_col,analysis_col,id_col,date_col){
  
  # y <- "Bottom-Up"
  # y <- "Long Short Equities"
  # data <- data_s2_3
  # style_col <- "Main_Investment_Strategy"
  # id_col <- identifier
  # date_col <- "yr_month"
  
  style <- unique(y[,style_col]) 
  
  #cat( "\n","Style", style, "\n")
  
  data_style <- data[data[,style_col]==style,c(id_col,date_col,analysis_col)]
  
  data_s2_3_prob_sub <- ldply(.data=unique(data_style[,id_col]), .fun = function(x,data_style,analysis_col,id_col,date_col){
    
    # x <- "5002"
    # x <- "5003"
    # data_style <- data_style
    
    indexrsq_screen <- data.frame(temp_id=x,indexrsq_screen_execute(data_style=data_style,id=x,ret_col=analysis_col,id_col=id_col,date_col=date_col),
                                  stringsAsFactors=FALSE)
    colnames(indexrsq_screen)[match("temp_id",names(indexrsq_screen))] <- id_col
    
    return(indexrsq_screen)
    
  }, data_style=data_style,analysis_col=analysis_col,id_col=id_col,date_col=date_col ,.progress = "none", .inform = FALSE)
  
  return(data_s2_3_prob_sub)
  
},data=data_s2_3,style_col="Main_Investment_Strategy",analysis_col=analysis_col,id_col=identifier, date_col="yr_month",.progress = "text", .inform = FALSE)

rm(data_s2_3,data_s2_3_style_counts)


### Create Flags

data_s2_3_full <- data.frame(data_s2_3_prob,indexrsq_percent_99=NA,indexrsq_percent_95=NA,indexrsq_percent_90=NA,stringsAsFactors=FALSE)
data_s2_3_full <- data_s2_3_full[order(data_s2_3_full[,identifier]),]
row.names(data_s2_3_full) <- seq(nrow(data_s2_3_full))

rm2(data_s2_3_prob)

data_s2_3_full[,"indexrsq_percent_99"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.01),1,0)
data_s2_3_full[,"indexrsq_percent_95"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.05),1,0)
data_s2_3_full[,"indexrsq_percent_90"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.10),1,0)

data_s2_3_final <- data_s2_3_full[,c(identifier,"indexrsq_percent_99","indexrsq_percent_95","indexrsq_percent_90")]

rm2(data_s2_3_full)


###############################################################################
cat("S2 - MERGE FLAGS", "\n")
###############################################################################

data_s2_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s2_final0)[match("temp_id",names(data_s2_final0))] <- identifier

data_s2_final1 <- merge(data_s2_final0, data_s2_3_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s2_final0,data_s2_3_final)


data_s2_final <- data_s2_final1

#rm2(data_s2,data_s2_final1)


###############################################################################
##
cat("SECTION: SCREEN 3 - UNCONDITIONAL SERIAL CORRELATION", "\n")
##
###############################################################################

data_s3 <- data.frame(data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)],stringsAsFactors=FALSE)


###############################################################################
cat("S3 - (1) AR(1)", "\n")
###############################################################################

ar_screen_execute <- function(data,ret_col,lag_ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  # lag_ret_col <- paste(analysis_col,"lag1",sep="_")
  
  
  data_s3_1_reg_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  ar_screen_temp <- ar_screen_ar_model(data=data,ret_col=ret_col,lag_ret_col=lag_ret_col,model_cols=data_s3_1_reg_cols)
  
  return(ar_screen_temp)
  
}

data_s3_1 <- data_s3[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)]
data_s3_1[,data_trim_lagged_trim_cols] <- round(data_s3_1[,data_trim_lagged_trim_cols],digits=4)

data_s3_1_prob <- ddply(.data=data_s3_1, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s3_1[data_s3_1[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  ar_screen <- ar_screen_execute(data=x, ret_col=analysis_col, lag_ret_col=paste(analysis_col,"lag1",sep="_"))
  
  return(ar_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s3_1)


### Create Flags

data_s3_1_full <- data.frame(data_s3_1_prob,ar_1_percent_99=NA,ar_1_percent_95=NA,ar_1_percent_90=NA,
                             stringsAsFactors=FALSE)

rm2(data_s3_1_prob)

data_s3_1_full[,"ar_1_percent_99"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.01),1,0)
data_s3_1_full[,"ar_1_percent_95"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.05),1,0)
data_s3_1_full[,"ar_1_percent_90"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.10),1,0)

data_s3_final <- data_s3_1_full[,c(identifier,"ar_1_percent_99","ar_1_percent_95","ar_1_percent_90")]

rm2(data_s3_1_full)

rm2(data_s3)


###############################################################################
##
cat("SECTION: SCREEN 4 - CONDITIONAL SERIAL CORRELATION", "\n")
##
###############################################################################

###############################################################################
cat("S4 - (1) CAR(1)", "\n")
###############################################################################



###############################################################################
##
cat("SECTION: SCREEN 5 - DATA QUALITY", "\n")
##
###############################################################################

data_s5 <- data.frame(data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)],stringsAsFactors=FALSE)


###############################################################################
cat("S5 - (1) NUM_ZERO and (6) PER_NEGATIVE", "\n")
###############################################################################

zero_neg_screen_execute <- function(data,ret_col,prob_type){
  
  # data <- x
  # ret_col <- analysis_col
  # prob_type <- "normal"
  
  data_s5_1_sum_cols <- c("sum_pos","sum_zero","sum_neg")
  data_s5_1_prob_ind_cols <- c("prob_ind_pos","prob_ind_zero","prob_ind_neg")
  data_s5_1_prob_cum_cols <- c("prob_cum_pos","prob_cum_zero","prob_cum_neg")
  
  data_s5_1_freq <- data.frame(sum_total=NA,matrix(NA, ncol=length(c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)), nrow=1, 
                                                   dimnames=list(c(), c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols))),stringsAsFactors=FALSE)
  
  data_s5 <- zero_neg_screen_ret_flags(z=data[,analysis_col],analysis_col=analysis_col,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_freq[,c("sum_total",data_s5_1_sum_cols)] <- zero_neg_screen_totals(y=data_s5,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_type <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("type","lower_tail"))), stringsAsFactors=FALSE)
  data_s5_1_type[1,] <- c("sum_pos",FALSE)
  data_s5_1_type[2,] <- c("sum_zero",FALSE)
  data_s5_1_type[3,] <- c("sum_neg",TRUE)
  
  data_s5_1_prob_ind_cum_cols <- c(paste(data_s5_1_type[,1],"ind_prop",sep="_"),paste(data_s5_1_type[,1],"cum_prop",sep="_"))
  
  data_s5_1_prob <- data.frame(prob_type=NA,total=NA,  matrix(NA, ncol=2*nrow(data_s5_1_type), nrow=2, dimnames=list(c(), data_s5_1_prob_ind_cum_cols)), 
                               stringsAsFactors=FALSE)
  data_s5_1_prob[,"prob_type"] <- c("population","normal")
  
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="population",] <- zero_neg_screen_pop_prob(data_ret=data_s5,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="normal",] <- zero_neg_screen_norm_prob(data_ret=data_s5,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  
  data_s5_1_freq[,c(data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)] <- data_s5_1_prob[data_s5_1_prob[,"prob_type"]==prob_type, data_s5_1_prob_ind_cum_cols]
  rm(data_s5_1_prob,data_s5_1_type,data_s5)
  rm(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_prob_ind_cum_cols)
  
  return(data_s5_1_freq)
  
}


data_s5_1 <- data_s5[c(data_trim_id_full_cols,analysis_col)]
data_s5_1[,analysis_col] <- round(data_s5_1[,analysis_col],digits=4)

data_s5_1_screen <- ddply(.data=data_s5_1, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5_1[data_s5_1[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  zero_and_neg_screen <- zero_neg_screen_execute(data=x,ret_col=analysis_col, prob_type="normal")
  
  return(zero_and_neg_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s5_1)


### Create Flags

data_s5_1_full <- data.frame(data_s5_1_screen,
                             per_positive_percent_99=NA,per_positive_percent_95=NA,per_positive_percent_90=NA,
                             num_zero_percent_99=NA,num_zero_percent_95=NA,num_zero_percent_90=NA,
                             per_negative_percent_99=NA,per_negative_percent_95=NA,per_negative_percent_90=NA,
                             stringsAsFactors=FALSE)
rm(data_s5_1_screen)

data_s5_1_full[,"per_positive_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.01,1,0)
data_s5_1_full[,"per_positive_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.05,1,0)
data_s5_1_full[,"per_positive_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.10,1,0)

data_s5_1_full[,"num_zero_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.01,1,0)
data_s5_1_full[,"num_zero_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.05,1,0)
data_s5_1_full[,"num_zero_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.10,1,0)

data_s5_1_full[,"per_negative_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.01,1,0)
data_s5_1_full[,"per_negative_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.05,1,0)
data_s5_1_full[,"per_negative_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.10,1,0)

data_s5_1_final <- data_s5_1_full[,c(identifier,"per_positive_percent_99","per_positive_percent_95","per_positive_percent_90",
                                     "num_zero_percent_99","num_zero_percent_95","num_zero_percent_90",
                                     "per_negative_percent_99","per_negative_percent_95","per_negative_percent_90")]

#rm2(data_s5_1_full)


###############################################################################
cat("S5 - (2) PER_REPEATS", "\n")
###############################################################################

per_repeat_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  repeat_num_data_count <- per_repeat_screen_counts(ret=data[,ret_col],ret_col=ret_col)
  
  repeat_num_data_sum <- per_repeat_screen_sum(data=repeat_num_data_count,data_col="freq")
  
  rm(repeat_num_data_count)
  
  return(repeat_num_data_sum)
  
}


data_s5_2 <- data_s5[c(data_trim_id_full_cols,analysis_col)]
data_s5_2[,analysis_col] <- round(data_s5_2[,analysis_col],digits=6)

data_s5_2_screen <- ddply(.data=data_s5_2, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5_2[data_s5_2[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
  # x <- data_s5_2[data_s5_2[,identifier]==6094, c(data_trim_id_full_cols,analysis_col)]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  per_repeat_screen <- per_repeat_screen_execute(data=x,ret_col=analysis_col)
  
  return(per_repeat_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s5_2)


### Create Flags

data_s5_2_full <- data.frame(data_s5_2_screen,cutoff_99=NA,cutoff_95=NA,cutoff_90=NA,
                             per_repeats_percent_99=NA,per_repeats_percent_95=NA,per_repeats_percent_90=NA,stringsAsFactors=FALSE)

rm(data_s5_2_screen)

data_s5_2_full[,"cutoff_99"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Per_Repeat"),"Per_0.99"]
data_s5_2_full[,"cutoff_95"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Per_Repeat"),"Per_0.95"]
data_s5_2_full[,"cutoff_90"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Per_Repeat"),"Per_0.90"]

data_s5_2_full[,"per_repeats_percent_99"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_99"],1,0)
data_s5_2_full[,"per_repeats_percent_95"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_95"],1,0)
data_s5_2_full[,"per_repeats_percent_90"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_90"],1,0)

data_s5_2_final <- data_s5_2_full[,c(identifier,"per_repeats_percent_99","per_repeats_percent_95","per_repeats_percent_90")]

rm2(data_s5_2_full)


###############################################################################
cat("S5 - (3) STRING", "\n")
###############################################################################

string_screen_execute <- function(data,ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  
  string_screen_counts <- string_screen_rle(data=data,ret_col=ret_col)
  
  string_screen_temp <- data.frame(Max_Length=NA,Total=NA,Prop_u=NA, stringsAsFactors=FALSE)
  
  string_screen_temp[,"Max_Length"] <- tail(string_screen_counts,1)
  string_screen_temp[,"Total"] <- nrow(data)
  string_screen_temp[,"Prop_u"] <- string_screen_temp[,"Max_Length"]/string_screen_temp[,"Total"] 
  
  return(string_screen_temp)
  
}


data_s5_3 <- data_s5[c(data_trim_id_full_cols,analysis_col)]
data_s5_3[,analysis_col] <- round(data_s5_3[,analysis_col],digits=6)

data_s5_3_screen <- ddply(.data=data_s5_3, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5_3[data_s5_3[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  string_screen <- string_screen_execute(data=x,ret_col=analysis_col)
  
  return(string_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s5_3)


### Create Flags

data_s5_3_full <- data.frame(data_s5_3_screen,cutoff_99=NA,cutoff_95=NA,cutoff_90=NA,
                             string_percent_99=NA,string_percent_95=NA,string_percent_90=NA,stringsAsFactors=FALSE)
rm(data_s5_3_screen)

data_s5_3_full[,"cutoff_99"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="String"),"Per_0.99"]
data_s5_3_full[,"cutoff_95"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="String"),"Per_0.95"]
data_s5_3_full[,"cutoff_90"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="String"),"Per_0.90"]

data_s5_3_full[,"string_percent_99"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_99"],1,0)
data_s5_3_full[,"string_percent_95"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_95"],1,0)
data_s5_3_full[,"string_percent_90"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_90"],1,0)

data_s5_3_final <- data_s5_3_full[,c(identifier,"string_percent_99","string_percent_95","string_percent_90")]

rm2(data_s5_3_full)


###############################################################################
cat("S5 - (4) NUM_PAIRS", "\n")
###############################################################################

num_pairs_screen_execute <- function(data,ret_col,lag_ret_col){
  
  # data <- x
  # ret_col <- analysis_col
  # lag_ret_col <- paste(analysis_col,"lag1",sep="_")
  
  num_pair_data <- num_pairs_screen_pairs(data=data,ret_col=ret_col,lag_ret_col=lag_ret_col)
  
  num_pairs_counts <- num_pairs_screen_counts(pairs=num_pair_data,pair_col="Pair")
  
  num_pairs_sum <- num_pairs_screen_sum(data=num_pairs_counts,data_col="freq")
  
  return(num_pairs_sum)
  
}


data_s5_4 <- data_s5[c(data_trim_id_full_cols,data_trim_lagged_trim_cols)]
data_s5_4[,data_trim_lagged_trim_cols] <- round(data_s5_4[,data_trim_lagged_trim_cols],digits=6)

data_s5_4_screen <- ddply(.data=data_s5_4, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5_4[data_s5_4[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  num_pairs_screen <- num_pairs_screen_execute(data=x,ret_col=analysis_col,lag_ret_col=paste(analysis_col,"lag1",sep="_"))
  
  return(num_pairs_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s5_4)


### Create Flags

data_s5_4_full <- data.frame(data_s5_4_screen,cutoff_99=NA,cutoff_95=NA,cutoff_90=NA,
                             num_pairs_percent_99=NA,num_pairs_percent_95=NA,num_pairs_percent_90=NA,stringsAsFactors=FALSE)

rm(data_s5_4_screen)

data_s5_4_full[,"cutoff_99"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Num_Pairs"),"Per_0.99"]
data_s5_4_full[,"cutoff_95"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Num_Pairs"),"Per_0.95"]
data_s5_4_full[,"cutoff_90"] <- cutoffs_comb[(cutoffs_comb[,"Type"]==cutoff_type & cutoffs_comb[,"Flag"]=="Num_Pairs"),"Per_0.90"]

data_s5_4_full[,"num_pairs_percent_99"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_99"],1,0)
data_s5_4_full[,"num_pairs_percent_95"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_95"],1,0)
data_s5_4_full[,"num_pairs_percent_90"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_90"],1,0)

data_s5_4_final <- data_s5_4_full[,c(identifier,"num_pairs_percent_99","num_pairs_percent_95","num_pairs_percent_90")]

rm2(data_s5_4_full)


###############################################################################
cat("S5 - (5) UNIFORM", "\n")
###############################################################################

uniform_screen_execute <- function(data,ret_col,graph,rounding_digit){
  
  # data <- x
  # ret_col <- analysis_col
  # graph <- FALSE
  # rounding_digit <- rounding_digit
  
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
    
    ggplot(screen_uniform_graph_temp, aes(x = fd, y = prob_actual)) + geom_bar(stat = "identity", fill = "blue") +
      geom_line(aes(x = fd, y = prob_theoretical, size = 0.1)) + geom_point(aes(x = fd, y = prob_theoretical, color = "red", size = 1)) +
      theme_bw() + scale_x_continuous(breaks = seq(min(screen_uniform_graph_temp[,"fd"]):max(screen_uniform_graph_temp[,"fd"])))
  }
  
  rm(screen_uniform_all_stats)
  
  return(screen_uniform_gof)
  
}


data_s5_5 <- data_s5[c(data_trim_id_full_cols,analysis_col)]
data_s5_5[,analysis_col] <- round(data_s5_5[,analysis_col],digits=4)

data_s5_5_screen <- ddply(.data=data_s5_5, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5_5[data_s5_5[,identifier]==0,c(data_trim_id_full_cols,analysis_col)]
  # x <- uniform_data[uniform_data[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  uniform_out <- uniform_screen_execute(data=x,ret_col=analysis_col,graph=FALSE,rounding_digit=4)
  
  return(uniform_out)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)

rm(data_s5_5)


### Create Flags

data_s5_5_screen_full <- data.frame(data_s5_5_screen,uniform_percent_99=NA, uniform_percent_95=NA, uniform_percent_90=NA, stringsAsFactors=FALSE)

rm(data_s5_5_screen)

data_s5_5_screen_full[,"uniform_percent_99"] <- ifelse(data_s5_5_screen_full[,"pval_uniform"]<=0.01,1,0)
data_s5_5_screen_full[,"uniform_percent_95"] <- ifelse(data_s5_5_screen_full[,"pval_uniform"]<=0.05,1,0)
data_s5_5_screen_full[,"uniform_percent_90"] <- ifelse(data_s5_5_screen_full[,"pval_uniform"]<=0.10,1,0)

data_s5_5_final <- data_s5_5_screen_full[,c(identifier, "uniform_percent_99", "uniform_percent_95", "uniform_percent_90")]

rm2(data_s5_5_screen_full)


###############################################################################
cat("S5 - MERGE FLAGS", "\n")
###############################################################################

data_s5_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_final0)[match("temp_id",names(data_s5_final0))] <- identifier

data_s5_final1 <- merge(data_s5_final0, data_s5_1_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final0,data_s5_1_final)

data_s5_final2 <- merge(data_s5_final1, data_s5_2_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final1,data_s5_2_final)

data_s5_final3 <- merge(data_s5_final2, data_s5_3_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final2,data_s5_3_final)

data_s5_final4 <- merge(data_s5_final3, data_s5_4_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final3,data_s5_4_final)

data_s5_final5 <- merge(data_s5_final4, data_s5_5_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final4,data_s5_5_final)

data_s5_final <- data_s5_final5

#rm2(data_s5,data_s5_final5)


###############################################################################
cat("MERGE ALL FLAGS", "\n")
###############################################################################

data_screen_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_screen_final0)[match("temp_id",names(data_screen_final0))] <- identifier

data_screen_final1 <- merge(data_screen_final0, data_s1_final, 
                            by.x=c(identifier), by.y=c(identifier), 
                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final0,data_s1_final)

data_screen_final2 <- merge(data_screen_final1, data_s2_final, 
                            by.x=c(identifier), by.y=c(identifier), 
                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final1,data_s2_final)

data_screen_final3 <- merge(data_screen_final2, data_s3_final, 
                            by.x=c(identifier), by.y=c(identifier), 
                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final2,data_s3_final)

data_screen_final4 <- merge(data_screen_final3, data_s5_final, 
                            by.x=c(identifier), by.y=c(identifier), 
                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final3,data_s5_final)

data_screen_final <- data_screen_final4

#rm(data_prescreen,data_trim_full,data_screen_final4)
gc()


###############################################################################
cat("OUTPUT FLAGS", "\n")
###############################################################################

data_screens <- data_screen_final 

write.csv(data_screens,file=paste(output_directory,"data_screens.csv",sep="\\"),na="",quote=TRUE,row.names=FALSE)

rm(data_screen_final)
rm(data_trim_id_full_cols,data_trim_lagged_trim_cols)