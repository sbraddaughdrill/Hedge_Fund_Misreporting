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
cat("SECTION: INITIAL SETUP","\n")
###############################################################################

# Clear workspace
rm(list=ls(all=TRUE))
rm(list=ls(all.names=TRUE))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE=500)

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors=FALSE)

# Default maxprint option
options(max.print=500)
# options(max.print=99999)

# Memory limit
#memory.limit(size=8183)

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
  
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)    
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)  
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)   
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else {
  
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)

source(file=paste(function_directory,"functions_misreporting_screens.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","ggplot2","limma","MASS","plyr","quantmod",
                       "reshape2","stringr","zoo")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=FALSE,USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SETUP","\n")
###############################################################################


identifier <- "Fund_ID"

#analysis_col <- "mktadjret"
#analysis_col <- "Monthly_Ret"
analysis_col <- "Monthly_Ret_org"

#strat_col <- "Main.Investment.Strategy"
strat_col <- "Primary_Investment_Strategy_combcol"

beg_year <- 2007
end_year <- 2013

cutoff_lengths_all <- c(24,36,48,60,120)

cutoff_num <- 24
#cutoff_num <- 36
#cutoff_num <- 48
#cutoff_num <- 60


###############################################################################
cat("IMPORT CUTOFF DATA","\n")
###############################################################################

cutoffs_comb_full <- ldply(.data=cutoff_lengths_all,.fun=function(x,path){
  
  out <- data.frame(Num=x,Type=paste(x,"Month History",sep="-"),Flag_ID=NA,Flag_Sub_ID=NA,
                    read.csv(file=paste(path,paste("Cutoff_Simulation_",sprintf("%03d",x),".csv",sep=""),sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),
                    stringsAsFactors=FALSE)
  
  #out[,"Flag_ID"] <- as.numeric(factor(out[,"Flag"]))
  out[,"Flag_ID"] <- as.numeric(cumsum(!duplicated(out[,"Flag"])))
  
  out <- ddply(.data=out,.variables="Flag_ID",.fun=function(y){y[,"Flag_Sub_ID"] <- seq(1,nrow(y)) ; return(y)})
  return(out)
  
},path=paste(output_directory,"cutoff_simulation",sep="\\"),.progress="none")


cutoffs_comb <- ddply(.data=cutoffs_comb_full,.variables=c("Num","Flag_ID"),.fun=function(x){return(head(x,1))})

rm(cutoff_lengths_all,cutoffs_comb_full)

cutoffs_comb_trim <- cutoffs_comb[(cutoffs_comb[,"Num"]==cutoff_num),]

rm(cutoffs_comb)


###############################################################################
cat("IMPORT RETURN DATA","\n")
###############################################################################

data_trim_lag_count <- 1

data_trim_cols_id <- c(identifier,"yr","month","yr_month")

# data_trim_cols_unlagged <- c(analysis_col,"mktadjret")
# #data_trim_cols_unlagged_trim <- data_trim_cols_unlagged[grepl(analysis_col,data_trim_cols_unlagged)]
# 
# data_trim_cols_lagged <- sort(unlist(lapply(data_trim_cols_unlagged,function(x,lag_length){paste(x,"_lag",seq(1,lag_length),sep="")},lag_length=data_trim_lag_count)))
# data_trim_cols_lagged_trim <- data_trim_cols_lagged[grepl(analysis_col,data_trim_cols_lagged)]
# 
# data_trim_cols_import <- c(data_trim_cols_id,strat_col,analysis_col,data_trim_cols_lagged_trim)

data_trim_cols_import <- c(data_trim_cols_id,strat_col,analysis_col)

data_prescreen <- read.columns(file=paste(output_directory,"data_prescreen.csv",sep="\\"),
                               required.col=data_trim_cols_import,
                               sep=",",na.strings="NA",stringsAsFactors=FALSE)

# Clean Strategy
data_prescreen[,strat_col] <- gsub(" {2,}"," ",data_prescreen[,strat_col],perl=TRUE)
data_prescreen[,strat_col] <- gsub("^\\s+|\\s+$","",data_prescreen[,strat_col],perl=TRUE)
#data_prescreen[,strat_col] <- gsub(" ","",data_prescreen[,strat_col],perl=TRUE)
data_prescreen[,strat_col] <- ifelse(data_prescreen[,strat_col]=="",NA,data_prescreen[,strat_col])

data_prescreen <- data_prescreen[order(data_prescreen[,identifier],data_prescreen[,"yr"],data_prescreen[,"month"]),]
row.names(data_prescreen) <- seq(nrow(data_prescreen))

data_prescreen <- ddply(.data=data_prescreen,.variables=identifier,.fun=function(x){
  
  # x <- data_prescreen[(data_prescreen[,identifier]==5002),]
  
  out <- data.frame(x,temp_lag=NA,stringsAsFactors=FALSE)
  out[,"temp_lag"] <- c(NA,out[1:nrow(x)-1,analysis_col])
  return(out)
},.progress="none")

data_trim_cols_lagged_trim <- paste(analysis_col,"lag1",sep="_")

colnames(data_prescreen)[match("temp_lag",colnames(data_prescreen))] <- data_trim_cols_lagged_trim


###############################################################################
cat("TRIM DATA","\n")
###############################################################################

## Remove any duplicate observations
data_trim0 <- unique(data_prescreen)

rm2(data_prescreen)


## Remove leading/trailing NAs but make sure middle NAs stay

data_trim1 <- ddply(.data=data_trim0,.variables=c(identifier),.fun=function(x){
  
  # x <- data_trim0[(data_trim0[,identifier]==5019),]
  # x <- data_trim0[(data_trim0[,identifier]==5019 & data_trim0[,strat_col]=="BOTTOM UP"),] 
  # x <- data_trim0[(data_trim0[,identifier]==5021),]
  # x <- data_trim0[(data_trim0[,identifier]==5021 & data_trim0[,strat_col]=="BOTTOM UP"),]
  # x <- data_trim0[(data_trim0[,identifier]==5021 & data_trim0[,strat_col]=="TOP DOWN"),]
  # x <- data_trim0[(data_trim0[,identifier]==15373),]
  # x <- data_trim0[(data_trim0[,identifier]==23590),]
  
  ###TEST
  # x <- x[x[,"yr_month"]!="2010_12",]
  ###
  
  yr_month_comb <- data.frame(temp_id=NA,expand.grid(yr=seq(min(x[,"yr"],na.rm=T)-1,max(x[,"yr"],na.rm=T)+1,1),month=seq(1,12,1)),yr_month=NA,stringsAsFactors=FALSE)
  
  yr_month_comb <- yr_month_comb[order(yr_month_comb[,"yr"],yr_month_comb[,"month"]),]
  row.names(yr_month_comb) <- seq(nrow(yr_month_comb))
  
  colnames(yr_month_comb)[match("temp_id",colnames(yr_month_comb))] <- identifier
  
  yr_month_comb[,identifier] <- unique(x[,identifier])
  yr_month_comb[,"yr_month"] <- paste(yr_month_comb[,"yr"],sprintf("%02d",yr_month_comb[,"month"]),sep="_")
  
  x_trim1 <- x[!is.na(x[,analysis_col]),]
  #x_trim1 <- x_trim1[!is.na(x_trim1[,data_trim_cols_lagged_trim]),]
  
  yr_month_comb_trim <- yr_month_comb
  yr_month_comb_trim[,"yr_month"] <- ifelse(yr_month_comb_trim[,"yr"]<min(x_trim1[,"yr"],na.rm=T),NA,yr_month_comb_trim[,"yr_month"])
  yr_month_comb_trim[,"yr_month"] <- ifelse((yr_month_comb_trim[,"yr"]==min(x_trim1[,"yr"],na.rm=T) 
                                             & yr_month_comb_trim[,"month"] < min(x_trim1[x_trim1[,"yr"]==min(x_trim1[,"yr"],na.rm=T),"month"],na.rm=T)),
                                            NA,yr_month_comb_trim[,"yr_month"])
  yr_month_comb_trim[,"yr_month"] <- ifelse(yr_month_comb_trim[,"yr"]>max(x_trim1[,"yr"],na.rm=T),NA,yr_month_comb_trim[,"yr_month"])
  yr_month_comb_trim[,"yr_month"] <- ifelse((yr_month_comb_trim[,"yr"]==max(x_trim1[,"yr"],na.rm=T) 
                                             & yr_month_comb_trim[,"month"] > max(x_trim1[x_trim1[,"yr"]==max(x_trim1[,"yr"],na.rm=T),"month"],na.rm=T)),
                                            NA,yr_month_comb_trim[,"yr_month"])
  
  yr_month_comb_trim <- yr_month_comb_trim[(!is.na(yr_month_comb_trim[,"yr_month"])),]
  
  rm(yr_month_comb)
  
  x_no_strat_expand <- merge(yr_month_comb_trim,unique(x_trim1[,!(colnames(x_trim1) %in% strat_col)]),
                             by.x=c(identifier,"yr","month","yr_month"),by.y=c(identifier,"yr","month","yr_month"),
                             all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
  
  x_no_strat_expand <- x_no_strat_expand[,c(identifier,colnames(x_no_strat_expand)[!(colnames(x_no_strat_expand) %in% identifier)])]
  x_no_strat_expand <- x_no_strat_expand[order(x_no_strat_expand[,"yr"],x_no_strat_expand[,"month"]),]
  row.names(x_no_strat_expand) <- seq(nrow(x_no_strat_expand))
  
  # Fix Lags
  lagmatrix <- function(x,max.lag){embed(c(rep(NA,max.lag),x),max.lag+1)}
  lag_df <- ddply(x_no_strat_expand,identifier,.fun=function(x,maxlag=data_trim_lag_count,col=analysis_col){lagmatrix(x[,col],maxlag)})
  colnames(lag_df) <- c(identifier,analysis_col,paste(analysis_col,"_lag",seq(1,data_trim_lag_count),sep=""))
  
  x_no_strat_expand[,data_trim_cols_lagged_trim] <- lag_df[,(ncol(lag_df)-data_trim_lag_count+1):ncol(lag_df)]
  
  rm(lag_df) 
  
  x_strat <- data.frame(unique(x_trim1[,c(identifier,"yr","month","yr_month",strat_col)]),
                        #yr_month_lag_temp=NA,yr_month_lead_temp=NA,
                        strat_id=NA,strat_sub_id=NA,strat_sub_id_na=NA,
                        stringsAsFactors=FALSE)
  
  x_strat <- x_strat[order(x_strat[,strat_col],x_strat[,"yr"],x_strat[,"month"]),]
  row.names(x_strat) <- seq(nrow(x_strat))
  
  x_strat[,"strat_id"] <- as.numeric(cumsum(!duplicated(x_strat[,strat_col])))
  
  rm(x_trim1)
  
  x_strat2 <- ddply(.data=x_strat,.variables="strat_id",.fun=function(y,yr_month_expand){
    
    y_expand <- merge(yr_month_expand,y,
                      by.x=c(identifier,"yr","month","yr_month"),by.y=c(identifier,"yr","month","yr_month"),
                      all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
    
    y_expand <- y_expand[order(y_expand[,"yr"],y_expand[,"month"]),]
    row.names(y_expand) <- seq(nrow(y_expand))
    
    y_expand[,"strat_sub_id_na"] <- ifelse(is.na(y_expand[,"strat_id"]),0,1)
    y_expand[,"strat_sub_id"] <- c(0,y_expand[1:nrow(y_expand)-1,"strat_sub_id_na"])
    y_expand[,"strat_sub_id"] <- ifelse(y_expand[,"strat_sub_id_na"]==y_expand[,"strat_sub_id"],0,1)
    y_expand[,"strat_sub_id"] <- ifelse(is.na(y_expand[,strat_col]),0,y_expand[,"strat_sub_id"])
    y_expand[,"strat_sub_id"] <- cumsum(y_expand[,"strat_sub_id"])
    y_expand[,"strat_sub_id"] <- ifelse(is.na(y_expand[,strat_col]),NA,y_expand[,"strat_sub_id"])
    y_expand <- y_expand[!is.na(y_expand[,strat_col]),]
    
    return(y_expand)
    
  },yr_month_expand=yr_month_comb_trim,.progress="none")
  
  rm(x_strat,yr_month_comb_trim)
  
  #x_strat2 <- x_strat2[order(x_strat2[,"yr"],x_strat2[,"month"]),]
  #row.names(x_strat2) <- seq(nrow(x_strat2))
  
  x_full <- merge(x_no_strat_expand,x_strat2[,!(colnames(x_strat2) %in% c("strat_id","strat_sub_id","strat_sub_id_na"))],
                  by.x=c(identifier,"yr","month","yr_month"),by.y=c(identifier,"yr","month","yr_month"),
                  all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
  
  x_full <- x_full[order(x_full[,"yr"],x_full[,"month"]),]
  row.names(x_full) <- seq(nrow(x_full))
  
  rm(x_no_strat_expand,x_strat2)
  
  return(x_full)
  
},.progress="text")

rm2(data_trim0)

data_trim1 <- data_trim1[,c(identifier,colnames(data_trim1)[!(colnames(data_trim1) %in% identifier)])]
data_trim1 <- data_trim1[order(data_trim1[,identifier],data_trim1[,"yr"],data_trim1[,"month"]),]
row.names(data_trim1) <- seq(nrow(data_trim1))

## Keep only data between beg_year & end_year
data_trim2 <- data_trim1[(data_trim1[,"yr"]>=beg_year & data_trim1[,"yr"]<=end_year),]

rm2(data_trim1)



# ### TEST #######
# firms_ret_na <- data_trim4[is.na(data_trim4[,analysis_col]),c(identifier)]
# 
# firms_strat_mult0 <- count(unique(data_trim4[!is.na(data_trim4[,strat_col]),c(identifier,strat_col)]),c(identifier))
# firms_strat_mult <- firms_strat_mult0[firms_strat_mult0[,"freq"]>1,c(identifier)]
# rm(firms_strat_mult0)
# 
# firm_bad_both <- intersect(firms_ret_na,firms_strat_mult)
# ### END TEST ###


### Make sure funds have atleast X months of returns

data_trim2_firm_counts0 <- data_trim2[!is.na(data_trim2[,c(analysis_col)]),]
data_trim2_firm_counts1 <- count(data_trim2_firm_counts0,c(identifier))
#data_trim2_firm_counts1 <- count(data_trim2_firm_counts0,c(identifier,strat_col))

data_trim2_firm_keep <- data_trim2_firm_counts1[data_trim2_firm_counts1[,"freq"]>=cutoff_num,]
data_trim2_firm_keep <- data_trim2_firm_keep[!is.na(data_trim2_firm_keep[,c(identifier)]),]
#data_trim2_firm_keep <- data_trim2_firm_keep[!is.na(data_trim2_firm_keep[,c(strat_col)]),]
row.names(data_trim2_firm_keep) <- seq(nrow(data_trim2_firm_keep))

data_trim2_firm_drop <- data_trim2_firm_counts1[data_trim2_firm_counts1[,"freq"]<cutoff_num,]
data_trim2_firm_drop <- data_trim2_firm_drop[!is.na(data_trim2_firm_drop[,c(identifier)]),]
#data_trim2_firm_drop <- data_trim2_firm_drop[!is.na(data_trim2_firm_drop[,c(strat_col)]),]
row.names(data_trim2_firm_drop) <- seq(nrow(data_trim2_firm_drop))

data_trim3 <- data_trim2[(data_trim2[,c(identifier)] %in% data_trim2_firm_keep[,c(identifier)]),]
#data_trim3 <- data_trim3[,!(colnames(data_trim3) %in% strat_col)]

row.names(data_trim3) <- seq(nrow(data_trim3))

rm(data_trim2,data_trim2_firm_keep,data_trim2_firm_drop)
rm(data_trim2_firm_counts0,data_trim2_firm_counts1)


### Make sure funds have atleast X months of consecutive returns

data_trim4 <- ddply(.data=data_trim3,.variables=identifier,.fun=function(x,analysis_col){
  
  # x <- data_trim3[data_trim3[,identifier]==5002,]
  # x <- data_trim3[data_trim3[,identifier]==5019,]
  # x <- data_trim3[data_trim3[,identifier]==5021,]
  # x <- data_trim3[data_trim3[,identifier]==5025,]
  # x <- data_trim3[data_trim3[,identifier]==5078,]
  # x <- data_trim3[data_trim3[,identifier]==5528,]
  # x <- data_trim3[data_trim3[,identifier]==15373,]
  # x <- data_trim3[data_trim3[,identifier]==23590,]
  
  #cat(unique(x[,identifier]),"\n")
  
  firm0 <- data.frame(x,NA_flag=NA,NA_cum_sum=NA,seq_flag=NA,seq_id=NA,
                      seq_freq_consecutive=NA,ret_freq_overall=NA,ret_freq_consecutive_max=NA,stringsAsFactors=FALSE)
  
  # firm0[,"NonNA_flag"] <- 1
  # firm0[,"NonNA_flag"] <- ifelse(is.na(firm0[,analysis_col]),0,firm0[,"NonNA_flag"])
  # firm0[,"NonNA_flag"] <- ifelse(is.na(firm0[,data_trim_cols_lagged_trim]),0,firm0[,"NonNA_flag"])
  # firm0[,"NonNA_cum_sum"] <- cumsum(firm0[,"NonNA_flag"])
  
  firm0[,"NA_flag"] <- 0
  firm0[,"NA_flag"] <- ifelse(is.na(firm0[,analysis_col]),1,firm0[,"NA_flag"])
  #firm0[,"NA_flag"] <- ifelse(is.na(firm0[,data_trim_cols_lagged_trim]),1,firm0[,"NA_flag"])
  firm0[,"NA_cum_sum"] <- cumsum(firm0[,"NA_flag"])
  
  firm1 <- ddply(.data=firm0,.variables=c(identifier,"NA_cum_sum"),.fun=function(z){
    out <- z
    #out <- out[out[,"NA_flag"]!=1,]
    return(data.frame(freq=nrow(out)))
  })
  
  #firm1_mult <- firm1[firm1[,"freq"]>1,]
  firm1_mult <- firm1
  colnames(firm1_mult)[match("freq",names(firm1_mult))] <- "seq_flag"
  firm1_mult[,"seq_flag"] <- seq(1,nrow(firm1_mult))
  
  rm(firm1)
  
  firm2 <- merge(firm0[,!(colnames(firm0) %in% "seq_flag")],firm1_mult,
                 by.x=c(identifier,"NA_cum_sum"),by.y=c(identifier,"NA_cum_sum"),
                 all.x=TRUE,all.y=FALSE,sort=TRUE,suffixes=c(".x",".y"))
  firm2 <- firm2[order(firm2[,identifier],firm2[,"yr_month"]),]
  row.names(firm2) <- seq(nrow(firm2))
  
  firm2 <- firm2[,colnames(firm0)]
  
  rm(firm0,firm1_mult)
  
  firm3 <- ddply(.data=firm2,.variables="seq_flag",.fun=function(y){
    
    # y <- firm2[firm2[,"seq_flag"]==1,]
    
    y[,"seq_flag"] <- ifelse(is.na(y[,"seq_flag"]),0,y[,"seq_flag"])
    y[,"seq_id"] <- 1
    y[,"seq_id"] <- ifelse(is.na(y[,analysis_col]),0,y[,"seq_id"])
    y[,"seq_id"] <- ifelse(is.na(y[,data_trim_cols_lagged_trim]),0,y[,"seq_id"])
    y[,"seq_id"] <- cumsum(y[,"seq_id"])
    y[,"seq_id"] <- ifelse(is.na(y[,analysis_col]),NA,y[,"seq_id"])
    y[,"seq_id"] <- ifelse(is.na(y[,data_trim_cols_lagged_trim]),NA,y[,"seq_id"])
    
    #y[,"seq_freq_consecutive"] <- tryCatch({max(y[,"seq_id"],na.rm=TRUE)},
    #                                       warning=function(w,x){cat("\n",unique(x[,identifier]),"\n")},
    #                                       error=function(e) {cat("\n",unique(x[,identifier]),"\n")})
    
    y[,"seq_freq_consecutive"] <- suppressWarnings(max(y[,"seq_id"],na.rm=TRUE))
    y[,"seq_freq_consecutive"] <- ifelse(is.infinite(y[,"seq_freq_consecutive"]),NA,y[,"seq_freq_consecutive"])
    
    return(y)
  },.progress="none")
  
  rm(firm2)
  
  firm3 <- firm3[order(firm3[,identifier],firm3[,"yr_month"]),]
  row.names(firm3) <- seq(nrow(firm3))
  
  firm3[,"ret_freq_overall"] <- sum(unique(firm3[,c("seq_flag","seq_freq_consecutive")])[,"seq_freq_consecutive"],na.rm=TRUE)
  firm3[,"ret_freq_consecutive_max"] <- suppressWarnings(max(firm3[,"seq_freq_consecutive"],na.rm=TRUE))
  
  return(firm3[,!(colnames(firm3) %in% c("NA_flag","NA_cum_sum"))])
  
},analysis_col=analysis_col,.progress="text")

rm2(data_trim3)

#  a <- data_trim4[data_trim4[,identifier]==5019,]
#  a <- data_trim4[data_trim4[,identifier]==5021,]
#  a <- data_trim4[data_trim4[,identifier]==5025,]
#  a <- data_trim4[data_trim4[,identifier]==5078,]
#  a <- data_trim4[data_trim4[,identifier]==5445,]
#  a <- data_trim4[data_trim4[,identifier]==5528,]
#  a <- data_trim4[data_trim4[,identifier]==15373,]
#  a <- data_trim4[data_trim4[,identifier]==18309,]
#  a <- data_trim4[data_trim4[,identifier]==23590,]

data_trim4_full_seq_flag_count <- unique(data_trim4[data_trim4[,identifier]!=0,
                                                    c(identifier,"seq_flag","seq_freq_consecutive","ret_freq_overall","ret_freq_consecutive_max")])
data_trim4_full_seq_flag_count <- data_trim4_full_seq_flag_count[order(data_trim4_full_seq_flag_count[,identifier],data_trim4_full_seq_flag_count[,"seq_flag"]),]
row.names(data_trim4_full_seq_flag_count) <- seq(nrow(data_trim4_full_seq_flag_count))


data_trim5 <- data_trim4[data_trim4[,"seq_freq_consecutive"]>=cutoff_num,]
data_trim5 <- data_trim5[!is.na(data_trim5[,c(identifier)]),]
row.names(data_trim5) <- seq(nrow(data_trim5))

#  b <- data_trim5[data_trim5[,identifier]==5019,]
#  b <- data_trim5[data_trim5[,identifier]==5021,]
#  b <- data_trim5[data_trim5[,identifier]==5025,]
#  b <- data_trim5[data_trim5[,identifier]==5078,]
#  b <- data_trim5[data_trim5[,identifier]==5445,]
#  b <- data_trim5[data_trim5[,identifier]==5528,]
#  b <- data_trim5[data_trim5[,identifier]==15373,]
#  b <- data_trim5[data_trim5[,identifier]==23590,]

data_trim5_full_seq_flag_count <- unique(data_trim4[data_trim4[,identifier]!=0,
                                                    c(identifier,"seq_flag","seq_freq_consecutive","ret_freq_overall","ret_freq_consecutive_max")])
data_trim5_full_seq_flag_count <- data_trim5_full_seq_flag_count[order(data_trim5_full_seq_flag_count[,identifier],data_trim5_full_seq_flag_count[,"seq_flag"]),]
row.names(data_trim5_full_seq_flag_count) <- seq(nrow(data_trim5_full_seq_flag_count))

data_trim <- data_trim5

rm2(data_trim4,data_trim4_full_seq_flag_count)
rm2(data_trim5,data_trim5_full_seq_flag_count)


###############################################################################
cat("FINALIZE RET DATA","\n")
###############################################################################

### Combine Overall Data

#data_trim_overall <- data_trim
#data_trim_overall[,identifier] <- 0
#data_trim_full <- rbindlist(list(data_trim_overall,data_trim))
#data_trim_full <- as.data.frame(data_trim_full,stringsAsFactors=FALSE)
#rm(data_trim_overall,data_trim)

data_trim_full <- data_trim
rm2(data_trim)



# a0a <- round(data_prescreen[,data_trim_unlagged_cols],digits=4)
# colnames(a0a) <- paste(data_trim_unlagged_cols,"round",sep="_")
# 
# a0b <- round2(data_prescreen[,data_trim_unlagged_cols],digits=4)
# colnames(a0b) <- paste(data_trim_unlagged_cols,"round2",sep="_")
# 
# a1_trim0 <- cbind(a0a,a0b)
# a1_trim1 <- data.frame(a1_trim0[,colnames(a1_trim0) %in% c(paste(data_trim_unlagged_cols,"round",sep="_"),paste(data_trim_unlagged_cols,"round2",sep="_"))],
#                        matrix(NA,ncol=length(data_trim_unlagged_cols),nrow=1,dimnames=list(c(),paste(data_trim_unlagged_cols,"same_flag",sep="_"))),
#                        stringsAsFactors=FALSE)
# a1_trim1 <- a1_trim1[,sort(colnames(a1_trim1))]
# a1_trim2 <- a1_trim1[rowSums(is.na(a1_trim1[,1:ncol(a1_trim1)]))<ncol(a1_trim1),]
# 
# a1_trim2[,paste(data_trim_unlagged_cols[1],"round",sep="_")] <- as.character(a1_trim2[,paste(data_trim_unlagged_cols[1],"round",sep="_")])
# a1_trim2[,paste(data_trim_unlagged_cols[1],"round2",sep="_")] <- as.character(a1_trim2[,paste(data_trim_unlagged_cols[1],"round2",sep="_")])
# a1_trim2[,paste(data_trim_unlagged_cols[2],"round",sep="_")] <- as.character(a1_trim2[,paste(data_trim_unlagged_cols[2],"round",sep="_")])
# a1_trim2[,paste(data_trim_unlagged_cols[2],"round2",sep="_")] <- as.character(a1_trim2[,paste(data_trim_unlagged_cols[2],"round2",sep="_")])
# 
# a1_trim2[,paste(data_trim_unlagged_cols[1],"same_flag",sep="_")] <- ifelse((is.na(a1_trim2[,paste(data_trim_unlagged_cols[1],"round",sep="_")]) | is.na(a1_trim2[,paste(data_trim_unlagged_cols[1],"round2",sep="_")])),NA,
#                                                                            ifelse(a1_trim2[,paste(data_trim_unlagged_cols[1],"round",sep="_")]==a1_trim2[,paste(data_trim_unlagged_cols[1],"round2",sep="_")],1,0))
# 
# a1_trim2[,paste(data_trim_unlagged_cols[2],"same_flag",sep="_")] <- ifelse((is.na(a1_trim2[,paste(data_trim_unlagged_cols[2],"round",sep="_")]) | is.na(a1_trim2[,paste(data_trim_unlagged_cols[2],"round2",sep="_")])),NA,
#                                                                            ifelse(a1_trim2[,paste(data_trim_unlagged_cols[2],"round",sep="_")]==a1_trim2[,paste(data_trim_unlagged_cols[2],"round2",sep="_")],1,0))
# 
# a1_trim3 <- a1_trim2[!(is.na(a1_trim2[,paste(data_trim_unlagged_cols[1],"same_flag",sep="_")]) | is.na(a1_trim2[,paste(data_trim_unlagged_cols[2],"same_flag",sep="_")])),]
# a1_trim3 <- a1_trim3[(a1_trim3[,paste(data_trim_unlagged_cols[1],"same_flag",sep="_")]==0 | a1_trim3[,paste(data_trim_unlagged_cols[2],"same_flag",sep="_")]==0),]
# 
# rm(a0a,a0b,a1_trim0,a1_trim1,a1_trim2,a1_trim3)


###############################################################################
cat("SECTION: SCREENS","\n")
###############################################################################

###############################################################################
##
cat("SECTION: SCREEN 1 - DISCONTINUITY AT ZERO","\n")
##
###############################################################################

#data_s1 <- data_trim_full[,c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")]
data_s1 <- data_trim_full[,c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
data_s1 <- data_s1[data_s1[,identifier]!=0,]

###############################################################################
cat("S1 - (1) KINK","\n")
###############################################################################

# data_s1_1 <- data_s1[,c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")]
# #data_s1_1[,c(analysis_col,data_trim_cols_lagged_trim)] <- round(data_s1_1[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)
# data_s1_1[,c(analysis_col,data_trim_cols_lagged_trim)] <- round2(data_s1_1[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)

data_s1_1 <- data_s1[,c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
#data_s1_1[,c(analysis_col)] <- round(data_s1_1[,c(analysis_col)],digits=4)
data_s1_1[,c(analysis_col)] <- round2(data_s1_1[,c(analysis_col)],digits=4)

data_s1_bins <- ddply(.data=data_s1_1,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s1_1[(data_s1_1[,identifier]==5002 & data_s1_1[,"seq_flag"]==1),]  
  # x <- data_s1_1[(data_s1_1[,identifier]==5021 & data_s1_1[,"seq_flag"]==1),]
  # x <- data_s1_1[(data_s1_1[,identifier]==5445 & data_s1_1[,"seq_flag"]==36),]
  # x <- data_s1_1[(data_s1_1[,identifier]==32420 & data_s1_1[,"seq_flag"]==1),]
  # x <- data_s1_1[(data_s1_1[,identifier]==38049 & data_s1_1[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c("bin_count1","bin_count2","bin_count3","outside_bin_avg","kink_ratio")
  
  if (nrow(x_trim)==0)
  {
    kink_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    #kink_screen_final[,c("id")] <- seq(1,nrow(kink_screen_final),1)
    kink_screen_final[,c("id")] <- row.names(x)
  } else {
   
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    z[,"id"] <- as.integer(row.names(z))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      kink_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    } else
    {
      kink_screen <- rollapply(data=z,width=width,function(w,ret_col){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=48),]
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
        
        #return(kink_screen_execute(data=as.data.frame(w),ret_col=ret_col))
        return(kink_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col))
        
      },ret_col=analysis_col,by.column=FALSE,fill=NA,partial=FALSE,align="right") 
      
      kink_screen_df  <- data.frame(kink_screen,stringsAsFactors=FALSE)
      kink_screen_df2  <- data.frame(id=z[,"id"],kink_screen_df,stringsAsFactors=FALSE)
      kink_screen_df2[,"id"] <- as.integer(kink_screen_df2[,"id"])
      row.names(kink_screen_df2) <- seq(nrow(kink_screen_df2))
      kink_screen_final <- merge(x2,kink_screen_df2,
                                 by.x=c("id"),by.y=c("id"),
                                 all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      kink_screen_final <- kink_screen_final[order(kink_screen_final[,"id"]),] 
      row.names(kink_screen_final) <- seq(nrow(kink_screen_final))
      
      rm(kink_screen,kink_screen_df,kink_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(kink_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s1_1)


### Create Flags

kink_percentiles <- c("kink_percent_99","kink_percent_95","kink_percent_90","kink_percent_75","kink_percent_66","kink_percent_50")

data_s1_bins_full <- data.frame(data_s1_bins,matrix(NA,ncol=length(kink_percentiles),nrow=1,dimnames=list(c(),kink_percentiles)),stringsAsFactors=FALSE)

#data_s1_bins_full[,"kink_percent_99"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.99,1,0)
#data_s1_bins_full[,"kink_percent_95"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.95,1,0)
#data_s1_bins_full[,"kink_percent_90"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.90,1,0)
#data_s1_bins_full[,"kink_percent_75"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.75,1,0)
#data_s1_bins_full[,"kink_percent_66"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.66,1,0)
#data_s1_bins_full[,"kink_percent_50"] <- ifelse(data_s1_bins_full[,"kink_ratio"]>=0.50,1,0)

#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.99,1,0),na.rm=T)
#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.95,1,0),na.rm=T)
#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.90,1,0),na.rm=T)
#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.75,1,0),na.rm=T)
#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.66,1,0),na.rm=T)
#  mean(ifelse(data_s1_bins_full[,"kink_ratio"]>=0.50,1,0),na.rm=T)

data_s1_bins_type <- 1 #  one-sided t-test
#data_s1_bins_type <- 2 #  two-sided t_test

data_s1_bins_width <- cutoff_num
#data_s1_bins_width <- 9999999

data_s1_bins_full[,"kink_percent_99"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.010000/data_s1_bins_type),data_s1_bins_width-2)),1,0)
data_s1_bins_full[,"kink_percent_95"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.050000/data_s1_bins_type),data_s1_bins_width-2)),1,0)
data_s1_bins_full[,"kink_percent_90"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.100000/data_s1_bins_type),data_s1_bins_width-2)),1,0)
data_s1_bins_full[,"kink_percent_75"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.250000/data_s1_bins_type),data_s1_bins_width-2)),1,0)
data_s1_bins_full[,"kink_percent_66"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.333333/data_s1_bins_type),data_s1_bins_width-2)),1,0)
data_s1_bins_full[,"kink_percent_50"] <- ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.500000/data_s1_bins_type),data_s1_bins_width-2)),1,0)

#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.010000/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)
#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.050000/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)
#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.100000/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)
#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.250000/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)
#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.333333/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)
#  mean(ifelse((data_s1_bins_full[,"diff"]<0 & abs(data_s1_bins_full[,"t_stat"])>=qt(1-(0.500000/data_s1_bins_type),data_s1_bins_width-2)),1,0),na.rm=T)

#Trigger if fails at any point in time series
data_s1_bins_full_any <- misreport_cumm_any(data=data_s1_bins_full,id_col=identifier,pct_cols=kink_percentiles,suffix="any")

#Trigger if fails more than half
data_s1_bins_full_avg <- misreport_cumm_avg(data=data_s1_bins_full,id_col=identifier,pct_cols=kink_percentiles,suffix="avg")


#Merge flags
data_s1_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s1_final0)[match("temp_id",names(data_s1_final0))] <- identifier

data_s1_final1 <- merge(data_s1_final0,data_s1_bins_full_any,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s1_final2 <- merge(data_s1_final1,data_s1_bins_full_avg,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s1_final <- data_s1_final2

#rm2(data_s1_bins,data_s1_bins_full)
rm2(data_s1_final0,data_s1_final1,data_s1_final2)
rm2(data_s1_bins_full_any,data_s1_bins_full_avg)
rm2(data_s1_bins_type,data_s1_bins_width)

rm2(data_s1)

###############################################################################
##
cat("SECTION: SCREEN 2 - LOW CORRELATION WITH OTHER ASSETS","\n")
##
###############################################################################

# data_s2_temp <- unique(data.frame(data_trim_full[,c(data_trim_cols_id,analysis_col)],stringsAsFactors=FALSE))
# data_s2_style_temp1 <- unique(data_prescreen[,c(identifier,"yr","month",strat_col)])
# data_s2_merge <- merge(data_s2_temp,data_s2_style_temp1,
#                        by.x=c(identifier,"yr","month"),by.y=c(identifier,"yr","month"),
#                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
# data_s2_merge <- unique(data_s2_merge)
# rm2(data_s2_temp,data_s2_style_temp1)

data_s2_merge <- data_trim_full[,c(data_trim_cols_id,analysis_col,strat_col,"seq_flag","seq_id")]

data_s2_merge[,strat_col] <- ifelse(data_s2_merge[,identifier]==0,"ALL",data_s2_merge[,strat_col])
data_s2_merge <- data_s2_merge[order(data_s2_merge[,identifier],data_s2_merge[,"yr_month"]),]
row.names(data_s2_merge) <- seq(nrow(data_s2_merge))

data_s2_merge_trim0 <- data_s2_merge
data_s2_merge_trim1 <- data_s2_merge_trim0[data_s2_merge_trim0[,identifier]!=0,]

#data_s2_merge_trim1_no_strat <- unique(data_s2_merge_trim1[!is.na(data_s2_merge_trim1[,strat_col]),identifier])
#data_s2_merge_trim2 <- data_s2_merge_trim1[!is.na(data_s2_merge_trim1[,strat_col]),]

data_s2_merge_trim2 <- data_s2_merge_trim1
data_s2_merge_trim2[,strat_col] <- ifelse(is.na(data_s2_merge_trim1[,strat_col]),"MISSING",data_s2_merge_trim2[,strat_col])

data_s2 <- data_s2_merge_trim2

rm2(data_s2_merge)
rm2(data_s2_merge_trim0,data_s2_merge_trim1,data_s2_merge_trim2)


###############################################################################
cat("S2 - (1) MAXRSQ ","\n")
###############################################################################


###############################################################################
cat("S2 - (2) SWITCHRSQ","\n")
###############################################################################


###############################################################################
cat("S2 - (3) INDEXRSQ","\n")
###############################################################################

data_s2_3 <- data.frame(style_id=NA,data_s2,stringsAsFactors=FALSE)
data_s2_3[,"style_id"] <- as.numeric(as.factor(data_s2_3[,strat_col]))

#data_s2_3[,analysis_col] <- round(data_s2_3[,analysis_col],digits=4)
data_s2_3[,analysis_col] <- round2(data_s2_3[,analysis_col],digits=4)


data_s2_3_prob <- ddply(.data=data_s2_3,.variables="style_id",.fun=function(y,style_col,analysis_col,id_col,date_col,width){
  
  # y <- data_s2_3[data_s2_3[,strat_col]=="BOTTOM UP",]
  # y <- data_s2_3[data_s2_3[,strat_col]=="LONG SHORT EQUITIES",] 
  # y <- data_s2_3[data_s2_3[,strat_col]=="MISSING",] 
  # y <- data_s2_3[data_s2_3[,strat_col]=="MACRO",]
  # style_col <- strat_col
  # analysis_col <- analysis_col
  # id_col <- identifier
  # date_col <- "yr_month"
  # width <- cutoff_num

  cat("\n","Style",unique(y[,style_col]),"\n")

  data_s2_3_prob_sub <- ddply(.data=y[,c(id_col,date_col,analysis_col,"seq_flag","seq_id")],.variables=c(identifier,"seq_flag"),.fun=function(x,data_style,analysis_col,id_col,width){
    
    # x <- y[(y[,identifier]==5002 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]  
    # x <- y[(y[,identifier]==5003 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]  
    # x <- y[(y[,identifier]==5019 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]
    # x <- y[(y[,identifier]==5021 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]  
    # x <- y[(y[,identifier]==5121 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]  
    # x <- y[(y[,identifier]==5445 & y[,"seq_flag"]==36),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]
    # x <- y[(y[,identifier]==38049 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]
    
    # x <- y[(y[,identifier]==39187 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")] # LONG SHORT EQUITIES
    
    # x <- y[(y[,identifier]==30705 & y[,"seq_flag"]==1),c(id_col,date_col,analysis_col,"seq_flag","seq_id")]

    
    # analysis_col <- analysis_col
    # id_col <- identifier
    # width <- cutoff_num
    
    temp_id <- unique(x[,id_col])
    temp_seq_flag <- unique(x[,"seq_flag"])
      
    #cat(paste(temp_id,temp_seq_flag,sep=" ; "),"\n")
    
    #x_dates <- sort(unique(data_style[data_style[,id_col]==temp_id,date_col]))
    #data_style_trim <- data_style[data_style[,date_col] %in% x_dates,]
    
    x_trim <- x[!is.na(x[,"seq_id"]),]

    model_cols <- c("Estimate","Std_Error","t_value","Pr_t")
    
    if (nrow(x_trim)==0)
    {
      indexrsq_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      #indexrsq_screen_final[,c("id")] <- seq(1,nrow(indexrsq_screen_final),1)
      indexrsq_screen_final[,c("id")] <- row.names(x)
    } else {
      
      z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
      #z[,"id"] <- as.integer(row.names(z))
      z[,"id"] <- as.integer(seq(nrow(z)))
      row.names(z) <- seq(nrow(z))

      row.names(x) <- seq(nrow(x))
      
      x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
      x2[,"id"] <- as.integer(row.names(x2))
      row.names(x2) <- seq(nrow(x2))
      
      if (nrow(z)<cutoff_num)
      {
        indexrsq_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
        
      } else
      {
        indexrsq_screen <- rollapply(data=z,width=width,function(w,data_style,temp_id,temp_seq_flag,ret_col,id_col,date_col){
          
          # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
          # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
          # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
          # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
          
          #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
          
          w_dates_temp <- sort(unique(w[,date_col]))
          #data_style_trim <- as.data.frame(data_style[data_style[,date_col] %in% w_dates_temp,])
          data_style_trim <- as.data.frame(data_style[data_style[,date_col] %in% w_dates_temp,],stringsAsFactors=FALSE)
          
          indexrsq_screen <- data.frame(temp_id=temp_id,temp_flag=temp_seq_flag,
                                        indexrsq_screen_execute(data_style=data_style_trim,id=temp_id,ret_col=ret_col,id_col=id_col,date_col=date_col),stringsAsFactors=FALSE)
          colnames(indexrsq_screen)[match("temp_id",names(indexrsq_screen))] <- id_col
          colnames(indexrsq_screen)[match("temp_flag",names(indexrsq_screen))] <- "seq_flag"
          
          return(indexrsq_screen)
          
        },data_style=data_style,temp_id=temp_id,temp_seq_flag=temp_seq_flag,ret_col=analysis_col,id_col=id_col,date_col=date_col,by.column=FALSE,fill=NA,partial=FALSE,align="right") 
        
        indexrsq_screen_df  <- data.frame(indexrsq_screen,stringsAsFactors=FALSE)
        indexrsq_screen_df2  <- data.frame(id=z[,"id"],indexrsq_screen_df[,colnames(indexrsq_screen_df)[!(colnames(indexrsq_screen_df) %in% c(identifier,"seq_flag"))]],stringsAsFactors=FALSE)
        indexrsq_screen_df2[,"id"] <- as.integer(indexrsq_screen_df2[,"id"])
        row.names(indexrsq_screen_df2) <- seq(nrow(indexrsq_screen_df2))
        indexrsq_screen_final <- merge(x2,indexrsq_screen_df2,
                                       by.x=c("id"),by.y=c("id"),
                                       all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
        indexrsq_screen_final <- indexrsq_screen_final[order(indexrsq_screen_final[,"id"]),] 
        row.names(indexrsq_screen_final) <- seq(nrow(indexrsq_screen_final))
        
        rm(indexrsq_screen,indexrsq_screen_df,indexrsq_screen_df2)
      }
      
      rm(z,x2)
    }
    rm(temp_id,temp_seq_flag)
    rm(x_trim,model_cols)

    return(indexrsq_screen_final)
    
  },data_style=y,analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")
  
  # a <- count(data_s2_3_prob_sub,c(identifier))
  # b <- count(y,c(identifier))

  return(data_s2_3_prob_sub)
  
},style_col=strat_col,analysis_col=analysis_col,id_col=identifier,date_col="yr_month",width=cutoff_num,.progress="text")

rm(data_s2_3)


### Create Flags

indexrsq_percentiles <- c("indexrsq_percent_99","indexrsq_percent_95","indexrsq_percent_90")

#data_s2_3_full <- data.frame(data_s2_3_prob[,!(colnames(data_s2_3_prob) %in% c("style_id","id",analysis_col,data_trim_cols_lagged_trim))],
#                             matrix(NA,ncol=length(indexrsq_percentiles),nrow=1,dimnames=list(c(),indexrsq_percentiles)),stringsAsFactors=FALSE)
data_s2_3_full <- data.frame(data_s2_3_prob,matrix(NA,ncol=length(indexrsq_percentiles),nrow=1,dimnames=list(c(),indexrsq_percentiles)),stringsAsFactors=FALSE)
data_s2_3_full <- data_s2_3_full[order(data_s2_3_full[,identifier]),]
row.names(data_s2_3_full) <- seq(nrow(data_s2_3_full))

data_s2_3_full[,"indexrsq_percent_99"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.01),1,0)
data_s2_3_full[,"indexrsq_percent_95"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.05),1,0)
data_s2_3_full[,"indexrsq_percent_90"] <- ifelse((data_s2_3_full[,"Pr_t"]>0.10),1,0)

#Trigger if fails at any point in time series
data_s2_3_full_any <- misreport_cumm_any(data=data_s2_3_full,id_col=identifier,pct_cols=indexrsq_percentiles,suffix="any")

#Trigger if fails more than half
data_s2_3_full_avg <- misreport_cumm_avg(data=data_s2_3_full,id_col=identifier,pct_cols=indexrsq_percentiles,suffix="avg")


#Merge flags
data_s2_3_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s2_3_final0)[match("temp_id",names(data_s2_3_final0))] <- identifier

data_s2_3_final1 <- merge(data_s2_3_final0,data_s2_3_full_any,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s2_3_final2 <- merge(data_s2_3_final1,data_s2_3_full_avg,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s2_3_final <- data_s2_3_final2

#rm2(data_s2_3_prob,data_s2_3_full)
rm2(data_s2_3_final0,data_s2_3_final1,data_s2_3_final2)
rm2(data_s2_3_full_any,data_s2_3_full_avg)


###############################################################################
cat("S2 - MERGE FLAGS","\n")
###############################################################################

data_s2_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s2_final0)[match("temp_id",names(data_s2_final0))] <- identifier

data_s2_final1 <- merge(data_s2_final0,data_s2_3_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s2_final <- data_s2_final1

#rm2(data_s2_3_final)
rm2(data_s2_final0,data_s2_final1)
rm2(data_s2)


###############################################################################
##
cat("SECTION: SCREEN 3 - UNCONDITIONAL SERIAL CORRELATION","\n")
##
###############################################################################

data_s3 <- data_trim_full[,c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")]
data_s3 <- data_s3[data_s3[,identifier]!=0,]

###############################################################################
cat("S3 - (1) AR(1)","\n")
###############################################################################

data_s3_1 <- data_s3[,c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")]
#data_s3_1[,c(analysis_col,data_trim_cols_lagged_trim)] <- round(data_s3_1[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)
data_s3_1[,c(analysis_col,data_trim_cols_lagged_trim)] <- round2(data_s3_1[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)


data_s3_1_prob <- ddply(.data=data_s3_1,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s3_1[(data_s3_1[,identifier]==5002 & data_s3_1[,"seq_flag"]==1),]  
  # x <- data_s3_1[(data_s3_1[,identifier]==5021 & data_s3_1[,"seq_flag"]==1),]
  # x <- data_s3_1[(data_s3_1[,identifier]==5445 & data_s3_1[,"seq_flag"]==36),]
  # x <- data_s3_1[(data_s3_1[,identifier]==32420 & data_s3_1[,"seq_flag"]==1),]
  # x <- data_s3_1[(data_s3_1[,identifier]==38049 & data_s3_1[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")

  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c("Estimate","Std_Error","t_value","Pr_t")
  
  if (nrow(x_trim)==0)
  {
    ar_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    ar_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]

    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      ar_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      ar_screen <- rollapply(data=z,width=width,function(w,ret_col,lag_ret_col,model_cols){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        # lag_ret_col <- data_trim_cols_lagged_trim
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")

        #print(str(w))
        #w <- as.data.frame(w,stringsAsFactors=FALSE)
        #print(str(w))
        
        ar_screen_temp <- ar_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col,lag_ret_col=lag_ret_col)
  
        if(length(which(ar_screen_temp[,"var"]=="x1"))==0){
          
          #cat("ALL OBS THE SAME - ID:",unique(x[,identifier]),";","SEQ_FLAG:",unique(x[,"seq_flag"]),";","ROW FIRST:",min(as.integer(row.names(w))),";","ROW LAST:",max(as.integer(row.names(w))),"\n")
          
          ar_screen_temp_out <- data.frame(matrix(NA,ncol=length(model_cols),nrow=1,dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
          ar_screen_temp_out[1,] <- c(NA,0,Inf,0)
          
        } else {
          ar_screen_temp_out <- ar_screen_temp[ar_screen_temp[,"var"]=="x1",model_cols]
        }
   
        rm(model_cols,ar_screen_temp)
        
        return(ar_screen_temp_out)
          
      },ret_col=analysis_col,lag_ret_col=data_trim_cols_lagged_trim,model_cols,by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      ar_screen_df  <- data.frame(ar_screen,stringsAsFactors=FALSE)
      ar_screen_df2  <- data.frame(id=z[,"id"],ar_screen_df,stringsAsFactors=FALSE)
      ar_screen_df2[,"id"] <- as.integer(ar_screen_df2[,"id"])
      row.names(ar_screen_df2) <- seq(nrow(ar_screen_df2))
      ar_screen_final <- merge(x2,ar_screen_df2,
                                 by.x=c("id"),by.y=c("id"),
                                 all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      ar_screen_final <- ar_screen_final[order(ar_screen_final[,"id"]),] 
      row.names(ar_screen_final) <- seq(nrow(ar_screen_final))
      
      rm(ar_screen,ar_screen_df,ar_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(ar_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s3_1)


### Create Flags

ar_1_percentiles <- c("ar_1_percent_99","ar_1_percent_95","ar_1_percent_90")

data_s3_1_full <- data.frame(data_s3_1_prob,matrix(NA,ncol=length(ar_1_percentiles),nrow=1,dimnames=list(c(),ar_1_percentiles)),stringsAsFactors=FALSE)

data_s3_1_full[,"ar_1_percent_99"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.01),1,0)
data_s3_1_full[,"ar_1_percent_95"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.05),1,0)
data_s3_1_full[,"ar_1_percent_90"] <- ifelse((data_s3_1_full[,"Estimate"]>=0 & data_s3_1_full[,"Pr_t"]<=0.10),1,0)

data_s3_1_full[,"ar_1_percent_99"] <- ifelse((!is.na(data_s3_1_full[,"Std_Error"]) & data_s3_1_full[,"Std_Error"]==0),1,data_s3_1_full[,"ar_1_percent_99"])
data_s3_1_full[,"ar_1_percent_95"] <- ifelse((!is.na(data_s3_1_full[,"Std_Error"]) & data_s3_1_full[,"Std_Error"]==0),1,data_s3_1_full[,"ar_1_percent_95"])
data_s3_1_full[,"ar_1_percent_90"] <- ifelse((!is.na(data_s3_1_full[,"Std_Error"]) & data_s3_1_full[,"Std_Error"]==0),1,data_s3_1_full[,"ar_1_percent_90"])

# test1 <- data_s3_1_full[(data_s3_1_full[,identifier]==32420 & data_s3_1_full[,"seq_flag"]==1),]
# test2 <- data_s3_1_full[(data_s3_1_full[,identifier]==38049 & data_s3_1_full[,"seq_flag"]==1),]
# 
# a_est_na <- data_s3_1_full[(is.na(data_s3_1_full[,"Estimate"]) & !is.na(data_s3_1_full[,"Std_Error"])),]
# a_est_na <- a_est_na[rowSums(is.na(a_est_na[,1:ncol(a_est_na)]))<ncol(a_est_na),]
# 
# a_est_0 <- data_s3_1_full[(!is.na(data_s3_1_full[,"Estimate"]) & data_s3_1_full[,"Estimate"]==0),]
# a_est_0 <- a_est_0[rowSums(is.na(a_est_0[,1:ncol(a_est_0)]))<ncol(a_est_0),]
# 
# a_std_err_0 <- data_s3_1_full[(!is.na(data_s3_1_full[,"Std_Error"]) & data_s3_1_full[,"Std_Error"]==0),]
# a_std_err_0 <- a_std_err_0[rowSums(is.na(a_std_err_0[,1:ncol(a_std_err_0)]))<ncol(a_std_err_0),]
#
# a_t_val_inf <- data_s3_1_full[is.infinite(data_s3_1_full[,"t_value"]),]
# a_std_err_0 <- a_std_err_0[rowSums(is.na(a_std_err_0[,1:ncol(a_std_err_0)]))<ncol(a_std_err_0),]


#Trigger if fails at any point in time series
data_s3_1_full_any <- misreport_cumm_any(data=data_s3_1_full,id_col=identifier,pct_cols=ar_1_percentiles,suffix="any")

#Trigger if fails more than half
data_s3_1_full_avg <- misreport_cumm_avg(data=data_s3_1_full,id_col=identifier,pct_cols=ar_1_percentiles,suffix="avg")


#Merge flags
data_s3_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s3_final0)[match("temp_id",names(data_s3_final0))] <- identifier

data_s3_final1 <- merge(data_s3_final0,data_s3_1_full_any,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s3_final2 <- merge(data_s3_final1,data_s3_1_full_avg,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s3_final <- data_s3_final2

#rm2(data_s3_1_prob,data_s3_1_full)
rm2(data_s3_final0,data_s3_final1,data_s3_final2)
rm2(data_s3_1_full_any,data_s3_1_full_avg)

rm2(data_s3)


###############################################################################
##
cat("SECTION: SCREEN 4 - CONDITIONAL SERIAL CORRELATION","\n")
##
###############################################################################

###############################################################################
cat("S4 - (1) CAR(1)","\n")
###############################################################################



###############################################################################
##
cat("SECTION: SCREEN 5 - DATA QUALITY","\n")
##
###############################################################################

#data_s5 <- data.frame(data_trim_full[,c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")],stringsAsFactors=FALSE)
data_s5 <- data.frame(data_trim_full[,c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")],stringsAsFactors=FALSE)
data_s5 <- data_s5[data_s5[,identifier]!=0,]

###############################################################################
cat("S5 - (1) NUM_ZERO and (6) PER_NEGATIVE","\n")
###############################################################################

data_s5_1 <- data_s5[c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
#data_s5_1[,analysis_col] <- round(data_s5_1[,analysis_col],digits=4)
data_s5_1[,analysis_col] <- round2(data_s5_1[,analysis_col],digits=4)

data_s5_1_screen <- ddply(.data=data_s5_1,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s5_1[(data_s5_1[,identifier]==5002 & data_s5_1[,"seq_flag"]==1),]  
  # x <- data_s5_1[(data_s5_1[,identifier]==5021 & data_s5_1[,"seq_flag"]==1),]
  # x <- data_s5_1[(data_s5_1[,identifier]==5445 & data_s5_1[,"seq_flag"]==36),]
  # x <- data_s5_1[(data_s5_1[,identifier]==32420 & data_s5_1[,"seq_flag"]==1),]
  # x <- data_s5_1[(data_s5_1[,identifier]==38049 & data_s5_1[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c("sum_total","sum_pos","sum_zero","sum_neg","prob_ind_pos","prob_ind_zero","prob_ind_neg","prob_cum_pos","prob_cum_zero","prob_cum_neg")
  
  if (nrow(x_trim)==0)
  {
    zero_and_neg_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    zero_and_neg_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      zero_and_neg_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      zero_and_neg_screen <- rollapply(data=z,width=width,function(w,ret_col,prob_type){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        # prob_type="normal"
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")

        return(zero_neg_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col,prob_type=prob_type))
        
      },ret_col=analysis_col,prob_type="normal",by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      zero_and_neg_screen_df  <- data.frame(zero_and_neg_screen,stringsAsFactors=FALSE)
      zero_and_neg_screen_df2  <- data.frame(id=z[,"id"],zero_and_neg_screen_df,stringsAsFactors=FALSE)
      zero_and_neg_screen_df2[,"id"] <- as.integer(zero_and_neg_screen_df2[,"id"])
      row.names(zero_and_neg_screen_df2) <- seq(nrow(zero_and_neg_screen_df2))
      zero_and_neg_screen_final <- merge(x2,zero_and_neg_screen_df2,
                               by.x=c("id"),by.y=c("id"),
                               all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      zero_and_neg_screen_final <- zero_and_neg_screen_final[order(zero_and_neg_screen_final[,"id"]),] 
      row.names(zero_and_neg_screen_final) <- seq(nrow(zero_and_neg_screen_final))
      
      rm(zero_and_neg_screen,zero_and_neg_screen_df,zero_and_neg_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(zero_and_neg_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s5_1)


### Create Flags

per_positive_percentiles <- c("per_positive_percent_99","per_positive_percent_95","per_positive_percent_90")
num_zero_percentiles <- c("num_zero_percent_99","num_zero_percent_95","num_zero_percent_90")
per_negative_percentiles <- c("per_negative_percent_99","per_negative_percent_95","per_negative_percent_90")

zero_and_neg_percentiles <- c(per_positive_percentiles,num_zero_percentiles,per_negative_percentiles)

data_s5_1_full <- data.frame(data_s5_1_screen,
                             matrix(NA,ncol=length(zero_and_neg_percentiles),nrow=1,dimnames=list(c(),zero_and_neg_percentiles)),
                             stringsAsFactors=FALSE)

data_s5_1_full[,"per_positive_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.01,1,0)
data_s5_1_full[,"per_positive_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.05,1,0)
data_s5_1_full[,"per_positive_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.10,1,0)

data_s5_1_full[,"num_zero_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.01,1,0)
data_s5_1_full[,"num_zero_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.05,1,0)
data_s5_1_full[,"num_zero_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.10,1,0)

data_s5_1_full[,"per_negative_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.01,1,0)
data_s5_1_full[,"per_negative_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.05,1,0)
data_s5_1_full[,"per_negative_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.10,1,0)


#Trigger if fails at any point in time series
data_s5_1_full_any <- misreport_cumm_any(data=data_s5_1_full,id_col=identifier,pct_cols=zero_and_neg_percentiles,suffix="any")

#Trigger if fails more than half
data_s5_1_full_avg <- misreport_cumm_avg(data=data_s5_1_full,id_col=identifier,pct_cols=zero_and_neg_percentiles,suffix="avg")

#Merge flags
data_s5_1_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_1_final0)[match("temp_id",names(data_s5_1_final0))] <- identifier

data_s5_1_final1 <- merge(data_s5_1_final0,data_s5_1_full_any,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s5_1_final2 <- merge(data_s5_1_final1,data_s5_1_full_avg,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_1_final <- data_s5_1_final2

#rm2(data_s5_1_screen,data_s5_1_full)
rm2(data_s5_1_final0,data_s5_1_final1,data_s5_1_final2)
rm2(data_s5_1_full_any,data_s5_1_full_avg)


###############################################################################
cat("S5 - (2) PER_REPEATS","\n")
###############################################################################

data_s5_2 <- data_s5[c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
#data_s5_2[,analysis_col] <- round(data_s5_2[,analysis_col],digits=6)
data_s5_2[,analysis_col] <- round2(data_s5_2[,analysis_col],digits=6)

# data_s5_2_screen <- ddply(.data=data_s5_2,.variables=identifier,.fun=function(x,analysis_col,id_col){
#   
#   # x <- data_s5_2[data_s5_2[,identifier]==0,c(data_trim_cols_id,analysis_col)]
#   # x <- data_s5_2[data_s5_2[,identifier]==5002,c(data_trim_cols_id,analysis_col)]
#   # x <- data_s5_2[data_s5_2[,identifier]==6094,c(data_trim_cols_id,analysis_col)]
#   # analysis_col <- analysis_col
#   # id_col <- identifier
#   
#   per_repeat_screen <- per_repeat_screen_execute(data=x,ret_col=analysis_col)
#   
#   return(per_repeat_screen)
#   
# },analysis_col=analysis_col,id_col=identifier,.progress="text")

data_s5_2_screen <- ddply(.data=data_s5_2,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s5_2[(data_s5_2[,identifier]==5002 & data_s5_2[,"seq_flag"]==1),]  
  # x <- data_s5_2[(data_s5_2[,identifier]==5021 & data_s5_2[,"seq_flag"]==1),]
  # x <- data_s5_2[(data_s5_2[,identifier]==5445 & data_s5_2[,"seq_flag"]==36),]
  # x <- data_s5_2[(data_s5_2[,identifier]==32420 & data_s5_2[,"seq_flag"]==1),]
  # x <- data_s5_2[(data_s5_2[,identifier]==38049 & data_s5_2[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <-  c("Count_1s","Count_u","Total","Prop_1s","Prop_u","Prop_1s_one_minus","Prop_u_one_minus","Prop_1s_u","Prop_1s_u_one_minus")
  
  if (nrow(x_trim)==0)
  {
    per_repeat_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    per_repeat_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      per_repeat_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      per_repeat_screen <- rollapply(data=z,width=width,function(w,ret_col){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
        
        return(per_repeat_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col))
        
      },ret_col=analysis_col,by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      per_repeat_screen_df  <- data.frame(per_repeat_screen,stringsAsFactors=FALSE)
      per_repeat_screen_df2  <- data.frame(id=z[,"id"],per_repeat_screen_df,stringsAsFactors=FALSE)
      per_repeat_screen_df2[,"id"] <- as.integer(per_repeat_screen_df2[,"id"])
      row.names(per_repeat_screen_df2) <- seq(nrow(per_repeat_screen_df2))
      per_repeat_screen_final <- merge(x2,per_repeat_screen_df2,
                                         by.x=c("id"),by.y=c("id"),
                                         all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      per_repeat_screen_final <- per_repeat_screen_final[order(per_repeat_screen_final[,"id"]),] 
      row.names(per_repeat_screen_final) <- seq(nrow(per_repeat_screen_final))
      
      rm(per_repeat_screen,per_repeat_screen_df,per_repeat_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(per_repeat_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s5_2)


### Create Flags

per_repeat_cutoffs <- c("cutoff_99","cutoff_95","cutoff_90")
per_repeat_percentiles <- c("per_repeats_percent_99","per_repeats_percent_95","per_repeats_percent_90")

data_s5_2_full <- data.frame(data_s5_2_screen,
                             matrix(NA,ncol=length(per_repeat_cutoffs),nrow=1,dimnames=list(c(),per_repeat_cutoffs)),
                             matrix(NA,ncol=length(per_repeat_percentiles),nrow=1,dimnames=list(c(),per_repeat_percentiles)),stringsAsFactors=FALSE)


data_s5_2_full[,"cutoff_99"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Per_Repeat"),"Per_0.99"]
data_s5_2_full[,"cutoff_95"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Per_Repeat"),"Per_0.95"]
data_s5_2_full[,"cutoff_90"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Per_Repeat"),"Per_0.90"]

data_s5_2_full[,"per_repeats_percent_99"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_99"],1,0)
data_s5_2_full[,"per_repeats_percent_95"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_95"],1,0)
data_s5_2_full[,"per_repeats_percent_90"] <- ifelse(data_s5_2_full[,"Prop_u_one_minus"]>data_s5_2_full[,"cutoff_90"],1,0)

#Trigger if fails at any point in time series
data_s5_2_full_any <- misreport_cumm_any(data=data_s5_2_full,id_col=identifier,pct_cols=per_repeat_percentiles,suffix="any")

#Trigger if fails more than half
data_s5_2_full_avg <- misreport_cumm_avg(data=data_s5_2_full,id_col=identifier,pct_cols=per_repeat_percentiles,suffix="avg")

#Merge flags
data_s5_2_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_2_final0)[match("temp_id",names(data_s5_2_final0))] <- identifier

data_s5_2_final1 <- merge(data_s5_2_final0,data_s5_2_full_any,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s5_2_final2 <- merge(data_s5_2_final1,data_s5_2_full_avg,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_2_final <- data_s5_2_final2

#rm2(data_s5_2_screen,data_s5_2_full)
rm2(data_s5_2_final0,data_s5_2_final1,data_s5_2_final2)
rm2(data_s5_2_full_any,data_s5_2_full_avg)


###############################################################################
cat("S5 - (3) STRING","\n")
###############################################################################

data_s5_3 <- data_s5[c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
# #data_s5_3[,analysis_col] <- round(data_s5_3[,analysis_col],digits=6)
# data_s5_3[,analysis_col] <- round2(data_s5_3[,analysis_col],digits=6)

#data_s5_3[,analysis_col] <- round(data_s5_3[,analysis_col],digits=4)
data_s5_3[,analysis_col] <- round2(data_s5_3[,analysis_col],digits=4)

data_s5_3_screen <- ddply(.data=data_s5_3,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s5_3[(data_s5_3[,identifier]==5002 & data_s5_3[,"seq_flag"]==1),]  
  # x <- data_s5_3[(data_s5_3[,identifier]==5021 & data_s5_3[,"seq_flag"]==1),]
  # x <- data_s5_3[(data_s5_3[,identifier]==5445 & data_s5_3[,"seq_flag"]==36),]
  # x <- data_s5_3[(data_s5_3[,identifier]==32420 & data_s5_3[,"seq_flag"]==1),]
  # x <- data_s5_3[(data_s5_3[,identifier]==38049 & data_s5_3[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c("Max_Length","Total","Prop_u")
  
  if (nrow(x_trim)==0)
  {
    string_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    string_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      string_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      string_screen <- rollapply(data=z,width=width,function(w,ret_col){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
        
        return(string_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col))
        
      },ret_col=analysis_col,by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      string_screen_df  <- data.frame(string_screen,stringsAsFactors=FALSE)
      string_screen_df2  <- data.frame(id=z[,"id"],string_screen_df,stringsAsFactors=FALSE)
      string_screen_df2[,"id"] <- as.integer(string_screen_df2[,"id"])
      row.names(string_screen_df2) <- seq(nrow(string_screen_df2))
      string_screen_final <- merge(x2,string_screen_df2,
                                      by.x=c("id"),by.y=c("id"),
                                      all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      string_screen_final <- string_screen_final[order(string_screen_final[,"id"]),] 
      row.names(string_screen_final) <- seq(nrow(string_screen_final))
      
      rm(string_screen,string_screen_df,string_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(string_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s5_3)


### Create Flags

string_cutoffs <- c("cutoff_99","cutoff_95","cutoff_90")
string_percentiles <- c("string_percent_99","string_percent_95","string_percent_90")

data_s5_3_full <- data.frame(data_s5_3_screen,
                             matrix(NA,ncol=length(string_cutoffs),nrow=1,dimnames=list(c(),string_cutoffs)),
                             matrix(NA,ncol=length(string_percentiles),nrow=1,dimnames=list(c(),string_percentiles)),stringsAsFactors=FALSE)

data_s5_3_full[,"cutoff_99"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="String"),"Per_0.99"]
data_s5_3_full[,"cutoff_95"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="String"),"Per_0.95"]
data_s5_3_full[,"cutoff_90"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="String"),"Per_0.90"]

data_s5_3_full[,"string_percent_99"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_99"],1,0)
data_s5_3_full[,"string_percent_95"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_95"],1,0)
data_s5_3_full[,"string_percent_90"] <- ifelse(data_s5_3_full[,"Max_Length"]>data_s5_3_full[,"cutoff_90"],1,0)


#Trigger if fails at any point in time series
data_s5_3_full_any <- misreport_cumm_any(data=data_s5_3_full,id_col=identifier,pct_cols=string_percentiles,suffix="any")

#Trigger if fails more than half
data_s5_3_full_avg <- misreport_cumm_avg(data=data_s5_3_full,id_col=identifier,pct_cols=string_percentiles,suffix="avg")

#Merge flags
data_s5_3_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_3_final0)[match("temp_id",names(data_s5_3_final0))] <- identifier

data_s5_3_final1 <- merge(data_s5_3_final0,data_s5_3_full_any,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s5_3_final2 <- merge(data_s5_3_final1,data_s5_3_full_avg,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_3_final <- data_s5_3_final2

#rm2(data_s5_2_screen,data_s5_3_full)
rm2(data_s5_3_final0,data_s5_3_final1,data_s5_3_final2)
rm2(data_s5_3_full_any,data_s5_3_full_avg)


###############################################################################
cat("S5 - (4) NUM_PAIRS","\n")
###############################################################################

data_s5_4 <- data_s5[c(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim,"seq_flag","seq_id")]
# #data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)] <- round(data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)],digits=6)
# data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)] <- round2(data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)],digits=6)

#data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)] <- round(data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)
data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)] <- round2(data_s5_4[,c(analysis_col,data_trim_cols_lagged_trim)],digits=4)

data_s5_4_screen <- ddply(.data=data_s5_4,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s5_4[(data_s5_4[,identifier]==5019 & data_s5_4[,"seq_flag"]==1),]  
  # x <- data_s5_4[(data_s5_4[,identifier]==5021 & data_s5_4[,"seq_flag"]==1),]  
  # x <- data_s5_4[(data_s5_4[,identifier]==5445 & data_s5_4[,"seq_flag"]==36),]  
  
  # x <- data_s5_4[(data_s5_4[,identifier]==8340 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==26786 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==28718 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==29172 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==30244 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==37302 & data_s5_4[,"seq_flag"]==1),]
  # x <- data_s5_4[(data_s5_4[,identifier]==38787 & data_s5_4[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c("Max_Pairs","Max_Pairs_Adj","Total","Prop","Prop_Adj")
  
  if (nrow(x_trim)==0)
  {
    num_pairs_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    num_pairs_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      num_pairs_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      num_pairs_screen <- rollapply(data=z,width=width,function(w,ret_col,lag_ret_col,both_ways){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        # lag_ret_col <- data_trim_cols_lagged_trim
        # both_ways <- both_ways
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
        
        return(num_pairs_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col,lag_ret_col=lag_ret_col,both_ways=both_ways))
        
      },ret_col=analysis_col,lag_ret_col=data_trim_cols_lagged_trim,both_ways=TRUE,by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      num_pairs_screen_df  <- data.frame(num_pairs_screen,stringsAsFactors=FALSE)
      num_pairs_screen_df2  <- data.frame(id=z[,"id"],num_pairs_screen_df,stringsAsFactors=FALSE)
      num_pairs_screen_df2[,"id"] <- as.integer(num_pairs_screen_df2[,"id"])
      row.names(num_pairs_screen_df2) <- seq(nrow(num_pairs_screen_df2))
      num_pairs_screen_final <- merge(x2,num_pairs_screen_df2,
                                      by.x=c("id"),by.y=c("id"),
                                      all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      num_pairs_screen_final <- num_pairs_screen_final[order(num_pairs_screen_final[,"id"]),] 
      row.names(num_pairs_screen_final) <- seq(nrow(num_pairs_screen_final))
      
      rm(num_pairs_screen,num_pairs_screen_df,num_pairs_screen_df2)
    }
    
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(num_pairs_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s5_4)

### Create Flags

num_pairs_cutoffs <- c("cutoff_99","cutoff_95","cutoff_90")
num_pairs_percentiles <- c("num_pairs_percent_99","num_pairs_percent_95","num_pairs_percent_90")

data_s5_4_full <- data.frame(data_s5_4_screen,
                             matrix(NA,ncol=length(num_pairs_cutoffs),nrow=1,dimnames=list(c(),num_pairs_cutoffs)),
                             matrix(NA,ncol=length(num_pairs_percentiles),nrow=1,dimnames=list(c(),num_pairs_percentiles)),stringsAsFactors=FALSE)

data_s5_4_full[,"cutoff_99"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Num_Pairs"),"Per_0.99"]
data_s5_4_full[,"cutoff_95"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Num_Pairs"),"Per_0.95"]
data_s5_4_full[,"cutoff_90"] <- cutoffs_comb_trim[(cutoffs_comb_trim[,"Num"]==cutoff_num & cutoffs_comb_trim[,"Flag"]=="Num_Pairs"),"Per_0.90"]

data_s5_4_full[,"num_pairs_percent_99"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_99"],1,0)
data_s5_4_full[,"num_pairs_percent_95"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_95"],1,0)
data_s5_4_full[,"num_pairs_percent_90"] <- ifelse(data_s5_4_full[,"Max_Pairs_Adj"]>data_s5_4_full[,"cutoff_90"],1,0)

#Trigger if fails at any point in time series
data_s5_4_full_any <- misreport_cumm_any(data=data_s5_4_full,id_col=identifier,pct_cols=num_pairs_percentiles,suffix="any")

#Trigger if fails more than half
data_s5_4_full_avg <- misreport_cumm_avg(data=data_s5_4_full,id_col=identifier,pct_cols=num_pairs_percentiles,suffix="avg")

#Merge flags
data_s5_4_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_4_final0)[match("temp_id",names(data_s5_4_final0))] <- identifier

data_s5_4_final1 <- merge(data_s5_4_final0,data_s5_4_full_any,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s5_4_final2 <- merge(data_s5_4_final1,data_s5_4_full_avg,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_4_final <- data_s5_4_final2

#rm2(data_s5_4_screen,data_s5_4_full)
rm2(data_s5_4_final0,data_s5_4_final1,data_s5_4_final2)
rm2(data_s5_4_full_any,data_s5_4_full_avg)

#aa1 <- data_s5_4_full[(!is.na(data_s5_4_full[,"Max_Pairs_Adj"]) & data_s5_4_full[,"Max_Pairs_Adj"]>0),]
#aa2 <- data_s5_4_full[data_s5_4_full[,identifier] %in% unique(aa1[,identifier]),]


###############################################################################
cat("S5 - (5) UNIFORM","\n")
###############################################################################

data_s5_5 <- data_s5[c(data_trim_cols_id,analysis_col,"seq_flag","seq_id")]
#data_s5_5[,analysis_col] <- round(data_s5_5[,analysis_col],digits=4)
data_s5_5[,analysis_col] <- round2(data_s5_5[,analysis_col],digits=4)

data_s5_5_screen <- ddply(.data=data_s5_5,.variables=c(identifier,"seq_flag"),.fun=function(x,analysis_col,id_col,width){
  
  # x <- data_s5_5[(data_s5_5[,identifier]==5002 & data_s5_5[,"seq_flag"]==1),]  
  # x <- data_s5_5[(data_s5_5[,identifier]==5021 & data_s5_5[,"seq_flag"]==1),]
  # x <- data_s5_5[(data_s5_5[,identifier]==5445 & data_s5_5[,"seq_flag"]==36),]
  # x <- data_s5_5[(data_s5_5[,identifier]==32420 & data_s5_5[,"seq_flag"]==1),]
  # x <- data_s5_5[(data_s5_5[,identifier]==38049 & data_s5_5[,"seq_flag"]==1),]
  
  # analysis_col <- analysis_col
  # id_col <- identifier
  # width <- cutoff_num
  
  #cat(paste(unique(x[,identifier]),unique(x[,"seq_flag"]),sep=" ; "),"\n")
  
  x_trim <- x[!is.na(x[,"seq_id"]),]
  
  model_cols <- c( "x_squared_logarithmic","df_logarithmic","pval_logarithmic","x_squared_uniform","df_uniform","pval_uniform")
  
  if (nrow(x_trim)==0)
  {
    uniform_screen_final <- data.frame(id=1,x,matrix(NA,ncol=length(model_cols),nrow=nrow(x),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
    uniform_screen_final[,c("id")] <- row.names(x)
    
  } else {
    
    z <- data.frame(id=NA,x_trim,stringsAsFactors=FALSE)
    #z[,"id"] <- as.integer(row.names(z))
    z[,"id"] <- as.integer(seq(nrow(z)))
    row.names(z) <- seq(nrow(z))
    z <- z[,colnames(z)[!(colnames(z) %in% c(id_col,"yr_month","seq_flag","seq_id"))]]
    
    row.names(x) <- seq(nrow(x))
    
    x2 <- data.frame(id=NA,x,stringsAsFactors=FALSE)
    x2[,"id"] <- as.integer(row.names(x2))
    row.names(x2) <- seq(nrow(x2))
    
    if (nrow(z)<cutoff_num)
    {
      uniform_screen_final <- data.frame(x2,matrix(NA,ncol=length(model_cols),nrow=nrow(x2),dimnames=list(c(),model_cols)),stringsAsFactors=FALSE)
      
    } else
    {
      uniform_screen <- rollapply(data=z,width=width,function(w,ret_col,graph,rounding_digit){
        
        # w <- z[(as.integer(row.names(z))>=1 & as.integer(row.names(z))<=24),]
        # w <- z[(as.integer(row.names(z))>=2 & as.integer(row.names(z))<=25),]
        # w <- z[(as.integer(row.names(z))>=17 & as.integer(row.names(z))<=40),]
        # w <- z[(as.integer(row.names(z))>=58 & as.integer(row.names(z))<=81),]
        # w <- z[(as.integer(row.names(z))>=61 & as.integer(row.names(z))<=84),]
        # ret_col <- analysis_col
        # graph <- FALSE
        # graph <- TRUE
        # rounding_digit <- 4
        
        #cat(paste(min(as.integer(row.names(w))),max(as.integer(row.names(w))),sep=" ; "),"\n")
        
        #uniform_out <- uniform_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col,graph=graph,rounding_digit=rounding_digit)
        
        #ok <- TRUE
        tryCatch(uniform_out <- uniform_screen_execute(data=as.data.frame(w,stringsAsFactors=FALSE),ret_col=ret_col,graph=graph,rounding_digit=rounding_digit),
                 warning=function(v) {cat("\n", unique(x[,identifier]), "\n") ; uniform_out <<- NA})
        #if (!ok) {cat(unique(x[,identifier]), "\n")}
        
        return(uniform_out)
        
      },ret_col=analysis_col,graph=FALSE,rounding_digit=4,by.column=FALSE,fill=NA,partial=FALSE,align="right")
      
      uniform_screen_df  <- data.frame(uniform_screen,stringsAsFactors=FALSE)
      uniform_screen_df2  <- data.frame(id=z[,"id"],uniform_screen_df,stringsAsFactors=FALSE)
      uniform_screen_df2[,"id"] <- as.integer(uniform_screen_df2[,"id"])
      row.names(uniform_screen_df2) <- seq(nrow(uniform_screen_df2))
      uniform_screen_final <- merge(x2,uniform_screen_df2,
                                    by.x=c("id"),by.y=c("id"),
                                    all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
      uniform_screen_final <- uniform_screen_final[order(uniform_screen_final[,"id"]),] 
      row.names(uniform_screen_final) <- seq(nrow(uniform_screen_final))
      
      rm(uniform_screen,uniform_screen_df,uniform_screen_df2)
    }
    rm(z,x2)
  }
  rm(x_trim,model_cols)
  
  return(uniform_screen_final)
},analysis_col=analysis_col,id_col=identifier,width=cutoff_num,.progress="text")

rm(data_s5_5)


### Create Flags

uniform_percentiles <- c("uniform_percent_99","uniform_percent_95","uniform_percent_90")

data_s5_5_full <- data.frame(data_s5_5_screen,matrix(NA,ncol=length(uniform_percentiles),nrow=1,dimnames=list(c(),uniform_percentiles)),stringsAsFactors=FALSE)

data_s5_5_full[,"uniform_percent_99"] <- ifelse(data_s5_5_full[,"pval_uniform"]<=0.01,1,0)
data_s5_5_full[,"uniform_percent_95"] <- ifelse(data_s5_5_full[,"pval_uniform"]<=0.05,1,0)
data_s5_5_full[,"uniform_percent_90"] <- ifelse(data_s5_5_full[,"pval_uniform"]<=0.10,1,0)

#Trigger if fails at any point in time series
data_s5_5_full_any <- misreport_cumm_any(data=data_s5_5_full,id_col=identifier,pct_cols=uniform_percentiles,suffix="any")

#Trigger if fails more than half
data_s5_5_full_avg <- misreport_cumm_avg(data=data_s5_5_full,id_col=identifier,pct_cols=uniform_percentiles,suffix="avg")

#Merge flags
data_s5_5_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_5_final0)[match("temp_id",names(data_s5_5_final0))] <- identifier

data_s5_5_final1 <- merge(data_s5_5_final0,data_s5_5_full_any,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))
data_s5_5_final2 <- merge(data_s5_5_final1,data_s5_5_full_avg,
                          by.x=c(identifier),by.y=c(identifier),
                          all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_5_final <- data_s5_5_final2

#rm2(data_s5_5_screen,data_s5_5_full)
rm2(data_s5_5_final0,data_s5_5_final1,data_s5_5_final2)
rm2(data_s5_5_full_any,data_s5_5_full_avg)


###############################################################################
cat("S5 - MERGE FLAGS","\n")
###############################################################################

data_s5_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_final0)[match("temp_id",names(data_s5_final0))] <- identifier

data_s5_final1 <- merge(data_s5_final0,data_s5_1_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_final2 <- merge(data_s5_final1,data_s5_2_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_final3 <- merge(data_s5_final2,data_s5_3_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_final4 <- merge(data_s5_final3,data_s5_4_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_final5 <- merge(data_s5_final4,data_s5_5_final,
                        by.x=c(identifier),by.y=c(identifier),
                        all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_s5_final <- data_s5_final5

#rm2(data_s5_1_final,data_s5_2_final,data_s5_3_final,data_s5_4_final,data_s5_5_final)
rm2(data_s5_final0,data_s5_final1,data_s5_final2,data_s5_final3,data_s5_final4,data_s5_final5)
rm2(data_s5)



###############################################################################
cat("STATS","\n")
###############################################################################

kink_percentiles_expand <- c(paste(kink_percentiles,"any",sep="_"),paste(kink_percentiles,"avg",sep="_"))
data_s1_final_stats_mean <- colMeans(data_s1_final[,kink_percentiles_expand],na.rm=T,dims=1)

indexrsq_percentiles_expand <- c(paste(indexrsq_percentiles,"any",sep="_"),paste(indexrsq_percentiles,"avg",sep="_"))
data_s2_3_final_stats_mean <- colMeans(data_s2_3_final[,indexrsq_percentiles_expand],na.rm=T,dims=1)

ar_1_percentiles_expand <- c(paste(ar_1_percentiles,"any",sep="_"),paste(ar_1_percentiles,"avg",sep="_"))
data_s3_final_stats_mean <- colMeans(data_s3_final[,ar_1_percentiles_expand],na.rm=T,dims=1)

zero_and_neg_percentiles_expand <- c(paste(zero_and_neg_percentiles,"any",sep="_"),paste(zero_and_neg_percentiles,"avg",sep="_"))
data_s5_1_final_stats_mean <- colMeans(data_s5_1_final[,zero_and_neg_percentiles_expand],na.rm=T,dims=1)

per_repeat_percentiles_expand <- c(paste(per_repeat_percentiles,"any",sep="_"),paste(per_repeat_percentiles,"avg",sep="_"))
data_s5_2_final_stats_mean <- colMeans(data_s5_2_final[,per_repeat_percentiles_expand],na.rm=T,dims=1)

string_percentiles_expand <- c(paste(string_percentiles,"any",sep="_"),paste(string_percentiles,"avg",sep="_"))
data_s5_3_final_stats_mean <- colMeans(data_s5_3_final[,string_percentiles_expand],na.rm=T,dims=1)

num_pairs_percentiles_expand <- c(paste(num_pairs_percentiles,"any",sep="_"),paste(num_pairs_percentiles,"avg",sep="_"))
data_s5_4_final_stats_mean <- colMeans(data_s5_4_final[,num_pairs_percentiles_expand],na.rm=T,dims=1)

uniform_percentiles_expand <- c(paste(uniform_percentiles,"any",sep="_"),paste(uniform_percentiles,"avg",sep="_"))
data_s5_5_final_stats_mean <- colMeans(data_s5_5_final[,uniform_percentiles_expand],na.rm=T,dims=1)


#rm2(kink_percentiles,kink_percentiles_expand)
#rm2(indexrsq_percentiles,indexrsq_percentiles_expand)
#rm2(ar_1_percentiles,ar_1_percentiles_expand)
#rm2(zero_and_neg_percentiles,zero_and_neg_percentiles_expand,per_positive_percentiles,num_zero_percentiles,per_negative_percentiles)
#rm2(per_repeat_percentiles,per_repeat_percentiles_expand,per_repeat_cutoffs)
#rm2(string_percentiles,string_percentiles_expand,string_cutoffs)
#rm2(num_pairs_percentiles,num_pairs_percentiles_expand,num_pairs_cutoffs)
#rm2(uniform_percentiles,uniform_percentiles_expand)


###############################################################################
cat("MERGE ALL FLAGS","\n")
###############################################################################

data_screen_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_screen_final0)[match("temp_id",names(data_screen_final0))] <- identifier

data_screen_final1 <- merge(data_screen_final0,data_s1_final,
                            by.x=c(identifier),by.y=c(identifier),
                            all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_screen_final2 <- merge(data_screen_final1,data_s2_final,
                            by.x=c(identifier),by.y=c(identifier),
                            all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_screen_final3 <- merge(data_screen_final2,data_s3_final,
                            by.x=c(identifier),by.y=c(identifier),
                            all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_screen_final4 <- merge(data_screen_final3,data_s5_final,
                            by.x=c(identifier),by.y=c(identifier),
                            all.x=TRUE,all.y=FALSE,sort=FALSE,suffixes=c(".x",".y"))

data_screen_final <- data_screen_final4

rm2(data_screen_final0,data_screen_final1,data_screen_final2,data_screen_final3,data_screen_final4)
#rm2(data_s1_final,data_s2_final,data_s3_final,data_s5_final)
#rm2(data_prescreen,data_trim_full)

invisible(gc(verbose=FALSE,reset=TRUE))


###############################################################################
cat("OUTPUT FLAGS","\n")
###############################################################################

data_screens <- data_screen_final 

#write.csv(data_screens,file=paste(output_directory,"data_screens.csv",sep="\\"),na="",quote=TRUE,row.names=FALSE)
write.csv(data_screens,file=paste(output_directory,paste("data_screens_",sprintf("%03d",cutoff_num),".csv",sep=""),sep="\\"),na="",quote=T,row.names=F)

#rm(data_screen_final)
#rm(data_trim_cols_id,analysis_col,data_trim_cols_lagged_trim)

write.csv(data_trim_full,file=paste(output_directory,paste("data_prescreen_trim_",sprintf("%03d",cutoff_num),".csv",sep=""),sep="\\"),na="",quote=T,row.names=F)

