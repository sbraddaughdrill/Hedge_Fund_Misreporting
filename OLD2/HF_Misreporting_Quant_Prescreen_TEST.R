# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Quant_Prescreen.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: Merge the performance and text data
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

source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("benford.analysis","BenfordTests","data.table","gdata","ggplot2","MASS","plyr","quantmod",
                       "reshape2","RSQLite","stringr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="\\")
data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="\\")


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

beg_year <- 2007
end_year <- 2013

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

#Fund Information
fund_table <- "EurekahedgeHF_Excel_aca_full14"
EurekahedgeHF_Excel_aca_full_import_vars_remove <- c("Minimum_Investment_Currency_combcol","Subsequent_Investment_Currency_combcol",
                                                     "Average_Net_Exposure","Average_Gross_Exposure","Annualized_Target_Return","Annualized_Target_Volatility",
                                                     "Exchange_Name","Accounting_Method_combcol","HMRC_Reporting_Status","SEC_Exemption",
                                                     "Subscription_Frequency","Redemption_Frequency","Redemption_Notification_Period","Penalty","Key_Man_Clause",
                                                     "Monthly_Ret2","Yearly_Ret2")
EurekahedgeHF_Excel_aca_full_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==fund_table,c("field")]
EurekahedgeHF_Excel_aca_full_import_vars_keep1 <- EurekahedgeHF_Excel_aca_full_import_vars_keep0[!(EurekahedgeHF_Excel_aca_full_import_vars_keep0 %in% EurekahedgeHF_Excel_aca_full_import_vars_remove)]
EurekahedgeHF_Excel_aca_full_import_vars_keep2 <- paste(EurekahedgeHF_Excel_aca_full_import_vars_keep1,sep="",collapse=", ")

#Text Information
text_table <- "text_stats_ios"
text_stats_ios_import_vars_remove <- c("lines_ios")
text_stats_ios_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==text_table,c("field")]
text_stats_ios_import_vars_keep1 <- text_stats_ios_import_vars_keep0[!(text_stats_ios_import_vars_keep0 %in% text_stats_ios_import_vars_remove)]
text_stats_ios_import_vars_keep2 <- paste(text_stats_ios_import_vars_keep1,sep="",collapse=", ")

rm2(descriptive_stats_tables,descriptive_stats_fields)


###############################################################################
cat("IMPORT FUND DATA AND PREALLOCATE NEW COLUMNS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full_expand_cols <- c("yr_month","total_fee","fund_ret_mkt_neg","Domicile_onshore_bin")

query_EurekahedgeHF_Excel_aca_full <- ""
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "select       ",EurekahedgeHF_Excel_aca_full_import_vars_keep2, sep=" ")
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "from         ",fund_table, "                                ", sep=" ")
query_EurekahedgeHF_Excel_aca_full <- trim(gsub(" {2,}", " ", query_EurekahedgeHF_Excel_aca_full))

rm2(EurekahedgeHF_Excel_aca_full_import_vars_keep2)

#EurekahedgeHF_Excel_aca_full <- runsql("SELECT * FROM EurekahedgeHF_Excel_aca_full14",descriptive_stats_db)

EurekahedgeHF_Excel_aca_full <- data.frame(runsql(query_EurekahedgeHF_Excel_aca_full,descriptive_stats_db),
                                           matrix(NA, ncol=length(EurekahedgeHF_Excel_aca_full_expand_cols), nrow=1,dimnames=list(c(),EurekahedgeHF_Excel_aca_full_expand_cols)),
                                           stringsAsFactors=FALSE)

#colnames(EurekahedgeHF_Excel_aca_full) <- tolower(colnames(EurekahedgeHF_Excel_aca_full))

#EurekahedgeHF_Excel_aca_full2 <- unknown_to_NA(EurekahedgeHF_Excel_aca_full,unknowns_strings)
for(k in which(colnames(EurekahedgeHF_Excel_aca_full) %in% EurekahedgeHF_Excel_aca_full_import_vars_keep1))
{
  #k <- 1
  
  EurekahedgeHF_Excel_aca_full[[k]] <- unknownToNA(EurekahedgeHF_Excel_aca_full[[k]], unknown=unknowns_strings,force=TRUE)
  EurekahedgeHF_Excel_aca_full[[k]] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full[[k]]),NA,EurekahedgeHF_Excel_aca_full[[k]])
}
rm2(k)

rm2(EurekahedgeHF_Excel_aca_full_import_vars_remove,EurekahedgeHF_Excel_aca_full_import_vars_keep0,EurekahedgeHF_Excel_aca_full_import_vars_keep1)
rm2(text_stats_ios_import_vars_remove,text_stats_ios_import_vars_keep0,text_stats_ios_import_vars_keep1)


###############################################################################
cat("FIX FUND DATA AND FILL PREALLOCATED COLUMNS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,identifier]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"yr"]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"month"]),]

EurekahedgeHF_Excel_aca_full2[,"date"] <- as.Date(EurekahedgeHF_Excel_aca_full2[,"date"],origin="1970-01-01",na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"Date_Added"] <- as.Date(EurekahedgeHF_Excel_aca_full2[,"Date_Added"],origin="1970-01-01",na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"Inception_Date"] <- as.Date(EurekahedgeHF_Excel_aca_full2[,"Inception_Date"],origin="1970-01-01",na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"chgdt"] <- as.Date(EurekahedgeHF_Excel_aca_full2[,"chgdt"],format="%Y-%m-%d",na.rm=TRUE)


#Create total fees, negative return, and yr_month
EurekahedgeHF_Excel_aca_full2[,"total_fee"] <- rowSums(EurekahedgeHF_Excel_aca_full2[,c("Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin")],na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]<0, EurekahedgeHF_Excel_aca_full2[,"mktadjret"], 0)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]), NA, EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"])
EurekahedgeHF_Excel_aca_full2[,"yr_month"] <- paste(EurekahedgeHF_Excel_aca_full2[,"yr"],sprintf("%02d", EurekahedgeHF_Excel_aca_full2[,"month"]),sep="_")

rm(EurekahedgeHF_Excel_aca_full)


monthly_data_all04 <- EurekahedgeHF_Excel_aca_full2
rm2(EurekahedgeHF_Excel_aca_full2)

#Strip out comments in parenetheses
monthly_data_all_strip_comments_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")

#Rename original columns
monthly_data_all05 <- rename.vars(monthly_data_all04, monthly_data_all_strip_comments_cols, paste(monthly_data_all_strip_comments_cols,"_org",sep=""))

rm2(monthly_data_all04)


strip_cols <- c(monthly_data_all_strip_comments_cols, paste(monthly_data_all_strip_comments_cols,"_comments",sep=""))

monthly_data_all06 <-  data.frame(monthly_data_all05, 
                                  matrix(NA, ncol=length(strip_cols), nrow=nrow(monthly_data_all05), dimnames=list(c(), strip_cols)), 
                                  stringsAsFactors=FALSE)

#monthly_data_all06 <- monthly_data_all06[,sort(colnames(monthly_data_all06), decreasing = FALSE)]

monthly_data_all06 <- strip_comments(monthly_data_all06,monthly_data_all_strip_comments_cols)
monthly_data_all06 <- as.data.frame(monthly_data_all06,stringsAsFactors=FALSE)

rm2(monthly_data_all05,monthly_data_all_strip_comments_cols,strip_cols)

#Get text before comments
monthly_data_all_yn_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all07 <- create_noncomments(monthly_data_all06,monthly_data_all_yn_cols)
monthly_data_all07 <- as.data.frame(monthly_data_all07,stringsAsFactors=FALSE)

rm2(monthly_data_all06,monthly_data_all_yn_cols)

#Check for uknowns
monthly_data_all_check_unknown_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all08 <- data.table(monthly_data_all07)[, (monthly_data_all_check_unknown_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), 
                                                      .SDcols = monthly_data_all_check_unknown_cols]
monthly_data_all08 <- as.data.frame(monthly_data_all08, stringsAsFactors=FALSE)

rm2(monthly_data_all07,monthly_data_all_check_unknown_cols)



#Change not specificied to NA
NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT APPILCABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","UPON REQUEST",
                "SUBJECT TO MANAGER'S DISCRETION")
monthly_data_all_not_specified_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all09 <- not_specified_to_na(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)
monthly_data_all09 <- as.data.frame(monthly_data_all09,stringsAsFactors=FALSE)

rm2(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)

#Change no phrases to NO
NO_Phrases <- c("NIL","NONE","NONE AFTER 12 MONTHS","NONE AFTER 1ST YEAR","NO DIVIDEND","NON DIVIDEND","LITTLE OR NO")

monthly_data_all_no_phrases_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all10 <- no_to_no(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)
monthly_data_all10 <- as.data.frame(monthly_data_all10,stringsAsFactors=FALSE)

rm2(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)

#Change yes phrases to YES
YES_Phrases <- c("RARELY","OCCASIONALLY")
monthly_data_all_yes_phrases_cols <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all11 <- yes_to_yes(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)
monthly_data_all11 <- as.data.frame(monthly_data_all11,stringsAsFactors=FALSE)

rm2(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)

#Change Y/N to binary
monthly_data_all_yn_to_bin_cols <-  c("Leverage", "Lockup")

bin_cols <- paste(monthly_data_all_yn_to_bin_cols,"_bin",sep="")

monthly_data_all12 <-  data.frame(monthly_data_all11, matrix(NA, ncol=length(bin_cols), nrow=nrow(monthly_data_all11), dimnames=list(c(), bin_cols)), stringsAsFactors=FALSE)
monthly_data_all12[,bin_cols] <- monthly_data_all12[,monthly_data_all_yn_to_bin_cols]

for(k in which(colnames(monthly_data_all12) %in% bin_cols))
{
  #k <- 1
  monthly_data_all12[[k]] <- ifelse(is.na(monthly_data_all12[[k]]),
                                    NA,ifelse(monthly_data_all12[[k]] %in% c("Yes","No"),monthly_data_all12[[k]],"Yes"))
  
}
rm2(k)

monthly_data_all12 <- yn_to_binary(monthly_data_all12,bin_cols)
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

monthly_data_all12 <- data.table(monthly_data_all12)[, (bin_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = bin_cols]
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

rm2(monthly_data_all11,monthly_data_all_yn_to_bin_cols,bin_cols)


#Fix Investment Size

monthly_data_all_size_cols <-  c("Minimum_Investment_Size", "Subsequent_Investment_Size")

size_cols <- paste(monthly_data_all_size_cols,"_bin",sep="")

monthly_data_all13 <-  data.frame(monthly_data_all12, matrix(NA, ncol=length(size_cols), nrow=nrow(monthly_data_all12), dimnames=list(c(), size_cols)), stringsAsFactors=FALSE)
monthly_data_all13[,size_cols] <- monthly_data_all13[,monthly_data_all_size_cols]

for(k in which(colnames(monthly_data_all13) %in% size_cols))
{
  #k <- 1
  
  monthly_data_all13[[k]] <-ifelse(monthly_data_all13[[k]] %in% c("no min","no Min","No min","No Min",
                                                                  "no minimum","no Minimum","No minimum","No Minimum",
                                                                  "no limitation","no Limitation","No limitation","No Limitation",
                                                                  "no limit","no Limit","No limit","No Limit",
                                                                  "no restriction","no Restriction","No restriction","No Restriction",
                                                                  "no","No"),"0",monthly_data_all13[[k]])
  monthly_data_all13[[k]] <-ifelse(grepl("(up to investor|any|no min|discretion)", monthly_data_all13[[k]], ignore.case=TRUE, perl=TRUE),"0",monthly_data_all13[[k]])
  monthly_data_all13[[k]] <- gsub(pattern="1 million", replacement="1000000", monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  
  monthly_data_all13[[k]] <- gsub(pattern="([[:alpha:]]|[[:punct:]])", replacement=" ", monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  monthly_data_all13[[k]] <- gsub("([^[:digit:]-\\+\\.])", " ",monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  monthly_data_all13[[k]] <- gsub(" {2,}", " ",monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  monthly_data_all13[[k]] <- gsub("^\\s+|\\s+$", "",monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  monthly_data_all13[[k]] <- gsub(" ", "",monthly_data_all13[[k]], perl=TRUE, ignore.case=TRUE)
  
  #monthly_data_all13[[k]] <-ifelse(monthly_data_all13[[k]]=="","0",monthly_data_all13[[k]])
  monthly_data_all13[[k]] <- ifelse(is.na(monthly_data_all13[[k]]),NA,monthly_data_all13[[k]])
  
}
rm2(k)

#Convert size to numeric
monthly_data_all_num_cols <- c(size_cols,"Fund_Size_USm")

for (i in 1:length(monthly_data_all_num_cols)) {
  #i <- 1
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- destring(monthly_data_all13[,monthly_data_all_num_cols[i]])
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- as.numeric(monthly_data_all13[,monthly_data_all_num_cols[i]])
}


#Create Domicile dummy

monthly_data_all13[,"Domicile"] <- gsub("[[:punct:]]", "", monthly_data_all13[,"Domicile"])
monthly_data_all13[,"Domicile"] <- gsub(" {2,}", " ",monthly_data_all13[,"Domicile"], perl=TRUE, ignore.case=TRUE)
monthly_data_all13[,"Domicile"] <- gsub("^\\s+|\\s+$", "",monthly_data_all13[,"Domicile"], perl=TRUE, ignore.case=TRUE)

monthly_data_all13[,"Domicile_onshore_bin"] <- 0
monthly_data_all13[,"Domicile_onshore_bin"] <- ifelse(is.na(monthly_data_all13[,"Domicile"]), NA, 
                                                      ifelse(toupper(monthly_data_all13[,"Domicile"])=="UNITED STATES", 1, 
                                                             ifelse(toupper(monthly_data_all13[,"Domicile"])=="USA", 1, 
                                                                    ifelse(toupper(monthly_data_all13[,"Domicile"])=="US", 1, monthly_data_all13[,"Domicile_onshore_bin"]))))



monthly_data_all14_cols0 <- c("Domicile","Currency_combcol","Geography_combcol","Leverage","Lockup","Minimum_Investment_Size","Subsequent_Investment_Size")
monthly_data_all14_cols1 <- paste(monthly_data_all14_cols0,"_org",sep="")
monthly_data_all14_cols2 <- paste(monthly_data_all14_cols0,"_comments",sep="")
monthly_data_all14_cols_remove <- sort(c(monthly_data_all14_cols0,monthly_data_all14_cols1,monthly_data_all14_cols2))


#Remove unwanted columns
monthly_data_all14 <- monthly_data_all13[,!(colnames(monthly_data_all13) %in% monthly_data_all14_cols_remove)]

rm2(monthly_data_all13,monthly_data_all14_cols0,monthly_data_all14_cols1,monthly_data_all14_cols2,monthly_data_all14_cols_remove)


#Make sure that fund has a strategy category

fund_type_remove_trim <- monthly_data_all14[monthly_data_all14["yr_month"]>="2007_01",c(identifier,"yr_month","Monthly_Ret","Primary_Investment_Strategy_combcol")]

fund_type_remove_u1 <- unique(fund_type_remove_trim[,c(identifier,"Primary_Investment_Strategy_combcol")])
fund_type_remove_u2 <- fund_type_remove_u1[!is.na(fund_type_remove_u1[,"Primary_Investment_Strategy_combcol"]),]

fund_type_remove <- fund_type_remove_u1[!(fund_type_remove_u1[,identifier] %in% unique(fund_type_remove_u2[,identifier])),]

#fund_type_remove <- monthly_data_all14[is.na(monthly_data_all14[,"main_investment_strategy"]),]
#fund_type_remove <- monthly_data_all14[is.na(monthly_data_all14[,"Primary_Investment_Strategy_combcol"]),]

fund_type_remove1 <- unique(fund_type_remove[,identifier])
fund_type_remove2 <- fund_type_remove1[!is.na(fund_type_remove1)]

monthly_data_all15 <- monthly_data_all14[!(monthly_data_all14[,identifier] %in% fund_type_remove2),]

rm2(monthly_data_all14)


#Make sure funds have atleast 24 months of consecutive returns

firm0 <- data.frame(monthly_data_all15[,c(identifier,"yr_month","Monthly_Ret")],NonNA_flag=NA,NonNA_cum_sum=NA,NA_flag=NA,NA_cum_sum=NA,stringsAsFactors=FALSE)

firm0[,"NonNA_flag"] <- ifelse(!is.na(firm0[,"Monthly_Ret"]),1,0)
firm0[,"NonNA_cum_sum"] <- cumsum(firm0[,"NonNA_flag"])

firm0[,"NA_flag"] <- ifelse(is.na(firm0[,"Monthly_Ret"]),1,0)
firm0[,"NA_cum_sum"] <- cumsum(firm0[,"NA_flag"])

firm1 <- count(firm0, c(identifier,"NA_cum_sum"))
firm1_mult <- firm1[firm1[,"freq"]>1,]
colnames(firm1_mult)[match("freq",names(firm1_mult))] <- "seq_flag"
firm1_mult[,"seq_flag"] <- seq(1,nrow(firm1_mult))

firm2 <- merge(firm0, firm1_mult, 
               by.x=c(identifier,"NA_cum_sum"), by.y=c(identifier,"NA_cum_sum"), 
               all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))
firm2 <- firm2[order(firm2[,identifier],firm2[,"yr_month"]),]
row.names(firm2) <- seq(nrow(firm2))

firm2[,"seq_flag"] <- ifelse(is.na(firm2[,"seq_flag"]),0,firm2[,"seq_flag"])

firm3 <- ddply(.data=firm2, .variables="seq_flag", .fun = function(x){
  out <- data.frame(x,seq_freq=NA,stringsAsFactors=FALSE)
  out[,"seq_freq"] <- c(NA,seq(1,nrow(out)-1))
  return(out)
},.progress = "text")

firm3[,"seq_freq"] <- ifelse(is.na(firm3[,"Monthly_Ret"]),NA,firm3[,"seq_freq"])
firm3 <- firm3[order(firm3[,identifier],firm3[,"yr_month"]),]
row.names(firm3) <- seq(nrow(firm3))

firm <- ddply(.data=firm3, .variables=identifier, .fun = function(x){
  return(unique(data.frame(ret_freq_overall=max(x[,"NonNA_cum_sum"],na.rm=TRUE),ret_freq_consecutive=max(x[,"seq_freq"],na.rm=TRUE),stringsAsFactors=FALSE)))
},.progress = "none")

firm_keep <- firm[firm[,"ret_freq_consecutive"]>=24,]
firm_keep <- firm_keep[!is.na(firm_keep[,c(identifier)]),]
row.names(firm_keep) <- seq(nrow(firm_keep))

monthly_data_all16 <- monthly_data_all15[(monthly_data_all15[,c(identifier)] %in% firm_keep[,c(identifier)]),]
row.names(monthly_data_all16) <- seq(nrow(monthly_data_all16))

rm(monthly_data_all15,firm,firm_keep)
rm(firm0,firm1,firm1_mult,firm2,firm3)


#Trim AUM
monthly_data_all17 <- monthly_data_all16
#monthly_data_all17 <- monthly_data_all16[!is.na(monthly_data_all16[,"AUM"]),]

rm2(monthly_data_all16)

monthly_data_all18 <- monthly_data_all17

#monthly_data_all18 <- monthly_data_all18[monthly_data_all18[,"AUM"]>=0.1,]

monthly_data_all18[,"AUM"] <- ifelse(monthly_data_all18[,"AUM"]<0.1,NA,monthly_data_all18[,"AUM"])
monthly_data_all18[,"AUM_lag1"] <- ifelse(monthly_data_all18[,"AUM_lag1"]<0.1,NA,monthly_data_all18[,"AUM_lag1"])
monthly_data_all18[,"AUM_lag2"] <- ifelse(monthly_data_all18[,"AUM_lag2"]<0.1,NA,monthly_data_all18[,"AUM_lag2"])
monthly_data_all18[,"AUM_lag3"] <- ifelse(monthly_data_all18[,"AUM_lag3"]<0.1,NA,monthly_data_all18[,"AUM_lag3"])
monthly_data_all18[,"AUM_lag4"] <- ifelse(monthly_data_all18[,"AUM_lag4"]<0.1,NA,monthly_data_all18[,"AUM_lag4"])

monthly_data_all18[,"AUM_log"] <- ifelse(monthly_data_all18[,"AUM_log"]<suppressWarnings(log(0.1)),NA,monthly_data_all18[,"AUM_log"])
monthly_data_all18[,"AUM_log_lag1"] <- ifelse(monthly_data_all18[,"AUM_log_lag1"]<suppressWarnings(log(0.1)),NA,monthly_data_all18[,"AUM_log_lag1"])
monthly_data_all18[,"AUM_log_lag2"] <- ifelse(monthly_data_all18[,"AUM_log_lag2"]<suppressWarnings(log(0.1)),NA,monthly_data_all18[,"AUM_log_lag2"])
monthly_data_all18[,"AUM_log_lag3"] <- ifelse(monthly_data_all18[,"AUM_log_lag3"]<suppressWarnings(log(0.1)),NA,monthly_data_all18[,"AUM_log_lag3"])
monthly_data_all18[,"AUM_log_lag4"] <- ifelse(monthly_data_all18[,"AUM_log_lag4"]<suppressWarnings(log(0.1)),NA,monthly_data_all18[,"AUM_log_lag4"])

rm2(monthly_data_all17)


#Finalize the data
monthly_data_all19 <- monthly_data_all18

rm2(monthly_data_all18)

monthly_data_all20 <- monthly_data_all19[rowSums(is.na(monthly_data_all19[,1:ncol(monthly_data_all19)]))<ncol(monthly_data_all19),]


monthly_data_all20 <- monthly_data_all20[order(monthly_data_all20[,identifier], 
                                               monthly_data_all20[,"yr"],
                                               monthly_data_all20[,"month"]),]

row.names(monthly_data_all20) <- seq(nrow(monthly_data_all20))

rm2(monthly_data_all19)


###############################################################################
cat("IMPORT AND FIX TEXT DATA", "\n")
###############################################################################

query_text_stats_ios_full <- ""
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "select       ",text_stats_ios_import_vars_keep2, sep=" ")
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "from         ",text_table, "                  ", sep=" ")
query_text_stats_ios_full <- trim(gsub(" {2,}", " ", query_text_stats_ios_full))

text_stats_ios_full <- data.frame(runsql(query_text_stats_ios_full,descriptive_stats_db),
                                  avg_grade_level_acf_ios=NA,
                                  avg_grade_level_ac_ios=NA,
                                  yr_month=NA,
                                  stringsAsFactors=FALSE)

#Create additonal average readbility measures
text_stats_ios_full[,"avg_grade_level_acf_ios"] <- rowMeans(text_stats_ios_full[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios")],na.rm=TRUE)
text_stats_ios_full[,"avg_grade_level_ac_ios"] <- rowMeans(text_stats_ios_full[,c("ARI_ios","Coleman_Liau_ios")],na.rm=TRUE)
text_stats_ios_full[,"yr_month"] <- paste(text_stats_ios_full[,"yr"],sprintf("%02d", text_stats_ios_full[,"month"]),sep="_")

#colnames(text_stats_ios_full) <- tolower(colnames(text_stats_ios_full))

text_stats_ios_full1 <- text_stats_ios_full[!(text_stats_ios_full[,identifier] %in% fund_type_remove2),]

rm2(text_stats_ios_full)

#text_stats_ios_yr_trim <- text_stats_ios_full1[(text_stats_ios_full1[,"yr"]>=beg_year & text_stats_ios_full1[,"yr"]<=end_year),]
text_stats_ios_yr_trim <- text_stats_ios_full1

rm2(text_stats_ios_full1)

text_stats_ios_sim_cols <- names(text_stats_ios_yr_trim)[grep("_similarity", names(text_stats_ios_yr_trim))] 

text_stats_ios <- text_stats_ios_yr_trim
for (i in 1:length(text_stats_ios_sim_cols)){ text_stats_ios <- text_stats_ios[!(is.na(text_stats_ios[,text_stats_ios_sim_cols[i]])),] }

text_stats_ios_trim <- text_stats_ios[((!is.na(text_stats_ios[,"ARI_ios"])) & 
                                         (!is.na(text_stats_ios[,"Coleman_Liau_ios"])) & 
                                         (!is.na(text_stats_ios[,"Flesch_Kincaid_ios"])) & 
                                         (!is.na(text_stats_ios[,"FOG_ios"])) & 
                                         (!is.na(text_stats_ios[,"SMOG_ios"]))),]

rm2(text_stats_ios_import_vars_keep2,text_stats_ios_yr_trim,text_stats_ios_sim_cols,text_stats_ios)


###############################################################################
cat("MERGE FUND AND TEXT DATA", "\n")
###############################################################################

data0 <- merge(monthly_data_all20, text_stats_ios_trim[,!(colnames(text_stats_ios_trim) %in% c("Primary_Investment_Strategy_combcol"))], 
               by.x=c(identifier,"yr","month","yr_month"), by.y=c(identifier,"yr","month","yr_month"), 
               all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

data0 <- data0[order(data0[,identifier],data0[,"yr_month"]),]
row.names(data0) <- seq(nrow(data0))

data1 <- data0[(data0[,"yr"]>=beg_year & data0[,"yr"]<=end_year),]

rm2(data0)

rm2(fund_table,text_table,fund_type_remove,fund_type_remove2)
#rm2(monthly_data_all20,text_stats_ios_trim)


###############################################################################
cat("IMPORT AND FIX FACTOR DATA", "\n")
###############################################################################


###############################################################################
cat("MERGE IN FACTORS AND ALPHAS", "\n")
###############################################################################


###############################################################################
cat("COMPUTE ALPHAS", "\n")
###############################################################################


###############################################################################
cat("MERGE IN FUNDS AND ALPHAS", "\n")
###############################################################################

data2_no_na <- data1

rm2(data1)

###############################################################################
cat("COMPUTE ADDITIONAL VARIABLES", "\n")
###############################################################################

#EXRET_squared
#EXRET_neg
#ALPHAS_squared
#ALPHAs_neg


###############################################################################
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("nflow","nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                    "sdnet_flow","sdnet_flow_lag1",
                    "pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                    "sdpct_flow","sdpct_flow_lag1",
                    #"exret","exret_lag1","exret_lag2","exret_lag3","exret_lag4",
                    #"minimum_investment_size",
                    "Sharpe_Ratio","Sortino_Ratio")

data2 <- data2_no_na
for (i in 1:length(winsorize_vars))
{
  data2[,winsorize_vars[i]] <- winsorize_both(data2[,winsorize_vars[i]],q=0.025)
} 
rm2(data2_no_na,winsorize_vars,i)


###############################################################################
cat("COMPUTE QUANTILES - IOS", "\n")
###############################################################################

descriptive_stats_by_var_year <- "yr"

fund_count_yr1 <- ddply(data2, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                      count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                      stringsAsFactors=FALSE)})
fund_count_yr2 <- data.frame(temp_var="ZZZ",
                             var="number_of_funds", 
                             count=as.numeric(length(unique(data2[,identifier],comparables=FALSE))),
                             stringsAsFactors=FALSE)
colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_year

fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)

rm(fund_count_yr1,fund_count_yr2)

data_temp_no_id1 <- data2[,!(colnames(data2) %in% identifier)]
data_temp_no_id2 <- data_temp_no_id1[,!(colnames(data_temp_no_id1) %in% c("month","yr_month","chgdt","date","Date_Added","Dead_Date","Inception_Date",
                                                                          "Primary_Investment_Strategy_combcol"))]

rm(data_temp_no_id1)

data_temp_no_id3 <- data_temp_no_id2[,as.vector(unlist(!(sapply(data_temp_no_id2, is.character))))]
row.names(data_temp_no_id3) <- seq(nrow(data_temp_no_id3))

rm(data_temp_no_id2)


quantile_vars_ios <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                       "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                       "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                       "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                       "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                       "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                       "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                       "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

#descriptive_stats_temp_full_all_var_year <- describeBy2(descrip_stats_fund2,"yr")
#descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)

data_temp_no_id4 <- data_temp_no_id3[,colnames(data_temp_no_id3) %in% c(descriptive_stats_by_var_year,quantile_vars_ios)]

rm(data_temp_no_id3)

descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id4, descriptive_stats_by_var_year)

rm(data_temp_no_id4)


###############################################################################
cat("COMPUTE DV FOR ABOVE AND BELOW SIMILARITY/READABILITY QUANTILE - IOS", "\n")
###############################################################################

#quantile_vars_data_ios <- descriptive_stats_temp_full_all_var_year[tolower(descriptive_stats_temp_full_all_var_year[,"var"]) %in% quantile_vars_ios,c("yr","var","quartile1","quartile3")] 
quantile_vars_data_ios <- descriptive_stats_temp_full_all_var_year[descriptive_stats_temp_full_all_var_year[,"var"] %in% quantile_vars_ios,c("yr","var","quartile1","quartile3")] 


quantile_vars_dv_temp_ios <- lapply(quantile_vars_ios,quantile_dvs,
                                    data=data2,
                                    group_var=c(identifier,"yr","month"),quantile_data=quantile_vars_data_ios,
                                    quantile_col_low="quartile1",quantile_col_high="quartile3")

quantile_vars_dv_temp2_ios <- do.call(cbind, quantile_vars_dv_temp_ios)
quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[order(quantile_vars_dv_temp2_ios[,identifier],
                                                               quantile_vars_dv_temp2_ios[,"yr"],
                                                               quantile_vars_dv_temp2_ios[,"month"]),]
row.names(quantile_vars_dv_temp2_ios) <- seq(nrow(quantile_vars_dv_temp2_ios))

quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[,unique(colnames(quantile_vars_dv_temp2_ios))]

rm2(quantile_vars_ios,quantile_vars_data_ios,quantile_vars_dv_temp_ios)


###############################################################################
cat("MERGE QUANTILE DVs", "\n")
###############################################################################

quantile_vars_dv <- quantile_vars_dv_temp2_ios
quantile_vars_dv <- quantile_vars_dv[order(quantile_vars_dv[,identifier],
                                           quantile_vars_dv[,"yr"],
                                           quantile_vars_dv[,"month"]),]
row.names(quantile_vars_dv) <- seq(nrow(quantile_vars_dv))

data_all <- merge(data2, quantile_vars_dv, 
                  by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

data_all <- data_all[order(data_all[,identifier],
                           data_all[,"yr"],
                           data_all[,"month"],
                           data_all[,"yr_month"]),]
row.names(data_all) <- seq(nrow(data_all))


###############################################################################
cat("OUTPUT DATA", "\n")
###############################################################################

ExportTable(data_fulll_db,"data_prescreen",data_all)
write.csv(data_all,file=paste(output_directory,"data_prescreen.csv",sep="\\"),na="",quote=TRUE,row.names=FALSE)
