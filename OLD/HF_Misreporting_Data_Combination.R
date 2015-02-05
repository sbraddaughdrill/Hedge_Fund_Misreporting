# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Data_Combintion.R
# Version: 1.0
# Date:    09.08.2014
# Purpose: Combine all data sources
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

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1


if (Location == 1) {

  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE) 
  
} else if (Location == 2) {

  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 3) {

  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {

  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE) 
  
} else if (Location == 5) {

  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)       
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)



###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_finance.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","DataCombine","fastmatch","foreign","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
                       "pander","pbapply","plm","plyr","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
                       "sandwich","sqldf","stargazer","stringr","texreg","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")


###############################################################################
cat("IMPORT READABILITY TEXT DATA", "\n")
###############################################################################

identifier <- "fund_id"

#Import .CSV files

read_stats_ios_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

read_stats_ios_f <- trim_by_format(read_stats_ios_f,"character")

read_stats_ios_f <- data.table(read_stats_ios_f)[, (colnames(read_stats_ios_f)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(read_stats_ios_f)]
read_stats_ios_f <- as.data.frame(read_stats_ios_f,stringsAsFactors=FALSE)

sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

sample_data_all <- trim_by_format(sample_data_all,"character")

sample_data_all <- data.table(sample_data_all)[, (colnames(sample_data_all)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(sample_data_all)]
sample_data_all <- as.data.frame(sample_data_all,stringsAsFactors=FALSE)

#read_stats_ios_f <- subset(read_stats_ios_f,select=-c(foreign_ios))
read_stats_ios_f <- read_stats_ios_f[,!(colnames(read_stats_ios_f) %in% "foreign_ios")]

#sample_data_all <- subset(sample_data_all,select=-c(Strategy))
sample_data_all <- sample_data_all[,!(colnames(sample_data_all) %in% "Strategy")]

colnames(read_stats_ios_f) <- tolower(colnames(read_stats_ios_f))
colnames(sample_data_all) <- tolower(colnames(sample_data_all))


###############################################################################
cat("IMPORT SIMILARITY TEXT DATA", "\n")
###############################################################################

text_variables <- c("ios")
text_percentages <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#text_percentages <- c("900pct")

temp_input_data_name_short <- "year_id_unique_sim_cosine_normalized"

sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], 
                                                   sample_data_all_temp[,"yr"]),]
row.names(sample_data_all_temp) <- seq(nrow(sample_data_all_temp))

for (j in 1:length(text_variables))
{
  #j <- 1
  
  assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
  
}
rm2(sample_data_all_temp)

for (i in 1:length(text_variables))
{
  #i <- 1
  
  for (j in 1:length(text_percentages))
  {
    
    #j <- 1
    
    temp_input_data_name_full <- paste(temp_input_data_name_short,text_percentages[j],text_variables[i],"avg",sep="_")
    
    year_sim <- as.data.frame(fread(paste(output_directory,temp_input_data_name_full,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),
                              stringsAsFactors=FALSE)
    
    colnames(year_sim) <- tolower(colnames(year_sim))
    
    year_sim_char_cols <- colnames(year_sim)[laply(year_sim,class,.progress = "text",.drop = FALSE)=="character"]
    year_sim <- trim_dt(year_sim,year_sim_char_cols)
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    year_sim <- data.table(year_sim)[, (colnames(year_sim)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), 
                                     .SDcols = colnames(year_sim)]
    year_sim <- as.data.frame(year_sim,stringsAsFactors=FALSE)
    
    year_sim_years <- colnames(year_sim)
    year_sim_years <- year_sim_years[!(year_sim_years==identifier)] 
    
    assign(paste("years",text_variables[i],text_percentages[j],sep="_"), year_sim_years, envir=.GlobalEnv)
    
    year_sim_stacked0 <- lapply(year_sim_years, function(x,data){ data.frame(identifier=data[,identifier], 
                                                                             yr=as.integer(x), 
                                                                             similarity_all_ios=data[,x], stringsAsFactors=FALSE) }, data = year_sim)
    
    year_sim_stacked <- do.call(rbind, year_sim_stacked0) 
    colnames(year_sim_stacked)[match("identifier",names(year_sim_stacked))] <- identifier
    
    year_sim_stacked <- year_sim_stacked[order(year_sim_stacked[,identifier],
                                               year_sim_stacked[,"yr"]),] 
    row.names(year_sim_stacked) <- seq(nrow(year_sim_stacked))
    
    colnames(year_sim_stacked) <- c(identifier,"yr",paste("similarity",text_percentages[j],text_variables[i],sep="_"))
    
    if (text_variables[i]=="ios")
    {
      year_sim_ios_all_stacked <- merge(year_sim_ios_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else if (text_variables[i]=="pr")
    {
      year_sim_pr_all_stacked <- merge(year_sim_pr_all_stacked, year_sim_stacked, by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_variables), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_percentages))
    
    rm2(year_sim,year_sim_years,year_sim_stacked0,year_sim_stacked)
    
  }
  
} 

year_sim_ios_all_stacked <- year_sim_ios_all_stacked[!(rowSums(is.na(year_sim_ios_all_stacked[,3:ncol(year_sim_ios_all_stacked)]))==(ncol(year_sim_ios_all_stacked)-2)),]
year_sim_ios_all_stacked <- year_sim_ios_all_stacked[order(year_sim_ios_all_stacked[,identifier], 
                                                           year_sim_ios_all_stacked[,"yr"]),]
row.names(year_sim_ios_all_stacked) <- seq(nrow(year_sim_ios_all_stacked))

colnames(year_sim_ios_all_stacked) <- paste("all",colnames(year_sim_ios_all_stacked),sep="_")
colnames(year_sim_ios_all_stacked)[1:2] <- c(identifier,"yr")

colnames(year_sim_ios_all_stacked) <- tolower(colnames(year_sim_ios_all_stacked))


###############################################################################
cat("IMPORT CRSP DATA", "\n")
###############################################################################

#crsp_fundno_unique <- data.frame(crsp_fundno=unique(sample_data_all[,c("crsp_fundno")], incomparables=FALSE),stringsAsFactors=FALSE)

#ExportTable(crsp_db,"crsp_fundno_unique",crsp_fundno_unique)

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

Crspa_msi <- runsql("SELECT * FROM Crspa_msi",crsp_db)

Crspa_msi <- trim_by_format(Crspa_msi,"character")

Crspa_msi <- data.table(Crspa_msi)[, (colnames(Crspa_msi)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(Crspa_msi)]
Crspa_msi <- as.data.frame(Crspa_msi,stringsAsFactors=FALSE)

#DeleteTable(crsp_db,"crsp_fundno_unique")

colnames(Crspa_msi) <- tolower(colnames(Crspa_msi))

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)


###############################################################################
cat("CODE MISSING VARIABLES AS NA", "\n")
###############################################################################

Crspa_msi[,"vwretd"] <- ifelse(Crspa_msi[,"vwretd"]==-99, NA, Crspa_msi[,"vwretd"])

Crspa_msi[,"vwretx"] <- ifelse(Crspa_msi[,"vwretx"]==-99, NA, Crspa_msi[,"vwretx"])

###############################################################################
cat("CONVERT RETURNS TO MONTHLY", "\n")
###############################################################################

Crspa_msi_monthly <- data.frame(vwretd_annualized=Crspa_msi[,"vwretd"],
                                   vwretx_annualized=Crspa_msi[,"vwretx"],
                                   Crspa_msi,
                                   stringsAsFactors=FALSE)

Crspa_msi_monthly[,"vwretd"] <- (((Crspa_msi_monthly[,"vwretd"] + 1)^(1/12))-1)
Crspa_msi_monthly[,"vwretx"] <- (((Crspa_msi_monthly[,"vwretx"] + 1)^(1/12))-1)

rm2(Crspa_msi)


###############################################################################
cat("CREATE YEAR AND MONTH VARIABLE FOR Crspa_msi", "\n")
###############################################################################

crspa_msi_full <- transform(Crspa_msi_monthly,yr=year(as.IDate(caldt)),month=month(as.IDate(caldt)))

#crspa_msi_trim <- subset(crspa_msi_full,select=c(yr,month,vwretd,vwretx))
crspa_msi_trim <- crspa_msi_full[,(colnames(crspa_msi_full) %in% c("yr","month","vwretd_annualized","vwretd","vwretx_annualized","vwretx"))]
crspa_msi_trim <- crspa_msi_trim[,c("yr","month","vwretd_annualized","vwretd","vwretx_annualized","vwretx")]


###############################################################################
cat("COMPUTE MONTHLY FUND RETURN VOLATILITY", "\n")
###############################################################################

# daily_returns_full <- transform(daily_returns,yr=year(as.IDate(caldt)),month=month(as.IDate(caldt)))

# # daily_returns_trim <- subset(daily_returns_full,select=-c(caldt))
# daily_returns_trim <- daily_returns_full[,!(colnames(daily_returns_full) %in% "caldt")]

# fund_mret_volatility <- ddply(daily_returns_trim, c("crsp_fundno","yr","month"), summarize, sddret = sd(dret, na.rm = TRUE))


###############################################################################
cat("IMPORT EUREKA HEDGE DATA", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full <- read.csv(file=paste(output_directory,"EurekahedgeHF_Excel_aca_merge",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[rowSums(is.na(EurekahedgeHF_Excel_aca_full[,1:ncol(EurekahedgeHF_Excel_aca_full)]))<ncol(EurekahedgeHF_Excel_aca_full),]

colnames(EurekahedgeHF_Excel_aca_full) <- tolower(colnames(EurekahedgeHF_Excel_aca_full))


###############################################################################
cat("REMOVE DECIMALS IN COLUMN NAMES", "\n")
###############################################################################

colnames(EurekahedgeHF_Excel_aca_full) <- gsub("\\.","_",colnames(EurekahedgeHF_Excel_aca_full))
colnames(EurekahedgeHF_Excel_aca_full) <- gsub("__","_",colnames(EurekahedgeHF_Excel_aca_full))


###############################################################################
cat("INITIAL CLEANING OF EUREKA HEDGE DATA", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full  <- EurekahedgeHF_Excel_aca_full[order(EurekahedgeHF_Excel_aca_full[,identifier],
                                                                    EurekahedgeHF_Excel_aca_full[,"fund_name"],
                                                                    EurekahedgeHF_Excel_aca_full[,"yr"]),]
row.names(EurekahedgeHF_Excel_aca_full) <- seq(nrow(EurekahedgeHF_Excel_aca_full))

EurekahedgeHF_Excel_aca_full <- trim_by_format(EurekahedgeHF_Excel_aca_full,"character")

EurekahedgeHF_Excel_aca_full <- data.table(EurekahedgeHF_Excel_aca_full)[, (colnames(EurekahedgeHF_Excel_aca_full)) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = colnames(EurekahedgeHF_Excel_aca_full)]
EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full_char_to_date_cols <- c("date_added","dead_date","inception_date","date")

EurekahedgeHF_Excel_aca_full <- char_to_date_dt(EurekahedgeHF_Excel_aca_full,EurekahedgeHF_Excel_aca_full_char_to_date_cols,"%Y-%m-%d")
EurekahedgeHF_Excel_aca_full <- as.data.frame(EurekahedgeHF_Excel_aca_full,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full <- EurekahedgeHF_Excel_aca_full[rowSums(is.na(EurekahedgeHF_Excel_aca_full[,1:ncol(EurekahedgeHF_Excel_aca_full)]))<ncol(EurekahedgeHF_Excel_aca_full),]


###############################################################################
cat("STRIP COMMENTS FROM VARIABLES", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full3 <- EurekahedgeHF_Excel_aca_full

#Strip out comments in parenetheses
EurekahedgeHF_Excel_aca_full_strip_comments_cols <- c("dividend_policy","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                                      "management_fee","other_fee","performance_fee")

#Rename original columns
EurekahedgeHF_Excel_aca_full3 <- rename.vars(EurekahedgeHF_Excel_aca_full3, 
                                             EurekahedgeHF_Excel_aca_full_strip_comments_cols, 
                                             paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols,"_org",sep=""))

strip_cols <- c(EurekahedgeHF_Excel_aca_full_strip_comments_cols, 
                paste(EurekahedgeHF_Excel_aca_full_strip_comments_cols,"_comments",sep=""))

EurekahedgeHF_Excel_aca_full4 <-  data.frame(EurekahedgeHF_Excel_aca_full3, 
                                             matrix(NA, ncol=length(strip_cols), nrow=nrow(EurekahedgeHF_Excel_aca_full3), dimnames=list(c(), strip_cols)), 
                                             stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[,sort(colnames(EurekahedgeHF_Excel_aca_full4), decreasing = FALSE)]

EurekahedgeHF_Excel_aca_full4 <- strip_comments(EurekahedgeHF_Excel_aca_full4,EurekahedgeHF_Excel_aca_full_strip_comments_cols)
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4,stringsAsFactors=FALSE)

#Get text before comments
EurekahedgeHF_Excel_aca_full_yn_cols <- c("dividend_policy","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                          "management_fee","other_fee","performance_fee")
EurekahedgeHF_Excel_aca_full4 <- create_noncomments(EurekahedgeHF_Excel_aca_full4,EurekahedgeHF_Excel_aca_full_yn_cols)
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4,stringsAsFactors=FALSE)

#Check for uknowns
EurekahedgeHF_Excel_aca_full_check_unknown_cols <- c("dividend_policy","exchange_name","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                                     "management_fee","other_fee","performance_fee")
EurekahedgeHF_Excel_aca_full4 <- data.table(EurekahedgeHF_Excel_aca_full4)[, (EurekahedgeHF_Excel_aca_full_check_unknown_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), 
                                                                           .SDcols = EurekahedgeHF_Excel_aca_full_check_unknown_cols]
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4, stringsAsFactors=FALSE)

#Change not specificied to NA
NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT APPILCABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","UPON REQUEST",
                "SUBJECT TO MANAGER'S DISCRETION")
EurekahedgeHF_Excel_aca_full_not_specified_cols <- c("dividend_policy","domicile","exchange_name","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                                     "management_fee","other_fee","performance_fee","fund_size_us_m")
EurekahedgeHF_Excel_aca_full4 <- not_specified_to_na(EurekahedgeHF_Excel_aca_full4,EurekahedgeHF_Excel_aca_full_not_specified_cols,NA_Phrases)
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4,stringsAsFactors=FALSE)


#Change no phrases to NO
NO_Phrases <- c("NIL","NONE","NONE AFTER 12 MONTHS","NONE AFTER 1ST YEAR","NO DIVIDEND","NON DIVIDEND","LITTLE OR NO")

EurekahedgeHF_Excel_aca_full_no_phrases_cols <- c("dividend_policy","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                                  "management_fee","other_fee","performance_fee")
EurekahedgeHF_Excel_aca_full4 <- no_to_no(EurekahedgeHF_Excel_aca_full4,EurekahedgeHF_Excel_aca_full_no_phrases_cols,NO_Phrases)
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4,stringsAsFactors=FALSE)


#Change yes phrases to YES
YES_Phrases <- c("RARELY","OCCASIONALLY")
EurekahedgeHF_Excel_aca_full_yes_phrases_cols <- c("dividend_policy","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange",
                                                   "management_fee","other_fee","performance_fee")
EurekahedgeHF_Excel_aca_full4 <- yes_to_yes(EurekahedgeHF_Excel_aca_full4,EurekahedgeHF_Excel_aca_full_yes_phrases_cols,YES_Phrases)
EurekahedgeHF_Excel_aca_full4 <- as.data.frame(EurekahedgeHF_Excel_aca_full4,stringsAsFactors=FALSE)

#Convert fee columns to numeric
EurekahedgeHF_Excel_aca_full4_num_cols <- c("management_fee","other_fee","performance_fee")
for (i in 1:length(EurekahedgeHF_Excel_aca_full4_num_cols)) {
  #i <- 1
  EurekahedgeHF_Excel_aca_full4[,EurekahedgeHF_Excel_aca_full4_num_cols[i]] <- 
    as.numeric(EurekahedgeHF_Excel_aca_full4[,EurekahedgeHF_Excel_aca_full4_num_cols[i]])
}
#temp <- unique(EurekahedgeHF_Excel_aca_full4[,c("fund_closed","fund_closed_comments","fund_closed_org")])
#temp <- unique(EurekahedgeHF_Excel_aca_full4[,c("dividend_policy","dividend_policy_comments","dividend_policy_org")])
#temp <- unique(EurekahedgeHF_Excel_aca_full4[,c("performance_fee","performance_fee_comments","performance_fee_org")])

#Finalize the data
EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[rowSums(is.na(EurekahedgeHF_Excel_aca_full4[,1:ncol(EurekahedgeHF_Excel_aca_full4)]))<ncol(EurekahedgeHF_Excel_aca_full4),]


EurekahedgeHF_Excel_aca_full4 <- EurekahedgeHF_Excel_aca_full4[order(EurekahedgeHF_Excel_aca_full4[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full4[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full4[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full4) <- seq(nrow(EurekahedgeHF_Excel_aca_full4))

rm2(EurekahedgeHF_Excel_aca_full3)


###############################################################################
cat("CHANGE Y/N TO BINARY", "\n")
###############################################################################

#EurekahedgeHF_Excel_aca_full_yn_to_bin_cols <- c("flagship","closed","limited","dead","invest_in_private_placements","managed_accounts_offered","ucits",
#                                                 "dividend_policy","exchange_name","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange")
EurekahedgeHF_Excel_aca_full_yn_to_bin_cols <-  c("flagship","closed","limited","dead","invest_in_private_placements","managed_accounts_offered","ucits",
                                                  "dividend_policy","fund_closed","high_water_mark","hurdle_rate","listed_on_exchange")

bin_cols <- paste(EurekahedgeHF_Excel_aca_full_yn_to_bin_cols,"_bin",sep="")

EurekahedgeHF_Excel_aca_full5 <-  data.frame(EurekahedgeHF_Excel_aca_full4, matrix(NA, ncol=length(bin_cols), nrow=nrow(EurekahedgeHF_Excel_aca_full4), dimnames=list(c(), bin_cols)), stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full5[,bin_cols] <-  EurekahedgeHF_Excel_aca_full5[,EurekahedgeHF_Excel_aca_full_yn_to_bin_cols]

EurekahedgeHF_Excel_aca_full5 <- yn_to_binary(EurekahedgeHF_Excel_aca_full5,bin_cols)
EurekahedgeHF_Excel_aca_full5 <- as.data.frame(EurekahedgeHF_Excel_aca_full5,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full5 <- data.table(EurekahedgeHF_Excel_aca_full5)[, (bin_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = bin_cols]
EurekahedgeHF_Excel_aca_full5 <- as.data.frame(EurekahedgeHF_Excel_aca_full5,stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[rowSums(is.na(EurekahedgeHF_Excel_aca_full5[,1:ncol(EurekahedgeHF_Excel_aca_full5)]))<ncol(EurekahedgeHF_Excel_aca_full5),]


EurekahedgeHF_Excel_aca_full5 <- EurekahedgeHF_Excel_aca_full5[order(EurekahedgeHF_Excel_aca_full5[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full5[,"yr"],
                                                                     EurekahedgeHF_Excel_aca_full5[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full5) <- seq(nrow(EurekahedgeHF_Excel_aca_full5))

rm2(EurekahedgeHF_Excel_aca_full4)


###############################################################################
cat("SPLIT (EXPAND) COLUMNS", "\n")
###############################################################################

split_cols <- c("custodian","principal_prime_broker_broker","secondary_prime_broker_broker",
                "synthetic_prime_broker","legal_advisor_offshore","legal_advisor_onshore")

#test <- EurekahedgeHF_Excel_aca_full5[,split_cols]
#test2 <- unique(test[,colnames(test)])

for (i in 1:length(split_cols))
{
  #i <- 2
  
  temp1 <- str_split(EurekahedgeHF_Excel_aca_full5[,split_cols[i]], ",")
  
  temp1_len <- unlist(lapply(temp1, function(x) length(x)))
  temp1_len_max <- as.integer(tail(sort(temp1_len, decreasing = FALSE),1))
  
  temp2 <- lapply(temp1, function(x,max_len){if (length(x) != (max_len+1)) append(x,matrix(NA, ncol=max_len-length(x)+1, nrow=1)) else x}, max_len = temp1_len_max)
  
  temp3 <- do.call(rbind, temp2)
  
  temp4 <- as.data.frame(temp3,stringsAsFactors=FALSE)
  colnames(temp4) <- c(paste(split_cols[i],1:(ncol(temp4)-1),sep=""),paste(split_cols[i],"_count",sep=""))
  
  for (j in 1:ncol(temp4))
  {
    #j <- 1
    
    temp4[,j] <- trim(temp4[,j])
    
  }
  
  temp4[,paste(split_cols[i],"_count",sep="")] <- apply(temp4,1,function(x) sum(!is.na(x[1:(length(x)-1)])))
  
  temp4[,paste(split_cols[i],"_count",sep="")] <- 
    ifelse(toupper(temp4[,paste(split_cols[i],"_count",sep="")])=="0", NA, temp4[,paste(split_cols[i],"_count",sep="")])
  
  
  if (i==1)
  {
    output <- temp4
    
  } else
  {
    output <- cbind(output,temp4)
  }
  
  cat("Loop: ",i," of ",length(split_cols), "\n")
} 


EurekahedgeHF_Excel_aca_full6 <- cbind(EurekahedgeHF_Excel_aca_full5,output)

rm2(temp1,temp2,temp3,temp4,output,EurekahedgeHF_Excel_aca_full5)


###############################################################################
cat("CONVERT PERCENTAGES TO DECIMAL", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full6b <- EurekahedgeHF_Excel_aca_full6

decimal_cols <- c("yearly_ret","monthly_ret","monthly_ret2","annualised_return","best_monthly_return","worst_monthly_return",
                  "rise_in_nav_since_inception","last_3_months","one_year_rolling_return","two_year_rolling_return","five_year_rolling_return",
                  "annualised_standard_deviation","downside_deviation","upside_deviation","maximum_drawdown",
                  "percentage_of_positive_months","var_90pct","var_95pct","var_99pct",
                  "management_fee","performance_fee","other_fee")


for (i in 1:length(decimal_cols))
{
  #i <- 1
  
  EurekahedgeHF_Excel_aca_full6b[,decimal_cols[i]] <- (EurekahedgeHF_Excel_aca_full6b[,decimal_cols[i]]/100)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(decimal_cols), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

EurekahedgeHF_Excel_aca_full6b <- EurekahedgeHF_Excel_aca_full6b[rowSums(is.na(EurekahedgeHF_Excel_aca_full6b[,1:ncol(EurekahedgeHF_Excel_aca_full6b)]))<ncol(EurekahedgeHF_Excel_aca_full6b),]

rm2(EurekahedgeHF_Excel_aca_full)


###############################################################################
cat("CONVERT AUM TO MILLIONS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full6c <- EurekahedgeHF_Excel_aca_full6b
EurekahedgeHF_Excel_aca_full6c[,"aum"] <- (as.numeric(EurekahedgeHF_Excel_aca_full6c[,"aum"])*1000000)

rm2(EurekahedgeHF_Excel_aca_full6b)


###############################################################################
cat("GET TNA FOR EACH CRSP_FUNDNO", "\n")
###############################################################################
# 
# monthly_tna_full <- merge(mflink1, monthly_tna_ret_nav2, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# # monthly_tna_full <- subset(monthly_tna_full,select=-c(caldt,mnav,mret))
# monthly_tna_full <- monthly_tna_full[,!(colnames(monthly_tna_full) %in% c("caldt","mnav","mret"))]
# 
# monthly_tna_full <- monthly_tna_full[c(identifier,"crsp_fundno","yr","month","mtna")]
# 
# monthly_tna_full <- monthly_tna_full[order(monthly_tna_full[,identifier], 
#                                            monthly_tna_full[,"crsp_fundno"], 
#                                            monthly_tna_full[,"yr"],
#                                            monthly_tna_full[,"month"]),]
# row.names(monthly_tna_full) <- seq(nrow(monthly_tna_full))
EurekahedgeHF_Excel_aca_full6d <- EurekahedgeHF_Excel_aca_full6c


#TRIM AUM
#aum_q <- 0.005
#aum_extrema <- quantile(EurekahedgeHF_Excel_aca_full6d[,"aum"], c(aum_q, 1-aum_q)) 
#EurekahedgeHF_Excel_aca_full6d <- EurekahedgeHF_Excel_aca_full6d[EurekahedgeHF_Excel_aca_full6d[,"aum"]<aum_extrema[2],]

EurekahedgeHF_Excel_aca_full6d[,"aum"] <- winsorize_top(EurekahedgeHF_Excel_aca_full6d[,"aum"],q=0.025)

rm2(EurekahedgeHF_Excel_aca_full6c)


###############################################################################
cat("MERGE MONTHLY_TNA_RET_NAV2 AND INDEX RETURNS/FUND VOLATILITY", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full7 <- merge(EurekahedgeHF_Excel_aca_full6d, crspa_msi_trim, 
                                       by.x=c("yr","month"), by.y=c("yr","month"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

#EurekahedgeHF_Excel_aca_full7 <- transform(EurekahedgeHF_Excel_aca_full7, mktadjret=monthly_ret-vwretx)
EurekahedgeHF_Excel_aca_full7 <- transform(EurekahedgeHF_Excel_aca_full7, mktadjret=monthly_ret-vwretd)

EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[rowSums(is.na(EurekahedgeHF_Excel_aca_full7[,1:ncol(EurekahedgeHF_Excel_aca_full7)]))<ncol(EurekahedgeHF_Excel_aca_full7),]

EurekahedgeHF_Excel_aca_full7 <- EurekahedgeHF_Excel_aca_full7[order(EurekahedgeHF_Excel_aca_full7[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full7[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full7[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full7) <- seq(nrow(EurekahedgeHF_Excel_aca_full7))

rm2(EurekahedgeHF_Excel_aca_full6d,crspa_msi_trim)

#temp <- EurekahedgeHF_Excel_aca_full7[,c("yr","month","fund_id","fund_name","mktadjret","monthly_ret","vwretd")]


###############################################################################
cat("WINSORIZE VALUES OF RETURN", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full7b <- EurekahedgeHF_Excel_aca_full7

monthly_tna_ret_nav2_vars <- c("monthly_ret","mktadjret")
for (i in 1:length(monthly_tna_ret_nav2_vars))
{
  #i <- 1
  #i <- 2
  EurekahedgeHF_Excel_aca_full7b[,monthly_tna_ret_nav2_vars[i]] <- 
    winsorize_both(EurekahedgeHF_Excel_aca_full7b[,monthly_tna_ret_nav2_vars[i]],q=0.01)
  
} 

rm2(EurekahedgeHF_Excel_aca_full7)

#temp <- EurekahedgeHF_Excel_aca_full7b[,c("yr","month","fund_id","fund_name","mktadjret","monthly_ret","vwretd")]

###############################################################################
cat("WINSORIZE VALUES OF FEES", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full8 <- EurekahedgeHF_Excel_aca_full7b

fund_fees_month_vars <- c("fund_capacity_us_m","maximum_drawdown","minimum_investment_size","management_fee", "performance_fee","other_fee")

for (i in 1:length(fund_fees_month_vars))
{
  #i <- 1
  #i <- 2
  EurekahedgeHF_Excel_aca_full8[,fund_fees_month_vars[i]] <- 
    winsorize_both(EurekahedgeHF_Excel_aca_full8[,fund_fees_month_vars[i]],q=0.01)
  
} 

rm2(EurekahedgeHF_Excel_aca_full7b)

#colMeans(EurekahedgeHF_Excel_aca_full8[,c("mktadjret","monthly_ret","vwretd")], na.rm = TRUE)


###############################################################################
cat("ADD ALL DATES", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full8_min_date1 <- min(EurekahedgeHF_Excel_aca_full8[,"date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_min_date2 <- min(EurekahedgeHF_Excel_aca_full8[,"dead_date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_min_date3 <- min(EurekahedgeHF_Excel_aca_full8[,"inception_date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_min_date4 <- min(EurekahedgeHF_Excel_aca_full8[,"date_added"],na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_min_date <- min(EurekahedgeHF_Excel_aca_full8_min_date1,
                                               EurekahedgeHF_Excel_aca_full8_min_date2,
                                               EurekahedgeHF_Excel_aca_full8_min_date3,
                                               EurekahedgeHF_Excel_aca_full8_min_date4,na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_max_date1 <- max(EurekahedgeHF_Excel_aca_full8[,"date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_max_date2 <- max(EurekahedgeHF_Excel_aca_full8[,"dead_date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_max_date3 <- max(EurekahedgeHF_Excel_aca_full8[,"inception_date"],na.rm = TRUE)
EurekahedgeHF_Excel_aca_full8_max_date4 <- max(EurekahedgeHF_Excel_aca_full8[,"date_added"],na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_max_date <- max(EurekahedgeHF_Excel_aca_full8_max_date1,
                                              EurekahedgeHF_Excel_aca_full8_max_date2,
                                              EurekahedgeHF_Excel_aca_full8_max_date3,
                                              EurekahedgeHF_Excel_aca_full8_max_date4,na.rm = TRUE)

EurekahedgeHF_Excel_aca_full8_all_fund_ids <- unique(EurekahedgeHF_Excel_aca_full8[,c(identifier)])
EurekahedgeHF_Excel_aca_full8_all_fund_ids <- data.frame(EurekahedgeHF_Excel_aca_full8_all_fund_ids,stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids)[1] <- identifier

EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- data.table(EurekahedgeHF_Excel_aca_full8_all_fund_ids)[, seq(as.Date(EurekahedgeHF_Excel_aca_full8_min_date), 
                                                                                                                  as.Date(EurekahedgeHF_Excel_aca_full8_max_date), 
                                                                                                                  by = "month"),
                                                                                                            by=eval(paste(identifier,sep=""))]
EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- data.frame(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand,
                                                       yr=NA,
                                                       month=NA,
                                                       stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand)[2] <- "date"

EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"yr"] <- year(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"date"])
EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"month"] <- month(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,"date"])
EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand <- EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand[,!(colnames(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand) %in% "date")]

EurekahedgeHF_Excel_aca_full8_expand <- merge(EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand, EurekahedgeHF_Excel_aca_full8, 
                                       by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                                       all.x=TRUE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_full8_expand <- EurekahedgeHF_Excel_aca_full8_expand[order(EurekahedgeHF_Excel_aca_full8_expand[,identifier], 
                                                                                   EurekahedgeHF_Excel_aca_full8_expand[,"yr"], 
                                                                                   EurekahedgeHF_Excel_aca_full8_expand[,"month"]),] 

row.names(EurekahedgeHF_Excel_aca_full8_expand) <- seq(nrow(EurekahedgeHF_Excel_aca_full8_expand))
#EurekahedgeHF_Excel_aca_full8_expand <- EurekahedgeHF_Excel_aca_full8_expand[rowSums(is.na(EurekahedgeHF_Excel_aca_full8_expand[,1:ncol(EurekahedgeHF_Excel_aca_full8_expand)]))<ncol(EurekahedgeHF_Excel_aca_full8_expand),]


#temp2 <- EurekahedgeHF_Excel_aca_full8_expand[EurekahedgeHF_Excel_aca_full8_expand[,identifier]==6131,c(identifier,"yr","month","aum","monthly_ret")]



rm2(EurekahedgeHF_Excel_aca_full8_min_date,EurekahedgeHF_Excel_aca_full8_min_date1,EurekahedgeHF_Excel_aca_full8_min_date2)
rm2(EurekahedgeHF_Excel_aca_full8_min_date3,EurekahedgeHF_Excel_aca_full8_min_date4)
rm2(EurekahedgeHF_Excel_aca_full8_max_date,EurekahedgeHF_Excel_aca_full8_max_date1,EurekahedgeHF_Excel_aca_full8_max_date2)
rm2(EurekahedgeHF_Excel_aca_full8_max_date3,EurekahedgeHF_Excel_aca_full8_max_date4)
rm2(EurekahedgeHF_Excel_aca_full8_all_fund_ids,EurekahedgeHF_Excel_aca_full8_all_fund_ids_expand)
rm2(EurekahedgeHF_Excel_aca_full8)


###############################################################################
cat("CREATE LAG OF AUM AND RETURNS", "\n")
###############################################################################

#duplicates1 <- sqldf("SELECT *, count(Fund_ID) as count FROM EurekahedgeHF_Excel_aca_full6 group by Fund_ID,date")
#duplicates2 <- sqldf("SELECT * FROM duplicates1 where count<>1")
#data_u <- unique(data_in[,c(group,time)])

#returns_short <- returns[(returns[,identifier]==5002 | returns[,identifier]==5003) ,]

lag_vars <- c("aum","monthly_ret","mktadjret")
lag_count <- 4

#[,c(identifier,"yr","month","date",lag_vars)]

EurekahedgeHF_Excel_aca_full9 <- data.frame(EurekahedgeHF_Excel_aca_full8_expand,
                                            matrix(NA, ncol=length(lag_vars)*lag_count, nrow=nrow(EurekahedgeHF_Excel_aca_full8_expand)),
                                            stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full9) <- c(colnames(EurekahedgeHF_Excel_aca_full8_expand),
                                             paste(lag_vars[1],"_lag",seq(1,lag_count),sep=""),
                                             paste(lag_vars[2],"_lag",seq(1,lag_count),sep=""),
                                             paste(lag_vars[3],"_lag",seq(1,lag_count),sep=""))


for (i in 1:length(lag_vars))
{
  #i <- 1
  #i <- 2
  #i <- 3
  
  EurekahedgeHF_Excel_aca_full9[,paste(lag_vars[i],"_lag",seq(1,lag_count),sep="")] <- 
    create_lags2(EurekahedgeHF_Excel_aca_full9,lag_vars[i],identifier,lag_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(lag_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,sort(colnames(EurekahedgeHF_Excel_aca_full9))]

starting_cols <- c(identifier,"fund_name","date_added","flagship","closed","limited","dead","dead_date","dead_reason",
                   "eurekahedge_id","isin","sedol","valoren","cusip","bloomberg","reuters","date","yr","month",
                   "aum",paste("aum","_lag",seq(1,lag_count),sep=""),
                   "monthly_ret",paste("monthly_ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "monthly_ret2","yearly_ret")

all_cols <- colnames(EurekahedgeHF_Excel_aca_full9)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[rowSums(is.na(EurekahedgeHF_Excel_aca_full9[,1:ncol(EurekahedgeHF_Excel_aca_full9)]))<ncol(EurekahedgeHF_Excel_aca_full9),]

EurekahedgeHF_Excel_aca_full9 <- EurekahedgeHF_Excel_aca_full9[order(EurekahedgeHF_Excel_aca_full9[,identifier], 
                                                                     EurekahedgeHF_Excel_aca_full9[,"yr"], 
                                                                     EurekahedgeHF_Excel_aca_full9[,"month"]),] 
row.names(EurekahedgeHF_Excel_aca_full9) <- seq(nrow(EurekahedgeHF_Excel_aca_full9))

rm2(EurekahedgeHF_Excel_aca_full8_expand)

#EurekahedgeHF_Excel_aca_full9[EurekahedgeHF_Excel_aca_full9[,c("aum")]==0,c("yr","month","fund_id","aum","aum_lag1","monthly_ret")]

###############################################################################
cat("CREATE FLOW AND LAG FLOW", "\n")
###############################################################################

lag_flow_vars <- c("nflow","pflow")
lag_flow_count <- 4

EurekahedgeHF_Excel_aca_full10 <- data.frame(EurekahedgeHF_Excel_aca_full9,
                                             nflow=NA,
                                             pflow=NA,
                                             matrix(NA, ncol=length(lag_flow_vars)*lag_flow_count, nrow=nrow(EurekahedgeHF_Excel_aca_full9)),
                                             stringsAsFactors=FALSE)
colnames(EurekahedgeHF_Excel_aca_full10) <- c(colnames(EurekahedgeHF_Excel_aca_full9),
                                              lag_flow_vars[1],
                                              lag_flow_vars[2],
                                              paste(lag_flow_vars[1],"_lag",seq(1,lag_flow_count),sep=""),
                                              paste(lag_flow_vars[2],"_lag",seq(1,lag_flow_count),sep=""))

EurekahedgeHF_Excel_aca_full10[,"nflow"] <- as.data.frame(data.table(EurekahedgeHF_Excel_aca_full10)[,list(nflow=aum-(aum_lag1*(1+monthly_ret))),by=identifier], stringsAsFactors=FALSE)["nflow"]
EurekahedgeHF_Excel_aca_full10[,"nflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"aum"]),NA, EurekahedgeHF_Excel_aca_full10[,"nflow"])
EurekahedgeHF_Excel_aca_full10[,"nflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"aum_lag1"]),NA, EurekahedgeHF_Excel_aca_full10[,"nflow"])
EurekahedgeHF_Excel_aca_full10[,"nflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"monthly_ret"]),NA, EurekahedgeHF_Excel_aca_full10[,"nflow"])

EurekahedgeHF_Excel_aca_full10[,"pflow"] <- as.data.frame(data.table(EurekahedgeHF_Excel_aca_full10)[,list(pflow=((aum-(aum_lag1*(1+monthly_ret)))/aum_lag1)),by=identifier], stringsAsFactors=FALSE)["pflow"]
EurekahedgeHF_Excel_aca_full10[,"pflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"aum"]),NA, EurekahedgeHF_Excel_aca_full10[,"pflow"])
EurekahedgeHF_Excel_aca_full10[,"pflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"aum_lag1"]),NA, EurekahedgeHF_Excel_aca_full10[,"pflow"])
EurekahedgeHF_Excel_aca_full10[,"pflow"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full10[,"monthly_ret"]),NA, EurekahedgeHF_Excel_aca_full10[,"pflow"])

for (i in 1:length(lag_flow_vars))
{
  #i <- 1
  #i <- 2
  
  EurekahedgeHF_Excel_aca_full10[,paste(lag_flow_vars[i],"_lag",seq(1,lag_flow_count),sep="")] <- 
    create_lags2(EurekahedgeHF_Excel_aca_full10,lag_flow_vars[i],identifier,lag_flow_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(lag_flow_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 


EurekahedgeHF_Excel_aca_full10 <- EurekahedgeHF_Excel_aca_full10[,sort(colnames(EurekahedgeHF_Excel_aca_full10))]

starting_cols <- c(identifier,"fund_name","date_added","flagship","closed","limited","dead","dead_date","dead_reason",
                   "eurekahedge_id","isin","sedol","valoren","cusip","bloomberg","reuters","date","yr","month",
                   "aum",paste("aum","_lag",seq(1,lag_count),sep=""),
                   "monthly_ret",paste("monthly_ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "monthly_ret2","yearly_ret",
                   "nflow",paste("nflow","_lag",seq(1,lag_flow_count),sep=""),  
                   "pflow",paste("pflow","_lag",seq(1,lag_flow_count),sep=""))

all_cols <- colnames(EurekahedgeHF_Excel_aca_full10)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full10 <- EurekahedgeHF_Excel_aca_full10[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full10 <- EurekahedgeHF_Excel_aca_full10[rowSums(is.na(EurekahedgeHF_Excel_aca_full10[,1:ncol(EurekahedgeHF_Excel_aca_full10)]))<ncol(EurekahedgeHF_Excel_aca_full10),]

EurekahedgeHF_Excel_aca_full10 <- EurekahedgeHF_Excel_aca_full10[order(EurekahedgeHF_Excel_aca_full10[,identifier], 
                                                                       EurekahedgeHF_Excel_aca_full10[,"yr"],
                                                                       EurekahedgeHF_Excel_aca_full10[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_full10) <- seq(nrow(EurekahedgeHF_Excel_aca_full10))

EurekahedgeHF_Excel_aca_full10 <- unknown_to_NA(EurekahedgeHF_Excel_aca_full10,unknowns_strings)

rm2(EurekahedgeHF_Excel_aca_full9)

#colMeans(EurekahedgeHF_Excel_aca_full10[,c("nflow","pflow")], na.rm = TRUE)
#EurekahedgeHF_Excel_aca_full10[is.infinite(EurekahedgeHF_Excel_aca_full10[,c("pflow")]),c("yr","month","fund_id","aum","aum_lag1","monthly_ret","nflow","pflow")]


###############################################################################
cat("COMPUTE ANNUAL FUND FLOW VOLATILITY", "\n")
###############################################################################

annual_flow_trim <- EurekahedgeHF_Excel_aca_full10[,c(identifier,"yr","nflow","pflow")]

fund_aflow_volatility <- ddply(annual_flow_trim, c(identifier,"yr"), summarize, 
                               sdnet_flow=sd(nflow, na.rm = TRUE),sdpct_flow=sd(pflow, na.rm = TRUE))

fund_aflow_volatility_full <- data.frame(fund_aflow_volatility,
                                         sdnet_flow_lag1=shift(fund_aflow_volatility$sdnet_flow,-1),
                                         sdpct_flow_lag1=shift(fund_aflow_volatility$sdpct_flow,-1),stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_full11 <- merge(EurekahedgeHF_Excel_aca_full10, fund_aflow_volatility_full, 
                                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(annual_flow_trim,fund_aflow_volatility,EurekahedgeHF_Excel_aca_full10)


###############################################################################
cat("REORDER COLUMNS", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[,sort(colnames(EurekahedgeHF_Excel_aca_full11))]

starting_cols <- c(identifier,"fund_name","date_added","flagship","closed","limited","dead","dead_date","dead_reason",
                   "eurekahedge_id","isin","sedol","valoren","cusip","bloomberg","reuters","date","yr","month",
                   "aum",paste("aum","_lag",seq(1,lag_count),sep=""),
                   "monthly_ret",paste("monthly_ret","_lag",seq(1,lag_count),sep=""),                  
                   "mktadjret",paste("mktadjret","_lag",seq(1,lag_count),sep=""),                  
                   "monthly_ret2","yearly_ret",
                   "nflow",paste("nflow","_lag",seq(1,lag_flow_count),sep=""),  
                   "pflow",paste("pflow","_lag",seq(1,lag_flow_count),sep=""))

all_cols <- colnames(EurekahedgeHF_Excel_aca_full11)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[,c(starting_cols,other_cols)]

EurekahedgeHF_Excel_aca_full11 <- EurekahedgeHF_Excel_aca_full11[rowSums(is.na(EurekahedgeHF_Excel_aca_full11[,1:ncol(EurekahedgeHF_Excel_aca_full11)]))<ncol(EurekahedgeHF_Excel_aca_full11),]


###############################################################################
cat("FIND AGE FOR EVERY WFICN", "\n")
###############################################################################

fund_age0 <- EurekahedgeHF_Excel_aca_full11[!is.na(EurekahedgeHF_Excel_aca_full11[,c("inception_date")]),c(identifier,"yr","month","inception_date")]

fund_age <- data.table(fund_age0)[,list(chgdt=min(inception_date,na.rm=TRUE)),by=identifier]
fund_age <- data.frame(fund_age,stringsAsFactors=FALSE)

fund_age_trim <- data.table(fund_age)[,list(chgdt=min(chgdt,na.rm=TRUE)),by=identifier]
fund_age_trim <- as.data.frame(fund_age_trim,stringsAsFactors=FALSE)

rm2(fund_age0,fund_age)

fund_age_trim2 <- fund_age_trim
fund_age_trim2[,c("chgdt")] <- as.IDate(fund_age_trim2[,c("chgdt")],origin = "1970-01-01",na.rm=TRUE)

fund_age_trim3 <- fund_age_trim2
fund_age_trim3 <- fund_age_trim3[!(is.na(fund_age_trim3[,identifier])),]
fund_age_trim3 <- fund_age_trim3[!(is.na(fund_age_trim3[,"chgdt"])),]
row.names(fund_age_trim3) <- seq(nrow(fund_age_trim3))

rm2(fund_age_trim,fund_age_trim2)

#fund_age_trim4 <- data.frame(fund_age_trim3,today=today(),stringsAsFactors=FALSE)
#fund_age_trim4[,"chgdt"] <- as.character(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"chgdt"] <- as.Date(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"today"] <- as.Date(fund_age_trim4[,"today"])
#fund_age_trim4[,"chgdt"] <- as.IDate(fund_age_trim4[,"chgdt"])
#fund_age_trim4[,"today"] <- as.IDate(fund_age_trim4[,"today"])
#fund_age_trim4[,"chgdt"] <- as.POSIXct(fund_age_trim4[,"chgdt"], format="%Y-%m-%d", origin = "1970-01-01")
#fund_age_trim4[,"today"] <- as.POSIXct(fund_age_trim4[,"today"], format="%Y-%m-%d", origin = "1970-01-01")


#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(chgdt,today(),"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                  by=eval(paste(identifier,",chgdt",sep=""))]
# fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(chgdt,
#                                                           today_temp,
#                                                           "days");
#                                                     list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                   by=eval(paste(identifier,",chgdt",sep=""))]

#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(from=chgdt,to=Sys.Date(),by="days")},
#                                 by=list(fund_id,chgdt)]

fund_age_month_temp <- data.table(fund_age_trim3)[, seq(as.Date(chgdt, origin="1970-01-01"), Sys.Date(), by="days"),
                                                  by=eval(paste(identifier,",chgdt",sep=""))]

#fund_age_month_temp <- data.table(fund_age_trim4)[,{s=seq(from=chgdt,
#                                                          to=today,
#                                                          by="days");
#                                                    list(yr=year(unlist(s)),month=month(unlist(s)))},
#                                                  by=eval(paste(identifier,",chgdt",sep=""))]



#test <- seq(as.POSIXct(fund_age_trim4[1,"chgdt"], format="%Y-%m-%d"),
#            as.POSIXct(today(), format="%Y-%m-%d"),
#            "days")
#test <- as.data.frame(test,stringsAsFactors=FALSE)  

rm2(fund_age_trim3)

fund_age_month_temp <- as.data.frame(fund_age_month_temp,stringsAsFactors=FALSE)
colnames(fund_age_month_temp)[ncol(fund_age_month_temp)] <- "data_expand"

fund_age_month_temp <- data.frame(fund_age_month_temp,
                                  yr=year(fund_age_month_temp[,"data_expand"]),
                                  month=month(fund_age_month_temp[,"data_expand"]),
                                  stringsAsFactors=FALSE)

fund_age_month_temp2 <- fund_age_month_temp[,!(colnames(fund_age_month_temp) %in% "data_expand")]

rm2(fund_age_month_temp)

fund_age_month_temp2 <- unique(fund_age_month_temp2, incomparables=FALSE)
row.names(fund_age_month_temp2) <- seq(nrow(fund_age_month_temp2))

fund_age_month_temp2 <- transform(fund_age_month_temp2, chgdt=as.character(chgdt))

fund_age_month <- data.table(fund_age_month_temp2)[,list(yr,month,chgdt,age_m=(seq(1,.N)-1),age_y=((seq(1,.N)-1)/12)),by=identifier]
fund_age_month <- as.data.frame(fund_age_month,stringsAsFactors=FALSE)

rm2(fund_age_month_temp2)

EurekahedgeHF_Excel_aca_full12 <- merge(EurekahedgeHF_Excel_aca_full11, fund_age_month, 
                                        by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
                                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(fund_age_month,EurekahedgeHF_Excel_aca_full11)


# age_vars <- c("age_m","age_y")
# for (i in 1:length(age_vars))
# {
#   #i <- 1
#   #i <- 2
#   EurekahedgeHF_Excel_aca_full12[,age_vars[i]] <- 
#     winsorize_both(EurekahedgeHF_Excel_aca_full12[,age_vars[i]],q=0.01)
#   
# } 


#colMeans(EurekahedgeHF_Excel_aca_full12[,c("nflow","pflow")], na.rm = TRUE)

# ###############################################################################
# cat("MERGE AGGREGATE DATA", "\n")
# ###############################################################################
# 
# monthly_agg2 <- merge(agg_mtna, agg_retail_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_open_to_inv_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_inst_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_m_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_vau_fund_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, agg_dead_flag_dv, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg2 <- merge(monthly_agg2, fund_age_month, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# rm2(agg_retail_fund_dv,agg_open_to_inv_dv,agg_inst_fund_dv,agg_m_fund_dv,agg_vau_fund_dv,agg_dead_flag_dv)
# 
# #monthly_agg2 <- subset(monthly_agg2,select=-c(mtna_agg))
# monthly_agg2 <- monthly_agg2[,!(colnames(monthly_agg2) %in% "mtna_agg")]

# ###############################################################################
# cat("MERGE MONTHLY_AGG_LAGS_FULL AND MONTHLY_AGG2", "\n")
# ###############################################################################
# 
# monthly_agg_full <- merge(monthly_agg_lags_full, monthly_agg2, by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full <- monthly_agg_full[order(monthly_agg_full[,identifier],
#                                            monthly_agg_full[,"yr"],
#                                            monthly_agg_full[,"month"]),] 
# 
# #rm2(monthly_agg_lags_full,monthly_agg2)
# 
# 
# ###############################################################################
# cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN FUND_STYLE_MONTH", "\n")
# ###############################################################################
# 
# fund_style_full <- merge(mflink1,fund_style_month, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                          all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# # fund_style_full_trim <- subset(fund_style_full,select=-c(crsp_fundno,begdt,enddt,lipper_class_name,accrual_fund,sales_restrict))
# fund_style_full_trim <- fund_style_full[,!(colnames(fund_style_full) %in% c("crsp_fundno","begdt","enddt","lipper_class_name","accrual_fund","sales_restrict"))]
# 
# fund_style_full_trim <- fund_style_full_trim[c(identifier,"yr","month","crsp_obj_cd","lipper_class","lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")]
# 
# fund_style_full_trim <- fund_style_full_trim[order(fund_style_full_trim[,identifier], fund_style_full_trim[,"yr"], fund_style_full_trim[,"month"]),]
# 
# fund_style_full_trim <- unique(fund_style_full_trim, incomparables=FALSE)
# 
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"crsp_obj_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_class"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_name"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_asset_cd"])),]
# fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_tax_cd"])),]
# 
# query_remove_styles <- "select distinct   wficn,yr, month, Count(wficn) Count
# from              fund_style_full_trim 
# group by          wficn, yr, month
# order by          wficn, yr, month"
# remove_styles <- sqldf(query_remove_styles)
# 
# remove_styles2 <- remove_styles[!(remove_styles[,"Count"]==1),]
# 
# colnames(remove_styles2)[match("Count",names(remove_styles2))] <- "Remove"
# remove_styles2[,"Remove"] <- 1
# 
# fund_style_full_trim2 <- merge(fund_style_full_trim,remove_styles2, 
#                                by.x=c(identifier, "yr", "month"), by.y=c(identifier, "yr", "month"), 
#                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_style_full_trim3 <- fund_style_full_trim2[is.na(fund_style_full_trim2[,"Remove"]),]
# # fund_style_full_trim3 <- subset(fund_style_full_trim3,select=-c(Remove))
# fund_style_full_trim3 <- fund_style_full_trim3[,!(colnames(fund_style_full_trim3) %in% "Remove")]
# 
# for(i in which(sapply(fund_style_full_trim3,class)=="character"))
# {
#   fund_style_full_trim3[[i]] = trim(fund_style_full_trim3[[i]])
# }
# for (i in 1:ncol(fund_style_full_trim3))
# {
#   #i <- 1
#   fund_style_full_trim3[,i] <- unknownToNA(fund_style_full_trim3[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
#   fund_style_full_trim3[,i] <- ifelse(is.na(fund_style_full_trim3[,i]),NA, fund_style_full_trim3[,i])
# } 
# 
# #rm2(fund_style_full,fund_style_full_trim,query_remove_styles,remove_styles,remove_styles2,fund_style_full_trim2)
# 
# 
# ###############################################################################
# cat("MERGE FUND_STYLE_MONTH INTO MONTHLY_AGG_LAGS_FULL", "\n")
# ###############################################################################
# 
# monthly_agg_full2 <- merge(monthly_agg_full, fund_style_full_trim3, 
#                            by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"), 
#                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_agg_full2 <- monthly_agg_full2[order(monthly_agg_full2[,identifier],
#                                              monthly_agg_full2[,"yr"],
#                                              monthly_agg_full2[,"month"]),] 
# 
# #rm2(monthly_agg_full,fund_style_full_trim3)
# 
# 
# ###############################################################################
# cat("TRIM YEARS IN MONTHLY_AGG_FULL", "\n")
# ###############################################################################
# 
# monthly_agg_full2_trim <- monthly_agg_full2[(monthly_agg_full2[,"yr"]>=1999 & monthly_agg_full2[,"yr"]<=2012),]



# ###############################################################################
# cat("CREATE LOG ASSETS, SQUARE RETURNS, AND STLYE DUMMIES", "\n")
# ###############################################################################

EurekahedgeHF_Excel_aca_full13 <- data.frame(EurekahedgeHF_Excel_aca_full12,
                                             log_AUM=NA,log_AUM_lag1=NA,log_AUM_lag2=NA,log_AUM_lag3=NA,log_AUM_lag4=NA,
                                             mktadjret_sq=NA,mktadjret_lag1_sq=NA,mktadjret_lag2_sq=NA,mktadjret_lag3_sq=NA,mktadjret_lag4_sq=NA)

EurekahedgeHF_Excel_aca_full13[,"log_AUM"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"aum"]))
EurekahedgeHF_Excel_aca_full13[,"log_AUM_lag1"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"aum_lag1"]))
EurekahedgeHF_Excel_aca_full13[,"log_AUM_lag2"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"aum_lag2"]))
EurekahedgeHF_Excel_aca_full13[,"log_AUM_lag3"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"aum_lag3"]))
EurekahedgeHF_Excel_aca_full13[,"log_AUM_lag4"] <- suppressWarnings(log(EurekahedgeHF_Excel_aca_full13[,"aum_lag4"]))

EurekahedgeHF_Excel_aca_full13[,"mktadjret_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag1_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag1"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag2_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag2"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag3_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag3"])^2
EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag4_sq"] <- (EurekahedgeHF_Excel_aca_full13[,"mktadjret_lag4"])^2

#monthly_data_all2[,"avg_mkt_exp"] <- (monthly_data_all2[,"actual_12b1_agg"]/monthly_data_all2[,"exp_ratio_agg"])
#monthly_data_all2[,"avg_mkt_explag1"] <- (monthly_data_all2[,"actual_12b1_agglag1"]/monthly_data_all2[,"exp_ratio_agglag1"])
#monthly_data_all2[,"avg_mkt_explag2"] <- (monthly_data_all2[,"actual_12b1_agglag2"]/monthly_data_all2[,"exp_ratio_agglag2"])
#monthly_data_all2[,"avg_mkt_explag3"] <- (monthly_data_all2[,"actual_12b1_agglag3"]/monthly_data_all2[,"exp_ratio_agglag3"])
#monthly_data_all2[,"avg_mkt_explag4"] <- (monthly_data_all2[,"actual_12b1_agglag4"]/monthly_data_all2[,"exp_ratio_agglag4"])

#lipper_class_u <- unique(monthly_data_all2[,"lipper_class"],incomparables=FALSE)
#lipper_obj_cd_u <- unique(monthly_data_all2[,"lipper_obj_cd"],incomparables=FALSE)
#lipper_asset_cd_u <- unique(monthly_data_all2[,"lipper_asset_cd"],incomparables=FALSE)
#lipper_tax_cd_u <- unique(monthly_data_all2[,"lipper_tax_cd"],incomparables=FALSE)
# 
# monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="EQ", 1, monthly_data_all2$lipper_asset_cd_eq_dv)
# monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="TX" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="MB"), 0, monthly_data_all2$lipper_asset_cd_eq_dv)
# 
# monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="TX", 1, monthly_data_all2$lipper_asset_cd_tx_dv)
# monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="MB" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="EQ"), 0, monthly_data_all2$lipper_asset_cd_tx_dv)
# 
# monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="MB", 1, monthly_data_all2$lipper_asset_cd_mb_dv)
# monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="EQ" | 
#                                                          toupper(monthly_data_all2$lipper_asset_cd)=="TX"), 0, monthly_data_all2$lipper_asset_cd_mb_dv)
# 
# monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 1, monthly_data_all2$lipper_tax_cd_t_dv)
# monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 0, monthly_data_all2$lipper_tax_cd_t_dv)
# 
# monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 1, monthly_data_all2$lipper_tax_cd_te_dv)
# monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 0, monthly_data_all2$lipper_tax_cd_te_dv)

rm2(EurekahedgeHF_Excel_aca_full12)

# ###############################################################################
# cat("MERGE FUND_NAMES AND MDM_DATA_TRIM", "\n")
# ###############################################################################
# 
# fund_names_msd <- merge(fund_names, Mdmf_data_trim, 
#                         by.x=c("ncusip"), by.y=c("CUSIP"), all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_names_msd <- fund_names_msd[order(fund_names_msd[,"crsp_fundno"], 
#                                        fund_names_msd[,"chgdt"]),] 
# 
# 
# fund_names_msd2 <- merge(fund_names_msd, mflink1, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
#                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# fund_names_msd2 <- fund_names_msd2[c(identifier,"crsp_fundno","Broad_Cat_Group","Global_Cat","MS_Cat","MS_Inst_Cat",
#                                      "MS_Rating_Overall", "US_Broad_Asset_Class","Equity_Style_Box_Long","MS_Anal_Rating",
#                                      "Firm_Name","Branding_Name","Prospectus_Objective")]
# 
# fund_names_msd2 <- fund_names_msd2[order(fund_names_msd2[,identifier], fund_names_msd2[,"crsp_fundno"]),]
# fund_names_msd2 <- unique(fund_names_msd2,comparables=FALSE)
# 
# #fund_names_msd3 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Firm_Name,Branding_Name,Global_Cat,
# #                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# fund_names_msd3 <- fund_names_msd2[,!(colnames(fund_names_msd2) %in% c("crsp_fundno","MS_Rating_Overall","Firm_Name","Branding_Name","Global_Cat",
#                                                                       "MS_Cat","MS_Inst_Cat","US_Broad_Asset_Class","MS_Anal_Rating","Prospectus_Objective"))]
# 
# #fund_names_msd3[fund_names_msd3[,identifier]==103259,"MS_Anal_Rating"] <- "Silver" 
# #fund_names_msd3[fund_names_msd3[,identifier]==103380,"MS_Anal_Rating"] <- "Bronze" 
# #fund_names_msd3[fund_names_msd3[,identifier]==400001,"MS_Anal_Rating"] <- "Bronze" 
# 
# fund_names_msd3 <- unique(fund_names_msd3,comparables=FALSE)
# 
# fund_names_msd3_row_count <- as.data.frame(data.table(fund_names_msd3)[, list(count=.N),by=identifier],stringsAsFactors=FALSE)
# fund_names_msd3_bad <- fund_names_msd3_row_count[fund_names_msd3_row_count[,"count"]>1,]
# 
# fund_names_msd4 <- fund_names_msd3[!(fund_names_msd3[,identifier] %in% fund_names_msd3_bad[,identifier]),]
# 
# #rm2(fund_names_msd,fund_names_msd2,fund_names_msd3,fund_names_msd3_row_count,fund_names_msd3_bad)
# 
# #fund_names_msd5 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Broad_Cat_Group,Equity_Style_Box_Long,Global_Cat,
# #                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
# #fund_names_msd5 <- subset(fund_names_msd5,select=-c(Firm_Name))
# fund_names_msd5 <- fund_names_msd2[,!(colnames(fund_names_msd2) %in% c("crsp_fundno","MS_Rating_Overall","Broad_Cat_Group","Equity_Style_Box_Long","Global_Cat",
#                                                     "MS_Cat","MS_Inst_Cat","US_Broad_Asset_Class","MS_Anal_Rating","Prospectus_Objective"))]
# fund_names_msd5 <- fund_names_msd5[,!(colnames(fund_names_msd5) %in% c("Firm_Name"))]

# fund_names_msd5 <- unique(fund_names_msd5,comparables=FALSE)
# 
# fund_names_msd5_row_count <- as.data.frame(data.table(fund_names_msd5)[, list(count=.N),by=identifier],stringsAsFactors=FALSE)
# fund_names_msd5_bad <- fund_names_msd5_row_count[fund_names_msd5_row_count[,"count"]>1,]
# 
# fund_names_msd6 <- fund_names_msd5[!(fund_names_msd5[,identifier] %in% fund_names_msd5_bad[,identifier]),]
# 
# #rm2(fund_names_msd5,fund_names_msd5_row_count,fund_names_msd5_bad)
# 
# 
# #test <- as.data.frame(unique(fund_names_msd6[,"Branding_Name"],comparables=FALSE),stringsAsFactors=FALSE)
# #colnames(test) <- "Branding_Name"
# #test <- test[order(test[,"Branding_Name"]),]
# 
# 
# ###############################################################################
# cat("ADD DVS FOR STYLE AND RATING", "\n")
# ###############################################################################
# 
# #aa_Broad_Cat_Group_u <- as.data.frame(unique(fund_names_msd4[,"Broad_Cat_Group"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_Equity_Style_Box_Long_u <- as.data.frame(unique(fund_names_msd4[,"Equity_Style_Box_Long"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_MS_Anal_Rating_u <- as.data.frame(unique(fund_names_msd4[,"MS_Anal_Rating"],comparables=FALSE),stringsAsFactors=FALSE)
# #aa_Prospectus_Objective_u <- as.data.frame(unique(fund_names_msd4[,"Prospectus_Objective"],comparables=FALSE),stringsAsFactors=FALSE)
# 
# fund_names_msd_dv <- data.frame(fund_names_msd4,
#                                 Broad_Cat_Group_eq_dv=NA,Broad_Cat_Group_al_dv=NA,Broad_Cat_Group_fi_dv=NA,Broad_Cat_Group_alt_dv=NA,Broad_Cat_Group_tp_dv=NA,
#                                 Equity_Style_Box_Long_lg_dv=NA,Equity_Style_Box_Long_lv_dv=NA,Equity_Style_Box_Long_lb_dv=NA,
#                                 Equity_Style_Box_Long_mg_dv=NA,Equity_Style_Box_Long_mv_dv=NA,Equity_Style_Box_Long_mb_dv=NA,
#                                 Equity_Style_Box_Long_sg_dv=NA,Equity_Style_Box_Long_sv_dv=NA,Equity_Style_Box_Long_sb_dv=NA)
# #MS_Anal_Rating_gold_dv=NA,MS_Anal_Rating_silver_dv=NA,MS_Anal_Rating_bronze_dv=NA,
# #MS_Anal_Rating_neutral_dv=NA,MS_Anal_Rating_negative_dv=NA)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="EQUITY", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_eq_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALLOCATION", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_al_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="FIXED INCOME", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_fi_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALTERNATIVE", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_alt_dv)
# 
# fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="TAX PREFERRED", 1, 0)
# fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_tp_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lb_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mb_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL GROWTH", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sg_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL VALUE", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sv_dv)
# 
# fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL BLEND", 1, 0)
# fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sb_dv)
# 
# ####CANNOT USE THESE BECAUSE THERE AREN"T ACROSS TIME
# # fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="GOLD", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_gold_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="SILVER", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_silver_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="BRONZE", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_bronze_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEUTRAL", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_neutral_dv)
# # 
# # fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEGATIVE", 1, 0)
# # fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_negative_dv)
# # 
# 
# monthly_data_all3 <- merge(monthly_data_all2, fund_names_msd_dv, 
#                            by.x=c(identifier), by.y=c(identifier), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# monthly_data_all4 <- data.frame(monthly_data_all3,FundRetMktNeg=NA,stringsAsFactors=FALSE)
# 
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(monthly_data_all4$mktadjret_agg<0, monthly_data_all4$mktadjret_agg, 0)
# monthly_data_all4[,"FundRetMktNeg"] <- ifelse(is.na(monthly_data_all4$mktadjret_agg), NA, monthly_data_all4$FundRetMktNeg)
# 
# monthly_data_all4 <- monthly_data_all4[order(monthly_data_all4[,identifier],
#                                              monthly_data_all4[,"yr"],
#                                              monthly_data_all4[,"month"]),]
# 
# #rm2(fund_names_msd_dv)
# 
# 
# ###############################################################################
# cat("COMPUTE AVERAGE GRADE-LEVEL", "\n")
# ###############################################################################

read_stats_ios_f  <- read_stats_ios_f[((!is.na(read_stats_ios_f[,"ari_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"coleman_liau_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"flesch_kincaid_ios"])) & 
                                         (!is.na(read_stats_ios_f[,"fog_ios"])) &   
                                         (!is.na(read_stats_ios_f[,"smog_ios"]))),]

avg_grade_level_ios <- as.data.frame(rowMeans(read_stats_ios_f[,c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")], na.rm=TRUE),stringsAsFactors=FALSE)
colnames(avg_grade_level_ios) <- "avg_grade_level_ios"

read_stats_ios_f <- cbind(read_stats_ios_f,avg_grade_level_ios)

rm2(avg_grade_level_ios)


# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY BROAD_CAT_GROUP AND YEAR", "\n")
# ###############################################################################

#aa <- EurekahedgeHF_Excel_aca_full13[EurekahedgeHF_Excel_aca_full13[,identifier]==6131,c(identifier,"fund_name","yr","month","pflow","aum","aum_lag1","monthly_ret")]
#cc <- bb[bb[,identifier]==6131,c(identifier,"fund_name","yr","month","pflow","aum","aum_lag1","monthly_ret")]


EurekahedgeHF_Excel_aca_full14 <- EurekahedgeHF_Excel_aca_full13[!is.na(EurekahedgeHF_Excel_aca_full13[,"fund_name"]),]

#similarity_db_tables <- ListTables(similarity_db)
#similarity_db_fields <- ListFields(similarity_db)

group_column <- "main_investment_strategy"

sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]

temp_stacked_full <- merge(EurekahedgeHF_Excel_aca_full14[,c(identifier,group_column)], sample_data_all_temp[,c("yr",identifier)], 
                           by.x=c(identifier), by.y=c(identifier), 
                           all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
temp_stacked_full <- temp_stacked_full[order(temp_stacked_full[,identifier], 
                                             temp_stacked_full[,"yr"]),] 
temp_stacked_full <- unique(temp_stacked_full,comparables=FALSE)
row.names(temp_stacked_full) <- seq(nrow(temp_stacked_full))

text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
text_group_vars <- toupper(text_group_vars)
text_group_vars <- trim(text_group_vars)
text_group_vars <- text_group_vars[!is.na(text_group_vars)]
#text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]

#Create empty data.frame for merging
for (j in 1:length(text_variables))
{
  #j <- 1
  
  text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
  text_percentages_temp_col <- empty.df(text_percentages_temp)
  for (k in 1:ncol(text_percentages_temp_col))
  {
    text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
  }
  header_col <- data.frame(temp=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
  colnames(header_col)[1] <- identifier
  assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
  
  rm2(text_percentages_temp,text_percentages_temp_col,header_col)
}


for (i in 1:length(text_group_vars))
{
  #i <- 1
  
  for (j in 1:length(text_variables))
  {
    #j <- 1
    
    temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier)
    
    if (text_variables[j]=="ios")
    {
      year_sim_ios_main_investment_strategy_stacked <- rbind(year_sim_ios_main_investment_strategy_stacked,temp_sim_stacked)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    #rm2(temp_sim_stacked)
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
    
    
  }
  
}

colnames(year_sim_ios_main_investment_strategy_stacked) <- paste(group_column,colnames(year_sim_ios_main_investment_strategy_stacked),sep="_")
colnames(year_sim_ios_main_investment_strategy_stacked)[1:3] <- c(identifier,"yr",group_column)

year_sim_ios_main_investment_strategy_stacked <- year_sim_ios_main_investment_strategy_stacked[order(year_sim_ios_main_investment_strategy_stacked[,identifier], 
                                                                                                     year_sim_ios_main_investment_strategy_stacked[,"yr"]),]


rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)


# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY EQUITY STYLE LONG BOX AND YEAR", "\n")
# ###############################################################################
# 
# group_column <- "Equity_Style_Box_Long"
# 
# sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd4[,c(identifier,group_column)], sample_data_all_temp[,c("yr",identifier)], 
#                            by.x=c(identifier), by.y=c(identifier), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
# text_group_vars <- toupper(text_group_vars)
# text_group_vars <- trim(text_group_vars)
# text_group_vars <- text_group_vars[!is.na(text_group_vars)]
# 
# #Create empty data.frame for merging
# for (j in 1:length(text_variables))
# {
#   #j <- 1
#   
#   text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
#   text_percentages_temp_col <- empty.df(text_percentages_temp)
#   for (k in 1:ncol(text_percentages_temp_col))
#   {
#     text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
#   }
#   header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
#   assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
#   
#   rm2(text_percentages_temp,text_percentages_temp_col,header_col)
# }
# 
# for (i in 1:length(text_group_vars))
# {
#   #i <- 1
#   
#   for (j in 1:length(text_variables))
#   {
#     #j <- 1
#     
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier)
#     
#     if (text_variables[j]=="ios")
#     {
#       year_sim_ios_equity_style_box_long_stacked <- rbind(year_sim_ios_equity_style_box_long_stacked,temp_sim_stacked)
#       
#     } else if (text_variables[j]=="pr")
#     {
#       year_sim_pr_equity_style_box_long_stacked <- rbind(year_sim_pr_equity_style_box_long_stacked,temp_sim_stacked)
#       
#     } else
#     {
#       cat("ERROR!", "\n")
#     }
#     
#     rm2(temp_sim_stacked)
#     
#     progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
#                       inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
#     
#     
#   }
#   
# }
# 
# colnames(year_sim_ios_equity_style_box_long_stacked) <- paste(group_column,colnames(year_sim_ios_equity_style_box_long_stacked),sep="_")
# colnames(year_sim_ios_equity_style_box_long_stacked)[1:3] <- c(identifier,"yr",group_column)
# 
# year_sim_ios_equity_style_box_long_stacked <- year_sim_ios_equity_style_box_long_stacked[order(year_sim_ios_equity_style_box_long_stacked[,identifier], 
#                                                                                                  year_sim_ios_equity_style_box_long_stacked[,"yr"]),]
# 
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
# 
# 
# ###############################################################################
# cat("COMPUTE AVERAGE SIMILARITY BY FAMILY AND YEAR", "\n")
# ###############################################################################
# 
# group_column <- "Branding_Name"
# 
# sample_data_all_temp <- sample_data_all[,c(identifier,"yr")]
# sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
# sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,identifier], sample_data_all_temp[,"yr"]),]
# 
# temp_stacked_full <- merge(fund_names_msd6[,c(identifier,group_column)], sample_data_all_temp[,c("yr",identifier)], 
#                            by.x=c(identifier), by.y=c(identifier), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
# 
# text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
# text_group_vars <- toupper(text_group_vars)
# text_group_vars <- trim(text_group_vars)
# text_group_vars <- text_group_vars[!is.na(text_group_vars)]
# #text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]
# 
# #Create empty data.frame for merging
# for (j in 1:length(text_variables))
# {
#   #j <- 1
#   
#   text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
#   text_percentages_temp_col <- empty.df(text_percentages_temp)
#   for (k in 1:ncol(text_percentages_temp_col))
#   {
#     text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
#   }
#   header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
#   assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
#   
#   rm2(text_percentages_temp,text_percentages_temp_col,header_col)
# }
# 
# 
# for (i in 1:length(text_group_vars))
# {
#   #i <- 1
#   
#   for (j in 1:length(text_variables))
#   {
#     #j <- 1
#     
#     temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j],identifier)
#     
#     if (text_variables[j]=="ios")
#     {
#       year_sim_ios_branding_name_stacked <- rbind(year_sim_ios_branding_name_stacked,temp_sim_stacked)
#       
#     } else if (text_variables[j]=="pr")
#     {
#       year_sim_pr_branding_name_stacked <- rbind(year_sim_pr_branding_name_stacked,temp_sim_stacked)
#       
#     } else
#     {
#       cat("ERROR!", "\n")
#     }
#     
#     rm2(temp_sim_stacked)
#     
#     progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
#                       inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
#     
#     
#   }
#   
# }
# 
# colnames(year_sim_ios_branding_name_stacked) <- paste(group_column,colnames(year_sim_ios_branding_name_stacked),sep="_")
# colnames(year_sim_ios_branding_name_stacked)[1:3] <- c(identifier,"yr",group_column)
# 
# year_sim_ios_branding_name_stacked <- year_sim_ios_branding_name_stacked[order(year_sim_ios_branding_name_stacked[,identifier], 
#                                                                                  year_sim_ios_branding_name_stacked[,"yr"]),]
# 
# rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)
# 
# 
# ###############################################################################
# cat("BACKUP SIMIALRITY DATA", "\n")
# ###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"year_sim_ios_all_stacked",year_sim_ios_all_stacked)
ExportTable(descriptive_stats_db,"year_sim_ios_main_investment_strategy_stacked",year_sim_ios_main_investment_strategy_stacked)
#ExportTable(descriptive_stats_db,"year_sim_ios_equity_style_box_long_stacked",year_sim_ios_equity_style_box_long_stacked)
#ExportTable(descriptive_stats_db,"year_sim_ios_branding_name_stacked",year_sim_ios_branding_name_stacked)

#year_sim_ios_all_stacked                   <- runsql("SELECT * FROM year_sim_ios_all_stacked",descriptive_stats_db)
#year_sim_ios_broad_cat_group_stacked       <- runsql("SELECT * FROM year_sim_ios_broad_cat_group_stacked",descriptive_stats_db)
#year_sim_ios_equity_style_box_long_stacked <- runsql("SELECT * FROM year_sim_ios_equity_style_box_long_stacked",descriptive_stats_db)
#year_sim_ios_branding_name_stacked         <- runsql("SELECT * FROM year_sim_ios_branding_name_stacked",descriptive_stats_db)


# ###############################################################################
# cat("MERGE IOS TEXT DATA", "\n")
# ###############################################################################

read_stats_ios <- merge(EurekahedgeHF_Excel_aca_full14[,c(identifier,"yr","month")], read_stats_ios_f, 
                        #by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
read_stats_ios <- read_stats_ios[order(read_stats_ios[,identifier],read_stats_ios[,"yr"],read_stats_ios[,"month"]),]

sim_stats_ios <- merge(EurekahedgeHF_Excel_aca_full14[,c(identifier,"yr","month")], year_sim_ios_all_stacked,
                       by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_main_investment_strategy_stacked, 
                       by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
#sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_equity_style_box_long_stacked, 
#                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
#                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
#sim_stats_ios <- merge(sim_stats_ios, year_sim_ios_branding_name_stacked, 
#                        by.x=c(identifier,"yr"), by.y=c(identifier,"yr"), 
#                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_ios <- sim_stats_ios[order(sim_stats_ios[,identifier],
                                     sim_stats_ios[,"yr"],
                                     sim_stats_ios[,"month"]),]

text_stats_ios <- merge(read_stats_ios, sim_stats_ios, by.x=c(identifier,"yr","month"), 
                        by.y=c(identifier,"yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)


#rm2(year_sim_ios_all_stacked,year_sim_ios_broad_cat_group_stacked)
rm2(read_stats_ios,sim_stats_ios)


# ###############################################################################
# cat("BACKUP DATA", "\n")
# ###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"EurekahedgeHF_Excel_aca_full14",EurekahedgeHF_Excel_aca_full14)
ExportTable(descriptive_stats_db,"text_stats_ios",text_stats_ios)

