# TODO: Add comment
# 
# Author:  Brad
# File:    Benford_Test2.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: THIS IS JUST A TEST
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
  
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4",winslash="\\", mustWork=TRUE)
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
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE) 
  
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
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("benford.analysis","BenfordTests","data.table","gdata","ggplot2","MASS","plyr","quantmod","reshape2","stringr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

###############################################################################
cat("SECTION: IMPORT DATA", "\n")
###############################################################################

######## Import Data

file <- "prices.tsv.gz"

orig0 <- read.csv(paste(input_directory,"//",file,sep=""), sep = "\t")

orig0[sapply(orig0, is.factor)] <- lapply(orig0[sapply(orig0, is.factor)], as.character)


######## Find Item Order

orig1_key <- data.frame(Item_Order=NA,
                        Item=unique(orig0[,"Item"]),
                        stringsAsFactors=FALSE)
orig1_key[,"Item_Order"] <- seq(1,nrow(orig1_key))


######## Shorten Data 

#orig1 <- orig0

#orig_keep_col <- c("Cereal","Crackers","Dinners","Entrees","Grooming","Juices","Laundry","Shampoo")
#orig_keep_col <- c("Cereal","Crackers")
#orig1 <- orig0[orig0[,"Item"] %in% orig_keep_col,]

orig1 <- ddply(.data=orig0, .variables=c("Item"), .fun = function(x,keep_num){ head(x,keep_num) }, keep_num=100)

#rm(orig0)
gc()


######## Create Coputational Columns Data 

#orig2 <- data.frame(item_order=NA,
#                    orig1,
#                    Price_First_Digit_LHS=NA,
#                    Price_FirstTwo_Digits_RHS = NA,
#                    Price_First_Digit_RHS=NA,
#                    stringsAsFactors=FALSE)

orig2 <- data.frame(Item_Order=NA,
                    SubItem_Order=NA,
                    orig1,
                    Price_First_Digit_LHS=NA,
                    Price_FirstTwo_Digits_RHS = NA,
                    Price_First_Digit_RHS=NA,
                    Ret=NA,
                    Ret_First_Digit_LHS=NA,
                    Ret_FirstTwo_Digits_RHS=NA,
                    Ret_First_Digit_RHS=NA,
                    Ret_Percent=NA,
                    Ret_Percent_First_Digit_LHS=NA,
                    stringsAsFactors=FALSE)

rm(orig1)
gc()


######## Add Keys

for (i in 1:nrow(orig1_key))
{
  #i <- 1
  
  orig2[,"Item_Order"] <- ifelse(orig2[,"Item"]==orig1_key[i,"Item"],orig1_key[i,"Item_Order"], orig2[,"Item_Order"])
  
}
rm(i,orig1_key)

orig2 <- ddply(.data=orig2, .variables=c("Item_Order"), .fun = function(x){ 
  
  x[,"SubItem_Order"] <- seq(1,nrow(x))
  
  return(x)
  
},.drop = FALSE)

orig2 <- orig2[order(orig2[,"Item_Order"],
                     orig2[,"SubItem_Order"]),]
row.names(orig2) <- seq(nrow(orig2))


######## Find Return

orig2 <- ddply(.data=orig2, .variables=c("Item_Order"), .fun = function(x){ 
  
  x[,"Ret"] <- Delt(x[,"Price"],k=1,type='arithmetic')
  
  return(x)
  
},.drop = FALSE)


######## Remove Rows with No Returns

orig3 <- orig2[!is.na(orig2[,"Ret"]),]
row.names(orig3) <- seq(nrow(orig3))

rm(orig2)
gc()


######## Find Return in Percent

orig3[,"Ret_Percent"] <- orig3[,"Ret"] * 100


###############################################################################
cat("SECTION: COMPUTE LHS DIGITS", "\n")
###############################################################################

digits_LHS_price <- 1
digits_LHS_ret <- 1
digits_LHS_ret_percent <- 1

orig4 <- orig3

######## Compute First Digit LHS - Price

orig4[,"Price_First_Digit_LHS"] <- ldply(.data=orig4[,"Price"], .fun = function(x,digits){
  
  # x <- orig4[1,"Price"]
  x_out0 <- formatC(abs(x), width = digits, format = "d", flag = "0")
  x_out1 <- head(strsplit(x_out0,'')[[1]],n=digits)
  x_out2 <- paste(x_out1,sep="",collapse="")
  return(x_out2) 
  
},digits=digits_LHS_price)

orig4[,"Price_First_Digit_LHS"] <- as.numeric(orig4[,"Price_First_Digit_LHS"])


######## Compute First Digit RHS - Return

orig4[,"Ret_First_Digit_LHS"] <- ldply(.data=orig4[,"Ret"], .fun = function(x,digits){
  
  # x <- orig4[1,"Ret"]
  x_out0 <- formatC(abs(x), width = digits, format = "d", flag = "0")
  x_out1 <- head(strsplit(x_out0,'')[[1]],n=digits)
  x_out2 <- paste(x_out1,sep="",collapse="")
  return(x_out2) 
  
},digits=digits_LHS_ret)

orig4[,"Ret_First_Digit_LHS"] <- as.numeric(orig4[,"Ret_First_Digit_LHS"])


######## Compute First Digit LHS - Return (Percentage)

orig4[,"Ret_Percent_First_Digit_LHS"] <- ldply(.data=orig4[,"Ret_Percent"], .fun = function(x,digits){
  
  # x <- orig4[1,"Ret"]
  x_out0 <- formatC(abs(x), width = digits, format = "d", flag = "0")
  x_out1 <- head(strsplit(x_out0,'')[[1]],n=digits)
  x_out2 <- paste(x_out1,sep="",collapse="")
  return(x_out2) 
  
},digits=digits_LHS_ret_percent)

orig4[,"Ret_Percent_First_Digit_LHS"] <- as.numeric(orig4[,"Ret_Percent_First_Digit_LHS"])

rm(digits_LHS_price,digits_LHS_ret,digits_LHS_ret_percent)


###############################################################################
cat("SECTION: COMPUTE RHS DIGITS", "\n")
###############################################################################

digits_RHS_price <- 2
digits_RHS_ret <- 2

######## Compute First Digit RHS - Price

orig4[,"Price_FirstTwo_Digits_RHS"] <- round(abs(orig4[,"Price"]) * (10^digits_RHS_price)) %% (10^digits_RHS_price)
orig4[,"Price_First_Digit_RHS"] <- ifelse(orig4[,"Price_FirstTwo_Digits_RHS"]<((10^digits_RHS_price)/10),
                                          round(orig4[,"Price_FirstTwo_Digits_RHS"]),
                                          orig4[,"Price_FirstTwo_Digits_RHS"] %/% 10)


######## Compute First Digit RHS - Return

orig4[,"Ret_FirstTwo_Digits_RHS"] <- round(abs(orig4[,"Ret"]) * (10^digits_RHS_ret)) %% (10^digits_RHS_ret)
orig4[,"Ret_First_Digit_RHS"] <- ifelse(orig4[,"Ret_FirstTwo_Digits_RHS"]<((10^digits_RHS_ret)/10),
                                        round(orig4[,"Ret_FirstTwo_Digits_RHS"]),
                                        orig4[,"Ret_FirstTwo_Digits_RHS"] %/% 10)


rm(digits_RHS_price,digits_RHS_ret)

rm(orig3)
gc()


###############################################################################
cat("SECTION: RUN BEDFORD TEST", "\n")
###############################################################################

#test_col <- "Price_First_Digit_RHS"
#test_col <- "Ret_First_Digit_RHS"
test_col <- "Ret_Percent_First_Digit_LHS"

######## Get Counts for Each Digit

df2_groups_temp <- ddply(.data=orig4, .variables=c("Item_Order","Item"), .fun = function(x,col){ 
  
  # x <- orig4[orig4[,"Item"] %in% "Cereal",]
  
  return(count(x, col))
  
},col=test_col,.drop = FALSE)


df2_overall_temp0 <- count(orig4, test_col)
df2_overall_temp <- data.frame(Item_Order=NA,
                               Item=NA,
                               df2_overall_temp0,
                               stringsAsFactors=FALSE)
df2_overall_temp[,"Item_Order"] <- 0
df2_overall_temp[,"Item"] <- "Overall"

df2_all_temp <- rbind(df2_overall_temp,df2_groups_temp)

rm(df2_groups_temp,df2_overall_temp0,df2_overall_temp)


######## Make Sure that All Digits are Present

df2_all_temp_ids <- unique(df2_all_temp[,c("Item_Order","Item")])
row.names(df2_all_temp_ids) <- seq(nrow(df2_all_temp_ids))

df2_all_temp_full <- sapply(data.frame(df2_all_temp_ids,temp_col=NA,freq=0,stringsAsFactors=FALSE), 
                            rep.int, 
                            times=10)
df2_all_temp_full <- as.data.frame(df2_all_temp_full,stringsAsFactors=FALSE)


rm(df2_all_temp_ids)

df2_all_temp_full <- df2_all_temp_full[order(df2_all_temp_full[,"Item_Order"],
                                             df2_all_temp_full[,"Item"]),]
row.names(df2_all_temp_full) <- seq(nrow(df2_all_temp_full))

df2_all_temp_full <- ddply(.data=df2_all_temp_full, .variables=c("Item_Order"), .fun = function(x){ 
  
  #  x <- df2_all_temp_full[df2_all_temp_full[,"Item_Order"]==0,]
  
  x[,"temp_col"] <- seq(0,9,1)
  
  return(x)
  
},.drop = FALSE)

colnames(df2_all_temp_full)[match("temp_col",names(df2_all_temp_full))] <- test_col

for (i in 1:nrow(df2_all_temp))
{
  #i <- 1
  
  df2_all_temp_full[,"freq"] <- ifelse((df2_all_temp_full[,"Item_Order"]==df2_all_temp[i,"Item_Order"] & 
                                          df2_all_temp_full[,"Item"]==df2_all_temp[i,"Item"] & 
                                          df2_all_temp_full[,test_col]==df2_all_temp[i,test_col]) ,
                                       df2_all_temp[i,"freq"], 
                                       df2_all_temp_full[,"freq"])
  
}
rm(i)

df2_all <- df2_all_temp_full
df2_all[,"Item_Order"] <- as.integer(df2_all[,"Item_Order"])
df2_all[,test_col] <- as.numeric(df2_all[,test_col])
df2_all[,"freq"] <- as.numeric(df2_all[,"freq"])

rm(df2_all_temp_full,df2_all_temp)


######## Compute Stats

df3_all <- ddply(.data=df2_all, .variables=c("Item_Order"), .fun = function(x,col){ 
  
  # x <- df2_all[df2_all[,"Item"] %in% "Overall",]
  # x <- df2_all[df2_all[,"Item"] %in% "Cereal",]
  
  x_out <- data.frame(x,
                      prob=NA,
                      benford = NA,
                      stringsAsFactors=FALSE)
  x_out[,"prob"] <- prop.table(x_out[,"freq"])
  x_out[,"benford"] <- log10(x_out[,col]+ 1) - log10(x_out[,col] + 0)
  
  return(x_out)
  
},col=test_col,.drop = FALSE)


######## Graph

df3_temp <- df3_all[df3_all[,"Item"] %in% "Overall",]

colnames(df3_temp)[match(test_col,names(df3_temp))] <- "fd"

ggplot(df3_temp, aes(x = fd, y = prob)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_line(aes(x = fd, y = benford, size = 0.1)) +
  geom_point(aes(x = fd, y = benford, color = "red", size = 1)) +
  theme_bw()

scale_x_continuous(breaks = seq(1:9))

