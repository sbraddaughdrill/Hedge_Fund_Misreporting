# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Screens_Combination.R
# Version: 1.0
# Date: 09.8.2014
# Purpose: Compute descriptive statistics
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP","\n")
###############################################################################

# Clear workspace
rm(list=ls(all=T))
rm(list=ls(all.names=T))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE=500)

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=F)

# String as factors is F -- used for read.csv
options(StringsAsFactors=F)

# Default maxprint option
options(max.print=500)
# options(max.print=99999)

# Memory limit
#memory.limit(size=8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1


if (Location == 1) {
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T) 
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep=""),echo=F)
#source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
#source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=F)

###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
external_packages <- c("plyr")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=F,USE.NAMES=F)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("SETUP","\n")
###############################################################################

identifier <- "Fund_ID"

pct <- c(90,95,99)
type <- c("any","avg")
cutoff_nums <- c(24,36,48,60)


###############################################################################
cat("IMPORT REVISION DATA","\n")
###############################################################################

revision_directory <- normalizePath("F:/Import_Data/Data/Eurekahedge",winslash="\\",mustWork=T)
revision_folder_path <- paste(revision_directory,"Revision",sep="//",collapse="//")  

#Ret0_sources <- read.csv(file=paste(revision_folder_path,"//","Ret0_sources",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
#Ret0_sources_flags <- read.csv(file=paste(revision_folder_path,"//","Ret0_sources_flags",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
Ret0_sources_flags_sum <- read.csv(file=paste(revision_folder_path,"//","Ret0_sources_flags_sum",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)

###############################################################################
cat("CLEAN REVISIONS","\n")
###############################################################################

Ret0_sources_flags_sum_trim <- Ret0_sources_flags_sum[,c(identifier,"Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")]

rm(Ret0_sources_flags_sum)


###############################################################################
cat("IMPORT FUND DATA","\n")
###############################################################################

data_prescreen <- read.csv(file=paste(output_directory,"data_prescreen",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)

#data_prescreen_trim <- read.csv(file=paste(output_directory,"data_prescreen_trim",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
data_prescreen_trim_comb <- ldply(.data=cutoff_nums, .fun=function(x,dir){
  name <- paste("data_prescreen_trim_",sprintf("%03d",unlist(x)),sep="")
  #assign(name,read.csv(file=paste(dir,name,".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F),envir=.GlobalEnv)
  out <- data.frame(cutoff=sprintf("%03d",unlist(x)),read.csv(file=paste(dir,name,".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F),stringsAsFactors=F)
  return(out)
},dir=output_directory,.progress="none")


###############################################################################
cat("IMPORT SCREEN DATA","\n")
###############################################################################

ids_u0 <- c(sort(unique(Ret0_sources_flags_sum_trim[,identifier])),sort(unique(data_prescreen[,identifier])),sort(unique(data_prescreen_trim_comb[,identifier])))
ids_u <- data.frame(temp_id=sort(unique(ids_u0)),stringsAsFactors=F)
colnames(ids_u)[match("temp_id",names(ids_u))] <- identifier

rm(ids_u0)

#data_screens <- read.csv(file=paste(output_directory,"data_screens",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
data_screens_comb0 <- llply(.data=cutoff_nums, .fun=function(x,dir,ids){
  
  # x <- cutoff_nums[[1]]
  # dir <- output_directory
  # ids <- ids_u
  
  name <- paste("data_screens_",sprintf("%03d",unlist(x)),sep="")
  #assign(name,read.csv(file=paste(dir,name,".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F),envir=.GlobalEnv)
  out <- data.frame(cutoff=sprintf("%03d",unlist(x)),read.csv(file=paste(dir,name,".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F),stringsAsFactors=F)
  
  cols_id <- colnames(out)[(colnames(out) %in% identifier)]
  cols_nonid <- colnames(out)[!(colnames(out) %in% identifier)]
  out <- out[,c(cols_id,cols_nonid)]
  colnames(out) <- c(cols_id,paste(cols_nonid,sprintf("%03d",unlist(x)),sep="_"))
  out <- out[order(out[,identifier]),] 
  row.names(out) <- seq(nrow(out))
  
  out2 <- merge(ids,out,
                by.x=identifier,by.y=identifier,
                all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))
  
  out2 <- out2[order(out2[,identifier]),] 
  row.names(out2) <- seq(nrow(out2))
  
  return(out2)
},dir=output_directory,ids=ids_u,.progress="none")

names(data_screens_comb0) <- rep("",length(data_screens_comb0))

data_screens_comb1 <- do.call(cbind,data_screens_comb0)
data_screens_comb1 <- data_screens_comb1[order(data_screens_comb1[,identifier]),] 
row.names(data_screens_comb1) <- seq(nrow(data_screens_comb1))

data_screens_comb2 <- data_screens_comb1[,unique(colnames(data_screens_comb1))]

data_screens_comb3 <- data_screens_comb2[rowSums(is.na(data_screens_comb2[,2:ncol(data_screens_comb2)]))<ncol(data_screens_comb2)-1,]

data_screens_comb4 <- data_screens_comb3[,colnames(data_screens_comb3)[!(colnames(data_screens_comb3) %in% paste("cutoff",sprintf("%03d",cutoff_nums),sep="_"))]]

data_screens_comb4 <- data_screens_comb4[order(data_screens_comb4[,identifier]),] 
row.names(data_screens_comb4) <- seq(nrow(data_screens_comb4))
  
rm2(ids_u,data_screens_comb0,data_screens_comb1,data_screens_comb2,data_screens_comb3)

# aa <- c(sort(unique(data_screens_024[,identifier])),
#         sort(unique(data_screens_036[,identifier])),
#         sort(unique(data_screens_048[,identifier])),
#         sort(unique(data_screens_060[,identifier])))
# bb <- sort(unique(aa))


###############################################################################
cat("MERGE REVISIONS AND SCREEN DATA","\n")
###############################################################################

data_screens_comb <- merge(data_screens_comb4,Ret0_sources_flags_sum_trim,
                           by.x=c(identifier),by.y=c(identifier),
                           all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

data_screens_comb <- data_screens_comb[order(data_screens_comb[,identifier]),]
row.names(data_screens_comb) <- seq(nrow(data_screens_comb))

rm2(data_screens_comb4,Ret0_sources_flags_sum_trim)


###############################################################################
cat("CREATE QUALITY SCORE","\n")
###############################################################################

#score_input <- expand.grid(type,pct)
score_input0 <- ldply(.data=type,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x, rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=pct,cols_out=c("type","pct"))

score_input <- adply(.data=score_input0,.margins=1,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x, rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=cutoff_nums,cols_out=c(colnames(score_input0),"cutoff_nums"))

score_input[,"pct"] <- as.integer(score_input[,"pct"])
score_input[,"cutoff_nums"] <- as.integer(score_input[,"cutoff_nums"])

score_input <- score_input[order(score_input[,"type"],-score_input[,"pct"],score_input[,"cutoff_nums"]),] 
row.names(score_input) <- seq(nrow(score_input))

score_input[,"cutoff_nums"] <- sprintf("%03d",score_input[,"cutoff_nums"])

rm(score_input0)

score_cols <- c("quality_score_trim0","quality_score_trim1","quality_score_trim2","quality_score_trim3","quality_score_trim4")

data_screens_score_comb0 <- apply(score_input,1,function(x,data,score_cols){
  
  # x <- score_input[1,]
  # x <- score_input[2,]
  # data <- data_screens_comb

  type <- x[["type"]]
  pct <-  x[["pct"]]
  cutoff_num <-  x[["cutoff_nums"]]
  suffix <- paste(pct,type,cutoff_num,sep="_")
  
  temp_all_cols <- colnames(data)
  temp_all_cols_type <- temp_all_cols[grep(paste("_",type,"_",sep=""),temp_all_cols)]
  temp_all_cols_pct <- temp_all_cols_type[grep(paste("_",pct,"_",sep=""),temp_all_cols_type)]
  temp_all_cols_cutoff <- temp_all_cols_pct[grep(paste("_",cutoff_num,"",sep=""),temp_all_cols_pct)]
  
  temp_data <- data[,c(identifier,temp_all_cols_cutoff,"Revision_DV")]

  # Full
  trim0_col <- colnames(temp_data)[!(colnames(temp_data) %in% identifier)]
  
  # No Percent Positive
  trim1_col <- trim0_col[!(trim0_col %in% trim0_col[grep("per_positive",trim0_col)])]
  
  # No Revision DV
  trim2_col <- trim0_col[!(trim0_col %in% trim0_col[grep("Revision_DV",trim0_col)])]
  
  # No positive or repeats
  trim3_col <- trim0_col[!(trim0_col %in% trim0_col[grep("per_positive",trim0_col)])]
  trim3_col <- trim3_col[!(trim3_col %in% trim3_col[grep("kink_percent",trim3_col)])]
  trim3_col <- trim3_col[!(trim3_col %in% trim3_col[grep("indexrsq",trim3_col)])]
  trim3_col <- trim3_col[!(trim3_col %in% trim3_col[grep("ar_1_percent",trim3_col)])]
  trim3_col <- trim3_col[!(trim3_col %in% trim3_col[grep("Revision_DV",trim3_col)])]
  
  # Only the ones that are most significant
  trim4_col <- trim0_col[!(trim0_col %in% trim0_col[grep("per_positive",trim0_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("kink_percent",trim4_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("indexrsq",trim4_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("string",trim4_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("num_pairs",trim4_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("uniform",trim4_col)])]
  trim4_col <- trim4_col[!(trim4_col %in% trim4_col[grep("Revision_DV",trim4_col)])]
  
  temp_score <- data.frame(temp_data,matrix(NA,ncol=length(score_cols),nrow=1,dimnames=list(c(),paste(score_cols,suffix,sep="_"))),stringsAsFactors=F)
  temp_score[,paste(score_cols[1],suffix,sep="_")] <- ifelse(rowSums(is.na(temp_score[,trim0_col]))==length(trim0_col),NA,rowSums(temp_score[,trim0_col],na.rm=T))
  temp_score[,paste(score_cols[2],suffix,sep="_")] <- ifelse(rowSums(is.na(temp_score[,trim1_col]))==length(trim1_col),NA,rowSums(temp_score[,trim1_col],na.rm=T))
  temp_score[,paste(score_cols[3],suffix,sep="_")] <- ifelse(rowSums(is.na(temp_score[,trim2_col]))==length(trim2_col),NA,rowSums(temp_score[,trim2_col],na.rm=T))
  temp_score[,paste(score_cols[4],suffix,sep="_")] <- ifelse(rowSums(is.na(temp_score[,trim3_col]))==length(trim3_col),NA,rowSums(temp_score[,trim3_col],na.rm=T))
  temp_score[,paste(score_cols[5],suffix,sep="_")] <- ifelse(rowSums(is.na(temp_score[,trim4_col]))==length(trim4_col),NA,rowSums(temp_score[,trim4_col],na.rm=T))
  
  temp_score <- temp_score[order(temp_score[,identifier]),]
  row.names(temp_score) <- seq(nrow(temp_score))

  return(temp_score[,c(identifier,paste(score_cols,suffix,sep="_"))])
  
},data=data_screens_comb,score_cols=score_cols)

names(data_screens_score_comb0) <- rep("",length(data_screens_score_comb0))

data_screens_score_comb1 <- do.call(cbind,data_screens_score_comb0)
data_screens_score_comb1 <- data_screens_score_comb1[order(data_screens_score_comb1[,identifier]),] 
row.names(data_screens_score_comb1) <- seq(nrow(data_screens_score_comb1))

data_screens_score_comb <- data_screens_score_comb1[,unique(colnames(data_screens_score_comb1))]

rm2(data_screens_score_comb0,data_screens_score_comb1)
rm2(score_input)


###############################################################################
cat("MERGE SCREENS","\n")
###############################################################################

data_screens_full_all0 <- data.frame(temp_id=unique(data_prescreen[,identifier]),stringsAsFactors=F)
colnames(data_screens_full_all0)[match("temp_id",names(data_screens_full_all0))] <- identifier

data_screens_full_all1 <- merge(data_screens_full_all0,data_screens_comb,
                                by.x=c(identifier),by.y=c(identifier),
                                all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

data_screens_full_all2 <- merge(data_screens_full_all1,data_screens_score_comb,
                                by.x=c(identifier),by.y=c(identifier),
                                all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

data_screens_full_all <- data_screens_full_all2

data_screens_full_all <- data_screens_full_all[order(data_screens_full_all[,identifier]),]
row.names(data_screens_full_all) <- seq(nrow(data_screens_full_all))

rm(data_screens_full_all0,data_screens_full_all1,data_screens_full_all2)

#aa <- data_screens_full_all[,"kink_percent_99_any_024"]
#bb <- aa[!is.na(aa)]


###############################################################################
cat("MERGE DATA","\n")
###############################################################################

data_all0 <- merge(unique(data_prescreen_trim_comb[,c(identifier,"yr","month","yr_month")]),data_prescreen,
                   by.x=c(identifier,"yr","month","yr_month"),by.y=c(identifier,"yr","month","yr_month"),
                   all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

data_all1 <- merge(data_all0,data_screens_full_all,
                   by.x=c(identifier),by.y=c(identifier),
                   all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))

data_all <- data_all1

data_all <- data_all[order(data_all[,identifier],data_all[,"yr"],data_all[,"month"]),]
row.names(data_all) <- seq(nrow(data_all))

rm(data_all0,data_all1)

#cc <- unique(data_all[,identifier])

###############################################################################
cat("OUTPUT DATA","\n")
###############################################################################

write.csv(data_all,file=paste(output_directory,"data_all.csv",sep="\\"),na="",quote=T,row.names=F)


