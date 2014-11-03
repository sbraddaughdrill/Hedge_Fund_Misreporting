# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Univariate_Inverse.R
# Version: 1.0
# Date: 09.23.2014
# Purpose: Run Univariate Analysis
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
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

quantile_cuts2 <- function(x,data,dep_var,quantile_count_dep,quantile_count_indep,group_var,quantile_var,cut_group1,cut_group2){
  
  # x <- vars_indep[[1]]
  # x <- vars_indep[[10]]
  # data <- data
  # dep_var <- dep_var
  # quantile_count_dep <- quantile_count_dep                
  # quantile_count_indep <- 1
  # group_var <- group_var
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  ### YEAR
  # cut_group1 <- c(group_var)
  # cut_group2 <- c(group_var,"quantile_var_indep1")
  
  ### AGG
  # cut_group1 <- NULL
  # cut_group2 <- c("quantile_var_indep1")
  
  require(plyr)  
  
  quantiles <- data.frame(data[,c(dep_var,group_var,x)],matrix(NA, ncol=2, nrow=nrow(data), dimnames=list(c(),quantile_var)),stringsAsFactors=FALSE)
  quantiles <- quantiles[!is.na(quantiles[,x]),]
  quantiles <- quantiles[order(quantiles[,group_var],quantiles[,x],quantiles[,dep_var]),] 
  
  quantiles <- ddply(.data=quantiles, .variables=cut_group1, 
                     function(z,var,quantile_num,col,eps){
                       
                       z[,col] <- as.integer(with(z, cut(z[,var], breaks=quantile(z[,var], probs=(0:quantile_num)/quantile_num,na.rm=TRUE)+eps*(0:quantile_num),include.lowest=TRUE)))
                       
                       return(z)
                     }, var=x, quantile_num=quantile_count_dep,col=quantile_var[1],eps=.Machine$double.eps )
  
  quantiles <- ddply(.data=quantiles, .variables=cut_group2, 
                     function(z,var,quantile_num,col,eps){
                       
                       z[,col] <- as.integer(with(z, cut(z[,var], breaks=quantile(z[,var], probs=(0:quantile_num)/quantile_num,na.rm=TRUE)+eps*(0:quantile_num),include.lowest=TRUE)))
                       
                       return(z)
                     }, var=x, quantile_num=quantile_count_indep,col=quantile_var[2],eps=.Machine$double.eps )
  
  quantiles <- quantiles[,(colnames(quantiles) %in% c(group_var,dep_var,x,quantile_var))]
  quantiles <- quantiles[,c(group_var,dep_var,colnames(quantiles[,!(colnames(quantiles) %in% c(group_var,dep_var))]))]
  
  quantiles <- quantiles[order(quantiles[,group_var],quantiles[,quantile_var[1]],quantiles[,quantile_var[2]],quantiles[,x],quantiles[,dep_var]),] 
  row.names(quantiles) <- seq(nrow(quantiles))
  
  return(quantiles)
  
}


quantile_cuts_manual2 <- function(x,data,dep_var,quantile_count_dep,quantile_count_indep,group_var,quantile_var,cut_group1,cut_group2,breaks){
  
  # x <- vars_indep[[1]]
  # x <- "quality_score_trim2_90"
  # data <- data
  # dep_var <- dep_var
  # quantile_count_dep <- quantile_count_dep                
  # quantile_count_indep <- 1
  # group_var <- group_var
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  ### YEAR
  # cut_group1 <- c(group_var)
  # cut_group2 <- c(group_var,"quantile_var_indep1")
  
  ### AGG
  # cut_group1 <- NULL
  # cut_group2 <- c("quantile_var_indep1")
  
  require(plyr)  
  
  quantiles <- data.frame(data[,c(dep_var,group_var,x)],matrix(NA, ncol=2, nrow=nrow(data), dimnames=list(c(),quantile_var)),stringsAsFactors=FALSE)
  quantiles <- quantiles[!is.na(quantiles[,x]),]
  quantiles <- quantiles[order(quantiles[,group_var],quantiles[,x],quantiles[,dep_var]),] 
  
  breaks_trim <- breaks[breaks[,"indep_var"]==x,]
  breaks_trim <- breaks_trim[order(breaks_trim[,"start_break"],breaks_trim[,"end_break"]),] 
  row.names(breaks_trim) <- seq(nrow(breaks_trim))
  
  
  for (i in 1:nrow(breaks_trim))
  {
    #i <- 1
    #i <- 2
    
    quantiles[,quantile_var[1]] <- ifelse(quantiles[,x]>=breaks_trim[i,"start_break"] & quantiles[,x]<=breaks_trim[i,"end_break"],i, quantiles[,quantile_var[1]])
    
  }
  
  
  #   quantiles <- ddply(.data=quantiles, .variables=cut_group1, 
  #                      function(z,var,quantile_num,col,eps){
  #                        # z <- quantiles[quantiles[,group_var]=="1994_01",]
  
  #                        z[,col] <- as.integer(with(z, cut(z[,var], breaks=quantile(z[,var], probs=(0:quantile_num)/quantile_num,na.rm=TRUE)+eps*(0:quantile_num),include.lowest=TRUE)))
  
  #                        return(z)
  #                      }, var=x, quantile_num=quantile_count_dep,col=quantile_var[1],eps=.Machine$double.eps )
  
  quantiles[,quantile_var[2]] <- 1
  
  #   quantiles <- ddply(.data=quantiles, .variables=cut_group2, 
  #                      function(z,var,quantile_num,col,eps){
  #                        
  #                        z[,col] <- as.integer(with(z, cut(z[,var], breaks=quantile(z[,var], probs=(0:quantile_num)/quantile_num,na.rm=TRUE)+eps*(0:quantile_num),include.lowest=TRUE)))
  #                        
  #                        return(z)
  #                      }, var=x, quantile_num=quantile_count_indep,col=quantile_var[2],eps=.Machine$double.eps )
  
  
  
  quantiles <- quantiles[,(colnames(quantiles) %in% c(group_var,dep_var,x,quantile_var))]
  quantiles <- quantiles[,c(group_var,dep_var,colnames(quantiles[,!(colnames(quantiles) %in% c(group_var,dep_var))]))]
  
  quantiles <- quantiles[order(quantiles[,group_var],quantiles[,quantile_var[1]],quantiles[,quantile_var[2]],quantiles[,x],quantiles[,dep_var]),] 
  row.names(quantiles) <- seq(nrow(quantiles))
  
  return(quantiles)
  
}


quantile_melt2 <- function(quantiles,data,dep_var,indep_var,group_var,quantile_var){
  
  require(plyr)
  
  # quantiles <- quantiles3
  # dep_var <- dep_var   
  # indep_var <- indep_var
  # group_var <- group_var
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  quantiles_melt <- ddply(.data=quantiles, .variables=c(group_var), 
                          function(y,var_dep,quantile_dep,var_indep,quantile_indep) {
                            
                            # y <- quantiles[quantiles[,group_var]=="1994_01",]
                            
                            bb_dep <- melt(data=y,id.vars=c(quantile_dep),measure.vars=c(var_dep))
                            colnames(bb_dep)[match(c("variable"),names(bb_dep))] <- "variable_dep"
                            colnames(bb_dep)[match(c("value"),names(bb_dep))] <- "value_dep"
                            bb_dep[sapply(bb_dep, is.factor)] <- lapply(bb_dep[sapply(bb_dep, is.factor)], as.character)
                            bb_dep <- as.data.frame(bb_dep, stringsAsFactors=FALSE)
                            
                            bb_indep <- melt(data=y,id.vars=c(quantile_indep),measure.vars=c(var_indep))
                            colnames(bb_indep)[match(c("variable"),names(bb_indep))] <- "variable_indep"
                            colnames(bb_indep)[match(c("value"),names(bb_indep))] <- "value_indep"
                            bb_indep[sapply(bb_indep, is.factor)] <- lapply(bb_indep[sapply(bb_indep, is.factor)], as.character)
                            bb_indep <- as.data.frame(bb_indep, stringsAsFactors=FALSE)
                            bb <- cbind(bb_dep,bb_indep)
                            
                          },var_dep=dep_var,quantile_dep=quantile_var[1],var_indep=indep_var,quantile_indep=quantile_var[2])
  
  return(quantiles_melt)
}


quantile_cast2 <- function(quantiles_melt,group_var,quantile_var){
  
  # quantiles_melt <- quantiles_melt
  # quantiles_melt <- quantiles_melt1
  # quantiles_melt <- quantiles_melt2
  
  # group_var <- group_var
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  require(plyr)
  
  #quantiles_melt <- quantiles_melt[,!(colnames(quantiles_melt) %in% c("value_indep"))]
  #quantiles_melt <- quantiles_melt[,c(group_var,"variable_dep","variable_indep",quantile_var,"value_dep")]
  #quantiles_melt <- quantiles_melt[order(quantiles_melt[,group_var],quantiles_melt[,"variable_dep"],quantiles_melt[,"variable_indep"],quantiles_melt[,quantile_var[1]], quantiles_melt[,quantile_var[2]]),] 
  
  colnames(quantiles_melt)[match(c("value_indep"),names(quantiles_melt))] <- "id"
  quantiles_melt[,"id"] <- NA
  quantiles_melt <- quantiles_melt[,c(group_var,"variable_dep","variable_indep",quantile_var,"value_dep","id")]
  quantiles_melt <- quantiles_melt[order(quantiles_melt[,group_var],quantiles_melt[,"variable_dep"],quantiles_melt[,"variable_indep"],quantiles_melt[,quantile_var[1]], quantiles_melt[,quantile_var[2]]),] 
  
  quantiles6 <- ddply(.data=(quantiles_melt), .variables=c(group_var,"variable_dep","variable_indep",quantile_var), 
                      function(x,var) {x[,"id"] <- seq_along(x[,var]) ; return(x)},var="value_dep")
  
  quantiles_cast <- ddply(.data=quantiles6, .variables=c(group_var), 
                          function(x,group_var,cat1,quantile1,cat2,quantile2,value_var,id_var) {
                            
                            # x <- quantiles6[quantiles6[,"yr"]==1994,]
                            # x <- quantiles6[quantiles6[,group_var]=="1994_01",]
                            
                            # cat1 <- "variable_dep"
                            # quantile1 <- quantile_var[1]
                            # cat2 <- "variable_indep"
                            # quantile2 <- quantile_var[2]
                            # value_var <- "value_dep"
                            # id_var <- "id"
                            
                            x2 <- x[,!(colnames(x) %in% group_var)]
                            
                            tempcast <- dcast(x2,formula=eval(parse(text=paste(paste(cat2,quantile2,id_var,sep="+"),paste(cat1,quantile1,sep="+"),sep="~"))), 
                                              value.var=value_var, fill = NA_real_, fun.aggregate=function(X) mean(X, na.rm=TRUE), margins=c(cat1, cat2))
                            
                            return(tempcast)
                            
                          },group_var=group_var,cat1="variable_dep",quantile1=quantile_var[1],cat2="variable_indep",quantile2=quantile_var[2],value_var="value_dep",id_var="id")
  
  quantiles_cast2a <- quantiles_cast
  quantiles_cast2a[sapply(quantiles_cast2a, is.factor)] <- lapply(quantiles_cast2a[sapply(quantiles_cast2a, is.factor)], as.character)
  
  quantiles_cast2b <- as.data.frame(quantiles_cast2a,stringsAsFactors=FALSE)
  quantiles_cast2b <- quantiles_cast2b[,!(colnames(quantiles_cast2b) %in% c("id","(all)_(all)"))]
  quantiles_cast2b <- quantiles_cast2b[!(quantiles_cast2b[,"variable_indep"] %in% c("(all)")),]
  quantiles_cast2b <- quantiles_cast2b[!(quantiles_cast2b[,quantile_var[2]] %in% c("(all)")),]
  quantiles_cast2b <- quantiles_cast2b[order(quantiles_cast2b[,group_var],quantiles_cast2b[,"variable_indep"],quantiles_cast2b[,quantile_var[2]]),] 
  row.names(quantiles_cast2b) <- seq(nrow(quantiles_cast2b))
  
  #quantiles_cast2b <- as.data.frame(quantiles_cast2b,stringsAsFactors=FALSE)
  
  quantiles_cast_sort <- ddply(quantiles_cast2b, c(group_var,"variable_indep",quantile_var[2]), function(z){
    
    temp1 <- z[,!(colnames(z) %in% c(group_var,"variable_indep","quantile_var_indep2"))]
    temp2 <- alply(.data=temp1, .margins=2, function(x){
      
      x2 <- data.frame(x[order(x[,1]),], stringsAsFactors=FALSE)
      colnames(x2) <- colnames(x)
      return(x2)
    }, .expand = FALSE)
    temp3 <- do.call(cbind,temp2)
    
    return(temp3)
  })
  row.names(quantiles_cast_sort) <- seq(nrow(quantiles_cast_sort))
  
  quantiles_cast_trim <- quantiles_cast_sort[!(rowSums(is.na(quantiles_cast_sort[,4:ncol(quantiles_cast_sort)]))==(ncol(quantiles_cast_sort)-3)),]
  row.names(quantiles_cast_trim) <- seq(nrow(quantiles_cast_trim))
  
  return(quantiles_cast_trim)
  
}

univariate_bins_continuous <- function(data,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
  
  # dep_var <- dep_var
  # vars_indep <- univariate_vars_indep_continuous
  # parameters <- x
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  require(plyr)
  
  note <- parameters$note
  group_var <- parameters$group_var
  quantile_count_dep <- as.integer(parameters$nums)
  quantile_count_indep <- 1
  quantile_type <- "quantile"
  group <- parameters$type
  
  #name1 <- paste("quantiles",group,dep_var,"yearly",note,quantile_count_dep,sep="_")
  
  quantiles_pct_flow_temp <- llply(.data=vars_indep,.fun=function(v,data,dep_var,quantile_type,quantile_count_dep,quantile_count_indep,group_var,group,quantile_var){
    
    # v <- vars_indep[[1]]
    # v <- vars_indep[[2]]
    # v <- vars_indep[[35]]
    
    indep_var <- unlist(v)
    
    cat("\n","INDEP VAR:",indep_var, "\n")
    
    #cat("CUT", "\n")
    if (group == "year") {
      
      quantiles3 <- quantile_cuts2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                   quantile_var=quantile_var,cut_group1=c(group_var),cut_group2=c(group_var,quantile_var[1]))
      
    } else if (group == "agg") {
      
      quantiles3 <- quantile_cuts2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                   quantile_var=quantile_var,cut_group1=NULL,cut_group2=c(quantile_var[1]))
      
    } else { 
      
      cat("ERROR IN GROUPS", "\n")
      
    }
    
    #cat("MELT", "\n")
    quantiles_melt <- quantile_melt2(quantiles=quantiles3,dep_var=dep_var,indep_var=indep_var,group_var=group_var,quantile_var=quantile_var)
    
    
    if (quantile_type == "dv") {
      quantiles_melt[,quantile_var[1]] <- quantiles_melt[,"value_indep"]
    }
    
    #cat("CAST", "\n")
    quantiles_cast <- quantile_cast2(quantiles_melt=quantiles_melt,group_var=group_var,quantile_var=quantile_var)
    
  },data=data,dep_var=dep_var,quantile_type=quantile_type,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,
  group_var=group_var,group=group,quantile_var=quantile_var,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
  
  quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
  
  rm(quantiles_pct_flow_temp)
  
  quantiles_pct_flow <- quantiles_pct_flow[,c(group_var,"variable_indep",quantile_var[2],
                                              colnames(quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% c(group_var,"variable_indep",quantile_var[2]))]))]
  colnames(quantiles_pct_flow) <- c(group_var,"cut_var",quantile_var[2],paste("X",seq(1,quantile_count_dep),sep=""))
  row.names(quantiles_pct_flow) <- seq(nrow(quantiles_pct_flow))
  
  rm(note,group_var,quantile_count_dep,quantile_count_indep,quantile_type,group)
  
  return(quantiles_pct_flow)
  
}


univariate_bins_binary <- function(data,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
  
  # dep_var <- dep_var
  # vars_indep <- univariate_vars_indep_binary
  # parameters <- x
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  require(plyr)
  
  note <- parameters$note
  group_var <- parameters$group_var
  quantile_count_dep <- as.integer(parameters$nums)
  quantile_count_indep <- 1
  quantile_type <- "dv"
  group <- parameters$type
  
  quantiles_pct_flow_temp <- llply(.data=vars_indep,.fun=function(v,data,dep_var,quantile_type,quantile_count_dep,quantile_count_indep,group_var,group,quantile_var){
    
    # v <- vars_indep[[1]]
    # v <- vars_indep[[2]]
    # v <- vars_indep[[10]]
    
    indep_var <- unlist(v)
    
    cat("\n","INDEP VAR:",indep_var, "\n")
    
    #cat("CUT", "\n")
    if (group == "year") {
      
      quantiles3 <- quantile_cuts2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                   quantile_var=quantile_var,cut_group1=c(group_var),cut_group2=c(group_var,quantile_var[1]))
      
    } else if (group == "agg") {
      
      quantiles3 <- quantile_cuts2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                   quantile_var=quantile_var,cut_group1=NULL,cut_group2=c(quantile_var[1]))
      
    } else { 
      
      cat("ERROR IN GROUPS", "\n")
      
    }
    
    #cat("MELT", "\n")
    quantiles_melt <- quantile_melt2(quantiles=quantiles3,dep_var=dep_var,indep_var=indep_var,group_var=group_var,quantile_var=quantile_var)
    
    
    if (quantile_type == "dv") {
      quantiles_melt[,quantile_var[1]] <- quantiles_melt[,"value_indep"]
    }
    
    #cat("CAST", "\n")
    quantiles_cast <- quantile_cast2(quantiles_melt=quantiles_melt,group_var=group_var,quantile_var=quantile_var)
    
  },data=data,dep_var=dep_var,quantile_type=quantile_type,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,
  group_var=group_var,group=group,quantile_var=quantile_var,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
  
  quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
  
  rm(quantiles_pct_flow_temp)
  
  quantiles_pct_flow <- quantiles_pct_flow[,c(group_var,"variable_indep",quantile_var[2],
                                              colnames(quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% c(group_var,"variable_indep",quantile_var[2]))]))]
  colnames(quantiles_pct_flow) <- c(group_var,"cut_var",quantile_var[2],paste("X",seq(0,(quantile_count_dep-1)),sep=""))
  row.names(quantiles_pct_flow) <- seq(nrow(quantiles_pct_flow))
  
  rm(note,group_var,quantile_count_dep,quantile_count_indep,quantile_type,group)
  
  return(quantiles_pct_flow)
  
}

univariate_bins_manual <- function(data,dep_var,dep_vars_all,vars_indep,parameters,quantile_var,breaks){
  
  # dep_var <- dep_var
  # vars_indep <- univariate_vars_indep_manual
  # parameters <- x
  # quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
  
  require(plyr)
  
  note <- parameters$note
  group_var <- parameters$group_var
  quantile_count_dep <- as.integer(parameters$nums)
  quantile_count_indep <- 1
  quantile_type <- "quantile"
  group <- parameters$type
  
  quantiles_pct_flow_temp <- llply(.data=vars_indep,.fun=function(v,data,dep_var,quantile_type,quantile_count_dep,quantile_count_indep,group_var,group,quantile_var,breaks){
    
    # v <- vars_indep[[1]]
    
    indep_var <- unlist(v)
    
    cat("\n","INDEP VAR:",indep_var, "\n")
    
    #cat("CUT", "\n")
    if (group == "year") {
      
      quantiles3 <- quantile_cuts_manual2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                          quantile_var=quantile_var,cut_group1=c(group_var),cut_group2=c(group_var,quantile_var[1]),breaks=breaks)
      
    } else if (group == "agg") {
      
      quantiles3 <- quantile_cuts_manual2(indep_var,data=data,dep_var=dep_var,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,group_var=group_var,
                                          quantile_var=quantile_var,cut_group1=NULL,cut_group2=c(quantile_var[1]),breaks=breaks)
      
    } else { 
      
      cat("ERROR IN GROUPS", "\n")
      
    }
    
    #cat("MELT", "\n")
    quantiles_melt <- quantile_melt2(quantiles=quantiles3,dep_var=dep_var,indep_var=indep_var,group_var=group_var,quantile_var=quantile_var)
    #quantiles_melt2 <- quantiles_melt
    
    if (quantile_type == "dv") {
      quantiles_melt[,quantile_var[1]] <- quantiles_melt[,"value_indep"]
    }
    #quantiles_melt1 <- quantiles_melt
    
    #cat("CAST", "\n")
    quantiles_cast <- quantile_cast2(quantiles_melt=quantiles_melt,group_var=group_var,quantile_var=quantile_var)
    
  },data=data,dep_var=dep_var,quantile_type=quantile_type,quantile_count_dep=quantile_count_dep,quantile_count_indep=1,
  group_var=group_var,group=group,quantile_var=quantile_var,breaks=breaks,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
  
  quantiles_pct_flow <- do.call(rbind.fill, quantiles_pct_flow_temp)
  
  rm(quantiles_pct_flow_temp)
  
  quantiles_pct_flow <- quantiles_pct_flow[,c(group_var,"variable_indep",quantile_var[2],
                                              colnames(quantiles_pct_flow[,!(colnames(quantiles_pct_flow) %in% c(group_var,"variable_indep",quantile_var[2]))]))]
  colnames(quantiles_pct_flow) <- c(group_var,"cut_var",quantile_var[2],paste("X",seq(1,quantile_count_dep),sep=""))
  row.names(quantiles_pct_flow) <- seq(nrow(quantiles_pct_flow))
  
  rm(note,group_var,quantile_count_dep,quantile_count_indep,quantile_type,group)
  
  return(quantiles_pct_flow)
  
}

univariate_bins_diff <- function(bins,range_str,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
  
  # bins <- quantiles_pct_flow
  # range_str <- "yearly"
  
  # bins <- bins_trim
  # range_str <- paste(Start_yr,End_yr,sep="_")
  
  ### Continuous
  # quantile_first_col <- "X1"
  # quantile_last_col <- paste("X",quantile_count_dep,sep="")
  # vars_indep <- univariate_vars_indep_continuous
  
  ### Binary
  # quantile_first_col <- "X0"
  # quantile_last_col <- paste("X",(quantile_count_dep-1),sep="")
  # vars_indep <- univariate_vars_indep_binary
  
  # dep_var <- dep_var
  # dep_vars_all <- dep_vars_all

  
  # parameters <- x
  # quantile_var <- quantile_var
  # quantile_first_col <- quantile_first_col
  # quantile_last_col <- quantile_last_col
  
  require(plyr)
  
  note <- parameters$note
  group_var <- parameters$group_var
  quantile_count_dep <- as.integer(parameters$nums)
  group <- parameters$type
  output_dir <- parameters$output_dir
  
  name1 <- paste("quantiles",group,dep_var,range_str,note,quantile_count_dep,sep="_")
  
  
  #averages_yr_quan_all_cast <- diff_in_mean(bins,c("cut_var",quantile_var[2]),group_var,quantile_first_col,quantile_last_col)
  averages_yr_quan_all_cast <- diff_in_mean2(bins,c("cut_var",quantile_var[2]),group_var,quantile_first_col,quantile_last_col)
  
  averages_yr_quan_all_cast <- ddply(.data=averages_yr_quan_all_cast, .variables=c(group_var,quantile_var[2]), 
                                     .fun = function(x,var_order){x[order(order(var_order)),] },var_order=vars_indep)
  
  averages_yr_quan_all_cast <- averages_yr_quan_all_cast[!(averages_yr_quan_all_cast[,"cut_var"] %in% dep_vars_all),]
  row.names(averages_yr_quan_all_cast) <- seq(nrow(averages_yr_quan_all_cast))
  
  for (j in 4:ncol(averages_yr_quan_all_cast))
  {
    # j <- 1
    averages_yr_quan_all_cast[,j] <- ifelse(is.infinite(averages_yr_quan_all_cast[,j]), NA, averages_yr_quan_all_cast[,j])
    averages_yr_quan_all_cast[,j] <- ifelse(is.na(averages_yr_quan_all_cast[,j]), NA, averages_yr_quan_all_cast[,j])
  }
  rm(j)  
  
  averages_yr_quan_all_cast[,"t_stat"] <- ifelse(is.na(averages_yr_quan_all_cast[,"t_p_val"]), NA, averages_yr_quan_all_cast[,"t_stat"])
  averages_yr_quan_all_cast[,"t_p_val"] <- ifelse(is.na(averages_yr_quan_all_cast[,"t_stat"]), NA, averages_yr_quan_all_cast[,"t_p_val"])
  
  averages_yr_quan_all_cast[,"f_stat"] <- ifelse(is.na(averages_yr_quan_all_cast[,"f_p_val"]), NA, averages_yr_quan_all_cast[,"f_stat"])
  averages_yr_quan_all_cast[,"f_p_val"] <- ifelse(is.na(averages_yr_quan_all_cast[,"f_stat"]), NA, averages_yr_quan_all_cast[,"f_p_val"])
  
  
  averages_yr_quan_all_cast <- averages_yr_quan_all_cast[,c(group_var,quantile_var[2],"cut_var",colnames(averages_yr_quan_all_cast[,!(colnames(averages_yr_quan_all_cast) %in% c(group_var,quantile_var[2],"cut_var"))]))]
  
  
  averages_yr_quan_all_cast2 <- data.frame(averages_yr_quan_all_cast,t_p_val_str=NA,f_p_val_str=NA,stringsAsFactors=FALSE)
  
  averages_yr_quan_all_cast2[,"t_p_val_str"] <- ifelse(averages_yr_quan_all_cast2[,"t_p_val"] < .0100, "***", 
                                                       ifelse(averages_yr_quan_all_cast2[,"t_p_val"] < .0500, "** ", 
                                                              ifelse(averages_yr_quan_all_cast2[,"t_p_val"] < .1000, "*  ", "   ")))
  averages_yr_quan_all_cast2[,"t_p_val_str"] <- ifelse(is.na(averages_yr_quan_all_cast2[,"t_p_val_str"]),"",averages_yr_quan_all_cast2[,"t_p_val_str"])
  
  averages_yr_quan_all_cast2[,"f_p_val_str"] <- ifelse(averages_yr_quan_all_cast2[,"f_p_val"] < .0100, "***", 
                                                       ifelse(averages_yr_quan_all_cast2[,"f_p_val"] < .0500, "** ", 
                                                              ifelse(averages_yr_quan_all_cast2[,"f_p_val"] < .1000, "*  ", "   ")))
  averages_yr_quan_all_cast2[,"f_p_val_str"] <- ifelse(is.na(averages_yr_quan_all_cast2[,"f_p_val_str"]),"",averages_yr_quan_all_cast2[,"f_p_val_str"])
  
  averages_yr_quan_all_cast2[,4:(ncol(averages_yr_quan_all_cast2)-2)] <- format(round(averages_yr_quan_all_cast2[,4:(ncol(averages_yr_quan_all_cast2)-2)],  digits = 4))
  
  averages_yr_quan_all_cast2[,"t_stat"] <- paste(averages_yr_quan_all_cast2[,"t_stat"],averages_yr_quan_all_cast2[,"t_p_val_str"],sep="")
  averages_yr_quan_all_cast2[,"f_stat"] <- paste(averages_yr_quan_all_cast2[,"f_stat"],averages_yr_quan_all_cast2[,"f_p_val_str"],sep="")
  
  averages_yr_quan_all_cast2[,"t_p_val"] <- paste(averages_yr_quan_all_cast2[,"t_p_val"],averages_yr_quan_all_cast2[,"t_p_val_str"],sep="")
  averages_yr_quan_all_cast2[,"f_p_val"] <- paste(averages_yr_quan_all_cast2[,"f_p_val"],averages_yr_quan_all_cast2[,"f_p_val_str"],sep="")
  
  averages_yr_quan_all_cast2 <- averages_yr_quan_all_cast2[!is.na(averages_yr_quan_all_cast2[,group_var]),]
  
  write.csv(averages_yr_quan_all_cast2,file=paste(output_dir,name1,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
  
  rm(note,group_var,quantile_count_dep,group,output_dir,name1)
  
  return(averages_yr_quan_all_cast2)
  
}


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","descr","fastmatch","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
                       "pander","pbapply","PerformanceAnalytics","plm","plyr","psych","quantreg","R.oo","R2wd",
                       "reporttools","reshape2","rms","RSQLite","sandwich","sqldf","stargazer","stringr",
                       "texreg","taRifx","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
#descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")

###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

start_year <- 1994
end_year <- 2011

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)


data_all0 <- read.csv(file=paste(output_directory,"data_all_tone",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                    "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                    "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                    "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                    "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")

data_all <- data_all0
# for (i in 1:length(winsorize_vars))
# {
#   #i <- 1
#   #i <- 2
#   data_all[,winsorize_vars[i]] <- winsorize_both(data_all[,winsorize_vars[i]],q=0.025)
#   
# }
# rm(i)

rm2(data_all0,winsorize_vars)


###############################################################################
cat("UNIVARIATE ANALYSIS - VARIABLES", "\n")
###############################################################################

### All pattern cols

pattern_cols_99 <- c("per_positive_percent_99","num_zero_percent_99","per_repeats_percent_99","uniform_percent_99",
                     "string_percent_99","num_pairs_percent_99","per_negative_percent_99","ar_1_percent_99","indexrsq_percent_99",
                     "kink_percent_99","quality_score_trim0_99","quality_score_trim1_99","quality_score_trim2_99")
pattern_cols_95 <- c("per_positive_percent_95","num_zero_percent_95","per_repeats_percent_95","uniform_percent_95",
                     "string_percent_95","num_pairs_percent_95","per_negative_percent_95","ar_1_percent_95","indexrsq_percent_95",
                     "kink_percent_95","quality_score_trim0_95","quality_score_trim1_95","quality_score_trim2_95")
pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
                     "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
                     "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90")

pattern_cols_trim0 <- c(pattern_cols_99,pattern_cols_95,pattern_cols_90)

pattern_cols_trim1 <- pattern_cols_trim0[!(pattern_cols_trim0 %in% c(pattern_cols_trim0[grep("per_positive",pattern_cols_trim0)],pattern_cols_trim0[grep("per_repeats",pattern_cols_trim0)]))]
#pattern_cols_trim1 <- pattern_cols_trim0[!(pattern_cols_trim0 %in% c(pattern_cols_trim0[grep("per_positive",pattern_cols_trim0)]))]

pattern_cols_trim2 <- pattern_cols_trim1[!(pattern_cols_trim1 %in% c(pattern_cols_trim1[grep("trim0",pattern_cols_trim1)],pattern_cols_trim1[grep("trim1",pattern_cols_trim1)]))]
#pattern_cols_trim2 <- pattern_cols_trim1[!(pattern_cols_trim1 %in% c(pattern_cols_trim1[grep("trim0",pattern_cols_trim1)],pattern_cols_trim1[grep("trim2",pattern_cols_trim1)]))]

pattern_cols <- pattern_cols_trim2[grep("_90", pattern_cols_trim2)] 


### Dep Vars (Text Vars)

#univariate_vars_dep <- pattern_cols[!(pattern_cols %in% pattern_cols[grep("quality_score",pattern_cols)])]
univariate_vars_dep <- pattern_cols


### Continuous Vars

univariate_vars_continuous_fund <- c("pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4","sdpct_flow_lag1",
                                     "mktadjret","mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                     "mktadjret_lag1_sq","mktadjret_lag2_sq","mktadjret_lag3_sq","mktadjret_lag4_sq",
                                     "log_aum_lag1","log_aum_lag2","log_aum_lag3","log_aum_lag4",
                                     "age_y","total_fee","sharpe_ratio","sortino_ratio")

univariate_vars_continuous_text <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                                     "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                                     "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                                     "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                                     "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")

univariate_vars_continuous_tone <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

univariate_vars_continuous <- c(univariate_vars_continuous_fund,univariate_vars_continuous_text,univariate_vars_continuous_tone)

### Binary Vars

univariate_vars_binary_fund <- c("listed_on_exchange_bin","hurdle_rate_bin","high_water_mark_bin","domicile_onshore_bin",
                                 "leverage_bin","lock_up_bin","flagship_bin","closed_bin","dead_bin")

univariate_vars_binary <- c(univariate_vars_binary_fund)

rm2(pattern_cols_99,pattern_cols_95,pattern_cols_90)
rm2(pattern_cols_trim0,pattern_cols_trim1,pattern_cols_trim2)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS", "\n")
###############################################################################

output_directory_univariate_continuous <- paste(output_directory,"univariate_inverse_continuous","\\",sep="")
create_directory(output_directory_univariate_continuous,remove=1)


data_all_univariate_continuous <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_continuous)]

univariate_continuous_quantiles <- 4

univariate_continuous_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=FALSE)
univariate_continuous_parameters[1,] <- c(output_directory_univariate_continuous,"continuous","data_all_univariate_continuous","XXX","yr_month",univariate_continuous_quantiles,"year")
univariate_continuous_parameters[2,] <- c(output_directory_univariate_continuous,"continuous","data_all_univariate_continuous","XXX","yr_month",univariate_continuous_quantiles,"agg")

univariate_continuous_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=FALSE)
univariate_continuous_year_groups[1,] <- c(start_year,end_year)


a_ply(.data=univariate_continuous_parameters,.margins=1,.fun = function(x,vars_dep,vars_indep,identifier,year_groups){
  
  # x <- univariate_continuous_parameters[1,]
  # x <- univariate_continuous_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- univariate_vars_continuous
  # identifier <- identifier
  # year_groups <- univariate_continuous_year_groups
  
  l_ply(.data=vars_dep, .fun = function(y,dep_vars_all,x,vars_indep,identifier,year_groups){
    
    # y <- vars_dep[[1]]
    # y <- vars_dep[[2]]
    # dep_vars_all <- vars_dep
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var, "\n")
    
    data <- get(x=unlist(x$data), envir = globalenv())
    group_var <- x$group_var
    
    #univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,y))]
    univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles_pct_flow  <- univariate_bins_continuous(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X1"
    quantile_last_col <- paste("X",x$nums,sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    quantiles_pct_flow_diff_overall <- univariate_bins_diff(bins=quantiles_pct_flow,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                            dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    
    # Differences by year groups and group 
    
    a_ply(.data=year_groups, .margins=1, .fun = function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles_pct_flow
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS", "\n")
      }
      bins_trim[,group_var] <- 9999
      
      quantiles_pct_flow_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                             dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var)
      
    }, group_var=group_var,bins=quantiles_pct_flow,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var, 
    .expand = TRUE, .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
    
    rm(dep_var,data,group_var,univariate_vars_indep_continuous,quantile_var,quantiles_pct_flow)
    rm(quantile_first_col,quantile_last_col,range_str,quantiles_pct_flow_diff_overall)
    
    
  }, x=x, dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier, year_groups=year_groups,
  .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
  
},vars_dep=univariate_vars_dep,vars_indep=univariate_vars_continuous,identifier=identifier,year_groups=univariate_continuous_year_groups,.progress = "none")

rm2(output_directory_univariate_continuous,univariate_continuous_quantiles,univariate_continuous_parameters)


###############################################################################
cat("UNIVARIATE ANALYSIS - BINARY", "\n")
###############################################################################

output_directory_univariate_binary <- paste(output_directory,"univariate_inverse_binary","\\",sep="")
create_directory(output_directory_univariate_binary,remove=1)


data_all_univariate_binary <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_binary)]

univariate_binary_quantiles <- 2

univariate_binary_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=FALSE)
univariate_binary_parameters[1,] <- c(output_directory_univariate_binary,"binary","data_all_univariate_binary","XXX","yr_month",univariate_binary_quantiles,"year")
univariate_binary_parameters[2,] <- c(output_directory_univariate_binary,"binary","data_all_univariate_binary","XXX","yr_month",univariate_binary_quantiles,"agg")

univariate_binary_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=FALSE)
univariate_binary_year_groups[1,] <- c(start_year,end_year)


a_ply(.data=univariate_binary_parameters,.margins=1,.fun = function(x,vars_dep,vars_indep,identifier,year_groups){
  
  # x <- univariate_binary_parameters[1,]
  # x <- univariate_binary_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- univariate_vars_binary
  # identifier <- identifier
  # year_groups <- univariate_binary_year_groups
  
  l_ply(.data=vars_dep, .fun = function(y,dep_vars_all,x,vars_indep,identifier,year_groups){
    
    # y <- vars_dep[[1]]
    # dep_vars_all <- vars_dep
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var, "\n")
    
    data <- get(x=unlist(x$data), envir = globalenv())
    group_var <- x$group_var
    
    univariate_vars_indep_binary <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles_pct_flow  <- univariate_bins_binary(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X0"
    quantile_last_col <- paste("X",(as.integer(x$nums)-1),sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    quantiles_pct_flow_diff_overall <- univariate_bins_diff(bins=quantiles_pct_flow,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                            dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var)
    
    # Differences by year groups and group 
    
    a_ply(.data=year_groups, .margins=1, .fun = function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles_pct_flow
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS", "\n")
      }
      bins_trim[,group_var] <- 9999
      
      quantiles_pct_flow_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                             dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var)
      
    }, group_var=group_var,bins=quantiles_pct_flow,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var, 
    .expand = TRUE, .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
    
    
    rm(dep_var,data,group_var,univariate_vars_indep_binary,quantile_var,quantiles_pct_flow)
    rm(quantile_first_col,quantile_last_col,range_str,quantiles_pct_flow_diff_overall)
    
  }, x=x, dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier, year_groups=year_groups,
  .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
  
},vars_dep=univariate_vars_dep,vars_indep=univariate_vars_binary,identifier=identifier,year_groups=univariate_binary_year_groups,.progress = "none")

rm2(output_directory_univariate_binary,univariate_binary_quantiles,univariate_binary_parameters)

