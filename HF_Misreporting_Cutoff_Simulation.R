# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Cutoff_Simulation.R
# Version: 1.0
# Date:    09.07.2014
# Purpose: This Simulates the Data Quality (S5) cutoffs from Bollen_Pool (2012)
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
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
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
external_packages <- c("fGarch","LaplacesDemon","metRology","plyr","sn")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: RUN SIMULATION -  60 PERIOD", "\n")
###############################################################################

identifier <- "sim_type_id"

#analysis_col <- "mktadjret"
analysis_col <- "monthly_ret"

percentiles <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

simulation_type <- data.frame(sim_type_id=NA,include=NA,package=NA,command=NA,stringsAsFactors=FALSE)
simulation_type[1,] <- c(1,1,"sn","sn::rst(n=num_obs+1, xi=location, omega=scale, alpha=skewness, nu=kurtosis)")
simulation_type[2,] <- c(2,1,"metRology","metRology::rt.scaled(n=num_obs+1, mean=location, sd=scale, ncp=skewness, df=kurtosis)")
simulation_type[3,] <- c(3,1,"fGarch","fGarch::rsstd(n=num_obs+1, mean=location, sd=scale, nu=kurtosis)")
simulation_type[4,] <- c(4,1,"LaplacesDemon","LaplacesDemon::rst(n=num_obs+1, mu=location, sigma=scale, nu=kurtosis)")

simulation_inputs1 <- data.frame(sim_id=1,num_obs=60,location=0.1,scale=0.15,skewness=0,
                                 kurtosis=3,simulations=10000,rounding_digit=4,stringsAsFactors=FALSE)

simulation_inputs2 <- data.frame(sim_id=2,num_obs=120,location=0.05,scale=0.1,skewness=0,
                                 kurtosis=3,simulations=10000,rounding_digit=1,stringsAsFactors=FALSE)

simulation_inputs <- rbind(simulation_inputs1,simulation_inputs2)

rm(simulation_inputs1,simulation_inputs2)

sim_out <- adply(.data=simulation_inputs, .margins=1, .fun = function(x,sim_type,identifier,analysis_col,percentiles,output_directory){
  
  # x <- simulation_inputs[1,]
  # x <- simulation_inputs[2,]
  
  # sim_type <- simulation_type
  # identifier <- identifier
  # analysis_col <- analysis_col
  # percentiles <- percentiles
  # output_directory <- output_directory
  
  cat("Simulation:",unique(x[,"sim_id"]), "\n")
  
  num_obs <- x[,"num_obs"]
  location <- x[,"location"]
  scale <- x[,"scale"]
  skewness <- x[,"skewness"]
  kurtosis <- x[,"kurtosis"]
  simulations <- x[,"simulations"]
  rounding_digit <- x[,"rounding_digit"]
  
  simulation_commands <- sapply(data.frame(overall_id=NA,sim_type[sim_type[,"include"]==1,],stringsAsFactors=FALSE), rep.int,
                                times=ceiling(simulations/nrow(data.frame(overall_id=NA,sim_type[sim_type[,"include"]==1,],stringsAsFactors=FALSE))))
  simulation_commands <- as.data.frame(simulation_commands,stringsAsFactors=FALSE)
  simulation_commands[,"overall_id"] <- seq(1,nrow(simulation_commands))
  simulation_commands <- simulation_commands[1:simulations,]
  
  output_name <- paste("Cutoff_Simulation","_",num_obs,".csv",sep="")
  
  
  ###############################################################################
  cat("S5 - CREATE DATA", "\n")
  ###############################################################################
  
  data_s5 <- adply(.data=simulation_commands, .margins=1, .fun = function(x,command_col){
    
    #  x <- simulation_commands[1,]
    #  command_col <- "command"
    
    #overall_id <- unique(x[,"overall_id"])
    #cat(overall_id, "\n")
    
    ret_temp <- data.frame(ret=as.numeric(eval(parse(text = paste0(x[,command_col])))),
                           ret_lag=NA,stringsAsFactors=FALSE)
    ret_temp[,"ret_lag"] <- c(NA,ret_temp[1:(nrow(ret_temp)-1),"ret"])     
    ret_temp <- ret_temp[2:nrow(ret_temp),]
    #row.names(ret_temp) <- seq(nrow(ret_temp))
    
    return(ret_temp)
    
  },command_col="command", .expand = FALSE,.progress = "text", .inform = FALSE)
  
  colnames(data_s5)[match("X1",names(data_s5))] <- identifier
  colnames(data_s5)[match("ret",names(data_s5))] <- analysis_col
  colnames(data_s5)[match("ret_lag",names(data_s5))] <- paste(analysis_col,"lag1",sep="_")
  
  data_trim_id_full_cols <- identifier
  
  ### Round
  data_s5[,analysis_col] <- round(data_s5[,analysis_col],digits=rounding_digit)
  data_s5[,paste(analysis_col,"lag1",sep="_")] <- round(data_s5[,paste(analysis_col,"lag1",sep="_")],digits=rounding_digit)
  
  rm(num_obs,location,scale,skewness,kurtosis,simulations,rounding_digit)
  
  
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
  
  data_s5_1_screen <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
    
    # x <- data_s5[data_s5[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
    # analysis_col <- analysis_col
    # id_col <- identifier
    
    zero_and_neg_screen <- zero_neg_screen_execute(data=x,ret_col=analysis_col, prob_type="normal")
    
    return(zero_and_neg_screen)
    
  }, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)
  
  
  data_s5_1_cutoffs <- data.frame(sim_type_id=data_s5_1_screen[,identifier],
                                  num_zero=data_s5_1_screen[,"sum_zero"],
                                  per_neg1=data_s5_1_screen[,"sum_neg"]/data_s5_1_screen[,"sum_total"],
                                  per_neg2=data_s5_1_screen[,"prob_ind_neg"],
                                  per_neg3=data_s5_1_screen[,"prob_cum_neg"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_1_screen)
  
  
  ###############################################################################
  cat("S5 - (2) PER_REPEATS", "\n")
  ###############################################################################
  
  per_repeat_screen_execute <- function(data,ret_col){
    
    # data <- x
    # ret_col <- analysis_col
    
    repeat_num_data_count <- per_repeat_screen_counts(ret=data[,ret_col],ret_col=ret_col)
    
    repeat_num_data_sum <- per_repeat_screen_sum(data=repeat_num_data_count,data_col="Freq")
    
    rm(repeat_num_data_count)
    
    return(repeat_num_data_sum)
    
  }
  
  data_s5_2_screen <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
    
    # x <- data_s5[data_s5[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
    # analysis_col <- analysis_col
    # id_col <- identifier
    
    per_repeat_screen <- per_repeat_screen_execute(data=x,ret_col=analysis_col)
    
    return(per_repeat_screen)
    
  }, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)
  
  data_s5_2_cutoffs <- data.frame(sim_type_id=data_s5_2_screen[,identifier],
                                  per_repeat=1-data_s5_2_screen[,"Prop_u"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_2_screen)
  
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
  
  data_s5_3_screen <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
    
    # x <- data_s5[data_s5[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
    # analysis_col <- analysis_col
    # id_col <- identifier
    
    string_screen <- string_screen_execute(data=x,ret_col=analysis_col)
    
    return(string_screen)
    
  }, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)
  
  data_s5_3_cutoffs <- data.frame(sim_type_id=data_s5_3_screen[,identifier],
                                  string=data_s5_3_screen[,"Max_Length"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_3_screen)
  
  
  ###############################################################################
  cat("S5 - (4) NUM_PAIRS", "\n")
  ###############################################################################
  
  num_pairs_screen_execute <- function(data,ret_col,lag_ret_col){
    
    # data <- x
    # ret_col <- analysis_col
    # lag_ret_col <- paste(analysis_col,"lag1",sep="_")
    
    num_pair_data <- num_pairs_screen_pairs(data=data,ret_col=ret_col,lag_ret_col=lag_ret_col)
    
    num_pairs_counts <- num_pairs_screen_counts(pairs=num_pair_data,pair_col="Pair")
    
    num_pairs_screen_temp <- data.frame(Max_Pairs=NA,Total=NA,Prop_u=NA, stringsAsFactors=FALSE)
    
    num_pairs_screen_temp[,"Max_Pairs"] <- tail(num_pairs_counts[,"freq"],1)
    num_pairs_screen_temp[,"Total"] <- nrow(data)
    num_pairs_screen_temp[,"Prop_u"] <- num_pairs_screen_temp[,"Max_Pairs"]/num_pairs_screen_temp[,"Total"] 
    
    return(num_pairs_screen_temp)
    
  }
  
  data_s5_4_screen <- ddply(.data=data_s5, .variables=identifier, .fun = function(x,analysis_col,id_col){
    
    # x <- data_s5[data_s5[,identifier]==0,]
    # analysis_col <- analysis_col
    # id_col <- identifier
    
    num_pairs_screen <- num_pairs_screen_execute(data=x,ret_col=analysis_col,lag_ret_col=paste(analysis_col,"lag1",sep="_"))
    
    return(num_pairs_screen)
    
  }, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)
  
  data_s5_4_cutoffs <- data.frame(sim_type_id=data_s5_4_screen[,identifier],
                                  num_pairs=data_s5_4_screen[,"Max_Pairs"]-1,
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_4_screen)
  
  
  ###############################################################################
  cat("S5 - (5) UNIFORM", "\n")
  ###############################################################################
  
  uniform_screen_execute <- function(data,ret_col,graph){
    
    # data <- x
    # ret_col <- analysis_col
    # graph <- FALSE
    
    screen_uniform <- data.frame(data,Ret_Digits=NA,stringsAsFactors=FALSE)
    
    #screen_uniform[,"Ret_Digits"] <- uniform_screen_LHS_digits(screen_uniform[,analysis_col],1)
    screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,analysis_col],1,4)
    #screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,"Monthly_Ret_Percent"],1,2)
    
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
  
  
  data_s5_5_screen <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
    
    # x <- data_s5[data_s5[,identifier]==0,c(data_trim_id_full_cols,analysis_col)]
    # x <- uniform_data[uniform_data[,identifier]==0,]
    # analysis_col <- analysis_col
    # id_col <- identifier
    
    uniform_out <- uniform_screen_execute(data=x,ret_col=analysis_col,graph=FALSE)
    
    return(uniform_out)
    
  }, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)
  
  data_s5_5_cutoffs <- data.frame(sim_type_id=data_s5_5_screen[,identifier],
                                  uniform=data_s5_5_screen[,"x_squared_uniform"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_5_screen)
  
  ###############################################################################
  cat("S5 - CREATE PERCENTILES", "\n")
  ###############################################################################
  
  data_s5_cutoff_percentiles <- data.frame(Flag=NA,matrix(NA, ncol=length(percentiles), nrow=8, dimnames=list(c(), paste("per",percentiles,sep="_"))), 
                                           stringsAsFactors=FALSE)
  
  data_s5_cutoff_percentiles[1,] <- c("num_zero",t(quantile(data_s5_1_cutoffs[,"num_zero"],percentiles)))
  data_s5_cutoff_percentiles[2,] <- c("per_repeat",t(quantile(data_s5_2_cutoffs[,"per_repeat"],percentiles)))
  data_s5_cutoff_percentiles[3,] <- c("uniform",t(quantile(data_s5_5_cutoffs[,"uniform"],percentiles)))
  data_s5_cutoff_percentiles[4,] <- c("string",t(quantile(data_s5_3_cutoffs[,"string"],percentiles)))
  data_s5_cutoff_percentiles[5,] <- c("num_pairs",t(quantile(data_s5_4_cutoffs[,"num_pairs"],percentiles)))
  data_s5_cutoff_percentiles[6,] <- c("per_neg",t(quantile(data_s5_1_cutoffs[,"per_neg1"],percentiles)))
  data_s5_cutoff_percentiles[7,] <- c("per_neg",t(quantile(data_s5_1_cutoffs[,"per_neg2"],percentiles)))
  data_s5_cutoff_percentiles[8,] <- c("per_neg",t(quantile(data_s5_1_cutoffs[,"per_neg3"],percentiles)))
  
  ###############################################################################
  #cat("OUTPUT FLAGS", "\n")
  ###############################################################################
  
  write.csv(data_s5_cutoff_percentiles,file=paste(output_directory,output_name,sep="\\"),na="",quote=TRUE,row.names=FALSE)
  
  rm(output_name)
  
  return(x)
  
}, sim_type=simulation_type,identifier=identifier,analysis_col=analysis_col,percentiles=percentiles,output_directory=output_directory,
.expand = TRUE, .progress = "none", .inform = FALSE, .parallel = FALSE,.paropts = NULL)


