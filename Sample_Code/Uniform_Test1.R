
library(MASS)       # load the MASS package 

tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency tab

chisq.test(tbl) 

tbl_df <- as.data.frame(tbl,stringsAsFactors=FALSE)

colnames(tbl_df) <- c("Smoke","Exer","Freq")

#chisq.test(x=tbl_df[,"Freq"],p=rep(1/nrow(tbl_df), nrow(tbl_df)))
#p <- rep(1/length(unique(tbl_df[,"Exer"])), nrow(tbl_df))
p <- prop.table(tbl_df[,"Freq"])
chisq.test(x=tbl_df[,"Freq"],p=p)

chisq.test(tbl_df)


rm(tbl,tbl_df,survey)

#############################

M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("M","F"), party = c("Democrat","Independent", "Republican"))


m_df <- data.frame(gender_flag=NA,party_flag=NA,M,
                   stringsAsFactors=FALSE)
bob[sapply(bob, is.factor)] <- lapply(bob[sapply(bob, is.factor)], as.character)

m_df[,"gender_flag"] <- ifelse(m_df[,"gender"]=="M",1,m_df[,"gender_flag"])
m_df[,"gender_flag"] <- ifelse(m_df[,"gender"]=="F",2,m_df[,"gender_flag"])

m_df[,"party_flag"] <- ifelse(m_df[,"party"]=="Democrat",1,m_df[,"party_flag"])
m_df[,"party_flag"] <- ifelse(m_df[,"party"]=="Independent",2,m_df[,"party_flag"])
m_df[,"party_flag"] <- ifelse(m_df[,"party"]=="Republican",3,m_df[,"party_flag"])

both_flag_temp <- data.frame(both_flag=NA,expand.grid(list(unique(m_df[,"gender"]),unique(m_df[,"party"]))),stringsAsFactors=FALSE)
colnames(both_flag_temp) <- c("both_flag","gender","party")
both_flag_temp[,"both_flag"] <- seq(1,nrow(both_flag_temp))

m_df <- merge(m_df, both_flag_temp, 
              by.x=c("gender","party"), by.y=c("gender","party"), 
              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

chisq.test(M)
chisq.test(x=M)
chisq.test(x=m_df[m_df[,"gender"]=="F",c("party_flag","Freq")]) 
chisq.test(x=m_df[m_df[,"gender"]=="M",c("party_flag","Freq")]) 
chisq.test(x=m_df[,c("party_flag","Freq")]) 

sex <- "F"
p <- prop.table(m_df[m_df[,"gender"]==sex,c("Freq")])
chisq.test(x=m_df[m_df[,"gender"]==sex,c("Freq")],p=p) 

sex <- "M"
p <- prop.table(m_df[m_df[,"gender"]==sex,c("Freq")])
chisq.test(x=m_df[m_df[,"gender"]==sex,c("Freq")],p=p) 

sex <- "F"
p <- c(m_df[m_df[,"gender"]==sex,c("Freq")])



Xsq <- chisq.test(M)  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals






###################################

Y = structure(c(0, 35, 0, 0, 0, 0, 84, 84, 0, 48, 84, 0, 22, 0, 0, 
                0, 0, 0, 10, 0, 48, 0, 0, 48, 0, 22, 0, 0, 0, 0, 84, 84, 0, 48, 
                84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 48, 0, 0, 48), .Dim = c(6L, 8L 
                ), .Dimnames = list(c("1", "2", "3", "4", "5", "6"), c("V1", 
                                                                       "V2", "V3", "V4", "W1", "W2", "W3", "W4"))) 

d1 <- as.data.frame(Y,stringsAsFactors=FALSE)

temp <- apply(d1[,1:4], 1, order, decreasing=TRUE)[1:2,] 
temp <- rbind(temp, temp+4) 
result <- sapply(1:nrow(d1), function(i) 
  chisq.test(matrix(as.matrix(d1[i,temp[,i]]), ncol=2))) 



#####################################


#some data
set.seed(42)
df <- data.frame(a=rbinom(1000,5,0.3),
                 b=rbinom(1000,5,0.001),
                 c=rbinom(1000,5,0.1),
                 d=rbinom(1000,5,0.9))

#function to calculate the adj. p-value
fun <- function(x,y) {
  p.adjust(chisq.test(df[,x],df[,y])$p.value,method="holm",n=choose(ncol(df),2))
}

p.adj <- outer(names(df),names(df),FUN=Vectorize(fun)) #use outer to get a matrix
diag(p.adj) <- 1  #you should find out why chisq.test returns zero at the diag
rownames(p.adj) <- names(df)
colnames(p.adj) <- names(df)


###############################


UCBAdmissions <- datasets::UCBAdmissions     # read in the dataset
 x = ftable(UCBAdmissions)     # flatten
 x                             # what i
p.adj


################


HairEyeColor <- datasets::HairEyeColor

HairEyeColor_df <- as.data.frame(HairEyeColor)

margin.table(HairEyeColor, 2)

ddply(HairEyeColor_df,"Eye",function(x)sum(x[,"Freq"]))


###########################

survey <-  MASS::survey
str(survey)

table(survey$Smoke)

smokers <- table(survey$Smoke)

chisq.test(smokers, p=c(.1, .7, .1, .1))

smokers_df <- ddply(survey,"Smoke",function(x){data.frame(Freq=nrow(x))})
smokers_df <- smokers_df[!is.na(smokers_df[,"Smoke"]),]


chisq.test(smokers_df[,"Freq"], p=c(.1, .7, .1, .1))


#############################

data <- data.frame(Freq=NA,Actual_Prob=NA,Theo_Prob=rep.int(0.1,10))

#multiplier <-1
#multiplier <-2
multiplier <-3
multiplier <-4
multiplier <-5
multiplier <-6

data[,"Freq"] <- seq(1,10*multiplier,multiplier)
data[,"Actual_Prob"] <- data[,"Freq"]/sum(data[,"Freq"])
data[,"Actual_Prob"]
mean(data[,"Actual_Prob"])
sd(data[,"Actual_Prob"])

chisq.test(data[,"Freq"], p=data[,"Theo_Prob"])


##############################


data_dice <- data.frame(Freq=NA,Actual_Prob=NA,Theo_Prob=rep.int(1/6,6))

data_dice[,"Freq"] <- c(8,5,9,2,7,5)
data_dice[,"Actual_Prob"] <- data_dice[,"Freq"]/sum(data_dice[,"Freq"])
data_dice[,"Actual_Prob"]
mean(data_dice[,"Actual_Prob"])
sd(data_dice[,"Actual_Prob"])

chisq.test(data_dice[,"Freq"], p=data_dice[,"Theo_Prob"])
