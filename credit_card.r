rm(list = ls())
setwd("D:/DATA SCIENCE")
#loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart")
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)
#Load File
#Loading Data
original_data = read.csv('card.csv',stringsAsFactors = FALSE)
df = original_data 
View(df)

# Analysis of Outliers for the Dataset :
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+2*s
  LC <- m-2*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}


#New Variables creation# 

df$Monthly_Avg_PURCHASES <- df$PURCHASES/(df$PURCHASES_FREQUENCY*df$TENURE)
df$Monthly_CASH_ADVANCE <- df$CASH_ADVANCE/(df$CASH_ADVANCE_FREQUENCY*df$TENURE)
df$LIMIT_USAGE <- df$BALANCE/df$CREDIT_LIMIT
df$MIN_PAYMENTS_RATIO <- df$PAYMENTS/df$MINIMUM_PAYMENTS

write.csv(df,"New_variables_creation.csv")

# Getting variables of Dataset :-
Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

df<-t(data.frame(apply(df[Num_Vars], 2, mystats)))
View(df)

write.csv(df,"New_variables_creation.csv")

Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "Monthly_Avg_PURCHASES",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "Monthly_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

df<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(df)

# Outlier Treatment 

df$BALANCE[df$BALANCE>5727.53]<-5727.53
df$BALANCE_FREQUENCY[df$BALANCE_FREQUENCY>1.3510787]<-1.3510787
df$PURCHASES[df$PURCHASES>5276.46]<-5276.46
df$Monthly_Avg_PURCHASES[df$Monthly_Avg_PURCHASES>800.03] <- 800.03
df$ONEOFF_PURCHASES[df$ONEOFF_PURCHASES>3912.2173709]<-3912.2173709
df$INSTALLMENTS_PURCHASES[df$INSTALLMENTS_PURCHASES>2219.7438751]<-2219.7438751
df$CASH_ADVANCE[df$CASH_ADVANCE>5173.1911125]<-5173.1911125
df$Monthly_CASH_ADVANCE[df$Monthly_CASH_ADVANCE>2558.53] <- 2558.53
df$PURCHASES_FREQUENCY[df$PURCHASES_FREQUENCY>1.2930919]<-1.2930919
df$ONEOFF_PURCHASES_FREQUENCY[df$ONEOFF_PURCHASES_FREQUENCY>0.7991299]<-0.7991299
df$PURCHASES_INSTALLMENTS_FREQUENCY[df$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329]<-1.1593329
df$CASH_ADVANCE_FREQUENCY[df$CASH_ADVANCE_FREQUENCY>0.535387]<-0.535387
df$CASH_ADVANCE_TRX[df$CASH_ADVANCE_TRX>16.8981202]<-16.8981202
df$PURCHASES_TRX[df$PURCHASES_TRX>64.4251306]<-64.4251306
df$CREDIT_LIMIT[df$CREDIT_LIMIT>11772.09]<-11772.09
df$LIMIT_USAGE[df$LIMIT_USAGE>1.1683] <- 1.1683
df$PAYMENTS[df$PAYMENTS>7523.26]<-7523.26
df$MINIMUM_PAYMENTS[df$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423
df$MIN_PAYMENTS_RATIO[df$MIN_PAYMENTS_RATIO>249.9239] <- 249.9239
df$PRC_FULL_PAYMENT[df$PRC_FULL_PAYMENT>0.738713]<-0.738713
df$TENURE[df$TENURE>14.19398]<-14.19398


# Missing Vlaue Analysis : 

# Imputation with Mean :-
df$MINIMUM_PAYMENTS[which(is.na(df$MINIMUM_PAYMENTS))] <- 721.9256368
df$CREDIT_LIMIT[which(is.na(df$CREDIT_LIMIT))] <- 4343.62
df$Monthly_Avg_PURCHASES[which(is.na(df$Monthly_Avg_PURCHASES))] <-184.8991609
df$Monthly_CASH_ADVANCE[which(is.na(df$Monthly_CASH_ADVANCE))] <- 717.7235629
df$LIMIT_USAGE[which(is.na(df$LIMIT_USAGE))] <-0.3889264
df$MIN_PAYMENTS_RATIO[which(is.na(df$MIN_PAYMENTS_RATIO))]  <- 9.3500701

# checking missing value :- 
check_Missing_Values<-t(data.frame(apply(credit[Num_Vars], 2, mystats)))
View(check_Missing_Values)

write.csv(df,"Missing_value_treatment.csv")


# Variable Reduction (Factor Analysis)

Step_nums <- credit[Num_Vars]
corrm<- cor(Step_nums)    
View(corrm)

write.csv(corrm, "Correlation_matrix.csv")

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)

eigen(corrm)$values
require(dplyr)

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))


write.csv(eigen_values, "EigenValues2.csv")

require(psych)
FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")
#SORTING THE LOADINGS
FA_SORT<-fa.sort(FA)      
FA_SORT$loadings

Loadings<-data.frame(FA_SORT$loadings[1:ncol(Step_nums),])
write.csv(Loadings, "loadings2.csv")

# standardizing the data
segment_prepared <-credit[Num_Vars]

segment_prepared = scale(segment_prepared)

write.csv(segment_prepared, "standardized data.csv")# standardizing the data
segment_prepared <-credit[Num_Vars]

segment_prepared = scale(segment_prepared)

write.csv(segment_prepared, "standardized data.csv")

#building clusters using k-means clustering 
cluster_three <- kmeans(segment_prepared,3)
cluster_four <- kmeans(segment_prepared,4)
cluster_five <- kmeans(segment_prepared,5)
cluster_six <- kmeans(segment_prepared,6)

credit_new<-cbind(credit,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(credit_new)


# Profiling
Num_Vars2 <- c(
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

require(tables)
tt <-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                     factor(km_clust_6)~Heading()*length*All(credit[1]),
                   data=credit_new),tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                                              factor(km_clust_6)~Heading()*mean*All(credit[Num_Vars2]),
                                            data=credit_new))

tt2 <- as.data.frame.matrix(tt)
View(tt2)
rownames(tt2)<-c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")


colnames(tt2)<-c(
  "SEGMENT_SIZE",
  "Monthly_Avg_PURCHASES",
  "Monthly_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MIN_PAYMENTS_RATIO",
  "BALANCE",
  "TENURE"
)

cluster_profiling2 <- t(tt2)

write.csv(cluster_profiling2,'cluster_profiling2.csv')
