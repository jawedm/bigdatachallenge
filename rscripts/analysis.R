
library(doBy)

#IMPORTING CSV FILES AND ASSIGNING VARIABLE
val <- read.csv("VALDATA.csv", header=TRUE)
tran <- read.csv("TRANDATA.csv", header=TRUE)
target <- read.csv("TARGET.csv", header=TRUE)
stores <- read.csv("STORES.csv", header=TRUE)
customers <- read.csv("CUSTOMERS.csv", header=TRUE)

#STORES TOTAL SUM
sum(val$SALES[val[,"STORE_ID"] == 20001319])
sum(val$SALES[val[,"STORE_ID"] == 20001321])
sum(val$SALES[val[,"STORE_ID"] == 20001323])
sum(val$SALES[val[,"STORE_ID"] == 20004029])
sum(val$SALES[val[,"STORE_ID"] == 20004053])

#STORES 2012 SUM
sum(val[ val$STORE_ID == 20001319 & val$YEAR == 2012, 10])
sum(val[ val$STORE_ID == 20001321 & val$YEAR == 2012, 10])
sum(val[ val$STORE_ID == 20001323 & val$YEAR == 2012, 10])
sum(val[ val$STORE_ID == 20004029 & val$YEAR == 2012, 10])
sum(val[ val$STORE_ID == 20004053 & val$YEAR == 2012, 10])

#STORE 20001319 SUM BETWEEN WEEK 20 AND 30 IN 2013
store1Data2012 <- sum(val[val$STORE_ID == 20001319 & val$YEAR == 2012 & val$WEEK >= 20 & val$WEEK <= 30, 10])
store1Data2012

#DETERMING WHICH CUSTOMERS SHOPPED AT STORE 20001319 BETWEEN WEEK 20 AND 30 IN 2013
customersStore1 <- (val[val$STORE_ID == 20001319 & val$YEAR == 2012 & val$WEEK >= 20 & val$WEEK <= 30, 3])
customersStore1

tran_store1 <- tran[tran$STORE_ID=='20001319' & tran$YEAR==2011,]
sum.store1 <- summaryBy(SALES~YEAR+STORE_ID+WEEK+CAT_DESC, data=tran_store1, FUN=c(sum))
library(ggplot2)
qplot(factor(WEEK), SALES.sum, data = sum.store1, geom=c("boxplot", "jitter"))
d <- ggplot(sum.store1, aes(WEEK, SALES.sum)) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ CAT_DESC)

