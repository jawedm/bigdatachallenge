library(doBy)
library(ggplot2)

#IMPORTING CSV FILES AND ASSIGNING VARIABLE
val <- read.csv("VALDATA.csv", header=TRUE)
tran <- read.csv("TRANDATA.csv", header=TRUE)
target <- read.csv("TARGET.csv", header=TRUE)
stores <- read.csv("STORES.csv", header=TRUE)
customers <- read.csv("CUSTOMERS.csv", header=TRUE)

all.tran <- merge(val,tran,all=TRUE)
tran <-all.tran

# 2012 Analysis
tran2012<-tran[tran$YEAR==2012,]
store.sales.2012 <- summaryBy(SALES~YEAR+STORE_ID+WEEK, data=tran2012, FUN=c(sum))

store.sales.2012$STORE <- factor(store.sales.2012$STORE_ID)

p <- ggplot(store.sales.2012, aes(x = WEEK, y = SALES.sum, color = STORE))
p + geom_line() + ylab("SALES") + ggtitle("2012 Sales")

#2013 Analysis
tran2013<-tran[tran$YEAR==2013,]
store.sales.2013 <- summaryBy(SALES~YEAR+STORE_ID+WEEK, data=tran2013, FUN=c(sum))

store.sales.2013$STORE <- factor(store.sales.2013$STORE_ID)

p <- ggplot(store.sales.2013, aes(x = WEEK, y = SALES.sum, color = STORE))
p + geom_line() + ylab("SALES") + ggtitle("2013 Sales")


#STORE 20001319 SUM BETWEEN WEEK 20 AND 30 IN 2013
store1Data2012 <- sum(val[val$STORE_ID == 20001319 & val$YEAR == 2012 & val$WEEK >= 20 & val$WEEK <= 30, 10])
store1Data2012

#DETERMING WHICH CUSTOMERS SHOPPED AT STORE 20001319 BETWEEN WEEK 20 AND 30 IN 2013
customersStore1 <- (val[val$STORE_ID == 20001319 & val$YEAR == 2012 & val$WEEK >= 20 & val$WEEK <= 30, 3])
customersStore1

tran_store1 <- tran[tran$STORE_ID=='20001319' & tran$YEAR==2011,]
sum.store1 <- summaryBy(SALES~YEAR+STORE_ID+WEEK+CAT_DESC, data=tran_store1, FUN=c(sum))

qplot(factor(WEEK), SALES.sum, data = sum.store1, geom=c("boxplot", "jitter"))
d <- ggplot(sum.store1, aes(WEEK, SALES.sum)) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ CAT_DESC)



#LOOKING AT DATA FROM STORE 20001319 IN 2012 ONLY
combined <- merge(customers, tran, by.y="CUSTOMER_ID")
store1v1  <- combined[combined$STORE_ID == 20001321 & combined$YEAR == 2012, ]  
store1v1$POSTCODE <- substr(store1v1$POSTCODE, 1, 3)
sum.store1v1 <- summaryBy(SALES~YEAR+POSTCODE, data=store1v1, FUN=c(sum))

ggplot(sum.store1v1, aes(POSTCODE, SALES.sum)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 
