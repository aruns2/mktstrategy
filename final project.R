## ----global_options, include=FALSE, fig.width = 10----------------------------
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)


## ---- warning = FALSE, message=FALSE------------------------------------------
# Import libraries
library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(cluster)
library(factoextra)
library('dplyr')
library("readxl")
library(knitr)
library(leaps)
library(boot)


## -----------------------------------------------------------------------------
ltv.data <- read.csv('ltv Dataset.csv')
head(ltv.data)


## ----warning = FALSE, message=FALSE-------------------------------------------
ltv.data <- ltv.data %>% mutate_at(vars(id, status, gender, entered, completed, holiday), funs(as.factor(.)))
ltv.data$date <- as.Date(ltv.data$date, '%m/%d/%Y')
summary(ltv.data)


## -----------------------------------------------------------------------------
sapply(ltv.data, function(x) sum(is.na(x)))


## -----------------------------------------------------------------------------
hist(ltv.data$pages)
hist(ltv.data$onsite)


## -----------------------------------------------------------------------------
# Quantiles
quantile(ltv.data$onsite, c(0,.1,.25,.5,.75,.9,.95,.99,1))


## -----------------------------------------------------------------------------
ltv.data[ltv.data$onsite>60,"onsite"] <- 60
summary(ltv.data)


## -----------------------------------------------------------------------------
# Number of users
print(paste0("Number of users: " , n_distinct(ltv.data$id)))

# Number of users by gender
print("Users by gender:")
print.data.frame(ltv.data %>% group_by(gender) %>% summarise(count = n_distinct(id)))



## -----------------------------------------------------------------------------
# Attrition
att.summ <- ltv.data %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% group_by(year, status) %>% summarise(count = n_distinct(id))

att.summ$status <- mapvalues(att.summ$status, 
          from=c(0,1,2), 
          to=c("new","open","cancelled"))

ggplot(att.summ, aes(x = year, y = count,fill=status)) +
    geom_bar(stat='identity')


## ----setup, include=FALSE-----------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(tidyverse)
library(plyr)
library(dplyr)
library(reshape2)
library(GGally)
library(caret)




## ---- include= FALSE----------------------------------------------------------
# reading data
ltv.data <- read.csv("ltv Dataset.csv")

#converting to date format
ltv.data$date <- as.Date.factor(ltv.data$date, "%m/%d/%Y")



## ---- echo= FALSE-------------------------------------------------------------
#identifying customers who churned is.churn = 1

ltv.data$is.churn <- 0

customer.churn<- unique(ltv.data[ltv.data$id %in% c(ltv.data[ltv.data$status == 2,]$id),]$id)
ltv.data[ltv.data$id %in% customer.churn,]$is.churn <- 1

#customers who did not churn
customer.notchurn <- unique(ltv.data[!ltv.data$id %in% c(ltv.data[ltv.data$status == 2,]$id),]$id)

#calculate intertime visit
ltv.data <- ddply(ltv.data, "id", transform, inter.time = c(0, diff(date)))

#unique(ltv.data[,c(1,10)])
add.days <- ltv.data[ltv.data$id %in% customer.notchurn,] %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(diff= as.Date("31/12/2014", "%d/%m/%Y")- max(date))

#adding frequency

max.date <- ltv.data %>% 
  dplyr::group_by(id) %>% 
  dplyr:: summarise(max.date = max(date))

ltv.data <- merge(ltv.data, max.date, by = "id")
ltv.data$len.final <- ltv.data$max.date - ltv.data$date
ltv.data$last.seen <- as.Date("31/12/2014", "%d/%m/%Y")-ltv.data$max.date

summary.data <- ltv.data %>%
  dplyr::group_by(id)%>%
  dplyr::summarise(total.pg = sum(pages), avg.pg = mean(pages),
                   total.time = sum(onsite), avg.time = mean(onsite),
                   total.entered = sum(entered), avg.entered = mean(entered),
                   total.completed = sum(completed), avg.completed = mean(completed),
                   avg.intertime = mean(inter.time),
                   is.churn = mean(is.churn), count.visit = n(), 
                   transact.days = (max(date)- min(date)),
                   last.seen = mean(last.seen))



# merging the total transaction duration for customers


final.data <- merge(summary.data, add.days, by = "id", all.x = TRUE)
final.data[is.na(final.data$diff),]$diff <- 0
final.data$tenure <- final.data$transact.days +final.data$diff

#removing columns transact days and diff- dont need them for analysis
final.data <- final.data[,-c(12,13)]


#adding gender to final data
final.data <- merge(final.data, unique(ltv.data[,c(1,3)]), by = "id")


## ---- fig.width =10, fig.height=10,cache=TRUE,echo= FALSE---------------------

#checking for coorelation
library("PerformanceAnalytics")
mydata = final.data[, c(3,5,7,9,10,12,14)]
mydata$tenure <- as.numeric(mydata$tenure)
mydata$last.seen <- as.numeric(mydata$last.seen)
chart.Correlation(mydata, histogram = TRUE, pch =18)


# removing correlated variable from the data
final.data1 <- final.data[, -c(2,4,6,8)]



## ---- echo= FALSE-------------------------------------------------------------
library(tree)
final.data1 <- final.data1[,-9]
final.data1$is.churn <-  recode_factor(final.data1$is.churn, `0` = "No", `1` ="Yes")
train <- sample(1:nrow(final.data1), 7000)
attrition.test <- final.data1[-train,]
attrition.test.actual <- final.data1[-train,]$is.churn
tree.attrition <- tree(is.churn~. -id,final.data1,subset=train)

tree.pred <- predict(tree.attrition,attrition.test,type="class")

#confusion Matrix

cm <- confusionMatrix(tree.pred, attrition.test.actual )

cm


## ---- echo= FALSE-------------------------------------------------------------

final.data$is.churn <-  recode_factor(final.data$is.churn, `0` = "No", `1` ="Yes")
train <- sample(1:nrow(final.data), 7000)
attrition.test <- final.data[-train,]
attrition.test.actual <- final.data[-train,]$is.churn
glm.attrition <- glm(is.churn~.-id,final.data,subset=train, family = "binomial")

summary(glm.attrition)

glm.pred <- predict(glm.attrition,attrition.test)
table(tree.pred,attrition.test.actual)

#confusion matrix
cm <- confusionMatrix(tree.pred, attrition.test.actual )

cm


## ----fig.width=10, fig.height=10, echo= FALSE---------------------------------

library(randomForest)

train <- sample(1:nrow(final.data1), 7000)
attrition.test <- final.data1[-train,]
attrition.test.actual <- final.data1[-train,]$is.churn

rf.attrition <- randomForest(is.churn ~ . -id,data=final.data1[train,],mtry = 4, importance=TRUE)
plot(rf.attrition)
rf.pred <- predict(rf.attrition,attrition.test)
table(rf.pred,attrition.test.actual)

#building a confusion matrix
cm <- confusionMatrix(rf.pred, attrition.test.actual )

cm



## ---- echo= FALSE-------------------------------------------------------------
kable(summary(importance(rf.attrition)))
varImpPlot(rf.attrition)


## ---- echo = FALSE------------------------------------------------------------

data <- read_excel("ltv.xlsx")

max_date <- as.Date("2014-12-31")

my_data_two <- data %>% group_by(id) %>% mutate(counter = row_number(id))
my_data_two <- my_data_two %>% group_by(id) %>% mutate(male = ifelse(gender == "M",1,0))
my_data_two <- my_data_two %>% group_by(id) %>% mutate(subscribed = as.Date(date[status == 0]))



my_data_two <- my_data_two %>% group_by(id) %>% mutate(ended = if_else(mean(status) == 1, as.Date(max(date)), (max_date)))

my_data_two_summary <- 
  my_data_two %>% 
  group_by(id, male, subscribed , ended) %>% 
  summarize(num_activities = n()-2 ,
            
             
            avg_time_between_activity = round(as.numeric(sum(diff(date)))/ n(),2),
            avg_onsite = round(mean(onsite[status == 1]),2),
            avg_pages = round(sum(pages)/ (n()),2),
            percent_completed = as.numeric(round(sum(completed)/ (n()),2)*100),
            num_enter_not_complete = sum(entered)-sum(completed),
            ratio_completed_holiday =  (sum(holiday)/sum(completed))
            
            )

my_data_two_summary <- my_data_two_summary  %>% 
  mutate(ltv = as.numeric(ceiling(round(as.numeric((ended) - subscribed),0)/30)))

my_data_summary <- as.data.frame(my_data_two_summary)
my_data_summary[is.na(my_data_summary)] <- 0
my_data_summary = subset(my_data_summary, select = -c(subscribed,ended,id) )



## ---- echo= FALSE-------------------------------------------------------------

ltv.reg.model <- glm(ltv ~ . , data = my_data_summary)

a <- summary(ltv.reg.model)$coeff[-1,4] < 0.05
significant <- names(a)[a == TRUE] 
paste(significant)


## ---- echo= FALSE-------------------------------------------------------------
data.subset <- regsubsets(ltv ~ .,
               data = my_data_summary,
               nbest = 1,    # 1 best model for each number of predictors
               nvmax = NULL,    # NULL for no limit on number of variables
               method = "forward", really.big = TRUE)


x <- c(0)
rsq <- c()
bic <- c()
aic <- c()
for (i in 1:9) {
  x[i] <- i
  rsq[i] <- summary(data.subset)$rsq[i]
  bic[i] <- summary(data.subset)$bic[i]
  aic[i] <- summary(data.subset)$cp[i]
  
}



# validation set function, code partially adopted from assignment 3 


validation_set <- function(x) {



vse <- c()

for (i in 1:50) {
  

# Form a random split
rand.split <- sample(cut(1:nrow(x), breaks = 2, labels = FALSE))
# Fit model on first part
ltv.glm.train <- glm(ltv ~ ., data = x[rand.split == 1,])
# Predict on the second part
ltv.glm.pred <- predict(ltv.glm.train, newdata = x[rand.split == 2, ])
# Calculate MSE on the second part
vse[i] <- mean((x$ltv[rand.split == 2] - ltv.glm.pred)^2)

}

avg.vse <- mean(vse)
std.vse <- sd(vse)
coef.var.vse <- std.vse / avg.vse

return(avg.vse)
}


vs <- c()
num_predictors <- c(2:8)

for (i in 2:8) {
  
 
m <- my_data_summary %>% select("ltv" ,(names(coef(data.subset, id = i )))[2:i+1])

vs[i-1] <- validation_set(m)
}



## ---- echo = FALSE------------------------------------------------------------

par(mfrow=c(2,2))
plot(x, rsq, 
     xlab = "number of predictors", ylab = "R-Squared")
plot(x, aic, 
     xlab = "number of predictors", ylab = "AIC")
plot(x, bic, 
     xlab = "number of predictors", ylab = "BIC")

plot(num_predictors,vs , xlab = "Number of predictors" , ylab = "Validation set MSE")



## -----------------------------------------------------------------------------
ltv.final.model <- glm(ltv ~ male + num_activities + avg_pages + ratio_completed_holiday, data = my_data_summary)


kable(coef(ltv.final.model))



## ---- warning = FALSE, message=FALSE------------------------------------------
# Import libraries
library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(cluster)
library(factoextra)


## ---- include = FALSE---------------------------------------------------------
ltv.data <- read.csv('ltv Dataset.csv')
head(ltv.data)


## ----warning = FALSE, message=FALSE-------------------------------------------
ltv.data <- ltv.data %>% mutate_at(vars(id, status, gender, entered, completed, holiday), funs(as.factor(.)))
ltv.data$date <- as.Date(ltv.data$date, '%m/%d/%Y')
summary(ltv.data)


## ---- include = FALSE---------------------------------------------------------
sapply(ltv.data, function(x) sum(is.na(x)))


## ----include = FALSE----------------------------------------------------------
hist(ltv.data$pages)
hist(ltv.data$onsite)


## ---- include = FALSE---------------------------------------------------------
# Quantiles
quantile(ltv.data$onsite, c(0,.1,.25,.5,.75,.9,.95,.99,1))


## ----include = FALSE----------------------------------------------------------
ltv.data[ltv.data$onsite>60,"onsite"] <- 60
summary(ltv.data)


## ----include = FALSE----------------------------------------------------------
# Number of users
print(paste0("Number of users: " , n_distinct(ltv.data$id)))

# Number of users by gender
print("Users by gender:")
print.data.frame(ltv.data %>% group_by(gender) %>% summarise(count = n_distinct(id)))



## ---- include = FALSE---------------------------------------------------------
# Attrition
att.summ <- ltv.data %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% group_by(year, status) %>% summarise(count = n_distinct(id))

att.summ$status <- mapvalues(att.summ$status, 
          from=c(0,1,2), 
          to=c("new","open","cancelled"))

ggplot(att.summ, aes(x = year, y = count,fill=status)) +
    geom_bar(stat='identity')


## ---- echo = FALSE------------------------------------------------------------
# Customer tenure
tenure <- dcast(ltv.data[ltv.data$status != 1,], id ~ status, value.var = 'date')
tenure[,2] <- as.Date(tenure[,2],origin = "1970-01-01")
tenure[,3] <- as.Date(tenure[,3], origin = "1970-01-01")
tenure$tenure <- as.integer(tenure[,3] - tenure[,2]) 
hist(tenure$tenure)
tenure$is.attr <- "yes"
tenure[is.na(tenure$tenure),]$is.attr <- "no"
tenure[is.na(tenure$tenure),]$tenure <- as.Date("2014-12-31") - tenure[is.na(tenure$tenure),2]
tenure[is.na(tenure$tenure),]$tenure <- tenure[is.na(tenure$tenure),3] - as.Date("2011-01-01")
tenure %>%
  ggplot(aes(x = tenure, fill = is.attr))+
  geom_histogram(position = "stack")

summary(tenure$tenure)

tapply(tenure$tenure, tenure$is.attr, summary)



## ----echo = FALSE-------------------------------------------------------------
# List of cancelled customers
attr.list <- ltv.data %>% group_by(id) %>% summarise(final.status = max(as.integer(as.character(status))))
attr.list <- attr.list$id[attr.list$final.status == 2]


## ---- echo = FALSE------------------------------------------------------------
# Last seen

last.seen <- ltv.data %>% group_by(id) %>% summarise(diff= as.integer(as.Date("31/12/2014", "%d/%m/%Y")- max(date)))

hist(last.seen$diff)


## ---- echo = FALSE------------------------------------------------------------
# Interarrival time
ltv.data.copy <- ltv.data[order(ltv.data$id, ltv.data$date),]
ltv.data.copy <- ltv.data.copy[!(ltv.data.copy$id %in% attr.list),]
ltv.data.copy$id.lag <- lag(ltv.data.copy$id,1)
ltv.data.copy$date.lag <- lag(ltv.data.copy$date,1)
ltv.data.copy$time.since.last.event <- ltv.data.copy$date - ltv.data.copy$date.lag
ltv.data.copy$time.since.last.event[ltv.data.copy$id != ltv.data.copy$id.lag] <- NA

frequency <- ltv.data.copy %>% group_by(id) %>% summarise(last.seen.days= as.integer(as.Date("31/12/2014", "%d/%m/%Y")- max(date)), last.seen = max(date),
                                                          num.freq = length(which(!is.na(time.since.last.event))),
                                                          min.freq = as.integer(min(time.since.last.event, na.rm = TRUE)), 
                                                          max.freq = as.integer(max(time.since.last.event, na.rm = TRUE)), 
                                                          q1.freq = as.integer(quantile(time.since.last.event, probs = 0.25,na.rm = TRUE)), 
                                                          q3.freq = as.integer(quantile(time.since.last.event, probs = 0.75,na.rm = TRUE)),
                                                          avg.freq = as.integer(mean(time.since.last.event, na.rm = TRUE)),
                                                          perctentile.last.seen = sum(time.since.last.event <= last.seen.days, 
                                                                                      na.rm = TRUE)/length(which(!is.na(time.since.last.event))))


## ---- echo = FALSE------------------------------------------------------------
frequency <- merge(x= frequency, y = tenure[,c('id', 'tenure')], by= 'id', all.x = TRUE)
summary(frequency)


## ---- echo = FALSE------------------------------------------------------------
frequency <- na.omit(frequency)
clust.data <- scale(frequency[,c(2,4,10,11)])
clusters <- hclust(dist(clust.data))
plot(clusters)

## ---- echo = FALSE------------------------------------------------------------
# Split to 10 groups and compare groups
frequency$hclust.groups <- cutree(clusters, k=10)


## ---- echo = FALSE------------------------------------------------------------
agg.data <- frequency %>% group_by(hclust.groups) %>% summarise(avg.perc.last.seen = mean(perctentile.last.seen, na.rm = TRUE),
                                                             avg.last.seen.days = mean(last.seen.days), na.rm = TRUE,
                                                             avg.tenure = mean(tenure, na.rm = TRUE), 
                                                             avg.total.freq = mean(num.freq, na.rm = TRUE))
ggplot(agg.data, aes(x=hclust.groups, y= avg.perc.last.seen)) + 
  geom_line()
ggplot(agg.data, aes(x=hclust.groups, y= avg.last.seen.days)) + 
  geom_line()
ggplot(agg.data, aes(x=hclust.groups, y= avg.tenure)) + 
  geom_line()
ggplot(agg.data, aes(x=hclust.groups, y= avg.total.freq)) + 
  geom_line()



## ---- echo = FALSE------------------------------------------------------------
frequency %>% group_by(hclust.groups) %>% summarise(n= length(id)) %>% arrange(desc(n))


## ---- echo = FALSE------------------------------------------------------------
library(meanShiftR)
ms.result <- meanShift(na.omit(clust.data))
frequency$ms.groups <- ms.result$assignment


## ---- echo = FALSE------------------------------------------------------------
frequency %>% group_by(ms.groups)  %>% summarise(n= length(id)) %>% arrange(desc(n))


## ---- echo = FALSE------------------------------------------------------------
clust.data <- scale(frequency[,c(2,4,9,10,11)])


## ---- echo = FALSE------------------------------------------------------------
# Elbow method using sum of squares
fviz_nbclust(clust.data, kmeans, method = "wss")


## ---- echo = FALSE------------------------------------------------------------
# average silhouette for k clusters
fviz_nbclust(clust.data, kmeans, method = "silhouette")


## ---- echo = FALSE------------------------------------------------------------
set.seed(123456)
km.clusters <- kmeans(clust.data, centers = 3, nstart = 25)
frequency$km.groups <- km.clusters$cluster


agg.data <- frequency %>% group_by(km.groups) %>% summarise(avg.perc.last.seen = mean(perctentile.last.seen, na.rm = TRUE),
                                                             avg.last.seen.days = mean(last.seen.days, na.rm = TRUE),
                                                             avg.tenure = mean(tenure, na.rm = TRUE), 
                                                             avg.total.freq = mean(num.freq, na.rm = TRUE))

ggplot(agg.data, aes(x=km.groups, y= avg.perc.last.seen)) + 
  geom_line()
ggplot(agg.data, aes(x=km.groups, y= avg.last.seen.days)) + 
  geom_line()
ggplot(agg.data, aes(x=km.groups, y= avg.tenure)) + 
  geom_line()
ggplot(agg.data, aes(x=km.groups, y= avg.total.freq)) + 
  geom_line()

