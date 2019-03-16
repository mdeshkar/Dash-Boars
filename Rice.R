#############################
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
################### Loading the data set ##################33

dataset <- read.csv(file.choose())
dim(dataset)

summary(dataset)

str(dataset)

x <- dataset[,c(3,11,12,13,17,20)]

t<- cor(x)


corrplot(t)

#######################Descriptive statistics ############
attach(dataset)
qplot(noutput, geom = 'density', color = 'red', fill = 'red')

qplot(log(noutput), geom = 'density', color = 'red', fill = 'red')

qplot(log(goutput), geom = "density", fill = 'red', color = 'red')

############################ Individual contineous variables on the Net output and Gross output###########

####### Effect of size on the output ###########
qplot(log(size), log(goutput)) + geom_smooth(method = 'lm')
qplot(log(size), log(noutput)) + geom_smooth(method = "lm")
## Shows high linear relationship ###

######effect of pseeds on the output ###############
qplot(pseed, goutput)
qplot(pseed, noutput) + geom_area()
## Seems Random Doesn't give much information

################# effects of pphosph on the output ##############
qplot(log(pphosph), log(goutput)) + geom_point()
qplot(log(pphosph), log(noutput)) + geom_point()

################# effect of wage on the output ###############
qplot(log(wage), log(goutput)) + geom_jitter()
qplot(log(wage), log(noutput))




################## Checking the effect of the ordial variables on the gross and net out put ##########

########### seed V/S output ##########
qplot(log(seed), log(goutput)) + geom_jitter()
qplot(log(seed), log(noutput)) + geom_jitter()

########## urea V/S output ##########
qplot(log(urea), log(goutput)) + geom_jitter()
qplot(log(urea), log(noutput)) + geom_jitter()

########## hired labour in hours VS output #############
qplot(log(hiredlabor), log(goutput)) + geom_smooth(method = "lm", se=FALSE)
qplot(log(hiredlabor), log(noutput)) + geom_smooth(method = "loess", se = FALSE)

######### Hired family labour in hours vs output ##########
qplot(log(famlabor), log(goutput)) + geom_smooth(method = 'lm', se = FALSE)
qplot(log(famlabor), log(noutput))

######### Total labour excluding harvesting vs output #######
qplot(log(totlabor),log(goutput)) + geom_smooth(method = 'lm', se = FALSE)
qplot(log(totlabor), log(noutput)) + geom_smooth(method = "loess", se = FALSE)


######################## Factor variables vs the output #########

###### Effect of the farm status on the rice output #####
qplot(status, log(goutput), data = dataset, geom = "boxplot")
qplot(status, log(noutput), data = dataset, geom = "boxplot")

###### Effect of the rice varietes on the rice output #####
qplot(varieties, log(goutput), data = dataset , geom = "boxplot")
qplot(varieties, log(noutput), data = dataset, geom = "boxplot")

###### Effect of bimas on the rice output ##########
qplot(bimas, log(goutput), data = dataset, geom = "boxplot")
qplot(bimas, log(goutput), data = dataset, geom = "boxplot")

###### Effect of the region on the rice production ##########
qplot(region, log(goutput), data = dataset, geom = "boxplot")
qplot(region, log(noutput), data = dataset, geom = "boxplot")



############# Mixed graphs dipicting factors affecting the contineous variables #######
qplot(log(size), log(goutput), color = region, geom = 'smooth', se = FALSE)
qplot(log(size), log(goutput), color = status, geom = 'smooth', se = FALSE)
qplot(log(size), log(goutput), color = varieties, geom = 'smooth', se = FALSE)
qplot(log(size), log(goutput)) + facet_grid(.~region)


###################Model Selection ###############################
library(leaps)
subs <- regsubsets(noutput ~.-goutput-X ,data = dataset)
summary(subs)

# qplot(1:8,summary(subs)$adjr2)
# qplot(1:8, summary(subs)$cp)
# qplot(1:8, summary(subs)$bic)
# 

df <- data.frame(
  est = c(summary(subs)$adjr2, summary(subs)$cp,
          summary(subs)$bic),
  x = rep(1:8, 3),
  type = rep(c("adjr2", "cp", "bic"), each = 8)
)

qplot(x, est, data = df, geom = "line") + 
  theme_bw() + facet_grid(type~.,scales = "free_y")

coef(subs, 4)

fit <- lm(noutput ~ size + phosphate + pesticide + totlabor, data = dataset)

y_hat <- predict(fit)

qplot(noutput, predict(fit), data = dataset, geom = "line")

par(row(2,2))
plot(fit)

##################Random FOrest Regressor########################
# spliting data set into testing and training sets #####
library(caTools)
set.seed(123)
split = sample.split(dataset$noutput, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

############## Scaling Dataset ###################################
# training_set = scale(training_set)
# testing_set = scale(testing_set)

#################################################################
# install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor = randomForest(x = training_set[, -c(1,2,18,19)],
                         y = training_set$noutput,
                         ntree = 500)
y_pred = predict(regressor, testing_set)
plot(y_pred, regressor)
qplot(regressor)
varImpPlot(regressor)
summary(regressor)
error = testing_set$noutput - y_pred
qplot(error)
################### Visualization ##############################

lsize <- ifelse(size != 0, log(size), log(size + 1))
sum(is.na(lsize))
ltotlabor <- ifelse(totlabor != 0, log(totlabor), log(totlabor + 1))
sum(is.na(lsize))
lurea <- ifelse(urea != 0, log(urea), log(urea+1))
sum(is.na(lurea))
lhiredlabor <- ifelse(hiredlabor!=0, log(hiredlabor), log(hiredlabor))
sum(is.na(lhiredlabor))
lseed <- ifelse(seed!=0, log(seed), log(seed))
sum(is.na(lseed))
lphosphate <- ifelse(phosphate != 0, log(phosphate), log(phosphate+1))
sum(is.na(lphosphate))
lnoutput <- ifelse(noutput!=0, log(noutput), log(noutput+1))                   

reg_data <- data.frame(lnoutput,lsize, ltotlabor,lurea, lhiredlabor, lseed, lphosphate)
library(caTools)
set.seed(123)
split = sample.split(reg_data$lnoutput, SplitRatio = 2/3)
reg_train = subset(reg_data, split = TRUE)
reg_test = subset(reg_data, split = FALSE)

fit <- lm(lnoutput~ lsize + ltotlabor + lurea + lhiredlabor + lseed + lphosphate,
          data = reg_train)
summary(fit)
y_hat <- predict(fit, newdata = reg_data)
reg_error <- lnoutput - y_hat
qplot(reg_error)
par(mfrow=c(2,2))
plot(fit)
# spliting data set into testing and training sets #####
par(mfrow = c(1,1))
boxplot(exp(y_hat), y_pred, xaxt = "n")
qplot(y_hat)








