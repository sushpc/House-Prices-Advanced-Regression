# packages
library(tidyverse)
library(readr)
library(randomForest)
library(caret)
library(corrplot)
library(scales)

#Loading Data and combining train and test dataset
train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")
test_data$SalePrice <- NA
train_data$type <- "training_set"
test_data$type <- "test_set"
dataset <- rbind(train_data, test_data)


# finding the number of numeric columns
num_col <- which(sapply(train_data,is.numeric))  # index of the numerical column
train_data_num <- train_data[, num_col]
num_colNames <- names(num_col)  # storing the column names in a new variable

corVar <- cor(train_data_num, use = "pairwise.complete.obs")
corVar_Sorted <- as.matrix(sort(corVar[,"SalePrice"], decreasing = TRUE))
corVar_High <- names(which(apply(corVar_Sorted, 1 , function(x) abs(x)>0.5))) ## select only high correlations

corVar <- corVar[corVar_High, corVar_High]
corrplot.mixed(corVar, tl.col = "black", upper = "circle", tl.pos = "lt")

# Sale Price is strongly correlated with OverallQual
ggplot(data=dataset[dataset$type=="training_set",], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), label = comma)  # labels = comma comes from scales library

# Column having missing values

missingColNum <- which(colSums(is.na(dataset))>0)
missingCol <- sort(colSums(sapply(dataset[missingColNum], is.na)), decreasing = TRUE)
length(missingCol)

# Imputing Missing Values
# firstly looking at PoolQC(pool quality)
unique(dataset$PoolQC)
dataset$PoolQC[is.na(dataset$PoolQC)] <- "None"
dataset$PoolQC <- factor(dataset$PoolQC , levels = c("None", "Fa", "Gd", "Ex"), labels = c(0,1,2,3), ordered = TRUE)

dataset[dataset$PoolArea > 0 & dataset$PoolQC == 0, c('PoolArea', 'PoolQC')]
index <- which(dataset$PoolArea > 0 & dataset$PoolQC == 0)
dataset$PoolArea[index] <- 0     # Imputing 0 as there is no pool available

# Looking at MiscFeature variable
unique(dataset$MiscFeature)
dataset$MiscFeature[is.na(dataset$MiscFeature)] <- "None"
dataset$MiscFeature <- as.factor(dataset$MiscFeature)

ggplot(dataset[dataset$type=="training_set",], aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='lightblue') + 
  geom_text(stat = 'summary', fun.y = "median", aes(label = round(..y..)), vjust = -0.5)+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count..), y = 100)+
  theme_bw()


# Looking at the feature "Alley"
unique(dataset$Alley)
dataset$Alley[is.na(dataset$Alley)] <- "noAlley"
dataset$Alley <- as.factor(dataset$Alley)

ggplot(dataset[dataset$type=="training_set",], aes(x = Alley, y=SalePrice)) +
  stat_summary(geom = 'bar', fun.y = "median", fill= 'lightblue')+
  geom_text(stat = 'summary', fun.y = "median", aes(label = round(..y..)), vjust = -0.5)+
  scale_y_continuous(breaks= seq(0, 200000, by=20000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count..), y = 100)+
  theme_bw()


# Looking at the feature "Fence"
unique(dataset$Fence)
dataset$Fence[is.na(dataset$Fence)] <- "None"
dataset %>% filter(type == 'training_set') %>% group_by(Fence) %>% summarise('mean' = mean(SalePrice), 'count'=n())

dataset$Fence <- as.factor(dataset$Fence)

# looking at the feature "FireplaceQu"
unique(dataset$FireplaceQu)
dataset$FireplaceQu[is.na(dataset$FireplaceQu)] <- 'None'
ggplot(dataset[dataset$type=="training_set",], aes(x = Fireplaces, y=SalePrice)) +
  stat_summary(geom = 'bar', fun.y = "median", fill= 'lightblue')+
  geom_text(stat = 'summary', fun.y = "median", aes(label = round(..y..)), vjust = -0.5)+
  scale_y_continuous(breaks= seq(0, 400000, by=40000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count..), y = 100)+
  theme_bw()
dataset$FireplaceQu <- factor(dataset$FireplaceQu , levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)
table(dataset$FireplaceQu)
table(dataset$Fireplaces)


# Looking at the feature "LotFrontage"- Linear feet of street connected to property
unique(dataset$LotFrontage)
 # Missing values in LotFrontage can be imputed using the median value per neighbourhood
ggplot(dataset[!is.na(dataset$LotFrontage), ], aes(x = Neighborhood,y=LotFrontage))+
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'lightblue')+
  stat_summary(aes(label = ..y..), fun.y ='median', geom = 'text', vjust= -0.5)+
  theme(axis.text.x =element_text(angle = 90))
 
for( i in 1:nrow(dataset)){
  if(is.na(dataset$LotFrontage[i])){
    dataset$LotFrontage[i] <- median(dataset$LotFrontage[which(dataset$Neighborhood == dataset$Neighborhood[i])], na.rm = TRUE)
  }
}
 
# Looking at the feature "Garage" 
dataset[!is.na(dataset$GarageType)& is.na(dataset$GarageQual), c("GarageType","GarageFinish","GarageCond","GarageArea","GarageCars","GarageYrBlt","GarageQual")]
which(!is.na(dataset$GarageType)& is.na(dataset$GarageQual))
dataset[c(2127, 2577), c("GarageType","GarageFinish","GarageCond","GarageArea","GarageCars","GarageYrBlt","GarageQual")]

# imputing garageFinish, garageCond, garageQual with the most common value
dataset$GarageCond[2127] <- names(sort(-table(dataset$GarageCond)))[1]
dataset$GarageQual[2127] <- names(sort(-table(dataset$GarageQual)))[1]
dataset$GarageFinish[2127] <- names(sort(-table(dataset$GarageFinish)))[1]

# there is one missing value for garagecars and GarageArea
unique(dataset$GarageArea)
dataset[2577, c("GarageFinish", "GarageCond", "GarageQual", "GarageType")] <- "No Garage"
dataset[2577, c( "GarageCars", "GarageArea")] <- 0

# GarageType
unique(dataset$GarageType)
dataset$GarageType[is.na(dataset$GarageType)] <- "No Garage"
dataset$GarageType <- as.factor(dataset$GarageType)
table(dataset$GarageType)

# For GarageFinish which is an ordinal variable

sum(is.na(dataset$GarageFinish))
dataset$GarageFinish[is.na(dataset$GarageFinish)] <- "No Garage"
dataset$GarageFinish <- factor(dataset$GarageFinish, levels= c("No Garage", "Unf", "RFn", "Fin"), labels = c(0,1,2,3), ordered = TRUE)
table(dataset$GarageFinish)

# feature GarageCond
dataset$GarageCond[is.na(dataset$GarageCond)] <- "No Garage"
dataset$GarageCond <- factor(dataset$GarageCond, levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)
table(dataset$GarageCond)

#Feature GarageQUal
dataset$GarageQual[is.na(dataset$GarageQual)] <- "No Garage"
dataset$GarageQual <- factor(dataset$GarageQual, levels = c("No Garage","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)
table(dataset$GarageQual)

# GarageYrBlt
ggplot(dataset[!is.na(dataset$GarageYrBlt)& !is.na(dataset$SalePrice), ], aes(x = GarageYrBlt,y=SalePrice))+
  geom_point(stat = 'summary', fun.y = 'median')+ 
  geom_smooth(se = FALSE)

dataset[, c("YearBuilt", "YearRemodAdd", "GarageYrBlt")]  # replacing the missing values with the yearBuilt 
dataset$GarageYrBlt[is.na(dataset$GarageYrBlt)] <- dataset$YearBuilt[is.na(dataset$GarageYrBlt)]

# Looking for Basement Variables
# checking if the missing values are common
sum(is.na(dataset$BsmtCond) & is.na(dataset$BsmtExposure) & is.na(dataset$BsmtQual) & is.na(dataset$BsmtFinType1) & is.na(dataset$BsmtFinType2))
# So there are 79 common missing value

bsmt_all <- c("BsmtCond","BsmtQual","BsmtExposure","BsmtFinType1","BsmtFinType2", "BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF", "BsmtFullBath", "BsmtHalfBath", "TotalBsmtSF")
dataset[(is.na(dataset$BsmtCond) & !is.na(dataset$BsmtFinType1)), bsmt_all]

# fixing the single missing value first
dataset[is.na(dataset$TotalBsmtSF), c("BsmtCond","BsmtQual","BsmtExposure","BsmtFinType1","BsmtFinType2")] <- c("None", "None", "None", "None", "None")
dataset[is.na(dataset$TotalBsmtSF), c("BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF", "BsmtFullBath", "BsmtHalfBath", "TotalBsmtSF")] <- c(0,0,0,0,0,0)
sum(is.na(dataset$TotalBsmtSF))
# fixing the remaining single missing value
dataset[is.na(dataset$BsmtHalfBath)|is.na(dataset$BsmtFullBath), bsmt_all]
dataset[is.na(dataset$BsmtHalfBath)|is.na(dataset$BsmtFullBath), c("BsmtCond","BsmtQual","BsmtExposure","BsmtFinType1","BsmtFinType2")] <- c("None", "None", "None", "None", "None")
dataset[is.na(dataset$BsmtHalfBath), c("BsmtHalfBath", "BsmtFullBath")] <- c(0,0)

# fixing other missing values
dataset[!is.na(dataset$BsmtFinType1) & (is.na(dataset$BsmtCond)|is.na(dataset$BsmtQual)|is.na(dataset$BsmtExposure)|is.na(dataset$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
which(!is.na(dataset$BsmtFinType1) & (is.na(dataset$BsmtCond)|is.na(dataset$BsmtQual)|is.na(dataset$BsmtExposure)|is.na(dataset$BsmtFinType2)))
# Imputing mode values
dataset$BsmtFinType2[333] <- names(sort(-table(dataset$BsmtFinType2)))[1]
dataset$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(dataset$BsmtExposure)))[1]
dataset$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(dataset$BsmtCond)))[1]
dataset$BsmtQual[c(2218, 2219)] <- names(sort(-table(dataset$BsmtQual)))[1]

#BsmtQual
dataset$BsmtQual[is.na(dataset$BsmtQual)] <- 'None'
dataset$BsmtQual <- factor(dataset$BsmtQual, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)
table(dataset$BsmtQual)

#BsmtCond
dataset$BsmtCond[is.na(dataset$BsmtCond)] <- 'None'
dataset$BsmtCond <- factor(dataset$BsmtCond, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)
table(dataset$BsmtCond)

#Bsmt
dataset$BsmtExposure[is.na(dataset$BsmtExposure)] <- 'None'
dataset$BsmtExposure <- factor(dataset$BsmtExposure, levels = c("None","No", "Mn", "Av", "Gd"), labels = c(0,1,2,3,4), ordered = TRUE)
table(dataset$BsmtExposure)

#BsmtFinType1
dataset$BsmtFinType1[is.na(dataset$BsmtFinType1)] <- 'None'
dataset$BsmtFinType1 <- factor(dataset$BsmtFinType1, levels = c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), labels = c(0,1,2,3,4,5,6), ordered = TRUE)
table(dataset$BsmtFinType1)

#BsmtFinType2
dataset$BsmtFinType2[is.na(dataset$BsmtFinType2)] <- 'None'
dataset$BsmtFinType2 <- factor(dataset$BsmtFinType2, levels = c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), labels = c(0,1,2,3,4,5,6), ordered = TRUE)
table(dataset$BsmtFinType2)


#Looking at Masonry Variable

dataset[is.na(dataset$MasVnrType) & !is.na(dataset$MasVnrArea), c("MasVnrType", "MasVnrArea")]
which(is.na(dataset$MasVnrType) & !is.na(dataset$MasVnrArea))
dataset[2611, "MasVnrType"] <- names(sort(-table(dataset$MasVnrType)))[2]

dataset$MasVnrType[is.na(dataset$MasVnrType)] <- 'None'

dataset[!is.na(dataset$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)

dataset$MasVnrType <- factor(dataset$MasVnrType, levels = c("None", "BrkCmn", "BrkFace", "Stone"), labels = c(0,1,2,3), ordered = TRUE)
table(dataset$MasVnrType)

dataset$MasVnrArea[is.na(dataset$MasVnrArea)] <-0

# Looking at MSZoning Variable
unique(dataset$MSZoning)
dataset$MSZoning[is.na(dataset$MSZoning)] <- names(sort(-table(dataset$MSZoning)))[1]
dataset$MSZoning <- as.factor(dataset$MSZoning)
table(dataset$MSZoning)

# Looking at variable Utilities
unique(dataset$Utilities)
table(dataset$Utilities)

dataset$Utilities <- NULL

#Looking at KitchenQual
table(dataset$KitchenQual)
dataset$KitchenQual[is.na(dataset$KitchenQual)] <- 'TA' #replace with most common value
dataset$KitchenQual <- factor(dataset$KitchenQual, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)

#Looking at Functional
table(dataset$Functional)
dataset$Functional[is.na(dataset$Functional)] <- names(sort(-table(dataset$Functional)))[1]
dataset$Functional <- factor(dataset$Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), labels = c(0,1,2,3,4,5,6,7), ordered = TRUE)

# Looking at Exterior Variable
dataset$Exterior1st[is.na(dataset$Exterior1st)] <- names(sort(-table(dataset$Exterior1st)))[1]

dataset$Exterior1st <- as.factor(dataset$Exterior1st)
table(dataset$Exterior1st)

#Exterior2nd
dataset$Exterior2nd[is.na(dataset$Exterior2nd)] <- names(sort(-table(dataset$Exterior2nd)))[1]

dataset$Exterior2nd <- as.factor(dataset$Exterior2nd)

#ExterQual
unique(dataset$ExterQual)
dataset$ExterQual <- factor(dataset$ExterQual, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)

#ExterCond
unique(dataset$ExterCond)
dataset$ExterCond <- factor(dataset$ExterCond, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)


# Looking at the feature Electrical
#imputing mode
dataset$Electrical[is.na(dataset$Electrical)] <- names(sort(-table(dataset$Electrical)))[1]

dataset$Electrical <- as.factor(dataset$Electrical)
table(dataset$Electrical)

# Feature SaleType
table(dataset$SaleType)
unique(dataset$SaleType)
dataset$SaleType[is.na(dataset$SaleType)] <- names(sort(-table(dataset$SaleType)))[1]
dataset$SaleType <- factor(dataset$SaleType)

dataset$SaleCondition <- as.factor(dataset$SaleCondition)

# Label Encoding/factorising the remaining variables
Col_char <- names(dataset[,sapply(dataset, is.character)])
Col_char
# Street variable
table(dataset$Street)
dataset$Street <- factor(dataset$Street, levels =c("Grvl", "Pave"), labels = c(0,1))

table(dataset$PavedDrive)
dataset$PavedDrive <- factor(dataset$PavedDrive, levels =c("N", "P", "Y"), labels = c(0,1,2), ordered = TRUE)

# Lot variable

table(dataset$LotShape)
dataset$LotShape <- factor(dataset$LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), labels = c(0,1,2,3), ordered = TRUE)
table(dataset$LotConfig)
dataset$LotConfig <- as.factor(dataset$LotConfig)

# Land Variable
table(dataset$LandContour)
dataset$LandContour <- as.factor(dataset$LandContour)

table(dataset$LandSlope)
dataset$LandSlope <- factor(dataset$LandSlope, levels =c("Gtl", "Mod", "Sev"), labels = c(0,1,2), ordered = TRUE)

# Heating Variables
table(dataset$Heating)
dataset$Heating <- as.factor(dataset$Heating)

table(dataset$HeatingQC)
dataset$HeatingQC <- factor(dataset$HeatingQC, levels = c("None","Po", "Fa", "TA", "Gd", "Ex"), labels = c(0,1,2,3,4,5), ordered = TRUE)

table(dataset$CentralAir)
dataset$CentralAir <- factor(dataset$CentralAir, levels =c("N", "Y"), labels = c(0,1))

# Roof Variable
table(dataset$RoofMatl)
dataset$RoofMatl <- as.factor(dataset$RoofMatl)
dataset$RoofStyle <- as.factor(dataset$RoofStyle)

# HouseStyle and BldgType
table(dataset$HouseStyle)
dataset$HouseStyle <- as.factor(dataset$HouseStyle)

table(dataset$BldgType)
dataset$BldgType <- as.factor(dataset$BldgType)

#Neighborhood and Conditions
table(dataset$Neighborhood)
dataset$Neighborhood <- as.factor(dataset$Neighborhood)
dataset$Condition1 <- as.factor(dataset$Condition1)
dataset$Condition2 <- as.factor(dataset$Condition2)

# Foundation
dataset$Foundation <- as.factor(dataset$Foundation)

# Looking at the MSSubClass and MoSOld (given as numbers but actually is a factor)
dataset$MSSubClass <- as.factor(dataset$MSSubClass)
dataset$MoSold <- as.factor(dataset$MoSold)

#### Investigating the correlation of variables with SalePrice 
num_col <- which(sapply(train_data,is.numeric))  # index of the numerical column
train_data_num <- train_data[, num_col]
num_colNames <- names(num_col)  # storing the column names in a new variable

corVar <- cor(train_data, use = "pairwise.complete.obs")
corVar_Sorted <- as.matrix(sort(corVar[,"SalePrice"], decreasing = TRUE))
corVar_High <- names(which(apply(corVar_Sorted, 1 , function(x) abs(x)>0.5))) ## select only high correlations

corVar <- corVar[corVar_High, corVar_High]
corrplot.mixed(corVar, tl.col = "black", upper = "circle", tl.pos = "lt")

# Distribution of Sale Price

ggplot(dataset[dataset$type=='training_set',], aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 1 Histogram of SalePrice") +
  ylab("House Count") +
  xlab("Housing Price")
# As the distribution is left-skewed

#split your training data into test & training sets
rows <- sample(nrow(train_data))
train_data <- train_data[rows,]
split = nrow(train_data)*0.80
train <- train_data[1:split, ]
test<- train_data[(split+1):nrow(train_data), ]

#removing the Id variable
dataset$Id <- NULL









#Building a RandomForest model with all variables
formula <- as.formula(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + FullBath + TotRmsAbvGrd)

model_rf_all <- randomForest(x = dataset[dataset$type == 'training_set', c(-79,-80)], y = dataset$SalePrice[dataset$type=='training_set'],ntree = 100, importance = TRUE)
model_rf_1 <- randomForest(formula,data = dataset[dataset$type == 'training_set',],ntree = 100, importance = TRUE)
imp_rf <- importance(model_rf_all)



# Building RF model with tuning the hyperparameter using grid search

library(caret)
myControl <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
model_rf_custom <- train(x = dataset[dataset$type == 'training_set', c(-79,-80)],
                         y = dataset$SalePrice[dataset$type=='training_set'],
                         tuneLength = 3,metric = 'ROC', method = 'ranger', 
                         trControl = myControl)

model_rf_custom1 <- train(x = dataset[dataset$type == 'training_set', c(-79,-80)], 
                          y = dataset$SalePrice[dataset$type=='training_set'],
                          tuneGrid = data.frame(mtry = c(35,40,45,50), splitrule = "variance", min.node.size = 5), 
                          tuneLength = 3, method = 'ranger', trControl = myControl)

model_rf_custom2 <- train(x = dataset[dataset$type == 'training_set', c(-79,-80)],
                          y = dataset$SalePrice[dataset$type=='training_set'],
                          tuneGrid = data.frame(mtry = c(8,10,12,17,20,25), splitrule = "variance", min.node.size = 5),
                          tuneLength = 3, method = 'ranger', trControl = myControl)


# One Hot Encoding the categorical data for xgboost
library(vtreat)
library(magrittr)
var <- colnames(dataset[dataset$type == 'training_set', c(-79,-80)])
treatplan <- designTreatmentsZ(dataset, var, verbose = FALSE)
newvars <- treatplan %>% use_series(scoreFrame) %>% 
            filter(code %in% c("clean", "lev")) %>%
            use_series(varName)
(newvars <- treatplan$scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
    select(varName))
newvars <- newvars$varName

dataset_train <- prepare(treatplan, dataset[dataset$type == 'training_set', c(-79,-80)], varRestriction = newvars)
dataset_test <- prepare(treatplan, dataset[dataset$type == 'test_set', c(-79,-80)], varRestriction = newvars)


## building an XGBoost model
## using cross validation to find the appropriate number of trees
cv <- xgb.cv(data = as.matrix(dataset_train), 
             label = dataset$SalePrice[dataset$type=='training_set'],
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

elog <- cv$evaluation_log # get the evaluation log
# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min((train_rmse_mean)),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min((test_rmse_mean)))   # find the index of min(test_rmse_mean)

# Run xgboost
model_xgb <- xgboost(data = as.matrix(dataset_train), # training data as matrix
                          label = dataset$SalePrice[dataset$type=='training_set'],  # column of outcomes
                          nrounds = 57,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

#Predicting the result for the test set
test_data$SalePrice <- predict(model_xgb, as.matrix(dataset_test))

submission <- test_data %>% select(Id, SalePrice)
write_csv(submission, "House_pricePred.csv")
