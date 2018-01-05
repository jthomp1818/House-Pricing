# Linear Model #2
# This script is based off of Kernel, "Regularized Linear Models (In R)" by JMT5802

##################################################################################################################################
# Libraries

library(data.table)
library(ggplot2)
library(stringr)

##################################################################################################################################
# Read clean train and test datasets

train <- readRDS("~/Documents/Rstuff/House.Pricing/Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

##################################################################################################################################
# Data Preprocessing

# Combine train & test data for processing
all.data <- rbind(train[,2:80], test[,2:80])

# Create data table with SalePrice & LogSalePrice for facet plot
sale.log.price <- rbind(
  data.table(version = "SalePrice", x = train$SalePrice), 
  data.table(version = "LogSalePrice", x = train$LogSalePrice)
  )

ggplot(data = sale.log.price) +
  facet_wrap(~ version, ncol = 2, scales = "free_x") +
  geom_histogram(aes(x = x))

##################################################################################################################################
# For numeric features with excessive skewness, perform log transformation

# First get data type for each feature
feature.classes <- sapply(names(all.data), function(x){class(all.data[[x]])})   # Neat! Creates a function on the fly
numeric.features <- names(feature.classes[feature.classes == "integer"])

# Determine Skew for each numeric feature
skewed.features <- sapply(numeric.features, function(x){skewness(all.data[[x]], na.rm = TRUE)})   # Using e1071 package
#skewed.features <- skewness(train[, numeric.features, with = F])     # Using mltools

# Graph some examples
num.feat.plot <- grid.arrange(
  ggplot(data = all.data) + geom_histogram(aes(x = LotFrontage)),  
  ggplot(data = all.data) + geom_histogram(aes(x = LotArea)),
  ggplot(data = all.data) + geom_histogram(aes(x = YearBuilt)),
  ggplot(data = all.data) + geom_histogram(aes(x = YearRemodAdd)),
  ggplot(data = all.data) + geom_histogram(aes(x = MasVnrArea)),
  ggplot(data = all.data) + geom_histogram(aes(x = BsmtFinSF1)),
  ggplot(data = all.data) + geom_histogram(aes(x = BsmtFinSF2)),
  ggplot(data = all.data) + geom_histogram(aes(x = BsmtUnfSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = TotalBsmtSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = FirstFlrSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = SecondFlrSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = LowQualFinSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = GrLivArea)),
  ggplot(data = all.data) + geom_histogram(aes(x = BsmtFullBath)),
  ggplot(data = all.data) + geom_histogram(aes(x = BsmtHalfBath)),
  ggplot(data = all.data) + geom_histogram(aes(x = FullBath)),
  ggplot(data = all.data) + geom_histogram(aes(x = HalfBath)),
  ggplot(data = all.data) + geom_histogram(aes(x = BedroomAbvGr)),
  ggplot(data = all.data) + geom_histogram(aes(x = KitchenAbvGr)),
  ggplot(data = all.data) + geom_histogram(aes(x = TotRmsAbvGrd)),
  ggplot(data = all.data) + geom_histogram(aes(x = Fireplaces)),
  ggplot(data = all.data) + geom_histogram(aes(x = GarageYrBlt)),
  ggplot(data = all.data) + geom_histogram(aes(x = GarageCars)),
  ggplot(data = all.data) + geom_histogram(aes(x = GarageArea)),
  ggplot(data = all.data) + geom_histogram(aes(x = WoodDeckSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = OpenPorchSF)),
  ggplot(data = all.data) + geom_histogram(aes(x = EnclosedPorch)),
  ggplot(data = all.data) + geom_histogram(aes(x = ThreeSsnPorch)),
  ggplot(data = all.data) + geom_histogram(aes(x = ScreenPorch)),
  ggplot(data = all.data) + geom_histogram(aes(x = PoolArea)),
  ggplot(data = all.data) + geom_histogram(aes(x = MiscVal)),
  ggplot(data = all.data) + geom_histogram(aes(x = MoSold)),
  ggplot(data = all.data) + geom_histogram(aes(x = YrSold))
  )

# First run of skewness of various features
# LotFrontage       LotArea     YearBuilt  YearRemodAdd    MasVnrArea    BsmtFinSF1    BsmtFinSF2     BsmtUnfSF   TotalBsmtSF    FirstFlrSF
# 0.02200155   12.81584284   -0.59949735   -0.45078863    2.61224919    1.42449786    4.14401285    0.91886685    1.15629969    1.46884929
# SecondFlrSF  LowQualFinSF     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath      HalfBath  BedroomAbvGr  KitchenAbvGr  TotRmsAbvGrd
# 0.86123199   12.08254943    1.26870545    0.62451106    3.92957367    0.16751961    0.69420961    0.32615667    4.30004374    0.75797723
# Fireplaces   GarageYrBlt    GarageCars    GarageArea    WoodDeckSF   OpenPorchSF EnclosedPorch ThreeSsnPorch   ScreenPorch      PoolArea
# 0.73311771   -3.90419773   -0.21946812    0.23913398    1.84148611    2.53381111    4.00183390   11.37021931    3.94466577   16.88964504
# MiscVal        MoSold        YrSold
# 21.93591767    0.19578329    0.13233078

##################################################################################################################################
# Set 0 values to NA to correctly adjust skewness
train.analyze <- copy(train)
test.analyze <- copy(test)
all.data.analyze <- rbind(train.analyze[,2:80], test.analyze[,2:80])
all.data.analyze[all.data.analyze == 0] <- NA

# Determine Skew for each numeric feature
skewed.features <- sapply(numeric.features, function(x){skewness(all.data.analyze[[x]], na.rm = TRUE)})   # Using e1071 package

# Graph some examples
num.feat.analyze.plot <- grid.arrange(
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = LotFrontage)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = LotArea)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = YearBuilt)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = YearRemodAdd)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = MasVnrArea)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BsmtFinSF1)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BsmtFinSF2)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BsmtUnfSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = TotalBsmtSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = FirstFlrSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = SecondFlrSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = LowQualFinSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = GrLivArea)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BsmtFullBath)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BsmtHalfBath)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = FullBath)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = HalfBath)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = BedroomAbvGr)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = KitchenAbvGr)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = TotRmsAbvGrd)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = Fireplaces)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = GarageYrBlt)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = GarageCars)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = GarageArea)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = WoodDeckSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = OpenPorchSF)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = EnclosedPorch)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = ThreeSsnPorch)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = ScreenPorch)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = PoolArea)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = MiscVal)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = MoSold)),
  ggplot(data = all.data.analyze) + geom_histogram(aes(x = YrSold))
)

##################################################################################################################################
# Transform excessively skewed features with log(x)
skewed.features <- skewed.features[skewed.features > 0.75]

for (x in names(skewed.features)) {
  all.data.analyze[[x]] <- log(all.data.analyze[[x]])
}

##################################################################################################################################
# Assess categorical features

categorical.features <- names(feature.classes[str_detect(feature.classes, "ord.+|fact.+")])

# Use caret dummyVars function for hot one encoding for categorical features
dummies <- dummyVars(~., all.data[,categorical.features, with = FALSE], fullRank = TRUE)
categorical.1.hot <- predict(dummies, all.data[, categorical.features, with = FALSE])
categorical.1.hot[is.na(categorical.1.hot)] <- 0

# Reconstruct all.data with pre-processed data
all.data <- cbind(all.data.analyze[, numeric.features, with = FALSE], categorical.1.hot)

# Create data for trainging and test
x.train <- all.data[1:nrow(train),]
x.test <- all.data[(nrow(train) + 1):nrow(all.data),]
y <- log(train$SalePrice)
x.train.trial <- cbind(x.train, LogSalePrice = y)

##################################################################################################################################
# Models

# set up caret model training parameters
# model specific training parameter
caret.train.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)    # Creates cross validation parameters

# Test out ridge regression model
lambdas <- seq(1, 0 , -0.001)

# Train model
model.ridge <- train(
  x = x.train.trial,
  y = y,
  method = "glmnet", 
  metric = "RMSE", 
  maximize = FALSE, 
  trControl = caret.train.ctrl,
  tuneGrid = expand.grid(alpha = 0.5, lambda = lambdas)     # Ridge Regression (Somehow...)
  )













