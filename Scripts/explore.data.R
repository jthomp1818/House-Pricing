# House Pricing
# This R script was created to predict a given house's value given various features of the house and the property it sits on

options(scipen = 20)

# Libraries Used
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggthemr)
library(reshape2)

# Read clean train and test datasets
train <- readRDS("Data/wrangled/clean.train.rds")
test <- readRDS("Data/wrangled/clean.test.rds")

##################################################################################################################################
# 1.0 Simple Analyses of Existing Data

#======================================================================================================
# 1.1 Neighborhood

#--------------------------------------------------
# Price of house based on neighborhood
price.by.neighborhood <- train[, list(Quantity = .N, AvgSalePrice = round(mean(SalePrice), digits = -3)), by = Neighborhood]

price.neighborhood.bar.plot <- ggplot(price.by.neighborhood, aes(x = Neighborhood, y = AvgSalePrice)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(price.neighborhood.bar.plot + ggtitle("Price by Neighborhood"))

#--------------------------------------------------
# Price of house based on neighborhood and Age of Home
price.neighborhood.age.dotplot <- ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(aes(color = YearRemodAdd), alpha = 0.5, size = 1.5, position = position_jitter(width = 0.25, height = 0))
print(price.neighborhood.age.dotplot + ggtitle("Price by Neighborhood & Age of Home"))

#======================================================================================================
# 1.2 Type of Dwelling

#--------------------------------------------------
# Price of house based on type of dwelling
# MSSubClass
price.dwellingtype.age.dotplot <- ggplot(train, aes(x = MSSubClass, y = SalePrice)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(aes(color = YearRemodAdd), alpha = 0.5, size = 1.5, position = position_jitter(width = 0.25, height = 0))
print(price.dwellingtype.age.dotplot + ggtitle("Price by Dwelling Type & Age of Home"))

# #======================================================================================================
# # 1.3 Zoning
# 
# #--------------------------------------------------
# #Prediction Error = 36%
# 
# # Calc Prediction
# train[, Pred := exp(mean(LogSalePrice)), by = MSZoning]
# 
# # Calc Error
# rmsle(train$Pred, train$SalePrice)
# 
# #--------------------------------------------------
# # Plot Price of Dwelling based on Zoning
# # MSZoning
# price.by.zoning <- train[, list(Quantity = .N, AvgSalePrice = round(mean(SalePrice), digits = -3)), by = MSZoning]
# price.zoning.bar.plot <- ggplot(price.by.zoning, aes(x = MSZoning, y = AvgSalePrice)) +
#   geom_bar(stat = "identity")
# plot(price.zoning.bar.plot)

#======================================================================================================
# 1.4 OverallQual Variable

ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Price by Overall Quality")

#======================================================================================================
# 1.5 Compare YearRemodAdd & YearBuilt

ggplot(train, aes(x = YearRemodAdd, y = SalePrice)) +
  geom_point(shape = 1) +
  ggtitle("Price by Year of Remodel")

ggplot(train, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(shape = 1) +
  ggtitle("Price by Year Built")

# The Great Appalachian Storm of 1950! Caused remodels
ggplot(train, aes(x = YearBuilt, y = SalePrice)) +
  geom_point(aes(x = YearBuilt, y = SalePrice), color = "blue", shape = 1, alpha = 0.5) +
  geom_point(aes(x = YearRemodAdd, y = SalePrice), color = "green", shape = 2, alpha = 0.5) +
  theme_bw() +
  ggtitle("Price by YearRemodAdd & YearBuilt", subtitle = "The Great Applachian Storm of 1950!")

#======================================================================================================
# 1.6 Did time of sale affect price of home? Did current market affect prices? (Housing crash of 2010)

p1 <- ggplot(train, aes(x = DateSold, y = SalePrice/10000)) + 
  geom_point(shape = 1) +
  theme_bw() + 
  ggtitle("Housing Market Affect", subtitle = "Sale Price in 10,000's")

num.homes.sold <- train[, list(NumHomesSold = .N), by = DateSold]
p2 <- ggplot(num.homes.sold, aes(x = DateSold, y = NumHomesSold)) +
  geom_line() +
  theme_bw() + 
  ggtitle("Housing Market Affect")

library(gridExtra)
grid.arrange(p1, p2, ncol = 1)

#======================================================================================================
# 1.7 Did Central Air Correlate with SalePrice? Yes, noticeable correlation!

ggplot(data = train) +
  geom_boxplot(aes(x = CentralAir, y = SalePrice), shape = 1) +
  theme_bw() +
  ggtitle("Price based on Central Air")

#======================================================================================================
# 1.8 Average Room Size vs Sale Price - Poor correlation

ggplot(data = train) +
  geom_point(mapping = aes(x = AvgRmSF, y = SalePrice), shape = 1) +
  theme_bw() +
  ggtitle("Price by Room Size")

cor(x = train$AvgRmSF, y = train$SalePrice)

#======================================================================================================
# 1.9 

cormat <- cor(train[, list(
  MSSubClass = as.numeric(MSSubClass), LotFrontage, LotArea, OverallQual = as.numeric(OverallQual), 
  OverallCond = as.numeric(OverallCond), YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF,
  FirstFlrSF, SecondFlrSF, LowQualFinSF, GrLivArea, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr,
  TotRmsAbvGrd, Fireplaces, GarageYrBlt, GarageCars, GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, ThreeSsnPorch, 
  ScreenPorch, PoolArea, MiscVal, MoSold, YrSold, TotCoolSF, TotUnSF, TotBathrooms, AvgRmSF, SalePrice
)])

# Get Upper Triangle
cormat[lower.tri(cormat)]<- NA

# Reshape data.table
library(reshape2)
melted.cormat <- melt(cormat, na.rm = TRUE)

ggplot(data = melted.cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle("Correlation Heatmap")
















