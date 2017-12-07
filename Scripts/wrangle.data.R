# House Pricing
# This R script was created to predict a given house's value given various features of the house and the property it sits on

# Libraries Used
library(data.table)

#======================================================================================================
# Load Datasets

train <- fread("Data/raw/train.csv", na.strings = c("", "NA"))
test <- fread("Data/raw/test.csv", na.strings = c("", "NA"))

#======================================================================================================
# Format Data

setnames(train, "1stFlrSF", "FirstFlrSF")
setnames(train, "2ndFlrSF", "SecondFlrSF")
setnames(train, "3SsnPorch", "ThreeSsnPorch")
train[, `:=`(     # Check to see if integer 
  MSSubClass = factor(MSSubClass, levels=c("20","30","40","45","50","60","70","75","80","85","90","120","150","160","180","190")),
  MSZoning = factor(MSZoning, levels = c("Other","A", "C", "FV", "I", "RH", "RL", "RP", "RM")),
  Street = factor(Street, levels = c("Grvl","Pave")),
  Alley = factor(Alley, levels = c("None","Grvl","Pave")),
  LotShape = factor(LotShape, levels = c("Reg","IR1","IR2","IR3")),
  LandContour = factor(LandContour, levels = c("Lvl","Bnk","HLS","Low")),
  Utilities = factor(Utilities, levels = c("AllPub","NoSewr","NoSeWa","ELO")),
  LotConfig = factor(LotConfig, levels = c("Inside","Corner","CulDSac","FR2","FR3")),
  LandSlope = factor(LandSlope, levels = c("Gtl","Mod","Sev")),
  Neighborhood = factor(Neighborhood, levels = c(
    "Blmngtn","Blueste","BrDale","BrkSide","ClearCr","CollgCr","Crawfor","Edwards","Gilbert","IDOTRR","MeadowV","Mitchel","Names",
    "NoRidge","NPkVill","NridgHt","NWAmes","OldTown","SWISU","Sawyer","SawyerW","Somerst","StoneBr","Timber","Veenker","Other"
  )),
  Condition1 = factor(Condition1, levels = c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe","RRAe")),
  Condition2 = factor(Condition2, levels = c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe","RRAe")),
  BldgType = factor(BldgType, levels = c("Other","1Fam","2FmCon","Duplx","TwnhsE","TwnhsI")),
  HouseStyle = factor(HouseStyle, levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl")),
  OverallQual = factor(OverallQual, levels = c("1","2","3","4","5","6","7","8","9","10"), ordered = T),
  OverallCond = factor(OverallCond, levels = c("1","2","3","4","5","6","7","8","9","10"), ordered = T),
  RoofStyle = factor(RoofStyle, levels = c("Flat","Gable","Gambrel","Hip","Mansard","Shed")),
  RoofMatl = factor(RoofMatl, levels = c("ClyTile","CompShg","Membran","Metal","Roll","Tar&Grv","WdShake","WdShngl")),
  Exterior1st = factor(Exterior1st, levels = c(
    "AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other","Plywood","PreCast","Stone",
    "Stucco","VinylSd","Wd Sdng","WdShing"
  )),
  Exterior2nd = factor(Exterior2nd, levels = c(
    "AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other","Plywood","PreCast","Stone",
    "Stucco","VinylSd","Wd Sdng","WdShing"
  )),
  MasVnrType = factor(MasVnrType, levels = c("BrkCmn","BrkFace","CBlock","None","Stone")),
  ExterQual = factor(ExterQual, levels = c("Po","Fa","TA","Gd","Ex"), ordered = T),
  ExterCond = factor(ExterCond, levels = c("Po","Fa","TA","Gd","Ex"), ordered = T),
  Foundation = factor(Foundation, levels = c("BrkTil","CBlock","PConc","Slab","Stone","Wood")),
  BsmtQual = factor(BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  BsmtCond = factor(BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  BsmtExposure = factor(BsmtExposure, levels = c("None","No","Mn","Av","Gd"), ordered = T),
  BsmtFinType1 = factor(BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered = T),
  BsmtFinType2 = factor(BsmtFinType2, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered = T),
  Heating = factor(Heating, c("Floor","GasA","GasW","Grav","OthW","Wall")),
  HeatingQC = factor(HeatingQC, c("Po","Fa","TA","Gd","Ex"), ordered = T),
  CentralAir = factor(CentralAir, c("N", "Y")),
  Electrical = factor(Electrical, c("SBrkr","FuseA","FuseF","FuseP","Mix")),
  KitchenQual = factor(KitchenQual, c("Po","Fa","TA","Gd","Ex"), ordered = T),
  Functional = factor(Functional, c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), ordered = T),
  FireplaceQu = factor(FireplaceQu, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  GarageType = factor(GarageType, c("None","Detchd","CarPort","BuiltIn","Basment","Attchd","2Types"), ordered = T),
  GarageYrBlt = as.integer(GarageYrBlt),
  GarageFinish = factor(GarageFinish, c("None","Unf","RFn","Fin"), ordered = T),
  GarageQual = factor(GarageQual, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  GarageCond = factor(GarageCond, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  PavedDrive = factor(PavedDrive, c("N","P","Y"), ordered = T),
  PoolQC = factor(PoolQC, c("None","Fa","TA","Gd","Ex"), ordered = T),
  Fence = factor(Fence, c("None","MnWw","GdWo","MnPrv","GdPrv"), ordered = T),
  MiscFeature = factor(MiscFeature, c("None","Elev","Gar2","Othr","Shed","TenC")),
  SaleType = factor(SaleType, c("WD","CWD","VWD","New","COD","Con","ConLw","ConLI","ConLD","Oth")),
  SaleCondition = factor(SaleCondition, c("Normal","Abnorml","AdjLand","Alloca","Family","Partial"))
)]

# Test Dataset
setnames(test, "1stFlrSF", "FirstFlrSF")
setnames(test, "2ndFlrSF", "SecondFlrSF")
setnames(test, "3SsnPorch", "ThreeSsnPorch")
test[, `:=`(     # Check to see if integer 
  MSSubClass = factor(MSSubClass, levels=c("20","30","40","45","50","60","70","75","80","85","90","120","150","160","180","190")),
  MSZoning = factor(MSZoning, levels = c("Other","A", "C", "FV", "I", "RH", "RL", "RP", "RM")),
  Street = factor(Street, levels = c("Grvl","Pave")),
  Alley = factor(Alley, levels = c("None","Grvl","Pave")),
  LotShape = factor(LotShape, levels = c("Reg","IR1","IR2","IR3")),
  LandContour = factor(LandContour, levels = c("Lvl","Bnk","HLS","Low")),
  Utilities = factor(Utilities, levels = c("AllPub","NoSewr","NoSeWa","ELO")),
  LotConfig = factor(LotConfig, levels = c("Inside","Corner","CulDSac","FR2","FR3")),
  LandSlope = factor(LandSlope, levels = c("Gtl","Mod","Sev")),
  Neighborhood = factor(Neighborhood, levels = c(
    "Blmngtn","Blueste","BrDale","BrkSide","ClearCr","CollgCr","Crawfor","Edwards","Gilbert","IDOTRR","MeadowV","Mitchel","Names",
    "NoRidge","NPkVill","NridgHt","NWAmes","OldTown","SWISU","Sawyer","SawyerW","Somerst","StoneBr","Timber","Veenker","Other"
  )),
  Condition1 = factor(Condition1, levels = c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe","RRAe")),
  Condition2 = factor(Condition2, levels = c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe","RRAe")),
  BldgType = factor(BldgType, levels = c("Other","1Fam","2FmCon","Duplx","TwnhsE","TwnhsI")),
  HouseStyle = factor(HouseStyle, levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl")),
  OverallQual = factor(OverallQual, levels = c("1","2","3","4","5","6","7","8","9","10"), ordered = T),
  OverallCond = factor(OverallCond, levels = c("1","2","3","4","5","6","7","8","9","10"), ordered = T),
  RoofStyle = factor(RoofStyle, levels = c("Flat","Gable","Gambrel","Hip","Mansard","Shed")),
  RoofMatl = factor(RoofMatl, levels = c("ClyTile","CompShg","Membran","Metal","Roll","Tar&Grv","WdShake","WdShngl")),
  Exterior1st = factor(Exterior1st, levels = c(
    "AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other","Plywood","PreCast","Stone",
    "Stucco","VinylSd","Wd Sdng","WdShing"
  )),
  Exterior2nd = factor(Exterior2nd, levels = c(
    "AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other","Plywood","PreCast","Stone",
    "Stucco","VinylSd","Wd Sdng","WdShing"
  )),
  MasVnrType = factor(MasVnrType, levels = c("BrkCmn","BrkFace","CBlock","None","Stone")),
  ExterQual = factor(ExterQual, levels = c("Po","Fa","TA","Gd","Ex"), ordered = T),
  ExterCond = factor(ExterCond, levels = c("Po","Fa","TA","Gd","Ex"), ordered = T),
  Foundation = factor(Foundation, levels = c("BrkTil","CBlock","PConc","Slab","Stone","Wood")),
  BsmtQual = factor(BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  BsmtCond = factor(BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  BsmtExposure = factor(BsmtExposure, levels = c("None","No","Mn","Av","Gd"), ordered = T),
  BsmtFinType1 = factor(BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered = T),
  BsmtFinType2 = factor(BsmtFinType2, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered = T),
  Heating = factor(Heating, c("Floor","GasA","GasW","Grav","OthW","Wall")),
  HeatingQC = factor(HeatingQC, c("Po","Fa","TA","Gd","Ex"), ordered = T),
  CentralAir = factor(CentralAir, c("N", "Y")),
  Electrical = factor(Electrical, c("SBrkr","FuseA","FuseF","FuseP","Mix")),
  KitchenQual = factor(KitchenQual, c("Po","Fa","TA","Gd","Ex"), ordered = T),
  Functional = factor(Functional, c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), ordered = T),
  FireplaceQu = factor(FireplaceQu, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  GarageType = factor(GarageType, c("None","Detchd","CarPort","BuiltIn","Basment","Attchd","2Types"), ordered = T),
  GarageYrBlt = as.integer(GarageYrBlt),
  GarageFinish = factor(GarageFinish, c("None","Unf","RFn","Fin"), ordered = T),
  GarageQual = factor(GarageQual, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  GarageCond = factor(GarageCond, c("None","Po","Fa","TA","Gd","Ex"), ordered = T),
  PavedDrive = factor(PavedDrive, c("N","P","Y"), ordered = T),
  PoolQC = factor(PoolQC, c("None","Fa","TA","Gd","Ex"), ordered = T),
  Fence = factor(Fence, c("None","MnWw","GdWo","MnPrv","GdPrv"), ordered = T),
  MiscFeature = factor(MiscFeature, c("None","Elev","Gar2","Othr","Shed","TenC")),
  SaleType = factor(SaleType, c("WD","CWD","VWD","New","COD","Con","ConLw","ConLI","ConLD","Oth")),
  SaleCondition = factor(SaleCondition, c("Normal","Abnorml","AdjLand","Alloca","Family","Partial"))
)]

#======================================================================================================
# Fill in Missing or Incorrect Data

train[MSZoning == "C (all)", MSZoning := "C"]
train[is.na(MSZoning), MSZoning := "Other"]
train[is.na(BldgType), BldgType := "Other"]
train[is.na(LotFrontage), LotFrontage := 0]
train[is.na(Alley), Alley := "None"]
train[is.na(Neighborhood), Neighborhood := "Other"]
train[is.na(Exterior2nd), Exterior2nd := "Other"]
train[is.na(MasVnrType), MasVnrType := "None"]
train[is.na(MasVnrArea), MasVnrArea := 0]
train[is.na(BsmtQual), BsmtQual := "None"]
train[is.na(BsmtCond), BsmtCond := "None"]
train[is.na(CentralAir), BsmtCond := "None"]
train[Id == 949, BsmtExposure := "No"]
train[is.na(BsmtExposure), BsmtExposure := "None"]
train[is.na(BsmtFinType1), BsmtFinType1 := "None"]
train[is.na(BsmtFinType2), BsmtFinType2 := "None"]
train[is.na(Electrical), Electrical := "SBrkr"]
train[is.na(GarageType), GarageType := "None"]
train[is.na(GarageYrBlt), GarageYrBlt := 0]
train[is.na(GarageFinish), GarageFinish := "None"]
train[is.na(GarageQual), GarageQual := "None"]
train[is.na(GarageCond), GarageCond := "None"]
train[is.na(FireplaceQu), FireplaceQu := "None"]
train[is.na(PoolQC), PoolQC := "None"]
train[is.na(Fence), Fence := "None"]
train[is.na(MiscFeature), MiscFeature := "None"]

test[MSZoning == "C (all)", MSZoning := "C"]
test[is.na(MSZoning), MSZoning := "Other"]
test[is.na(BldgType), BldgType := "Other"]
test[is.na(SaleType), SaleType := "WD"]
test[is.na(LotFrontage), LotFrontage := 0]
test[is.na(Alley), Alley := "None"]
test[is.na(Utilities), Utilities := "AllPub"]
test[is.na(Neighborhood), Neighborhood := "Other"]
test[is.na(Exterior1st), Exterior1st := "Other"]
test[is.na(Exterior2nd), Exterior2nd := "Other"]
test[is.na(MasVnrType), MasVnrType := "None"]
test[is.na(MasVnrArea), MasVnrArea := 0]
test[Id %in% c(2218, 2219), BsmtQual := "TA"]
test[is.na(BsmtQual), BsmtQual := "None"]
test[Id %in% c(2041, 2186, 2525), BsmtCond := "TA"]
test[is.na(BsmtCond), BsmtCond := "None"]
test[Id %in% c(1488, 2349), BsmtExposure := "No"]
test[is.na(BsmtExposure), BsmtExposure := "None"]
test[is.na(BsmtFinType1), BsmtFinType1 := "None"]
test[is.na(BsmtFinSF1), BsmtFinSF1 := 0]
test[is.na(BsmtFinType2), BsmtFinType2 := "None"]
test[is.na(BsmtFinSF2), BsmtFinSF2 := 0]
test[is.na(BsmtUnfSF), BsmtUnfSF := 0]
test[is.na(TotalBsmtSF), TotalBsmtSF := 0]
test[is.na(BsmtFullBath), BsmtFullBath := 0]
test[is.na(BsmtHalfBath), BsmtHalfBath := 0]
test[is.na(KitchenQual), KitchenQual := "TA"]
test[is.na(GarageType), GarageType := "None"]
test[is.na(GarageYrBlt), GarageYrBlt := 0]
test[is.na(GarageFinish), GarageFinish := "None"]
test[Id == 2577, `:=`(GarageType = "None", GarageCars = 0)]
test[is.na(GarageArea), GarageArea := 0]
test[is.na(GarageQual), GarageQual := "None"]
test[is.na(GarageCond), GarageCond := "None"]
test[is.na(FireplaceQu), FireplaceQu := "None"]
test[is.na(PoolQC), PoolQC := "None"]
test[is.na(Fence), Fence := "None"]
test[is.na(MiscFeature), MiscFeature := "None"]
test[is.na(Functional), Functional := "Typ"]

##################################################################################################################################
# Engineer Data

#======================================================================================================
# Group Variables

#--------------------------------------------------
# Square Footage Groupings

train[, `:=`(
  TotCoolSF = FirstFlrSF + SecondFlrSF + LowQualFinSF + BsmtFinSF1 + BsmtFinSF2, 
  TotUnSF = BsmtUnfSF + GarageArea + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch
    )]

test[, `:=`(
  TotCoolSF = FirstFlrSF + SecondFlrSF + LowQualFinSF + BsmtFinSF1 + BsmtFinSF2, 
  TotUnSF = BsmtUnfSF + GarageArea + OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch
)]

#--------------------------------------------------
# Number of Bedrooms and Bathrooms Groupings

train[, `:=`(TotBathrooms = FullBath + BsmtFullBath + (HalfBath + BsmtHalfBath)/2)]
test[, `:=`(TotBathrooms = FullBath + BsmtFullBath + (HalfBath + BsmtHalfBath)/2)]

#--------------------------------------------------
# Combine Sold Mo & Yr to create Date

train[, DateSold := as.Date(ISOdate(year = YrSold, month = MoSold, day = 1))]
test[, DateSold := as.Date(ISOdate(year = YrSold, month = MoSold, day = 1))]

#--------------------------------------------------
# Average Room SQ FT

train[, AvgRmSF := TotCoolSF / TotRmsAbvGrd]
test[, AvgRmSF := TotCoolSF / TotRmsAbvGrd]

#--------------------------------------------------
# Create Log of Sale Price Column so that random forrest can be tailored for error reduction

train[, LogSalePrice := log(SalePrice)]

#======================================================================================================
# Test to see if Empty!

apply(X = is.na(train), MARGIN = 2, FUN = sum)
apply(X = is.na(test), MARGIN = 2, FUN = sum)

##################################################################################################################################
# Write Data to File
saveRDS(train, "Data/wrangled/clean.train.rds")
saveRDS(test, "Data/wrangled/clean.test.rds")























