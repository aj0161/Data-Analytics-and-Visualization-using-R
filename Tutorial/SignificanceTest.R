# Checking for significance: Continuous Variables
# Selecting signifiant variables (continuous) - checking for p-Values

inputData <-(Updated.df[1:13])
significantPreds_cont <- character()  # initialise output. Significant preds will accrue here
response <- Updated.df$Profit

for (predName in names(inputData)) {
  print(predName)
  pred <-as.matrix( inputData[predName])

  mod <- lm(response ~ pred,data = inputData)  # build linear model with only current predictor
  
  p_value <- summary(mod)$coefficients[, 4][2]  # capture p-Value
  
  if (p_value < 0.1 & p_value > 0) {  # check for significance
    
    significantPreds_cont <- c (significantPreds_cont, predName)  # if selected, bind the predictor name if main output
  }
}

inputData_cont_signif <- inputData[, names(inputData) %in% significantPreds_cont]  # filter selected 
inputData_cont_signif["Budget"] <- Updated.df$Budget
inputData_cont_signif["Profit"] <- Updated.df$Profit

head(inputData_cont_signif)



# Checking for significance: Categorical Variables
# Selecting signifiant variables (categorical) - checking for chi-sq test
inputData_cat <- df[c("Title","Genre", "Country" ,"Director", "Writer", "Actors", "Plot", "Language", "Poster", "Type", "Production")]

significantPreds_cat <- character()  # initialise output. Significant preds will accrue here

for (predName in names(inputData_cat)) {
  print(predName)
  pred <-as.matrix( inputData_cat[predName])
  chi_sq <- invisible(suppressWarnings(chisq.test(table(response, pred))))  # perform chi-Squared test
  p_value <- chi_sq[3]  # capture p-Value
  
  if (p_value < 0.1 & p_value > 0) {  # check for significance
    significantPreds_cat <- c (significantPreds_cat, predName)
  }
}

inputData_cat_signif <- as.data.frame(inputData_cat[, names(inputData_cat) %in% significantPreds_cat])

colnames(inputData_cat_signif) <- significantPreds_cat  # assign column names
inputData_signif <- cbind(response, inputData_cont_signif, inputData_cat_signif)


# Decide if a variable is important or not using Boruta
library(Boruta)
inputData <-(Updated.df[1:13])
boruta_output <- Boruta(response ~ ., data=na.omit(inputData), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables


# Box Cox Transformation for Continuous variables
library(moments)
library(caret)
# inputData_cont <-inputData_cont_signif
inputData_cont <-(Updated.df[1:15])
head(inputData_cont)
inputData_response <- Updated.df$Profit

skewness(inputData_response, na.rm=T)  # 0.878864

# Transforming the predictor variables
boxcoxTransformedVars <- character(0)  # variables that underwent box-cox transformation collects here.

for (colname in colnames(inputData_cont[, -1])) {
  x <-as.matrix(inputData_cont[colname])
  
  if(abs(skewness(x, na.rm=T)) > 1){  # check for high skewness. 
    boxcoxMod <- BoxCoxTrans(x)  # calculates lambda value and store in a model
    boxcoxTransformedVars <<- c(boxcoxTransformedVars, colname)
    inputData_cont[colname] <- predict(boxcoxMod, as.vector(x) )  # calculate transformed variable
  }
} 

head(inputData_cont)
head(Updated.df)
