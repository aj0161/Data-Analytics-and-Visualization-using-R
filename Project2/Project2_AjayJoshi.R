#Name:Ajay Joshi
#Project 2

library(ggplot2)
library(digest )
library(reshape )
library(tm) 
library(NLP)
library(SnowballC) 
library(robustHD)
library(stringr)
library(GGally)
library(lubridate)
require(dplyr)
library(tidyr)
library(reshape2)
library(tibble)
library(Matrix)
library(glmnet)
library(Boruta)

LinearRegression_ForAllSpilt <- function(df,log.Transform = FALSE,
                                         Squared.Transform = FALSE, 
                                         No.Transform = TRUE, 
                                         Binned.variableTransform= FALSE) {
  
  Acc_testAndTrain1 <- matrix(ncol=4, nrow=19)
  colnames(Acc_testAndTrain1) <- c("Train.MSE","Test.MSE", "Train_Percentage", "R.Squared")
  counter <- 0
  for(i in 1:19) {
    counter <-  sum(counter, 0.050) #increment by 5%
    if (No.Transform) {
      print(paste('Calling linear model for split: ', counter))
      result <- LinearRegression_byOptions(df, counter, Iteration = 10,log.Transform = FALSE, 
                                           Squared.Transform = FALSE,No.Transform = TRUE, 
                                           Binned.variableTransform= FALSE)
    } else if (Squared.Transform) {
      print(paste('Calling Squared - linear model for split: ', counter))
      result <- LinearRegression_byOptions(df, counter, Iteration = 10,log.Transform = FALSE, 
                                           Squared.Transform = TRUE,No.Transform = FALSE, 
                                           Binned.variableTransform= FALSE)
    }else if (log.Transform) {
      print(paste('Calling log - linear model for split: ', counter))
      result <- LinearRegression_byOptions(df, counter, Iteration = 10,log.Transform = TRUE, 
                                           Squared.Transform = FALSE,No.Transform = FALSE, 
                                           Binned.variableTransform= FALSE)
    }else if (Binned.variableTransform) {
      print(paste('Calling Binned.variableTransform - linear model for split: ', counter))
      result <- LinearRegression_byOptions(df, counter, Iteration = 10,log.Transform = FALSE, 
                                           Squared.Transform = FALSE,No.Transform = FALSE, 
                                           Binned.variableTransform= TRUE)
    }
    Acc_testAndTrain1[i,] <- c( mean(result$Train.MSE),
                                mean(result$Test.MSE), 
                                counter, 
                                mean(result$R.Squared))
  }
  res <- as.data.frame(Acc_testAndTrain1)
  return(res)
} 

LinearRegression_byOptions <- function(df, Split.value, Iteration = 10, 
                                       log.Transform = FALSE, 
                                       Squared.Transform = FALSE, 
                                       No.Transform = TRUE, 
                                       Binned.variableTransform= FALSE)
{
  result <- matrix(ncol=3, nrow=Iteration)
  colnames(result) <- c("Train.MSE","Test.MSE", "R.Squared")
  
  for(i in 1:Iteration) #10 times
  {
    M1 <-NA
    data.list <- CreateRandomPartition(df, Split.value)
    
    Training.Data <- data.list$trainset
    test_data <- data.list$testset
    
    Y.Profit <- Training.Data$Profit
    Actual.test_profit <- test_data$Profit
    
    #remove profit columns from training and testing data
    Training.Data <- Training.Data[ , !(names(Training.Data) %in% "Profit")]
    test_data <- test_data[ , !(names(test_data) %in% "Profit")]
    
    # Fit the regression model using regular linear regression
    if (No.Transform || Binned.variableTransform) {
      M1 <- lm(Y.Profit ~ .,data = Training.Data)
    } else if (Squared.Transform) {
      #Mix of regular and polynoimal variables
      M1 <- lm((Y.Profit) ~ I(Runtime) + I(Runtime^2) + I(Runtime^3) +
                 I(Awards) +  I(Awards^2) + I(Awards^3) +
                 I(Metascore) + I(Metascore^2) +  I(Metascore^3) +
                 imdbRating  + imdbVotes + tomatoMeter  +
                 tomatoRating + tomatoReviews + tomatoFresh + tomatoRotten +
                 tomatoUserMeter + tomatoUserRating + tomatoUserReviews +
                 I(Budget) + I(Budget^2) + I(Budget^3),data = Training.Data)
    }else if (log.Transform) {
      M1 <- lm(I(Log.transform(Y.Profit)) ~ .,data = Training.Data) #Log of Profit 
    }
    
    Train.MSE <- mean(M1$residuals^2) 
    predicated.Testvalue = predict(M1, newdata = test_data)
    Test.MSE <- mean((Actual.test_profit - predicated.Testvalue)^2) 
    R_squared <- summary(M1)$r.squared #r square
    result[i,] <- c(Train.MSE, Test.MSE, R_squared )
  }
  return(as.data.frame(result))
}

#function: get a feature variable, perform binning, and merge into main dataframe
Get_Binned_Merged_finalized_cleaned.df <- function(col, main_df, afeature_variable, df.cleaned_finalized, bin_count) {
  colname <-NA
  colname <- c(col, "Binned")
  colname <- paste(colname, collapse=".")
  main_df <- Binning(afeature_variable, bins =bin_count, main_df, colname, Path=FALSE)
  
  colname1 <- c(colname, 1)
  colname1 <- paste(colname1, collapse="-")
  
  formula <- formula(paste( "~", paste(colname1, collapse= "")))
  
  Finalize.df <- with(main_df,data.frame(model.matrix(formula,main_df)))
  Dirty_Finalize.df <- cbind(data.frame(df.cleaned_finalized, Finalize.df))#merge two dataframe
  Binned_Merged_finalized_cleaned.df <- Dirty_Finalize.df[ , -which(names(Dirty_Finalize.df) %in% c(as.character(col)))] 
  
  return(Binned_Merged_finalized_cleaned.df)
}

CalculateProfit <- function(gross, budget){
  profit <- gross - budget
  return(profit)
}

# convert string runtime value to numeric in minute format.
TimeToMinutes <- function(x) {
  if (is.na(x)) return(NA)
  len <- 0 # default minute value is 0
  isHourThere <- FALSE
  SpiltedValues <- 0 
  
  #check if hour value exists.
  if(grepl("h", x)) { 
    isHourThere <- TRUE
    SpiltedValues   <- strsplit(x,'h')
    hour <- sapply(SpiltedValues, function(x) x[1])
    
    for (i in 1:12) { #Assume upto 12 hours
      if(as.numeric(hour) == i) {
        #convert hour to min
        len <- sum(len, i*60) 
        break;
      }
    }
  }
  min <- 0
  
  #check if min exists
  if (grepl("min", x)) {
    
    if (isHourThere == 1) {
      min <- sapply(SpiltedValues, function(x) x[2]) # "XX min"
      SpiltedMinute   <- strsplit(min,'min') #spilt it
      min <- sapply(SpiltedMinute, function(x) x[1]) # first element is min value
      
    } else { 
      # hour value is missing
      SpiltedMinute   <- strsplit(x,'min')
      min <- sapply(SpiltedMinute, function(x) x[1])
    } 
    
    result<- 0
    result <- sum(len, as.numeric(min))
    return(result)
  }
  else { return(len) }  
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)# Trimmer

CreateRandomPartition <- function( df, split) {
  sample <- sample(1:nrow(df), round(split * nrow(df))) 
  trainset <- df[sample,] 
  testset <- df[-sample,]
  
  return(list(trainset = trainset, testset = testset))
}

plotGraph <- function(df1, df_BinningData, title ) {
  ggplot()  +
    geom_line(data = df1, aes(x = df1$Train_Percentage, y = df1$Train.MSE, color = "black"), size = 2) +
    geom_line(data = df_BinningData, aes(x = df_BinningData$Train_Percentage, y = df_BinningData$Train.MSE, color ="yellow"), size = 1)+
    geom_line(data = df1, aes(x = df1$Train_Percentage, y = df1$Test.MSE, color = "red"), size = 2)+
    geom_line(data = df_BinningData, aes(x = df_BinningData$Train_Percentage, y = df_BinningData$Test.MSE, color = "Green"), size = 1)+
    scale_color_manual(labels = c("Previous_Test.MSE", "Train.MSE", "Previous_Train.MSE", "Test.MSE"), values = c("black", "red", "yellow", "Green")) + 
    xlab('Train_Percentage') + ylab('MSE') + ggtitle(as.character(title))
}

#function to perform lm from 0.05 to 0.05 / train/test split
Perform_LmForAllSpilt <- function(df) {
  
  # adding a feature only if it lowers the mse,
  Acc_testAndTrain1 <- matrix(ncol=3, nrow=19)
  colnames(Acc_testAndTrain1) <- c("Train.MSE","Test.MSE", "Train_Percentage")
  
  counter <- 0
  for(i in 1:19) {
    counter <-  sum(counter, 0.050) #increment by 5%
    result <- LinearRegression_byOptions(df, counter, Iteration = 10)
    Acc_testAndTrain1[i,] <- c( mean(result$Train.MSE), mean(result$Test.MSE), counter)
  }
  
  res <- as.data.frame(Acc_testAndTrain1)
  return(res)
} 

Log.transform <- function(Y) {
  LY = log10(Y + 1 - min(Y))
  return(LY)
}

Square.transform <- function(Y) {
  return(Y^2)
}

Cube.transform <- function(Y) {
  return(Y^3)
}

# create a function for binning. 
#Bin Frequency
#first column (0 to 33.3 %]
#second column [ 33.3 % to 66.67% ]
#Third column [ 66.67% to 100% ]
Bin_it <- function(x, bins =3, Path= FALSE) {
  cutpoints<-quantile(x,(0:bins)/bins)
  print(cutpoints)
  interval.values <- cut(x,cutpoints,include.lowest=TRUE)
  binned <-cut(x,cutpoints,include.lowest=TRUE, labels = c(1:bins)) # three levels of factors
  # Df[columnName] <- binned
  Df <- as.data.frame(binned)
  Df <-model.matrix(~.-1,Df)
  return(Df)
}

# create a function for binning
Binning <- function(x, bins =3, Df, columnName, Path= FALSE) {
  cutpoints<-quantile(x,(0:bins)/bins)
  print(cutpoints)
  interval.values <- cut(x,cutpoints,include.lowest=TRUE)
  binned <-cut(x,cutpoints,include.lowest=TRUE, labels = c(1:bins)) # three levels of factors
  Df[columnName] <- binned
  
  if(Path) { 
    columnName <- c(columnName, "Interval")
    columnName <- paste(columnName, collapse="_")
    Df[columnName] <- interval.values
    return(Df)
  }
  return(Df)
}

#compare mse: adding a feature only if it lowers the TEST MSE,otherwise don't add it.
Is_MSE_Lower <- function(df.master, df.comparer, df.mergeFrom, df.mergeTo ){
  
  #if df.mergeFrom, df.mergeTo  are NA
  if ( (is.na(df.mergeFrom)) && (is.na(df.mergeTo))) {
    IsTestMSE.better <-  mean(df.master$Test.MSE) > mean(df.comparer$Test.MSE)
    IsTrainMSE.better <-  mean(df.master$Train.MSE) > mean(df.comparer$Train.MSE)
    
    if (IsTestMSE.better || IsTrainMSE.better) {
      return( list(IsAdded = TRUE , df.Binning= NULL ))
    }
  }
  
  if ( (is.na(df.master)) || (is.na(df.comparer)) || (is.na(df.mergeFrom)) || (is.na(df.mergeTo))) {
    print("Null dataframe is passed")
    return( list(IsAdded = FALSE,df.Binning= NULL ))
  }
  
  IsTestMSE.better <-  mean(df.master$Test.MSE) > mean(df.comparer$Test.MSE)
  
  if (IsTestMSE.better ) {
    df.mergeTo <- with(df.mergeTo,data.frame(df.mergeFrom)) #merging dataframe
    print("Additional variables are merged !!")
    
    # print(paste('Master Test.MSE: ', mean(df.master$Test.MSE)))
    # print(paste('Current Test.MSE: ', mean(df.comparer$Test.MSE)))
    return( list(IsAdded = TRUE,df.Binning= df.mergeTo ))
  } 
  print("variables are not added because MSE is higher than previous run !!")
  return( list(IsAdded = FALSE,df.Binning= df.mergeTo ))
}

computeMSE <- function(my_df) {
  Acc_testAndTrain1 <- matrix(ncol=4, nrow=19)
  colnames(Acc_testAndTrain1) <- c("Train.MSE","Test.MSE", "Train_Percentage", "R_squared")
  counter <- 0
  
  for(i in 1:19) {
    counter <-  sum(counter, 0.050) #increment by 5%
    
    data.list <- CreateRandomPartition(my_df, counter)
    Training.Data <- data.list$trainset
    test_data <- data.list$testset
    
    Y.Profit <-(Training.Data$Profit)
    Actual.test_profit <- (test_data$Profit)
    
    #remove profit patten columns from training and testing data
    Training.Data <-  Training.Data[, -grep("Profit", colnames(Training.Data))]
    test_data <-  test_data[, -grep("Profit", colnames(test_data))]
    
    # Fit the regression model using regular linear regression
    M1 <- lm(Y.Profit ~ .,data = Training.Data)
    Train.MSE <- mean(M1$residuals^2) # the MSE from the training data
    predicated.Testvalue = predict(M1, newdata = test_data)
    Test.MSE <- mean((Actual.test_profit - predicated.Testvalue)^2) # the test MSE 
    R_squared <- summary(M1)$r.squared #r square
    
    Acc_testAndTrain1[i,] <- c(Train.MSE,Test.MSE, counter, R_squared)
  } #for end
  
  res <- as.data.frame(Acc_testAndTrain1)
  return( list(Output_df = res, 
               TestMse = mean(Train.MSE), 
               TrainMse = mean(Test.MSE),
               R_squared = mean(R_squared)))
}

#Merge all to one dataframe
merge.all <- function(x, ..., by = "row.names") {
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = by)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}

TransformAndFitModel <- function(movies_df, bins ) {
  
  Binned.numeric <- data.frame(matrix(ncol = 1, nrow = nrow(movies_df)))
  Log.numeric <- data.frame(matrix(ncol = 1, nrow = nrow(movies_df)))
  Square.numeric <- data.frame(matrix(ncol = 1, nrow = nrow(movies_df)))
  
  Model_result <- NA
  counter <- 1
  bins <- 3
  new_mse <-  mean(df1$Test.MSE) #test mse from q1
  possible_transformations <- c("Log", "Square", "Cube", "Binning")
  possible_transformations <- c("Log", "Square", "Binning")
  numerical_variables <- names(movies_df)
  
  for (transformation in possible_transformations) {
    print(transformation)
    cat( sep="\n\n")
    initial_MSE <- new_mse # 
    for (variable in numerical_variables) {
      
      WillComputeMSE <- FALSE
      if (transformation == "Log") {
        
        movies_df[[paste0(variable, "_",transformation, counter)]] <- Log.transform(movies_df[[variable]])
        Log.numeric[[paste0(variable, "_",transformation, counter)]] <- Log.transform(movies_df[[variable]])
        WillComputeMSE <- TRUE
        
      }else if (transformation == "Square") {
        
        movies_df[[paste0(variable, "_",transformation, counter)]] <- Square.transform(movies_df[[variable]])
        Square.numeric[[paste0(variable, "_",transformation, counter)]] <- Square.transform(movies_df[[variable]])
        WillComputeMSE <- TRUE
        
      } else if(transformation == "Cube") {
        
        movies_df[[paste0(variable, "_",transformation, counter)]] <- Cube.transform(movies_df[[variable]])
        WillComputeMSE <- TRUE
        
      }else if(transformation == "Binning") {
        
        temp.df <- NA
        temp.df <- as.data.frame(Bin_it(movies_df$Awards, bins, Path=FALSE))
        Bin.count <- 1
        while (Bin.count <= bins) {
          movies_df[[paste0(variable,Bin.count, "_",transformation, counter)]] <- temp.df[,Bin.count]
          Binned.numeric[[paste0(variable,Bin.count, "_",transformation, counter)]] <- temp.df[,Bin.count]
          Bin.count <- Bin.count +1
        }
        WillComputeMSE <- TRUE
      }
      
      if (WillComputeMSE) {
        print("i will compute MSE")
        Model_result <- computeMSE(movies_df)
        new_mse <- mean(Model_result$TestMse)
        
        if (new_mse < initial_MSE) {#TODO: overfitting check
          print(paste('The variable is added ', (variable)))
          counter <- counter + 1
          
        }  else {
          print(paste('New MSE is greater than the initial MSE. The variable is not added ', (variable)))
          if (transformation == "Binning") {
            movies_df <- movies_df[,-c((ncol(movies_df) +1 - bins) : ncol(movies_df) )]#remove the three last added column
          } else {
            movies_df <- movies_df[,-c(ncol(movies_df))]#remove the recent added column
          }
        }
      }
      print(new_mse)
    }#inner for loop
  }
  return(list(Model_result = Model_result, Binned.numeric = Binned.numeric, 
              Log.numeric = Log.numeric,Square.numeric = Square.numeric))
}


#################################---Prerequisites END---######################################

# remove all variables except functions
rm(list = setdiff(ls(), lsf.str()))

# get the data from the url source
load(url("https://s3.amazonaws.com/content.udacity-data.com/courses/gt-cs6262/project/movies_merged"))

#--------------------------------------Data preprocessing-----------------------------------------------------

# omit all rows in which gross and budget are not available
df <- movies_merged[complete.cases(movies_merged$Budget),]
df <- movies_merged[complete.cases(movies_merged$Gross),]

# To predict the profit associated with a movie, Generates the data with movies only.
df <- df[df$Type == "movie",]

#convert string N/A to NA and delete it
df$Writer <- sapply(df$Writer,function(x) ifelse(x== "N/A",NA,x))

#remove all movies released prior to 2000.
df <- df[df$Year > 2000,]

#convert Runtime, Awards, and Metascore to numeric.
df$Runtime <- sapply(as.character(df$Runtime), TimeToMinutes) #convert Runtime

#Assumption: "Awards" variable is numeric and I'll using it on question 1 as numeric vraible
df$Awards[df$Awards == "N/A"]<- as.numeric(0) #replace N\A to 0
df$Awards <- str_replace_all(df$Award, "[[:punct:]]", " ") #remove special characters
df$Awards <- trim(df$Awards) #trim
x <- sapply(df$Awards, function(x) as.numeric(str_extract_all(x,"\\(?[0-9,.]+\\)?")[[1]]))#regular expression
y <- lapply(x, function(x) sum(x))#summation
ans <- unlist(y)#unlist it
df$Awards <- as.numeric(ans) #replace Awards column with new values:ans

#convert Metascore
df$Metascore[df$Metascore == "N/A"]<- as.numeric(0) #replace N\A to 0
df$Metascore <- sapply(df$Metascore, as.numeric)

#remove all NAs
df <- df[complete.cases(df),]

#remove gross, domestic gross and box_office and all non- numeric columns
Updated.df <- subset(df, select=-c(Title, Rated, Released, Genre, Director, Writer, Actors, Plot, Language, Country, Poster,imdbID, Type, tomatoImage, tomatoConsensus, tomatoURL, DVD,  Production,Website, Response, Gross,Domestic_Gross, BoxOffice, Year, Date ))

#making sure all columns are numeric
for(i in seq(ncol(Updated.df))) {
  Updated.df[,i] <- as.numeric(as.character(Updated.df[,i]))
}

#calculate profit = gross - budget
profit <- mapply(CalculateProfit,df$Gross,df$Budget)

#add profit to the dataframe
Updated.df["Profit"] <- as.numeric(profit) #Numeric only - dataframe
df["Profit"] <- as.numeric(profit) 

#--------------------------------------Question 1--------------------------------------

#Use linear regression to predict profit based on all available numeric variables. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?

Model_result <-LinearRegression_ForAllSpilt(Updated.df)

df1 <- as.data.frame(Model_result)

ggplot() +
  geom_line(data = df1, aes(x = df1$Train_Percentage, y = df1$Train.MSE, color = "red")) +
  geom_line(data = df1, aes(x = df1$Train_Percentage, y = df1$Test.MSE, color = "blue"))   +
  scale_color_manual(labels = c("Train.MSE", "Test.MSE"), values = c("blue", "red")) + 
  xlab('Train_Percentage') + ylab('MSE')

mean(df1$Test.MSE)
mean(df1$Train.MSE)
mean(df1$R.Squared)
#---------------------------------------------Question 2---------------------------------------

# perform log-transformation on response variable. Use log(Profit) as response variable
df_numeric <- Updated.df
YLog.model <- LinearRegression_ForAllSpilt(df_numeric,
                                           log.Transform = TRUE, 
                                           No.Transform = FALSE)

df_YlogData <- as.data.frame(YLog.model)
plotGraph(df1, df_YlogData, "Figure:2.1 Log of Profit Transformation") 
mean(df_YlogData$Test.MSE)
mean(df_YlogData$Train.MSE)
mean(df_YlogData$R.Squared)
#Note: It doesnt perform well!!

#squared transformation
squared.model<-LinearRegression_ForAllSpilt(df_numeric, Squared.Transform = TRUE,
                                            No.Transform = FALSE)

df_squaredData <- as.data.frame(squared.model)
plotGraph(df1, df_squaredData, "Figure:2.2  Selective Squared Transformation")
mean(df_squaredData$Test.MSE)
mean(df_squaredData$Train.MSE)
mean(df_squaredData$R.Squared)

#First attempt: My approach to get lower MSE:
#1. each numeric variable is binned and converted to one-hot encoding, and added to other unbinned numeric variables
#2. perform linear regression
#3. Check if the MSE is lower with the binned variable: 
#YES:  add the binned variable to the dataframe
#NO: drop the binned variable but keep the unbinned variable in the dataframe

FeatureSelection_BinningOnly <- function(All.df, bin_num) {
  All.df <- df_numeric
  NotAddedList <- list()
  column_List <- list()
  Finalize.MSE <- NA
  Finalize.df <- NA
  column_List$name <- c("Budget", "Runtime", "Awards", "Metascore", "imdbRating", 
                        "imdbVotes", "tomatoMeter", "tomatoRating", "tomatoReviews", 
                        "tomatoFresh", "tomatoRotten","tomatoUserMeter", 
                        "tomatoUserRating","tomatoUserReviews")
  
  #Budget--------------------------------------------------------------------------------------
  colname <- c(column_List$name[1], "Binned")
  colname <- paste(colname, collapse=".")
  All.df <- Binning(All.df$Budget, bins =bin_num, All.df, colname, Path=FALSE) #bbinning process
  
  colname1 <- c(colname, 1)
  colname1 <- paste(colname1, collapse="-")
  formula <- formula(paste( "~", paste(colname1, collapse= "")))
  
  Finalize.df <-with(All.df,data.frame(model.matrix(formula,All.df)))# binned variables separeted into 3 cols
  
  Dirty_Finalize.df <- cbind(data.frame(All.df, Finalize.df))
  
  myCleaned_Finalize.df <- Dirty_Finalize.df[ , -which(names(Dirty_Finalize.df) %in% c(as.character(column_List$name[1]), colname))] #remove column
  
  # perform lm
  mse_budget <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  #update to main items
  Finalize.MSE <- mse_budget
  Cleaned_Finalize.df <-myCleaned_Finalize.df
  
  #Runtime-------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[2], All.df, All.df$Runtime, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_Runtime <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_Runtime$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_Runtime$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print("Runtime is added")
    #merge local mse to main mse 
    Finalize.MSE <- mse_Runtime 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[2], NotAddedList)
    print(paste('Binned variable:', column_List$name[2], 
                'are not added because Test MSE is higher than that of previous variable'))
  }
  
  #Awards-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[3], All.df, All.df$Awards, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_Awards <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_Awards$Test.MSE)
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print("Awards binned variables are added")
    #merge local mse to main mse 
    Finalize.MSE <- mse_Awards 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[3], NotAddedList)
    print(paste('Binned variable:', column_List$name[3], 
                'are not added because Test MSE is higher than that of previous variable'))
  }
  
  #Metascore-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[4], All.df, All.df$Metascore, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_Metascore <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_Metascore$Test.MSE)
  #isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_Metascore$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print("Metascore binned variables are added")
    #merge local mse to main mse 
    Finalize.MSE <- mse_Metascore 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[4], NotAddedList)
    print(paste('Binned variable:', column_List$name[4], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #imdbRating-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[5], All.df, All.df$imdbRating, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_imdbRating <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_imdbRating$Test.MSE)
  #isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_imdbRating$Test.MSE)
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[5]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_imdbRating 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[5], NotAddedList)
    print(paste('Binned variable:', column_List$name[5], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  
  #imdbVotes-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[6], All.df, All.df$imdbVotes, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_imdbVotes <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_imdbVotes$Test.MSE)
  #isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_imdbVotes$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[6]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_imdbVotes 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[6], NotAddedList)
    print(paste('Binned variable:', column_List$name[6], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoMeter-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[7], All.df, All.df$tomatoMeter, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoMeter <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoMeter$Test.MSE)
  #isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoMeter$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[7]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoMeter 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[7], NotAddedList)
    print(paste('Binned variable:', column_List$name[7], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoRating-------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[8], All.df, All.df$tomatoRating, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoRating <- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoRating$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoRating$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[8]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoRating 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[8], NotAddedList)
    print(paste('Binned variable:', column_List$name[8], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoReviews------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[9], All.df, All.df$tomatoReviews, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoReviews<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoReviews$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoReviews$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[9]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoReviews 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[9], NotAddedList)
    print(paste('Binned variable:', column_List$name[9], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoFresh------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[10], All.df, All.df$tomatoFresh, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoFresh<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoFresh$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoFresh$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[10]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoFresh 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[10], NotAddedList)
    print(paste('Binned variable:', column_List$name[10], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoRotten------------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[11], All.df, All.df$tomatoRotten, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoRotten<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoRotten$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoRotten$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[11]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoRotten 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[11], NotAddedList)
    print(paste('Binned variable:', column_List$name[11], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoUserMeter----------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[12], All.df, All.df$tomatoUserMeter, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoUserMeter<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoUserMeter$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoUserMeter$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[12]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoUserMeter 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[12], NotAddedList)
    print(paste('Binned variable:', column_List$name[12], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoUserRating---------------------------------------------------------------------------------------------------------------
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[13], All.df, All.df$tomatoUserRating, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoUserRating<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoUserRating$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoUserRating$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[13]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoUserRating 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[13], NotAddedList)
    print(paste('Binned variable:', column_List$name[13], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  #tomatoUserReviews--------------------------------------------------------------------------------------------------------------
  
  
  myCleaned_Finalize.df<- Get_Binned_Merged_finalized_cleaned.df(column_List$name[14], All.df, All.df$tomatoUserReviews, Cleaned_Finalize.df, bin_num)
  
  # perform lm
  mse_tomatoUserReviews<- LinearRegression_ForAllSpilt(myCleaned_Finalize.df,log.Transform = FALSE, Squared.Transform = FALSE, No.Transform = FALSE, Binned.variableTransform= TRUE)
  
  isMSE_smaller <- mean(Finalize.MSE$Test.MSE) > mean(mse_tomatoUserReviews$Test.MSE)
  # isMSE_smaller <- mean(df1$Test.MSE) > mean(mse_tomatoUserReviews$Test.MSE) 
  
  #if MSE is lower than before then add it!!
  if (isMSE_smaller) {
    print(paste(column_List$name[14]," binned variables are added"))
    #merge local mse to main mse 
    Finalize.MSE <- mse_tomatoUserReviews 
    #merge a recent binned variables to parent dataframe "Cleaned_Finalize.df" 
    Cleaned_Finalize.df <-myCleaned_Finalize.df 
  } else { 
    NotAddedList <-append(column_List$name[14], NotAddedList)
    print(paste('Binned variable:', column_List$name[14], 
                'are not added because MSE is higher than that of previous variable'))
  }
  
  # Q2- End of first attempt------------------------------------------------------------------------------------------------------
  return(list(Final_Output = Finalize.MSE, mse_budget=mse_budget, mse_Runtime = mse_Runtime, mse_Awards = mse_Awards, 
              mse_Metascore = mse_Metascore, mse_imdbRating = mse_imdbRating, mse_imdbVotes = mse_imdbVotes, 
              mse_tomatoMeter = mse_tomatoMeter,mse_tomatoRating = mse_tomatoRating, 
              mse_tomatoReviews = mse_tomatoReviews, mse_tomatoFresh = mse_tomatoFresh,  
              mse_tomatoRotten = mse_tomatoRotten,mse_tomatoUserMeter = mse_tomatoUserMeter, 
              mse_tomatoUserRating = mse_tomatoUserRating,mse_tomatoUserReviews = mse_tomatoUserReviews  ))
  
}#end functiion

result_FeatureSelection_BinningOnly<- FeatureSelection_BinningOnly(df_numeric, 4)

plotGraph(df1, result_FeatureSelection_BinningOnly$Final_Output, "Figure:2.3-Final_Output: FeatureSelection_BinningOnly")#Final graph: all binned graph
mean(result_FeatureSelection_BinningOnly$Final_Output$Test.MSE)
mean(result_FeatureSelection_BinningOnly$Final_Output$Train.MSE)
mean(result_FeatureSelection_BinningOnly$Final_Output$R.Squared)

#graph when each binned variable is added on linear model
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_budget, "All Numeric + Binned.Budget")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_Runtime, "All Numeric + Binned.Runtime")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_Awards, "All Numeric + Binned.Awards")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_Metascore, "All Numeric + Binned.Metascore")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_imdbRating, "All Numeric + Binned.imdbRating")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_imdbVotes, "All Numeric + Binned.imdbVotes")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoMeter, "All Numeric + Binned.tomatoMeter")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoRating, "All Numeric + Binned.tomatoRating")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoReviews, "All Numeric + Binned.tomatoReviews")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoFresh, "All Numeric + Binned.tomatoFresh")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoReviews, "All Numeric + Binned.tomatoReviews")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoRotten, "All Numeric + Binned.tomatoRotten")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoUserMeter, "All Numeric + Binned.tomatoUserMeter")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoUserRating, "All Numeric + tomatoUserRating")
plotGraph(df1, result_FeatureSelection_BinningOnly$mse_tomatoUserReviews, "All Numeric + Binned.tomatoUserReviews")


#Second attempt: My approach to get lower MSE:
#1. each numeric variable is binned and converted to one-hot encoding,
#2. perform linear regression only on the binned variable 
#3. Check if the MSE is lower with the older binned variable: 
#YES:  add the binned variable to the dataframe
#NO: skip to the next unbinned variable

FeatureSelection2 <- function(df) {
  
  # bining process starts####################################################################################################
  d <- df
  df.Binning <-NA
  NotAddedList <- list()
  
  #Budget-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$Budget, bins =3, d, "Binned.Budget", Path=TRUE)
  df.Binning <- with(d,data.frame(model.matrix(~Binned.Budget-1,d)))
  
  df.Binning["Profit"] <- d$Profit # add profit
  mse_budget<- Perform_LmForAllSpilt(df.Binning)
  
  mseForAllRun <- mse_budget
  #Runtime-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$Runtime, bins =3, d, "Binned.Runtime", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.Runtime-1,d)), df.Binning)
  
  mse_Runtime <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_Runtime,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("Runtime is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_Runtime
  } else { NotAddedList <- c("Runtime")}
  
  #Awards-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$Awards, bins =3, d, "Binned.Awards", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.Awards-1,d)), df.Binning)
  
  mse_Awards <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_Awards,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("Awards is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_Awards
  }else {  NotAddedList <-append("Awards", NotAddedList)}
  
  #Metascore-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$Metascore, bins =3, d, "Binned.Metascore", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.Metascore-1,d)), df.Binning)
  
  mse_Metascore <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_Metascore,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("Metascore is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_Metascore
  }else {  NotAddedList <-append("Metascore", NotAddedList)}
  
  #imdbRating-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$imdbRating, bins =3, d, "Binned.imdbRating", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.imdbRating-1,d)), df.Binning)
  
  mse_imdbRating <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_imdbRating,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("imdbRating is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_imdbRating
  }else {  NotAddedList <-append("imdbRating", NotAddedList)}
  
  #imdbVotes-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$imdbVotes, bins =3, d, "Binned.imdbVotes", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.imdbVotes-1,d)), df.Binning)
  
  mse_imdbVotes <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_imdbVotes,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("imdbVotes is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_imdbVotes
  }else {  NotAddedList <-append("imdbVotes", NotAddedList)}
  
  #tomatoMeter-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoMeter, bins =3, d, "Binned.tomatoMeter", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoMeter-1,d)), df.Binning)
  
  mse_tomatoMeter <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoMeter,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoMeter is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoMeter
  } else {  NotAddedList <-append("tomatoMeter", NotAddedList)}
  
  #tomatoRating-------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoRating, bins =3, d, "Binned.tomatoRating", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoRating-1,d)), df.Binning)
  
  mse_tomatoRating <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoRating,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoRating is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoRating
  } else {  NotAddedList <-append("tomatoRating", NotAddedList)}
  
  #tomatoReviews------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoReviews, bins =3, d, "Binned.tomatoReviews", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoReviews-1,d)), df.Binning)
  
  mse_tomatoReviews <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoReviews,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoReviews is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoReviews
  } else {  NotAddedList <-append("tomatoReviews", NotAddedList)}
  
  #tomatoFresh------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoFresh, bins =3, d, "Binned.tomatoFresh", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoFresh-1,d)), df.Binning)
  
  mse_tomatoFresh <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoFresh,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoFresh is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoFresh
  } else {  NotAddedList <-append("tomatoFresh", NotAddedList)}
  
  #tomatoRotten------------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoRotten, bins =3, d, "Binned.tomatoRotten", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoRotten-1,d)), df.Binning)
  
  mse_tomatoRotten <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoRotten,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoRotten is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoRotten
  } else {  NotAddedList <-append("tomatoRotten", NotAddedList)}
  
  #tomatoUserMeter----------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoUserMeter, bins =3, d, "Binned.tomatoUserMeter", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoUserMeter-1,d)), df.Binning)
  
  mse_tomatoUserMeter <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoUserMeter,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoUserMeter is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoUserMeter
  } else {  NotAddedList <-append("tomatoUserMeter", NotAddedList)}
  
  #tomatoUserRating---------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoUserRating, bins =3, d, "Binned.tomatoUserRating", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoUserRating-1,d)), df.Binning)
  
  mse_tomatoUserRating <- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoUserRating,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoUserRating is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoUserRating
  } else {  NotAddedList <-append("tomatoUserRating", NotAddedList)}
  
  #tomatoUserReviews--------------------------------------------------------------------------------------------------------------
  d <- Binning(d$tomatoUserReviews, bins =3, d, "Binned.tomatoUserReviews", Path=TRUE)
  temp <- cbind(data.frame(model.matrix(~Binned.tomatoUserReviews-1,d)), df.Binning)
  
  mse_tomatoUserReviews<- Perform_LmForAllSpilt(temp)
  MSE <- Is_MSE_Lower(mseForAllRun, mse_tomatoUserReviews,temp, df.Binning)
  
  if (MSE$IsAdded) {
    print("tomatoUserReviews is added")
    df.Binning <- MSE$df.Binning
    mseForAllRun <- mse_tomatoUserReviews
  } else {  NotAddedList <-append("tomatoUserReviews", NotAddedList)}
  
  return(list(Final_Output = mseForAllRun, mse_budget=mse_budget, mse_Runtime = mse_Runtime, mse_Awards = mse_Awards, 
              mse_Metascore = mse_Metascore, mse_imdbRating = mse_imdbRating, mse_imdbVotes = mse_imdbVotes, 
              mse_tomatoMeter = mse_tomatoMeter,mse_tomatoRating = mse_tomatoRating, 
              mse_tomatoReviews = mse_tomatoReviews, mse_tomatoFresh = mse_tomatoFresh,  
              mse_tomatoRotten = mse_tomatoRotten,mse_tomatoUserMeter = mse_tomatoUserMeter, 
              mse_tomatoUserRating = mse_tomatoUserRating,mse_tomatoUserReviews = mse_tomatoUserReviews  ))
}
result_using_FeatureSelection2<- FeatureSelection2(df_numeric)

#best graph
plotGraph(df1, result_using_FeatureSelection2$Final_Output, "Figure:2.4-Second attempt: FeatureSelection_BinningOnly")
mean(result_using_FeatureSelection2$Final_Output$Train.MSE)
mean(result_using_FeatureSelection2$Final_Output$Test.MSE)


#Third attempt:
# I'm using mixture of log, square, cude, and binning tranformation to achieve to lower MSE than that in Q1 
bins <- 3
ThirdAttempt.result <- TransformAndFitModel(df_numeric, bins)

plotGraph(df1, ThirdAttempt.result$Model_result$Output_df, "Figure:2.5-Mix of log, square, cube, & binning tranformation")
plotGraph(log(df1), log(ThirdAttempt.result$Model_result$Output_df), "Figure:2.4-Mix of log, square, cube, & binning tranformation in Log Form")

mean(ThirdAttempt.result$Model_result$Output_df$Train.MSE)
mean(ThirdAttempt.result$Model_result$Output_df$Test.MSE)
mean(ThirdAttempt.result$Model_result$Output_df$R_squared)

All.binned.Numeric.df <- as.data.frame(ThirdAttempt.result$Binned.numeric[2: (ncol(ThirdAttempt.result$Binned.numeric) -3)])
All.Log.Numeric.df <- as.data.frame(ThirdAttempt.result$Log.numeric[2: (ncol(ThirdAttempt.result$Log.numeric) -1)])
All.Squared.Numeric.df <- as.data.frame(ThirdAttempt.result$Square.numeric[2: (ncol(ThirdAttempt.result$Log.numeric) -1)])
#---------------------------------------------Question 3----------------------------------------------------

# Write code that featurizes genre (can use code from Part-I), actors, directors, and other categorical variables. Explain how you encoded the variables into features.

mydf <- df
Kth_popular.Value <- 10

#--------------------------------------------genre#----------------------------------------------

GetTopKthGenre <- function(movies_sub, Kth_popular.Value ) {
  
  movies_sub$Genre <- strsplit(movies_sub$Genre,"(\\s)?,(\\s)?")
  unnested <- unnest(movies_sub) # unnested data frame to create the indicator columns
  Binned.Genre <- dcast(unnested, ... ~Genre, fun.aggregate = length) ## We remove useless levels
  
  ## Lets quite track of the unique genres
  all_genres <- unique(unlist(movies_sub$Genre))
  
  Kth_popular.genres <- head(sort(colSums(Binned.Genre[all_genres]), decreasing = T), Kth_popular.Value)
  Kth_popular.genresNames <- names(Kth_popular.genres)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Genre) %in% c(Kth_popular.genresNames, "Title")  
  
  Top_Kth_Binned.Genre <- Binned.Genre[myvars]
  Other_Binned.Genre <- Binned.Genre[!myvars]
  
  #make them numeric
  Other_Binned.Genre <- as.data.frame( sapply(Other_Binned.Genre,function(x) as.numeric(x)))
  
  #sum of row
  SumOfGenre <- rowSums(Other_Binned.Genre, na.rm = TRUE)
  
  Other_Binned.Genre[,"Other"] <- SumOfGenre
  Other_Binned.Genre[,"Other"] <- sapply(Other_Binned.Genre$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Genre <- add_column(Other_Binned.Genre, Top_Kth_Binned.Genre$Title, .after = 1)
  
  Top_Kth_Binned.Genre[,"Other"] <- Other_Binned.Genre$Other
  colnames(Top_Kth_Binned.Genre) <- paste("Is_Genre", colnames(Top_Kth_Binned.Genre), sep = "_")
  
  return(Top_Kth_Binned.Genre)
}

movies_sub <- df[c("Title","Genre")]
Top_Kth_Binned.Genre <- GetTopKthGenre(movies_sub, Kth_popular.Value) #top 10 

#--------------------------------------------Director----------------------------------------------
Kth_popular.Value <- 75
GetTopKthDirector <- function(movies_sub, Kth_popular.Value ) {
  
  movies_sub$Director <- strsplit(movies_sub$Director,"(\\s)?,(\\s)?")
  # unnested <- unnest(movies_sub) # unnested data frame to create the indicator columns
  Binned.Director <- dcast(unnest(movies_sub), ... ~Director, fun.aggregate = length) # remove useless levels
  
  ## track of the unique genres
  all_directors <- unique(unlist(movies_sub$Director))
  
  Kth_popular.Director <- head(sort(colSums(Binned.Director[all_directors]), decreasing = T), Kth_popular.Value)/ nrow(movies_sub)
  
  
  #Select only kth popular genre column
  myvars <- names(Binned.Director) %in% c(names(Kth_popular.Director), "Title")  
  
  Top_Kth_Binned.Director <- Binned.Director[myvars]
  Other_Binned.Director <- Binned.Director[!myvars]
  
  #make them numeric
  Other_Binned.Director <- as.data.frame(sapply(Other_Binned.Director,function(x) as.numeric(x)))
  
  #sum of row
  SumOfDirectors <- rowSums(Other_Binned.Director, na.rm = TRUE)
  
  Other_Binned.Director[,"Other"] <- SumOfDirectors
  Other_Binned.Director[,"Other"] <- sapply(Other_Binned.Director$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Director <- add_column(Other_Binned.Director, Top_Kth_Binned.Director$Title, .after = 1)
  
  Top_Kth_Binned.Director[,"Other"] <- Other_Binned.Director$Other
  colnames(Top_Kth_Binned.Director) <- paste("Is_Director", colnames(Top_Kth_Binned.Director), sep = "_")
  
  
  Kth_popular.Director_df <- data.frame(Director = names(Kth_popular.Director) , Percentage = Kth_popular.Director)
  #Graph: Top Kth popular director
  ggplot(Kth_popular.Director_df, aes(reorder(Director, Percentage), Percentage)) +
    geom_bar(stat = "identity", position = "dodge",  fill = "#27CC99") +
    geom_text(aes(y = Percentage, label = scales::percent(Percentage)), hjust = 1.2) +
    coord_flip() + xlab("Director")
  
  return(Top_Kth_Binned.Director)
}

movies_sub <- df[c("Title","Director")]
Top_Kth_Binned.Director <- GetTopKthDirector(movies_sub, Kth_popular.Value)

#--------------------------------------------Writer----------------------------------------------

GetTopKthWriter <- function(movies_sub, Kth_popular.Value ) {
  
  #lets get rid off string inside the parenthesis
  movies_sub$Writer <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Writer))
  movies_sub$Writer <- strsplit(movies_sub$Writer,"(\\s)?,(\\s)?")
  
  Binned.Writer <- dcast(unnest(movies_sub), ... ~Writer, fun.aggregate = length)
  
  ## track of the unique genres
  all_Writers <- unique(unlist(movies_sub$Writer))
  
  Kth_popular.Writer <- head(sort(colSums(Binned.Writer[all_Writers]), decreasing=T), Kth_popular.Value)/ nrow(movies_sub)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Writer) %in% c(names(Kth_popular.Writer), "Title")  
  
  Top_Kth_Binned.Writer <- Binned.Writer[myvars]
  Other_Binned.Writer <- Binned.Writer[!myvars]
  
  #make them numeric
  Other_Binned.Writer <- as.data.frame(sapply(Other_Binned.Writer,function(x) as.numeric(x)))
  
  #sum of row
  SumOfWriters <- rowSums(Other_Binned.Writer, na.rm = TRUE)
  
  Other_Binned.Writer[,"Other"] <- SumOfWriters
  Other_Binned.Writer[,"Other"] <- sapply(Other_Binned.Writer$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Writer <- add_column(Other_Binned.Writer, Top_Kth_Binned.Writer$Title, .after = 1)
  
  Top_Kth_Binned.Writer[,"Other"] <- Other_Binned.Writer$Other
  colnames(Top_Kth_Binned.Writer) <- paste("Is_Writer", colnames(Top_Kth_Binned.Writer), sep = "_")
  
  return(Top_Kth_Binned.Writer)
}

movies_sub <- df[c("Title","Writer")]
Top_Kth_Binned.Writer <- GetTopKthWriter(movies_sub, Kth_popular.Value)

#--------------------------------------------Actors----------------------------------------------
GetTopKthActor <- function(movies_sub, Kth_popular.Value ) {
  
  #lets get rid off string inside the parenthesis
  movies_sub$Actors <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Actors))
  movies_sub$Actors <- strsplit(movies_sub$Actors,"(\\s)?,(\\s)?")
  
  Binned.Actors <- dcast(unnest(movies_sub), ... ~Actors, fun.aggregate = length)
  
  ## track of the unique genres
  all_Actors <- unique(unlist(movies_sub$Actors))
  
  Kth_popular.Actors <- head(sort(colSums(Binned.Actors[all_Actors]), decreasing=T), Kth_popular.Value)/ nrow(movies_sub)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Actors) %in% c(names(Kth_popular.Actors), "Title")  
  
  Top_Kth_Binned.Actors <- Binned.Actors[myvars]
  Other_Binned.Actors <- Binned.Actors[!myvars]
  
  #make them numeric
  Other_Binned.Actors <- as.data.frame(sapply(Other_Binned.Actors,function(x) as.numeric(x)))
  
  #sum of row
  SumOfActors <- rowSums(Other_Binned.Actors, na.rm = TRUE)
  
  Other_Binned.Actors[,"Other"] <- SumOfActors
  Other_Binned.Actors[,"Other"] <- sapply(Other_Binned.Actors$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Actors <- add_column(Other_Binned.Actors, Top_Kth_Binned.Actors$Title, .after = 1)
  
  Top_Kth_Binned.Actors[,"Other"] <- Other_Binned.Actors$Other
  colnames(Top_Kth_Binned.Actors) <- paste("Is_Actor", colnames(Top_Kth_Binned.Actors), sep = "_")
  
  # Kth_popular.Actors_df <- data.frame(Actors = names(Kth_popular.Actors) , Percentage = Kth_popular.Actors)
  # #Graph: Top Kth popular director
  # ggplot(Kth_popular.Actors_df, aes(reorder(Actors, Percentage), Percentage)) +
  #   geom_bar(stat = "identity", position = "dodge",  fill = "#27CC99") +
  #   geom_text(aes(y = Percentage, label = scales::percent(Percentage)), hjust = 1.2) +
  #   coord_flip() + xlab("Actors")
  
  return(Top_Kth_Binned.Actors)
}

movies_sub <- df[c("Title","Actors")]
Top_Kth_Binned.Actors <- GetTopKthActor(movies_sub, Kth_popular.Value)

#--------------------------------------------Language----------------------------------------------
GetTopKthLanguages <- function(movies_sub, Kth_popular.Value ) {
  
  #lets get rid off string inside the parenthesis
  movies_sub$Language <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Language))
  movies_sub$Language <- strsplit(movies_sub$Language,"(\\s)?,(\\s)?")
  
  Binned.Language <- dcast(unnest(movies_sub), ... ~Language, fun.aggregate = length)
  
  ## track of the unique genres
  all_Language <- unique(unlist(movies_sub$Language))
  
  Kth_popular.Language <- head(sort(colSums(Binned.Language[all_Language]), decreasing=T), Kth_popular.Value)/ nrow(movies_sub)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Language) %in% c(names(Kth_popular.Language), "Title")  
  
  Top_Kth_Binned.Language <- Binned.Language[myvars]
  Other_Binned.Language <- Binned.Language[!myvars]
  
  #make them numeric
  Other_Binned.Language <- as.data.frame(sapply(Other_Binned.Language,function(x) as.numeric(x)))
  
  #sum of row
  SumOfLanguage <- rowSums(Other_Binned.Language, na.rm = TRUE)
  
  Other_Binned.Language[,"Other"] <- SumOfLanguage
  Other_Binned.Language[,"Other"] <- sapply(Other_Binned.Language$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Language <- add_column(Other_Binned.Language, Top_Kth_Binned.Language$Title, .after = 1)
  
  Top_Kth_Binned.Language[,"Other"] <- Other_Binned.Language$Other
  colnames(Top_Kth_Binned.Language) <- paste("Is_Language", colnames(Top_Kth_Binned.Language), sep = "_")
  
  Kth_popular.Language_df <- data.frame(Language = names(Kth_popular.Language) , Percentage = Kth_popular.Language)
  #Graph: Top Kth popular director
  ggplot(Kth_popular.Language_df, aes(reorder(Language, Percentage), Percentage)) +
    geom_bar(stat = "identity", position = "dodge",  fill = "#27CC99") +
    geom_text(aes(y = Percentage, label = scales::percent(Percentage)), hjust = 1.2) +
    coord_flip() + xlab("Language")
  
  return(Top_Kth_Binned.Language)
}

movies_sub <- df[c("Title","Language")]
Kth.Value <- 3
Top_Kth_Binned.Languages <- GetTopKthLanguages(movies_sub, Kth.Value)

#--------------------------------------------country----------------------------------------------

GetTopKthCountry <- function(movies_sub, Kth_popular.Value ) {
  
  #lets get rid off string inside the parenthesis
  movies_sub$Country <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Country))
  movies_sub$Country <- strsplit(movies_sub$Country,"(\\s)?,(\\s)?")
  
  Binned.Country <- dcast(unnest(movies_sub), ... ~Country, fun.aggregate = length)
  
  ## track of the unique genres
  all_Country <- unique(unlist(movies_sub$Country))
  
  Kth_popular.Country <- head(sort(colSums(Binned.Country[all_Country]), decreasing=T), Kth_popular.Value)/ nrow(movies_sub)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Country) %in% c(names(Kth_popular.Country), "Title")  
  
  Top_Kth_Binned.Country <- Binned.Country[myvars]
  Other_Binned.Country <- Binned.Country[!myvars]
  
  #make them numeric
  Other_Binned.Country <- as.data.frame(sapply(Other_Binned.Country,function(x) as.numeric(x)))
  
  #sum of row
  SumOfCountry <- rowSums(Other_Binned.Country, na.rm = TRUE)
  
  Other_Binned.Country[,"Other"] <- SumOfCountry
  Other_Binned.Country[,"Other"] <- sapply(Other_Binned.Country$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Country <- add_column(Other_Binned.Country, Top_Kth_Binned.Country$Title, .after = 1)
  
  Top_Kth_Binned.Country[,"Other"] <- Other_Binned.Country$Other
  colnames(Top_Kth_Binned.Country) <- paste("Is_Country", colnames(Top_Kth_Binned.Country), sep = "_")
  
  Kth_popular.Country_df <- data.frame(Country = names(Kth_popular.Country) , Percentage = Kth_popular.Country)
  #Graph: Top Kth popular director
  ggplot(Kth_popular.Country_df, aes(reorder(Country, Percentage), Percentage)) +
    geom_bar(stat = "identity", position = "dodge",  fill = "#27CC99") +
    geom_text(aes(y = Percentage, label = scales::percent(Percentage)), hjust = 1.2) +
    coord_flip() + xlab("Country")
  
  return(Top_Kth_Binned.Country)
}

movies_sub <- df[c("Title","Country")]
Kth.Value <- 3
Top_Kth_Binned.Country <- GetTopKthCountry(movies_sub, Kth.Value)

#--------------------------------------------Production----------------------------------------------
GetTopKthProduction <- function(movies_sub, Kth_popular.Value ) {
  
  #lets get rid off string inside the parenthesis
  movies_sub$Production <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Production))
  movies_sub$Production <- strsplit(movies_sub$Production,"(\\s)?,(\\s)?")
  
  Binned.Production <- dcast(unnest(movies_sub), ... ~Production, fun.aggregate = length)
  
  ## track of the unique genres
  all_Production <- unique(unlist(movies_sub$Production))
  
  Kth_popular.Production <- head(sort(colSums(Binned.Production[all_Production]), decreasing=T), Kth_popular.Value)/ nrow(movies_sub)
  
  #Select only kth popular genre column
  myvars <- names(Binned.Production) %in% c(names(Kth_popular.Production), "Title")  
  
  Top_Kth_Binned.Production <- Binned.Production[myvars]
  Other_Binned.Production <- Binned.Production[!myvars]
  
  #make them numeric
  Other_Binned.Production <- as.data.frame(sapply(Other_Binned.Production,function(x) as.numeric(x)))
  
  #sum of row
  SumOfProduction <- rowSums(Other_Binned.Production, na.rm = TRUE)
  
  Other_Binned.Production[,"Other"] <- SumOfProduction
  Other_Binned.Production[,"Other"] <- sapply(Other_Binned.Production$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.Production <- add_column(Other_Binned.Production, Top_Kth_Binned.Production$Title, .after = 1)
  
  Top_Kth_Binned.Production[,"Other"] <- Other_Binned.Production$Other
  colnames(Top_Kth_Binned.Production) <- paste("Is_Production", colnames(Top_Kth_Binned.Production), sep = "_")
  
  # Kth_popular.Production_df <- data.frame(Production = names(Kth_popular.Production) , Percentage = Kth_popular.Production)
  # #Graph: Top Kth popular director
  # ggplot(Kth_popular.Production_df, aes(reorder(Production, Percentage), Percentage)) +
  #   geom_bar(stat = "identity", position = "dodge",  fill = "#27CC99") +
  #   geom_text(aes(y = Percentage, label = scales::percent(Percentage)), hjust = 1.2) +
  #   coord_flip() + xlab("Production")
  
  return(Top_Kth_Binned.Production)
}

movies_sub <-as.data.frame( df[c("Title","Production")])
Kth.Value <- 10

Top_Kth_Binned.Production <- GetTopKthProduction(movies_sub, Kth.Value)

#--------------------------------------------Plot----------------------------------------------
# It doesnt make sense to add Plot variable because I cant think oof any relationship between profit and plot

#all categorical binned variables
AllCAtegoricalVar.df <- merge.all(Top_Kth_Binned.Genre, Top_Kth_Binned.Director, 
                                  Top_Kth_Binned.Writer, Top_Kth_Binned.Actors, 
                                  Top_Kth_Binned.Languages, Top_Kth_Binned.Country, 
                                  Top_Kth_Binned.Production)

# rename col name
names(AllCAtegoricalVar.df)[1] <-paste("X")
names(AllCAtegoricalVar.df)

Final_Q3result <- AllCAtegoricalVar.df[, -grep("_Title", colnames(AllCAtegoricalVar.df))]
names(Final_Q3result)[1] <-paste("Title")
names(Final_Q3result)

#--------------------------------------------Q3:alternative using lasso----------------------------------

FindKthPopular <- function(dataframe1) {
  Y <- dataframe1$Profit
  X <- Matrix(as.matrix(dataframe1[!names(dataframe1) %in% c("Title", "Profit")]), sparse = TRUE)
  
  # get the train and test set (70% for train and 30% for test)
  samp_train <- sample(1:nrow(dataframe1), round(0.70 * nrow(dataframe1)))
  
  X_train <- X[samp_train,]
  Y_train <- Y[samp_train]
  
  X_test <- X[-samp_train,]
  Y_test <- Y[-samp_train]
  
  cvfit <- cv.glmnet(X_train,  ## inputs
                     Y_train,  ## target
                     family = "gaussian",   ## linear regression
                     alpha = 1,   ## select Lasso
                     type.measure = "mse",  ## train to minimize mse
                     nfolds = 10)   ## 10-folds cross-validation
  
  # Lets convert the coefficients found by LASSO into a dataframe
  coef_df <- as.data.frame(as.matrix(coef(cvfit, s = "lambda.min")))
  names(coef_df) <- c("coef") 
  coef_df$variables <- row.names(coef_df)
  
  coef_df1 <-  coef_df[order(coef_df$coef, decreasing = TRUE),]
  coef_df1 <- coef_df1[coef_df1$coef != 0, ]
  return(as.data.frame(coef_df1))
}

GetTopKth_variable <- function(df_popular, df_variable, prefix) {
  
  #Select only kth popular genre column
  myvars <- names(df_variable) %in% c((df_popular), "Title")  
  
  Top_Kth_Binned.df <- df_variable[myvars]
  Other_Binned.df <- df_variable[!myvars]
  
  #make them numeric
  Other_Binned.df <- as.data.frame(sapply(Other_Binned.df,function(x) as.numeric(x)))
  
  #sum of row
  SumOfRow <- rowSums(Other_Binned.df, na.rm = TRUE)
  
  Other_Binned.df[,"Other"] <- SumOfRow
  Other_Binned.df[,"Other"] <- sapply(Other_Binned.df$Other,function(x) ifelse(x >= 1,1,0))
  
  #add title column
  Other_Binned.df <- add_column(Other_Binned.df, df_variable$Title, .after = 1)
  
  Top_Kth_Binned.df[,"Other"] <- Other_Binned.df$Other
  colnames(Top_Kth_Binned.df) <- paste(prefix, colnames(Top_Kth_Binned.df), sep = "_")
  
  return(Top_Kth_Binned.df)
}

k <- 250
#--------------------------------------------Director----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Director")]
movies_sub$Director <- strsplit(movies_sub$Director,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Director <- paste0("Director_", gsub("\\s","_",unnested$Director))
movies_sub <- dcast(unnested, ... ~ Director, fun.aggregate = length)

df.Kth_Director <- FindKthPopular(as.data.frame(movies_sub))

#get rid off intecept
df.Kth_Director<- df.Kth_Director[!grepl("(Intercept)", df.Kth_Director$variables),]

Kth_popular.Director <- head(df.Kth_Director$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Director <- GetTopKth_variable(Kth_popular.Director, movies_sub, "Is_")

#--------------------------------------------genre#----------------------------------------------
movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Genre")]
movies_sub$Genre <- strsplit(movies_sub$Genre,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Genre <- paste0("Genre_", gsub("\\s","_",unnested$Genre))
movies_sub <- dcast(unnested, ... ~ Genre, fun.aggregate = length)

df.Kth_Genre <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Genre<- df.Kth_Genre[!grepl("(Intercept)", df.Kth_Genre$variables),]

Kth_popular.Genre <- head(df.Kth_Genre$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Genre <- GetTopKth_variable(Kth_popular.Genre, movies_sub, "Is_")

#--------------------------------------------Writer#----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Writer")]
#lets get rid off string inside the parenthesis
movies_sub$Writer <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Writer))
movies_sub$Writer <- strsplit(movies_sub$Writer,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Writer <- paste0("Writer_", gsub("\\s","_",unnested$Writer))
movies_sub <- dcast(unnested, ... ~ Writer, fun.aggregate = length)
df.Kth_Writer <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Writer<- df.Kth_Writer[!grepl("(Intercept)", df.Kth_Writer$variables),]

Kth_popular.Writer <- head(df.Kth_Writer$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Writer <- GetTopKth_variable(Kth_popular.Writer, movies_sub, "Is_")


#--------------------------------------------Actors----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Actors")]
#lets get rid off string inside the parenthesis
movies_sub$Actors <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Actors))
movies_sub$Actors <- strsplit(movies_sub$Actors,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Actors <- paste0("Actors_", gsub("\\s","_",unnested$Actors))
movies_sub <- dcast(unnested, ... ~ Actors, fun.aggregate = length)
df.Kth_Actors <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Actors<- df.Kth_Actors[!grepl("(Intercept)", df.Kth_Actors$variables),]

Kth_popular.Actors <- head(df.Kth_Actors$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Actors <- GetTopKth_variable(Kth_popular.Actors, movies_sub, "Is_")


#--------------------------------------------Language#----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Language")]
#lets get rid off string inside the parenthesis
movies_sub$Language <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Language))
movies_sub$Language <- strsplit(movies_sub$Language,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Language <- paste0("Language_", gsub("\\s","_",unnested$Language))
movies_sub <- dcast(unnested, ... ~ Language, fun.aggregate = length)
df.Kth_Language <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Language<- df.Kth_Language[!grepl("(Intercept)", df.Kth_Language$variables),]

Kth_popular.Language <- head(df.Kth_Language$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Language <- GetTopKth_variable(Kth_popular.Language, movies_sub, "Is_")

#--------------------------------------------Country#----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Country")]
#lets get rid off string inside the parenthesis
movies_sub$Country <-gsub("\\s*\\([^\\)]+\\)","",as.character(movies_sub$Country))
movies_sub$Country <- strsplit(movies_sub$Country,"(\\s)?,(\\s)?")
unnested <- unnest(movies_sub)
unnested$Country <- paste0("Country_", gsub("\\s","_",unnested$Country))
movies_sub <- dcast(unnested, ... ~ Country, fun.aggregate = length)
df.Kth_Country <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Country<- df.Kth_Country[!grepl("(Intercept)", df.Kth_Country$variables),]

Kth_popular.Country <- head(df.Kth_Country$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Country <- GetTopKth_variable(Kth_popular.Country, movies_sub, "Is_")

#--------------------------------------------Production#----------------------------------------------

movies_sub <- NA
movies_sub <- df[c("Title", "Profit", "Production")]
movies_sub$Production <- strsplit(movies_sub$Production,"/")
unnested <- unnest(movies_sub)
unnested$Production <- paste0("Production_", gsub("\\s","_",unnested$Production))
movies_sub <- dcast(unnested, ... ~ Production, fun.aggregate = length)

df.Kth_Production <- FindKthPopular(movies_sub) 

#get rid off intecept
df.Kth_Production<- df.Kth_Production[!grepl("(Intercept)", df.Kth_Production$variables),]

Kth_popular.Production <- head(df.Kth_Production$variables, k)
movies_sub <- subset(movies_sub, select = -c(Profit)) #remove profit col
Top_Kth_Binned.Production <- GetTopKth_variable(Kth_popular.Production, movies_sub, "Is_")


#------------------------------------------------------------------------------------------

# combine all categorical binned variables
AllCAtegoricalVar.df_lasso <- merge.all(Top_Kth_Binned.Genre, Top_Kth_Binned.Director, 
                                        Top_Kth_Binned.Writer, Top_Kth_Binned.Actors, 
                                        Top_Kth_Binned.Language, Top_Kth_Binned.Country, 
                                        Top_Kth_Binned.Production)

# rename col name
names(AllCAtegoricalVar.df_lasso)[1] <-paste("X")

Final_Q3result_lasso <- AllCAtegoricalVar.df_lasso[, -grep("_Title", colnames(AllCAtegoricalVar.df_lasso))]
names(Final_Q3result_lasso)[1] <-paste("Title")

names(Final_Q3result_lasso)
dim(Final_Q3result_lasso)

#---------------------------------------------Question 4-------------------------------------------------

#--------------------using question3 df--------------------
df4 <- Final_Q3result
df4["Profit"] <- mydf$Profit

df_4 <- df4[2:ncol(df4)]
names(df_4)
dim(df_4)
#making sure all columns are numeric
for(i in seq(ncol(df_4))) {
  df_4[,i] <- as.numeric(as.character(df_4[,i]))
}

#linear model
Model4 <- LinearRegression_ForAllSpilt(df_4)
df_Data <- as.data.frame(Model4)
plotGraph(df1, df_Data, "Figure:4.1- top kth popular non-numeric variables")


#--------------------using question3 using lASSO--------------------

df4.lASSO <- Final_Q3result_lasso
df4.lASSO["Profit"] <- mydf$Profit

df_4.lASSO <- df4.lASSO[2:ncol(df4.lASSO)]
names(df_4.lASSO)
dim(df_4.lASSO)
#making sure all columns are numeric
for(i in seq(ncol(df_4.lASSO))) {
  df_4.lASSO[,i] <- as.numeric(as.character(df_4.lASSO[,i]))
}

#linear model
Model4.lasso <- LinearRegression_ForAllSpilt(df_4.lASSO)
df.lasso_Data <- as.data.frame(Model4.lasso)
plotGraph(df1, df.lasso_Data, "Figure:4.2- top kth popular non-numeric variables using lasso")


#---------------------------------------------Question 5-------------------------------------------------

# combine all categorical variables and all binned numeric variables
AllData.df <- merge.all(df_4,All.binned.Numeric.df )

#Attempt1: all binned numeric variables and  non numeric data are used.
df5_result_binned <- as.data.frame(LinearRegression_ForAllSpilt(AllData.df))
plotGraph(df1, df5_result_binned, "Figure:5.1- Binned numeric variables and Categorical data")
#
#
#
#
# combine all categorical variables and all log numeric variables
AllData.df <- merge.all(df_4.lASSO,All.Log.Numeric.df )

#Attempt2: all log numeric variables and  non numeric data are used.
df5_result_log <- as.data.frame(LinearRegression_ForAllSpilt(AllData.df))
plotGraph(df1, df5_result_log, "5.2:log numeric and Categorical data Using LASSO")
#
#
#
#
# combine all categorical variables and all log numeric variables
AllData.df <- merge.all(df_4.lASSO,All.Squared.Numeric.df )

#Attempt3: all Squared numeric variables and lasso non numeric data are used.
df5_result_Squared <- as.data.frame(LinearRegression_ForAllSpilt(AllData.df))
plotGraph(df1, df5_result_Squared, "Figure:5.3- Squared numeric variables and lasso Categorical data")
plotGraph(log(df1), log(df5_result_Squared), "5.3- Squared numeric & Categorical data in Log form")
#
#
#
#
#
#
#
#
All_CombinedData.df <- merge.all(df_4,All.Squared.Numeric.df,All.Log.Numeric.df,All.binned.Numeric.df, df_2)
names(All_CombinedData.df)


#Attempt3: Selecting signifiant variables by checking for p-Values
inputData <- subset(All_CombinedData.df, select = -c(Profit) )
significantPreds_cont <- character()  # initialise output. Significant preds will accrue here
response <- All_CombinedData.df$Profit

for (predName in names(inputData)) {
  
  pred <-as.matrix( inputData[predName])
  mod <- lm(response ~ pred,data = inputData)  # build linear model with only current predictor
  p_value <- summary(mod)$coefficients[, 4][2]  # capture p-Value
  
  if (!is.na(p_value)) {
    if (p_value < 0.1 & p_value > 0) {  # check for significance
      significantPreds_cont <- c (significantPreds_cont, predName)  # if selected, bind the predictor name if main output
    }
  }
  
}

inputData_cont_signif <- inputData[, names(inputData) %in% significantPreds_cont]  # filter selected 
inputData_cont_signif["Profit"] <- All_CombinedData.df$Profit

df5_result <- as.data.frame(LinearRegression_ForAllSpilt(inputData_cont_signif))
plotGraph(df1, df5_result, "5.4 - Linear regression on signifiant variables only")
#
#
#
#
#
#
#
#
# Decide if a variable is important or not using Boruta

inputData <-(All_CombinedData.df)
boruta_output <- Boruta(response ~ ., data=na.omit(inputData), doTrace=2)  # Boruta search

# collect Confirmed and Tentative variables
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed")]) 

boruta_signif <- gsub('\\`', '', boruta_signif)

dfToRun <- subset(All_CombinedData.df, select = c(boruta_signif))

#Attempt6: Getting only variables that are important by boruta
df5_result_Boruta <- as.data.frame(LinearRegression_ForAllSpilt(dfToRun))
plotGraph(df1, df5_result_Boruta, "Figure:5.5- Linear regression on signifiant variables only using Boruta")





