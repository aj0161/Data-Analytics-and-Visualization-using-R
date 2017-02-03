#Namne: Ajay Joshi 

########################################################Prerequisites############################################################

Needed <- c( "ggplot2", "tm","NLP", "SnowballC", "robustHD", "lubridate", "reshape","digest", "stringr", "knitr", "GGally") 
#install.packages(Needed, dependencies = TRUE)

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

#Global
df.GenreWideData <- NA #this data frame is calculated in Question 5

# get the data from the url source
load(url("https://s3.amazonaws.com/content.udacity-data.com/courses/gt-cs6262/project/movies_merged"))

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

#Numeric check
IsNumeric <- function(x) {
  is.numeric(x) & !is.na(x)
}

# Trimmer
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Data Mining code requires Tm library
Data_Mining <- function( df) {
  
  val = df$Genre
  
  my.corpus = Corpus(VectorSource(val))
  
  # cleaning data 
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, PlainTextDocument)
  
  tm_map(my.corpus, stemDocument)
  
  dtm <- DocumentTermMatrix(my.corpus)
  
  title <- df$Title
  Genre <- df$Genre
  Gross <- df$Gross
  
  res<- data.frame(title, Genre,inspect(dtm), Gross)
  return(res)
}

GetTop10Genres <- function(df) {
  
  #perform data mining
  newData <- Data_Mining(df)
  
  #Check how many times each of the genres used for movie title. TODO: make better naming convention
  Mov1 <- length( newData$title[newData$action == 1])
  Mov2 <-length( newData$title[newData$adult == 1])
  Mov3 <-length(newData$title[newData$adventure == 1])
  Mov4 <-length( newData$title[newData$animation == 1])
  Mov5 <- length(newData$title[newData$biography == 1])
  Mov6 <- length(newData$title[newData$comedy == 1])
  Mov7 <- length(newData$title[newData$crime == 1])
  Mov8 <- length(newData$title[newData$documentary == 1])
  Mov9 <- length(newData$title[newData$drama == 1])
  Mov10 <-length( newData$title[newData$family == 1])
  Mov11 <-length( newData$title[newData$fantasy == 1])
  Mov12 <- length(newData$title[newData$filmnoir == 1])
  Mov14 <-length( newData$title[newData$history == 1])
  Mov15 <- length(newData$title[newData$horror == 1])
  Mov16 <- length(newData$title[newData$music == 1])
  Mov17 <- length(newData$title[newData$musical == 1])
  Mov18 <- length(newData$title[newData$mystery == 1])
  Mov19 <-length( newData$title[newData$news == 1])
  Mov21 <- length(newData$title[newData$romance == 1])
  Mov22 <- length(newData$title[newData$scifi == 1])
  Mov23 <- length(newData$title[newData$short == 1])
  Mov24 <- length(newData$title[newData$sport == 1])
  Mov26 <- length(newData$title[newData$thriller == 1])
  Mov27 <-length( newData$title[newData$war == 1])
  Mov28<- length(newData$title[newData$western == 1])
  
  MoviesCountWithGenreType <- c(Mov1,Mov2,Mov3,Mov4,Mov5,Mov6,Mov7,Mov8,Mov9,Mov10,Mov11,Mov12,Mov14,Mov15
                                ,Mov16,Mov17,Mov18,Mov19,
                                Mov21,Mov22,Mov23,Mov24,Mov26,Mov27,Mov28)
  
  #List of all possible genres from data: df.WithoutNA_GenreAndGross
  AllPossibleGenres <- sort.default(unique(trim(unlist(df$Genre))))
  
  df.TileAndGenresAndGross <- data.frame(AllPossibleGenres, MoviesCountWithGenreType)
  
  #Just get sort and filter out to top 10 genres
  df.TileAndGenres_Top10MostUsedGenre <-head(df.TileAndGenresAndGross[order(-MoviesCountWithGenreType),]  ,-15)
  
  top10Genres <- df.TileAndGenres_Top10MostUsedGenre$AllPossibleGenres
  return(top10Genres)
}

########################################################---Prerequisites END---##############################

#Question 1--------------------------------------------------------------------------------------------------
#  #Remove all rows that do not correspond to movies. How many rows did you remove?
# --->798 rows has been removed.

#Generates the data with movies 
df.MoviesType11 <- movies_merged[movies_merged$Type == "movie",]

#Question 2--------------------------------------------------------------------------------------------------

# Data soruce
df <- movies_merged

#replace "N/A" to NA 
df$Runtime <- sapply(df$Runtime,function(x) ifelse(x== "N/A",NA,x))

#convert Runtime character to numeric
df$Runtime <- sapply(as.character(df$Runtime), TimeToMinutes)

#Check Runtime values are numeric. 
IsNumeric(df$Runtime)

#graph:Figure:2.1 Year Vs Runtime
ggplot(df, aes(x=df$Year, df$Runtime)) + 
  geom_point(size=1, shape=10) +
  xlab("Year") + 
  ylab("Runtime") +
  ylim(0, 300) + #  set the limits of the Y axis
  #scale_y_continuous(breaks = seq(0, 425, by = 25)) +
  scale_x_continuous(breaks = seq(1888, 2018, by = 5))  +
  ggtitle("Figure:2.1 Year Vs Runtime")

#Investigation
## The figure 2.1 shows that the runtime values are the lowest from 1888 till around 1912 and there's gradual increase of runtime from a certain point (around 1925). some of the high runtime movies are made in the range of 2010-2018.

#graph:Figure:2.2 Budget Vs Runtime
ggplot(df, aes(x=df$Budget, df$Runtime)) + 
  geom_point(size=1, shape=10) +
  xlab("Budget") + 
  ylab("Runtime") +
  scale_y_continuous(breaks = seq(0, 1000, by = 50)) +
  scale_x_continuous(breaks = seq(0, 425000000, by = 50000000))  +
  ggtitle("Figure:2.2 Budget Vs Runtime")

# According to the figure 2.2, the runtime over the budget of the movie is relatively linear.

#Question 3--------------------------------------------------------------------------------------------------

my.docs = movies_merged$Genre

#remove Genre with NA
df.GenreWithoutNA <- movies_merged[movies_merged$Genre != "N/A",]

my.corpus = Corpus(VectorSource(df.GenreWithoutNA$Genre))

# cleaning data 
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, PlainTextDocument)

tm_map(my.corpus, stemDocument)

#create the dictionary
dtm <- DocumentTermMatrix(my.corpus)

title <- df.GenreWithoutNA$Title
Genre <- df.GenreWithoutNA$Genre
Gross <- df.GenreWithoutNA$Gross
releaseDate <- df.GenreWithoutNA$Released

# data frame with dicitionary
Df_binaryVectorGenre <-  data.frame(title,Gross,releaseDate,Genre,inspect(dtm))

# make this dataframe so that others can use it.
df.GenreWideData <- Df_binaryVectorGenre

######################################top 10 genres vs Title######################################

#List of all genres
uniqueGenres <- sort.default(unique(trim(unlist(strsplit(df.GenreWithoutNA$Genre, "[,]")))))

#Check how many times each of the genres used for movie title. TODO: make better naming convention
Mov1 <- length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$action == 1])
Mov2 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$adult == 1])
Mov3 <-length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$adventure == 1])
Mov4 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$animation == 1])
Mov5 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$biography == 1])
Mov6 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$comedy == 1])
Mov7 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$crime == 1])
Mov8 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$documentary == 1])
Mov9 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$drama == 1])
Mov10 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$family == 1])
Mov11 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$fantasy == 1])
Mov12 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$filmnoir == 1])
Mov13 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$gameshow == 1])
Mov14 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$history == 1])
Mov15 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$horror == 1])
Mov16 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$music == 1])
Mov17 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$musical == 1])
Mov18 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$mystery == 1])
Mov19 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$news == 1])
Mov20 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$realitytv == 1])
Mov21 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$romance == 1])
Mov22 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$scifi == 1])
Mov23 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$short == 1])
Mov24 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$sport == 1])
Mov25 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$talkshow == 1])
Mov26 <- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$thriller == 1])
Mov27 <-length( Df_binaryVectorGenre$title[Df_binaryVectorGenre$war == 1])
Mov28<- length(Df_binaryVectorGenre$title[Df_binaryVectorGenre$western == 1])

MoviesCountWithGenreType <- c(Mov1,Mov2,Mov3,Mov4,Mov5,Mov6,Mov7,Mov8,Mov9,Mov10,Mov11,Mov12,Mov13,Mov14,Mov15
                              ,Mov16,Mov17,Mov18,Mov19,Mov20,
                              Mov21,Mov22,Mov23,Mov24,Mov25,Mov26,Mov27,Mov28)


df.TileAndGenres <- data.frame(uniqueGenres, MoviesCountWithGenreType)

#change column names
colnames(df.TileAndGenres)[1] <- "Top 10 Genre"
colnames(df.TileAndGenres)[2] <- "# of title"

# sort by Most used Genre
df.TileAndGenres_Top10MostUsedGenre <- df.TileAndGenres[order(-MoviesCountWithGenreType),] 

#Just get top 10 genres
df.TileAndGenres_Top10MostUsedGenre <-head(df.TileAndGenres_Top10MostUsedGenre,-18)

#Figure: 3.1-Use bar plot [Top 10 Genre Vs Count of Title]
ggplot(df.TileAndGenres_Top10MostUsedGenre, aes(x =df.TileAndGenres_Top10MostUsedGenre$uniqueGenres , y = df.TileAndGenres_Top10MostUsedGenre$MoviesCountWithGenreType, fill=uniqueGenres )) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="Top 10 Genre",y="Count of Title") +
  ggtitle("Figure: 3.1 Top 10 Genre Vs Count of Title") 

#Observation
#According to the figure 3.1, Drama and comedy movies have highest nuumber of title count wheareas animation, adventure, and documentary genred movies have lowest title count.


#####################################------top 10 genres vs Title vs Gross------#############################

#filter out the rows with NA Genres and Gross
df.WithoutNA_GenreAndGross <- df.GenreWithoutNA[!is.na(df.GenreWithoutNA$Gross),] #4580 entries

# new data frame with title, Genre, and Gross. 4580 entries
df.toProcess <- data.frame(df.WithoutNA_GenreAndGross$Title, df.WithoutNA_GenreAndGross$Gross)
df.toProcess[ , "Genre"] <- df.WithoutNA_GenreAndGross$Genre

#change column names
colnames(df.toProcess)[1] <- "Title"
colnames(df.toProcess)[2] <- "Gross"

s <- strsplit(df.toProcess$Genre, split = ",")

Title = rep(df.toProcess$Title, sapply(s, length))
Genre = unlist(s)
Gross =rep(df.toProcess$Gross, sapply(s, length))
Genre_trimmed <- trim(Genre)

df.TitleGenreGross <- data.frame(Title, Gross)
df.TitleGenreGross[ , "Genre"] <- Genre_trimmed

#Find top 10 genres in data:df.TitleGenreGross
top10Genres <- GetTop10Genres(df.TitleGenreGross)

# get Dataframe that contain top10Genres types only.
df.toplot <-lapply(top10Genres, function(n) {df.TitleGenreGross[df.TitleGenreGross$Genre == n,]})

# list to data frame
df <- do.call(rbind.data.frame, df.toplot)

winsorized.Gross <- winsorize(df$Gross)

#Figure: 3.2  
ggplot(df, aes(reorder(df$Genre,-winsorized.Gross, median),winsorized.Gross, fill=Genre )) +
  geom_boxplot() + 
  labs(x="Gross",y="Top 10 Genre") +
  scale_y_continuous(breaks = seq(1, 3000000000, by = 10000000)) +
  ggtitle("The distribution of gross revenue across top 10 genre")

#observation
#According to the figure 3.2, the highest median value is adventure genre and the lowest is drama. Action, adventure, and Sci-fi genres have approx. maximum values but the least mimimum values among the three is of Sci-fi.

#Question 4--------------------------------------------------------------------------------------------------

x <- movies_merged

#I think, the year is the first released date and the released column is the release date for some region/country. 
#Using “Date” or another data columns will be more appropriate to analyze the mismatch. 
#If there's the merge from two different sources based on title, its likely to see “title” with slightly 
#different names displays on multiple rows but the same “Released” and “Year” but it doesnt happen on the data provided.
#Hence, there’s no discrepancy between "Year" and "Release".  


##############################Alternative###############################################################################################

#My attempt to find the mismatch
x <- movies_merged
t <- gsub("\\-.*","",x$Released)

ReleaseNumeric <- as.numeric(t)
YearNumeric <- as.numeric(x$Year)

ReleaseNumeric[is.na(ReleaseNumeric)] <- -1

Compare.YearAndRelease <- YearNumeric == ReleaseNumeric
newData <- data.frame( x$Title, x$Gross, x$Date, YearNumeric, ReleaseNumeric, Compare.YearAndRelease)

#find all rows where a mismatch between these two variables
df.MactchYearAndRel <- newData[newData$Compare.YearAndRelease == TRUE, ]

df.MisMactchYearAndRel <- newData[newData$Compare.YearAndRelease != TRUE, ]

#Question 5--------------------------------------------------------------------------------------------------

#Note: Need to compile Q3 first

# Data source
x <- movies_merged

#remove Gross with 0 and NA values
Gross.WithoutNAAnd0 <- x[ !is.na(x$Gross) ,]
Gross.WithoutNAAnd0 <- Gross.WithoutNAAnd0[ Gross.WithoutNAAnd0$Gross != 0 ,] #4296 entries

#Make released data in Date format and update it on the data: Gross.WithoutNAAnd0
Released.year <- gsub("\\-.*","",Gross.WithoutNAAnd0$Released)
Gross.WithoutNAAnd0$Released <- Released.year

#remove release NA in the dataframe
Gross.WithoutNAAnd0 <- Gross.WithoutNAAnd0[!is.na(Gross.WithoutNAAnd0$Released), ] #4272 entries

#create a new df
newdata <- data.frame(Gross.WithoutNAAnd0$Title ,Gross.WithoutNAAnd0$Released, Gross.WithoutNAAnd0$Genre, Gross.WithoutNAAnd0$Gross)

#convert string to date
RelAsDate <- as.Date(newdata$Gross.WithoutNAAnd0.Released, "%Y")

# update df
newdata$Gross.WithoutNAAnd0.Released <- RelAsDate

gross <-(newdata$Gross.WithoutNAAnd0.Gross)
reldate <-(newdata$Gross.WithoutNAAnd0.Released)

# Figure:5.1  overall graph
ggplot(newdata, aes(reldate, gross)) +
  geom_point()  + 
  scale_y_continuous(breaks = seq(100000000, 2883918982, by = 100000000)) +
  scale_x_date(date_minor_breaks = "5 year") + 
  ggtitle("Figure: 5.1 Release date and Gross Revenue")

# By looking at figure 5.1 graph, roughly, we can see that the most high revenue movies released in the range of 2010 to 2020. After around 1980, there's gradual increase in movies' budget.The most high revenue movies were released in 2009. However, it is not clear enough to investigate the on what times of year are most high revenue movies released in. Figure 5.2 shows that the relations between the Gross revenue and year that is between 1994 and 2020. The reason I picked the year between 1994 and 2020 is because those years have most movies with released date and gross data. By looking at the figure 5.2,we can conclude that the most high revenue movies were usually released in and around 2015.   

#convert string release date to year format
newdata$Gross.WithoutNAAnd0.Released <- as.numeric(gsub("\\-.*","",newdata$Gross.WithoutNAAnd0.Released))

#create a new df in which release date is greater than equal to 1994
df <- newdata[newdata$Gross.WithoutNAAnd0.Released >= 1994 ,]

# sort by Gross 
#rank.value <- seq(1, length(df$Gross.WithoutNAAnd0.Gross), by=1 )
#newdata <- df[order(-df$Gross.WithoutNAAnd0.Gross),]

################-----------comment on what times of year are most high revenue movies released in----------##

#Figure:5.2  overall graph from 1994 to 2020
ggplot(newdata, aes(newdata$Gross.WithoutNAAnd0.Released, newdata$Gross.WithoutNAAnd0.Gross)) + 
  geom_point() + 
  ggtitle("Figure: 5.2 Release date from 1994 to 2020 and Gross Revenue") +
  xlab("Year") +
  ylab("Gross")

# By looking at the figure 5.2,we can conclude that the most high revenue movies were usually released in and around 2015.

################################genre-based recommendation#################################################

#Note: Need to compile Q3 first
if (is.data.frame(df.GenreWideData) && nrow(df.GenreWideData)==0) { #If Start
  print(" DF is Null. Please compile Question 3 first!!")
} else {
 
df <- Df_binaryVectorGenre

Released.Month <- month(as.POSIXlt(df$releaseDate, format="%y/%m/%d"))
Released.Month <- month.abb[Released.Month]

#add new column
df[, "ReleaseMonth"] <- Released.Month

#delete unwanted columns
df[, c( "releaseDate", "Genre")] <- list(NULL)

#move 
df <- subset(df, select=c(ReleaseMonth,title:western))

#melt
mdata <- melt(df, id= c( "title", "ReleaseMonth", "Gross"))

# remove row that has values is equal 0
df <- mdata[mdata$value != 0,]

# remove all NAs
df <-df[complete.cases(df),]

gross.mean <- mean(df$Gross)
gross.sd <- sd(df$Gross)

#oct tototal movie revenue
#test
dftestQ5 <- df[df$ReleaseMonth == "Dec", ]

#Graph: Figure 5.3: Released Month Vs Gross
ggplot(df, aes(df$ReleaseMonth, df$Gross)) + 
  geom_bar(stat="identity") + labs(x="Gross",y="Released Month") +
  ggtitle("Figure 5.3: Released Month Vs Gross")

#Observation
#By looking at the figure 5.3, the best month to release a movie is in the month of Jun, May, Julyand the worst months are sep, april, jan and feb.

#Graph: Figure 5.4: Released Month Vs Gross Vs Genre
ggplot(df, aes(df$ReleaseMonth, df$Gross)) + 
  geom_bar(stat="identity") + facet_wrap(~variable) +labs(x="Gross",y="Released Month") +
  ggtitle("Figure 5.4: Released Month Vs Gross Vs Genre")

#Observation
#Figure5.4 is a graphical representation of a genre-based recommendation for release date that is likely to increase the title’s revenue. May month is best month for action, adventure, and western genre movies.

} #If End Question


#Question6.--------------------------------------------------------------------------------------------------
df <- data.frame(movies_merged$imdbRating,movies_merged$tomatoRating)

# remove all NAs
df <-df[complete.cases(df),]

#density plot
df.ToDisplay<- melt(df)

#Figure:6.1
ggplot(df.ToDisplay,aes(x=value, fill=variable)) + geom_density(alpha=0.25) +
  ggtitle("Figure:6.1 Density plot: Imdb Rating Vs Tomato Rating")

#Difference between two densities
Density.TomatoRating<- density(df$movies_merged.imdbRating) 
Density.imdbRating<- density(df$movies_merged.tomatoRating)

#Figure:6.2
plot(Density.TomatoRating$y-Density.imdbRating$y, main ="Figure:6.2 Density Difference: Imdb Rating Vs Tomato Rating")

####################correlation between imdb rating and tomato rating######################
plot(df$movies_merged.imdbRating, df$movies_merged.tomatoRating, main=" Figure: 6.3 correlation between imdb rating and tomato rating", xlab="Imdb Rating ", ylab="Tomato Rating ", pch=19)
# Add fit lines
abline(lm(df$movies_merged.tomatoRating ~ df$movies_merged.imdbRating), col="red") # regression line (y~x) 
lines(lowess(df$movies_merged.imdbRating,df$movies_merged.tomatoRating), col="blue") # lowess line (x,y)

CorValue <- cor(df.ToDisplay,use="complete.obs") # cor value: 0.79

# Similarities and difference between Imdb rating and Rotten tomato ratings

# . IMDB calculates rating based on the average based on how people fill out ratings and to avoid "vote stuffing" (Weighted Average Ratings). On the other hand, Rotten Tomatoes is based on scores given by critics. They do not average the rating. They categorized the rating into 'Fresh' and 'Rotten'. Any movie that receive 60% reading on the Tomatometer for that movie will be considered as 'Fresh' and if a movie receives less that 60 % reading will be considered as 'Rotten'.

# Figure: 6.1 shows the relationship between the imdb rating and tomato rating. Figure 6.2 explains the difference between the density of tomato rating ang imdb rating. Figure 6.3 shows the correlation graph using scatter plot. The correlation value(r) is 0.79. if the r value is between .5 to .8, it is considered as Medium positive correlation. By the looking at those graph,  For the top rated movies, the disparity is actually not high at all.

#relationships between these variables and the gross revenue.
x <- data.frame(movies_merged$imdbRating,movies_merged$tomatoRating, movies_merged$Gross)

# remove all NAs
x <-x[complete.cases(x),]

ImdbRating <- x$movies_merged.imdbRating
TomatoRating <- x$movies_merged.tomatoRating
Gross <-winsorize(x$movies_merged.Gross)

#Graph: All ratings Vs Gross
pairs(Gross ~ ImdbRating,
      main="Figure: 6.4 Gross Vs Imdb Rating Vs Tomato Rating")

#Observation
#Most imdb raters tend to rate movies that they liked and skip to rate that dont like.By looking at the figure 6.4, it is likely to get high rating for lower grossed movies and vice versa. Ratings are more dispersed on Tomato rating than that in imdb.

#Question 7--------------------------------------------------------------------------------------------------------------------------------------------
df <-  movies_merged

#replace N\A to 0
df$Awards[df$Awards == "N/A"]<- as.numeric(0)

#remove special characters
df$Awards <- str_replace_all(df$Award, "[[:punct:]]", " ")

#trim
df$Awards <- trim(df$Awards)

#new dataframe
newdata <- data.frame(df$Title, df$Awards, df$Gross)

#regular expression
x <- sapply(newdata$df.Awards, function(x) as.numeric(str_extract_all(x,"\\(?[0-9,.]+\\)?")[[1]]))

#summation
y <- lapply(x, function(x) sum(x))

#unlist it
ans <- unlist(y)

#replace Awards column with new values:ans
newdata$df.Awards <-ans

#first component represents no nomination or awards
newdata[, "None"] <- sapply(newdata$df.Awards,function(x) ifelse(x== 0,1,0))

#the second component represents some nominations/awards
newdata[, "Some"] <- sapply(newdata$df.Awards,function(x) ifelse(x < 5 && x > 0,1,0))

#the third component represents many nominations or awards. 
#The relationship between the second and the third categories is 5 to 1 ratio
newdata[, "Many"] <- sapply(newdata$df.Awards,function(x) ifelse(x >= 5,1,0))

#convert to three dimensional vector
matrix.Awards = as.matrix(data.frame(newdata$None, newdata$Some, newdata$Many))

#Construct new dataframe with, gross and Awards (binary format)
Df.Updated <- data.frame(newdata$df.Gross,  newdata$None, newdata$Some, newdata$Many)

#----------------------Variation of Gross value across these three categories-------------------------------------------------------------

#remove NAs
Df.Updated <-Df.Updated[complete.cases(Df.Updated),]

#remove the values with gross =0
Df.Updated <- Df.Updated[Df.Updated$newdata.df.Gross != 0, ]

#Figure 7: correlation graph
ggpairs(Df.Updated)

#graph: Figure:7.1 Gross Vs None
df.GrossVsNone <- data.frame(None=Df.Updated$newdata.None ,gross= Df.Updated$newdata.df.Gross)

#figure:7.1
ggplot(df.GrossVsNone, aes(x=df.GrossVsNone$gross, y=df.GrossVsNone$None)) + geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Gross")+
  ylab("None (No nomination)") +
  theme_bw() +
  ggtitle("Figure:7.1 Gross Vs No Nomination(None)")

#1.0 and 0.0 on the Y-axis represent the true and false variable respectively whereas x-axis represent the gross revenue in the Figure 7.1. The blue regression line accross the graph expresses the relationship between the gross revenue and Y-axis variables (true and false)  . Movies with high gross revenue win some to many awards which is represented by the plot points accross the Y-axis indexes. 

#Gross Vs Some
df.GrossVsSome <- data.frame(some=Df.Updated$newdata.Some ,gross= Df.Updated$newdata.df.Gross)

#graph: Figure:7.2
 ggplot(df.GrossVsSome, aes(x=df.GrossVsSome$gross, y=df.GrossVsSome$some)) +geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Gross")+
  ylab("Some Awards") +
  theme_bw() +
  ggtitle("Figure:7.2 Gross Vs some Awards")

#1.0 and 0.0 on the Y-axis represent the true and false variable respectively whereas x-axis represent the gross revenue in the Figure 7.1. The blue regression line accross the graph expresses the relationship between the gross revenue and Y-axis variables (true and false). As the gross revenue increases beyond gross revenue value 1e+09, the movies win either many awards or no awards at all which is represented by the plot points. 

#df
df.GrossVsMany <- data.frame( Many=Df.Updated$newdata.Many ,gross= Df.Updated$newdata.df.Gross)

#graph: Figure:7.3 Gross Vs Many
ggplot(df.GrossVsMany, aes(x=df.GrossVsMany$gross, y=df.GrossVsMany$Many)) + geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Gross")+
  ylab("Many Award") +
  theme_bw() +
  ggtitle("Figure:7.3 Gross Vs Many Awards")

#1.0 and 0.0 on the Y-axis represent the true and false variable respectively whereas x-axis represent the gross revenue in the Figure 7.1. The blue regression line accross the graph expresses the relationship between the gross revenue and Y-axis variables (true and false). As the gross revenue increases approx. beyond gross revenue value 1e+09, the movies win many awards as oppose to winning some or none awards which is represented by the plot points. 


#Question 8-----------------------------------------------------------------------------------------------------------------------
#[Insight -1 ]How many movie are there that generate atleast a biliion gross revenue ? 
#[Insight -2 ]How many of these movies were released in which year? 
#Describe and investigate a relationship between the movies and its gross revenue.

#Note: Question 5 must compile first. 'newdata' dataframe is used which is generate from question 3

abillion <- 1000000000

df <- newdata[newdata$Gross.WithoutNAAnd0.Gross >= abillion, ]

dates <- df$Gross.WithoutNAAnd0.Released

date.unique <- unique(dates)

# This function return count number of matched value
MatchFinder <- function(x) {
  count <- 0 
  for (date in dates) {
    if (x == date) {
      count <- sum(count,1)
    }
  }
  return(count)
}

MovieCount <- NA
i <- 0
for (date in date.unique) {
  i <- sum(i,1)
  MovieCount[i]   <- sapply(date, MatchFinder)
}

#df.updated : This data frame show that high revenue movie are in 2015 and 2012.
df.updated <- data.frame(date.unique, MovieCount)

#There are 22 movies that grossed atleast billion dollar.

plot(df.updated$date.unique, df.updated$MovieCount, main="Figure: 8.1: Movies count Vs Year", 
     xlab="Year ", ylab="Movie count ", pch=19)


#Observation
#By looking at the figure 8.1, there are 4 movies released in 2012 and 2015 that grossed atleast or more than billion dollar. In year: 1997, 2003, 2006, 2008, 2009, 2014, 2016, each of those years, a billion dollar grossed movies were released.   
  
  
# New insight that are not expected
  
# As the movie's gross revenue goes up, the probablilty of winning atleast an award goes up as well. This expected consequence is valid for majority of movies. However, this is not true for all movies since there are some movies that have grossed more than a billion dollars and won no awards, which is an unexpected consequence. For example, the movie "Harry Potter and the Deathly Hallows: Part II" has engorssed $1,341,511,219 and has won no awards. In Figure 7.3, the  blue regression line expresses the relationship between the gross revenue and winning many awards. However, the number of movies winning many awards lowers as the gross revenue increases more than the gross value 2e+09 which represents an unexpected conseqense. 
  
  
  
  






