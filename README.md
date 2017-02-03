Data Analytics and Visualization using R

The following homeworks are project are implemented using R.

<p>HW1. </p>
<p>1. Implement a function that computes the log of the factorial value of an integer using a for loop. Note that implementing it using log(A)+log(B)+ · · · avoids overflow while implementing it as log(A · B · · · · ) creates an overflow early on. </p>
<p>3. Implement a function that computes the log of the factorial value of an integer using recursion. </p>
<p>4. Using your two implementations of log-factorial in (2) and (3) above, com- pute the sum of the log-factorials of the integers 1, 2, . . . , N for various N values. </p>
<p>5. Compare the execution times of your two implementations for (4) with an implementation based on the o cial R function lfactorial(n). You may use the function system.time() to measure execution time. What are the growth rates of the three implementations as N increases? Use the command options(expressions=500000) to increase the number of nested recursions allowed. Compare the timing of the recursion implemen- tation as much as possible, and continue beyond that for the other two implementations. </p>


<p>HW 2<p/>
<p>1. Using the mpg data, describe the relationship between highway mpg and car manufacturer. Describe which companies produce the most and least fuel e cient cars, and display a graph supporting your conclusion.<p/>
<p>2. Using the mpg data, explore the three-way relationship between highway mpg, city mpg, and model class. What are your observations? Display a graph supporting these observations.<p/>
<p>3. What are the pros and cons of using a histogram vs a box plot? Which one will you prefer for what purpose?<p/>
<p>4. Generate two sets of N random points using the function runif and dis- play a corresponding scatter plot. If you save the file to disk, what is the resulting file size for the following file formats: ps, pdf, jpeg, png? How do these values scale with increasing N?<p/>
<p>5. The diamonds dataset within ggplot2 contains 10 columns (price, carat, cut, color, etc.) for 53940 di↵erent diamonds. Type help(diamonds) for more information. Plot histograms for color, carat, and price, and comment on their shapes. Investigate the three-way relationship between price, carat, and cut. What are your conclusions? Provide graphs that support your conclusions. If you encounter computational di culties, consider using a smaller dataframe whose rows are sampled from the original diamonds dataframe. Use the function sample to create a subset of indices that may be used to create the smaller dataframe.<p/>

<p>Project Part 1<p/>

<p>Data at https://s3.amazonaws.com/content.udacity-data.com/courses/gt-cs6262/project/movies_merged <p/>

<p>The file movies_merged1 contains a dataframe with the same name that has 40K rows and 39 columns. Each row represents a movie title and each column represents a descriptor such as Title, Actors, and Budget. I collected the data by querying IMDb’s API (see omdbapi.com) and joining it with a separate dataset of movie budgets and gross earnings (unknown to you). The join key was the movie title. This data is available for personal use, but IMDb’s terms of service do not allow it to be used for commercial purposes or for creating a competing repository.
The project’s theme is to investigate the relationship between the movie descriptors and the box office success of movies, as represented by the variable Gross. This task is extremely important as it can help a studio decide which titles to fund for production, how much to bid on produced movies, when to release a title, how much to invest in marketing and PR, etc. This information is most useful before a title is released, but it is still very valuable after the movie is already released to the public (for example it can affect additional marketing spend or how much a studio should negotiate with on- demand streaming companies for “second window” streaming rights).
Part I of the project involves getting the data ready for analysis and modeling and doing some preliminary investigations. It is 50% of the total project grade. Part II will involve modeling and predictions and will be released at a later date. Please complete the assignments below and submit a PDF file containing answers as well as a separate .R file containing the code that you used with adequate documentation showing which code was used for which of the 8 assignments below. The code can assume that the data file is already loaded but should have all library commands at the beginning and be self contained.<p/>

<p>The assignments below have equal weight and are sequential i.e., do step 2 after you processed the data as described in step 1. It is OK to handle missing values below by omission, but please omit as little as possible. It is worthwhile to invest in reusable and clear code as you may need to use it or modify it in part II of the project.<p/>

<p>1. The variable Type captures whether the row is a movie, a TV series, or a game. Remove all rows that do not correspond to movies. How many rows did you remove?<p/>

<p>2. The variable Runtime represents the length of the title as a string. Write R code to convert it to a numeric value (in minutes) and replace Runtime with the new numeric column. Investigate and describe the distribution of that value and comment on how it changes over years (variableYear) and how it changes in relation to the budget (variable Budget).<p/>

<p>3. The column Genre represents a list of genres associated with the movie in a string format. Write code to parse each text string into a binary vector with 1s representing the presence of a genre and 0s the absence and add it to the dataframe as additional columns. For example, if there are a total of 3 genres: Drama, Comedy, and Action a movie that is both Action and Comedy should be represented by a binary vector (0, 1, 1). Note that you need to first compile a dictionary of all possible genres and then figure out which movie has which genres (you can use the R tm package to create the dictionary). Graph and describe the relative proportions of titles having the top 10 genres and examine how the distribution of gross revenue (variable Gross) changes across genres.<p/>

<p>4. The dataframe was put together by merging two different sources of data and it is possible that the merging process was inaccurate in some cases (the merge was done based on movie title, but there are cases of different movies with the same title). The first source’s release time was represented by the column Year (numeric representation of the year) and the second by the column Release (string representation of release date). Find and remove all rows where you suspect a merge error occurred based on a mismatch between these two variables. To make sure subsequent analysis and modeling work well, avoid removing more than 10% of the rows that have a present Gross variable. What is your precise removal logic and how many rows did you end up removing?<p/>

<p>5. An important question is when to release a movie. Investigate the relationship between release date and gross revenue and comment on what times of year are most high revenue movies released in. Does your answer changes for different genres? Based on the data, can you formulate a genre-based recommendation for release date that is likely to increase the title’s revenue? If you have a recommendation motivate it with the appropriate disclaimers, or otherwise explain why you are unable to produce a recommendation.<p/>

<p>6. There are several variables that describe ratings including IMDb ratings (imdbRating represents average user ratings and imdbVotes represents the number of user ratings) and multiple Rotten Tomatoes ratings (represented by several variables pre-fixed by tomato). Read up on such ratings on the web (for example rottentomatoes.com/about and http:// www.imdb.com/help/show_leaf?votestopfaq) and investigate the pairwise relationships between these different descriptors using graphs. Comment on similarities and differences between the user ratings of IMDb and the critics ratings of Rotten Tomatoes. Comment on the relationships between these variables and the gross revenue. Which of these ratings are the most highly correlated with gross revenue (use the R function cor and remove rows with missing values)?<p/>

<p>7. The variable Awards describes nominations and awards in text format. Convert it to a three dimensional binary vector whose first component represents no nomination or awards, the second component represents some nominations/awards, and the third component represents many nominations or awards. The relationship between the second and the third categories should be close to 5:1 (not precisely - this is a broad guideline to help you avoid creating a third category that is useless due to being extremely small and to encourage consistency). How did you construct your conversion mechanism? How does the gross revenue distribution changes across these three categories.<p/>

<p>8. Come up with two new insights (backed up by the data and graphs) that are expected, and one new insight (backed up by data and graphs) that is unexpected at first glance and do your best to motivate it. By “new” here I mean insights that are not an immediate consequence of one of the above assignments.<p/>

<p>Project Part 2<p/>

<p>1.Use linear regression to predict profit based on all available numeric variables. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?<p/>

<p>2. Try to improve the prediction quality in (1) as much as possible by adding feature transformations of the numeric variables. Explore both numeric transformations such as power transforms and non-numeric transformations of the numeric variables like binning (e.g., is_budget_greater_than_3M). Explain which transformations you used and why you chose them. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?<p/>

<p>3. Write code that featurizes genre (can use code from Part-I), actors, directors, and other categorical variables. Explain how you encoded the variables into features.<p/>

<p>4. Use linear regression to predict profit based on all available non-numeric variables (using the transformations in (3). Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?<p/>

<p>5. Try to improve the prediction quality in (1) as much as possible by using both numeric and non- numeric variables as well as creating additional transformed features including interaction features (for example is_genre_comedy x is_budget_greater_than_3M). Explain which transformations you used and why you chose them. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?<p/>
