

#1.- THE INTRODUCTION: # The present project it´s "The Prediction Model Performance", based on The Sample Mean Square Error or RMSE. Using data analysis packages and the ten percent of the edx dataset: the validation set. With the purpose to develop a recommendation system, here it´s presenting a series of data analysis where RMSE it´s the standard deviation of the residuals, and these residuals are a measure of how far from the regression line data points are and how spread out these residuals are. And shows how concentrated the data is around the line of best fit, having a goal below 0.8649




#2.-THE DATA


  
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


ls()






    ## 2.1 The Variables: The variables are; userId, movieId, rating, timestamp, title and genres. the timestamp passed to as date time format creating a date column. Additionaly it´s created a year variable, extracted from movie column:
    # In the validation set, we have eight variables or columns: * UserId: A number to identify a user * MovieId: A number to identify a movie * Rating: One to five, in a sequence of 0.5, are 10 ratings * Timestamp: Record the time or date of * Title: The name of the movie * Genres: The gente of the movie, some movies have more than one * Date: The format of hour, minute and second registration * Year: The year of that registration ranking


  validation %>% select(where(is.numeric)) %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>% 
    ggplot(aes(x = Variable, y = Value, fill = Variable))+
    geom_boxplot()+
    scale_y_continuous(trans = "log10")+
    ggtitle("Variable Boxplot Distribution")
  


    ## 2.2 Rating Proportion: **Mode & Mean**: The mode it´s the rating 4 with more than two million and a half registration. And the mean it´s 3.5



edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))


edx %>%
  group_by(rating) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n))



    ## 2.3 Rating and the ratio histogram distribution: Plot with the percent ratio and rating distribution.


ggplot(data = edx %>%
         group_by(rating) %>%
         summarise(n = n()) %>%
         mutate(percent = n/sum(n)), aes(x = rating, y = percent))+
  geom_col(fill = "Blue", colour = "Black")+
  geom_text(aes(label = rating), vjust = -0.5, colour = "Black", size = 3)+
  geom_text(aes(label = round(x = percent, digits = 4), angle = 45, vjust = 0.8, hjust = 1, colour = percent), nudge_y = 0.03, size = 4)+
  theme(panel.background = element_rect(fill = "#d5e4eb"))+
  theme(plot.background = element_rect(fill = "#bfbe9c"))+
  theme(panel.background = element_rect(fill = "#bfbe9c"))+
  theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(legend.key.size = unit(0.2, "cm"))+
  theme(axis.title.x = element_text(color = "Black", size = 12))+
  theme(axis.title.y = element_text(color = "Black", size = 12))+
  xlab("Rating Distribution")+
  ylab("Percent Ratio")+
  theme(legend.position = "right")  #scale_fill_manual(values = c("0.5" = "#FCFED4", "1" = "#CCEDB1", "1.5" = "#41B7C4"))


          #### (Just the list of distribution in edx and validation)

 
  
(edx[,list("Total_Users" = uniqueN(userId),
          "Total_Movie" = uniqueN(movieId),
          "Total_Rating" = uniqueN(rating),
          "Total_Timestamp" = uniqueN(timestamp),
          "Total_Title" = uniqueN(title),
          "Total_Genres" = uniqueN(genres))])



(validation[,list("Total_Users" = uniqueN(userId),
          "Total_Movie" = uniqueN(movieId),
          "Total_Rating" = uniqueN(rating),
          "Total_Timestamp" = uniqueN(timestamp),
          "Total_Title" = uniqueN(title),
          "Total_Genres" = uniqueN(genres))])
  




class(edx)
  


          #### (Just create a year Column in validation data)


  
edx %>% select(genres) %>% separate_rows() %>% summarise(n = n()) 
edx = edx %>% mutate(year = as.numeric(str_sub(str_trim(title,side = "right"), start = -5,end = -2)))
validation = validation %>% mutate(year = as.numeric(str_sub(str_trim(title,side = "right"), start = -5,end = -2)))




    ## 2.4 The Sample Size: The issue of calculating the effective sample size it´s though this formula: n <- (p*q)*z^2/e^2


p <- 0.5 #(50%)
q <- 0.5 #(50%)
e <-  0.01 #(0,1%)
z <- 1.65

n <- (p*q)*z^2/e^2
n #The result it´s 6806.25, this is the simple size.




    ## 2.5 The Sample Plot 


# Sample Visualization genres
# p 0.5, q 0.5, error 1% and z-value 1.65, the result it´s 6806:
edx[sample(1:nrow(edx), 6806),] %>%
  filter(genres %in% c("Comedy", "Romance", "Action", "Crime", "Thriller", "Drama", 
                       "Sci-Fi", "Adventure", "Children", "Fantasy","War", 
                       "Animation", "Musical", "Western", "Mystery", "Film-Noir", 
                       "Horror", "Documentary", "IMAX")) %>% 
  ggplot(aes(x = rating, y = genres))+
  geom_boxplot(outlier.shape = NA, aes(fill = genres))+
  geom_point(position = "jitter", aes(color = genres))+
  ggtitle("C.I. 90%, z-value 1,64, se 1%, p-value 0.5, q-value 0.5", subtitle = "Result: Sample of 6.806")+
  theme(panel.background = element_rect(fill = "#d5e4eb"))+
  theme(plot.background = element_rect(fill = "#bfbe9c"))+
  theme(panel.background = element_rect(fill = "#bfbe9c"))+
  theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(axis.title.x = element_text(color = "Black", size = 12))+
  theme(axis.title.y = element_text(color = "Black", size = 12))+
  xlab("Rating Distribution")+
  ylab("Genres")+
  theme(legend.position = "bottom")



    ## 2.6 Movie ID movieId Frequency: The movieId frequency, it´s very notorious that are some movieId´s more frequent than others.



count(validation, movieId)

  count(validation, movieId) %>% ggplot(aes(n))+ 
    geom_histogram(bins = 20, color = "Blue")+
    scale_x_continuous(trans = "log10")+
    ggtitle("MovieId", subtitle = "MovieId Frequency")+
    theme(panel.background = element_rect(fill = "#d5e4eb"))+
    theme(plot.background = element_rect(fill = "#bfbe9c"))+
    theme(panel.background = element_rect(fill = "#bfbe9c"))+
    theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
    theme(legend.key = element_rect(fill = "transparent"))+
    theme(legend.key.size = unit(1, "cm"))+
    theme(axis.title.x = element_text(color = "Black", size = 12))+
    theme(axis.title.y = element_text(color = "Black", size = 12))+
    xlab("MovieId")+
    ylab("Frequency")



          ## The Rating Average of Each Movie   ###### b_i_rating_avg it´s the Rating Average of Each Movie <- in VALIDATION ([1] 3.512033)


mu <- mean(validation$rating)  
mu

b_i_rating_avg <- validation %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))
b_i_rating_avg 
b_i_rating_avg %>% knitr::kable() #KABLE (Exclude in rmd)



          ## The Rating Average of Each Movie Plot


validation %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu)) %>% 
  ggplot(aes(b_i))+
  geom_histogram(binwidth = 0.8, fill = "Blue", col = "Darkblue")+
  theme(panel.background = element_rect(fill = "#d5e4eb"))+
  theme(plot.background = element_rect(fill = "#bfbe9c"))+
  theme(panel.background = element_rect(fill = "#bfbe9c"))+
  theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(axis.title.x = element_text(color = "Black", size = 12))+
  theme(axis.title.y = element_text(color = "Black", size = 12))+
  xlab("Rating Average of Each Movie")+
  ylab("")+
  ggtitle("Rating Average Plot")


          ## Just Percentile Distribution of the Rating Average of Each Movie


summary(mu+validation %>%
  left_join(validation %>% 
              group_by(movieId) %>% 
              summarise(b_i = mean(rating - mu)), by = "movieId") %>% .$b_i)


    ## 2.7 Sci-Fi and Film-Noir to SciFI and FilmNoir


  validation %>% separate_rows(genres) %>% distinct(genres)
  
  
  # Rating average and Rating Standard Error
  
  validation <- validation %>% separate_rows(genres)
  validation$genres[validation$genres == "Sci-Fi"] <- "SciFi"
  validation$genres[validation$genres == "Sci"] <- "SciFi"
  validation$genres[validation$genres == "Fi"] <- "SciFi"
  validation$genres[validation$genres == "Film-Noir"] <- "FilmNoir"
  validation$genres[validation$genres == "Film"] <- "FilmNoir"
  validation$genres[validation$genres == "Noir"] <- "FilmNoir"
  
  validation
  







#3.- RMSE PREPARATION


    ## 3.1 The rating average of Each Movie b_i_rating_avg: 3.512033 it´s the mean validation rating. **The Rating Average of Each Movie**: Here it´s the head validation rating of each registration, With the rating average of each movie, it´s significant for the RMSE formula, so for that reason it´s bresented this value. **The Rating Average of Each Movie Plot**: The b_i_rating_avg it´s the Rating Average of Each Movie and Percentile Distribution

 
mu <- mean(validation$rating)  
mu

b_i_rating_avg <- validation %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

head(b_i_rating_avg)

validation %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu)) %>% 
  ggplot(aes(b_i))+
  geom_histogram(binwidth = 0.8, fill = "Blue", col = "Darkblue")+
  theme(panel.background = element_rect(fill = "#d5e4eb"))+
  theme(plot.background = element_rect(fill = "#bfbe9c"))+
  theme(panel.background = element_rect(fill = "#bfbe9c"))+
  theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(axis.title.x = element_text(color = "Black", size = 12))+
  theme(axis.title.y = element_text(color = "Black", size = 12))+
  xlab("Rating Average of Each Movie")+
  ylab("")+
  ggtitle("Rating Average Plot", subtitle = "Movie Averages Least Squares Estimates")




    ## 3.2 The User Average of Each userId b_u_user_avg: **The User Average**: Shows the average of each user.


  
  b_u_user_avg <- validation %>% 
    left_join(b_i_rating_avg, by = "movieId") %>% 
    group_by(userId) %>% #De movieId lo cambié a userId
    summarise(b_u = mean(rating - mu- b_i))
  
  head(b_u_user_avg)


    #       Plot The User Average distribution: **The User Average of Each User Plot**: Here it is the rating average of movies.


  validation %>% 
    left_join(validation %>% 
                group_by(movieId) %>% 
                summarise(b_i = mean(rating - mu)), by = "movieId") %>% 
    group_by(userId) %>% 
    summarise(b_u = mean(rating - mu - b_i)) %>% 
    ggplot(aes(b_u))+
    geom_histogram(bins = 9, fill = "Blue", col = "Darkblue")+
    theme(panel.background = element_rect(fill = "#d5e4eb"))+
    theme(plot.background = element_rect(fill = "#bfbe9c"))+
    theme(panel.background = element_rect(fill = "#bfbe9c"))+
    theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
    theme(legend.key = element_rect(fill = "transparent"))+
    theme(legend.key.size = unit(1, "cm"))+
    theme(axis.title.x = element_text(color = "Black", size = 12))+
    theme(axis.title.y = element_text(color = "Black", size = 12))+
    xlab("Rating Average of Each Movie")+
    ylab("")+
    ggtitle("B_u distribution", subtitle = "The mean(rating - mu - b_i)")



    ## 3.3 The Rating average and SE of each genre & plot


  validation %>% separate_rows(genres) %>% 
    select(c(title, genres, rating)) %>% 
    group_by(genres) %>%
    summarise(n = n(), average = mean(rating), se = sd(rating)/sqrt(n())) 






 
  
  # List in one chunk
  
  validation %>% separate_rows(genres) %>% 
    select(c(title, genres, rating)) %>% 
    group_by(genres) %>%
    summarise(n = n(), average = mean(rating), se = sd(rating)/sqrt(n())) %>% 
    ggplot(aes(genres, average))+
    geom_point(aes(genres, average, color = se), size = 2)+
    geom_label_repel(aes(label = se), nudge_x = 1.5, nudge_y = 0.05)+
    theme(axis.text.x = element_text(angle = 90))+
    theme(panel.background = element_rect(fill = "#d5e4eb"))+
    theme(plot.background = element_rect(fill = "#bfbe9c"))+
    theme(panel.background = element_rect(fill = "#bfbe9c"))+
    theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
    theme(legend.key = element_rect(fill = "transparent"))+
    theme(legend.key.size = unit(1, "cm"))+
    theme(axis.title.x = element_text(color = "Black", size = 12))+
    theme(axis.title.y = element_text(color = "Black", size = 12))+
    xlab("19 Genres")+
    ylab("Rating average")+
    ggtitle("Rating average and SE of each genre")
  
  



    ## 3.4 Distribution of b_g: Now it´s necessary to obtain the average of rating minus mu, b_i and b_u, taking the others averages, now b_g is added to the team: * The Rating Average of Each Movie: **b_i_rating_avg** * The User Average of each userId: **b_u_user_avg** * The Rating less mu less b_i and less b_u : **b_g_genres_avg**


  b_g <- validation %>% 
    left_join(b_i_rating_avg, by = "movieId") %>% 
    group_by(movieId) %>% 
    summarise(b_u = mean(rating - mu- b_i))
  
  head(b_g)



    ## 3.5 Distribution of b_g averages: Now all the variables that were created are accepted to make a prediction



b_g_genres_avg <- validation %>% 
  left_join(b_u_user_avg, by = "userId") %>% 
  left_join(b_i_rating_avg, by = "movieId") %>% 
  group_by(genres) %>% 
  summarise(b_g = mean(rating-mu-b_i-b_u))

  
validation %>% 
  left_join(b_u_user_avg, by = "userId") %>% 
  left_join(b_i_rating_avg, by = "movieId") %>% 
  group_by(genres) %>% 
  summarise(b_g = mean(rating-mu-b_i-b_u)) %>%
  ggplot(aes(b_g))+
  geom_histogram(bins = 10, fill = "Blue", col = "Darkblue")+
  theme(panel.background = element_rect(fill = "#d5e4eb"))+
  theme(plot.background = element_rect(fill = "#bfbe9c"))+
  theme(panel.background = element_rect(fill = "#bfbe9c"))+
  theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
  theme(legend.key = element_rect(fill = "transparent"))+
  theme(axis.title.x = element_text(color = "Black", size = 12))+
  theme(axis.title.y = element_text(color = "Black", size = 12))+
  xlab("Genres Average")+
  ylab("")+
  ggtitle("B_g distribution", subtitle = "b_g = mean(rating-mu-b_i-b_u")
  





#4.- PREDICTION: # Taking all these: mu + b_i + b_u + b_g, now it´s prediction:**Prediction Result** Here it is the prediction head and summary result:
  

prediction <- validation %>% 
  left_join(b_u_user_avg, by = "userId") %>% 
  left_join(b_i_rating_avg, by = "movieId") %>% 
  left_join(b_g_genres_avg, by = "genres") %>% 
  summarise(predict = mu + b_i + b_u + b_g) %>% 
  pull(predict)

  
# Prediction Result
 

  head(prediction)
  summary(prediction)





#5.- REGULARIZATION: # Good movies are attracted by users, these will watch and rate each movies, movies with few viewers generate variable results. This plot shows that the rating mean and number of movie per quantity of n_rating quantity, this is the relationship between the movies rating and the number of times have been rated.


  validation %>% 
    group_by(movieId) %>% 
    summarise(n = n(), rating = mean(rating)) %>% 
    ggplot(aes(n, rating))+
    theme(axis.text.x = element_text(angle = 90))+
    theme(panel.background = element_rect(fill = "#d5e4eb"))+
    theme(plot.background = element_rect(fill = "#bfbe9c"))+
    theme(panel.background = element_rect(fill = "#bfbe9c"))+
    theme(legend.background = element_rect(fill="#bfbe9c", size=0.2, color = "#bfbe9c", linetype="solid",colour = "none"))+
    theme(legend.key = element_rect(fill = "transparent"))+
    theme(legend.key.size = unit(1, "cm"))+
    theme(axis.title.x = element_text(color = "Black", size = 12))+
    theme(axis.title.y = element_text(color = "Black", size = 12))+
    xlab("Quantity")+
    ylab("Rating")+
    ggtitle("Rating and mean")+
    geom_point()+
    geom_smooth(color = "blue")



    ## 5.1 Residuals or Prediction errors: # Create a residuals data frame that is residuals, that is the rating minus mu + b_i + b_u + b_g, with this difference shows how concentrated the data is around the best fit or how far the data points are in this case: error = actual - predicted. This is the same as: residual = rating - predicted (mu + b_i + b_u + b_g)

 
  residuals <- validation %>% 
    left_join(b_u_user_avg, by = "userId") %>% 
    left_join(b_i_rating_avg, by = "movieId") %>% 
    left_join(b_g_genres_avg, by = "genres") %>% 
    mutate(residual = rating - (mu + b_i + b_u + b_g))
  
  residuals
  class(residuals)
  
  #Residuals of each rating distribution:
  
  residuals %>% arrange(desc(abs(residual))) %>% 
    ggplot(aes(rating, residual))+
    geom_point(aes(rating, residual))



    ## 5.2 BASIC RMSE: To obtain a Basic RMSE, take the rating and the mean of rating, both in validation data set. The basic RMSE it´s 1.054861



basic_rmse <- RMSE(validation$rating, mean(validation$rating))
rmse_res <- data_frame(method = "Just the mean result", RMSE = basic_rmse)
rmse_res



    ## 5.3 IMPROVEMENT: Thanks to all the vectors that were created before, now it´s possible to obtain the final RMSE, taking all the average that were obtained: * The Rating Average of Each Movie: **b_i_rating_avg** * The User Average of each userId: **b_u_user_avg** * The Rating less mu less b_i and less b_u : **b_g_genres_avg** Now the results are:


predicted_ratings <- mu + validation %>% 
  left_join(b_i_rating_avg, by = "movieId") %>% 
  .$b_i

  # Predicted Ratings Summary
  
  summary(predicted_ratings)
  
  final_model_rmse <- RMSE(validation$rating, predicted_ratings)
  rmse_res <- bind_rows(rmse_res,
                        data_frame(method = "The movie effect Model",
                                   RMSE = final_model_rmse))
  
  rmse_res %>% knitr::kable()
  
  






#6.- FINAL RMSE: ***0.81***: Now taking Movie, User and Genre Effects Model, the RMSE it´s better obtaining a 0.81 result.


  
  
predicted_ratings <- validation %>% 
  left_join(b_u_user_avg, by = "userId") %>% 
  left_join(b_i_rating_avg, by = "movieId") %>% 
  left_join(b_g_genres_avg, by = "genres") %>% 
  summarise(predict = mu + b_i + b_u + b_g) %>% 
  pull(predict) 

final_3_model_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_res <- bind_rows(rmse_res,
                      data_frame(method = "Movie + User + Genre Effects Model",
                                 RMSE = final_3_model_rmse))
rmse_res %>% knitr::kable() #exclude kable in rmd






#7.- CONCLUSION: Based on The Sample Mean Square Error or RMSE taking the averages of Movie, User and the Genre Effects, makes a Prediction Model Performance Using data analysis packages, obtaining a good RMSE result or approximation. The limitations in the RMSE result, even the same as the random forest, the RMSE needs more variables or columns to obtain a better result, more numerical variables to obtain a better RMSE. Other limitations was the time to execute the data code, but this was necessary the large data to obtain a good RMSE. In the future, to obtain a better result, it´s necessary to obtain more numerical variables or columns to obtain more averages. It´s important to have a larger data, all these to doing a matrix factorization, that can improve the model and the result.



