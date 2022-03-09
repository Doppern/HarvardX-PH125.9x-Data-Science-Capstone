# Install required packages if missing
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(scales)) install.packages("scales")
if(!require(patchwork)) install.packages("patchwork")
if(!require(lattice)) install.packages("lattice")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(lubridate)) install.packages("lubridate")
if(!require(Matrix)) install.packages("Matrix")
if(!require(recosystem)) install.packages("recosystem")
if(!require(parallel)) install.packages("parallel")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")

# Load required packages
library(tidyverse)
library(ggthemes)
library(scales)
library(patchwork)
library(lattice)
library(caret)
library(data.table)
library(lubridate)
library(Matrix)
library(recosystem)
library(parallel)
library(knitr)
library(kableExtra)

# Target measure
target_rmse <- 0.86490

# Set default ggplot theme for unified graphs
theme_set(theme_economist_white(gray_bg = F
                                , base_family = "Verdana") +
            theme(axis.title = element_text(margin = margin(t = 10
                                                            , r = 10
                                                            , b = 10
                                                            , l = 10)
                                            )
                  )
          )

# Provide a uniform style for tables throughout the document with an option for min digits
table_style <- function(table, min_two = F, results = F) {
  table %>%
    kbl(digits = if (results) {
      5
    } else {
      2
    }
    , format.args = if (min_two) {
      list(big.mark = ","
           , nsmall = 2
      )
    } else {
      list(big.mark = ",")
    }
    ) %>%
    kable_styling("striped"
                  , latex_options = c("HOLD_position"
                                      , "striped"
                  )
    ) %>%
    row_spec(0, bold = T)
}

# Download data files if not found on user's system assumes R version 4.0 or later
if (!file.exists("data/edx.rds") | !file.exists("data/validation.rds")) {
  dl <- tempfile()
  # get current timeout setting and adjust to 300s to allow for slow download
  to <- getOption("timeout")
  options(timeout = 300)
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  # if using R 3.6 or earlier:
  # movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
  #                                            title = as.character(title),
  #                                            genres = as.character(genres))
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
  options(timeout = to)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed, to)
  
  # create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
  }
  # save data to avoid having to re-download in future
  saveRDS(edx, "data/edx.rds")
  saveRDS(validation, "data/validation.rds")
} else {
  edx <- readRDS("data/edx.rds")
  validation <- readRDS("data/validation.rds")
}

# Create sequential user and movie IDs to avoid having non-existent entries later
user_ids <- edx[, .(UID = unique(userId) # Included to not have odd column names
                    )
                , keyby = .(userId) # Sort userId ascending and have only unique values
                ] [, .(userId
                       , UID = seq_len(.N) # Create sequential numbers for new UID
                       )
                   ]
movie_ids <- edx[, .(MID = unique(movieId)
                     )
                 , keyby = .(movieId)
                 ] [, .(movieId
                        , MID = seq_len(.N)
                        )
                    ]
# Left join the original tables and the sequential movie IDs
edx <- merge.data.table(user_ids, edx, by = "userId")
edx <- merge.data.table(movie_ids, edx, by = "movieId")
validation <- merge.data.table(user_ids, validation, by = "userId")
validation <- merge.data.table(movie_ids, validation, by = "movieId")
rm(user_ids, movie_ids)

# Update rating timestamp columns to be correct format for calculations
edx[, timestamp := as_datetime(timestamp)]
validation[, timestamp := as_datetime(timestamp)]

# Add rating week identifier
edx[, week := paste0(year(timestamp), week(timestamp))]
validation[, week := paste0(year(timestamp), week(timestamp))]

# Extract movie release year from the title
edx[, release_year := as.integer(str_match(title, "\\((\\d{4})\\)$")[,2])]
validation[, release_year := as.integer(str_match(title, "\\((\\d{4})\\)$")[,2])]

# Add years between review and release
edx[, years_passed := year(timestamp) - release_year]
validation[, years_passed := year(timestamp) - release_year]

# Useful counts to have at various places
n_users <- edx[, length(unique(UID))]
n_movies <- edx[, length(unique(MID))]

# Sparsity of ratings is the number of ratings in the dataset divided by number of possible ratings (users times movies)
sparsity <- edx[, .N] / (n_users * n_movies)

# How ratings are distributed between the different rating options
distribution_ratings <-
  edx[, .(count = .N)
      , keyby = .(rating = as.factor(rating))
      ] %>%
  ggplot(aes(x = rating,
             y = count
             )
         ) +
  geom_col() +
  scale_y_continuous(labels = function(x) x / 1e6
                     , breaks = breaks_width(1e6)
                     , minor_breaks = breaks_width(1e6 / 5)
                     ) +
  labs(title = "Distribution of ratings"
       , x = "Rating"
       , y = "Count of ratings in millions"
       ) +
  theme(panel.grid.minor.y = element_line(colour = "grey"
                                          , linetype = "dotted")
        )

# Top 5 highly rated movies
top_5_movies <- edx[, .(Rating = mean(rating)
                        , Reviews = .N
                        )
                    , by = .(Title = title)
                    ] [Reviews >= 10
                       ] [order(desc(Rating))
                          ] %>% top_n(5, Rating)

# Bottom 5 low rated movies
bottom_5_movies <- edx[, .(Rating = mean(rating)
                           , Reviews = .N
                           )
                       , by = .(Title = title)
                       ] [Reviews >= 10
                          ] [order(desc(Rating))
                             ] %>% top_n(-5, Rating)

# Top 5 users with many reviews
top_5_users <- edx[, .(Rating = mean(rating)
                       , Reviews = .N
                       )
                   , by = .(User = UID)
                   ] [order(desc(Rating))
                      ] %>% top_n(5, Rating)

# Bottom 5 users with few reviews
bottom_5_users <- edx[, .(Rating = mean(rating)
                          , Reviews = .N
                          )
                      , by = .(User = UID)
                      ] [order(desc(Rating))
                         ] %>% top_n(-5, Rating)

# Does movies with more ratings get higher ratings?
effect_of_number_of_ratings_per_movie <-
  edx[, .(rating = mean(rating)
          , num_rating = .N
          , rating_group = (.N %/% 50) # calculate number of ratings integer divided by 50 for grouping
          )
      , by = .(MID) 
      ] [, .(rating = sum(rating * num_rating) /
               sum(num_rating)
             )
         , by = .(rating_group)
         ] %>%
  ggplot(aes(x = rating_group
             , y = rating)) +
  geom_point() +
  geom_smooth(method = "loess"
              , formula = "y ~ x") +
  scale_x_continuous(name = "Number of reviews"
                     , labels = function(x) format(x * 50, big.mark = ",")
                     , breaks = breaks_width(10000 / 50)
                     ) +
  scale_y_continuous(name = "Average Rating"
                     , limits = c(0, 5)) +
  ggtitle("Movies")

# Does users with higher number of ratings give higher ratings?
effect_of_number_of_ratings_per_user <-
  edx[, .(rating = mean(rating)
          , num_rating = .N
          , rating_group = (.N %/% 50)
  )
  , by = .(UID)
  ] [, .(rating = sum(rating * num_rating) /
           sum(num_rating)
  )
  , by = .(rating_group)
  ] %>%
  ggplot(aes(x = rating_group
             , y = rating)) +
  geom_point() +
  geom_smooth(method = "loess"
              , formula = "y ~ x") +
  scale_x_continuous(name = "Number of reviews"
                     , labels = function(x) format(x * 50, big.mark = ",")
                     , breaks = breaks_width(1500 / 50)
  ) +
  scale_y_continuous(name = "Average Rating"
                     , limits = c(0, 5)) +
  ggtitle("Users")

# Does the genre of a movie affect it's average rating?
effect_of_genre_on_ratings <- edx[, .(Rating = mean(rating)
                                      , num_ratings = .N
                                      )
                                  , keyby = .(genres
                                              )
                                  ] %>%
  ggplot(aes(x = reorder(genres
                         , Rating
                         )
             , y = Rating
             )
         ) +#, size = num_ratings)) +
  geom_col() +
  labs(y = "Rating"
       , x = "Genre"
       , title = "Genre effect on rating"
       ) +
  scale_x_discrete(labels = c("Drama", "Comedy", "Romance", "War", "Documentary", "Horror", "Action", "Animation", "Mystery", "Thriller", "Comedy|Romance", "Documentary|Horror", "Animation|IMAX|Sci-Fi")
                   , breaks = c("Drama", "Comedy", "Romance", "War", "Documentary", "Horror", "Action", "Animation", "Mystery", "Thriller", "Comedy|Romance", "Documentary|Horror", "Animation|IMAX|Sci-Fi")
                   ) +
  theme(axis.text.x = element_text(angle = 90
                                   , vjust = 1
                                   , hjust = 1
                                   , size = 9
                                   # , lineheight = 15
                                   )
        )

# Add season based on month
edx[, season := factor(case_when(month(timestamp) %in% 1:3 ~ "Winter"
                                 , month(timestamp) %in% 4:6 ~ "Spring"
                                 , month(timestamp) %in% 7:9 ~ "Summer"
                                 , TRUE ~ "Autumn"
                                 )
                       , levels = c("Winter"
                                    , "Spring"
                                    , "Summer"
                                    , "Autumn"
                                    )
                       , ordered = TRUE
                       )
    ]
validation[, season := factor(case_when(month(timestamp) %in% 1:3 ~ "Winter"
                                        , month(timestamp) %in% 4:6 ~ "Spring"
                                        , month(timestamp) %in% 7:9 ~ "Summer"
                                        , TRUE ~ "Autumn"
                                        )
                              , levels = c("Winter"
                                           , "Spring"
                                           , "Summer"
                                           , "Autumn"
                                           )
                              , ordered = TRUE
                              )
           ]

# Calculate average review per season
avg_review_per_season <- edx[, .(rating = mean(rating)
                                 )
                             , by = .(season = season
                                      )
                             ] [order(season)
                                ] %>%
  pivot_wider(names_from = season
              , values_from = rating
              )

# Average rating per season and year zoomed in on the y-axis
rating_time_season_zoom <- edx[, .(Rating = mean(rating)
                                   , num_ratings = .N
                                   )
                               , keyby = .(Year = year(timestamp)
                                           , Season = season
                                           )
                               ] [num_ratings >= 10
                                  ] %>%
  ggplot(aes(x = Year
             , y = Rating
             , color = Season
             )
         ) +
  geom_line(size = 1.2) +
  scale_color_discrete() +
  scale_x_continuous(n.breaks = 8) +
  ggtitle("Zoomed") +
  guides(x = guide_axis(angle = 45))

# Zooming out on the y-axis for season and year
rating_time_season_full <- rating_time_season_zoom +
  scale_y_continuous(limits = c(0
                                , 5
                                )
                     ) +
  ggtitle("Full y-range")

# Winter 1996 data
spike_winter_96 <- edx[year(timestamp) == 1996
                       & season == "Winter"
                       , .(rating = mean(rating)
                           , n = .N
                       )
                       , by = .(title
                       )
                       ] [order(desc(n))
                          ] %>% top_n(5
                                      , n
                                      ) %>%
  select(Title = title
         , Reviews = n
         , Rating = rating
         )

# Does the year and week a review is given in affect the average?
effect_of_week_of_review <- edx[, .(rating = mean(rating)
                                    , number = .N
                                    )
                                , by = .(week)
                                ]  %>%
  ggplot(aes(x = week
             , y = rating
             , size = number
             , alpha = .3
             )
         ) +
  geom_point(shape = 1) +
  scale_y_continuous(limits = c(0
                                , 5
                                )
                     ) +
  scale_x_discrete(breaks = as.character(seq(edx[, min(week)]
                                             , edx[, max(week)]
                                             , 10
                                             )
                                         )
                   , labels = as.character(seq(edx[, min(week)]
                                               , edx[, max(week)]
                                               , 10
                                               )
                                           ) %>% (
                                             function(x){
                                               paste0(str_sub(x, 1, 4)
                                                      , "-"
                                                      , str_sub(x, 5, 6)
                                               )
                                               }
                                             )(.) # this is to pass the argument from the pipe to the function
                   ) +
  scale_size_continuous(labels = comma) +
  labs(x = "Year-week"
       , y = "Avg. Rating"
       , size = "Number of reviews"
       , title = "Effect of week of year on ratings"
       ) +
  theme(axis.text.x = element_text(angle = 45
                                   , vjust = 0.2
                                   )
        , legend.position = "top"
        ) +
  guides(alpha = "none")

# Aggregate by releaseyear to facilitate plotting
release_year <- edx[, .(rating = mean(rating)
                        , num_reviews = .N)
                    , by = .(release_year)
                    ] %>%
  mutate(smooth = loess(formula = rating ~ release_year
                        , data = edx[, .(rating = mean(rating))
                                     , by = .(release_year)
                                     ]
                        ) %>%
           predict()
         )

# Does the year a movie was released affect it's average rating?
effect_of_release_year_on_average <- release_year %>%
  ggplot(aes(x = release_year
             , y = rating
             )
         ) +
  geom_point() +
  geom_vline(xintercept = release_year$release_year[which.max(release_year$smooth)]
             , linetype = "longdash"
             , color = "green"
             , size = 1
             , alpha = 0.5
             ) +
  geom_vline(xintercept = 1980
             , linetype = "longdash"
             , color = "black"
             , size = 1
             , alpha = 0.5
             ) +
  geom_vline(xintercept = release_year$release_year[which.min(release_year$smooth)]
             , linetype = "longdash"
             , color = "red"
             , size = 1
             , alpha = 0.5
             ) +
  geom_smooth(method = "loess"
              , formula = y ~ x
              ) +
  geom_text(data = data.frame(release_year =
                                c(release_year$release_year[which.max(release_year$smooth)]
                                  , 1980
                                  , release_year$release_year[which.min(release_year$smooth)]
                                  )
                              , rating = 0.75
                              , value = c(max(release_year$smooth)
                                          , release_year[release_year == 1980, smooth]
                                          , min(release_year$smooth)
                                          )
                              )
            , nudge_x = -6
            , mapping = aes(label = paste("Year:"
                                          , release_year
                                          , "\nTrend-value:"
                                          , round(value
                                                  , 1
                                                  )
                                          )
                            )
            ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limits = c(0
                                , 5
                                )
                     ) +
  labs(x = "Release Year"
       , y = "Avg. Rating"
       , title = "Effect of release year on rating"
       )

# Does how old the movie is at the time of the review affect the average rating?
effect_of_years_between_review_release_on_average <- edx[, .(rating = mean(rating)
                                                             , number_reviews = .N
                                                             )
                                                         , by = .(years_passed)
                                                         ] %>%
  ggplot(aes(x = years_passed
             , y = rating
             , size = number_reviews
             # , shape = 12
             )
         ) +
  geom_point(shape = 1) +
  scale_size_continuous(labels = comma) +
  scale_y_continuous(limits = c(0
                                , 5
                                )
                     ) +
  labs(x = "Years between rating and release"
       , y = "Avg. Rating"
       , size = "Number of Reviews"
       , title = "Effect of movie age on rating"
       ) +
  theme(legend.position = "top")

# Create a training and testing set
set.seed(2021)
test_index <- createDataPartition(y = edx$rating
                                  , p = .2
                                  , list = F
                                  )
training_set <- edx[-test_index]
temp_test <- edx[test_index]
# Making sure there are no movies or users in the testing set that aren't in the training set
test_set <- temp_test %>%
  semi_join(training_set
            , by = "movieId"
            ) %>%
  semi_join(training_set
            , by = "userId"
            )
removed_test <- anti_join(temp_test
                          , test_set
                          )
training_set <- rbind(training_set
                      , removed_test
                      )
rm(temp_test
   , removed_test
   , test_index
   )

# calculate overall average
overall_training_avg <- training_set[, .(overall_training_avg =
                                           mean(rating)
                                         )
                                     ] %>% pull(overall_training_avg)

# Calculate each movie's difference from the overall average and add them to the datasets
movie_effect_on_avg <- training_set[, .(movie_effect_on_rating =
                                          mean(rating)
                                        - overall_training_avg
                                        )
                                    , by = .(MID)
                                    ]
training_set <- movie_effect_on_avg[training_set
                                    , on = .(MID)
                                    ]
test_set <- movie_effect_on_avg[test_set
                                , on = .(MID)
                                ]

# Calculate each user's difference from the overall average adjusted for movie effect and add them to the datasets
user_effect_on_avg <- training_set[, .(user_effect_on_rating =
                                         mean(rating
                                              - movie_effect_on_rating
                                              )
                                       - overall_training_avg
                                       )
                                   , by = .(UID)
                                   ]
training_set <- user_effect_on_avg[training_set
                                   , on = .(UID)
                                   ]
test_set <- user_effect_on_avg[test_set
                               , on = .(UID)
                               ]

# Calculate the average for each rating group of 50 ratings received and adjust for movie and user effects then add them to the datasets
training_set <-  training_set[, .(rating_group = .N %/% 50)
                              , by = .(MID)
                              ] [training_set
                                 , on = .(MID)
                                 ]
test_set <- training_set[, .(rating_group = .N %/% 50)
                         , by = .(MID)
                         ] [test_set
                            , on = .(MID)
                            ]

rating_group_effect_on_avg <- training_set[, .(rating_group_effect_on_rating =
                                                 mean(rating
                                                      - movie_effect_on_rating
                                                      - user_effect_on_rating
                                                      )
                                               - overall_training_avg
                                               )
                                           , by = .(rating_group)
                                           ]

training_set <- rating_group_effect_on_avg[training_set
                                           , on = .(rating_group)
                                           ]
test_set <- rating_group_effect_on_avg[test_set
                                       , on = .(rating_group)
                                       ]

# Calculate each genre's difference from the overall average adjusted for movie and user effect and add them to the datasets
genre_effect_on_avg <- training_set[, .(genre_effect_on_rating =
                                          mean(rating
                                               - movie_effect_on_rating
                                               - user_effect_on_rating
                                               - rating_group_effect_on_rating
                                               )
                                        - overall_training_avg
                                        )
                                    , by = .(genres)
                                    ]
training_set <- genre_effect_on_avg[training_set
                                    , on = .(genres)
                                    ]
test_set <- genre_effect_on_avg[test_set
                                , on = .(genres)
                                ]

# Calculate each week's difference from the overall average adjusted for movie, user and genre effect and add them to the datasets
week_effect_on_avg <- training_set[, .(week_effect_on_rating =
                                         mean(rating
                                              - movie_effect_on_rating
                                              - user_effect_on_rating
                                              - rating_group_effect_on_rating
                                              - genre_effect_on_rating
                                              )
                                       - overall_training_avg
                                       )
                                   , by = .(week)
                                   ]
training_set <- week_effect_on_avg[training_set
                                   , on = .(week)
                                   ]
test_set <- week_effect_on_avg[test_set
                               , on = .(week)
                               ]

# Calculate effect of release year of movie while adjusting for previous factors
release_year_effect_on_avg <- training_set[, .(release_year_effect_on_rating =
                                                 mean(rating
                                                      - movie_effect_on_rating
                                                      - user_effect_on_rating
                                                      - rating_group_effect_on_rating
                                                      - genre_effect_on_rating
                                                      - week_effect_on_rating
                                                      )
                                               - overall_training_avg
                                               )
                                           , by = .(release_year)
                                           ]
training_set <- release_year_effect_on_avg[training_set
                                           , on = .(release_year)
                                           ]
test_set <- release_year_effect_on_avg[test_set
                                       , on = .(release_year)
                                       ]

# Calculate effect of age of movie at time of review while adjusting for previous factors
age_effect_on_avg <- training_set[, .(age_effect_on_rating =
                                        mean(rating
                                             - movie_effect_on_rating
                                             - user_effect_on_rating
                                             - rating_group_effect_on_rating
                                             - genre_effect_on_rating
                                             - week_effect_on_rating
                                             - release_year_effect_on_rating
                                             )
                                      - overall_training_avg
                                      )
                                  , by = .(years_passed)
                                  ]
training_set <- age_effect_on_avg[training_set
                                  , on = .(years_passed)
                                  ]
test_set <- age_effect_on_avg[test_set
                              , on = .(years_passed)
                              ]

# Calculate the RMSEs with different factors taken into account:
# Global Average
overview_RMSEs <- data.table(Method = "Average only:"
                             , RMSE = RMSE(pred = overall_training_avg
                                           , obs = test_set[, rating]
                                           )
                             )
# Global average and the effect of each movie
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Movie effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add the effect of each user
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "User effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add number of ratings
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Rating group effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   + rating_group_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add movie genre
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Genre effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   + rating_group_effect_on_rating
                                                                   + genre_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add week of review
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Week effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   + rating_group_effect_on_rating
                                                                   + genre_effect_on_rating
                                                                   + week_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add release year
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Release year effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   + rating_group_effect_on_rating
                                                                   + genre_effect_on_rating
                                                                   + week_effect_on_rating
                                                                   + release_year_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add age of movie at time of review
overview_RMSEs <- rbind(overview_RMSEs
                        , data.table(Method = "Age of movie effect:"
                                     , RMSE = RMSE(pred = test_set[, overall_training_avg
                                                                   + movie_effect_on_rating
                                                                   + user_effect_on_rating
                                                                   + rating_group_effect_on_rating
                                                                   + genre_effect_on_rating
                                                                   + week_effect_on_rating
                                                                   + release_year_effect_on_rating
                                                                   + age_effect_on_rating
                                                                   ]
                                                   , obs = test_set[, rating]
                                                   )
                                     )
                        )
# Add column to check results
overview_RMSEs[, "Goal met:" := RMSE < target_rmse]

# initialize setup with number of logical cores and populate training and testing matrices
threads <- detectCores()
Y_train <- sparseMatrix(i = training_set[, UID]
                        , j = training_set[, MID]
                        , x = training_set[, rating]
                        , repr = "T"
                        )
Y_test <- sparseMatrix(i = test_set[, UID]
                       , j = test_set[, MID]
                       , x = test_set[, rating]
                       , repr = "T"
                       )

# Create the reco object
r <- Reco()

# Untuned recosystem still beats our target RMSE of 0.8649
# Train the model with default options:
opts <- list(nthreads = threads)
r$train(train_data = data_matrix(Y_train)
        , opts = opts
        )
# Predict with default options
preds <- r$predict(test_data = data_matrix(Y_test)
                   , out_pred = out_memory()
                   )
rmse_default <- RMSE(preds
                     , Y_test@x
                     )

# Recosystem comes with default tuning parameters,
# tune with the number of cores detected and default options takes a long time and finds these options:
# Uncomment to use pre-found options
opts <- list(dim = 20
             , costp_l1 = 0
             , costp_l2 = 0.01
             , costq_l1 = 0
             , costq_l2 = 0.1
             , lrate = 0.1
             , nthreads = threads
             )
# Uncomment to find options yourself
# opts <- r$tune(train_data = data_matrix(Y_train)
#                , opts = list(nthreads = threads)
#                )

# Train model with the tuned options
r$train(train_data = data_matrix(Y_train)
        , opts = opts
        )
# Predict results with the trained model
preds <- r$predict(test_data = data_matrix(Y_test)
                   , out_pred = out_memory()
                   )
rmse_default_tune <- RMSE(preds
                          , Y_test@x
                          )

# Finding the ideal options involves giving fairly large ranges for each option
# then running large and time consuming crossvalidation tuning runs I found these options after several hours
# but they can still be improved, but the modest improvement from default tune seems not worth it to put more time in for now
opts <- list(dim = 33
             , costp_l1 = 0
             , costp_l2 = 0.013
             , costq_l1 = 0
             , costq_l2 = 0.11
             , lrate = 0.11
             , nthreads = threads
             , niter = 30
             )
r$train(train_data = data_matrix(Y_train)
        , opts = opts
        )
preds <- r$predict(test_data = data_matrix(Y_test)
                   , out_pred = out_memory()
                   )
rmse_optimal_tune <- RMSE(preds
                          , Y_test@x
                          )

# Final run on whole working set with optimal parameters
Y_final_train <- sparseMatrix(i = edx[, UID]
                              , j = edx[, MID]
                              , x = edx[, rating]
                              , repr = "T"
                              )
Y_final_validation <- sparseMatrix(i = validation[, UID]
                                   , j = validation[, MID]
                                   , x = validation[, rating]
                                   , repr = "T"
                                   )
r$train(train_data = data_matrix(Y_final_train)
        , opts = opts
        )
preds <- r$predict(test_data = data_matrix(Y_final_validation)
                   , out_pred = out_memory()
                   )
rmse_final <- RMSE(preds
                   , Y_final_validation@x
                   )