movies_data <- read.csv("D:/MMA/2. Fall/MGSC 661/Midterm/cleaned_movies_data_nb.csv")
attach(movies_data)

###Libraries required----
require(ggplot2)
require(lmtest)
require(plm)
library(splines)
library(car)
library(boot)
library(corrplot)
library(glmnet)
library(dplyr)
library(tidyr)
library(car)
library(psych)
#----

###Step 0-Set the data----
movies_data_base <- read.csv("C:/Users/abmir/OneDrive/School/McGill/Fall/MGSC661/Midterm/IMDB_data_Fall_2024.csv")
# datadict <- read.csv("D:/MMA/2. Fall/MGSC 661/Midterm/data_dictionary_IMDB_Fall_2024.csv")
test_data <- read.csv("C:/Users/abmir/OneDrive/School/McGill/Fall/MGSC661/Midterm/test_data_IMDB_Fall_2024.csv")

movies_data_base <- rbind(movies_data_base, test_data)
attach(movies_data_base)

  # Missing values: Obtain detailed summary of dataset
skim(movies_data_base) # no NA values found in all rows and columns. good to proceed

  #Remove useless variables
columns_drop <- c("movie_id", "imdb_link", "actor1", "actor2", "actor3", "plot_keywords", "release_day", "release_year", "language", "colour_film","aspect_ratio")
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% columns_drop]
    # actor names wont be necessary w the actor meter. plot keywords will req NLP

#----

###Step 1-Dummyfing----

  #Genre----
# Summing the total number of genres each film has based on the alr dummified genre columns
genre_dummy <- c("action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime")
movies_data_base$total_genres <- rowSums(movies_data_base[,genre_dummy])
# Understanding the spread of total genres each film has
summary(movies_data_base$total_genres)
# Extracting info on observations with 0 genre tabulated
issue <- subset(movies_data_base, total_genres==0)
issue
# Multiple inconsistencies between dummified genre columns and genre column alone, drop dummified genre columns, create own dummified columns

# Split genre string into indiv genre type, but separated into rows
movies_data_base <- movies_data_base %>%
  separate_rows(genres, sep="\\|") # genres column now comprises of the indiv genre names

# Reorganise data such that same film with multiple genres are now combined into a single row, but genres are vectorized/dummified!
movies_data_base <- movies_data_base %>%
  mutate(value = 1) %>%  # Create a column with 1s to represent genre count x1
  pivot_wider(names_from = genres, values_from = value, values_fill = 0)
# pivot_wider reshapes data from long to wide format by forming individual columns for each unique value
# where genre is present, value = 1, otherwise 0

#### Removing genre-related columns that will not be used post dummification ###
# Remove the original dummified columns that came in the training data
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% genre_dummy]

# Removing column on total number of genres as well (I did a scatter plot on my own code and it was not very valuable so I will not include here unless we want it for the report)
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "total_genres"]
  
  #Month ----
table(movies_data_base$release_month) # Oct had the highest count, will be used as ref

# Visualising distribution of films across the months
ggplot(movies_data_base, aes(x = release_month)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of films by months", x = "Month", y = "Count") +
  theme_minimal() 


# Dummify months and imposing an order for subsequent analyses
movies_data_base$release_month <- as.factor(movies_data_base$release_month)

# Changing reference month to Oct
movies_data_base$release_month <- relevel(movies_data_base$release_month,ref="Oct")

# Visualise score against month
ggplot(movies_data_base, aes(x=release_month, y=imdb_score)) + 
  geom_point() + labs(title = "IMDB Score against Month", x = "Month", y = "IMDB Score")

m1 <- lm(imdb_score~release_month,data=movies_data_base)
summary(m1)
# coef are significant, month needs to be retained as a predictor
# not able to impose an order on the levels without resulting in a polynomial in the regression. will change to numerals

# Change months from string to numeric format
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

movies_data_base$release_month_num <- match(movies_data_base$release_month, month_names)

# Now relevel it using oct as ref, which has been converted to number 10
movies_data_base$release_month_num <- relevel(as.factor(movies_data_base$release_month_num), ref = "10")

# Now visualise the scatterplot between scores and months
ggplot(movies_data_base, aes(x=release_month_num, y=imdb_score)) + 
  geom_point() + labs(title = "IMDB Score against Month", x = "Month", y = "IMDB Score")
# Basic regression model between score and month
m <- lm(imdb_score~release_month_num,data=movies_data_base)
summary(m)  

# Drop month in string format [CLEANING]
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "release_month"]


  #Country----
table(movies_data_base$country) # noticed there was an erroenous entry "Official site" 

### Rectifying the wrong entry
# Import dataset again as the identifiers have been removed and the entry cannot be amended
wrong_country <- movies_data_base %>%
  filter(country == "Official site")
wrong_country # country = USA for the film!

# Updating the wrong field
movies_data_base <- movies_data_base %>%
  mutate(country = case_when(
    country == "Official site" ~ "USA",
    TRUE ~ country  # Keep other values unchanged
  ))

# Confirming once again that the country column has been cleaned up
table(movies_data_base$country) 
# perfect good to go, USA with most no of films, will be used as ref
movies_data_base$country = relevel(as.factor(movies_data_base$country), ref = "USA")

# Basic model to compare scores
t <- lm(imdb_score~country, data=movies_data_base)
summary(t)

# Regroup the countries into 4 main bins: USA, Canada, UK and Others. NZ and Kyrgyzstan have very few observations, not valuable to keep as indiv category
movies_data_base <- movies_data_base %>%
  mutate(country_grp = case_when(
    country == 'USA' ~ "USA",
    country == "UK" ~ "UK",
    country == "Canada" ~ "Canada",
    TRUE ~ "Others"))

# Dummifying country again and leveling it at USA
movies_data_base$country_grp <- relevel(as.factor(movies_data_base$country_grp),ref="USA")
table(movies_data_base$country_grp)

# Removing the original country column and retaining the newly created country column [CLEANING]
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "country"]
# Perfect, also ran a simple regression model and the subgroups are significantly diff from each other in their scores

  #Maturity rating----
table(movies_data_base$maturity_rating)
# Essentially 4 big umbrellas of maturity rating groups PG, PG-13, R and Adult (X, NC-17) but very few points for NC-17 and X anyway. will drop these
# Approved, Passed were also maturity ratings that were in the original set but were removed once the films released before 1968 were removed
# Good to have those 2 cats removed esp since the ratings at the time were effy

# Remove data points with X and NC-17
movies_data_base = movies_data_base[!(movies_data_base$maturity_rating %in% c("X", "NC-17")), ]

# Re-classifying the maturity ratings into 3 big umbrellas - PG, PG13 and R
# Classification based off https://en.wikipedia.org/wiki/Motion_Picture_Association_film_rating_system#:~:text=an%20MPAA%20trademark).-,From%20M%20to%20GP%20to%20PG,raised%20from%2016%20to%2017 
movies_data_base <- movies_data_base %>% 
  mutate(maturity_cat = case_when (
    maturity_rating %in% c("TV-G", "M", "GP", "PG") ~ "PG",
    maturity_rating %in% c("PG-13", "TV-14") ~ "PG-13",
    TRUE ~ "R"
  ))

# Drop original maturity column
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "maturity_rating"]

# Dummify the maturity column, and relevel with R as the ref 
movies_data_base$maturity_cat <- as.factor(movies_data_base$maturity_cat)
movies_data_base$maturity_cat <- relevel(movies_data_base$maturity_cat, ref="R")
### FYI: All categories are significantly different when I ran a regression model so the binning was decent enough considering R had such a huge number!


#----

###Step 2-Exploratory Analysis----

  #Duration ----
# Visualising the plot between score and duration
ggplot(movies_data_base, aes(x=duration, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Duration", x = "Duration", y = "IMDB Score")
# Does not seem to have much value comparing duration as a stand alone continuous variable
hist(movies_data_base$duration, breaks=30)
##### No visible pattern as a whole, consider re-categorising them/ applying natural log since the distribution follows a normal distribution of sorts anyway 

boxplot(movies_data_base$duration)
# Create a new column with the natural logarithm of the 'duration'
movies_data_base$log_duration <- log(movies_data_base$duration)
attach(movies_data_base)
  #Budget ----
# Visual plot of scores against budget
ggplot(movies_data_base, aes(x=movie_budget, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Budget", x = "Budget", y = "IMDB Score")

# Basic regression model between score and budget
bud <- lm(imdb_score~movie_budget, data=movies_data_base)
summary(bud)
# No clear pattern between these 2 variables, in fact it seems to suggest a decreasing trend, though a really really slight one
# Might still have some predictive power though, so it will be kept and we can let the model select possibly

# Check for outliers
outlierTest(bud) # no outlier, great! prob removed earlier under duration

# Heteroskedasticity check
ncvTest(bud)
### NOTE: HETEROSKEDASTICITY IN IMDB VS SCORE! 


  #nb_news_articles----
news <- lm(imdb_score~nb_news_articles, data=movies_data_base)
summary(news)
ggplot(movies_data_base, aes(x=log(nb_news_articles), y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of articles", x = "Number of articles", y = "IMDB Score")



  #nb_faces----
table(movies_data_base$nb_faces)
ggplot(movies_data_base, aes(x=nb_faces, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of faces on poster", x = "Number of faces", y = "IMDB Score")
#nb_faces
movies_data_base$faces = ifelse(movies_data_base$nb_faces >1, 1, 0)
#to not miss a lot of values
#faces on their poster we decided to create a new row that takes 1 as a value if there's faces on the poster and 0 otherwise.
attach(movies_data_base)

  #Movie meter pro----
summary(movies_data_base$movie_meter_IMDBpro)
mov_mtr <- lm(imdb_score~movie_meter_IMDBpro, data=movies_data_base)


# Simple visualisation
ggplot(movies_data_base, aes(x=movie_meter_IMDBpro, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against movie meter", x = "Movie meter", y = "IMDB Score")


  #Actors----
# Simple visualisation between score and star 1 meter
ggplot(movies_data_base, aes(x=actor1_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 1 star meter", x = "Actor 1 star meter", y = "IMDB Score")
# Visualisation between score and star 2 meter
ggplot(movies_data_base, aes(x=actor2_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 2 star meter", x = "Actor 2 star meter", y = "IMDB Score")
# Visualisation between score and star 3 meter
ggplot(movies_data_base, aes(x=actor3_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 3 star meter", x = "Actor 3 star meter", y = "IMDB Score")
#There is not a clear relationship between actors and target varible but there is with its average
movies_data_base$avg_actors = (movies_data_base$actor1_star_meter + movies_data_base$actor2_star_meter + movies_data_base$actor3_star_meter)/3
attach(movies_data_base)
ggplot(movies_data_base, aes(x=avg_actors, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 3 star meter", x = "Average Actors
       ", y = "IMDB Score")
#When outliers are remove this will change

  #Distributor----
# Understanding the distributors present in dataset
length(unique(movies_data_base$distributor)) # 331 unique distributors


# Create new df to check the number of movies associated with each distributor
df_distributor <- movies_data_base %>%
  group_by(distributor) %>%
  summarize(total_movies = n())

# Names of distributors are inconsistent, thus will be recategorised
# Create new column to store the re-categorised distributors
movies_data_base$distributors_2 = movies_data_base$distributor


# First distributor: Universal Pictures, which comprises of companies with Universal in the name
movies_data_base$distributors_2 <- gsub("MCA/?Universal.*|Universal.*",'Universal Pictures', 
                                        movies_data_base$distributors_2)


# Universal Picture is a subsidiary of NBC, so will pull out entries with NBC to investigate further
nbc_rows <- movies_data_base[grepl("National Broadcasting", movies_data_base$distributor, ignore.case = TRUE), ]
# Heist and Flipper are the films distributed by NBC. On research, Heist distributed by Lionsgate, Flipper by Universal Pictures by then

# Amending distributors_2 column for Flipper and Heist
movies_data_base <- movies_data_base %>% 
  mutate(distributors_2 = case_when(
    movie_title == "Heist" ~ "Lionsgate",
    movie_title == "Flipper" ~ "Universal Pictures",
    TRUE ~ distributors_2
  ))


# Cleaning up Walt Disney. Buena Vista was the old name of Disney, thus grouped here as well
movies_data_base$distributors_2 = gsub(".*Disney.*|.*Buena Vista.*", "Walt Disney Studios Motion Pictures", movies_data_base$distributors_2)

# Cleaning up Warner Bros
movies_data_base$distributors_2 = gsub("(?i)Warner.*", "Warner Bros.", movies_data_base$distributors_2)

# Cleaning up Sony, was previously Columbia Pictures/ Columbia Tristar/ Tristar Home
movies_data_base$distributors_2 = gsub("(?i)Sony.*|.*Columbia.*|.*Tristar.*", "Sony Pictures", movies_data_base$distributors_2)

# Cleaning up Paramount
movies_data_base$distributors_2 = gsub("(?i)Paramount.*", "Paramount Pictures", movies_data_base$distributors_2)

# Cleaning up Lionsgate
movies_data_base$distributors_2 = gsub("Lionsgate.*|Lions Gate.*", "Lionsgate", movies_data_base$distributors_2)

# Cleaning up 20th Century Fox
movies_data_base$distributors_2 = gsub(".*Fox.*", "Twentieth Century Fox", movies_data_base$distributors_2)

# Cleaning up MGM, comprising of orion
movies_data_base$distributors_2 = gsub(".*MGM.*|.*Metro Goldwyn.*|.*Orion.*", "Metro-Goldwyn-Mayer (MGM)", movies_data_base$distributors_2)

# Cleaning up Focus features
movies_data_base$distributors_2 = gsub(".*Focus.*", "Focus Features", movies_data_base$distributors_2)

# Cleaning up Summit Entertainment
movies_data_base$distributors_2 = gsub(".*Summit.*", "Summit Entertainment", movies_data_base$distributors_2)

# Good Universe, prev Mandate Pictures
movies_data_base$distributors_2 = gsub(".*Good Universe.*|.*Mandate.*", "Good Universe", movies_data_base$distributors_2)

# New Line Cinema, now part of warner bros
movies_data_base$distributors_2 = gsub(".*New Line.*", "New Line Cinema", movies_data_base$distributors_2)

# Hyde Park
movies_data_base$distributors_2 = gsub(".*Hyde Park.*", "Hyde Park Entertainment", movies_data_base$distributors_2)

# EuropaCorp
movies_data_base$distributors_2 = gsub(".*EuropaCorp.*", "EuropaCorp", movies_data_base$distributors_2)

# The reduction in categories:
length(unique(movies_data_base$distributor)) - length(unique(movies_data_base$distributors_2))

# Lets examine again the groups after the above changes
df_distributors_2 <- movies_data_base %>%
  group_by(distributors_2) %>%
  summarize(total_movies = n())

# Imposing descending order on the df
df_distributors_2 = df_distributors_2[order(-df_distributors_2$total_movies),]

# Calculate cumulative % to determine potential cut off for the num of dummies in the distributor column
df_distributors_2$cumulative_movies = cumsum(df_distributors_2$total_movies)/sum(df_distributors_2$total_movies)*100

# Consider the distributors with <5 movies as "others" (single category)-> they would represent (~20% of the sample); 
# and consider the rest distributors individually
# Create the list of distributors with < 5 movies
distributors_w_5 = df_distributors_2$distributors_2[df_distributors_2$total_movies<5]

# Re-classifying the distributor naming convention
movies_data_base$distributors_2 = ifelse(movies_data_base$distributors_2 %in% distributors_w_5,'Others',movies_data_base$distributors_2)

# Dummify re-classified column, and use Others as ref 
movies_data_base$distributors_2 <- as.factor(movies_data_base$distributors_2)
movies_data_base$distributors_2 <- relevel(movies_data_base$distributors_2, ref = 'Others')

table(movies_data_base$distributors_2)

# Test for significantly diff distributors from Others 
distri_reg <- lm(imdb_score~distributors_2,data=movies_data_base)
summary(distri_reg)


# Visualising the spread in score data across the distributor categories
plot = ggplot(data = movies_data_base, mapping = aes(x=distributors_2, y= imdb_score))
plot + geom_boxplot(fill = 'brown3') + labs(title = 'Boxplot: Distributors_2 vs IMBD score') + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))
table(movies_data_base$distributors_2)

# Considering the results of the linear regression, the companies that are not statistically different when compared to "Others" will be added in the category others
# Summit Entertainment is used as the ref point which is ranked 15 on the website

# Initiating variable that stores all the major distributors
significant_distributors = c(df_distributors_2$distributors_2[1:14],'HanWay Films', 'FilmNation Entertainment', "Gramercy Pictures (I) ",
                             'Dimension Films', 'Fine Line Features', "Mission Pictures International")

# Reclassifying the distributors again and dummifying them
movies_data_base$distributors_3 = ifelse(as.character(movies_data_base$distributors_2) %in% significant_distributors,as.character(movies_data_base$distributors_2),'Others')
movies_data_base$distributors_3 = as.factor(movies_data_base$distributors_3)
movies_data_base$distributors_3 = relevel(movies_data_base$distributors_3, ref = 'Others')
table(movies_data_base$distributors_3)

distri_reg2 <- lm(imdb_score~distributors_3,data=movies_data_base)
summary(distri_reg2)


# Drop title column now that it is no longer needed
add_cols_drop <- c("movie_title", "distributor", "production_company", "distributors_2")
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% add_cols_drop]




  #Director----
# List of famous directors https://www.imdb.com/list/ls052380992/
famous_directors = c(
  "Christopher Nolan", "David Fincher", "Ridley Scott", "Danny Boyle", 
  "Quentin Tarantino", "Roman Polanski", "Tim Burton", "Charles Chaplin", 
  "Sidney Lumet", "James Cameron", "John Ford", "Joel Coen", "Ingmar Bergman", 
  "David Lean", "Clint Eastwood", "Milos Forman", "Peter Jackson", 
  "John Huston", "Billy Wilder", "Woody Allen", "Francis Ford Coppola", 
  "Stanley Kubrick", "Alfred Hitchcock", "Martin Scorsese", "Steven Spielberg"
)

########### Understanding the subset of data comprising of films directed by famous directors to decide if it is a good idea to classify the directors that way
# Filter out observations with famous directors
fil_directors <- movies_data_base %>%
  filter(director %in% famous_directors)

# Dummify the director column
fil_directors$director <- as.factor(fil_directors$director)

# Creating a regression model to compare if there is statistically significant diff in imdb scores between these directors
model_dir <- lm(imdb_score~director, data=fil_directors)
summary(model_dir) # no statistical difference in scores between the directors! viable to group these directors into a single group!

# Create a new column director_fame: 1 if director is famous, 0 otherwise
movies_data_base$director_fame = ifelse(movies_data_base$director %in% famous_directors, 1, 0)

# Simple regression model for director_fame
fame_direct <- lm(imdb_score~director_fame,data=movies_data_base)
summary(fame_direct)


# Drop director column
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "director"]

  #Cinematographer----
# List of famous cinematographers https://www.musicgateway.com/blog/filmmaking/famous-cinematographers
famous_cinematographers = c(
  "Michael Chapman", "Roger Deakins", "Conrad Hall", "Janusz Kaminski", 
  "Ellen Kuras", "Emmanuel Lubezki", "Rachel Morrison", "John Seale", 
  "Vittorio Storaro", "Gregg Toland", "Gordon Willis"
)

#table(movies_data_base$cinematographer) # FYI there is a value "multiple"

########### Understanding the subset of data comprising of films w famous cinematographers to decide if it is a good idea to classify them that way
# Filter out observations with famous cinematographers
fil_cinemato <- movies_data_base %>%
  filter(cinematographer %in% famous_cinematographers)

# Dummify the cinematographer column
fil_cinemato$cinematographer <- as.factor(fil_cinemato$cinematographer)

# Creating a regression model to compare if there is statistically significant diff in imdb scores between these directors
model_cinema <- lm(imdb_score~cinematographer, data=fil_cinemato)
summary(model_cinema) 
# there is a statistical significant diff in imdb score for films involving gordon willis
# BUT he has since passed away. grouping the famous cinematographers into a single group will mostly reduce the accuracy of prediction for his films but for the rest of the directors, it should not be a huge impact as the coef are not significantly different
# FYI: Gordon willis was involved in just 4 films

# Create a new column cinemato_fame: 1 if cinematographer is famous, 0 otherwise
movies_data_base$cinemato_fame = ifelse(movies_data_base$cinematographer %in% famous_cinematographers, 1, 0)

# Simple regression model to compare score between the cinematographers
fame_cinema <- lm(imdb_score~cinemato_fame,data=movies_data_base)
summary(fame_cinema)


# Drop cinematographer
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% "cinematographer"]




  #Correlation between variables(multicollinearity)----
# Split predictors into quant and quali var
quant_var = movies_data_base[, sapply(movies_data_base,is.numeric)]
quali_var = movies_data_base[, sapply(movies_data_base,Negate(is.numeric))]

# Compute the correlation_matrix without predicted variable
quant_var = quant_var[, !colnames(quant_var) %in% "imdb_score"]
corr_matrix = cor(quant_var, use="complete.obs")

# Create a for loop to check which variables have certain degree of correlation 
threshold_corr = 0.3

for (i in 1:(ncol(corr_matrix) - 1)) {
  for (j in (i + 1):ncol(corr_matrix)) {
    if (abs(corr_matrix[i, j]) > threshold_corr) {
      cat("Correlation between", colnames(corr_matrix)[i], "and", colnames(corr_matrix)[j],
          "is", corr_matrix[i, j], "\n")
    }
  }
}

# 2nd check for multicollinearity among all quant var using vif
reg_vif <- lm(movies_data_base$imdb_score~., data = quant_var)

vif(reg_vif)

# Collinearity matrix values and vif are different so might need to be prudent in removing variables. Probably a better idea to only remove while the model is getting created!


  #Remove another useless variables----
columns_drop <- c("duration","actor1_star_meter","actor2_star_meter", "actor3_star_meter","nb_faces")
movies_data_base <- movies_data_base[, !colnames(movies_data_base) %in% columns_drop]
attach(movies_data_base)
#----


###Data modification for the model selection ----
movies_data <- movies_data_base
attach(movies_data)

test_data_movies=tail(movies_data, 12)#for final prediction
movies_data <- movies_data[1:(nrow(movies_data) - 12), ]
attach(movies_data)

#country_grp
movies_data$country_grp <- as.factor(movies_data$country_grp)
#release_month_num
movies_data$release_month_num <- as.factor(movies_data$release_month_num)
#movie_meter_IMBpro
movies_data = movies_data[(movies_data$movie_meter_IMDBpro > 100) & (movies_data$movie_meter_IMDBpro < 75000),]
#log_duration
movies_data <- movies_data[movies_data$log_duration > 4.0, ]
#Distributors
movies_data$distributors_3=as.factor(movies_data$distributors_3)
#nb_news_articles
movies_data = movies_data[movies_data$nb_news_articles>0,]
attach(movies_data)
#----

######Model selection######

#Rationality of each predictor ----

  #Duration: linear relationship with log duration poly 4
ggplot(data = movies_data, aes(x=log_duration, y=imdb_score)) +
  geom_point() + geom_smooth(method = 'glm', formula =y~poly(x,degree = 4)) + 
  labs(title = 'IMBD Score vs  Log Duration')

reg1 = lm(imdb_score ~ poly(log_duration,4))
summary(reg1)
residualPlots(reg1) #Linear relationship with a level of confidence of 95% for log duration poly 4


  #movie meter IMBD pro: linear relationship with the log movie_meter_IMBDpro
ggplot(data = movies_data, aes(x=log(movie_meter_IMDBpro), y=imdb_score)) +
  geom_point() + geom_smooth(method = 'lm') + 
  labs(title = 'IMBD Score vs  Log Movie Meter IMDB Pro')

reg1 = lm(imdb_score ~ log(movie_meter_IMDBpro))
summary(reg1)
residualPlots(reg1) #Linear relationship with a level of confidence of 95% for log movie_meter_IMBDpro

  #nb_news_articles: linear relationship with log news poly 2
ggplot(data = movies_data, aes(x=log(nb_news_articles), y=imdb_score)) +
  geom_point() + geom_smooth(method = 'glm', formula =y~poly(x,degree = 2)) + 
  labs(title = 'IMBD Score vs  Log News articles')

reg1 = lm(imdb_score ~ poly(log(nb_news_articles),2))
summary(reg1)
residualPlots(reg1) #Linear relationship with a level of confidence of 95% for log duration poly 2

  #avg_actors: linear relationship with the log avg_actors
ggplot(data = movies_data, aes(x=log(avg_actors), y=imdb_score)) +
  geom_point() + geom_smooth(method = 'lm') + 
  labs(title = 'IMBD Score vs  Average Actors')

reg1 = lm(imdb_score ~ log(avg_actors))
summary(reg1)
residualPlots(reg1) #Linear relationship with a level of confidence of 95% for log movie_meter_IMBDpro

#----

#Interactions between variables ----

  #Log(budget) and Log(movie_meter_IMBDpro)
ggplot(data = movies_data, aes(x=log(movie_meter_IMDBpro), y=log(movie_budget))) +
  geom_point() + geom_smooth(method = 'lm') + 
  labs(title = 'Log Budget vs  Log Movie Meter IMDB Pro')

reg1 = lm(imdb_score~log(movie_meter_IMDBpro)* log(movie_budget) )
reg2 = lm(imdb_score~log(movie_meter_IMDBpro)+ log(movie_budget) )
summary(reg1)
summary(reg2)
  #The R2 adjusted dont change significantly and predictors loss significance

  #Log(movie_meter_IMDBpro) and release_month_num
reg1 = lm(imdb_score~log(movie_meter_IMDBpro)* release_month_num )
reg2 = lm(imdb_score~log(movie_meter_IMDBpro)+ release_month_num )
summary(reg1)
summary(reg2)
  #The R2 adjusted increases when there is interaction from 0.2079 to 0.2098

  #Log(movie_meter_IMDBpro) and country_grp
reg1 = lm(imdb_score~log(movie_meter_IMDBpro)* country_grp )
reg2 = lm(imdb_score~log(movie_meter_IMDBpro)+ country_grp )
summary(reg1)
summary(reg2)
  #The R2 adjusted increases when there is interaction from 0.211 to 0.2165 and is significant the interactions

  #Log( duration) and country_grp
reg1 = lm(imdb_score~log_duration* country_grp )
reg2 = lm(imdb_score~log_duration+ country_grp )
summary(reg1)
summary(reg2)
  #The R2 adjusted increases when there is interaction from 0.1989 to 0.2039 and is significant the interactions

#----

#Model ----

fit=glm(imdb_score~poly(log_duration ,degree = 4) +
          log(movie_meter_IMDBpro)*country_grp + log(movie_meter_IMDBpro)*release_month_num +
          log_duration*country_grp + log(movie_budget)*log(movie_meter_IMDBpro) +
          distributors_3 + director_fame + cinemato_fame +
          Drama+ Biography + Sport + Horror + Thriller + Comedy + Action + Music + Family + Western+ Animation + Documentary+
          maturity_cat+poly(log(nb_news_articles),2)+faces+log(avg_actors))
mse=cv.glm(movies_data, fit, K=10)$delta[1]
mse #0.6353

  #Check for the model performance with all the data 
prediction <- predict(fit, movies_data)
movies_data$prediction= prediction

ggplot() +
  geom_point(data = movies_data, aes(x = imdb_score, y = imdb_score), color = "blue") +
  geom_point(data = movies_data, aes(x = prediction, y = imdb_score), color = "red") +
  labs(title = "Scatter Plot of IMDb Score vs prediction",
       x = "Prediction",
       y = "Real IMDb Score") +
  theme_minimal()
  #We see that this model performs well in medium values while showing less accuracy in the lowest and highest values
#----

#Model problems: Outliers, underfitting and overfitiing ----

  #Outliers
fitted_values <- fitted(fit)
studentized_residuals <- rstudent(fit)
ggplot(data = data.frame(fitted_values, studentized_residuals), aes(x = fitted_values, y = studentized_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 3, color = 'red') +
  geom_hline(yintercept = -3, color = 'red') + scale_y_continuous(breaks = seq(-4,4,by=1))

  #We remove the outliers that are 3 std away from the mean
movies_data$studentized_residuals <- studentized_residuals
movies_data2 <- movies_data[movies_data$studentized_residuals >= -3.00 & movies_data$studentized_residuals <= 3.00,]
attach(movies_data2)

#Final model
fit=glm(imdb_score~poly(log_duration ,degree = 4) +
          log(movie_meter_IMDBpro)*country_grp + log(movie_meter_IMDBpro)*release_month_num +
          log_duration*country_grp + log(movie_budget)*log(movie_meter_IMDBpro) +
          distributors_3 + director_fame + cinemato_fame +
          Drama+ Biography + Sport + Horror + Thriller + Comedy + Action + Music + Family + Western+ Animation + Documentary+
          maturity_cat+poly(log(nb_news_articles),2)+faces+log(avg_actors))
mse=cv.glm(movies_data2, fit, K=10)$delta[1]
mse #0.46

# White test for heteroskedasticity
white_test <- bptest(fit, ~ fitted(fit) + I(fitted(fit)^2))
print(white_test)
#Because p-value is less than 0.05 there is heteroskedasticity

#Check for the model performance with all the data 
prediction <- predict(fit, movies_data2)
movies_data2$prediction= prediction

ggplot() +
  geom_point(data = movies_data2, aes(x = imdb_score, y = imdb_score), color = "blue") +
  geom_point(data = movies_data2, aes(x = prediction, y = imdb_score), color = "red") +
  labs(title = "Scatter Plot of IMDb Score vs prediction",
       x = "Prediction",
       y = "Real IMDb Score") +
  theme_minimal()
#----

####Prediction model for 12 movies----

# Predict with confidence intervals
predictions_with_ci <- predict(fit, test_data_movies, se.fit = TRUE, type = "response")
test_data$prediction <- predictions_with_ci$fit  # Predicted values
test_data$se <- predictions_with_ci$se.fit         # Standard error of predictions


# Calculate the 95% confidence interval
# CI = prediction ± 1.96 * standard error (assuming normal distribution)
alpha <- 1.96

test_data$lwr <- predictions_with_ci$fit - alpha * predictions_with_ci$se.fit  # Lower bound
test_data$upr <- predictions_with_ci$fit + alpha * predictions_with_ci$se.fit  # Upper bound

test_data= test_data[, c("movie_title", "prediction","se","lwr","upr")]


#----



