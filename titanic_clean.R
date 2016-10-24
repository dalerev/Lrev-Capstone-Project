library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

setwd("C:/Users/Louise/Documents/R/tutorials/assignment resources")


# Loading dataset
titanic <- read_csv("titanic_original.csv", col_names = TRUE)
glimpse(titanic)
titanic


# Functions:
fill_NA <- function(x, y) {
  lapply(x, str_replace_na, replacement = "NA") %>%
    str_replace(pattern = "NA", replacement = y)
}

check_NA <- function(x, get.NA = TRUE) {
  a <- map_int(x, is.na)
  if(get.NA == TRUE) {
    index <- grep(1L, a, value = FALSE)
    print(index)
    print(paste0("No. of NAs: ", length(index)))
  } else if(get.NA == FALSE) {
    index <- grep(0L, a, value = FALSE)
    print(index)
    print(paste0("No. of elements: ", length(index)))
  }
}
  

# 1. PORT OF EMBARKATION: Finding missing values and replacing them with S

str(titanic)
any(is.na(titanic$embarked))
titanic$embarked %>%
  map_int(is.na) %>%
  grep(1L,., value = FALSE) -> i #%>% # finding indices: c(169,285, 1310)
  length() # number of NA's
titanic$embarked[i]

NA_embarked <- titanic$embarked %>%
  lapply(str_replace_na, replacement = "NA") %>%
  str_replace(pattern = "NA", replacement = "S")
NA_embarked -> titanic$embarked
titanic$embarked[i] # final check
titanic$embarked


# 2. AGE : Filling in missing values

str(titanic$age) # titanic$age returns floating numbers/numeric

check_NA(titanic$age, get.NA = TRUE)
(age_mean <- mean(titanic$age, na.rm = TRUE)) # mean = 29.88113
(age_median <- median(titanic$age, na.rm = TRUE)) # median = 28

# it isn't the convention to round "up" age to get to a 2 s.f. number
# changing floats(output of mean/median function) to integers 
(titanic$age <- as.integer(titanic$age)) 
(age_mean <- as.integer(mean(titanic$age, na.rm = TRUE))) # mean = 29 
(age_median <- as.integer(median(titanic$age, na.rm = TRUE))) # median = 28
(titanic$age <- as.integer(fill_NA(titanic$age, age_mean)))


# 3. LIFEBOAT: Filling in missing values with "NA"

str(titanic$boat)
check_NA(titanic$boat, get.NA = TRUE) 
(titanic$boat <- fill_NA(titanic$boat, "NA")) 


# 4. CABIN: Finding missing values

check_NA(titanic$cabin, get.NA = TRUE)
  # only 295 recorded values, and 1015 NA's. Possible to omit rows starting from [1250]
  # but would rather fill in NAs . It isn't required for calculation, and no possible
  # way to derive replacement values from other observations

(titanic$cabin <- fill_NA(titanic$cabin, "NA")) 


# 5. CABIN: Creating binary columns

dftest <- titanic

dftest <- dftest %>% mutate(has_cabin_number = as.integer(grepl("[^NA]", cabin)))
names(dftest)
dftest$has_cabin_number
dftest -> titanic 
names(titanic)
titanic <- titanic[, c("pclass", "survived","has_cabin_number", "name", "sex", "age", "sibsp", "parch", 
                       "ticket", "fare", "cabin", "embarked", "boat", "body", "home.dest")]
names(titanic)
head(titanic, n = 10)