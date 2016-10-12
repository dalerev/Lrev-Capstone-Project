library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
setwd("C:/Users/Louise/Documents/R/tutorials/assignment resources")

options(dyplr.width = Inf, dyplr.print_min = 10)



#Loading dataset
refinedf <- read_csv("refine_original.csv", col_names = TRUE)
dim(refinedf)
refinedf



#View
refinedf %>% View()



#inspecting structure
str(refinedf)



#Switching all characters to lowercase
refinedf <- refinedf %>% mutate_each(funs(tolower))
refinedf



#spelling corrections
df <- refinedf$company %>%
  gsub(pattern = "(^[ak].*?[o|0]$)", replacement = "akzo", .) %>%
  gsub(pattern = "(^[f|p].*?[s]$)", replacement = "philips", .) %>%
  gsub(pattern = "(^[u].*?[r]$)", replacement = "unilever", .) %>%
  gsub(pattern = "(^[v].*?[ten]$)", replacement = "van_houten", .)
df -> refinedf$company  
refinedf$company

  

#renaming
names(refinedf)[2] <- "product_code"



#separating column "product_code/number" into two columns
(refinedf <- refinedf %>% separate(product_code, c("product_code", "number"), sep = "-", remove = TRUE))



#geocoding
(refinedf <- refinedf %>% unite("address", c(address, city, country), sep = " , ") %>% arrange(company))



#new column: product_category
newcol <- refinedf %>% mutate(product_category = product_code)
newcol
newcol <- newcol[, c("company", "product_code", "product_category", "number", "address", "name")] #-> 
refinedf <- newcol
refinedf

newrepl <- refinedf$product_category %>%
  str_replace("p", "smartphone") %>%
  str_replace("q", "tablet") %>%
  str_replace("v", "tv") %>%
  str_replace("x", "laptop")
newrepl -> refinedf$product_category
refinedf$product_category



#add four binary(1,0) columns for company
split_company <- refinedf %>%
  mutate(value = 1,
         company = paste0("company_", company)) %>%
  spread(company, value, fill = 0)
split_company -> refinedf



#add four binary(1,0) columns for product_category: product_smartphone, product_tv, product_laptop, product_tablet
split_category <- refinedf %>%
  mutate(value = 1,
         product_category = paste0("product_", product_category)) %>%
  spread(product_category, value, fill = 0)
split_category -> refinedf
refinedf  


#inspecting structure of transformed dataframe
str(refinedf)


