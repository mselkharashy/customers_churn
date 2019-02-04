library(readr)
library(tidyverse)
library(corrplot)

dset <<- read_csv("~/Documents/workspace/Customer Churn/data/Churn_Modelling.csv")

glimpse(dset)
summary(dset)
n_distinct(dset$CustomerId)
table(dset$Exited)

(sapply(dset, anyNA))
(sapply(dset, n_distinct))
(sapply(dset %>% select(Geography, Gender, NumOfProducts, HasCrCard, IsActiveMember, Exited), table))

dset %>%
  mutate_if(is.character, factor) %>%
  mutate(NumOfProducts = as.factor(NumOfProducts), 
         HasCrCard = as.factor(HasCrCard),
         IsActiveMember = as.factor(IsActiveMember), 
         Exited = as.factor(Exited)) -> dset


glimpse(dset)

dset %>% ggplot(aes(x = Exited, fill = Geography)) + geom_bar()
dset %>% ggplot(aes(x = Exited, fill = Gender)) + geom_bar()
dset %>% ggplot(aes(x = Exited, fill = NumOfProducts)) + geom_bar()
dset %>% ggplot(aes(x = Exited, fill = HasCrCard)) + geom_bar()
dset %>% ggplot(aes(x = Exited, fill = IsActiveMember)) + geom_bar()

hist(dset$Age)
dset %>% ggplot(aes(x = Age, fill = Exited)) + geom_histogram(binwidth = 5)

dset %>% select_if(is.numeric) %>% select(-CustomerId, -RowNumber) %>% cor() %>% corrplot()

dset %>%
  mutate(Age = cut(Age, br = c(0,30,60,93),labels=c('Young','Adult','Old'))) %>%
  mutate(Tenure = cut(Tenure, br= c(-1,3,7,10), labels=c('Beginner','Growing','Mature')))


