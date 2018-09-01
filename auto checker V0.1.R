#an initial attempt ata  model basedo n a few variables

library(caret)
library(ggplot2)
library(quanteda)
library(parallel)

setwd("C:/Users/elkst/Documents/R/work stuff/auto checker")

######################### read in data a a bit of prep
data <- read.csv("C:/Users/elkst/Documents/R/work stuff/auto checker/Production report W.csv",
                 stringsAsFactors = FALSE)

#remove unwanted columns
data <- data[,c('Script_Total_Original', 'Writing.response.text', 'Module')]

#remove unwanted rows
data <- data[-(1:18),]

data <- data[-which(data$Script_Total_Original=='NULL'),]

#change encoding to latin script1
Encoding(data$Writing.response.text) <- 'latin1'

#check proportions of each score
prop.table(table(data$Script_Total_Original))

######################### create train and test set
set.seed(32984)

###### create indexes for trianing set
indexes <- createDataPartition(data$Script_Total_Original, times = 1,
                               p = 0.7, list = FALSE)

#assign training indexes to training set
train <- data[indexes,]
#assign non training indexes to test set
test <- data[-indexes,]

######################### feature engineering for training
train$Num_words <- sapply(train$Writing.response.text, function(x){
  tokens <- unlist(strsplit(x, ' '))
  return(length(tokens))
})

train$Lexical_diversity <- sapply(train$Writing.response.text, function(x){
  tokens <- unlist(strsplit(x, ' '))
  num_tokens <- unique(tokens)
  return(length(tokens)/length(num_tokens))
})

######################### train model

set.seed(48743)
#creates stratified random samples (used as index in next step)
cv.folds <- createMultiFolds(train$Writing.response.text, k = 5, times = 3)

#creates control file used in next setps of model creation
cv.cntrl <- trainControl(method = 'repeatedcv', number = 5,
                         repeats = 3, index = cv.folds)

# have reached this point. just need to run the model
# using doSNOW package allows parallel fold training

# to time execution of code
start.time <- Sys.time()

# createa a cluster ot worrk on 10  logical cores
cl <- makeCluster(detectCores()-1, type = 'SOCK')
registerDoSNOW(cl)

rpart.cv.1 <- train(Writing.response.text ~ c() , data = train.tokens.df, method = 'rpart',
                    trControl = cv.cntrl, tuneLength = 5
)

stopCluster(cl)

total.time <- Sys.time() - start.time

