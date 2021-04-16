run_analysis <- function(){

library(tidyverse)  
library(data.table)
library(stringi)  
  
  ## Reading, Merging and Renaming:
  
  testdata <- read.table(file = "test/X_test.txt")
  traindata <- read.table(file = "train/X_train.txt")
  features <- read.table(file = "features.txt") ## read data via data.table
  subject_train <- read.table(file = "train/subject_train.txt")
  subject_test <- read.table(file = "test/subject_test.txt")
  trainactivity <- read.table(file = "train/y_train.txt")
  testactivity <- read.table(file = "test/y_test.txt")
  
  testdata <- mutate(testdata, Activity = as.factor(as.vector(unlist(testactivity))))   ## Adding the subject IDs
  traindata <- mutate(traindata, Activity = as.factor(as.vector(unlist(trainactivity)))) ## Coercion to avoid nested data.frames
  testdata <- mutate(testdata,Subject.ID = as.factor(as.vector(unlist(subject_test))))   ## Adding the subject IDs
  traindata <- mutate(traindata,Subject.ID = as.factor(as.vector(unlist(subject_train)))) ## Coercion to avoid nested data.frames
  
  fulldata <- rbind(testdata,traindata) # merging the training and testset
  features <- rbind(features,c(562, "Activity") , c(563, "Subject.ID"))
  
  names(fulldata) <- features[,2] # renaming the columns by the features
  
  ## Processing
  relevantcolumns <- NULL
  for (i in 1:dim(features)[1]){
      if (stri_detect_fixed(features[i,2],"mean(") ==TRUE  | stri_detect_fixed(features[i,2],"std")==TRUE | stri_detect_fixed(features[i,2],"Subject.ID")==TRUE | stri_detect_fixed(features[i,2],"Activity")==TRUE){
          relevantcolumns <- rbind(relevantcolumns,features[i,2])  ## filtering out the relevant columns                                                           ## ("mean(" to eliminate the meanFreq-items)
      }
  }
  
relevantcolumns <- as.vector(relevantcolumns) ## converting to vector to be able to subset with select
processeddata <<- select(fulldata, relevantcolumns) ## subsetting

## Cleaning Names

names(processeddata) <- make.names(names(processeddata)) ##Making var-names uniform
## As the variables do not seem to get any more descriptive without further description, 
## by simply writing them out, I will refrain from further renaming here and refer to the codebook.

write.table(processeddata, file="processeddata.txt") # This completes the first data set
                                                     # from which, the second will be produced

## Creating second Dataset
naming <- NULL


activities <- c("Walk","Walk.Up", "Walk.Down","Sit", "Stand", "Lay")



for(j in 1:6){
    for(k in 1:30){
       rowselection <- filter(processeddata, Subject.ID == k & Activity == j)
       meanscolumn <- lapply(rowselection,mean)[-c(67,68)]
       meansprocessed <- cbind(meansprocessed, meanscolumn)
       naming <- rbind(naming, paste("Means.for.Subj", k, activities[j], sep = "."))
   }
}

meansprocessed <- t(meansprocessed) ## transposing to 
rownames(meansprocessed) <- naming  ## renaming Rows 

write.table(meansprocessed, "MeansbySubjectandActivity.txt")
view(meansprocessed)

 }
