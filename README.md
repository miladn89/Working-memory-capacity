# Working-memory-capacity

1.OPERATIONAL MEMORY CAPACITY

```{r}
library(dplyr)
library(readr)
library(readxl)
rawdata <- read.csv("C:\\Users\\user\\Downloads\\working memory raw data.csv")
data_nona <- subset(rawdata, !is.na(rawdata$trial.digit_span.load))
users <- unique(data_nona$session.subject.subject.code)
num_users <-length(users)
freq_users <- c(1:length(users))
user_num_of_trial <-c(1:length(users))

for (i in 1:length(users)) {freq_users[i] <- length(which(data_nona$session.subject.subject.code==users[i]))
user_num_of_trial_each_span <- data_nona %>% filter(session.subject.subject.code==users[i]& module.name=="Operation Span (digits)")%>% arrange(trial.digit_span.load)%>%
  summarize(tri2=sum(trial.digit_span.load==2)/2,tri3=sum(trial.digit_span.load==3)/3,tri4=sum(trial.digit_span.load==4)/4,tri5=sum(trial.digit_span.load==5)/5,tri6=sum(trial.digit_span.load==6)/6,tri7=sum(trial.digit_span.load==7)/7)
user_num_of_trial[i] <-sum(user_num_of_trial_each_span)
}


#set the working directory to the folder that is holding the data files from the operation span task


###function that takes the output file for a participant and returns summary information for that person###

operation_span_processing <- function(data.frame,n) {
  
  #seperate processing trials from digit span trials
  
  df.recall <- subset(data.frame, !is.na(trial.digit_span.load))
  df.operations <- subset(data.frame, is.na(trial.digit_span.load))
  df.recall <- df.recall %>% filter(module.name== "Operation Span (digits)")
  df.operations <- df.operations %>% filter(module.name=="Operation Span (digits)")
  if (nrow(df.recall)==0) {print("No Operation span")
  } else {
  #standard output form digit span has each element of a trial as a row, 
  #also need to collate these into success/failure for whole trials.
  num.trials <- user_num_of_trial[n]
  #set these vectors up now, to be filled later.
  trial.load <- numeric(num.trials)
  num.corr <- numeric(num.trials)
  prop.corr <- numeric(num.trials)
  trial.success <- numeric(num.trials)
  
  j <- 1
  for (i in 1:num.trials) {
    if (df.recall$trial.digit_span.load[j]==2){
    indi_trial <- df.recall$trial.digit_span.result[j:(j+1)]
    j <- j+2}
    else if(df.recall$trial.digit_span.load[j]==3){
      indi_trial <- df.recall$trial.digit_span.result[j:(j+2)] 
      j <- j+3}
    else if(df.recall$trial.digit_span.load[j]==4){
      indi_trial <- df.recall$trial.digit_span.result[j:(j+3)] 
      j <- j+4}
    else if(df.recall$trial.digit_span.load[j]==5){
      indi_trial <- df.recall$trial.digit_span.result[j:(j+4)] 
      j <- j+5}
    else if(df.recall$trial.digit_span.load[j]==6){
      indi_trial <- df.recall$trial.digit_span.result[j:(j+5)] 
      j <- j+6}
    else if(df.recall$trial.digit_span.load[j]==7){
      indi_trial <- df.recall$trial.digit_span.result[j:(j+6)] 
      j <- j+7}
    numDigits <- length(indi_trial)
    numCorr <- sum(indi_trial == "success")
    trial.success[i] <- numDigits == numCorr
    trial.load[i] <- numDigits
    num.corr[i] <- numCorr
    prop.corr[i] <- numCorr / numDigits
  }	
  
  df.recall.wholeTrial <- as.data.frame(cbind(trial.load, num.corr, prop.corr, trial.success))
  df.recall.wholeTrial <- transform(df.recall.wholeTrial, score = trial.load * trial.success)
  
  #highest load with a fully correct response
  if (length(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) == 0) {
    #no successes
    max.span <- NA
  } else {
    max.span <- max(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) 
  }
  
  #total number of words correctly recalled in correct serial position throughout
  number.successes <- sum(data.frame$trial.digit_span.result == "success")
  #span two trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 2)) == 0) {
    #then there were no span size 2 trials, therefore
    span.2.corr <- NA
  } else {
    span.2.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 2])
  }
  #span three trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 3)) == 0) {
    #then there were no span size 3 trials, therefore
    span.3.corr <- NA
  } else {
    span.3.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 3])
  }	
  #span four trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 4)) == 0) {
    #then there were no span size 4 trials, therefore
    span.4.corr <- NA
  } else {
    span.4.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 4])
  }	
  #span five trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 5)) == 0) {
    #then there were no span size 5 trials, therefore
    span.5.corr <- NA
  } else {
    span.5.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 5])
  }
  #span six trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 6)) == 0) {
    #then there were no span size 6 trials, therefore
    span.6.corr <- NA
  } else {
    span.6.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 6])
  }	
  #span seven trials correct
  if (nrow(subset(df.recall.wholeTrial, trial.load == 7)) == 0) {
    #then there were no span size 7 trials, therefore
    span.7.corr <- NA
  } else {
    span.7.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 7])
  }	
  
  fta.score <- sum(df.recall.wholeTrial$score)
  prop.score <- mean(df.recall.wholeTrial$prop.corr)
  number.successes <- sum(df.recall$trial.digit_span.result == "success")
  
  processing.accuracy <- sum(df.operations$trial.operation_processing.result == "success") / nrow(df.operations)
  processing.median.rt <- median(df.operations$trial.operation_processing.durationTime)
  
  
  p.summary <- c(fta.score, prop.score, number.successes, processing.accuracy, processing.median.rt, 
                 max.span, span.2.corr, span.3.corr, span.4.corr, span.5.corr, span.6.corr, span.7.corr)
  
  return(p.summary)
  }
}

##now can use that function on all our data files for operation span
#put all operation span files into one folder and set that folder as working directory

compile_operation_span_data <- function(filestring) {
  
  raw.data <- read.csv(filestring)
  ps <- unique(raw.data$session.subject.subject.code)
  operation.span.data <- matrix(nrow=length(ps), ncol=12) 	
  
  
  for (i in 1:length(ps)) {
    
    #subset data frame with first unique subj code
    sub.raw.data <- subset(raw.data, session.subject.subject.code == ps[i])
    
    tmp.os.summary <- operation_span_processing(sub.raw.data,i)
    
    operation.span.data[i, ] <- tmp.os.summary
    
  }
  
  operation.span.data <- as.data.frame(operation.span.data)
  operation.span.data$user <- ps
  
  os.names <- c("fta.score", "prop.score", "number.successes", "processing.accuracy", "processing.median.rt", 
                "max.span", "span.2.corr", "span.3.corr", "span.4.corr", "span.5.corr", "span.6.corr", "span.7.corr", "pcode")
  
  names(operation.span.data) <- os.names
  
  
  return(operation.span.data)	
  
}

operation.span.data <- compile_operation_span_data("C:\\Users\\user\\Downloads\\working memory raw data.csv")

write.csv(operation.span.data,"C:\\Users\\user\\Downloads\\operation.span.data.csv")
#rm(compile_operation_span_data, operation_span_processing)
```


OPERATIONAL MEMORY Higher Accuracy

```{r}
library(dplyr)
library(readr)
library(readxl)
rawdata <- read.csv("C:\\Users\\user\\Downloads\\working memory raw data.csv")
data_nona <- subset(rawdata, !is.na(rawdata$trial.digit_span.load))
users <- unique(data_nona$session.subject.subject.code)
num_users <-length(users)
freq_users <- c(1:length(users))
user_num_of_trial <-c(1:length(users))

for (i in 1:length(users)) {freq_users[i] <- length(which(data_nona$session.subject.subject.code==users[i]))
user_num_of_trial_each_span <- data_nona %>% filter(session.subject.subject.code==users[i]& module.name=="Operation Span (digits)" & trial.digit_span.load %in% c(2,3,4,5))%>% arrange(trial.digit_span.load)%>%
  summarize(tri2=sum(trial.digit_span.load==2)/2,tri3=sum(trial.digit_span.load==3)/3,tri4=sum(trial.digit_span.load==4)/4,tri5=sum(trial.digit_span.load==5)/5)
user_num_of_trial[i] <-sum(user_num_of_trial_each_span)
}


#set the working directory to the folder that is holding the data files from the operation span task


###function that takes the output file for a participant and returns summary information for that person###

operation_span_processing_no6and7 <- function(data.frame,n) {
  
  #seperate processing trials from digit span trials
  
  df.recall <- subset(data.frame, !is.na(trial.digit_span.load))
  df.operations <- subset(data.frame, is.na(trial.digit_span.load))
  df.recall <- df.recall %>% filter(module.name== "Operation Span (digits)" & trial.digit_span.load %in% c(2,3,4,5))
  df.operations <- df.operations %>% filter(module.name=="Operation Span (digits)"& trial.digit_span.load2 %in% c(2,3,4,5))
  if (nrow(df.recall)==0) {print("No Operation span")
  } else {
    #standard output form digit span has each element of a trial as a row, 
    #also need to collate these into success/failure for whole trials.
    num.trials <- user_num_of_trial[n]
    #set these vectors up now, to be filled later.
    trial.load <- numeric(num.trials)
    num.corr <- numeric(num.trials)
    prop.corr <- numeric(num.trials)
    trial.success <- numeric(num.trials)
    
    j <- 1
    for (i in 1:num.trials) {
      if (df.recall$trial.digit_span.load[j]==2){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+1)]
        j <- j+2}
      else if(df.recall$trial.digit_span.load[j]==3){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+2)] 
        j <- j+3}
      else if(df.recall$trial.digit_span.load[j]==4){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+3)] 
        j <- j+4}
      else if(df.recall$trial.digit_span.load[j]==5){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+4)] 
        j <- j+5}
  
      numDigits <- length(indi_trial)
      numCorr <- sum(indi_trial == "success")
      trial.success[i] <- numDigits == numCorr
      trial.load[i] <- numDigits
      num.corr[i] <- numCorr
      prop.corr[i] <- numCorr / numDigits
    }	
    
    df.recall.wholeTrial <- as.data.frame(cbind(trial.load, num.corr, prop.corr, trial.success))
    df.recall.wholeTrial <- transform(df.recall.wholeTrial, score = trial.load * trial.success)
    
    #highest load with a fully correct response
    if (length(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) == 0) {
      #no successes
      max.span <- NA
    } else {
      max.span <- max(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) 
    }
    
    #total number of words correctly recalled in correct serial position throughout
    number.successes <- sum(data.frame$trial.digit_span.result == "success")
    #span two trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 2)) == 0) {
      #then there were no span size 2 trials, therefore
      span.2.corr <- NA
    } else {
      span.2.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 2])
    }
    #span three trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 3)) == 0) {
      #then there were no span size 3 trials, therefore
      span.3.corr <- NA
    } else {
      span.3.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 3])
    }	
    #span four trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 4)) == 0) {
      #then there were no span size 4 trials, therefore
      span.4.corr <- NA
    } else {
      span.4.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 4])
    }	
    #span five trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 5)) == 0) {
      #then there were no span size 5 trials, therefore
      span.5.corr <- NA
    } else {
      span.5.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 5])
    }
    
    fta.score <- sum(df.recall.wholeTrial$score)
    prop.score <- mean(df.recall.wholeTrial$prop.corr)
    number.successes <- sum(df.recall$trial.digit_span.result == "success")
    
    processing.accuracy <- sum(df.operations$trial.operation_processing.result == "success") / nrow(df.operations)
    processing.median.rt <- median(df.operations$trial.operation_processing.durationTime)
    
    
    p.summary <- c(fta.score, prop.score, number.successes, processing.accuracy, processing.median.rt, 
                   max.span, span.2.corr, span.3.corr, span.4.corr, span.5.corr)
    
    return(p.summary)
  }
}

##now can use that function on all our data files for operation span
#put all operation span files into one folder and set that folder as working directory

compile_operation_span_data_no6and7 <- function(filestring) {
  
  raw.data <- read.csv(filestring)
  ps <- unique(raw.data$session.subject.subject.code)
  operation.span.data <- matrix(nrow=length(ps), ncol=10) 	
  
  
  for (i in 1:length(ps)) {
    
    #subset data frame with first unique subj code
    sub.raw.data <- subset(raw.data, session.subject.subject.code == ps[i])
    
    tmp.os.summary <- operation_span_processing_no6and7(sub.raw.data,i)
    
    operation.span.data[i, ] <- tmp.os.summary
    
  }
  
  operation.span.data <- as.data.frame(operation.span.data)
  operation.span.data$user <- ps
  
  os.names <- c("fta.score", "prop.score", "number.successes", "processing.accuracy", "processing.median.rt", 
                "max.span", "span.2.corr", "span.3.corr", "span.4.corr", "span.5.corr", "pcode")
  
  names(operation.span.data) <- os.names
  
  
  return(operation.span.data)	
  
}

operation.span.data.No6and7 <- compile_operation_span_data_no6and7("C:\\Users\\user\\Downloads\\working memory raw data.csv")

write.csv(operation.span.data.No6and7,"C:\\Users\\user\\Downloads\\operation.span.data.No6and7.csv")
#rm(compile_operation_span_data, operation_span_processing)
```

READING MEMORY CAPACITY

```{r}
library(dplyr)
library(readr)
library(readxl)
rawdata <- read.csv("C:\\Users\\user\\Downloads\\working memory raw data.csv")
data_nona <- subset(rawdata, !is.na(rawdata$trial.digit_span.load))
users <- unique(data_nona$session.subject.subject.code)

num_users <-length(users)
freq_users <- c(1:length(users))
user_num_of_trial <-c(1:length(users))

for (i in 1:length(users)) {freq_users[i] <- length(which(data_nona$session.subject.subject.code==users[i]))
user_num_of_trial_each_span <- data_nona %>% filter(session.subject.subject.code==users[i]& module.name=="Reading Span (digits)")%>% arrange(trial.digit_span.load)%>%
  summarize(tri2=sum(trial.digit_span.load==2)/2,tri3=sum(trial.digit_span.load==3)/3,tri4=sum(trial.digit_span.load==4)/4,tri5=sum(trial.digit_span.load==5)/5,tri6=sum(trial.digit_span.load==6)/6,tri7=sum(trial.digit_span.load==7)/7)
user_num_of_trial[i] <-sum(user_num_of_trial_each_span)
}


#set the working directory to the folder that is holding the data files from the operation span task


###function that takes the output file for a participant and returns summary information for that person###

reading_span_processing <- function(data.frame,n) {
  
  #seperate processing trials from digit span trials
  
  df.recall <- subset(data.frame, !is.na(trial.digit_span.load))
  df.sentences  <- subset(data.frame, is.na(trial.digit_span.load))
  df.recall <- df.recall %>% filter(module.name== "Reading Span (digits)")
  df.sentences  <- df.sentences  %>% filter(module.name=="Reading Span (digits)")
  if (nrow(df.recall)==0) {print("No Reading span")
  } else {
    #standard output form digit span has each element of a trial as a row, 
    #also need to collate these into success/failure for whole trials.
    num.trials <- user_num_of_trial[n]
    #set these vectors up now, to be filled later.
    trial.load <- numeric(num.trials)
    num.corr <- numeric(num.trials)
    prop.corr <- numeric(num.trials)
    trial.success <- numeric(num.trials)
    
    j <- 1
    for (i in 1:num.trials) {
      if (df.recall$trial.digit_span.load[j]==2){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+1)]
        j <- j+2}
      else if(df.recall$trial.digit_span.load[j]==3){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+2)] 
        j <- j+3}
      else if(df.recall$trial.digit_span.load[j]==4){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+3)] 
        j <- j+4}
      else if(df.recall$trial.digit_span.load[j]==5){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+4)] 
        j <- j+5}
      else if(df.recall$trial.digit_span.load[j]==6){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+5)] 
        j <- j+6}
      else if(df.recall$trial.digit_span.load[j]==7){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+6)] 
        j <- j+7}
      numDigits <- length(indi_trial)
      numCorr <- sum(indi_trial == "success")
      trial.success[i] <- numDigits == numCorr
      trial.load[i] <- numDigits
      num.corr[i] <- numCorr
      prop.corr[i] <- numCorr / numDigits
    }	
    
    df.recall.wholeTrial <- as.data.frame(cbind(trial.load, num.corr, prop.corr, trial.success))
    df.recall.wholeTrial <- transform(df.recall.wholeTrial, score = trial.load * trial.success)
    
    #highest load with a fully correct response
    if (length(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) == 0) {
      #no successes
      max.span <- NA
    } else {
      max.span <- max(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) 
    }
    
    #total number of words correctly recalled in correct serial position throughout
    number.successes <- sum(data.frame$trial.digit_span.result == "success")
    #span two trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 2)) == 0) {
      #then there were no span size 2 trials, therefore
      span.2.corr <- NA
    } else {
      span.2.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 2])
    }
    #span three trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 3)) == 0) {
      #then there were no span size 3 trials, therefore
      span.3.corr <- NA
    } else {
      span.3.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 3])
    }	
    #span four trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 4)) == 0) {
      #then there were no span size 4 trials, therefore
      span.4.corr <- NA
    } else {
      span.4.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 4])
    }	
    #span five trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 5)) == 0) {
      #then there were no span size 5 trials, therefore
      span.5.corr <- NA
    } else {
      span.5.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 5])
    }
    #span six trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 6)) == 0) {
      #then there were no span size 6 trials, therefore
      span.6.corr <- NA
    } else {
      span.6.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 6])
    }	
    #span seven trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 7)) == 0) {
      #then there were no span size 7 trials, therefore
      span.7.corr <- NA
    } else {
      span.7.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 7])
    }	
    #span eight trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 8)) == 0) {
      #then there were no span size 8 trials, therefore
      span.8.corr <- NA
    } else {
      span.8.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 8])
    }
    #span nine trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 9)) == 0) {
      #then there were no span size 9 trials, therefore
      span.9.corr <- NA
    } else {
      span.9.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 9])
    }	
    
    fta.score <- sum(df.recall.wholeTrial$score)
    prop.score <- mean(df.recall.wholeTrial$prop.corr)
    number.successes <- sum(df.recall$trial.digit_span.result == "success")
    
    processing.accuracy <- sum(df.sentences$trial.operation_processing.result == "success") / nrow(df.sentences)
    processing.median.rt <- median(df.sentences$trial.operation_processing.durationTime)
    
    
    p.summary <- c(fta.score, prop.score, number.successes, processing.accuracy, processing.median.rt, 
                   max.span, span.2.corr, span.3.corr, span.4.corr, span.5.corr, span.6.corr, span.7.corr, span.8.corr, 
                   span.9.corr)
    
    return(p.summary)
  }
}

##now can use that function on all our data files for operation span
#put all operation span files into one folder and set that folder as working directory

compile_reading_span_data <- function(filestring) {
  
  raw.data <- read.csv(filestring)
  ps <- unique(raw.data$session.subject.subject.code)
  reading.span.data <- matrix(nrow=length(ps), ncol=14) 	
  
  
  for (i in 1:length(ps)) {
    
    #subset data frame with first unique subj code
    sub.raw.data <- subset(raw.data, session.subject.subject.code == ps[i])
    
    tmp.os.summary <- reading_span_processing(sub.raw.data,i)
    
    reading.span.data[i, ] <- tmp.os.summary
    
  }
  
  reading.span.data <- as.data.frame(reading.span.data)
  reading.span.data$user <- ps
  
  os.names <- c("fta.score", "prop.score", "number.successes", "processing.accuracy", "processing.median.rt", 
                "max.span", "span.2.corr", "span.3.corr", "span.4.corr", "span.5.corr", "span.6.corr", "span.7.corr", "span.8.corr", 
                "span.9.corr", "pcode")
  
  names(reading.span.data) <- os.names
  
  
  return(reading.span.data)	
  
}

reading.span.data <- compile_reading_span_data("C:\\Users\\user\\Downloads\\working memory raw data.csv")

write.csv(reading.span.data,"C:\\Users\\user\\Downloads\\reading.span.data.csv")
#rm(compile_reading_span_data, reading_span_processing)
```

READING MEMORY HIGHER ACCURACY
```{r}
library(dplyr)
library(readr)
library(readxl)
rawdata <- read.csv("C:\\Users\\user\\Downloads\\working memory raw data.csv")
data_nona <- subset(rawdata, !is.na(rawdata$trial.digit_span.load))
users <- unique(data_nona$session.subject.subject.code)

num_users <-length(users)
freq_users <- c(1:length(users))
user_num_of_trial <-c(1:length(users))

for (i in 1:length(users)) {freq_users[i] <- length(which(data_nona$session.subject.subject.code==users[i]))
user_num_of_trial_each_span <- data_nona %>% filter(session.subject.subject.code==users[i]& module.name=="Reading Span (digits)"& trial.digit_span.load %in% c(2,3,4,5))%>% arrange(trial.digit_span.load)%>%
  summarize(tri2=sum(trial.digit_span.load==2)/2,tri3=sum(trial.digit_span.load==3)/3,tri4=sum(trial.digit_span.load==4)/4,tri5=sum(trial.digit_span.load==5)/5)
user_num_of_trial[i] <-sum(user_num_of_trial_each_span)
}


#set the working directory to the folder that is holding the data files from the operation span task


###function that takes the output file for a participant and returns summary information for that person###

reading_span_processing_no6and7 <- function(data.frame,n) {
  
  #seperate processing trials from digit span trials
  
  df.recall <- subset(data.frame, !is.na(trial.digit_span.load))
  df.sentences  <- subset(data.frame, is.na(trial.digit_span.load))
  df.recall <- df.recall %>% filter(module.name== "Reading Span (digits)"& trial.digit_span.load %in% c(2,3,4,5))
  df.sentences  <- df.sentences  %>% filter(module.name=="Reading Span (digits)"& trial.digit_span.load2 %in% c(2,3,4,5))
  if (nrow(df.recall)==0) {print("No Reading span")
  } else {
    #standard output form digit span has each element of a trial as a row, 
    #also need to collate these into success/failure for whole trials.
    num.trials <- user_num_of_trial[n]
    #set these vectors up now, to be filled later.
    trial.load <- numeric(num.trials)
    num.corr <- numeric(num.trials)
    prop.corr <- numeric(num.trials)
    trial.success <- numeric(num.trials)
    
    j <- 1
    for (i in 1:num.trials) {
      if (df.recall$trial.digit_span.load[j]==2){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+1)]
        j <- j+2}
      else if(df.recall$trial.digit_span.load[j]==3){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+2)] 
        j <- j+3}
      else if(df.recall$trial.digit_span.load[j]==4){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+3)] 
        j <- j+4}
      else if(df.recall$trial.digit_span.load[j]==5){
        indi_trial <- df.recall$trial.digit_span.result[j:(j+4)] 
        j <- j+5}
      
      numDigits <- length(indi_trial)
      numCorr <- sum(indi_trial == "success")
      trial.success[i] <- numDigits == numCorr
      trial.load[i] <- numDigits
      num.corr[i] <- numCorr
      prop.corr[i] <- numCorr / numDigits
    }	
    
    df.recall.wholeTrial <- as.data.frame(cbind(trial.load, num.corr, prop.corr, trial.success))
    df.recall.wholeTrial <- transform(df.recall.wholeTrial, score = trial.load * trial.success)
    
    #highest load with a fully correct response
    if (length(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) == 0) {
      #no successes
      max.span <- NA
    } else {
      max.span <- max(df.recall.wholeTrial$trial.load[df.recall.wholeTrial$trial.success == 1]) 
    }
    
    #total number of words correctly recalled in correct serial position throughout
    number.successes <- sum(data.frame$trial.digit_span.result == "success")
    #span two trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 2)) == 0) {
      #then there were no span size 2 trials, therefore
      span.2.corr <- NA
    } else {
      span.2.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 2])
    }
    #span three trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 3)) == 0) {
      #then there were no span size 3 trials, therefore
      span.3.corr <- NA
    } else {
      span.3.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 3])
    }	
    #span four trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 4)) == 0) {
      #then there were no span size 4 trials, therefore
      span.4.corr <- NA
    } else {
      span.4.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 4])
    }	
    #span five trials correct
    if (nrow(subset(df.recall.wholeTrial, trial.load == 5)) == 0) {
      #then there were no span size 5 trials, therefore
      span.5.corr <- NA
    } else {
      span.5.corr <- sum(df.recall.wholeTrial$trial.success[df.recall.wholeTrial$trial.load == 5])
    }
    
    fta.score <- sum(df.recall.wholeTrial$score)
    prop.score <- mean(df.recall.wholeTrial$prop.corr)
    number.successes <- sum(df.recall$trial.digit_span.result == "success")
    
    processing.accuracy <- sum(df.sentences$trial.operation_processing.result == "success") / nrow(df.sentences)
    processing.median.rt <- median(df.sentences$trial.operation_processing.durationTime)
    
    
    p.summary <- c(fta.score, prop.score, number.successes, processing.accuracy, processing.median.rt, 
                   max.span, span.2.corr, span.3.corr, span.4.corr, span.5.corr)
    
    return(p.summary)
  }
}

##now can use that function on all our data files for operation span
#put all operation span files into one folder and set that folder as working directory

compile_reading_span_data_no6and7 <- function(filestring) {
  
  raw.data <- read.csv(filestring)
  ps <- unique(raw.data$session.subject.subject.code)
  reading.span.data <- matrix(nrow=length(ps), ncol=10) 	
  
  
  for (i in 1:length(ps)) {
    
    #subset data frame with first unique subj code
    sub.raw.data <- subset(raw.data, session.subject.subject.code == ps[i])
    
    tmp.os.summary <- reading_span_processing_no6and7(sub.raw.data,i)
    
    reading.span.data[i, ] <- tmp.os.summary
    
  }
  
  reading.span.data <- as.data.frame(reading.span.data)
  reading.span.data$user <- ps
  
  os.names <- c("fta.score", "prop.score", "number.successes", "processing.accuracy", "processing.median.rt", 
                "max.span", "span.2.corr", "span.3.corr", "span.4.corr", "span.5.corr", "pcode")
  
  names(reading.span.data) <- os.names
  
  
  return(reading.span.data)	
  
}

reading.span.data.No6and7 <- compile_reading_span_data_no6and7("C:\\Users\\user\\Downloads\\working memory raw data.csv")

write.csv(reading.span.data.No6and7,"C:\\Users\\user\\Downloads\\reading.span.data.No6and7.csv")
#rm(compile_reading_span_data, reading_span_processing)
```




