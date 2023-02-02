### Functions to read in + analyze raw SPR results from PCIbex
### Miriam Schulz
### 15 September 2022


# Define function to round the aggregate mean results to two decimal points
meanRound <- function(x) {round(mean(x), digits = 2)}


# Define a function to read in the raw PCIbex results.csv file
readPCibex <- function(filename) {
  df <- read.csv(filename, header=F, comment.char="#", encoding="UTF-8",
                 col.names=c("Time", "IPhash", "IbexController", "IbexItem",
                             "IbexElement", "Label", "Group",
                             "PennElementType", "PennElementName",
                             "WordNum", "Word", "EventTime",
                             "Item", "Cond", "Block",
                             "Critical", "Target", "Plausible", "ContextRT",
                             "DecisionRT", "RT",
                             "Newline", "Sentence", "Comments"),
                 fill=TRUE,
                 stringsAsFactors=FALSE)
  df
}


# Define a function to extract the survey and demographics data
getSurvey <- function(df) {
  df <- df[df$Item == "ExperimentalSurvey", ]
  df <- df[df$PennElementName %in% c("demographics", "postexp_survey",
                                     "postexp_survey2"), c(2,1, 11:12)]
  colnames(df) <- c("Subject", "IPhash", "Question", "Answer")
  df$Answer <- gsub('%2C', ',', df$Answer)   # commas instead of %2C
  df
}


# Define a function to extract the reading time data of the target sentence 
# (NOW ALSO ANNOTATES RATING ACCURACY)
getReads <- function(df) {
  
  # Step 1: get ratings df
  ratings <- df[df$Label != "instructions", ]
  ratings <- ratings[ratings$PennElementName == "Decide", 
                     c("Subject", "IPhash", "Item", "Cond", "Word")]
  names(ratings)[names(ratings) == 'Word'] <- "PlausibilityRating"
  ratings$Item <- as.numeric(ratings$Item)
  ratings[ ,c("Subject", "IPhash", "Cond")] <-
    lapply(ratings[ ,c("Subject", "IPhash", "Cond")], as.factor)
  
  # Step 2: SPR Reading times
  df <- df[df$PennElementName == "DashedSentence", ]
  df$TrialNum <- df$IbexItem-11  # annotate trial position
  df <- df[, c(2,1, 11:24, 26:27)]

  df[ , c("Critical", "WordNum")] <- lapply(df[ ,c("Critical", "WordNum")], 
                                            as.numeric)
  
  # Annotate critical regions 
  # encode word position wrt. target word position:
  df$Region <- df$WordNum - df$Critical
  
  df[ ,c("Item", "RT")] <- lapply(df[ ,c("Item", "RT")], as.numeric)
  df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")] <-
    lapply(df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")], 
           as.factor)

  df$Word <- gsub('%2C', ',', df$Word)           # commas instead of %2C
  df$Sentence <- gsub('%2C', ',', df$Sentence)   # commas instead of %2C
  
  # Step 3: Merge ratings + reading times 
  df <- merge(df, ratings, by=c("Subject", "IPhash", "Item", "Cond"))
  
  # Step 4: Annotate rating accuracy
  df$PlausibleKey <- ifelse(df$Order == 1, 
                            ifelse(df$Plausible == "Yes", "K", "D"),
                            ifelse(df$Plausible == "Yes", "D", "K"))
  df$RatingCorrect <- ifelse(df$PlausibleKey == df$PlausibilityRating, 1, 0)
  df$PlausibleKey <- NULL
  
  df <- arrange(df, Item, Cond)
  
  df
}


# Define a function to extract the plausibility ratings + reaction times
getRatings <- function(df) {
  
  # 1. Rating:
  #df <- df[!(df$Label %in% c("practice", "instructions")), ]
  df <- df[df$Label != "instructions", ]
  
  ratings <- df[df$PennElementName == "Decide",
                c("Subject", "IPhash", "Item", "Cond", "Word")]
  names(ratings)[names(ratings) == 'Word'] <- 'PlausibilityRating'

  # 2. Reaction Time 
  # Calculate reaction times by subtraction: EndDisplayTime - StartDisplayTime
  df <- df[df$PennElementName %in% c("Respond", "Decide"), ]
  # To fix error in Acceptability ratings reaction times: 
  # make sure the data frame is ordered correctly 
  df$EventTime <- as.numeric(df$EventTime)
  df <- df[ with(df, order(EventTime)), ]
  start.time <- df[df$PennElementName == "Respond", ]$EventTime
  end.time <- df[df$PennElementName == "Decide", ]$EventTime

  DecisionRTs <- end.time - start.time
  df$DecisionRTManual <- rep(DecisionRTs, each = 2)
  df <- unique(df[df$PennElementName == "Decide", ])
  
  # Merge ratings with reaction times 
  df <- merge(df, ratings, by=c("Subject", "IPhash", "Item", "Cond"))

  df$Item <- as.numeric(df$Item)
  df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")] <-
    lapply(df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")], as.factor)

  # Annotate plausibility correct 
  #df$PlausibleKey <- ifelse(df$Plausible == "Yes", "K", "D")  # reverse for the other lists!
  df$PlausibleKey <- ifelse(df$Order == 1,
                            ifelse(df$Plausible == "Yes", "K", "D"),
                            ifelse(df$Plausible == "Yes", "D", "K"))
  df$RatingCorrect <- ifelse(df$PlausibleKey == df$PlausibilityRating, 1, 0)
  df$PlausibleKey <- NULL
  
  df <- df[, c(1:4, 16:21, 26:29)]
  df <- arrange(df, Item, Cond)
  # Remove filler and practice trials if necessary
  #df <- droplevels(df[df$Item < 100, ])
  
  df
}


# Aggregate by means and condition including SE 

aggMeansSE <- function(df, var.list) {
  
  if (length(var.list) == 2) {
    
    y <- parse(text = var.list[1])
    var1 <- parse(text = var.list[2])
    means.cond <- aggregate(eval(y) ~ eval(var1),
                            data = df,
                            FUN = mean,   # function to apply
                            na.rm = T)    # ignore NAs
    means.condSE <- aggregate(eval(y) ~ eval(var1),
                              data = df,
                              FUN = se,    # function to apply
                              na.rm = T)   # ignore NAs
    var.list <- var.list[-1]  # remove y
    colnames(means.cond) <- c(var.list, paste0("Mean", y))
    colnames(means.condSE) <- c(var.list, "SE")
    means.cond <- merge(means.cond, means.condSE, by=var.list)
    
   } else if (length(var.list) == 3) {
 
     y <- parse(text = var.list[1])
     var1 <- parse(text = var.list[2])
     var2 <- parse(text = var.list[3])
     means.cond <- aggregate(eval(y) ~ eval(var1) + eval(var2),
                             data = df,
                             FUN = mean,   # function to apply
                             na.rm = T)    # ignore NAs
     means.condSE <- aggregate(eval(y) ~ eval(var1) + eval(var2),
                               data = df,
                               FUN = se,    # function to apply
                               na.rm = T)   # ignore NAs
     var.list <- var.list[-1]  # remove y
     colnames(means.cond) <- c(var.list, paste0("Mean", y))
     colnames(means.condSE) <- c(var.list, "SE")
     means.cond <- merge(means.cond, means.condSE, by=var.list)

     
   } else if (length(var.list) == 4) {
     
     y <- parse(text = var.list[1])
     var1 <- parse(text = var.list[2])
     var2 <- parse(text = var.list[3])
     var3 <- parse(text = var.list[4])
     means.cond <- aggregate(eval(y) ~ eval(var1) + eval(var2) + eval(var3),
                             data = df,
                             FUN = mean,   # function to apply
                             na.rm = T)    # ignore NAs
     means.condSE <- aggregate(eval(y) ~ eval(var1) + eval(var2) + eval(var3),
                               data = df,
                               FUN = se,    # function to apply
                               na.rm = T)   # ignore NAs
     var.list <- var.list[-1]  # remove y
     colnames(means.cond) <- c(var.list, paste0("Mean", y))
     colnames(means.condSE) <- c(var.list, "SE")
     means.cond <- merge(means.cond, means.condSE, by=var.list)
     
     } else {
    stop("This function takes as input a dataframe and a list of 2-4 variables, starting with Y, then X.")
  }
  means.cond
}



# Function to remove outliers from RTs based on threshold values or SDs:
filterRTs <- function(df, criteria, method = "thresholds", 
                      remove.incorrect = FALSE, remove.contexts = 0,
                      regions = -1:2) {
  
  df <- filter(df, Region %in% regions)

  Nrows.original <- nrow(df)  # store how large the df was originally 
  
  if (method == "thresholds") {
    min.threshold <- criteria[1]
    max.threshold <- criteria[2]
    # Mark trials to exclude
    df$ExcludeTrial <- ifelse((df$RT < min.threshold |  df$RT > max.threshold),
                              "yes", "no")
    df <- filter(df, ExcludeTrial == "no")
    df$ExcludeTrial <- NULL
    
    #df <- filter(df,
    #             #Region == eval(parse(text = region)), 
    #             #Region == region, 
    #             RT > min.threshold,
    #             RT < max.threshold)
    
  } else if (method == "sd") {
    #TODO: do the following BY SUBJECT, not by grand mean of RTs
    sd.multiplicator <- criteria
    sd.interval <- sd(df$RT) * sd.multiplicator
    dfExcludeTrial <- ifelse((RT < mean(RT) - sd.interval) | 
                               (RT > mean(RT) + sd.interval), 
                             "yes", "no")
    df <- filter(df, ExcludeTrial == "no")
    df$ExcludeTrial <- NULL
    
    #df <- filter(df,
    #             RT >= mean(RT) - sd.interval & RT <= mean(RT) + sd.interval)
  } else {
    print("Please choose a valid method for filtering: 'threshold', '2sd' or '3sd'.")
  }
  
  # Remove trials with a too short context sentence reading time:
  if (remove.contexts > 0) {
    df <- filter(df, ContextRT >= remove.contexts)
  }
  
  # Remove incorrect trials 
  if (remove.incorrect == TRUE) {
    df <- filter(df, RatingCorrect == 1)
  }
  
  Nrows.new <- nrow(df)  # store size of reduced df for comparison
  
  # Print how much data was removed 
  print(paste0("Removing extreme values and incorrectly answered trials affected ",
               (round(1-(Nrows.new/Nrows.original), 4))*100,
               "% of the data."))
  print(paste0("(Original rows in df: ", Nrows.original,
               ". Remaining rows in df: ", Nrows.new, ".)"))
  
  df
}


# Function to get reading time for the html files (in seconds + in minutes)
getHtmlTime <- function(df) {
  
  df <- df[df$Item == 'ExperimentalSurvey', ]
  df <- filter(df, Word %in% c('Start', 'End'))
  
  df$EventTime <- as.numeric(df$EventTime)
  df <- df[ with(df, order(Time, EventTime)), ]

  start.time <- df[df$Word == "Start", ]$EventTime
  end.time <- df[df$Word == "End", ]$EventTime
  
  HtmlRTs <- end.time - start.time
  df$HtmlRTSeconds <- rep(HtmlRTs, each = 2)
  df$HtmlRTSeconds <- df$HtmlRTSeconds / 1000  # transform from ms to s
  df$HtmlRTMinutes <- round((df$HtmlRTSeconds / 60), 2)
  df$HtmlRTSeconds <- round(df$HtmlRTSeconds, 1)
  
  df <- unique(df[df$Word == "End", c(2, 1, 7, 27:28)])
  df
}
