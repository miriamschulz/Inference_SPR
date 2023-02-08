### Custom functions to preprocess + filter SPR results from PCIbex
### Miriam Schulz
### 15 September 2022

### Functions are documented using roxygen2.
### Includes functions for:
### - General helper functions
### - Data preprocessing 
### - Data filtering:
###   - removing practice+filler trials
###   - removing non-critical regions
###   - removing false plausibility rating trials
### - Outlier removal based on:
###   - SPR reading time (threshold values or sd)
###   - context sentence reading time



########################
### Helper functions ###
########################


#' Calculate mean and round to two digits
#'
#' @param x A numeric vector
#' @param d An integer: number of decimal places (default = 2)
#'
#' @return A number rounded to two decimals
#' @export
#'
#' @examples
#' meanRound(c(2.222, 9.999))
meanRound <- function(x, d=2) {round(mean(x), digits = d)}


#' Standard error
#'
#' @param x A numeric vector
#' @param n An integer: sample size (default = 40)
#'
#' @return
#' @export
#'
#' @examples
se <- function(x, n=40) {sd(x) / sqrt(n)}


#' Aggregate data using the mean, SD and SE
#'
#' @param df A data frame
#' @param agg.formula A formula as input for the aggregate function
#' @param n 
#' @param round.data Boolean: if TRUE, rounds numbers to 2 decimal places
#'
#' @return
#' @export
#'
#' @examples
aggregMeans <- function(df,
                        agg.formula, 
                        n=40,
                        round.data = FALSE) {
  agg.data <- aggregate(agg.formula,
                        data = df,
                        #na.rm=F,
                        FUN = function(x) {c(Mean = mean(x),
                                             SD = sd(x),
                                             SE = sd(x) / sqrt(n))})
  if (round.data == TRUE) {
    agg.data <- agg.data %>% 
      mutate_if(is.numeric, round, 2)
  }
  # Transform to data frame
  agg.data <- do.call(data.frame, agg.data)
  # Rename last columns
  newnames <- colnames(agg.data)[!colnames(agg.data) %in% 
                                   tail(colnames(agg.data), 3)]
  names(agg.data) <- c(newnames, "Mean", "SD", "SE")
  agg.data
}


#' Aggregate data using the mean and standard error (OLD FUNCTION)
#'
#' @description
#' Data are aggregated using the mean and se function.
#' Variables are passed as a character vector, beginning with the dependent
#' variable and continuing with one to three independent variables.
#'
#' @param df A data frame 
#' @param var.list A character vector of the variables (DV first, then 1-3 IVs)
#' @param use.sd Boolean: if true, uses the SD function instead of SE
#' @param n An integer: sample size to calculate the standard error
#'
#' @return A data frame of the aggregated means and the SE
#' @export
#'
#' @examples
#' aggMeansSE(df, c("RT", "Cond", "Region"))
aggMeansSE <- function(df, var.list, use.sd=FALSE, n=40) {
  
  if (use.sd==TRUE) {
    var.function = sd
    var.name = "SD"
  } else {
    var.function = se
    var.name = "SE"
  }
  if (length(var.list) == 2) {
    
    y <- parse(text = var.list[1])
    var1 <- parse(text = var.list[2])
    means.cond <- aggregate(eval(y) ~ eval(var1),
                            data = df,
                            FUN = mean)
    means.condVAR <- aggregate(eval(y) ~ eval(var1),
                               data = df,
                               FUN = var.function)
    var.list <- var.list[-1]  # remove y
    colnames(means.cond) <- c(var.list, paste0("Mean", y))
    colnames(means.condVAR) <- c(var.list, var.name)
    means.cond <- merge(means.cond, means.condVAR, by=var.list)
    
  } else if (length(var.list) == 3) {
    
    y <- parse(text = var.list[1])
    var1 <- parse(text = var.list[2])
    var2 <- parse(text = var.list[3])
    means.cond <- aggregate(eval(y) ~ eval(var1) + eval(var2),
                            data = df,
                            FUN = mean)
    means.condVAR <- aggregate(eval(y) ~ eval(var1) + eval(var2),
                               data = df,
                               FUN = var.function)
    var.list <- var.list[-1]  # remove y
    colnames(means.cond) <- c(var.list, paste0("Mean", y))
    colnames(means.condVAR) <- c(var.list, var.name)
    means.cond <- merge(means.cond, means.condVAR, by=var.list)
    
  } else if (length(var.list) == 4) {
    
    y <- parse(text = var.list[1])
    var1 <- parse(text = var.list[2])
    var2 <- parse(text = var.list[3])
    var3 <- parse(text = var.list[4])
    means.cond <- aggregate(eval(y) ~ eval(var1) + eval(var2) + eval(var3),
                            data = df,
                            FUN = mean)
    means.condVAR <- aggregate(eval(y) ~ eval(var1) + eval(var2) + eval(var3),
                               data = df,
                               FUN = var.function)
    var.list <- var.list[-1]  # remove y
    colnames(means.cond) <- c(var.list, paste0("Mean", y))
    colnames(means.condVAR) <- c(var.list, var.name)
    means.cond <- merge(means.cond, means.condVAR, by=var.list)
    
  } else {
    stop("This function takes as input a dataframe and a list of 2-4 variables, starting with Y, then X.")
  }
  means.cond
}


#' Scale several predictor variables at once
#'
#' @param df A data frame
#' @param predictor.list A list of predictors to be scaled
#'
#' @return A data frame with scaled versions of the selected predictors
#' @export
#'
#' @examples scalePredictors(df, c("Inference", "Coherence", "Association"))
scalePredictors <- function(df, predictor.list) {
  df[, predictor.list] <- lapply(df[, predictor.list], scale)
  df
}


#######################
### Data extraction ###
#######################


#' Read in the raw PCIbex results.csv file
#'
#' @description
#' Reads in raw PCIbex results.csv file and adds column names using a 
#' manually specified vector.
#' Note: It is important to specify the maximum number of columns from the 
#' PCIbex results file (html trials will have a different number of columns 
#' than SPR trials, etc).
#' Check the results file to find what the maximum number of columns is.
#'
#' @param filename A filename of a PCIbex results csv file
#'
#' @return A data frame with the correct column names
#' @export
#'
#' @examples
#' readPCibex("results.csv")
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


#' Extract survey and demographics data
#'
#' @param df A data frame containing the output of readPCIbex()
#'
#' @return A data frame containing the survey and demographics data
#' @export
#'
#' @examples
#' getSurvey(df)
getSurvey <- function(df) {
  df <- df[df$Item == "ExperimentalSurvey", ]
  df <- df[df$PennElementName %in% c("demographics", "postexp_survey",
                                     "postexp_survey2"), c(2,1, 11:12)]
  colnames(df) <- c("Subject", "IPhash", "Question", "Answer")
  df$Answer <- gsub('%2C', ',', df$Answer)   # commas instead of %2C
  df
}


#' Extract reading + reaction time data
#' 
#' @description
#' Annotates the reading time + reaction time data frame with additional info:
#' sentence position relative to target,
#' rating accuracy, list order, list+trial number.
#' 
#' @param df A data frame containing the output of readPCIbex()
#' 
#' @return A data frame
#' @export
#'
#' @examples
#' getReads(df)
getReads <- function(df) {
  
  # Step 0: list order annotation (1 = original, 2 = reversed):
  df.order.table <- df[df$Item == "2", c("Subject", "Block")]
  df.order.table$Order <- ifelse(df.order.table$Block == "Block3", 1, 2)
  df.order.table$Block <- NULL
  df.order.table <- unique(df.order.table)
  df <- suppressMessages(plyr::join(df, df.order.table))
  
  # Step 1: get ratings df
  ratings <- df[df$Label != "instructions", ]
  ratings <- ratings[ratings$PennElementName == "Decide", 
                     c("Subject", "IPhash", "Item", "Cond", "Word")]
  names(ratings)[names(ratings) == 'Word'] <- "PlausibilityRating"
  ratings$Item <- as.numeric(ratings$Item)
  ratings[ ,c("Subject", "IPhash", "Cond")] <-
    lapply(ratings[ ,c("Subject", "IPhash", "Cond")], as.factor)
  
  # Step 2: SPR Reading Times
  df <- df[df$PennElementName == "DashedSentence", ]
  df$TrialNum <- df$IbexItem-11  # annotate trial position
  df <- df[, c(2, 1, 11:24, 26:27)]
  df[ , c("Critical", "WordNum")] <- lapply(df[ ,c("Critical", "WordNum")], 
                                            as.numeric)
  
  # Annotate critical regions: encode word position wrt. target word position
  df$Region <- df$WordNum - df$Critical
  
  # Transform variables to numeric / factor
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
  
  # Step 5: Annotate list number  
  df <- arrange(df, Item, Cond)
  # Use item 3, since it has a different position for each list:
  df.list.table <- unique(df[df$Item == "3", c("Subject", "TrialNum")])
  df.lists <- data.frame(TrialNum = c(53, 51, 22, 89,  # Order 1 versions
                                      68, 70, 99, 32), # Order 2 versions
                         List = 1:8)
  df.list.table <- merge(df.list.table, df.lists, by="TrialNum")
  df.list.table$TrialNum <- NULL  # not needed 
  df <- suppressMessages(plyr::join(df, df.list.table))
  
  df
}


#' Print by-subject reading time for the html files
#'
#' @description
#' Prints the time each subject spend reading the html files.
#' This allows to check how much time was spent reading the instructions. 
#' Low instruction reading times suggest that the instructions were skipped.
#'
#' @param df A dataframe containing the output of readPCIbex()
#'
#' @return A dataframe containing the reading time for each html element
#' in minutes and seconds
#' @export
#'
#' @examples
#' getHtmlTime(df)
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



######################
### Data filtering ###
######################


#' Data filtering: remove fillers and/or practice trials
#'
#' @param df A data frame with the reading time data
#' @param fillers Boolean: if TRUE, remove filler trials
#' @param practice Boolean: if TRUE, remove practice trials
#'
#' @return A data frame with trials removed as specified
#' @export
#'
#' @examples
#' removeFillersPractice(df, fillers=FALSE, practice=TRUE)
removeFillersPractice <- function(df,
                                  fillers=TRUE,
                                  practice=TRUE) {
  if (practice==TRUE) {
    # Practice trials have item numbers 301-308
    df <- filter(df, Item < 300)
  }
  if (fillers==TRUE) {
    # Filler trials have item numbers 101-160
    df <- filter(df, Item > 300 | Item < 100)
  }
  df <- droplevels(df)
  df
}


#' Data filtering: remove non-critical regions
#'
#' @param df A data frame with reading times
#' @param keep.regions A numeric vector specifying the regions to keep, e.g.-1:2
#'
#' @return A data frame containing only the chosen regions
#' @export
#'
#' @examples
#' removeRegions(df, keep.regions=-1:2)
#' removeRegions(df, keep.regions=c(-2:3, 99))
removeRegions <- function(df,
                          keep.regions) {
  df <- droplevels(filter(df, Region %in% keep.regions))
  df
}


#' Data filtering: remove trials with wrong plausibility rating
#'
#' @param df A data frame
#'
#' @return A data frame without falsely answered trials
#' @export
#'
#' @examples
removeIncorrect <- function(df) {
  nrow.start <- nrow(df)  # store original size of df
  df <- filter(df, RatingCorrect == 1)
  nrow.end <- nrow(df)  # store new size of df
  printDataLoss(nrow.start, nrow.end)
  df
}


#######################
### Outlier removal ###
#######################


#' Calculate how much % of data is removed by an outlier removal method
#'
#' @param nrow.start An integer: nrow of original data frame
#' @param nrow.end An integer: nrow of data frame after outlier removal 
#'
#' @return
printDataLoss <- function(nrow.start, nrow.end) {
  cat(sprintf("Removing outliers values affected %s%% of the data.\n",
          (round(1-(nrow.end/nrow.start), 4))*100))
  cat(sprintf("Data points lost: %s\n", nrow.start-nrow.end))
}


#' Outlier removal: SPR word reading times
#'
#' @description
#' Can remove outlier reading time trials using two methods: 
#' either by specifying hard thresholds for cutoff, e.g. > 50ms and < 2500ms,
#' or by using a multiple of the standard deviation for each participant.
#' 
#' When entire.trial = TRUE, removes all data points of an affected trial,
#' so the RT of each word in that sentence for that Item-Condition-Subject.
#' IMPORTANT: entire.trial = TRUE will by default only remove trials for which 
#' one of the critical region RTs is an outlier (regions -1 to 2: 
#' precrit to spillover2)!
#' This is to prevent massive data loss when including all regions.
#' The regions parameter can be modified to allow entire.trial exclusion 
#' based on values in other regions as well (include all: regions = -100:100).
#'
#' @param df A dataframe containing the reading time data
#' @param method A string: accepts either "sd" or "threshold" 
#' @param min.rt An integer: lower bound for RT exclusion 
#' @param max.rt An integer: upper bound for RT exclusion 
#' @param sd.value An integer or float: sd value for sd-based exclusion
#' @param entire.trial Boolean
#' @param regions A numeric vector: regions that serve as basis for entire.trial
#'
#' @return A data frame without the outlier trials
#' @export
#'
#' @examples
#' removeOutliersSPR(df, "threshold", min.rt=100, max.rt=3000)
#' removeOutliersSPR(df, "sd", 2, entire.trial=FALSE)
removeOutliersSPR <- function(df,
                              method,
                              min.rt=50,
                              max.rt=2500,
                              sd.value=2.5,
                              entire.trial = TRUE,
                              regions = -1:2) {
  
  nrow.start <- nrow(df)  # store original size of df
  
  if (method == "threshold") {
    
    cat(sprintf("Filtering RTs using method: hard thresholds (%s ms < RT < %s ms)\n",
            min.rt, max.rt))

    # Mark trials to exclude
    df$ExcludeTrial <- ifelse((df$RT < min.rt | df$RT > max.rt), "yes", "no")
    
  } else if (method == "sd") {
    
    cat(sprintf("Filtering RTs using method: SD by participant (cutoff value: +- %s SD(s))\n",
            sd.value))
    
    participant.SDs <- df %>% 
      group_by(Subject) %>% 
      summarize(SubjectMean = mean(RT),
                SubjectSD = sd(RT))
      df <- merge(df, participant.SDs, by="Subject", all=TRUE)
      #df$ExcludeTrial <- ifelse((df$RT > df$SubjectMean + sd.value*df$SubjectSD | 
      #                             df$RT < df$SubjectMean - sd.value*df$SubjectSD),
      #                          "yes",
      #                          "no")
      
      df$UpperBound <- df$SubjectMean + sd.value*df$SubjectSD
      df$LowerBound <- df$SubjectMean - sd.value*df$SubjectSD
      df$ExcludeTrial <- ifelse((df$RT > df$UpperBound | df$RT < df$LowerBound),
                                "yes", "no")
      df$UpperBound <- NULL
      df$LowerBound <- NULL

  } else {
    stop("Please select a valid method (threshold or sd).")
  }
  
  # Optional: if a remove-marked data point is in a target region,
  # mark the entire trial for exclusion
  if (entire.trial==TRUE) {
    # Create df for target regions: trials which will have to be excluded
    df.exclude <- filter(df,
                         ExcludeTrial == "yes",
                         Region %in% regions)
    #print(nrow(df.exclude))

    # Update the exclusion criteria for all data points: 
    # Does the trial have a remove-marked data point in the critical regions?
    df.exclude.mini <- dplyr::select(df.exclude, Item, Cond, Subject)
    df.mini <- dplyr::select(df, Item, Cond, Subject)
    for (r in 1:nrow(df.mini)) {
      row <- df.mini[r, ]
      match.found <- nrow(suppressMessages(plyr::match_df(df.exclude.mini, row)))
      # Update the trial exclusion marking
      if (match.found > 0) {
        df[r, "ExcludeTrial"] <- "yes"
      }
    }
  }

  #print(summary(as.factor(df$ExcludeTrial)))

  # Remove marked trials
  df <- filter(df, ExcludeTrial=="no")
  df$ExcludeTrial <- NULL
  
  nrow.end <- nrow(df)  # store new size of df
  
  printDataLoss(nrow.start, nrow.end)

  df
}


#' Outlier removal: context sentence reading times
#'
#' @param df A data frame
#' @param min.rt An integer: minimum allowed context reading time
#' @param max.rt An integer: maximum allowed context reading time
#'
#' @return A data frame without low/high context reading time trials
#' @export
#'
#' @examples
#' removeOutliersContext(df, min.rt=500, max.rt=30000)
removeOutliersContext <- function(df, min.rt, max.rt) {
  nrow.start <- nrow(df)  # store original size of df
  df <- filter(df,
               ContextRT <= max.rt,
               ContextRT >= min.rt)
  nrow.end <- nrow(df)  # store new size of df
  printDataLoss(nrow.start, nrow.end)
  df
}


#' Outlier removal: plausibility rating reaction times
#'
#' @param df A data frame
#' @param method A string: "sd" or "threshold"
#' @param min.rt An integer: minimum allowed reaction time
#' @param max.rt An integer: maximum allowed reaction time
#' @param sd An integer or float: sd value for sd-based exclusion
#'
#' @return A data frame without low/high rating reaction time trials
#' @export
#'
#' @examples
# 'removeOutliersRatings(ratings, method="sd", sd=2.5)
# 'removeOutliersRatings(ratings, method="threshold", min.rt=300, max.rt=20000)
removeOutliersRatings <- function(df,
                                  method,
                                  min.rt=100, max.rt=30000,
                                  sd=2.5) {
  nrow.start <- nrow(df)  # store original size of df
  if (method == "threshold") {
    df <- filter(df,
                 DecisionRT <= max.rt,
                 DecisionRT >= min.rt)
  } else if (method == "sd") {
    participant.SDs <- df %>% 
      group_by(Subject) %>% 
      summarize(SubjectSD = sd(DecisionRT),
                SubjectMean = mean(DecisionRT))
    df <- merge(df, participant.SDs, by="Subject", all=TRUE)
    df <- filter(df,
                 DecisionRT <= SubjectMean + sd*SubjectSD,
                 DecisionRT >= SubjectMean - sd*SubjectSD)
  } else {
    stop("Please select a valid method (threshold or sd).")
  }
  nrow.end <- nrow(df)  # store new size of df
  printDataLoss(nrow.start, nrow.end)
  df
}
