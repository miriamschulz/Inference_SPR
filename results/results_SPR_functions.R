### Functions for the R script to read in + analyze raw SPR results from PCIbex ###
### 15 September 2022


### Functions


# Define function to round the aggregate mean results to two decimal points: 
meanRound <- function(x) {round(mean(x), digits = 2)}


# Define a function to read in the raw PCIbex results.csv file
readPCibex <- function(filename) {
  df <- read.csv(filename, header=F, comment.char="#", encoding="UTF-8",
                 col.names=c("Time", "IPhash", "IbexController", "IbexItem", "IbexElement",
                             "Label", "Group", "PennElementType", "PennElementName", "WordNum",
                             "Word", "EventTime", "Item", "Cond", "Block",
                             "Critical", "Target", "Plausible", "ContextRT",  "DecisionRT",
                             "RT", "Newline", "Sentence", "Comments"),
                 fill=TRUE,
                 stringsAsFactors=FALSE)
  df
}


# Define a function to extract the survey and demographics data
getSurvey <- function(df) {
  df <- df[df$Item == "ExperimentalSurvey", ]
  df <- df[df$PennElementName %in% c("demographics", "postexp_survey", "postexp_survey2"), c(2,1, 11:12)]
  colnames(df) <- c("Subject", "IPhash", "Question", "Answer")
  df$Answer <- gsub('%2C', ',', df$Answer)   # commas instead of %2C
  df
}


# Define a function to extract the reading time data of the target sentence 
# (NOW ALSO ANNOTATES RATING ACCURACY)
getReads <- function(df) {
  
  # Step 1: get ratings df
  ratings <- df[df$Label != "instructions", ]
  ratings <- ratings[ratings$PennElementName == "Decide", c("Subject", "IPhash", "Item", "Cond", "Word")]
  names(ratings)[names(ratings) == 'Word'] <- "PlausibilityRating"
  ratings$Item <- as.numeric(ratings$Item)
  ratings[ ,c("Subject", "IPhash", "Cond")] <-
    lapply(ratings[ ,c("Subject", "IPhash", "Cond")], as.factor)
  
  # Step 2: SPR Reading times
  df <- df[df$PennElementName == "DashedSentence", ]
  df$TrialNum <- df$IbexItem-11  # annotate trial position
  df <- df[, c(2,1, 11:24, 26:27)]

  df[ , c("Critical", "WordNum")] <- lapply(df[ ,c("Critical", "WordNum")], as.numeric)
  
  # Annotate critical regions 
  # encode word position wrt. target word position:
  df$Region <- df$WordNum - df$Critical
  
  df[ ,c("Item", "RT")] <- lapply(df[ ,c("Item", "RT")], as.numeric)
  df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")] <-
    lapply(df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")], as.factor)

  df$Word <- gsub('%2C', ',', df$Word)           # commas instead of %2C
  df$Sentence <- gsub('%2C', ',', df$Sentence)   # commas instead of %2C
  
  # Step 3: Merge ratings + reading times 
  df <- merge(df, ratings, by=c("Subject", "IPhash", "Item", "Cond"))
  
  # Step 4: Annotate rating accuracy
  df$PlausibleKey <- ifelse(df$Order == 1, ifelse(df$Plausible == "Yes", "K", "D"),
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
  
  ratings <- df[df$PennElementName == "Decide", c("Subject", "IPhash", "Item", "Cond", "Word")]
  names(ratings)[names(ratings) == 'Word'] <- 'PlausibilityRating'

  # 2. Reaction Time 
  # Calculate the reaction times by subtraction: EndDisplayTime - StartDisplayTime
  df <- df[df$PennElementName %in% c("Respond", "Decide"), ]
  # To fix error in Acceptability ratings reaction times: make sure the data frame is ordered correctly 
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
  df$PlausibleKey <- ifelse(df$Order == 1, ifelse(df$Plausible == "Yes", "K", "D"),
                            ifelse(df$Plausible == "Yes", "D", "K"))
  df$RatingCorrect <- ifelse(df$PlausibleKey == df$PlausibilityRating, 1, 0)
  df$PlausibleKey <- NULL
  
  df <- df[, c(1:4, 16:21, 26:29)]
  df <- arrange(df, Item, Cond)
  # Remove filler and practice trials if necessary
  #df <- droplevels(df[df$Item < 100, ])
  
  df
}


# Define a function to extract the reading time data of the context sentences
getContextReads <- function(df) {
  #df <- df[df$PennElementName %in% c("showContext", "removeContext"), c(1:2, 6, 9, 12, 14:19)]
  df <- df[df$PennElementName %in% c("showContext", "removeContext"), ]
  
  # Calculate the context reading times by subtraction: EndDisplayTime - StartDisplayTime
  df$EventTime <- as.numeric(df$EventTime)
  df <- df[ with(df, order(EventTime)), ]  # to prevent errors 
  start.time <- df[df$PennElementName == "showContext", ]$EventTime
  end.time <- df[df$PennElementName == "removeContext", ]$EventTime
  ContextRTs <- end.time - start.time
  df$ContextRTManual <- rep(ContextRTs, each = 2)
  
  df$Item <- as.numeric(df$Item)
  df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")] <-
    lapply(df[ ,c("Subject", "IPhash", "Cond", "Plausible", "Block")], as.factor)

  df <- df[df$PennElementName == "removeContext", c(2,1, 14:20, 26, 21, 27)]  # remove duplicate rows
  
  #df <- droplevels(df[df$Item < 100, ])
  df <- arrange(df, Item, Cond)
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
                      remove.incorrect = FALSE, remove.contexts = 0) {

  Nrows.original <- nrow(df)  # store how large the df was originally 
  
  if (method == "thresholds") {
    min.threshold <- criteria[1]
    max.threshold <- criteria[2]
    df <- filter(df,
                 #Region == eval(parse(text = region)), 
                 #Region == region, 
                 RT > min.threshold,
                 RT < max.threshold)
  } else if (method == "sd") {
    #TODO: do the following BY SUBJECT, not by grand mean of RTs
    sd.multiplicator <- criteria
    sd.interval <- sd(df$RT) * sd.multiplicator
    df <- filter(df,
                 RT >= mean(RT) - sd.interval & RT <= mean(RT) + sd.interval)
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



#####################
### rRT functions ###
#####################


# Function for rRT models
runModels <- function(df, model.type, m.formula,
                      regions = -1:2) {
  
  # Print chosen model type 
  model.types <- c("lm" = "simple LM",
                   "lmer" = "LMER",
                   "lm.by.subj" = "by-subject LMs")
  if (!model.type %in% names(model.types)) {
    stop("Please specify a model type from the following options: lm, lm.by.subj, lmer")
  }
  print(paste0("Chosen model: ", model.types[model.type]))
  
  # Order df by Region, then Subject (important for the final merging step)
  df <- filter(df, Region %in% regions) %>% arrange(Region, Subject)
  
  # Extract dependent var
  DV <- m.formula[[2]]
  
  # Extract subjects 
  subjects <- unique(df$Subject)
  
  # Reset the results data frame
  if (exists("output")) {rm("output")}
  
  # Loop over the regions and run one model per region 
  for (region in regions) {
    
    print(paste0("Processing region: ", region))
    df.region <- filter(df, Region == region)

    # Run model and extract residuals, fitted values, and model coefficients
    if (model.type == "lm") {
      
      ### Simple LM  
      m.region <- lm(m.formula, data = df.region)
      
      # 1.1 Residuals + fitted values
      output.region <- cbind(data.frame(Residuals = residuals(m.region),
                                        FittedValues = fitted.values(m.region)))
      # 1.2 Recover original values (only to check if the output df is correct)
      original.region <- m.region$model
      colnames(original.region) <- paste0("Model_", colnames(original.region))
      output.region <- cbind(output.region, original.region)
      
      # Step 2: Coefficients 
      m.coefs <- tidy(m.region)      # extract coefficients 
      for (r in 1:nrow(m.coefs)) {
        term <- m.coefs[r, "term"][[1]]
        term <- paste("Coef", term, sep = "_")
        estimate <- m.coefs[r, "estimate"][[1]]
        output.region[, term] <- estimate
      }
      
      # Bind to original df (or create)e
      if (exists("output") == FALSE) {
        output <- output.region
      } else {
        output <- rbind(output, output.region)
      }
      
    } else if (model.type == "lmer") {
      
      #TODO: set up lmer (does not work with tidy(modeloutput)!)
      ### LMER
      
      print("Running model: LMER")
      
      m.region <- lmer(m.formula, data = df.region)
      
      # 1.1 Residuals + fitted values
      output.region <- cbind(data.frame(Residuals = residuals(m.region),
                                        FittedValues = fitted.values(m.region)))
      # 1.2 Recover original values (only to check if the output df is correct)
      original.region <- m.region$model
      colnames(original.region) <- paste0("Model_", colnames(original.region))
      output.region <- cbind(output.region, original.region)

    } else {
      
      ### By subject LMs 
      for (subj in subjects) {
        
        # Subset data 
        df.subj <- filter(df.region, Subject == subj)
        
        # Run model
        m.subj <- lm(m.formula, data = df.subj)
        
        # Step 1: residuals, fitted values; recover original values
        
        # 1.1 Residuals + fitted values
        output.subj <- cbind(data.frame(Residuals = residuals(m.subj),
                                        FittedValues = fitted.values(m.subj)))
        
        # 1.2 Recover original values (only to check if the output df is correct)
        original.subj <- m.subj$model
        colnames(original.subj) <- paste0("Model_", colnames(original.subj))
        output.subj <- cbind(output.subj, original.subj)
        
        # Step 2: Coefficients 
        m.subj.coefs <- tidy(m.subj)    # extract coefficients 

        for (r in 1:nrow(m.subj.coefs)) {
          term <- m.subj.coefs[r, "term"][[1]]
          term <- paste("Coef", term, sep = "_")
          estimate <- m.subj.coefs[r, "estimate"][[1]]
          output.subj[, term] <- estimate
        }
        
        # Bind to original df (or create)
        if (exists("output") == FALSE) {
          output <- output.subj
        } else {
          output <- rbind(output, output.subj)
        }
        
        # Check if the data frame already exists and add coefficient estimates
        #if (exists("m.tidy.all") == FALSE) {
        #  m.tidy.all <- m.subj.coefs
        #} else {
        #  m.tidy.all <- rbind(m.tidy.all, m.subj.coefs)
        #}
      }
    }
    
  }
  
  # Merge model output with df 
  output <- cbind(df, output)
  
  ### Checks
  # 1. Check if the model df and original RTs are identical
  print(summary(output[, as.character(DV)] == output[, paste0("Model_", as.character(DV))]))
  # 2. Check if the model's fitted values + residuals are equal to the original RV
  print(summary(round(output$FittedValues + output$Residuals) == round(output[, as.character(DV)])))
  # 3. Check that the mean of the residuals in each region is == 0
  print(aggregate(Residuals ~ Region, FUN = meanRound, data = output))
  # 4. Check if the fitted values are equal to the mean in the region 
  fitted.means <- aggregate(FittedValues ~ Region, FUN = meanRound, data = output)
  original.means <- aggregate(get(DV) ~ Region, FUN = meanRound, data = df)
  fitted.vs.original.means <- merge(fitted.means, original.means, by = "Region")
  print(fitted.vs.original.means)
  
  # Return output data frame
  output
}


# Function for rRT plots
generatePlots <- function(model.output, model.name, model.type,
                          DV, y.unit, y.range, y.range.res, 
                          coef.plot = TRUE) {

  # Add model type to model name:
  model.types <- c("lm" = " (simple LM)",
                   "lmer" = " (LMER)",
                   "lm.by.subj" = " (by-subject LMs)")
  model.name <- paste0(model.name, model.types[model.type])
  
  # Observed data
  p.observed <- plotSPR(model.output, DV, "Observed RTs", y.unit, y.range)
  
  # Estimates
  p.estimates <- plotSPR(model.output, "FittedValues", "Estimated RTs", y.unit, y.range)

  # Residuals
  p.residuals <- plotSPR(model.output, "Residuals", "Residuals", y.unit, y.range.res)
  
  # Coefficients
  if (coef.plot == TRUE) {
    p.coefs <- plotCoefs(model.output, DV, "Coefficients", y.unit, y.range)

    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, p.coefs, nrow=2,
                      top = textGrob(model.name, gp=gpar(fontsize=14)))  # fontface = 2
    
    # Save
    filename <- paste0("./plots/", DV, "_", model.name, ".png")
    ggsave(filename, plot = p, width = 8, height = 6)
  } else {
    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, nrow=1,
                      top = textGrob(model.name, gp=gpar(fontsize=14)))  # fontface = 2
    # Save
    filename <- paste0("./plots/", DV, "_", model.name, ".png")
    ggsave(filename, plot = p, width = 12, height = 6)
  }
  p
}

plotSPR <- function(df, DV, plt.title, y.unit, y.range) {
  
  # Aggregate data 
  cond.means <- aggMeansSE(df, c(DV, "Region", "Cond"))
  y.var <- paste0("Mean", DV)

  # Create plot
  p <- cond.means %>% ggplot(aes(x = Region, y = get(y.var),
                                 color=Cond, group=Cond)) + 
    geom_point(size=2.5, shape="cross") + 
    geom_line(size=0.5) +
    geom_errorbar(aes(ymin = get(y.var) - SE,
                      ymax = get(y.var) + SE),
                  width=0.1, size=0.3) +
    ylim(y.range[1], y.range[2]) +
    ylab(y.unit) +
    ggtitle(plt.title) +
    scale_color_manual("Condition",  # Legend title
                       values=c("cornflowerblue", "chartreuse3",
                                "tomato2", "darkgoldenrod1")) +
    theme_minimal()
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", "0" = "Critical",
                                         "1" = "Spillover", "2" = "Post-spillover"))
  }
  
  # Add horizontal line for residual plot
  if (DV == "Residuals") {
    p <- p + 
      geom_hline(yintercept=0, linetype="dashed")
  }
  p
}

plotCoefs <- function(df, DV, plt.title, y.unit, y.range) {
  
  # Reduce df to coefficient columns and Region
  df <- df %>% dplyr::select("Region", starts_with("Coef_"))
  df <- unique(df)
  
  # Get all coefficient column names; remove the "Coef_" prefix 
  coef_colnames <- str_remove(names(df), "Coef_")
  names(df) <- coef_colnames

  # Add the intercept to each coefficient estimate (except in the intercept only case)
  if (length(coef_colnames) > 2) {
    for (colname in coef_colnames[3:length(coef_colnames)]) {
      df[, colname] <- df[, colname] + df[, "(Intercept)"]
    }
  }
  
  # Transform from wide to long format, keeping the Region column
  df <- df %>% gather(Coefficient, Estimate, -c(Region))

  # Aggregate data 
  coef.means <- aggMeansSE(df, c("Estimate", "Coefficient", "Region"))
  print(coef.means)

  # Manual color palette 
  coef_plot_colors <- c("#222222",  # black for the intercept
                        brewer.pal(n=max(3, length(unique(df$Coefficient))),
                                   "Pastel2"))[1:length(unique(df$Coefficient))]

  # Generate plot
  p <- coef.means %>% ggplot(aes(x = Region, y=MeanEstimate,
                                 color = Coefficient, group = Coefficient)) + 
    geom_point(size=2.5, shape="cross") + 
    geom_line(size=0.5) +
    geom_errorbar(aes(ymin = MeanEstimate - SE,
                      ymax = MeanEstimate + SE),
                  width=0.1, size=0.3) +
    #ylim(y.range[1], y.range[2]) +  # remove ylim for coef plots!
    ylab(y.unit) +
    ggtitle(plt.title) +
    scale_color_manual(values = coef_plot_colors) + 
    theme_minimal()
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", "0" = "Critical",
                                         "1" = "Spillover", "2" = "Post-spillover"))
  }
  
  p
}

