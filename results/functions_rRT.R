### Functions for the rRT analysis
### Miriam Schulz
### 15 October 2022



#' Function to run the model(s) and generate the plots from a single command
#'
#' @param plt.name A character string representative of the model formula
#' @param m.formula A model formula
#' @param df Model data frame 
#' @param m.type A character string indicating which model to run. Options: "lm", "lmer", "lm.by.subj"
#' @param regions A numeric vector of the regions of analysis to select from the data frame
#' @param DV A character string of the dependent variable (RT or logRT)
#' @param y.unit 
#' @param y.range 
#' @param y.range.res 
#' @param coef.plot Boolean 
#'
#' @return
#' @export
#'
#' @examples
modelAndPlot <- function(plt.name,
                         m.formula,
                         df,
                         m.type,
                         regions,
                         DV,
                         y.unit,
                         y.range,
                         y.range.res,
                         coef.plot = TRUE) {
  
  # Run a model for each region
  m <- runModels(df, m.type, m.formula, regions = regions)
  
  # Generate plots
  p <- generatePlots(m, plt.name, m.type,
                     DV, y.unit, y.range, y.range.res,
                     coef.plot = coef.plot)
}



#' Function to run rRT models
#'
#' @param df A data frame containing the input for the model
#' @param model.type A character string indicating which model to run. Options: "lm", "lmer", "lm.by.subj"
#' @param m.formula Model formula
#' @param regions A numeric vector of the regions of analysis to select from the data frame
#'
#' @return A data frame containing the model output: residuals, fitted values and coefficients, alongside the input data
#' @export
#'
#' @examples
runModels <- function(df, model.type, m.formula,
                      regions = -1:2, print.checks=FALSE) {
  
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
      
      # Bind to original df (or create)
      if (exists("output") == FALSE) {
        output <- output.region
      } else {
        output <- rbind(output, output.region)
      }
      
    } else if (model.type == "lmer") {
      
      ### LMER
      m.region <- lmer(m.formula, data = df.region)
      
      # 1.1 Residuals + fitted values
      output.region <- cbind(data.frame(Residuals = residuals(m.region),
                                        FittedValues = fitted.values(m.region)))

      # 1.2 Recover original values (only to check if the output df is correct)
      original.region <- model.frame(m.region)
      colnames(original.region) <- paste0("Model_", colnames(original.region))
      output.region <- cbind(output.region, original.region)
      
      # Step 2: Coefficients 
      m.coefs <- fixef(m.region)      # extract coefficients 
      for (i in 1:length(names(m.coefs))) {
        term <- names(m.coefs)[[i]]
        term <- paste("Coef", term, sep = "_")
        estimate <- m.coefs[[i]]
        output.region[, term] <- estimate
      }
      
      # Bind to original df (or create)
      if (exists("output") == FALSE) {
        output <- output.region
      } else {
        output <- rbind(output, output.region)
      }
      
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
      }
    }
  }
  
  # Merge model output with df 
  output <- cbind(df, output)
  
  ### Checks
  if (print.checks == TRUE) {
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
  }
  
  # Return output data frame
  output
}


#' Function to generate and export all rRT plots for a given model output
#'
#' @param model.output A dataframe containing the model output
#' @param model.name A character string for the title of the plot, typically the model formula
#' @param model.type A character string indicating which model to run. Options: "lm", "lmer", "lm.by.subj"
#' @param DV A character string: which version of the DV to use ("RT" or "logRT")
#' @param y.unit A character string: label of the y axis
#' @param y.range A numeric vector: range for the y axis of the Observed + Estimate plots
#' @param y.range.res A numeric vector: range for the y axis of the residuals plot
#' @param coef.plot Boolean: if TRUE, plots the model coefficients (use FALSE for interaction models with too many coefficients)
#'
#' @return A ggplot2 grid.arranged plot of 3 or 4 plots (coefficient plot optional)
#' @export
#'
#' @examples
generatePlots <- function(model.output, model.name, model.type,
                          DV,
                          y.unit,
                          y.range,
                          y.range.res, 
                          coef.plot = TRUE) {
  
  # Add model type to model name:
  model.types <- c("lm" = " (simple LM)",
                   "lmer" = " (LMER)",
                   "lm.by.subj" = " (by-subject LMs)")
  model.name <- paste0(model.name, model.types[model.type])
  
  # Observed data
  p.observed <- plotSPR(model.output, DV, "Observed RTs", y.unit, y.range)
  
  # Estimates
  p.estimates <- plotSPR(model.output, "FittedValues", "Estimated RTs", 
                         y.unit, y.range)
  
  # Residuals
  p.residuals <- plotSPR(model.output, "Residuals", "Residuals", 
                         y.unit, y.range.res)
  
  # Coefficients
  if (coef.plot == TRUE) {
    # Generate coefficient plot 
    p.coefs <- plotCoefs(model.output, DV, "Coefficients", y.unit, y.range)
    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, p.coefs, nrow=2,
                      top = textGrob(model.name, gp=gpar(fontsize=14)))  # fontface = 2
  } else {
    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, nrow=2,
                      top = textGrob(model.name, gp=gpar(fontsize=14)))  # fontface = 2
  }
  # Save
  filename <- paste0("./plots/", DV, "_", model.name, ".png")
  #ggsave(filename, plot = p, width = 12, height = 6)  # for 3 plots, excluding coef plot
  ggsave(filename, plot = p, width = 8, height = 6)
  p
}


#' Function to generate an individual plot
#'
#' @param df A data frame containing the model output 
#' @param DV A character string: which version of the DV to use ("RT" or "logRT")
#' @param y.unit A character string: label of the y axis
#' @param y.range A numeric vector: range for the y axis
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
plotSPR <- function(df, DV, y.unit, y.range) {
  
  # Aggregate data 
  cond.means <- aggregMeans(df, as.formula(get(DV) ~  Region + Cond))

  pd <- position_dodge(0)  # set to 0 for no jitter/position dodge

  # Create plot
  p <- cond.means %>% ggplot(aes(x = Region, y = Mean,
                                 color=Cond,
                                 shape=Cond,
                                 group=Cond,
                                 )) + 
    geom_errorbar(aes(ymin = Mean - SE,
                      ymax = Mean + SE),
                  width=1,
                  linewidth=0.7,
                  position = pd) +
    geom_line(linewidth=1.5,
              position = pd) +
    geom_point(size=6,
               #shape="cross",
               #shape=19,
               position = pd) + 
    #geom_jitter(position = position_jitter(width = 0.2)) +
    ylim(y.range[1], y.range[2]) +
    ylab(y.unit) +
    #ggtitle(plt.title) +
    scale_color_manual("Condition",  # Legend title
                       values=c("cornflowerblue", "chartreuse3",
                                "tomato2", "darkgoldenrod1")) +
    #scale_shape_manual(values = c(19,17,15,18)) +
    scale_shape_manual(values = rep("cross", times=4)) +
    guides(shape=guide_legend(title="Condition"),
           color=guide_legend(title="Condition"),
           linetype=guide_legend(title="Condition")) +
    theme_minimal() + 
    theme(text = element_text(size = 25),
          legend.key.width=unit(1,"cm"))
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", 
                                         "0" = "Critical",
                                         "1" = "Spillover", 
                                         "2" = "Post-spillover"))
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
    geom_line(linewidth=0.5) +
    geom_errorbar(aes(ymin = MeanEstimate - SE,
                      ymax = MeanEstimate + SE),
                  width=0.1, linewidth=0.3) +
    #ylim(y.range[1], y.range[2]) +  # remove ylim for coef plots!
    ylab(y.unit) +
    ggtitle(plt.title) +
    scale_color_manual(values = coef_plot_colors) + 
    theme_minimal()
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", 
                                         "0" = "Critical",
                                         "1" = "Spillover", 
                                         "2" = "Post-spillover"))
  }
  
  p
}