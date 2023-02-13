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
runModels <- function(df,
                      model.type,
                      m.formula,
                      regions = -1:2,
                      print.checks=FALSE) {
  
  # Print chosen model type 
  model.types <- c("lm" = "simple LM",
                   "lmer" = "LMER",
                   "lm.by.subj" = "by-subject LMs")
  if (!model.type %in% names(model.types)) {
    stop("Please specify a model type from the following options: lm, lm.by.subj, lmer")
  }
  cat(paste0("Chosen model: ", model.types[model.type]))
  
  # Order df by Region, then Subject (important for the final merging step)
  df <- filter(df, Region %in% regions) %>% arrange(Region, Subject)
  
  # Extract DV from formula
  DV <- m.formula[[2]]
  
  # Extract subjects 
  subjects <- unique(df$Subject)
  
  # Reset the results data frame
  if (exists("output")) {rm("output")}
  
  # Loop over the regions and run one model per region 
  for (region in regions) {
    
    #print(paste0("Processing region: ", region))
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
      
      # Step 2: Coefficients  + coefficient SE
      m.coefs <- fixef(m.region)          # extract coefficients 
      m.se <- sqrt(diag(vcov(m.region)))  # extract SE for each coefficient
      m.p <- anova(m.region)[6]$`Pr(>F)`  # extract p-value for each coefficient
      for (i in 1:length(names(m.coefs))) {
        term <- names(m.coefs)[[i]]
        term.coef <- paste("Coef", term, sep = "_")
        term.se <- paste("SE", term, sep = "_")
        term.zscore <- paste("ZSCORE", term, sep = "_")
        term.pvalue <- paste("PVALUE", term, sep = "_")
        estimate <- m.coefs[[i]]
        se <- m.se[[i]]
        output.region[, term.coef] <- estimate
        output.region[, term.se] <- se
        output.region[, term.zscore] <- estimate / se
        # Append p-value only if term is not intercept:
        #if ((i < length(names(m.coefs))+1) & !term == "(Intercept)") {
        if (!term == "(Intercept)") {
          output.region[, term.pvalue] <- m.p[[i-1]]
        }
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
generatePlots <- function(model.output,
                          model.name,
                          model.type,
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
  filename <- paste0("./plots/", DV, "_", model.name, ".png")
  
  # Observed data
  p.observed <- plotSPR(model.output, DV, y.unit, y.range)
  p.observed <- p.observed + ggtitle("Observed RTs")
  
  # Estimates
  p.estimates <- plotSPR(model.output, "FittedValues",
                         y.unit, y.range)
  p.estimates <- p.estimates + ggtitle("Estimated RTs")
  
  # Residuals
  p.residuals <- plotSPR(model.output, "Residuals",
                         y.unit, y.range.res)
  p.residuals <- p.residuals + ggtitle( "Residuals")
  
  # Coefficients
  if (coef.plot == TRUE) {
    # Generate coefficient plot 
    p.coefs <- plotCoefs(model.output)
    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, p.coefs, nrow=2,
                      top = textGrob(model.name, gp=gpar(fontsize=14)))  # fontface = 2
    ggsave(filename, plot = p, width = 8, height = 6)
    
  } else {
    # Combine into one plot
    p <- grid.arrange(p.observed, p.estimates, p.residuals, nrow=1,
                      top = textGrob(model.name, gp=gpar(fontsize=20)),
                      padding=unit(2, "cm"))  # fontface = 2
    # Save
    ggsave(filename, plot = p, width = 12, height = 5)  # for 3 plots, excluding coef plot
  }
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
  cond.means <- aggregMeans(df, as.formula(get(DV) ~ Region + Cond))

  pd <- position_dodge(0)  # set to 0 for no jitter/position dodge

  # Create plot
  p <- cond.means %>% ggplot(aes(x = Region,
                                 y = Mean,
                                 color=Cond,
                                 #shape=Cond,
                                 group=Cond,
                                 )) + 
    geom_errorbar(aes(ymin = Mean - SE,
                      ymax = Mean + SE),
                  width=0.5,
                  linewidth=0.3,
                  position = pd) +
    geom_line(linewidth=0.5,
              position = pd) +
    geom_point(size=3,
               shape="cross",
               position = pd) + 
    ylim(y.range[1], y.range[2]) +
    ylab(y.unit) +
    scale_color_manual("Condition",  # Legend title
                       values=c("A" = "cornflowerblue",
                                "B"= "chartreuse3",
                                "C"= "tomato2",
                                "D"= "darkgoldenrod1")) +
    #scale_shape_manual(values = c(19,17,15,18)) +
    guides(#shape=guide_legend(title="Condition"),
           color=guide_legend(title="Condition"),
           linetype=guide_legend(title="Condition")) +
    theme_minimal() + 
    theme(text = element_text(size = 11),
          #legend.key.width=unit(1,"cm"),
          #legend.key.size = unit(0.3, "cm"),
          legend.title = element_text(size=7),
          legend.text = element_text(size=7),
          legend.position = "bottom",
          plot.margin = unit(c(0,1,0,0), "cm"), # prevent x-axis text from being cut off at the right
          panel.grid.minor.x = element_blank() # remove vertical lines between x ticks
          #panel.grid.major = element_blank()
          #panel.grid.major.x = element_line(color = "grey",
          #                                  size = 0.5,
          #                                  linetype = "dotted")
          )
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Pre-critical", 
                                         "0" = "Critical",
                                         "1" = "Spillover", 
                                         "2" = "Post-spillover"))
  } else if (setequal(range(df$Region), c(0, 2))) {
    p <- p + scale_x_continuous(breaks=seq(0,2,1),
                                labels = c("0" = "Critical",
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


#' Plot coefficients
#'
#' @param df A data frame: model output, containing coefficient estimates
#'
#' @return
#' @export
#'
#' @examples
#' plotCoefs(model.output)
plotCoefs <- function(df) {

  # SE
  SEs <- df %>% dplyr::select("Region", starts_with("SE_")) %>% unique()

  # Reduce df to coefficient columns and Region
  df <- df %>% dplyr::select("Region", starts_with("Coef_")) %>% unique()

  # Get all coefficient column names; remove the "Coef_" prefix 
  coef_colnames <- str_remove(names(df), "Coef_")
  names(df) <- coef_colnames
  SEs.colnames <- str_remove(names(SEs), "SE_")
  names(SEs) <- SEs.colnames

  # Add the intercept to each coefficient estimate (except in the intercept only case)
  if (length(coef_colnames) > 2) {
    for (colname in coef_colnames[3:length(coef_colnames)]) {
      df[, colname] <- df[, colname] + df[, "(Intercept)"]
    }
  }
  
  # Transform from wide to long format, keeping the Region column
  df <- df %>% gather(Coefficient, Estimate, -c(Region))

  SEs <- SEs %>% gather(Coefficient, Estimate, -c(Region)) %>%
    rename(SE = Estimate)
  coef.means <- merge(df, SEs, by=c("Region", "Coefficient"))

    # Aggregate data 
  #coef.means <- aggregMeans(df, as.formula(Estimate ~ Coefficient + Region))
  #print(coef.means)

  # Manual color palette 
  #coef_plot_colors <- c("#222222",  # black for the intercept
  #                      brewer.pal(n=max(3, length(unique(df$Coefficient))),
  #                                 "Pastel2"))[1:length(unique(df$Coefficient))]
  coef_plot_colors <- c("#222222",  # black for the intercept
                        "deeppink2", "purple3", "cyan2",
                        "lightseagreen", "plum2", "powderblue",
                        "rosybrown1", "darkolivegreen1", "aquamarine",
                        "cadetblue1"
                        )[1:length(unique(df$Coefficient))]
  
  # Generate plot
  p <- coef.means %>%
    ggplot(aes(x = Region, y=Estimate,
               color = Coefficient, group = Coefficient)) + 
    geom_point(size=2.5, shape="cross") + 
    geom_line(linewidth=0.5) +
    geom_errorbar(aes(ymin = Estimate - SE,
                      ymax = Estimate + SE),
                  width=0.1, linewidth=0.3) +
    #ylim(y.range[1], y.range[2]) +  # remove ylim for coef plots!
    ylab("Model coefficient") +
    ggtitle("Coefficients") +
    scale_color_manual(values = coef_plot_colors) + 
    theme_minimal()
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", 
                                         "0" = "Critical",
                                         "1" = "Spillover", 
                                         "2" = "Post-spillover"))
   } else if (setequal(range(df$Region), c(0, 2))) {
     p <- p + scale_x_continuous(breaks=seq(0,2,1),
                                 labels = c("0" = "Critical",
                                            "1" = "Spillover",
                                            "2" = "Post-spillover"))
   }
  p
}


#' Plot SPR effect size + pvalue
#'
#' @param df A data frame: model output, containing coefficient estimates, SE, Zscore and Pvalue
#'
#' @return
#' @export
#'
#' @examples
plotEffects <- function(df) {
  # Reduce df to effect columns and Region
  zscores <- df %>% dplyr::select("Region",
                                  starts_with("ZSCORE_")) %>% unique()
  SEs <- df %>% dplyr::select("Region",
                                starts_with("SE_")) %>% unique()
  pvals <- df %>% dplyr::select("Region",
                                starts_with("PVALUE_")) %>% unique() #%>% 
    #mutate_if(is.numeric, is.significant)

  # Get all coefficient column names; remove the "ZSCORE_" prefix 
  zscores.colnames <- str_remove(names(zscores), "ZSCORE_")
  names(zscores) <- zscores.colnames
  # Get all coefficient column names; remove the "SE_" prefix 
  SEs.colnames <- str_remove(names(SEs), "SE_")
  names(SEs) <- SEs.colnames
  # Get all pvalue column names; remove the "PVALUE_" prefix 
  pvals.colnames <- str_remove(names(pvals), "PVALUE_")
  names(pvals) <- pvals.colnames
  
  # Transform from wide to long format, keeping the Region column
  zscores <- zscores %>% gather(Coefficient, Estimate, -c(Region)) %>%
    rename(Zscore = Estimate)
  SEs <- SEs %>% gather(Coefficient, Estimate, -c(Region)) %>%
    rename(SE = Estimate)
  pvals <- pvals %>% gather(Coefficient, Estimate, -c(Region)) %>% 
    rename(Pvalue = Estimate)
  
  # Remove intercept from zscores+SE
  zscores <- zscores %>%
    filter(Region != "(Intercept)") 
  SEs <- SEs %>%
    filter(Region != "(Intercept)") 
  
  df <- merge(zscores, SEs, by=c("Region", "Coefficient"))
  df <- merge(df, pvals, by=c("Region", "Coefficient"))
  df$Significant <- factor(ifelse(df$Pvalue < 0.05, "p<0.05", "ns"))

  effect_plot_colors <- c("deeppink2", "purple3", "cyan2",
                          "lightseagreen", "plum2", "powderblue",
                          "rosybrown1", "darkolivegreen1", "aquamarine", 
                          "cadetblue1"
  )[1:length(unique(df$Coefficient))]
  
  # Generate plot
  p <- df %>%
    ggplot(aes(x = Region,
               y=Zscore,
               color = Coefficient,
               group = Coefficient,
               shape = Significant)) + 
    #geom_point(size=2.5, shape="cross") + 
    geom_point(size=2.5) + 
    geom_line(linewidth=0.5) +
    ylab("Z score") +
    ggtitle("Effect size") +
    scale_color_manual(values = effect_plot_colors) + 
    scale_shape_manual(values = c(1, 8)) +  # shapes for the p-values
    geom_hline(yintercept=0, linetype="dashed") +
    theme_minimal()
  
  # Add region labels if the regions range from precritical to post-spillover
  if (setequal(range(df$Region), c(-1, 2))) {
    p <- p + scale_x_continuous(labels=c("-1" = "Precritical", 
                                         "0" = "Critical",
                                         "1" = "Spillover", 
                                         "2" = "Post-spillover"))
  } else if (setequal(range(df$Region), c(0, 2))) {
    p <- p + scale_x_continuous(breaks=seq(0,2,1),
                                labels = c("0" = "Critical",
                                           "1" = "Spillover",
                                           "2" = "Post-spillover"))
  }
  p
}


#' Compare residual plots for different predictor transformations
#'
#' @param df A data frame: possibly subset of the experimental conditions
#' @param var.untransformed A string: name of the first predictor column
#' @param var.log A string: name of the second predictor column
#'
#' @return A grid.arrange plot
#' @export
#'
#' @examples
#' compareLogToUntransformed(df.CD, "Association", "logAssociation")
compareLogToUntransformed <- function(df,
                                      var.untransformed,
                                      var.log,
                                      y.range.res=c(-0.055, 0.055)) {
  f1 <- as.formula(get(DV) ~ 1 + get(var.untransformed) + (1|Subject) + (1|Item))
  f2 <- as.formula(get(DV) ~ 1 + get(var.log) + (1|Subject) + (1|Item))
  df.m1 <- runModels(df, model.type="lmer", f1)
  df.m2 <- runModels(df, model.type="lmer", f2)
  p1 <- plotSPR(df.m1, "Residuals", y.unit, y.range.res) + ggtitle(var.untransformed)
  p2 <- plotSPR(df.m2, "Residuals", y.unit, y.range.res) + ggtitle(var.log)
  p <- grid.arrange(p1, p2, nrow=1,
                    top = textGrob(paste0("Comparing residuals for ",
                                          var.untransformed,
                                          " to ",
                                          var.log),
                                   gp=gpar(fontsize=14, fontface=2)))
  suppressMessages(ggsave(paste0("./plots/residual_comparison_", var.untransformed, "_",
                var.log, ".pdf"), p, dpi = 300))
  #p
}


#' Calculate the mean AIC or BIC score across different regions
#'
#' @param df A data frame
#' @param f A model formula
#' @param measure A string: either "AIC" or "BIC"
#' @param regions A mumeric vector: regions to use
#'
#' @return
#' @export
#'
#' @examples 
#' getMeanAICBIC(df, as.formula(logRT ~ Inference + (1|Subject) + (1|Item)), "AIC")
getMeanAICBIC <- function(df, f, measure, regions=-1:2) {
  if (!measure %in% c("AIC", "BIC")) {
    stop("Please choose a valid measure (AIC or BIC")
  }
  scores <- c()  # init
  for (r in regions) {
    df.region <- filter(df, Region==r)
    if (measure == "AIC") {
      aic <- AIC(lmer(f, data=df.region))
      print(paste0("AIC: ", aic))
      scores <- c(scores, aic)
    } else {
      bic <- BIC(lmer(f, data=df.region))
      print(paste0("BIC: ", bic))
      scores <- c(scores, bic)
    }
  }
  mean(scores)  # return the mean across regions
}


#' Transform a list of p-values to categorical factor: ns or p<0.05
#'
#' @param x A numeric vector of p-values
#'
#' @return A factor with levels = c(ns, p<0.05)
#' @export
#'
#' @examples
#' is.significant(x = c(0.05, 1, 0.01, 0.04))
is.significant <- function(x) {
  y <- ifelse(x < 0.05, "p<0.05", "ns")
  y <- factor(y, levels=c("ns", "p<0.05"))
  y
}
