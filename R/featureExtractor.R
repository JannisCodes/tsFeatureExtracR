#' Feature Extraction Function
#'
#' This function extracts features from a given dataset, grouping it by an id variables, and summarizing it into the time series features
#' for each item in the given list of items. It generates univariate features such as mean, median, standard deviation,
#' mean absolute deviation, root mean square of successive differences, and mean absolute change.
#' It also generates multivariate, applying linear and non-linear trends using generalized additive models (GAMs),
#' and calculates auto-regressive components and their confidence intervals.
#' It standardizes the features, and outputs a list containing raw and standardized features, as well as the GAM models.
#'
#' @param data A data frame from which to extract features.
#' @param items A character vector of the names of the variables for which to compute features.
#' @param pid A character string specifying the name of the person identifier variable.
#' @param tid A character string specifying the name of the time identifier variable.
#'
#' @return A list containing:
#'    * features: A data frame of raw features.
#'    * featuresZ: A data frame of standardized features.
#'    * featuresZMat: A data frame of standardized features with person identifiers as row names.
#'    * gam: A list of fitted GAM models for each person.
#'
#' @examples
#' \dontrun{
#' featureExtractor(df, c("item1", "item2"), "ID", "time")
#' }
#'
#' @importFrom dplyr %>% group_by summarise select filter arrange all_of any_of
#' @importFrom stats lm median sd mad predict acf coef na.omit na.pass quantile
#' @importFrom psych rmssd
#' @importFrom mgcv gam
#' @importFrom MASS mvrnorm
#' @importFrom nlme corAR1
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom ggplot2 aes ggplot geom_histogram labs
#'
#' @export


featureExtractor <- function(data, items, pid, tid) {

  # Change the names of the identifier columns to "ID" and "TIDnum"
  names(data)[names(data) == pid] <- "ID"
  names(data)[names(data) == tid] <- "TIDnum"

  # Generate univariate features
  featUnivar <- data %>%
    group_by(ID) %>%
    summarise(across(
      any_of(items),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ stats::median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        mad = ~ stats::mad(.x, na.rm = TRUE),
        rmssd = ~ psych::rmssd(.x, group=ID, lag = 1, na.rm=TRUE) %>% as.numeric,
        mac = ~ sum(abs(diff(.x, lag = 1)), na.rm = TRUE)/(sum(!is.na(.x)) - 1)
      )
    ))

  # Initialize Multivariate and time-based features
  featMultivar <- featUnivar %>%
    select(ID)

  # Initialize progress bar
  Results_GAM <- sapply(as.character(unique(data$ID)), function(x) NULL)
  pb <- txtProgressBar(min = 0, max = length(unique(featMultivar$ID)), style = 3)

  # Iterate over each ID and calculate the Multivariate and time-based features
  for (i in unique(featMultivar$ID)) {
    for (j in items) {
      if (sum(!is.na(featData[[j]][featData$ID == i])) == 0) next # skip if all IV vals NA

      # linear trend
      featMultivar[[paste(j, "lin", sep = "_")]][featMultivar$ID == i] <-
        stats::lm(data = featData %>% filter(ID == i),
           formula = get(j) ~ TIDnum)$coefficients[["TIDnum"]]

      # non-linear trend
      dfGam <- data %>%
        filter(ID == i) %>%
        select(time = TIDnum,
               var = all_of(j) )#,
      #varLag1 = all_of(as.character(paste0(j, "_lag1"))),
      #varLag2 = all_of(as.character(paste0(j, "_lag2"))),
      #varLag14 = all_of(as.character(paste0(j, "_lag14"))))

      varLength <- length(unique(na.omit(dfGam$var)))
      k <- ifelse(varLength<10, varLength, 10)
      featMultivar[[paste(j, "n", sep = "_")]][featMultivar$ID == i] <- varLength
      #featMultivar[[paste(j, "k", sep = "_")]][featMultivar$ID == i] <- k

      if (k <=2) next # skip two or less data points in DV because no spline possible

      # Estimate GAMs
      gam_y <- mgcv::gam(var ~ s(time, k=k, bs="tp"), data = dfGam, method = "REML")
      featMultivar[[paste(j, "edf", sep = "_")]][featMultivar$ID == i] <- summary(gam_y)[["edf"]]

      # Predict mean values
      newd <- data.frame(time = dfGam$time)
      Xp <- stats::predict(gam_y, newd, type="lpmatrix", seWithMean = TRUE)
      mean_y <- Xp[,1:k] %*% coef(gam_y)[1:k]

      # CIs for predicted values
      nboot <- 10000 # note that nboot is also used for calculating the confidence intervals around the derivative
      modr.mean.y <- MASS::mvrnorm(nboot, coef(gam_y), gam_y$Vp+diag(k)*k^(-30))
      mean.bs.y <- matrix(NA, nrow(dfGam), nboot)
      for (m in 1 : nboot) {
        mean.bs.y[,m] <-  Xp %*% modr.mean.y[m,]
      }
      meanYCI <- data.frame(
        TIDnum = dfGam$time,
        mean_y = mean_y,
        mean_y_ll = apply(mean.bs.y,1,quantile,.025),
        mean_y_ul = apply(mean.bs.y,1,quantile,.975)
      )

      # innertia
      ar <- stats::acf(dfGam$var, na.action=na.pass, plot=FALSE, lag.max = 14)
      featMultivar[[paste(j, "ar01", sep = "_")]][featMultivar$ID == i] <- ar$acf[2]
      featMultivar[[paste(j, "ar02", sep = "_")]][featMultivar$ID == i]  <- ar$acf[3]
      # featMultivar[[paste(j, "ar14", sep = "_")]][featMultivar$ID == i]  <- ar$acf[15]

      # Find out why these are different:
      # ar1 <- rcorr(dfGam$varLag1, dfGam$var)[["r"]][1,2]
      # ar2 <- rcorr(dfGam$varLag2, dfGam$var)[["r"]][1,2]
      # ar14 <- rcorr(dfGam$varLag14, dfGam$var)[["r"]][1,2]

      # Estimate GAM with AR1 corrected
      if (!is.na(ar$acf[2]) && k > 3) { # only estimate gam with AR1 if more than 10 data points are available
        gam_ar <-
          mgcv::gamm(
            var ~ s(time, k = k-1, bs = "tp"),
            data = dfGam,
            correlation = nlme::corAR1(form = ~ time, fixed = TRUE),
            control = list(
              opt = "nlminb",
              sing.tol = 1e-20,
              maxIter = 1e100
            )
          )
        featMultivar[[paste(j, "edf_ar", sep = "_")]][featMultivar$ID == i] <- summary(gam_ar$gam)[["edf"]]

        Xar <- predict(gam_ar$gam, newd, type="lpmatrix", seWithMean = TRUE)
        mean_ar <- cbind(newd, pred = Xar[,1:k-1] %*% coef(gam_ar$gam)[1:k-1])
        modr.mean.ar <- mvrnorm(nboot, coef(gam_ar$gam), gam_ar$gam$Vp+diag(k-1)*(k-1)^(-30))
        mean.bs.ar <- matrix(NA, nrow(dfGam), nboot)

        for (m in 1 : nboot) {
          mean.bs.ar[,m] <-  Xar %*% modr.mean.ar[m,]
        }
        meanArCI <- data.frame(
          TIDnum = dfGam$time,
          mean_ar = mean_ar,
          mean_ar_ll = apply(mean.bs.ar, 1, quantile, .025),
          mean_ar_ul = apply(mean.bs.ar, 1, quantile, .975)
        )
      } else {
        featMultivar[[paste(j, "edf_ar", sep = "_")]][featMultivar$ID == i] <- NA
        meanArCI <- data.frame(
          TIDnum = dfGam$time,
          mean_ar = NA,
          mean_ar_ll = NA,
          mean_ar_ul = NA
        )
      }

      # Merge mean CIs
      meanCI <- merge(meanYCI, meanArCI)

      # gam residual ar
      gam_y_acf <- acf(gam_y$residuals, plot=FALSE)
    }
    Results_GAM[[i]] <- list(
      "gamModel" = gam_y,
      "origDf" = df,
      "gamCI" = meanCI
    )
    setTxtProgressBar(pb, i)
    #if (i == length(unique(featMultivar$ID))) close(pb)
  }

  # Merge the univariate features and multivariate features
  featOut <-
    merge(
      featUnivar, featMultivar
    ) %>%
    arrange(ID) %>%
    select(
      ID,
      sort(colnames(.))
    )

  # Standardize the features
  featOutZ <-
    featOut %>%
    mutate(
      across(
        c(everything(), -ID),
        scale
      )
    )

  # Convert the ID column to row names
  featOutZRowNam <-
    featOutZ %>%
    column_to_rownames("ID") %>%
    mutate(across(everything(), as.vector)) # remove attributes

  # Prepare output
  out <- list(
    features = featOut,
    featuresZ = featOutZ,
    featuresZMat = featOutZRowNam,
    gam = Results_GAM
  )

  # End the progressbar
  close(pb)

  # Return output
  return(out)
}
