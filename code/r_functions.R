# This file contains the extra R functions I made.

# Changes the data to long format ad keeps the desired columns
data_change <- function(data, item_ids, Outcome){
  out <- data[,c("condition","rep", "error_dist","generating_model", paste0(item_ids,1:10))]
  out$id <- paste0("ID_",out$error_dist, "_",  out$generating_model, "_" , out$rep)
  out <- reshape(out, varying = list(paste0(item_ids,1:10)), direction = "long", idvar = "id")
  colnames(out) <- c(colnames(out)[1:5], "Item", Outcome)
  out$Item <- as.factor(out$Item)
  return(out)
}


# The follow check the normality and variance assumptions of ANOVA
# returns the summary stats by cell
anova_assumptions_check <- function(
  dat, outcome, factors, model = NULL, stats = NULL)
{
  if(is.null(stats) == T){
    g <- paste0('group',1:length(factors))
    stats <- c(g, 'n', 'mean', 'sd', 'skew', 
               'kurtosis', 'min', 'Q0.25','median',
               'Q0.75','max')
  }
  ## Check to see if a model was supplied
  if(is.null(model) == T){
    model <- as.formula(paste(outcome, '~', paste(factors,collapse = "*")))
  }
  
  cat('\n ============================= \n')
  cat('\n Tests and Plots of Normality:\n')
  # Assess normality
  aov.out = aov(model, data = dat)
  plot(aov.out)
  # shapiro-wilks test
  if(length(aov.out$residuals) > 5000){ 
    res <- sample(aov.out$residuals, 5000)
  } else res <- aov.out$residuals
  cat('\n Shapiro-Wilks Test of Normality of Residuals:\n')
  print(shapiro.test(res))
  
  # K-S Test 
  cat('\n K-S Test for Normality of Residuals:\n')
  print(ks.test(aov.out$residuals, 'pnorm', 
          alternative = 'two.sided'))
  cat('\n')
  
  # Histograms
  ## loop around ggplot to make histograms
  for(i in 1:length(factors)){
    print(
      ggplot(dat, aes_string(x=outcome, 
                             fill = factors[i], color = factors[i])) +
            geom_histogram(alpha=0.5, position="identity") +
            labs(title = paste(outcome, 'distribution by', factors[i]))
    ) ## Print plot 
  }
  
  cat('\n ============================= \n')
  cat('\n Tests of Homogeneity of Variance\n')
  # Varainces
    ## loop around levene's test
    for(i in 1:length(factors)){
      cat('\n \n Levenes Test: ', factors[i], '\n \n \n')
      print(leveneTest(as.formula(paste(outcome, '~',factors[i])),
                       data = dat,
                       center="mean"))
    }
  # return(summary_stats)
}


## General Form of Omega
o2 <- function(ss_num,ss_dem, df, n, mse)
{
  (ss_num - df*mse)/(ss_dem + (n-df)*mse)
}
# Estimating Omega**2
omega2 <- function(fit.sum){
  k <- length(fit.sum[[1]]$`Mean Sq`)
  ms_error <- fit.sum[[1]]$`Mean Sq`[k]
  omega <- matrix(NA,ncol=1,nrow=(k-1))
  dfs <- fit.sum[[1]]$Df
  ss <- fit.sum[[1]]$`Sum Sq`
  sst <- sum(fit.sum[[1]]$`Sum Sq`)
  for(i in 1:(k-1))
  {
    omega[i,] <- round(o2(ss[i],sst, dfs[i], dfs[i]+1, ms_error),4)
  }
  rownames(omega) <- rownames(fit.sum[[1]])[1:(k-1)]
  colnames(omega) <- "omega^2"
  return(omega)
}
# Estimate partial-omega**2
p_omega2 <- function(fit.sum){
  k <- length(fit.sum[[1]]$`Mean Sq`)
  ms_error <- fit.sum[[1]]$`Mean Sq`[k]
  N <- sum(fit.sum[[1]]$Df) + 1
  p.omega <- matrix(NA,ncol=1,nrow=(k-1))
  dfs <- fit.sum[[1]]$Df
  sss <- fit.sum[[1]]$`Sum Sq`
  for(i in 1:(k-1))
  {
    p.omega[i,] <- round(o2(sss[i],sss[i], dfs[i], N, ms_error),4)
  }
  rownames(p.omega) <- rownames(fit.sum[[1]])[1:(k-1)]
  colnames(p.omega) <- "partial-omega^2"
  return(p.omega)
}


## Function to calc hit-rates
# X = vector of fit statistic values
# cutoff = single value used to classification fitting vs. not
# direction = ">" or "<"
hit_rate_calc <- function(X, cutoff, direction = ">"){
  # X <- mydata$CFI
  # cutoff <- .95
  # direction <- ">"
  if(direction == ">")  y <- ifelse(X > cutoff, 1,0 )
  if(direction == "<")  y <- ifelse(X < cutoff, 1,0 )
  hit.rate <- mean(y, na.rm = T)
  return(hit.rate)
}

