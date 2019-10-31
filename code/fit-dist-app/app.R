# Shiny-App
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
# set-up packages
library(shiny)
## Packages
## General Packages
library(tidyverse)
# Formatting and Tables
library(kableExtra)
library(xtable)
# For plotting
library(ggplot2)
library(rlang)
theme_set(theme_bw())
# Data manipulating
library(dplyr)

# read in data - only needs to happen once
sim_results <- as_tibble(read.table('data/compiled_fit_results.txt', header=T,sep='\t'))

## Next, turn condition into a factor for plotting
sim_results$Condition <- as.factor(sim_results$Condition)

## Next, since TLI is non-normed, any value greater than 1 needs to be rescaled to 1.
sim_results$TLI <- ifelse(sim_results$TLI > 1, 1, sim_results$TLI)
sim_results$TLI <- ifelse(sim_results$TLI < 0, 0, sim_results$TLI)
## Next, summarize the results of the chi-square test of model fit. This is done simply by comparing the p-value to alpha (0.05) and indicating whether the model was flagged as fitting or not.
# Note: if  p < 0.05 then this variable is flagged as 0, and 1 otherwise
sim_results$Chi2_pvalue_decision <- ifelse(sim_results$chisqu_pvalue < 0.05, 0, 1)
# 0 = rejected that these data fit this model
# 1 = failed to reject that these data fit this model
## level-1 Sample size
ss_l1 <- c(5, 10, 30) ## 6 conditions each
ss_l2 <- c(30, 50, 100, 200) ## 18 condition each
icc_ov <- c(.1, .3, .5) ## 2 conditions each
icc_lv <- c(.1, .5) ## every other condition
nCon <- 72 # number of conditions
nRep <- 500 # number of replications per condition
nMod <- 12 ## numberof estimated models per conditions
## Total number of rows: 432,000
ss_l2 <- c(rep(ss_l2[1], 18*nRep*nMod), rep(ss_l2[2], 18*nRep*nMod), 
           rep(ss_l2[3], 18*nRep*nMod), rep(ss_l2[4], 18*nRep*nMod))
ss_l1 <- rep(c(rep(ss_l1[1],6*nRep*nMod), rep(ss_l1[2],6*nRep*nMod), rep(ss_l1[3],6*nRep*nMod)), 4)
icc_ov <- rep(c(rep(icc_ov[1], 2*nRep*nMod), rep(icc_ov[2], 2*nRep*nMod), rep(icc_ov[3], 2*nRep*nMod)), 12)
icc_lv <- rep(c(rep(icc_lv[1], nRep*nMod), rep(icc_lv[2], nRep*nMod)), 36)
## Force these vectors to be column vectors
n1 <- matrix(ss_l1, ncol=1)
n2 <- matrix(ss_l2, ncol=1)
icc_ov <- matrix(icc_ov, ncol=1)
icc_lv <- matrix(icc_lv, ncol=1)
## Add the labels to the results data frame
sim_results <- sim_results[order(sim_results$Condition),]
sim_results <- cbind(sim_results, n1, n2, icc_ov, icc_lv)
sim_results$total_n <- sim_results$n1*sim_results$n2
## Force the conditions to be factors
sim_results$n1 <- as.factor(sim_results$n1)
sim_results$n2 <- as.factor(sim_results$n2)
sim_results$total_n <- factor(sim_results$total_n, 
                              levels = sort(unique(sim_results$total_n)),
                              ordered = T)
sim_results$icc_ov <- as.factor(sim_results$icc_ov)
sim_results$icc_lv <- as.factor(sim_results$icc_lv)
sim_results$Model <- factor(sim_results$Model, levels = c('C','M1','M2','M12'), ordered = T)

sim_results <- filter(sim_results, Converge == 1)

ui <- fluidPage(
  headerPanel('ML-CFA Fit Index Distribution'),
  sidebarPanel(
    selectInput('adms',
                label = 'Select whether to include only admissible replications',
                choices = c("Yes" = "1", "No" = "0"),
                selected = 'Yes'),
    selectInput('index',
                label = 'Choose fit index to plot',
                choices = c('CFI', 'TLI', 'RMSEA', 'SRMRW', 'SRMRB'),
                selected = 'CFI'),
    checkboxGroupInput('est',
                       'Choose which estimation method(s) results to include',
                       choices = levels(sim_results$Estimator),
                       selected = levels(sim_results$Estimator)),
    helpText("Hint: You can choose between a total sample size or the specific conditions. If you select the total sample size, the individual sample size selections will be overridden."),
    checkboxGroupInput('n1',
                       'Choose which Level-1 sample size conditions to include',
                       choices = levels(sim_results$n1),
                       selected = levels(sim_results$n1)),
    checkboxGroupInput('n2',
                       'Choose which Level-2 sample size conditions to include',
                       choices = levels(sim_results$n2),
                       selected = levels(sim_results$n2)),
    checkboxGroupInput('totn',
                       'Choose which total sample size condition(s) to include',
                       choices = levels(sim_results$total_n),
                       selected = NULL),
    checkboxGroupInput('iccO',
                       'Choose which Observed ICC conditions to include',
                       choices = levels(sim_results$icc_ov),
                       selected = levels(sim_results$icc_ov)),
    checkboxGroupInput('iccL',
                       'Choose which Latent ICC conditions to include',
                       choices = levels(sim_results$icc_lv),
                       selected = levels(sim_results$icc_lv)),
    selectInput('fac',
                label = 'Choose a simulation condition divide plot up',
                choices = c("Estimation Method" = "Estimator",
                            "Level-1 Sample Size" = "n1",
                            "Level-2 Sample Size" = "n2",
                            "Total Sample Size" = "total_n",
                            "Obsered Variable ICC" = "icc_ov",
                            "Latent Variable ICC" = "icc_lv"),
                selected = "Estimation Method")
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    mydata <- filter(sim_results, Admissible == as.numeric(input$adms))
    if(is.null(input$totn) == F){
      mydata <- filter(mydata, total_n %in% as.numeric(input$totn))
    } else {
      mydata <- filter(mydata, n1 %in% as.numeric(input$n1))
      mydata <- filter(mydata, n2 %in% as.numeric(input$n2))
    }
    mydata <- filter(mydata, icc_ov %in% as.numeric(input$iccO))
    mydata <- filter(mydata, icc_lv %in% as.numeric(input$iccL))
    mydata <- filter(mydata, Estimator %in% input$est)
    
    mydata[, c(input$index, 'Model', 'Estimator', 'n1', 'n2', 'total_n', 'icc_ov', 'icc_lv')]
  })
  
  
  
  output$plot1 <- renderPlot({
    lb <- ifelse(input$index %in% c('CFI', 'TLI'), 0.5, 0)
    ub <- ifelse(input$index %in% c('CFI', 'TLI'), 1, 0.5)
    
    ggplot(selectedData(), 
           aes_string(x='Model', y=input$index))+
      geom_boxplot(outlier.alpha = .1) + 
      lims(y=c(lb,ub)) +
      facet_grid(reformulate(input$fac,"."))
  })
  
}

shinyApp(ui = ui, server = server)



