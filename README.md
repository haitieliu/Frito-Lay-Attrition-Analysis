
# Frito-Lay-Attrition-Analysis
![](https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Fritolay_company_logo.svg/1280px-Fritolay_company_logo.svg.png)
- In this analysis, we will be acting as a talent management analytics company for Fortune 100 company Frito Lay to predict employee turnover. As our lead data scientist, I will analyze existing employee data to identify the top three factors contributing to attrition and job role-specific trends. The analysis will involve robust experimentation and appropriate visualization in R, with a predictive model built to forecast employee turnover.  

  

- We have also developed algorithm simulator App for easier digestion of future data set into streamlined process. Users will be able to voluntarily upload the data set and choose the corresponding ML model to find out the best fitting results for future attrition analysis. The R shiny app is made available below. 


- [Use Rshiny App](https://haitieliu.shinyapps.io/Project2/) 

- [Full MarkDown Report](https://haitieliu.github.io/Rscript.html)




## Installation

1. download and install package
```{r}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(class)
library(e1071)
library(GGally)
library(ROCit)
```


2. Loading the trained data set, labled here as the casestudy2
## Objectives

1. We will first be cleaning out the training data, looking for any NA values and outliers, then standardize the data across the board. Encode the categorical variables into factor level. 

 
 

2. Runing hypothesis test between people who left and people who stayed determining what factor(s) drive attrition  

 
 

3. Fit the training data sets into selected model, here I chose Knn, Knn.cv, Navie Bayes and Linear Regression 

 
 

4. Use trained models to help identify future attrition! 



## Model Similator (R Shiny App)

A model simulator app was built to streamline the process described above, we can use future new data sets directly uploaded into our app and the system will automatically produce the best fitting results to expedite future attrition analysis. Link is provide above


## Authors

- [@haitieliu](https://www.github.com/octokatherine)

- [An exaple of this project can be accessed here](https://haitieliu.github.io/Rscript.html)
