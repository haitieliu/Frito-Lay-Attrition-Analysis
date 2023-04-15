
###encoding categorical variable 
#1=Non-Travel,2=Travel_Rarely,3=Travel_Frequently
data1$BusinessTravel=as.numeric(factor(data1$BusinessTravel,levels = c("Non-Travel","Travel_Rarely","Travel_Frequently")))
#1=Male,2=Female
data1$Gender=as.numeric(factor(data1$Gender,levels = c("Male","Female")))
#1=No, 2=Yes
data1$OverTime=as.numeric(factor(data1$OverTime,levels = c("No","Yes")))
#1=N,2=Y
data1$Over18=as.numeric(factor(data1$Over18,levels = c("N","Y")))
#standard hours
data1$StandardHours=as.numeric(factor(data1$StandardHours,levels = c("0","80")))
#Attrition
data1$Attrition=ifelse(data1$Attrition=="No",0,1)
#Department
data1$Department=as.numeric(factor((data1$Department)))
#Education Field
data1$EducationField=as.numeric(factor(data1$EducationField))
#Job Role
data1$JobRole=as.numeric(factor(data1$JobRole))
#Martial Status
data1$MaritalStatus=as.numeric(factor(data1$MaritalStatus))
```


```{r}

####scale data set

dataZ <- data.frame(data1[,1])

for (i in 2:ncol(data1)){
  dataZ[,i] <- scale(data1[,i])
}

colnames(dataZ)=colnames(data1)

names(data1)


dataZ2=dataZ

###separating data set into people who left and people who stayed

peoplewholeft=dataZ%>%filter(Attrition =="1")
peoplewhostayed=dataZ%>%filter(Attrition =="0")

peoplewholeft1=data1%>%filter(Attrition =="1")
peoplewhostayed1=data1%>%filter(Attrition =="0")

# summary(data1)  

summary(peoplewholeft)
summary(peoplewhostayed)

#calculating p value between columns for each datasets, see if there is significant difference when alpha =0.05
pvalues = numeric(36)
options(scipen=999)
# Loop through each column in peoplewholeft and compare to the corresponding column in peoplewhostaytend
# comparing p values, skip errors in case there is categorical variable like education field
# storing p values in a data frame called pvalues
for (i in 1:ncol(peoplewholeft)) {
  tryCatch({
    t_test = t.test(peoplewholeft[,i], peoplewhostayed[,i],conf.level =0.05)
    pval = t_test$p.value
    pvalues[i] = pval
  }, error=function(e){})
}
pvalues=as.data.frame(pvalues)
pvalues$columns=names(data1)

# Looping through pvalues see if anything is less  than 0.05 significance level, deem to be significant enough to investigate.

pvalues$significant_lessthan0.05="No"
for (i in 1:36) {
  if (pvalues[i,1] < 0.05){
    pvalues[i,3]="Yes"
  }
  if (pvalues[i,1] == 0){
    pvalues[i,3]="NA"
  }
}


#find out what feature is significantly influencing the attrition
pvalues[1,3]="Yes"
factors=pvalues%>%filter(significant_lessthan0.05 == "Yes")
factors

#Looking at factors that are much different from one another, labling data sets with people_left and people_stayed

people_left=peoplewholeft[,c(factors$columns)]
people_left$dataset = "people_left"
people_stayed=peoplewhostayed[,c(factors$columns)]
people_stayed$dataset = "people_stayed"
data2=rbind(people_left,people_stayed)
data2









#factorize the data and scale it. Adding column response assign it with Yes and No
data_scaled = 
  data1 %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, ~as.integer(factor(.)) - 1) 

preproc=preProcess(data_scaled)
data_scaled=predict(preproc,data_scaled)  


preproc


CompData=read.csv(file = "CaseStudy2CompSet No Attrition.csv")
CompData=CompData[,c(36,1:35)]
Compdata1=predict(preproc,CompData)
CompData=CompData %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, ~as.integer(factor(.)) - 1) 

Compdata1=predict(preproc,CompData)

  
  mutate(response = ifelse(as.numeric(factor(data1[,"Attrition"])) == 1, "Yes", "No"))



  ```{r}
  ###Ensemble with Knn on the top, with Knn.cv,naive Bayes, linear regression on the bottom.
  
  library(caret)
  
  # Split the data into training and testing sets
  
  
  data3=data2
  sample_rows <- sample(dim(data3)[1], dim(data3)[1] * 0.7)
  train_sample <- data3[sample_rows, ]
  test_sample <- data3[-sample_rows, ]
  
  
  # Train the k-NN classifier on the training set
  
  
  classification=knn(train_sample[,1:17],test_sample[,1:17],train_sample$Attrition,k=25,prob=TRUE)
  table(test_sample$Attrition,classification)
  cm=confusionMatrix(table(test_sample$Attrition,classification))
  
  probs <- ifelse(classification == "0", attributes(classification)$prob, 1 - attributes(classification)$prob)
  new_class2_knn <- ifelse(probs > 0.8, "0", "1")
  #confusionMatrix(table(new_class2_knn,test_sample$Attrition))
  #a=rocit(score=as.numeric(as.factor(new_class2_knn)),class=as.numeric(test_sample$Attrition))
  #a$AUC
  #plot(a)      
  
  # Train the Naive Bayes classifier on the training set
  
  
  
  nb3model=naiveBayes(Attrition~.,data=train_sample )
  nb3modelpred=predict(nb3model,test_sample[,1:17],type = "raw")
  nb3modelclass <- ifelse(nb3modelpred[,1] > 0.6, "0", "1")
  #confusionMatrix(table(nb3modelclass,test_sample$Attrition))
  #a=rocit(score=as.numeric(as.factor(nb3modelclass)),class=as.numeric(test_sample$Attrition))
  #a$AUC
  #plot(a)
  # Make predictions on the testing set using each classifier
  
  
  
  #linear regression
  fit1=glm(Attrition ~ Age+BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+
             JobLevel+JobSatisfaction+MonthlyIncome+OverTime+StockOptionLevel+TotalWorkingYears+
             WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsWithCurrManager,data=train_sample[,1:19],
           family=binomial)
  
  
  lmprob=predict(fit1,test_sample,type="response")
  lmclass=ifelse(lmprob>0.3,1,0)
  #confusionMatrix(table(lmclass,test_sample$Attrition))
  #a=rocit(score=as.numeric(as.factor(lmclass)),class=as.numeric(test_sample$Attrition))
  #a$AUC
  #plot(a)
  
  
  # Combine the predictions in a data frame
  
  baselayer=data.frame(knn=new_class2_knn ,NB=nb3modelclass,Attrition=test_sample$Attrition)
  
  
  ensemble=ifelse(lmclass == 0 & new_class2_knn == 0 & nb3modelclass==0, 0,
                  ifelse(lmclass == 1 & new_class2_knn == 1 & nb3modelclass == 1, 1, 0))
  
  confusionMatrix(table(ensemble,test_sample$Attrition))
  a=rocit(score=as.numeric(as.factor(ensemble)),class=as.numeric(test_sample$Attrition))
  a$AUC
  plot(a)
  
  
  
  ensemble = glm(Attrition~., data =baselayer,family=binomial )
  lmprob=predict(ensemble,newdata=test_sample,type="response")
  lmclass=ifelse(lmprob>0.3,1,0)
  
  # Train final model on the data set
  ensemble=knn.cv(baselayer[,1:3],baselayer$Attrition,k=21,prob = TRUE)
  probs_ensemble = ifelse(ensemble == "0",attributes(ensemble)$prob, 1- attributes(ensemble)$prob)
  
  ensemble_final = ifelse(probs_ensemble > 0.9, "0", "1")
  confusionMatrix(table(ensemble_final,test_sample$Attrition))
  a=rocit(score=as.numeric(as.factor(ensemble_final)),class=as.numeric(test_sample$Attrition))
  a$AUC
  plot(a)
  
  
  




