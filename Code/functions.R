###########################################
#Install Packages
###########################################

#install.packages("rstudioapi") 
#install.packages("ggplot2", type="binary")
#install.packages("isoband", type="binary")
#install.packages("farver", type="binary")
#install.packages('ggplot2')
install.packages('ROCR', type="binary")

library("rstudioapi") 
library("ggplot2")
library(ROCR)
theme_set(theme_minimal())
#install.packages("rmarkdown", dependencies=TRUE) 

###########################################
#Specify Root Directory 
###########################################
cur_dir = dirname(getSourceEditorContext()$path) 
root_dir = dirname(cur_dir)

setwd(root_dir)
packrat::init(root_dir)

###########################################
#EXERCISE 1
###########################################
train_df = read.table("Data/datasimul1_train.txt",header=T)
test_df = read.table("Data/datasimul1_test.txt",header=T)

# Utility Function
mse_calculator = function(train = train_df, test = test_df, degree_range = 1:10, display_plot = FALSE, question_num = "",include_test = FALSE){
  if (include_test == FALSE){
    perf_df = data.frame(matrix(ncol = 2, nrow = 0))
    colnames(perf_df) = c("degree","mse_train")
  }else{
    perf_df = data.frame(matrix(ncol = 3, nrow = 0))
    colnames(perf_df) = c("degree","mse_train","mse_test")
  }
  for (degree in degree_range){
    model = lm(y ~ poly(x,degree), data = train)
    lm_summary = summary(model)
    mse_train = mean(lm_summary$residuals^2)
    if (include_test == TRUE){
      mse_test = mean((test$y - predict.lm(model, test)) ^ 2)
      perf_df[nrow(perf_df) + 1,] = c(degree,mse_train,mse_test)
    }else{
      perf_df[nrow(perf_df) + 1,] = c(degree,mse_train)
    }
    
  }
  if (display_plot == TRUE) {
    p = ggplot(perf_df, aes(x=degree)) + 
      geom_line(aes(y = mse_train), color = "darkred") + 
      geom_line(aes(y = mse_test), color="steelblue", linetype="twodash")
    print(p)
  }
  
  #knitr::kable(perf_df, format = "markdown")
  description = paste("The Result for Question: ",question_num)
  print(description)
  print(perf_df)
  return(perf_df)
}

# Question 1.1
perf_df_1_1 = mse_calculator(train = train_df, test = test_df, degree_range = 1:10, display_plot = FALSE, question_num = "1.1",include_test = FALSE)
# Question 1.2
perf_df_1_2 = mse_calculator(train = train_df, test = test_df, degree_range = 1:10, display_plot = FALSE, question_num = "1.2",include_test = TRUE)

# Question 1.3
perf_df_1_3 = mse_calculator(train = train_df, test = test_df, degree_range = 1:10, display_plot = TRUE, question_num = "1.3",include_test = TRUE)

# Question 1.4
perf_df_1_4 = mse_calculator(train = train_df, test = test_df, degree_range = 10:1, display_plot = TRUE, question_num = "1.4",include_test = TRUE)

# Lecture et preparation des données (voir document sur l'analyse de ces donnees)
mydata=read.csv("Data/kc_house_data.csv")
mydata$log_price=log(mydata$price)
mydata$bedrooms2=mydata$bedroom^2
mydata$log_sqft_living=log(mydata$sqft_living)
mydata$log_sqft_lot=log(mydata$sqft_lot)
mydata$floors_cat=as.factor(mydata$floors)
mydata$condition_binaire=ifelse(mydata$condition>=3,1,0)
mydata$log_sqft_above=log(mydata$sqft_above)
mydata$age=2015-mydata$yr_built
mydata$age2=mydata$age^2
mydata$lat2=mydata$lat^2
mydata$long2=mydata$long^2
mydata$lat3=mydata$lat^3
mydata$long3=mydata$long^3

# Division de l'échantillon (train = 10,000 observations)
set.seed(200)  
n=nrow(mydata)
id.train=sample(1:n,10000,replace=FALSE)
id.valid=sample(setdiff(1:n,id.train),11613,replace=FALSE)

# Division de l'échantillon
mydata.train=mydata[id.train,]
mydata.valid=mydata[id.valid,]
model1=lm(log_price~bedrooms+bathrooms+log_sqft_living+log_sqft_lot+floors_cat
          +waterfront+condition_binaire+grade+log_sqft_above+sqft_basement
          +age+lat+long,data=mydata.train)
model1_summary = summary(model1)
mse_train_model1 = mean(model1_summary$residuals^2)
mse_test1 = mean((test_df$y - predict.lm(model1, test_df)) ^ 2)
  





compas_df = read.csv("Data/Compas.csv", sep=";",header=T)
compas_df$decil_score
compas_df$two_year_recid
compas_df$recidive_pred = ifelse(compas_df$decile_score>=6, 1, 0) 
mean(compas_df$recidive_pred != compas_df$two_year_recid)
pred <- prediction( compas_df$recidive_pred, compas_df$two_year_recid)
tpr <- pred@tp[[1]]/max(pred@tp[[1]])
fpr <- pred@fp[[1]]/max(pred@fp[[1]])
