###########################################
#Install Packages
###########################################

#install.packages("rstudioapi") 
#install.packages("ggplot2", type="binary")
#install.packages("isoband", type="binary")
#install.packages("farver", type="binary")
#install.packages('ggplot2')

library("rstudioapi") 
library("ggplot2")
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
mse_calculator = function(degree_range = 1:10, display_plot = FALSE){
  for (degree in degree_range){
    perf_df = data.frame(matrix(ncol = 3, nrow = 0))
    colnames(perf_df) = c("degree","mse_train","mse_test")
    model = lm(y ~ poly(x,degree), data = train_df)
    lm_summary = summary(model)
    mse_train = mean(lm_summary$residuals^2)
    mse_test = mean((test_df$y - predict.lm(model, test_df)) ^ 2)
    perf_df[nrow(perf_df) + 1,] = c(degree,mse_train,mse_test)
  }
  if (display_plot == TRUE) {
  ggplot(perf_df, aes(x=degree)) + 
    geom_line(aes(y = mse_train), color = "darkred") + 
    geom_line(aes(y = mse_test), color="steelblue", linetype="twodash")
  }
  knitr::kable(perf_df, format = "markdown")
  return(c(perf_df))
}

# Question 1.1
mse_calculator()

for (degree in 1:10){
  model = lm(y ~ poly(x,degree), data = train_df)
  lm_summary = summary(model)
  mse_train = mean(lm_summary$residuals^2)
  mse_test = mean((test_df$y - predict.lm(model, test_df)) ^ 2)
  perf_df[nrow(perf_df) + 1,] = c(degree,mse_train,mse_test)
}#test

ggplot(perf_df, aes(x=degree)) + 
  geom_line(aes(y = mse_train), color = "darkred") + 
  geom_line(aes(y = mse_test), color="steelblue", linetype="twodash")

for (degree in 10:1){
  model = lm(y ~ poly(x,degree), data = train_df)
  lm_summary = summary(model)
  mse_train = mean(lm_summary$residuals^2)
  mse_test = mean((test_df$y - predict.lm(model, test_df)) ^ 2)
  perf_df[nrow(perf_df) + 1,] = c(degree,mse_train,mse_test)
}

ggplot(perf_df, aes(x=degree)) + 
  geom_line(aes(y = mse_train), color = "darkred") + 
  geom_line(aes(y = mse_test), color="steelblue", linetype="twodash")

