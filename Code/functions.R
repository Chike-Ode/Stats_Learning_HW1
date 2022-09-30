install.packages("rstudioapi") 
library("rstudioapi") 
library("ggplot2")
theme_set(theme_minimal())
install.packages("rmarkdown", dependencies=TRUE) 


cur_dir = dirname(getSourceEditorContext()$path) 
root_dir = dirname(cur_dir)

setwd(root_dir)
packrat::init(root_dir)

train_df = read.table("Data/datasimul1_train.txt",header=T)
test_df = read.table("Data/datasimul1_test.txt",header=T)

perf_df = data.frame(matrix(ncol = 3, nrow = 0))
colnames(perf_df) = c("degree","mse_train","mse_test")

mse_calculator = function(degree_range = 1:10){
  for (degree in degree_range){
    model = lm(y ~ poly(x,degree), data = train_df)
    lm_summary = summary(model)
    mse_train = mean(lm_summary$residuals^2)
    mse_test = mean((test_df$y - predict.lm(model, test_df)) ^ 2)
    perf_df[nrow(perf_df) + 1,] = c(degree,mse_train,mse_test)
  }
  ggplot(perf_df, aes(x=degree)) + 
    geom_line(aes(y = mse_train), color = "darkred") + 
    geom_line(aes(y = mse_test), color="steelblue", linetype="twodash")
  
  return(c(perf_df))
}

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

