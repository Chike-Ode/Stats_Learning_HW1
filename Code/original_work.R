###########################################
#Install Packages
###########################################

#install.packages("rstudioapi") 
#install.packages("ggplot2", type="binary")
#install.packages("isoband", type="binary")
#install.packages("farver", type="binary")
#install.packages('ggplot2')
#install.packages('ROCR', type="binary")
#install.packages('proxy', type="binary")
#install.packages('e1071', type="binary")
#install.packages('Rcpp', type="binary")
#install.packages('rlang', type="binary")
#install.packages('caret', type="binary")
#install.packages("ISLR")
#install.packages("rpart.plot")
library("rstudioapi") 
library("ggplot2")
library(caret)
library(MASS)
library(ROCR)
library(ISLR)
library(rpart)
library(rpart.plot)
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

#Question 1.5
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
  



###########################################
#EXERCISE 2
###########################################

compas_df = read.csv("Data/Compas.csv", sep=";",header=T)
compas_df$decil_score
compas_df$two_year_recid
compas_df$recidive_pred = factor(ifelse(compas_df$decile_score>=6, 1, 0)) 

conf_matrix = confusionMatrix(data=factor(compas_df$recidive_pred), reference = factor(compas_df$two_year_recid))
fp = conf_matrix$table[2,1]
tp = conf_matrix$table[2,2]
tn = conf_matrix$table[1,1]
fn = conf_matrix$table[1,2]

#Question 2.2
# misclassification
mis_rate = (fp + fn)/(fp + fn + tn +tp)
# false positive rate
fpr = fp / (fp + tn)
# false discovery rates
fdr = fp / (fp + tp)

#Question 2.3
compas_df$decile_score_scaled = compas_df$decile_score / 10
compas_df$decile_score_scaled

pred = prediction(compas_df$decile_score_scaled,compas_df$two_year_recid)
perf <- performance(pred, "tpr", "fpr")
cost_perf <- performance(pred, "cost")
plot(perf,print.cutoffs.at=seq(0.1,by=0.1))
plot(cost_perf)

pred@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]

#Question 2.4
afr_amer_df = subset(compas_df,race == 'African-American')
conf_matrix_afr = confusionMatrix(data=factor(afr_amer_df$recidive_pred), reference = factor(afr_amer_df$two_year_recid))
fp_afr = conf_matrix_afr$table[2,1]
tp_afr = conf_matrix_afr$table[2,2]
tn_afr = conf_matrix_afr$table[1,1]
fn_afr = conf_matrix_afr$table[1,2]

# misclassification
mis_rate_afr = (fp_afr + fn_afr)/(fp_afr + fn_afr + tn_afr + tp_afr)
# false positive rate
fpr_afr = fp_afr / (fp_afr + tn_afr)
# false discovery rates
fdr_afr = fp_afr / (fp_afr + tp_afr)

cauc_df = subset(compas_df,race == 'Caucasian')
conf_matrix_cauc = confusionMatrix(data=factor(cauc_df$recidive_pred), reference = factor(cauc_df$two_year_recid))
fp_cauc = conf_matrix_cauc$table[2,1]
tp_cauc = conf_matrix_cauc$table[2,2]
tn_cauc = conf_matrix_cauc$table[1,1]
fn_cauc = conf_matrix_cauc$table[1,2]

# misclassification
mis_rate_cauc = (fp_cauc + fn_cauc)/(fp_cauc + fn_cauc + tn_cauc + tp_cauc)
# false positive rate
fpr_cauc = fp_cauc / (fp_cauc + tn_cauc)
# false discovery rates
fdr_cauc = fp_cauc / (fp_cauc + tp_cauc)

# Question 2.5
pred_afr_amer = prediction(afr_amer_df$decile_score_scaled,afr_amer_df$two_year_recid)
cost_perf_afr_amer <- performance(pred_afr_amer, "cost")
plot(cost_perf_afr_amer)

pred_afr_amer@cutoffs[[1]][which.min(cost_perf_afr_amer@y.values[[1]])]

pred_cauc = prediction(cauc_df$decile_score_scaled,cauc_df$two_year_recid)
cost_perf_cauc <- performance(pred_cauc, "cost")
plot(cost_perf_cauc)

pred_afr_cauc@cutoffs[[1]][which.min(cost_perf_cauc@y.values[[1]])]

###########################################
#EXERCISE 3
###########################################
dbm_df = read.csv("Data/dbm_final.csv",header=T)
dbm_df$region=as.factor(dbm_df$region)
dbm_df$revenu=as.factor(dbm_df$revenu)
model_dbm=glm(yachat~sexe + age + revenu + region + conjoint + anneeclient + semainedernier+montantdernier + montantunan + achatunan,family="binomial",data=dbm_df[dbm_df$train==1,] )
summary(model_dbm)
y_true = dbm_df$yachat
y_pred = predict(model_dbm, dbm_df, type="response")

min(y_pred)  
max(y_pred)
plot_roc = function(y_true,y_pred,increment = 0.02){
  perf_df = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(perf_df) = c("threshold","true_positive_rate","false_positive_rate")
  for(i in seq(from=0, to=1, by=increment)){
    y_temp_pred = ifelse(y_pred>=i,1,0)
    conf_matrix = confusionMatrix(data=factor(y_temp_pred), reference = factor(y_true))
    fp = conf_matrix$table[2,1]
    tp = conf_matrix$table[2,2]
    tn = conf_matrix$table[1,1]
    fn = conf_matrix$table[1,2]
    #sensitivity_temp = tp/(tp+fn)
    #specificity_temp = tn/(tn+fp)
    fpr = fp / (fp + tn)
    tpr = tp/(tp+fn)
    perf_df[nrow(perf_df) + 1,] = c(i,tpr,fpr)
  }
  #p = (ggplot(data = perf_df,aes(x=fpr,y=tpr,group = 1)) +
  #  geom_line())
  p = ggplot(perf_df,aes(false_positive_rate,true_positive_rate))+geom_line(size = 2, alpha = 0.7)
  print(p)
  return(perf_df)
}
df2 = plot_roc(y_true,y_pred)

###########################################
#EXERCISE 4
###########################################

rodeo_df = read.csv("Data/rodeo.csv",header=T)
model_rodeo=glm(achat~age,family="binomial",data=rodeo_df )
# Question 4.1 a
summary(model_rodeo)
# Question 4.1 b 
predict(model_rodeo, data.frame(age = c(35)), type="response")
predict(model_rodeo, data.frame(age = c(50)), type="response")
# Question 4.1 c
age_pred_df = data.frame(matrix(ncol = 2, nrow = 0))
colnames(age_pred_df) = c("age","prediction")

for(year in seq(from=0, to=100, by=2)){
  prediction_age = predict(model_rodeo, data.frame(age = c(year)), type="response")
  age_pred_df[nrow(age_pred_df) + 1,] = c(year,prediction_age)
}
p = ggplot(age_pred_df,aes(age,prediction))+geom_line(size = 2, alpha = 0.7)
print(p)

# Question 4.1 d
set.seed(101)
sample <- sample.int(n = nrow(rodeo_df), size = floor(.75*nrow(rodeo_df)), replace = F)
train_rod_df <- rodeo_df[sample, ]
test_rod_df  <- rodeo_df[-sample, ]

model_rodeo_cv=glm(achat~age,family="binomial",data=train_rod_df )
y_pred_rod_cv = ifelse(predict(model_rodeo_cv, test_rod_df, type="response")>=0.5,1,0)

length(y_pred_rod_cv)
length(test_rod_df$achat)

conf_matrix_rod_cv = confusionMatrix(data=factor(y_pred_rod_cv), reference = factor(test_rod_df$achat))
fp_rod_cv = conf_matrix_rod_cv$table[2,1]
tp_rod_cv = conf_matrix_rod_cv$table[2,2]
tn_rod_cv = conf_matrix_rod_cv$table[1,1]
fn_rod_cv = conf_matrix_rod_cv$table[1,2]
fpr_rod_cv = fp_rod_cv / (fp_rod_cv + tn_rod_cv)

################################################
# Question 4.2
set.seed(200)  
n=nrow(rodeo_df)
id.train=sample(1:n,300,replace=FALSE)
id.valid=sample(setdiff(1:n,id.train),100,replace=FALSE)
id.test=setdiff(setdiff(1:n,id.train),id.valid)

# Division de l'échantillon
rodeo_df.train=rodeo_df[id.train,]
rodeo_df.valid=rodeo_df[id.valid,]
rodeo_df.test=rodeo_df[id.test,]

################################################
# Question 4.3 a
model_rodeo_all_var=glm(achat~.,family="binomial",data=rodeo_df.train )
summary(model_rodeo_all_var)

# Question 4.3 b
y_pred_all_var_prob = predict(model_rodeo_all_var, rodeo_df.valid, type="response")
y_pred_all_var = ifelse(y_pred_all_var_prob>=0.5,1,0)
conf_matrix_all_var = confusionMatrix(data=factor(y_pred_all_var), reference = factor(rodeo_df.valid$achat))
fp_all_var = conf_matrix_all_var$table[2,1]
tp_all_var = conf_matrix_all_var$table[2,2]
tn_all_var = conf_matrix_all_var$table[1,1]
fn_all_var = conf_matrix_all_var$table[1,2]
sensitivity_all_var = tp_all_var/(tp_all_var+fn_all_var)
specificity_all_var = tn_all_var/(tn_all_var+fp_all_var)

# Question 4.3 c
pred_all_var = prediction(y_pred_all_var_prob,rodeo_df.valid$achat)
perf_sn_sp = performance(pred_all_var, "tpr","fpr")
plot(perf_sn_sp,colorize = T, lwd = 2)
abline(a = 0, b = 1) 
auc = performance(pred_all_var, measure = "auc")
print(auc@y.values)

################################################
# Question 4.4
View(rodeo_df.train)
model_rodeo_interactions=glm(achat~(.)*age,family="binomial",data=rodeo_df.train )
summary(model_rodeo_interactions)
y_pred_interactions = ifelse(predict(model_rodeo_interactions, rodeo_df.valid, type="response")>=0.5,1,0)
y_pred_interactions_prob = predict(model_rodeo_interactions, rodeo_df.valid, type="response")

AIC(model_rodeo_interactions)
BIC(model_rodeo_interactions)

conf_matrix_interactions = confusionMatrix(data=factor(y_pred_interactions), reference = factor(rodeo_df.valid$achat))
fp_interactions = conf_matrix_interactions$table[2,1]
tp_interactions = conf_matrix_interactions$table[2,2]
tn_interactions = conf_matrix_interactions$table[1,1]
fn_interactions = conf_matrix_interactions$table[1,2]

pred_interactions = prediction(y_pred_interactions_prob,rodeo_df.valid$achat)

auc_interactions = performance(pred_interactions, measure = "auc")
print(auc_interactions@y.values)

# misclassification
mis_rate_interactions = (fp_interactions + fn_interactions)/(fp_interactions + fn_interactions + tn_interactions +tp_interactions)


AIC(model_rodeo_all_var)
BIC(model_rodeo_all_var)

conf_matrix_all_var = confusionMatrix(data=factor(y_pred_all_var), reference = factor(rodeo_df.valid$achat))
fp_all_var = conf_matrix_all_var$table[2,1]
tp_all_var = conf_matrix_all_var$table[2,2]
tn_all_var = conf_matrix_all_var$table[1,1]
fn_all_var = conf_matrix_all_var$table[1,2]

# misclassification
mis_rate_all_var = (fp_all_var + fn_all_var)/(fp_all_var + fn_all_var + tn_all_var +tp_all_var)

# Question 4.5
sensitivity_interactions = tp_interactions/(tp_interactions+fn_interactions)
specificity_interactions = tn_interactions/(tn_interactions+fp_interactions)

# Question 4.6
step_wise_model_interactions <- stepAIC(model_rodeo_interactions, direction = "both", 
                      trace = FALSE)
y_pred_step_wise_interactions_prob = predict(model_rodeo_interactions, rodeo_df.valid, type="response")

summary(step_wise_model_interactions)
pred_step_wise_interactions = prediction(y_pred_step_wise_interactions_prob,rodeo_df.valid$achat)

auc_step_wise_interactions = performance(pred_step_wise_interactions, measure = "auc")
print(auc_step_wise_interactions@y.values)

perf_step_wise_interactions = performance(pred_step_wise_interactions, "tpr","fpr")
plot(perf_step_wise_interactions,colorize = T, lwd = 2)
abline(a = 0, b = 1) 

###########################################
#EXERCISE 5
###########################################
set.seed(12345)
x=rnorm(1000)
variations = c(0.1,0.5,1,1.5,2,2.5,3)
num_obs = c(10,30,50,100,200,500,1000)

beta0=1
beta1=2
sigma=1
y=beta0+beta1*x+ rlnorm(200)

x[1:100]

model=lm(y~x)
summary(model)
model$coefficients[2]

y
x

sim_df = data.frame(matrix(ncol = 3, nrow = 0))
colnames(sim_df) = c("error_variation","sample_size","b1_est")
for (var in variations){
  for (n in num_obs){
    x_sub = x[1:n]
    error_term = rnorm(n = n, sd = var)
    y = beta0 + beta1*x_sub + error_term
    model=lm(y~x_sub)
    #model_sum = summary(model)
    b1_est = model$coefficients[2]
    sim_df[nrow(sim_df) + 1,] = c(var,n,b1_est)
  }
}
standard_error = sd(sim_df$b1_est)/sqrt(length(sim_df$b1_est))

sim_df_500 = data.frame(matrix(ncol = 3, nrow = 0))
colnames(sim_df_500) = c("error_variation","sample_size","b1_est")
for (i in 1:500){
  for (var in variations){
    for (n in num_obs){
      x_sub = x[1:n]
      error_term = rnorm(n = n, sd = var)
      y = beta0 + beta1*x_sub + error_term
      model=lm(y~x_sub)
      #model_sum = summary(model)
      b1_est = model$coefficients[2]
      sim_df_500[nrow(sim_df_500) + 1,] = c(var,n,b1_est)
    }
  }
}
standard_error_500 = sd(sim_df_500$b1_est)/sqrt(length(sim_df_500$b1_est))

###########################################
#EXERCISE 6
###########################################

n=nrow(OJ)
set.seed(1234)
id.train=sample(1:n,size=800)
id.test=setdiff(1:n,id.train)
OJ.train=OJ[id.train,-3]
OJ.test=OJ[id.test,-3]

View(OJ.train)
# Question 6.2
mytree = rpart(Purchase~., data=OJ.train, method = "class")
plot(mytree)
text(mytree)
# Question 6.3
print(mytree)
tree_summary = summary(mytree)
# Question 6.4 TODO 
tree_summary$frame
tree_summary$terms
tree_summary
prp(mytree)
mytree$where

# Question 6.5
unique_dsc = length(unique(OJ.train$PctDiscMM)) 
unique_store = length(unique(OJ.train$STORE))
answer = unique_dsc + unique_store


# Question 6.6
y_pred_tree = predict(mytree,OJ.test, type="class")
conf_matrix_tree = confusionMatrix(data=factor(y_pred_tree), reference = factor(OJ.test$Purchase))
fp_tree = conf_matrix_tree$table[2,1]
tp_tree = conf_matrix_tree$table[2,2]
tn_tree = conf_matrix_tree$table[1,1]
fn_tree = conf_matrix_tree$table[1,2]

# misclassification
mis_rate_tree = (fp_tree + fn_tree)/(fp_tree + fn_tree + tn_tree +tp_tree)

# Question 6.7
mytree$cptable
cp_optimal=mytree$cptable[which.min(mytree$cptable[,4]),1]
mytree_optimal = prune(mytree,cp=cp_optimal)

y_pred_tree_optimal = predict(mytree_optimal,OJ.test, type="class")
conf_matrix_tree_optimal = confusionMatrix(data=factor(y_pred_tree_optimal), reference = factor(OJ.test$Purchase))
fp_tree_optimal = conf_matrix_tree_optimal$table[2,1]
tp_tree_optimal = conf_matrix_tree_optimal$table[2,2]
tn_tree_optimal = conf_matrix_tree_optimal$table[1,1]
fn_tree_optimal = conf_matrix_tree_optimal$table[1,2]

# misclassification
mis_rate_tree_optimal = (fp_tree_optimal + fn_tree_optimal)/(fp_tree_optimal + fn_tree_optimal + tn_tree_optimal +tp_tree_optimal)

# false positive rate
fpr_tree_optimal = fp_tree_optimal / (fp_tree_optimal + tn_tree_optimal)


n=nrow(OJ)
set.seed(2)
id.train=sample(1:n,size=800)
id.test=setdiff(1:n,id.train)
OJ.train=OJ[id.train,-3]
OJ.test=OJ[id.test,-3]

# Question 6.6
mytree = rpart(Purchase~., data=OJ.train, method = "class")
y_pred_tree = predict(mytree,OJ.test, type="class")
conf_matrix_tree = confusionMatrix(data=factor(y_pred_tree), reference = factor(OJ.test$Purchase))
fp_tree = conf_matrix_tree$table[2,1]
tp_tree = conf_matrix_tree$table[2,2]
tn_tree = conf_matrix_tree$table[1,1]
fn_tree = conf_matrix_tree$table[1,2]

# misclassification
mis_rate_tree = (fp_tree + fn_tree)/(fp_tree + fn_tree + tn_tree +tp_tree)

# Question 6.7
mytree$cptable
cp_optimal=mytree$cptable[which.min(mytree$cptable[,4]),1]
mytree_optimal = prune(mytree,cp=cp_optimal)

y_pred_tree_optimal = predict(mytree_optimal,OJ.test, type="class")
conf_matrix_tree_optimal = confusionMatrix(data=factor(y_pred_tree_optimal), reference = factor(OJ.test$Purchase))
fp_tree_optimal = conf_matrix_tree_optimal$table[2,1]
tp_tree_optimal = conf_matrix_tree_optimal$table[2,2]
tn_tree_optimal = conf_matrix_tree_optimal$table[1,1]
fn_tree_optimal = conf_matrix_tree_optimal$table[1,2]

# misclassification
mis_rate_tree_optimal = (fp_tree_optimal + fn_tree_optimal)/(fp_tree_optimal + fn_tree_optimal + tn_tree_optimal +tp_tree_optimal)

# false positive rate
fpr_tree_optimal = fp_tree_optimal / (fp_tree_optimal + tn_tree_optimal)
