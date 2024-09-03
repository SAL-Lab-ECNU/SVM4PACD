# Clear the workspace
rm(list=ls()) 
# Set the seed for reproducibility
set.seed(29)

# Load necessary packages
library(CDM)
library(MASS)

#setting parameters
attri_num<-3    #number of attributes
attri_level <- c(3, 3, 2)#number of attribute levels
N <- 200 #number of examinees
size_num<-100   #training sample size: [20,30,50,100,150]
cycles <- 30#number of iterations: 30
Item<-40        #number of items


datb <- as.matrix(read.csv("C:/Users/littlerain/Desktop/SVM for PACD/data/databind.csv"))
Qm <- as.matrix(read.csv("C:/Users/littlerain/Desktop/SVM for PACD/data/Qm.csv"))



fdinaPCCR <- matrix(NA, nrow = cycles, ncol = 1)
fdinaACCR <- matrix(NA, nrow = cycles, ncol = ncol(Qm))


for (b in 1:cycles){
  num<-sample(N,size_num,replace = FALSE)
  train_data<-datb[num,]
  test_data<-datb[-num,]
  
  train_dat <- train_data[,2:41]
  train_alpha <- train_data[,42:44]
  
  test_dat <- test_data[,2:41]
  test_alpha <- test_data[,(42:44)]
  
  test_estA <- 0 * test_alpha
  
  #DINA
  model_train <- CDM::gdina(train_dat, Qm, rule = "DINA", reduced.skillspace = FALSE, method = "ML")
  model_test <- CDM::gdina(test_dat, Qm, rule = "DINA", delta.fixed = model_train$delta,method = "ML")
  estdinaA<-IRT.factor.scores(model_test, "MLE")

  
  d_Wnk <- ifelse(test_alpha == estdinaA, 1, 0)
  
  dACCR <- apply(d_Wnk, 2, mean)
  dPCCR <- mean(apply(d_Wnk, 1, prod))
  
  fdinaACCR[b,] <- dACCR
  fdinaPCCR[b] <- dPCCR
}

mfdinaACCR <- apply(fdinaACCR, 2, mean)
mfdinaPCCR <- mean(fdinaPCCR)

# Save results to CSV files
file_fdinaACCR <- paste0("C:/Users/littlerain/Desktop/SVM for PACD/result/empirical-fdinaACCR-", size_num, ".csv")
file_fdinaPCCR <- paste0("C:/Users/littlerain/Desktop/SVM for PACD/result/empirical-fdinaPCCR-", size_num, ".csv")

write.csv(fdinaACCR, file = file_fdinaACCR)
write.csv(fdinaPCCR, file = file_fdinaPCCR)



