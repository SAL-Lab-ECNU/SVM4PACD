# Clear the workspace
rm(list=ls()) 
# Set the seed for reproducibility
set.seed(29)

# Load necessary packages
library(CDM)
library(MASS)


#setting parameters
attri_num<-3    #number of attributes: [3,4,5]
attri_level<-3  #number of attribute levels: [3,4,5]
Item<-30        #number of items: [30,60]
N<-5000         #number of examinees
sg<-0.05        #levels of slipping and guessing: [0.05,0.1,0.2]
size_num<-200   #training sample size: [20,30,50,100,150,200]
min_count<-2    #minimum number of samples per class
cycles<-1       #number of iterations: 30
ah<-2           #0:linear, 1:divergent, 2:convergent
ah_name_list <- c('linear', 'divergent', 'convergent')  # Attribute hierarchy name list
ah_name <- ah_name_list[ah + 1] #attribute hierarchy name

# Import the R matrix
file_path_Rmatrix<-paste0("C:/Users/littlerain/Desktop/SVM for PACD/Rmatrix/", ah_name, "_", attri_num, "_", attri_level, ".csv")
Rmatrix <- as.matrix(read.csv(file_path_Rmatrix))

# Calculate the highest level for each attribute
ha <- apply(Rmatrix, 1, max)
Q2 <- matrix(data=NA, nrow=0, ncol=ncol(Rmatrix))

# Convert multi-valued R matrix into a 0-1 matrix
for(i in 1:nrow(Rmatrix)){
  for(j in 1:ha[i]){
    Q1 = t(Rmatrix[i,] - j + 1)
    Q1[Q1 > 1] <- 1
    Q1[Q1 < 0] <- 0
    Q2 = rbind(Q2, Q1)
  }
}

# augment algorithm
Q2 <- t(Q2)
k <- nrow(Q2)

for(i in 1:k){
  for(j in 1:k-i){
    Z <- Q2[i,] + Q2[i+j,]
    Z[Z == 2] <- 1
    Q2 <- rbind(Q2, Z)
  }
  Q2 <- unique(Q2)
  k <- nrow(Q2)
}

Q2 <- t(Q2)

Qf <- matrix(data=NA, nrow=0, ncol=ncol(Q2))

# Convert 0-1 R matrix back to multi-valued R matrix
for(i in 1:nrow(Rmatrix)){
  Qk <- Q2[1:ha[i],]
  
  if(ha[i] == 1){
    Qf = rbind(Qf, t(Qk))
  }else{
    Qf = rbind(Qf, apply(Qk, 2, sum))
  }
  
  Q2 <- Q2[-c(1:ha[i]),]
}

Qf <- t(Qf)
Qff <- matrix(NA, nrow = 1, ncol = ncol(Qf))

#logical constraint of the ideal mastery pattern
for (i in 1:nrow(Qf)) {
  valid <- FALSE
  
  if (ah == 0) {
    if (attri_num == 3) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,2] >= Qf[i,3]
    } else if (attri_num == 4) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] &
        Qf[i,2] >= Qf[i,3] & Qf[i,2] >= Qf[i,4] & Qf[i,3] >= Qf[i,4]
    } else if (attri_num == 5) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] & Qf[i,1] >= Qf[i,5] &
        Qf[i,2] >= Qf[i,3] & Qf[i,2] >= Qf[i,4] & Qf[i,2] >= Qf[i,5] &
        Qf[i,3] >= Qf[i,4] & Qf[i,3] >= Qf[i,5] & Qf[i,4] >= Qf[i,5]
    }
  } else if (ah == 1) {
    if (attri_num == 3) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3]
    } else if (attri_num == 4) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] & Qf[i,3] >= Qf[i,4]
    } else if (attri_num == 5) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] & Qf[i,1] >= Qf[i,5] &
        Qf[i,3] >= Qf[i,4] & Qf[i,3] >= Qf[i,5]
    }
  } else if (ah == 2) {
    if (attri_num == 3) {
      valid <- Qf[i,1] >= Qf[i,3] & Qf[i,2] >= Qf[i,3]
    } else if (attri_num == 4) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] &
        Qf[i,2] >= Qf[i,4] & Qf[i,3] >= Qf[i,4]
    } else if (attri_num == 5) {
      valid <- Qf[i,1] >= Qf[i,2] & Qf[i,1] >= Qf[i,3] & Qf[i,1] >= Qf[i,4] & Qf[i,1] >= Qf[i,5] &
        Qf[i,2] >= Qf[i,3] & Qf[i,2] >= Qf[i,4] & Qf[i,2] >= Qf[i,5] &
        Qf[i,3] >= Qf[i,5] & Qf[i,4] >= Qf[i,5]
    }
  }
  
  if (valid) {
    Qff <- rbind(Qff, Qf[i, ])
  }
}

Qff <- Qff[-1,]

# Add a row for the completely unmastered pattern
Q0 <- matrix(0, nrow=1, ncol=nrow(Rmatrix))
Qfinal <- rbind(Qff, Q0)

# Extract Q matrix
Qnum <- sample(nrow(Qff), Item, replace = TRUE)
Qm <- Qff[Qnum,]


# Accuracy metrics
fdinaPCCR <- matrix(NA, nrow = cycles, ncol = 1)
fdinaACCR <- matrix(NA, nrow = cycles, ncol = ncol(Qm))
  
for(b in 1:cycles){
  # Simulate examinee response data
  Anum <- sample(nrow(Qfinal), N, replace = TRUE)
  alpha <- Qfinal[Anum,]
  
  guess <- matrix(sg, Item, byrow=TRUE)
  slip <- matrix(sg, Item, byrow=TRUE)
  dat <- latresp <- matrix(0, N, Item, byrow=TRUE)
  
  # Generate response probabilities based on true parameters, then compare with random numbers
  # If greater than the random number, assign 1, otherwise assign 0
  for (ii in 1:Item){
    latresp[,ii] <- 1*( rowMeans( alpha >= matrix(Qm[ii,], nrow=N, ncol=ncol(Qm), byrow=TRUE) ) == 1 )
    prob <- ifelse(latresp[,ii] == 1, 1-slip[ii], guess[ii])
    dat[,ii] <- 1 * (runif(N) < prob)
  }
  
  datb <- cbind(dat, alpha) # Combine response data and alpha
  
  # Sample training and test sets
  # Resample and check class counts
  valid_sample <- FALSE
  while (!valid_sample) {
    num<-sample(N,size_num,replace = FALSE)
    train_data <-datb[num,]
    test_data<-datb[-num,]
    
    train_dat <- train_data[, 1:Item]
    train_alpha <- train_data[, (Item+1):ncol(train_data)]
    test_dat <- test_data[, 1:Item]
    test_alpha <- test_data[, (Item+1):ncol(test_data)]
    
    # Check class counts
    class_counts <- apply(train_alpha, 2, function(x) table(factor(x, levels = 0:(attri_num-1))))
    valid_sample <- all(sapply(class_counts, function(counts) all(counts >= min_count)))
  }
  
  # Estimate DINA model item parameters (Train-Test)
  model_train <- CDM::gdina(train_dat, Qm, rule="DINA", reduced.skillspace=FALSE, method = "ML")
  model_test <- CDM::gdina(test_dat, Qm, rule="DINA", delta.fixed = model_train$delta, method = "ML")
  
  #if pgdina method is Only Test
  # model_test <- CDM::gdina(test_dat, Qm, rule="DINA", reduced.skillspace=FALSE, method = "ML")
  
  estdinaA <- IRT.factor.scores(model_test, "MLE")
  d_Wnk <- ifelse(test_alpha == estdinaA, 1, 0)
  
  dACCR <- apply(d_Wnk, 2, mean)
  dPCCR <- mean(apply(d_Wnk, 1, prod))
  
  fdinaACCR[b,] <- dACCR
  fdinaPCCR[b] <- dPCCR
}
  
mfdinaACCR <- apply(fdinaACCR, 2, mean)
mfdinaPCCR <- mean(fdinaPCCR)
  
# Save results to CSV files
file_fdinaACCR <- paste0("C:/Users/littlerain/Desktop/SVM for PACD/result/fdinaACCR-", ah_name, "-", attri_num, "-", attri_level, "-", size_num, "-", sg, "-", Item, ".csv")
file_fdinaPCCR <- paste0("C:/Users/littlerain/Desktop/SVM for PACD/result/fdinaPCCR-", ah_name, "-", attri_num, "-", attri_level, "-", size_num, "-", sg, "-", Item, ".csv")

write.csv(fdinaACCR, file = file_fdinaACCR)
write.csv(fdinaPCCR, file = file_fdinaPCCR)



