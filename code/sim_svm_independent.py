from sklearn.model_selection import GridSearchCV,RepeatedStratifiedKFold
from sklearn.svm import SVC
import pandas as pd
import numpy as np
from scipy.stats import multivariate_normal


# Function to generate independent data
def generate_independence_data(Rmatrix, Qf,elseQp,I,N,sg,attri_level):
    if I > Qf.shape[0]:
        Qother1 = np.random.choice(elseQp.shape[0], I - Qf.shape[0], replace=True)
        Qother = elseQp.iloc[Qother1, :]
        Qm = pd.concat([Qf, Qother], ignore_index=True)
    else:
        Qother1 = np.random.choice(elseQp.shape[0], I - Rmatrix.shape[1], replace=True)
        Qother = elseQp.iloc[Qother1, :]
        Qm = pd.concat([Qf.iloc[:Rmatrix.shape[1], :], Qother], ignore_index=True)    
    # Generate a diagonal matrix with ones on the diagonal
    sigma = np.diag(np.ones(Qm.shape[1]))
    
    # Generate a multivariate normal distribution
    alpha1 = multivariate_normal.rvs(mean=np.zeros(Qm.shape[1]), cov=sigma, size=N)
    alpha = np.zeros_like(alpha1)

    # Assign attribute levels based on thresholds
    if attri_level ==3:
        ql = [-0.43, 0.43]
        for aa in range(Qm.shape[1]):
            alpha[alpha1[:, aa] > ql[0], aa] = 1
            alpha[alpha1[:, aa] > ql[1], aa] = 2
    elif attri_level == 4:
        ql = [-0.68,0,0.68]
        for aa in range(Qm.shape[1]):
            alpha[alpha1[:, aa] > ql[0], aa] = 1
            alpha[alpha1[:, aa] > ql[1], aa] = 2
            alpha[alpha1[:, aa] > ql[2], aa] = 3
    elif attri_level == 5:
        ql = [-0.85,-0.26,0.26,0.85]
        for aa in range(Qm.shape[1]):
            alpha[alpha1[:, aa] > ql[0], aa] = 1
            alpha[alpha1[:, aa] > ql[1], aa] = 2
            alpha[alpha1[:, aa] > ql[2], aa] = 3  
            alpha[alpha1[:, aa] > ql[3], aa] = 4   
    alpha_df = pd.DataFrame(alpha, columns=Qm.columns)
    guess = np.full((I,),sg)
    slip = np.full((I,),sg)
    dat = pd.DataFrame(np.zeros((N,I)))
    latresp = pd.DataFrame(np.zeros((N,I)))

    # Generate latent responses and observed data
    for item in range(I):
        latresp.iloc[:,item] = (alpha_df.ge(Qm.iloc[item, :], axis=1).mean(axis=1) == 1).astype(int)
        prob = np.where(latresp.iloc[:, item] == 1, 1 - slip[item], guess[item])
        dat.iloc[:, item] = (np.random.rand(N) < prob).astype(int)
    datb = pd.concat([dat,alpha_df],axis=1)

    return datb        

# Function to split data into training and testing sets
def split_data(datb,size_num,min_count,N,I):
    valid_sample = False
    while not valid_sample:
        # Randomly select samples for training and testing
        num = np.random.choice(N, size_num, replace=False)
        training_data = datb.iloc[num, :]
        testing_data = datb.drop(num, axis=0)

        training_dat = training_data.iloc[:, :I]
        training_alpha1 = training_data.iloc[:, I:]
        testing_dat = testing_data.iloc[:, :I]
        testing_alpha1 = testing_data.iloc[:, I:]
        
        # Check if each class has enough samples
        class_counts = [training_alpha1.iloc[:, col].value_counts().reindex(range(attri_level), fill_value=0) for col in range(training_alpha1.shape[1])]
        valid_sample = all(counts.min() >= min_count for counts in class_counts)
    training_alpha = training_alpha1 + 1
    testing_alpha = testing_alpha1 + 1

    return training_dat,training_alpha,testing_dat,testing_alpha

# Function to train the model using a two-step grid search
def train_model_twostep(X_train,y_train,X_test,y_test,attri_num):
    csestA = np.zeros_like(y_test)
    param_info_list = []

    for j in range(attri_num):
        svc = SVC(kernel='rbf')
        # First step grid search
        C_values = 2.0**np.arange(-5, 15, 2)
        gamma_values = 2.0**np.arange(-15, 3, 2)
        param_grid = {'C':C_values,'gamma':gamma_values}

        skf = RepeatedStratifiedKFold(n_splits=2,n_repeats=10)
        grid = GridSearchCV(svc, param_grid, refit=True, cv=skf)
        grid.fit(X_train,y_train.iloc[:,j])

        best_params = grid.best_params_
        best_C = best_params['C']
        best_gamma = best_params['gamma']

        # Second step grid search with finer granularity
        fine_C_values = 2.0**np.arange(np.log2(best_C) - 2, np.log2(best_C) + 2, 0.5)
        fine_gamma_values = 2.0**np.arange(np.log2(best_gamma) - 1, np.log2(best_gamma) + 2, 0.5)
        fine_param_grid = {'C': fine_C_values, 'gamma': fine_gamma_values}
        fine_grid = GridSearchCV(svc, fine_param_grid, refit=True, cv=skf)
        fine_grid.fit(X_train,y_train.iloc[:,j])

        y_pred = fine_grid.predict(X_test)
        csestA[:, j] = y_pred

        param_info_list.append({
            'j': j,
            'best_C': best_C,
            'best_gamma': best_gamma
        })
    S_Wnk = (y_test == csestA).astype(int)

    SACCR = S_Wnk.mean(axis=0)
    SPCCR = np.prod(S_Wnk, axis=1).mean()

    return SACCR,SPCCR,param_info_list

if __name__ == '__main__':
    attri_num = 5   #number of attributes: [3,4,5]
    attri_level = 3 #number of attribute levels: [3,4,5]
    Item=30         #number of items: [30,60]
    N=5000          #number of examinees
    sg = 0.1        #levels of slipping and guessing: [0.05,0.1,0.2]
    size_num = 200  #training sample size: [20,30,50,100,150,200]
    min_count = 2   #minimum number of samples per class
    cycles = 1       #number of iterations: 30
    ah_name = 'independent' #attribute hierarchy name

    print("ah_name=%s, sg=%s, size=%d, Item=%d" % (ah_name, sg, size_num, Item))
    imp = pd.read_csv('../ideal_master_pattern/'+ah_name+'_'+str(attri_num)+'_'+str(attri_level)+'_IMP.csv')
    imp = imp.drop(imp.columns[0],axis=1)
    Rmatrix = pd.read_csv('../Rmatrix/'+ah_name+'_'+str(attri_num)+'_'+str(attri_level)+'.csv')
    Rmatrix = Rmatrix.T

    Qf = pd.read_csv('../Qm/'+ah_name+'_'+str(attri_num)+'_'+str(attri_level)+'_Qf.csv')
    Qf = Qf.drop(Qf.columns[0],axis=1)
    elseQp = pd.read_csv('../Qm/'+ah_name+'_'+str(attri_num)+'_'+str(attri_level)+'_elseQp.csv')
    elseQp = elseQp.drop(elseQp.columns[0],axis=1)

    saccr_results = pd.DataFrame()
    spccr_results = pd.DataFrame()
    all_param_info_list = []

    for i in range(cycles):
        datb = generate_independence_data(Rmatrix, Qf,elseQp,Item,N,sg,attri_level)
        X_train,y_train,X_test,y_test = split_data(datb,size_num,min_count,N,Item)
        SACCR,SPCCR,param_info_list = train_model_twostep(X_train,y_train,X_test,y_test,attri_num)

        saccr_results = pd.concat([saccr_results, SACCR], axis=1)
        spccr_results = pd.concat([spccr_results, pd.Series([SPCCR], name=f'Run_{i+1}')], axis=1)
        for param_info in param_info_list:
            param_info['iteration'] = i + 1
            all_param_info_list.append(param_info)

    saccr_results = saccr_results.T
    spccr_results = spccr_results.T   
    param_info_df = pd.DataFrame(all_param_info_list)

    filename_accr = f"../result/apresult/fSVMACCR-{ah_name}-{attri_num}-{attri_level}-{size_num}-{sg}-{Item}.csv"
    filename_pccr = f"../result/apresult/fSVMPCCR-{ah_name}-{attri_num}-{attri_level}-{size_num}-{sg}-{Item}.csv"
    filename_grid = f"../result/gridresult/{ah_name}-{attri_num}-{attri_level}-{size_num}-{sg}-{Item}.csv"
    saccr_results.to_csv(filename_accr, index=True)
    spccr_results.to_csv(filename_pccr, index=True)
    param_info_df.to_csv(filename_grid, index=True)