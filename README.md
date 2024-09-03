SVM4PACD
=======

## Description
This is the official code of research: Applying support vector machine for diagnostic classification model for polytomous attributes in small sample contexts

## File Structure
* Qm: Q matrix. ---file naming convention: {attribute structure name}\_{number of attributes}\_{number of attribute levels}\_Qm/elseQp.csv <br>
* Rmatrix ---file naming convention: {attribute structure name}\_{number of attributes}\_{number of attribute levels}.csv <br>
* ideal_master_pattern ---file naming convention: {attribute structure name}\_{number of attributes}\_{number of attribute levels}\_IMP.csv <br>
* code/:data files <br>
    * sim_svm_independent.py: Code for implementing the SVM model used to achieve the independent attribute structure in Simulation Study 1 and Simulation Study 2 <br>
    * sim_svm_dependent.py: Code for implementing the SVM model used to achieve the dependent attribute structure (linear, divergent, convergent) in Simulation Study 1 and Simulation Study 2 <br>
    * empirical_svm.ipynb: Code for implementing the SVM model used to achieve empirical study <br>
    * sim_pgdina_independent.R: Code for implementing the pG-DINA model used to achieve the independent attribute structure in Simulation Study 1 <br>
    * sim_pgdina_dependent.R: Code for implementing the pG-DINA model used to achieve the dependent attribute structure (linear, divergent, convergent) in Simulation Study 1 <br>
    * empirical_pgdina.R: Code for implementing the pG-DINA model used to achieve empirical study <br>

* data/:data files <br>
    * Qm.csv: Q matrix for empirical study <br>
    * databind.csv: an example data for empirical study. The empirical data were derived from Karelitz (2004), which can be obtained via email from the original author (tzur@nite.org.il). <br>

