# thesis2025
Genomes to Field Corn Hybrid Prediction (https://www.kaggle.com/competitions/genomes-to-field-corn-hybrid-prediction/overview)
This is a repo for Tsz Yau Iris Chow's thesis project on "PREDICTING HYBRID-SPECIFIC CORN YIELD ACROSS LOCATIONS: INTEGRATING COMPUTATIONAL METHODS, CLASSIFICATION, AND CONTEST-DRIVEN MODELING"
This thesis consist of three chapters:
- the first chapter is LITERATURE REVIEW ON ANALYTICAL COMPLETION IN PLANT BREEDING
- the second chapter is on METHODOLOGIES FOR HYBRID YIELD CLASSIFICATION AND PREDICTION
- the third chapter is APPLICATION TO HYBRID YIELD PREDICTION CONTESTS

Second Chapter code:
this chapter treat corn yield prediction as a classification problem.
We extract phenotypic data from G2F contest and did the data cleaning
Then, we run the envirotyping tool developed by Catherine Gilbert to add environment covariates into the data
We devised a binomial yield variable according to each hybrid’s yield is compared to a standard check, generating a binary label indicating performance success (Y for above-check yield, N otherwise)
We then tried to predict the success of maize hybrids (cultivars) across diverse environments in the United States using ensemble and random forests machine learning problem

Third Chapter code:
this chapter treat corn yield prediction as a regression problem.
Since some data are missing in the testing data, and there are many columns that are difficult to handle by the model,
we computed Principle Components to represent genetics and environment data. 
Also, we computed the PCs for areas that are unknown by their location.
We then run the full dataset in the random forest model
