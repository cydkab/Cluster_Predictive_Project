This project consists on the cluster and predictive analysis for a streaming service.

The guideline for this project was the CRISP-DM methodology from end-to-end, going through Business Understanding, 
Data Understanding, Data Preparation, Modeling, Evaluation and Deployment.

The data was all cleaned in R Studio having done some jobs on Jupyter Notebook (for example, the correlation matrix).

The final models applied were:

Clustering: Kmeans (having passed by the DBSCAN and K-medoids)

Classification(churn variable): Random Forest combined with the Linear Regression (also used Decision Tree, Support Vector Machine, 
Logistic Regression and Deep Learning)

In the predictive analysis we studied with Random Forest the probability of churn and using the Linear Regression to predict when will the user Logout.

The models were exececuted on RapidMiner, a VPL for machine learning algorithms.

The data for this problem isn't avaiable once it is a competetive company in the market and protected under RGPD(GDPR) compliance.
