# hurricaneDamagePredictions
Using publicly available data flood insurance claims and FEMA disaster assistance to predict household level hurricane damage distributions

## Objective:
Engineering based damage functions for hurricanes rely on specific inputs about precise storm conditions and difficult to obtain home characteristics. This project attempts to predict distributions of hurricane damage for individual households by combining past flood insurance claims with historical hurricane conditions. The folloing steps are used to generate the predictions.

## Step 1: Clean Data
Publically avaliable data on flood insurance claims and historical hurricane conditions are combined to create a data base of flood insurance claims for which the damage can be attributed to a hurricane. The cleaned data set contains information on the flood insurance claim, some home characteristics (flood zone, home type, elevated, basement, floors..), hurricane conditions for the county that the home is located in (total precipitation +/- 2 days from closest location, wind speed (max gust / 1 min sustained)). Indicators for if the home is in a coastal state and coastal county are added.

Inflation data from FRED are also added to inflation adjust dollar values. Adding data from Zillow's home value index also adds the ability to control for mean county home price which serves as an imperfect indicator for home value in the absence of detailed home value data. 

## Step 2: Train ML model
The next step is to train a machine learning model (xgboost in this case) to predict hurricane damage. See the python notebook for details on how this was done.

## Step 3: Cluster Observations 
One the ML model has been deemed to be sufficiently accurate, uncertainty in future hurricane conditions needs to be addressed. To create a distribution of hurricane conditions that is descriptive of what a particular home is subject to experience, I use K-means to cluster the data on home characteristics. I then extract all the hurricane conditions reported in each cluster and use those conditions to construct the distribution of possible condtions for each household in that same cluster.

## Step 4: Run Monte-Carlo Simulation
To generate the predicted distribution of damage in the event of a hurricane, the following steps are taken

  1) extract a single observation from the test data
  
  2) Identify which cluster the observation belongs to
  
  3) Extract the hurricane condtions from a different randomly selected observation that belongs to the same cluster as the choosen observtion. These hurricane condtions then get randomly perturbed by +/- up to 5% to create more variation in hurricane conditions.
  
  4) Plug in the random hurricane conditions generated in step 3 into the trained xgboost model along with the home characteristics of the selected observation to predict home damage.
  
  5) Repeat steps 3 and 4 many times to generate n single predictions. This set of n predictions forms the predicted distribution of hurricane damage for that observation.
  
  6) repeat step 1 - 5 for all observations in the test data.
  
  See predicted_claim_example.png for an example of a predicted distribution for a single household compared against the observed damage from the flood insurance claims dat


## Step 5: Validate
Check how often observed nfip claims fall within the predicted distribution. Currently, observed nfip claims are contained in the predicted damage interval 67% of the time and the average observation has a observed nfip claim that is at the 43rd percentile of the predicted damage distribution.

