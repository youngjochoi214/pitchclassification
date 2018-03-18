#PITCH_FX

PITCH_FX_Train = read.csv(file ="C:/Users/rolen27/Documents/PITCH FX/train.csv")
PITCH_FX_Test = read.csv(file ="C:/Users/rolen27/Documents/PITCH FX/test.csv")

#install.packages("nnet")
library(nnet)
#install.packages("foreign")
library("foreign")

# I will create one more column which is pitch_type2 to compare everything with fastball
# All outputs will be compared with the base case of fastball because the number of fastball is the largest in train data

PITCH_FX_Train$pitch_type2 = relevel(PITCH_FX_Train$pitch_type, ref = "FA")

# I will create my model called PITCH_FX_Model. I will use multinomial logistic regression
# because the data set does not have order in response variables (FA, SI, CU, CH...)
# Multinomial Logistic Regression is the linear regression analysis used when
# the response variables are more than two

PITCH_FX_Model = multinom(pitch_type2 ~ start_speed + x0 + z0 + px + pz + pfx_x + pfx_z + spinrateND    
                  + spindirND + vxf + vzf + xangle + zangle, data = PITCH_FX_Train)

# I will look at statistical validity of the model.
# First, I will exponentiate the coefficients of the model to get the odds ratio
# Then, I will exponentiate the confidence interval of the model

exp(coef(PITCH_FX_Model))

# Interpretation of the data

#(Intercept) start_speed          x0           z0         px           pz     pfx_x     pfx_z spinrateND
#CH 9.251486e+05   0.6206083  0.04582644 0.0034815940 15.3071939   536.720094 0.9812249 2.1929570  0.99

# For example, one unit increase in pz will positively (because 536.720094 > 1) influence 
# of chance of being change-up compared to fastball. 
# One unit increase in pfx_x  will positively (because 2.1929570 > 1) influence 
# of chance of being change-up compared to fastball.
# Therefore, it means if a pitch has more vertical location and movement, the pitch will
# more likely to be a change-up compared to fastball.
# One unit increase in z0 will adversely (because 0.0034815940 < 1) influence of chance of 
# being change-up compared to fastball.

exp(confint(PITCH_FX_Model))

# If the data set has a zero in confidence interval, I will drop it because it is not 
# statistically significant in the logistic regression.
# Since exp(0)=1, I will drop variables which has 1 in each exponentiated data.
# It does not have FA because it was used as a base case.

# For CH, I will drop pfx-x and spindirND
# For CS, I will drop pfx-x, pfx-z, spinrateND and spindirND
# For CU, I will drop pfx-z
# For EP, I will drop start_speed and pfx_z
# For FC, I will drop spindirND
# For FO, I will drop pfx-x, pfx-z, spinrateND and spindirND
# For FS, I will drop spindirND
# For KN, I will drop start_speed, spinrateND and spindirND
# For SI, I will drop pfx-x, pfx-z, spinrateND and spindirND
# For SL, I will drop pfx-x

# However, there is no single variable that I can drop in common, which
# means I need all these variables for my algorithm. For example, even though spindirND 
# is not important in many vaireables, it is still important in CU and SL

# More importantly, x0, z0, px, pz, vxf, vzf, xangle, zangle are all significant in 
# all types of pitches.

# Misclassification Error

# First, I will test my model with PITCH_FX_Train data

TestTrain = predict(PITCH_FX_Model, PITCH_FX_Train)
show(TestTrain)
# Then, I will compare the data from prediction with actual pitch_type
Predict_vs_Pitch_Type = table(predict(PITCH_FX_Model), PITCH_FX_Train$pitch_type)
print(Predict_vs_Pitch_Type)

# While horizontal column is actual data, vertical column is prediction
# The percentage of correct prediction is 0.78353
# Even though I dropped some variables in my model, the prediction did not improve.
# That's why I use all the variables in my model.

# Now, I will test my model with PITCH_FX_Test data

Test1 = predict(PITCH_FX_Model, PITCH_FX_Test, "probs")
show(Test1)
# Test1 shows the possibilities of each type of pitch.

Test2 = predict(PITCH_FX_Model, PITCH_FX_Test)
show(Test2)
# Test2 shows the pitch-type of PITCH_FX_Test data. However, it omitted 2413 entries
# The following function will show all the data
options(max.print = 4000) 

PITCH_FX_Test$pitch_type = predict(PITCH_FX_Model, PITCH_FX_Test)
# This will complete PITCH_FX_Test data by adding up a new column of pitch_type

write.csv(PITCH_FX_Test, file = 'PITCH_FX_Test with pitch-type.csv')
# This will create a new PITCH_FX-Test CSV file with pitch_type
