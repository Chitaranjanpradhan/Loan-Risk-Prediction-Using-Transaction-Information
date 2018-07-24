DS = read.csv("ds.csv")

trainingset = DS[1:2000,]
testingset = DS[2001:2500,]
str(DS)
library(neuralnet)
#model <- model.matrix(~ Default + Income + LoanBalance + MortgageType + Age + #MortgageYears + InterestRate + LoanToValue + CreditRating, data = DS )
model <- model.matrix(~ loan_status + loan_amnt + funded_amnt + term + annual_inc, data = DS )
net <- neuralnet(loan_status ~ loan_amnt + funded_amnt + term + annual_inc, data = model, hidden = 4, linear.output = F, lifesign = "minimal", threshold = 0.1)
plot(net)
testSet_Features = subset(testingset,select=c("loan_amnt", "funded_amnt", "term", "annual_inc"));
head(testSet_Features)
NNresults = compute(net , testSet_Features );
finalOutput = data.frame(Actual = testingset$Default,
                         Prediction = NNresults$net.result,
                         Matches = doesPredictionMatch( testingset$loan_status, NNresults$net.result, 0.3));
doesPredictionMatch = function(expected = Null, predicted = Null, threshold = 0.3){
  if(is.null(expected) || is.null(predicted)){
    print("Necessary arguments missing or null");
    stop();
  }
  
  results = rep(FALSE,length(expected));
  
  for(i in 1:length(expected)){
    if((!is.na(expected[i]))&&(!is.na(predicted[i]))){  
      if(abs(expected[i]-predicted[i])<threshold ){
        results[i] = TRUE;
      }
    }
    
  }
  return (results);
}
countSuccessPercent = function(input = NULL){
  count= 0;
  for(i in 1:length(input)){
    tmp =as.logical(input[i]); 
    if(is.logical(tmp) && tmp ==TRUE ){
      count=count+1;
    }
  }
  return ((count/length(input))*100);  
}
#sol <- data.frame(actual = testingset$Default, prediction = NNresults$net.result)
sol <- data.frame(actual = testingset$loan_status, prediction = NNresults$net.result)
sol[]