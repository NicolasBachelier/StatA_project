load("C:/Users/bachelien/Desktop/NIPS2013.data")
CorrelMat = eigen(cor(dataset))
CorrelMat$values
sum(CorrelMat$values)

which(ProgVar >= 0.5*423)[1]

##################################################
#PCA METHOD
##################################################

PCA_factors <- function(data, percentage)
{

#Percentage is the amount of variance that we wish to be explained
#by the selected factors

EigenMat = eigen(cor(dataset))

TotVar = sum(EigenMat$values)

ProgVar = cumsum(EigenMat$values)

nbFactors = which(ProgVar >= percentage*TotVar)[1]

MainFactors = EigenMat$vectors[,1:nbFactors]

return(MainFactors)
}



#################################
#Regression
#################################

#Get values of Eigenportfolios on the whole date range

RiskFactors = PCA_factors(dataset, 0.5)
HistoRiskFactors = dataset %*% RiskFactors




LinearModel( factor_model, percentage)
{
#First test of a regression method, will be enriched further later on
RiskFactors = PCA_factors(dataset, 0.5)
HistoRiskFactors = dataset %*% RiskFactors

reg=lm('dataset~.', data= data.frame(HistoRiskFactors))

return (reg$residual)



}

