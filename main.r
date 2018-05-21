library(moments)

blueberriesData <- scan("blueberriesData.txt", double(), quote = "")
redberriesData <- scan("redberriesData.txt", double(), quote = "")

#zad1
EMeasuresOfAssociation <- list(Mean = 1, Mode = 2, Minimum = 3, FirstQuartile = 4, Median = 5, ThirdQuartile = 6, Maximum = 7)

MeasuresOfAssociation <- function(data)
{
  Mode <- function(data)
  {
    uniqueVector <- unique(data)
    countTab <- tabulate(match(data, uniqueVector))
    resultIndex <- which.max(countTab)
    resultCount <- countTab[resultIndex]

    checkTab <- tabulate(countTab);

    if(checkTab[resultCount] > 1)
      return (NA)

    return (uniqueVector[resultIndex])
  }

  result <- vector(mode = "list", length = 7)

  names(result)[EMeasuresOfAssociation$Mean] = "Mean"
  result[EMeasuresOfAssociation$Mean] <- mean(data)

  names(result)[EMeasuresOfAssociation$Mode] = "Mode"
  result[EMeasuresOfAssociation$Mode] <- Mode(data)

  names(result)[EMeasuresOfAssociation$Minimum] ="Null quartile - minumum"
  result[EMeasuresOfAssociation$Minimum] <- quantile(data, 0.00)

  names(result)[EMeasuresOfAssociation$FirstQuartile] ="First quartile"
  result[EMeasuresOfAssociation$FirstQuartile] <- quantile(data, 0.25)

  names(result)[EMeasuresOfAssociation$Median] ="Second quartile - median"
  result[EMeasuresOfAssociation$Median] <- quantile(data, 0.50)

  names(result)[EMeasuresOfAssociation$ThirdQuartile] ="Third quartile"
  result[EMeasuresOfAssociation$ThirdQuartile] <- quantile(data, 0.75)

  names(result)[EMeasuresOfAssociation$Maximum] ="Fourth quartile - maximum"
  result[EMeasuresOfAssociation$Maximum] <- quantile(data, 1.00)

  return (result)
}

EMeasuresOfDiversity <- list(ResultsRange = 1, InterquartileRange = 2, Variance = 3, StandardDeviation = 4, VariationCoefficient = 5)

MeasuresOfDiversity <- function(data, associationMeasuresData)
{
  result <- vector(mode = "list", length = 5)

  names(result)[EMeasuresOfDiversity$ResultsRange] = "ResultsRange"
  result[EMeasuresOfDiversity$ResultsRange] <- (associationMeasuresData[[EMeasuresOfAssociation$Maximum]] - associationMeasuresData[[EMeasuresOfAssociation$Minimum]])

  names(result)[EMeasuresOfDiversity$InterquartileRange] = "InterquartileRange"
  result[EMeasuresOfDiversity$InterquartileRange] <- IQR(data)

  dataLength <- length(data)
  variance <- var(data) * (dataLength - 1) / dataLength;

  names(result)[EMeasuresOfDiversity$Variance] = "Variance"
  result[EMeasuresOfDiversity$Variance] <- variance

  standardDeviation <- sqrt(variance)

  names(result)[EMeasuresOfDiversity$StandardDeviation] = "StandardDeviation"
  result[EMeasuresOfDiversity$StandardDeviation] <- standardDeviation

  names(result)[EMeasuresOfDiversity$VariationCoefficient] = "VariationCoefficient"
  result[EMeasuresOfDiversity$VariationCoefficient] <- (standardDeviation / associationMeasuresData[[EMeasuresOfAssociation$Mean]])

  return (result)
}

EMeasuresOfAsymmetry <- list(SkewnessCoefficient = 1)

MeasuresOfAsymmetry <-function(data)
{
  result <- vector(mode = "list", length = 1)

  names(result)[EMeasuresOfAsymmetry$SkewnessCoefficient] = "SkewnessCoefficient"
  result[EMeasuresOfAsymmetry$SkewnessCoefficient] <- skewness(data)

  return (result)
}

EMeasuresOfConcentration <- list(Kurtosis = 1)

MeasuresOfConcentration <-function(data)
{
  result <- vector(mode = "list", length = 1)

  names(result)[EMeasuresOfConcentration$Kurtosis] = "Kurtosis"
  result[EMeasuresOfConcentration$Kurtosis] <- kurtosis(data)

  return (result)
}

CalculateHistogramBreaks <- function(data, associationMeasuresData)
{
  minimum <- associationMeasuresData[[EMeasuresOfAssociation$Minimum]]
  maximum <- associationMeasuresData[[EMeasuresOfAssociation$Maximum]]
  rangeLength <- (maximum - minimum) / sqrt(length(data))

  result <- seq(minimum, maximum, rangeLength)

  return (result);
}

blueberriesMeasuresOfAssociations <- MeasuresOfAssociation(blueberriesData)
blueberriesMeasuresOfDiversity <- MeasuresOfDiversity(blueberriesData, blueberriesMeasuresOfAssociations)
blueberriesMeasuresOfAsymmetry <- MeasuresOfAsymmetry(blueberriesData)
blueberriesMeasuresOfConcentration <- MeasuresOfConcentration(blueberriesData)
blueberriesHistogramBreaks <- CalculateHistogramBreaks(blueberriesData, blueberriesMeasuresOfAssociations)

writeLines("Blueberries' measures of associations:");
print(blueberriesMeasuresOfAssociations);

writeLines("Blueberries' measures of diversity:");
print(blueberriesMeasuresOfDiversity);

writeLines("Blueberries' measures of asymmetry:");
print(blueberriesMeasuresOfAsymmetry);

writeLines("Blueberries' measures of concentration:");
print(blueberriesMeasuresOfConcentration);

hist(blueberriesData, breaks = blueberriesHistogramBreaks)

redberriesMeasuresOfAssociations <- MeasuresOfAssociation(redberriesData)
redberriesMeasuresOfDiversity <- MeasuresOfDiversity(redberriesData, redberriesMeasuresOfAssociations)
redberriesMeasuresOfAsymmetry <- MeasuresOfAsymmetry(redberriesData)
redberriesMeasuresOfConcentration <- MeasuresOfConcentration(redberriesData)
redberriesHistogramBreaks <- CalculateHistogramBreaks(redberriesData, redberriesMeasuresOfAssociations)

writeLines("Redberries' measures of associations:");
print(redberriesMeasuresOfAssociations);

writeLines("Redberries' measures of diversity:");
print(redberriesMeasuresOfDiversity);

writeLines("Redberries' measures of asymmetry:");
print(redberriesMeasuresOfAsymmetry);

writeLines("Redberries' measures of concentration:");
print(redberriesMeasuresOfConcentration);

hist(redberriesData, breaks = redberriesHistogramBreaks)

blueberriesMeasuresOfAssociations <- MeasuresOfAssociation(blueberriesData)
blueberriesMeasuresOfAssociations$Mean
blueberriesMeasuresOfDiversity <- MeasuresOfDiversity(blueberriesData, blueberriesMeasuresOfAssociations)
blueberriesMeasuresOfDiversity$Variance
redberriesMeasuresOfAssociations <- MeasuresOfAssociation(redberriesData)
redberriesMeasuresOfAssociations$Mean
redberriesMeasuresOfDiversity <- MeasuresOfDiversity(redberriesData, redberriesMeasuresOfAssociations)
redberriesMeasuresOfDiversity$Variance

#zad2
Kolmogorow <- function(database, mea, varia)
{
  data<- sort(database)
  n <-length(data)
  stdX <- database
  distributionTable <- 0.264
  inn<- database
  distribution <-database
  difference <-database
  differenceTwo<- database

  for (i in 1:n)
    stdX[i]<- ((data[i] -mea )/ sqrt(varia) )

  for (i in 1:n)
    inn[i] <- (i / n )

  for (i in 1:n)
    distribution[i] <- pnorm(stdX[i])

  for (i in 1:n)
    difference[i] <-abs(distribution [i] -inn [i] )

  for (i in 1:n)
    differenceTwo[i] <-abs( (i -1) / n -distribution [i])

  tmp <-max (difference)
  tmpSec <-max (differenceTwo)
  testStatisticValue <-max (difference, differenceTwo)

  writeLines("Kolmogorow's statistic's value")
  print(testStatisticValue)
  writeLines("\nCritical area <0,264; 1>")

  if(testStatisticValue < distributionTable || testStatisticValue> 1)
  {
    writeLines("We can't rule out hypothesis H0\n")
    writeLines("Normal distribution\n")
  }
  else
  {
    writeLines("We can rule out hypothesis H0\n")
    writeLines("Non normal distribuion\n")
  }
}

writeLines("Blueberries:\n")
blueberrieKolmogorow <- Kolmogorow(blueberriesData,blueberriesMeasuresOfAssociations$Mean,  blueberriesMeasuresOfDiversity$Variance)

writeLines("Redberries:\n")
redberrieKolmogorow <- Kolmogorow(redberriesData, redberriesMeasuresOfAssociations$Mean,  redberriesMeasuresOfDiversity$Variance)

#zad 3
MeanIntervalEstimation <- function(Mean,StandardDeviation,DataLenght,AlfaFact)
{
  FactNorm <- qnorm(1 - AlfaFact/2)
  halfIntervalLenght <- FactNorm*StandardDeviation/sqrt(DataLenght)
  DownlimitMean <- Mean - halfIntervalLenght
  UplimitMean <-  Mean + halfIntervalLenght
  intervalLenght <- 2 * halfIntervalLenght
  PrecisionMean <- halfIntervalLenght/Mean * 100
  writeLines("Estimated mean range: ")
  print(DownlimitMean)
  print(UplimitMean)
  writeLines("Mean Interval lenght: ")
  print(intervalLenght)
  writeLines("Precision: ")
  print(PrecisionMean)
}
writeLines("Redberries:")
redberriesMeanIntervalEstimation <-MeanIntervalEstimation(redberriesMeasuresOfAssociations$Mean, redberriesMeasuresOfDiversity$StandardDeviation, 25, 0.02)
writeLines("\n")

#zad 4
VarianceIntervalEstimation <- function(Mean,StandardDeviation,DataLenght,AlfaFact)
{
  FactNorm <- qnorm(1 - AlfaFact/2)
  halfIntervalLenght <- FactNorm*StandardDeviation/sqrt(2*DataLenght)
  DownlimitMean <- StandardDeviation - halfIntervalLenght
  UplimitMean <-  StandardDeviation + halfIntervalLenght
  intervalLenght <- 2 * halfIntervalLenght
  PrecisionMean <- halfIntervalLenght/Mean * 100
  writeLines("Estimated standard deviation range: ")
  print(DownlimitMean)
  print(UplimitMean)
  writeLines("Standard deviation interval lenght: ")
  print(intervalLenght)
  writeLines("Precision: ")
  print(PrecisionMean)
}
writeLines("Blueberries:")
blueberriesMeanIntervalEstimation <-VarianceIntervalEstimation(blueberriesMeasuresOfAssociations$Mean, blueberriesMeasuresOfDiversity$StandardDeviation, 25, 0.02)
writeLines("\n")

#zad5
SameMeanValueTest <-function(FirstMean, FirstVariance, FirstCount, SecondMean, SecondVariance, SecondCount, AlfaFactor)
{
  Statistic <- (FirstMean-SecondMean)/sqrt(FirstVariance/FirstCount+SecondVariance/SecondCount)
  FactNorm <- qnorm(1 - 0.05/2)
  writeLines("Our Statistic: ")
  print(Statistic)
  writeLines("Critical area quantile: ")
  print(FactNorm)
  if(Statistic > FactNorm || Statistic < -FactNorm)
  {
    writeLines("We can rule out H0")
    writeLines("Means are not equal")
  }
  else
  {
    writeLines("We can't rule out H0")
    writeLines("Means are most likely equal")
  }
}
Test <-SameMeanValueTest(redberriesMeasuresOfAssociations$Mean, redberriesMeasuresOfDiversity$Variance, 25,blueberriesMeasuresOfAssociations$Mean, blueberriesMeasuresOfDiversity$Variance, 25)