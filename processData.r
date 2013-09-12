
#The two following methods are a subset of the code that can be found here https://github.com/chungkhim/titanic/blob/master/processData.r
enrichData = function(x)
{
  nameSplit = sapply(x["name"], function(y) {strsplit(y, "[,.] | [ ]")})
  x["lastName"] = sapply(nameSplit, function(y) {y[1]})
  x["title"] = sapply(nameSplit, function(y) {y[2]})
  x["otherName"] = sapply(nameSplit, function(y) {y[3]})    
 
  return(x)
}

#aggreating some rare occurence of titles into broader categories
refineData = function(x)
{
  ladies = c( "Mme" , "the Countess" , "Lady" ,  "Dona" )
  x$title[x$title %in% ladies] = "Mrs"
  
  miss = c("Mlle", "Ms")
  x$title[x$title %in% miss] = "Miss"
  
  ranks = c("Capt", "Col", "Major","Sir", "Don","Dr")
  x$title[x$title %in% ranks] = "Mr"
  
  masters = c("Jonkheer")
  x$title[x$title %in% masters] = "Master"
  
  
  return(x)
  
}  

#wrapper function to enrich and refine the train/data sets as long
#as casting some variables into factors
processData <- function(data){
  data <- enrichData(data)
  data <- refineData(data)  
  
  #pclass  sex and title should be considered as a factors
  data$pclass <- as.factor(data$pclass )
  data$sex <- as.factor(data$sex )
  data$title <- as.factor(data$title )
  
  return(data);
  
}






