bucketName <- "bucket"
numberOfBuckets = 10
seperator <- "\t"
fileData <- data.frame()
for ( i in 1:10){
	if(i > 9){
		fileName <- paste("house-votes/hv-", i, sep="")
	}else{
		fileName <- paste("house-votes/hv-0", i, sep="")
	}
	datalist = lapply(fileName, function(x){read.delim(file=x,header=F,
					sep=seperator)})
	Reduce(function(x,y) {merge(x,y)}, datalist)
	fileData <- rbind(fileData, data.frame(datalist))
}

names(fileData) <- c("class", "attr", "attr" , "attr" , 
	"attr" , "attr" , "attr" , "attr" , "attr",  "attr", 
	"attr", "attr", "attr", "attr" ,"attr", "attr" ,"attr")



#### create buckets in a list with names
bucketKeys <- vector()
for (i in 1:numberOfBuckets){
	bucketKeys <- c(bucketKeys, paste(bucketName, i, sep=""))
}
bucketList <- vector(mode="list", length=length(bucketKeys))
names(bucketList) <- bucketKeys


#################
### list of buckets


#################
#################
#### list of dataframes class by class

### first convert the class to factor, its jus convenient 
fileData$class <- as.factor(fileData$class )
numberOfClasses = length(levels(fileData$class))
numberOfClasses = levels(fileData$class)

ckeys <- vector()
for (i in numberOfClasses){
	ckeys <- c(ckeys, paste("class", i, sep=""))
}
clist <- vector(mode="list", length=length(ckeys))
names(clist) <- ckeys

################
#### populate each class
#### with own attributes 

y = 1
classValue = strsplit(numberOfClasses, "   ")
classValue[y]
for (i in names(clist)){
	if(paste(i) == paste("class", classValue[y], sep="")){
		clist[[i]] <- fileData[fileData$class == classValue[y], ]
	}
	y <- y + 1
}
#### How the list list looks now 
names(clist)







################
#### populate the buckets

maxClass <- max(as.data.frame(lapply(clist, function(x) nrow(x))))

#### new data frame generated from the 
newFile <- data.frame()

for (i in 1:maxClass){
	combined <- lapply(clist, function(class) class[i,])
	temp <- data.frame()
	for (value in combined){
		temp <- rbind(temp, value[complete.cases(value), ])
	}
	newFile <- rbind(newFile, temp)
}

bucketWheel <- function(frame, bn){
	
	if((bn %% 11) == 0 || bn == 0){
		bn <- 1
	}
	bucketList[[bn]] <<- rbind(bucketList[[bn]], frame[complete.cases(frame), ])
	
	bn <- (bn + 1) %% 11
	return(bn)
}

#### keep a copy of the newFile
v <- newFile 
#### keep track of the bucketList
bn <- 1
#### the range of rows that will be put into each bucket
range <- nrow(v[1:3,])
#### the number of iterations in the loop
loop <- nrow(v)
while (loop){
	bn <- bucketWheel(v[1:range, ], bn)
	v <- v[-c(1:range), ]
	loop <- nrow(v)
}

#### check the number of rows in the buckets
sum(as.data.frame(lapply(bucketList, function(x) nrow(x))))



################################
#### initiate cross validation
################################
cycleBuckets <- function(testBucket, bucketList){
	bucketList[testBucket] <- NULL
	frame <- do.call(rbind, bucketList)
	#numericalCols <- frame[c(-1, -length(frame))]
	#for (column in 1:length(numericalCols)){
	#	numericalCols[,column] <- normalizeColumn(column, numericalCols)
	#}
	#frame[c(-1, -length(frame))] <- numericalCols
	return(frame)
	

}


classify <- function(vector, prior, classList){
	result <- data.frame()
	for ( i in 1:nrow(prior)){
		prob <- prior[i,]$pr
		for (x in 1:length(classList)){
			for (y in 2:length(classList[[x]])){
				#### assuming vector data exits in the training set
				
				if (classList[[x]][[y]][classList
						[[x]][[y]]$Var2 == vector[,y], ]$Var1 == 
					prior[i,]$class){
					
					prob <- prob * classList[[x]][[y]][classList
							[[x]][[y]]$Var2 == vector[,y], ]$Freq
					r <- data.frame(prior[i,]$class, prob)
					
				}
			}
			
		}
		
		result <- rbind(result, r)
	}
	names(result) <- c("class", "prob")
	return(result)
}


cycleDataFrame <- data.frame()
vecDists <- data.frame()
resultFrame <- data.frame()
library(plyr)
for (testBucket in names(bucketList)){
	
	#### reserving the data frame for testing
	testFrame <- bucketList[[testBucket]]
	
	#### we skip the testBucket and
	#### return the cycle's data frame
	cycleDataFrame <- cycleBuckets(testBucket, bucketList)
	
	prior <- data.frame()
	## to be in the list 
	classCount <- count(cycleDataFrame$class)
	
	#### prior probablilities for each class
	for (theRow in 1:nrow(classCount )){
		pr <- classCount[theRow,]$freq / sum(classCount$freq)
		pr <- cbind(classCount[theRow,][1], pr)
		prior <- rbind(prior, pr)
	}
	names(prior) <- c("class", "pr")
	
	counts <- data.frame()
	count(cycleDataFrame[4])
	
	#### create for classes to hold conditional 
	#### probabilities for each given the attribute
	#### p(D|h)
	classKeys <- vector()
	for (i in levels(cycleDataFrame$class)){
		classKeys <- c(classKeys , i)
	}
	classList <- vector(mode="list", length=length(classKeys))
	names(classList) <- classKeys
	
	
	#### create a list for all attributes
	attrKeys <- vector()
	for (i in 1:length(cycleDataFrame)){
		attrKeys <- c(attrKeys , paste("attr", i, sep=""))
	}
	attrList <- vector(mode="list", length=length(attrKeys))
	names(attrList) <- attrKeys 
	
	
	#### populate conditional probabilities
	for (i in 1:nrow(classCount)){
		classList[[classCount[i,]$x]] <- attrList
		for ( x in 2:length(cycleDataFrame)){ 
			tmpAttr <- as.data.frame(as.matrix(table(
				cycleDataFrame$class, cycleDataFrame[[x]])))
			tmpAttr <- tmpAttr[tmpAttr$Var1 == 
				classCount[i,]$x, ]
			tmpAttr$Freq <- tmpAttr[tmpAttr$Var1 == 
				classCount[i,]$x, ]$Freq/classCount[i,]$freq
			classList[[classCount[i,]$x]][[paste("attr", x, sep="")]] <-
				tmpAttr
			
			
		}
		
	}
	
	
	#### classify
	for (i in 1:nrow(testFrame)){
		theRealClass <- testFrame[i,]$class
		results <- classify(testFrame[i,], prior, classList)
		r <- max(results$prob)
		classifiedAs <- results[results$prob == r, ]
		resultFrame <- rbind(resultFrame, cbind(theRealClass, classifiedAs))
	}
	
	
}

#### get the confusion matrix
confusionMatrix <- as.matrix(table(resultFrame$class, 
				resultFrame$theRealClass ))

confusionMatrix


#### some basic variables that will be 
#### needed to compute the evaluation metrics

#### number of instances
n = sum(confusionMatrix) 

#### number of classes
nc = nrow(confusionMatrix) 

#### number of correctly classified instances per class 
diag = diag(confusionMatrix) 

#### number of instances per class
rowsums = apply(confusionMatrix, 1, sum) 

#### number of predictions per class
colsums = apply(confusionMatrix, 2, sum) 

#### distribution of instances over the actual classes
p = rowsums / n 

#### distribution of instances over the predicted classes
q = colsums / n 


#### Accuracy
#### A key metric to start with is the overall classification accuracy. 
#### It is defined as the fraction of instances that are correctly classified.

accuracy = sum(diag) / n 
accuracy 

#### In order to assess the performance with respect to every class in the 
#### dataset, we will compute common per-class metrics such as precision, 
#### recall, and the F-1 score. These metrics are particularly useful when 
#### the class labels are not uniformly distributed (most instances belong 
#### to one class, for example). In such cases, accuracy could be misleading 
#### as one could predict the dominant class most of the time and still 
#### achieve a relatively high overall accuracy but very low precision or 
#### recall for other classes. 

#### Precision is defined as the fraction of correct predictions for a 
#### certain class, whereas 

#### recall is the fraction of instances of a class that were correctly 
#### predicted. 

#### Notice that there is an obvious trade off between these 2 metrics. 
#### When a classifier attempts to predict one class, say class "democrats", 
#### most of the time, it will achieve a high recall for "democrats"
#### (most of the instances of that class will be identified).
#### However, instances of other classes will most likely be 
#### incorrectly predicted as a in that process, 
#### resulting in a lower precision for a. 

#### In addition to precision and recall, the F-1 score is also commonly 
#### reported. It is defined as the harmonic mean (or a weighted average) of 
#### precision and recall.

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)

data.frame(precision, recall, f1) 




