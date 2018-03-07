####PROBLEM-1####
#this function loads files from a zip folder and reads the text files
read_digits <- function(filespath) {
  df=read.table(filespath)
  df=as.data.frame(df)
  df$V1 <- as.factor(df$V1)
  return(df)
}

test=read_digits("~/Documents/UCDavis/STA141A/final/digits/test.txt")
train=read_digits("~/Documents/UCDavis/STA141A/final/digits/train.txt")




####PROBLEM-2####
view_digit = function(rownum,filename){
  #subtract the row we interested from file, leave out first column, and convert it to vector
  v=as.vector(c(filename[rownum,-1]))
  
  #convert the data to numeric, and convert to 16 by 16 matrix, prepart to draw the 16 by 16 plot
  m=matrix(as.numeric(v),nrow=16,ncol=16,byrow = T)
  
  #draw the number based on matrix
  #https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
  image(t(m[nrow(m):1,]), xaxt = "n", yaxt = "n", col = grey(seq(0, 1, length = 256)))
}

#call function
view_digit(29,train)




####PROBLEM-3####
#part1
avg_digit=function(digit,filename){
  #subset all rows equal to certain digit
  number = subset(filename,filename$V1==digit)
  
  #calculate mean of each pixel, and convert it as vector
  avg_number=lapply(number,mean)
  v=as.vector(avg_number[c(2:257)])
  
  #convert the mean of each pixel to 16 by 16 matrix and draw it
  m=matrix(as.numeric(v),nrow=16,ncol=16,byrow = T)
  #https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
  image(t(m[nrow(m):1,]), xaxt = "n", yaxt = "n", col = grey(seq(0, 1, length = 256)))
}


par(mfrow = c(3,4))
#plot 10 digit in one graph using for loop
for(i in 0:9){
  #call function to draw each digit
  avg_digit(i,train)
}
par(mfrow = c(1,1))


#part2
##frequency
pixelusage= numeric(256)

#count number of time each pixel has value other than "-1"(which we define as being used here)
#I first used for loop, and than used sapply function to do it. both give same result
#eventually we did not include this part in the report
sapply(1:256, function(i){
  tablefreq = table(train[,i+1])
  pixelusage[i]=sum(tablefreq[-1])
})

#using for loop as second method
for (i in 1:256){
  tablefreq = table(train[,i+1])
  pixelusage[i]=sum(tablefreq[-1])
}

#draw plot of frequency of each pixel
m=matrix(as.numeric(pixelusage),nrow=16,ncol=16,byrow = T)
image(t(m[nrow(m):1,]), xaxt = "n", yaxt = "n", col = grey(seq(0, 1, length = 256)))

##variance
#calculate variance of each column(pixel),exclude column 1 since it is lable
variance = sapply(train,var)
variance=variance[-1]

#draw plot
m=matrix(as.numeric(variance),nrow=16,ncol=16,byrow = T)
image(t(m[nrow(m):1,]), xaxt = "n", yaxt = "n", col = grey(seq(0, 1, length = 256)))
#the greater the variance, the more pixel changes color, more useful

#find which column has greatest and minimum variance
which.max(m)
which.min(m)
which.min(variance)
which.max(variance)




####PROBLEM-4####
#this function just predicts the kth nearest neighbors
predict_knn <- function(prediction_points, training_points, distance_metric, k) {   #a point means a row of numbers
  matrix_together <- (rbind(training_points[, -1], prediction_points[,-1])) #bind the two data sets: training and the prediction
  distances <- as.matrix(dist(matrix_together, method= distance_metric))  #find the distances between matrices
  distances
  i = nrow(prediction_points)
  result = c()  #allocate space for the result vector
  result
  for (i in 1:i){   #iterate through each row of the prediction data set
    test_row <- as.data.frame(distances[nrow(training_points)+i, -ncol(distances)])  #make a data frame of that prediction row
    colnames(test_row) <- "Distance"
    test_row$Row_number <-  c(1: nrow(test_row)) #insert the row number of the training data
    test_row
    test_row$labels <- training_points$V1[1:nrow(test_row)]   #insert the labels of the training data
    ordered <- test_row[order(test_row$Distance),]  #order the data frame into order to find the smallest distance
    ordered
    kth_nearest_neighbors <- head(ordered, k)    #take the kth smallest distances
    frequency_labels <-data.frame(table(kth_nearest_neighbors$labels))
    frequency_labels
    #ties
    if(length(frequency_labels$Var1[which(frequency_labels$Freq == max(frequency_labels$Freq))]) > 1) { #if there are two max (with same frequency)
      result[i] <-as.character(sample(frequency_labels$Var1[which(frequency_labels$Freq== max(frequency_labels$Freq))], 1))  #sample the label for which their max frequency are the same
      #citation:https://stackoverflow.com/questions/31308313/two-equal-max-values-in-r
      
    }
    else{   #if not, just store the result into the vector result
      result[i] <-as.character(frequency_labels$Var1[which.max(frequency_labels$Freq)])
    }
    
  }
  result
}




####PROBLEM-5####
# Data preparation for the q5 function
# Randomized the train data , and use it in "cv_error_knn (R.train ,k distance_metric)" function

# R.train <- train[sample(nrow(train), replace= F),]
# DistMat_Euc <- as.matrix(dist(R.train[ ,-1], method= "euclidean")) # Distance Matrix
# DistMat_Man <- as.matrix(dist(R.train[ ,-1], method= "manhattan"))



# Start of Function
cv_error_knn= function(R.train, DistMat, k) {
  
  # Before the loop, I create two empty vector to store reuslts
  error= numeric(10)
  result= numeric(729)
  
  # Start of Looping 1: The goal here is to subset distance matrix into 10 folds
  for (i in 1:10) {
    
    s= (729*(i-1)+1):(729*i)  # Index for subseting !
    
    # Subset 
    dist.s=DistMat[s,-s] # Subset the distaince matrix
    lab= R.train[-s,1]   # Select the actual labels from randomized training dataset
    
    ## Start of Looping 2: 
    ## The goal here is to do prediction for one fold, based on the "k" neighbors.
    for (j in 1:729){
      
      ## Inner Start of Looping 2
      df = data.frame(dist.s[j, ], lab) # "j" # creat a df which has dist and actual labels
      df = df[order(df[ ,1]),] # order by df[1] which is the distance
      knn <- head(df, k) # select the "k" nearest neighbor
      freq.label <-data.frame(table(knn[2])) # count the most labels
      
      # randomly select, if there are ties
      # freq.label( predicted ) is stored in result
      if(length(freq.label$Var1[which(freq.label$Freq == max(freq.label$Freq))]) > 1) {
        result[j] <-as.character(sample(freq.label$Var1[which(freq.label$Freq== max(freq.label$Freq))], 1))
      }else{
        result[j] <-as.character(freq.label$Var1[which.max(freq.label$Freq)])
      }
      
    }## End of Looping 2
    
    # Error rate 
    # Result (predicted label) vs Actual label
    # Use the True (1) and False (0) to calc. the error rate
    error[i] = 1- mean(result == R.train[s,1] )
    
  }# End of Looping1
  
  error
  error.rate= mean(error)
  return(error.rate)
  
}### End of the function




####PROBLEM-6####

### Error rate vs K=1:15 ; DistMetric= "euclidean" ###
# Error rate vs K=1:15 ; DistMetric= "euclidean"
# Create a empty vector to store results
Error_Euc = numeric(15)
for(k in 1:15) {
  #save error rate for each k
  Error_Euc[k]= cv_error_knn(R.train, DistMat_Euc, k)
}

#call the function
df_Error_Euc = data.frame(Error_Euc, c(1:15))

### Error rate vs K=1:15 ; DistMetric= "manhattan" ###
Error_Man = numeric(15)
for(k in 1:15) {
  #save error rate for each k
  Error_Man[k]= cv_error_knn(R.train, DistMat_Man, k)
}

#call the function
df_Error_Man = data.frame(Error_Man, c(1:15))

#plot
plot(c(1:15), df_Error_Euc$Error_Euc , xlab= "k", lty= "solid", type= "l", 
     ylab= "Misclassification Error Rates", main= "Misclassification Error Rates vs. k", col= "blue", ylim= c(0.02,0.06))
lines(c(1:15), df_Error_Man$Error_Man, type= "l", lty= "dotted",  col= "green")
legend("bottomright", c("Euclidian", "Manhattan"), col= c("blue", "green"), lty= c("solid", "dotted"))




####PROBLEM-7####
pred.actual.func = function(R.train, DistMat, k) {
  
  # Before the loop, I create empty vectors to store information 
  p.a.vector= data.frame( prediction= rep(0,7290), actual= rep(0,7290) )
  result= numeric(729)
  
  # Start of Looping1: The goal is to subset the trainning into 10 folds
  for (i in 1:10) {
    
    s= (729*(i-1)+1):(729*i) # Index of subsetting
    
    # subset
    dist.s=DistMat[s,-s] # Subset the distance matrix
    lab= R.train[-s,1]   # Select the actual label from randomized trainning data set
    
    ## Start of Looping 2 : The goal is to do classification in each fold
    for (j in 1:729){
      
      ## Inner Start of Looping 2
      df = data.frame(dist.s[j, ], lab) # "j" # creat a df which has dist and actual labels
      df = df[order(df[ ,1]),] # order by df[1] which is the distance
      knn <- head(df, k) # select the "k" nearest neighbor
      freq.label <-data.frame(table(knn[2])) # count the most labels
      
      # randomly select, if there are ties
      if(length(freq.label$Var1[which(freq.label$Freq == max(freq.label$Freq))]) > 1) {
        result[j] <-as.character(sample(freq.label$Var1[which(freq.label$Freq== max(freq.label$Freq))], 1))
      }else{
        result[j] <-as.character(freq.label$Var1[which.max(freq.label$Freq)])
      }
      
    }## End of Looping 2
    
    # Auctual and predicted labels
    p.a.vector[s,1] = R.train[s,1] # Actual labels
    p.a.vector[s,2] = result       # predicted labels 
    
  }# End of Looping1
  
  return( p.a.vector )
  
}### End of the function

# Create confusion matrix
P.A.Euc.k1 = pred.actual.func(R.train, DistMat_Euc, 1) 
P.A.Euc.k3 = pred.actual.func(R.train, DistMat_Euc, 3) 
P.A.Euc.k4 = pred.actual.func(R.train, DistMat_Euc, 4) 
conf.mat.Euc.k1 = t ( as.matrix ( table(P.A.Euc.k1) ) ) # confusion matrix
conf.mat.Euc.k3 = t ( as.matrix ( table(P.A.Euc.k3) ) ) # confusion matrix
conf.mat.Euc.k4 = t ( as.matrix ( table(P.A.Euc.k4) ) ) # confusion matrix

# Use confusion matrix to calc error rate at k
Error.Euc.k1=  ( sum(conf.mat.Euc.k1) - sum(diag(conf.mat.Euc.k1 )) ) / sum(conf.mat.Euc.k1)
Error.Euc.k3=  ( sum(conf.mat.Euc.k3) - sum(diag(conf.mat.Euc.k3 )) ) / sum(conf.mat.Euc.k3)
Error.Euc.k4=  ( sum(conf.mat.Euc.k4) - sum(diag(conf.mat.Euc.k4 )) ) / sum(conf.mat.Euc.k4)



####PROBLEM-8####
#Euclidian method with k=1 has smallest error rate
#Data preparation for the q8 function
#add a column to dataset before randomization so we can keep track of original row number
train$index <- c(1:7291)

#randomize dataset, and calculate distances between each rows
R.train <- train[sample(nrow(train), replace= F),]
DistMat_Euc <- as.matrix(dist(R.train[ ,-c(1,258)], method= "euclidean")) # Distance Matrix
#DistMat_Man <- as.matrix(dist(R.train[ ,-c(1,258)], method= "manhattan"))

# Start of Function
cv_error_knn_8= function(R.train, DistMat, k) {
  
  # Before the loop
  error= numeric(10)
  result= numeric(729)
  true_or_false = matrix(rep(0,7290),nrow = 10 ,ncol = 729)
  
  # Start of Looping1
  for (i in 1:10) {
    
    s= (729*(i-1)+1):(729*i)  # Works!
    
    # subset
    dist.s=DistMat[s,-s] # Works!
    lab= R.train[-s,1] 
    
    ## Start of Looping 2
    for (j in 1:729){
      
      ## Inner Start of Looping 2
      df = data.frame(dist.s[j, ], lab) # "j" # creat a df which has dist and actual labels
      df = df[order(df[ ,1]),] # order by df[1] which is the distance
      knn <- head(df, k) # "k" nearest neighbor
      freq.label <-data.frame(table(knn[2]))
      
      if(length(freq.label$Var1[which(freq.label$Freq == max(freq.label$Freq))]) > 1) {
        result[j] <-as.character(sample(freq.label$Var1[which(freq.label$Freq== max(freq.label$Freq))], 1))
      }else{
        result[j] <-as.character(freq.label$Var1[which.max(freq.label$Freq)])
      }
      
    }## End of Looping 2
    result
    
    # create a matrix of TRUE/FALSE for whether our prediction is correct
    true_or_false[s] = (result == R.train[s,1])
    
  }# End of Looping1
  
  
  return(true_or_false)
  
}


#run function with k=1, using method of Euclidean
tof = cv_error_knn_8(R.train, DistMat_Euc, 1)

#find position where prediction is not right
postion_false = which(tof == 0)

#examine few numbers were being mis-classified
i=1
j=274

##find which a row which we predict wrong,find the label and image of that row
R.train[j,1]
view_digit(j,R.train[,-258])

##find what the number was wrongly being classified as
#simulate the process inside function of conducting KNN method
k=1
s= (729*(i-1)+1):(729*i)  # Works!

# subset
dist.s=DistMat_Euc[s,-s] # Works!
lab= R.train[-s,1] 

df = data.frame(dist.s[j, ], lab) # "j" # creat a df which has dist and actual labels
df = df[order(df[ ,1]),] # order by df[1] which is the distance
knn <- head(df, 10) # "k" nearest neighbor
freq.label <-data.frame(table(knn[2]))

#find particular mis-classified row match to which row wrongly
a=which(R.train[,258]==1619)

#find digit and image of that wrongly matched row
view_digit(a,R.train)
R.train[a,1]

#which number are mis-classified for what number of times
table(R.train[postion_false,1])
table(R.train[postion_false,1])/sum(table(R.train[postion_false,1]))


####Question 9####
#function to rbind the two data sets
combine_data <- function(training_points, prediction_points) {
  matrix_together <- (rbind(training_points[, -1], prediction_points[,-1]))
  matrix_together
}
combined_matrix <- combine_data(train, test)

#function to make a combined x combined matrix by calculating the distance between each row
dist_matrix <- function(matrix, distance_metric) {
  final_dist_matrix <- as.matrix(dist(matrix, method= distance_metric))
  final_dist_matrix
}
final_dist_matrix1 <- dist_matrix(combined_matrix, "euclidian")
final_dist_matrix2 <- dist_matrix(combined_matrix, "manhattan")

#function to snippet the distance matrix to only get the relevant part of the combined matrix
snippet <- function(dist_matrix_something) {
  dist_matrix_something <- dist_matrix_something[-(1:nrow(train)), -(nrow(train): nrow(dist_matrix_something))]  #we want a 2007 x 7291 matrix, so we just cut out the regions that aren't relevant
  dist_matrix_something
}
snipped_matrix1 <- snippet(final_dist_matrix1)
snipped_matrix2 <- snippet(final_dist_matrix2)

#function to compute the knn neighbors and misclass error rate
predict_test_knn <- function(final_matrix, training_points, k) {   #predict the knn for specified k and also how which distance metric
  i = nrow(final_matrix)
  final_result = c()
  final_result
  for (i in 1:i){    #similar to question 4 code
    test_row= as.data.frame(final_matrix[i,]) 
    colnames(test_row) <- "Distance"
    test_row$Row_number <-  c(1: nrow(test_row))   #want to get index of each column 
    test_row
    test_row$labels <- training_points$V1[1:nrow(test_row)]
    ordered <- test_row[order(test_row$Distance),]
    ordered
    kth_nearest_neighbors <- head(ordered, k)
    frequency_labels <-data.frame(table(kth_nearest_neighbors$labels))
    frequency_labels
    #ties
    if(length(frequency_labels$Var1[which(frequency_labels$Freq == max(frequency_labels$Freq))]) > 1) {
      final_result[i] <-as.character(sample(frequency_labels$Var1[which(frequency_labels$Freq== max(frequency_labels$Freq))], 1))
    }
    else{
      final_result[i] <-as.character(frequency_labels$Var1[which.max(frequency_labels$Freq)])
    }
    
  }
  final_result
  actual_labels <- test$V1
  compare <- data.frame(final_result, actual_labels)   #make a data frame of the predicted result and the actual results
  for(k in 1: nrow(compare)) {   #go through each row and compare the predicted to the actual
    test_error_rate <- 1- (length(which(compare$final_result== compare$actual_labels))/ (nrow(compare)))   #this is the misclassification error rate
    test_error_rate
  }  
  return(test_error_rate)
}

#do for k= 1:15 and for euclidian
test_error_rates_Euc= numeric(15)
for(k in 1:15) {
  test_error_rates_Euc[k] <- predict_test_knn(snipped_matrix1, train, k)  #run the above function for calculating the error rate for k= 1:15 for Euclidean
  test_error_rates_Euc
}

#do for k= 1:15, and for manhattan
test_error_rates_Man= numeric(15)
for(k in 1:15) {
  test_error_rates_Man[k] <- predict_test_knn(snipped_matrix2, train, k) #run the above function for calculating the error rate for k= 1:15 for Manhattan
  test_error_rates_Man
}

#plot the error rates against k for both distance metrics:
k= 1:15
hi <- data.frame(test_error_rates_Euc, test_error_rates_Man, k)
plot( k, hi$test_error_rates_Euc , xlab= "k", lty= "solid", type= "l", ylab= "Test Set Error Rates", main= "Test Set Error Rates vs. k", col= "blue", ylim= c(0.04, 0.09))
lines(k, hi$test_error_rates_Man , type= "l", lty= "dotted",  col= "green")
legend("bottomright", c("Euclidian", "Manhattan"), col= c("blue", "green"), lty= c("solid", "dotted"))
