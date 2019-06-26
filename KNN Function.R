######Krista Johnson



data3 = data.frame(read.csv("~/Downloads/digitsTrain.csv", header = TRUE))
#randomize the data
randomization = function(data){
  set.seed(57)
  random_nums = sapply(data, sample)
}
random = randomization(data3)

#Calculate the distance matrix outside of the function so that the matrix for each metric is only calculated once
man.dist = as.matrix(dist(random[, -1], method = "manhattan", diag = TRUE, upper = TRUE))
euc.dist = as.matrix(dist(random[, -1], method = "euclidian", diag = TRUE, upper = TRUE))
can.dist = as.matrix(dist(random[, -1], method = "canberra", diag = TRUE, upper = TRUE))

#Use only odd values for k
ks = (seq(3, 99, 2))
knn = function(data, distance, training_range, k){
  #get training set
  training = distance[training_range, -(training_range)]
  #order the set
  ordered = as.matrix(t(apply(training, 1, order)))
  #In order to find the position of the column for the training data we must go through the test data
  nearest = as.matrix(ordered[, 1:k])
  #find the labels for the k nearest neighbors
  knn_labels = t(apply(nearest, 1, function(m) {
    data[m, 1]
  }))
  #See how many counts each label has
  vote = apply(knn_labels, 1, table)
  #find the row of the values that do not have a single value that could be the majority
  preds = as.numeric(unlist(lapply(vote, function(b){
    names(which.max(b))
  })))
  #Check to see how many are correct
  check = sapply(preds, function(q) {q != data[training_range, 1]})
  #Calculate error rate
  error = sum(unlist(check))/length(preds)
}

cross_validate = function(random_dat, distances, k){
  ranges = list(c(1:1000), c(1001:2000), c(2001:3000), c(3001:4000), c(4001:5000))
  knn_ans = lapply(ranges, function(u) {
    knn(random_dat, distances, u, k)})
  first_part_error = sum(as.matrix(unlist(knn_ans)), na.rm = TRUE)
  error = first_part_error/5000
}

manh.ans = sapply(ks, function(t) {
  cross_validate(random, man.dist, t)}) 
eucl.ans = sapply(ks, function(t) {
  cross_validate(random, euc.dist, t)})
canb.ans = sapply(ks, function(t) {
  cross_validate(random, can.dist, t)})

all_models = data.frame(k = ks, euclidian = eucl.ans, manhattan = manh.ans, canberra = canb.ans)

best_model = function(data, k_info){
  #find the values of the errors without the labels
  no_ks = data[,-1]
  find_val = sapply(no_ks, min)
  #find smallest error rate
  colmin = which(find_val %in% min(no_ks))
  #find the row of the smallest error rate
  find_row = sapply(no_ks, which.min)
  rowmin = find_row[colmin]
  #returns the name of the min error rate and the k value corresponding to the row of the min value
  list(names(rowmin), k_info[rowmin])
}
model = best_model(all_models, ks)



#Plot findings
plot(all_models$manhattan, type="o", col= "red", 
     ylim=c(0,1), main = "Plot of Error Rates vs K values for Each Metric", xlab= "K Values", ylab= "Rate")
lines(all_models$euclidian, type="o", pch=22, lty=2, 
      col="blue") #axes=FALSE, ann=FALSE)
lines(all_models$canberra, type="o", pch=23, lty=3, 
      col= "forestgreen")
legend(1, .4, c("manhattan", "euclidian", "canberra"), cex=0.9, col=c("red", "blue", "forestgreen"), 
       pch=21:23, lty=1:2)


