
Independency <- function(k){
  row = rowSums(k)
  col = colSums(k)
  n = sum(row)
  r = length(row)
  s = length(col)
  sumR = 0;
  for (i in 1:r){
    sumS = 0;
    for (j in 1:s){
      sumS = sumS + (((k[i,j] - (row[i] * col[j]) / n)^2) / (row[i] * col[j]))
    }
    sumR = sumR + sumS;
  }
  Khi2 = n * sumR;
  cat('result: ', Khi2, '\n');
}

inputMatrix <- matrix(c(42,28,3,17,89,21), nrow = 2, ncol = 3, byrow = TRUE)

print(inputMatrix)

Independency(inputMatrix)