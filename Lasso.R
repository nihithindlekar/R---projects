



library(MASS)  
library(glmnet)
myData = Boston  #read the data
names(myData)[14] = "Y"   


iLog = c(1, 3, 5, 6, 8, 9, 10, 14);


myData[, iLog] = log(myData[, iLog]);


myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;

myData[, 13] = sqrt(myData[, 13]);

X = as.matrix(myData[, -14])
y = myData$Y

sd_y = sd(y)





lam.seq = c(0.30, 0.2, 0.1, 0.05, 0.02, 0.005)


#writing one step lasso function
one_step_lasso = function(r, x, lam){
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) - lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}



#Coordinate Descent function
MyLasso = function(X, y, lam.seq, maxit = 50, standardize = TRUE){
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam.seq: sequence of lambda values
  # maxit: number of updates for each lambda
  # standardize: if True, center and scale X and y. 
  
  n = length(y)  #set number of observations
  p = dim(X)[2]  
  nlam = length(lam.seq)   ##set number of lambda values
  B = matrix(data = NA, nrow = nlam, ncol = p+1)   #set up the matrix
  
  if(standardize==TRUE){
    # Center and scale X and y
    n = length(y)
    X.means = apply(X, 2, mean)
    X.sd = apply(X, 2, sd)*sqrt((n-1)/n)
    # remove mean and divided by sd for each column
    Xs = t((t(X) - X.means)/X.sd)  
    
    y.means = mean(y)
    y.sd = sd(y)*sqrt((n-1)/n)
    ys = (y - y.means)/y.sd 
      
    }
  
  
  # Initilize coef vector b and residual vector r
  b = rep(0, p)
  r = ys
  
  # Triple nested loop
  for(m in 1:nlam){
    lam = (2*n) * lam.seq[m]/(sd_y) # assign lambda value
    for(step in 1:maxit){
      for(j in 1:p){
        
        r = r + (Xs[,j]*b[j])
        b[j] = one_step_lasso(r, Xs[, j], lam)
        r = r - Xs[, j] * b[j]
        
      }
    }
    B[m,-1] = b   #filling up the intercepts
    
  }
  
  if(standardize==TRUE){
    # YOUR CODE
    # scale back the coefficients and update the intercepts B[, 1]
    
    
    for(l in 1:p){
      for(k in 1:nlam){
        B[k,l+1] = B[k,l+1] * sd_y/sd(X[,l])
      }
    }
  for(c in 1:nlam){
    B[c,1] =  mean(y)
    for(d in 2:p+1){
     B[c,1] = B[c,1]  - sum(B[c,d] * mean(X[,d-1]))  #updating intercepts
    }
  }
  }
  return(t(B))
}





#checking the accuracy
#glmnet output
lam.seq = c(0.30, 0.2, 0.1, 0.05, 0.02, 0.005)
lasso.fit = glmnet(X, y, alpha = 1, lambda = lam.seq, standardize = TRUE)
coef(lasso.fit)

#CD output
myout = MyLasso(X, y, lam.seq, maxit = 50, standardize = TRUE)
rownames(myout) = c("Intercept", colnames(X))
myout

#difference
max(abs(coef(lasso.fit) - myout))
