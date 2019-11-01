regression_data = read.table('C:/Users/mhrahman/Desktop/Regression assignmnet/Q03/DataQ03.txt',header=TRUE)
head(regression_data)
y=regression_data[,1]
n=length(y)
p=6
x=matrix(0,n,p+1)
x[,1]=1
x[,2:(p+1)]=as.matrix(regression_data[,c(2:7)])
cat_covariate=regression_data[,8]
level_name=levels(cat_covariate)
m=length(level_name)
z=matrix(0,n,(m-1))
for (i in 1:(m-1)) {
  z[,i]=as.numeric(cat_covariate==level_name[i])
}
x=cbind(x,z)
for (j in 2 :(p+1)) {
  (x[,j]-min(x[,j]))/(max(x[,j])-min(x[,j]))
}

# Answer to question 3<a>
x_full=x
p=p+m-1

# Answer to question 3<b>

eigenvalues_total= eigen(t(x) %*% x)$values
eignevectors_total=eigen(t(x) %*% x)$vectors
condition_indices=max(eigenvalues_total)/eigenvalues_total
large_CI_positions = which(condition_indices > 1000)
m = length(large_CI_positions)
large_CI_eigenvector = matrix(eignevectors_total[,c(8,9)],ncol=m)
for (i in 2:(p+1)) {
  reg_output =lm(x[,i]~x[,-c(1,i)])
  R_square=summary.lm(reg_output)$r.squared
  print(1/(1-R_square))
}
position_of_thelargest_VIF=5
x_new=x[,-c(position_of_thelargest_VIF+1)]
p=ncol(x_new)-1

eigenvalues_total= eigen(t(x_new) %*% x_new)$values
eignevectors_total=eigen(t(x_new) %*% x_new)$vectors
condition_indices=max(eigenvalues_total)/eigenvalues_total
large_CI_positions = which(condition_indices > 1000)
m = length(large_CI_positions)
large_CI_eigenvector = matrix(eignevectors_total[,8],ncol=m)
for (i in 2:(p+1)) {
  reg_output =lm(x_new[,i]~x_new[,-c(1,i)])
  R_square=summary.lm(reg_output)$r.squared
  print(1/(1-R_square))
}
position_of_thelargest_VIF=5
x_new=x_new[,-c(position_of_thelargest_VIF+1)]
p=ncol(x_new)-1

eigenvalues_total= eigen(t(x_new) %*% x_new)$values
eignevectors_total=eigen(t(x_new) %*% x_new)$vectors
condition_indices=max(eigenvalues_total)/eigenvalues_total
large_CI_positions = which(condition_indices > 1000)
m = length(large_CI_positions)

#Answer to the Question 3<c>
output_information=lm(y~0+x_new)
beta_hat=matrix(output_information$coefficients,ncol = 1)
y_hat=matrix(output_information$fitted.values,ncol = 1)
e=matrix(output_information$residuals,ncol = 1)
sigmahat_square=(t(e) %*% e)/(n-p-1)


a=matrix(c(0,1,0,-1,0,0,0),ncol = 1)
point_estimate=t(a) %*% beta_hat
alpha=0.05
variance_compute= sigmahat_square * (t(a) %*% solve(t(x_new) %*% x_new) %*% a)
conf_int_L = point_estimate - sqrt(variance_compute)*qt(1-alpha/2,df=n-p-1)
conf_int_U = point_estimate - sqrt(variance_compute)*qt( alpha/2, df=n-p-1)

#Answer to the question no. 3<d>
a=matrix(c(0,0,1,0,-1,0,0),ncol = 1)
alpha=0.05
variance_compute= sigmahat_square * (t(a) %*% solve(t(x_new) %*% x_new) %*% a)
test_stat = t(a)%*%beta_hat/sqrt(variance_compute)
p_value = 1 - pt( test_stat, df = (n-p-1))