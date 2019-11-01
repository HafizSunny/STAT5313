regression_data = read.table('C:/Users/mhrahman/Desktop/Regression assignmnet/Q02/DataQ2.txt',header=TRUE)
head(regression_data)
print(colnames(regression_data))
y=matrix(regression_data[,2],ncol = 1)
n=length(y)
x=matrix(1,n,1)
x=cbind(x,regression_data[,c(1,4,5,6)])
x=as.matrix(x)
cat_covariate= regression_data[,3]
level_name=levels(cat_covariate)
m=length(level_name)
z= matrix(0,n,(m-1))
for (i in 1:(m-1)) z[,i]=as.numeric(cat_covariate==level_name[i])
x=cbind(x,z)
colnames(x)[6]="Mid_age"
colnames(x)[7]="New"
p=ncol(x)-1

#Answer to the question 2(ii)
output_information=lm(y~0+x)
beta_hat=matrix(output_information$coefficients,ncol = 1)
y_hat=matrix(output_information$fitted.values,ncol = 1)
e=matrix(output_information$residuals,ncol = 1)
sigmahat_square=(t(e) %*% e)/(n-p-1)
q=2
A=matrix(0,q,p+1)
A[1,]=c(0,0,0,0,0,1,0)
A[2,]=c(0,0,0,0,0,0,1)
Dispersion= A %*% solve(t(x) %*% x) %*% t(A)
test_stat=t(A %*% beta_hat) %*% solve(Dispersion) %*% (A %*% beta_hat) / (q*sigmahat_square)
alpha= 0.05
p_value=1-pf(test_stat,df1=q,df2=n-p-1)


# Stepwise regression 

p1= 6
x_new= matrix(1,n,1)
covariate_outside=matrix(0,n,p1)
covariate_outside=x[,2:7]
covariate_name=colnames(covariate_outside[,1:6])
variable_outside=covariate_name
alpha=0.10

#Step1
#Forward Selection
for(i in 1:ncol(covariate_outside)){
  x_n= cbind(x_new,covariate_outside[,i])
  p_new=ncol(x_n)-1
  output_information=lm(y~0+x_n)
  beta_hat=matrix(output_information$coefficients,ncol = 1)
  y_hat=matrix(output_information$fitted.values,ncol = 1)
  e=matrix(output_information$residuals,ncol = 1)
  sigmahat_square_1=(t(e) %*% e)/(n-p_new-1)
  a=matrix(0,nrow = p_new+1,ncol = 1)
  a[p_new+1]=1
  variance_compute=sigmahat_square_1*(t(a) %*% solve(t(x_n) %*% x_n)%*%a)
  test_stat= t(a)%*%beta_hat/sqrt (variance_compute)
  p_value=2*(1-pt(abs(test_stat),df=(n-p_new-1)))
  print(p_value)
}

appropriate_index = 1
x_new = cbind(x_new,covariate_outside[ ,appropriate_index])
covariate_outside = covariate_outside[,-appropriate_index]
variable_inside = variable_outside[appropriate_index]
variable_outside = variable_outside[-appropriate_index]
print(variable_inside)
print(variable_outside)
p = ncol(x_new) - 1 

#Step-2
#Forward Selection
for(i in 1:ncol(covariate_outside)){
  x_n= cbind(x_new,covariate_outside[,i])
  p_new=ncol(x_n)-1
  output_information=lm(y~0+x_n)
  beta_hat=matrix(output_information$coefficients,ncol = 1)
  y_hat=matrix(output_information$fitted.values,ncol = 1)
  e=matrix(output_information$residuals,ncol = 1)
  sigmahat_square_1=(t(e) %*% e)/(n-p_new-1)
  a=matrix(0,nrow = p_new+1,ncol = 1)
  a[p_new+1]=1
  variance_compute=sigmahat_square_1*(t(a) %*% solve(t(x_n) %*% x_n)%*%a)
  test_stat= t(a)%*%beta_hat/sqrt (variance_compute)
  p_value=2*(1-pt(abs(test_stat),df=(n-p_new-1)))
  print(p_value)
}

appropriate_index = 1
x_new = cbind(x_new,covariate_outside[ ,appropriate_index])
covariate_outside = covariate_outside[,-appropriate_index]
variable_inside = variable_outside[appropriate_index]
variable_outside = variable_outside[-appropriate_index]
print(variable_inside)
print(variable_outside)
p = ncol(x_new) - 1 

#Backward Elimination

for (i in 2:(ncol(x_new)-1))
{
  output_information = lm(y~0+x_new)
  beta_hat = matrix(output_information$coefficients,ncol=1)
  e = matrix(output_information$residuals,ncol=1)
  sigmahat_square =(t(e)%*%e)/(n-p-1)
  a = matrix(0,nrow=p+1,ncol=1)
  a[i,1] = 1
  variance_compute = sigmahat_square*(t(a)%*%solve(t(x_new)%*%x_new)%*%a)
  test_stat = t(a)%*%beta_hat/sqrt(variance_compute)
  p_value = 2*(1 - pt( abs(test_stat), df = (n-p-1) ) )
  print(p_value)
}

#Step-3
#Forward Selection
for(i in 1:ncol(covariate_outside)){
  x_n= cbind(x_new,covariate_outside[,i])
  p_new=ncol(x_n)-1
  output_information=lm(y~0+x_n)
  beta_hat=matrix(output_information$coefficients,ncol = 1)
  y_hat=matrix(output_information$fitted.values,ncol = 1)
  e=matrix(output_information$residuals,ncol = 1)
  sigmahat_square_1=(t(e) %*% e)/(n-p_new-1)
  a=matrix(0,nrow = p_new+1,ncol = 1)
  a[p_new+1]=1
  variance_compute=sigmahat_square_1*(t(a) %*% solve(t(x_n) %*% x_n)%*%a)
  test_stat= t(a)%*%beta_hat/sqrt (variance_compute)
  p_value=2*(1-pt(abs(test_stat),df=(n-p_new-1)))
  print(p_value)
}
appropriate_index = 1
x_new = cbind(x_new,covariate_outside[ ,appropriate_index])
covariate_outside = covariate_outside[,-appropriate_index]
variable_inside = variable_outside[appropriate_index]
variable_outside = variable_outside[-appropriate_index]
print(variable_inside)
print(variable_outside)
p = ncol(x_new) - 1 

#Backward Elimination

for (i in 2:(ncol(x_new)-1))
{
  output_information = lm(y~0+x_new)
  beta_hat = matrix(output_information$coefficients,ncol=1)
  e = matrix(output_information$residuals,ncol=1)
  sigmahat_square =(t(e)%*%e)/(n-p-1)
  a = matrix(0,nrow=p+1,ncol=1)
  a[i,1] = 1
  variance_compute = sigmahat_square*(t(a)%*%solve(t(x_new)%*%x_new)%*%a)
  test_stat = t(a)%*%beta_hat/sqrt(variance_compute)
  p_value = 2*(1 - pt( abs(test_stat), df = (n-p-1) ) )
  print(p_value)
}

# Answer to the question 2<iv>
p=ncol(x_new)-1
output_information= lm(y~0+x_new)
beta_hat=matrix(output_information$coefficients,ncol = 1)
y_hat= matrix(output_information$fitted.values, ncol = 1)
e = matrix(output_information$residuals,ncol = 1)
sigmahat_square=(t(e)%*%e)/(n-p-1)

a=matrix(c(0,1,-1,0),ncol = 1)
alpha=0.05
variance_compute= sigmahat_square * (t(a) %*% solve(t(x_new) %*% x_new) %*% a)
test_stat = t(a)%*%beta_hat/sqrt(variance_compute)
p_value = 1 - pt( test_stat, df = (n-p-1))

#Answer to the Question 1<v>
SSE= t(e) %*% e
SSR=(t(y_hat) %*% y_hat) - n*(mean(y)^2.0)
TSS=SSR+SSE
MSR=SSR/p
MSE=sigmahat_square
Fstatistic = MSR/MSE
