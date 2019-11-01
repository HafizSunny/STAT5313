regression_data = read.table('C:/Users/mhrahman/Desktop/Regression assignmnet/Q01/DataQ1.txt',header=TRUE)
print(colnames(regression_data))
y=matrix(regression_data[,1],ncol = 1)
n=length(y)
x=matrix(1,n,1)
x=cbind(x,regression_data[,2:4])
x=as.matrix(x)
p=ncol(x)-1

#Answer to the Question No. 1<1>

output_information= lm(y~0+x)
beta_hat=matrix(output_information$coefficients,ncol = 1)

#Answer to the Question. 1<2>

y_hat= matrix(output_information$fitted.values, ncol = 1)
e = matrix(output_information$residuals,ncol = 1)
sigmahat_square=(t(e)%*%e)/(n-p-1)
a= matrix(c(0,0,1,0),ncol = 1)
point_estimate= t(a) %*% beta_hat
alpha=0.05
variance_compute= sigmahat_square * (t(a) %*% solve(t(x) %*% x) %*% a)
test_stat= point_estimate/sqrt(variance_compute)
p_value= 2 * (1-pt(abs(test_stat),df= (n-p-1) ))

#Answer to the Question 1<3>

a= matrix(c(0,1,0,-1),ncol = 1)
point_estimate= t(a) %*% beta_hat
alpha=0.05
variance_compute= sigmahat_square * (t(a) %*% solve(t(x) %*% x) %*% a)
conf_int_L = point_estimate - sqrt(variance_compute)*qt(1-alpha/2,df=n-p-1)
conf_int_U = point_estimate - sqrt(variance_compute)*qt( alpha/2, df=n-p-1)

#Answer to the Question 1<4>
a=matrix(c(0,1,1,0))
point_estimate=t(a) %*% beta_hat

#Answer to the Question of 1<5>
q=2
A=matrix(0,q,p+1)
A[1,]=c(0,0,1,0)
A[2,]=c(0,0,0,1)
Dispersion= A %*% solve(t(x) %*% x) %*% t(A)
test_stat=t(A %*% beta_hat) %*% solve(Dispersion) %*% (A %*% beta_hat) / (q*sigmahat_square)
alpha= 0.05
p_value=1-pf(test_stat,df1=q,df2=n-p-1)

#Answer to the Question 1<6>
SSE= t(e) %*% e
SSR=t(y_hat) %*% y_hat - n*(mean(y)^2.0)
TSS=SSR+SSE
MSR=SSR/p
MSE=sigmahat_square
Fstatistic = MSR/MSE
R_square= SSR/TSS
R_square_adjusted = ((n-1)*R_square-p)/(n-p-1)