#We demonstrate bootstrap resampling techniques using basic R commands
#In this demo we apply Iris dataset
getwd()
Data1<-iris
names(Data1)
levels(Species)
attach(Data1)#Make the variable names available in a current work space
n.v<-15 #Number of vesicolor species to sample
n.vi<-12 #Number of virginica species to sample
n.se<-10#Number of setosa species to sample
B<-10000#Number of itereations (Bootstrap samples to draw)
set.seed(12345)#For reproducability
#The following codes will conduct bootstrap resample for every species with respect to Sepal Length
Bsetosa<-matrix(sample(Sepal.Length[Species=="setosa"],
                       size = n.se*B,replace = TRUE),nrow = n.se,ncol = B)
Bvirginica<-matrix(sample(Sepal.Length[Species=="virginica"],
                          size = n.vi*B,replace = TRUE),nrow = n.vi,ncol=B)
Bvesicolor<-matrix(sample(Sepal.Length[Species=="versicolor"],
                          size = n.v*B,replace = TRUE),nrow=n.v,ncol = B)
Bsetosa[1:5,1:5];Bvirginica[1:5,1:5];Bvesicolor[1:5,1:5]#Observes first 5 rows and columns of our data
Mean.Diff<-colMeans(Bsetosa)-colMeans(Bvirginica)#Diffence in means
Mean.Diff1<-colMeans(Bsetosa)-colMeans(Bvesicolor)#Diffence in means
BootMed<-apply(Bsetosa,2,median)#Compute medians
BootMed[1:5]
quantile(Mean.Diff,probs = c(0.025,0.975))#95% Bootstrap CI for the difference in means

#############Conditions in R###############
#The switch statement in R
V1<-c(23,31,41,29,35,16,32)
option<-"median"
switch(option,
       "mean"=print(mean(V1)),
       "mode"=print(mode(V1)),
       "median"=print(median(V1)),
       "stdv"=print(sd(V1))
       )
#If elsle condition in R
x<-1
if(x>2){
  print("X is greater than 2")
}else{
  print(x)
}
#Repeat loop in R/Exit condition (Not advissable)
X<-2
repeat{
  X=X^2
  print(X)
  if(X>1000)
    break
}
#The while loop in R(Entering condition)
X<-2
while(X<1000){
  X=X^2
  print(X)
}
#The for loop in R (Not so efficient way to work with in R)
V2<-c(1:10)
for(i in V2){
  print(i)
}
#Fibonaci series with for loop and user defined function
fibo<-function(a){
  x1=0
  x2=1
  print(x1)
  print(x2)
  for(i in 1:a){
    x3=x1+x2
    print(x3)
    x1=x2
    x2=x3
  }
}
fibo(10)#call the fibo function to operate with parameter=10
#Simple function in R with no argument
Gretn<-function(){
  "Hellow"
}
 Gretn
#Funtion with one argument
 Gretn1<-function(name){
   paste("Hellow",name)
 }
 Gretn1("Marvel")
 
 #A simple function to compute area of a triangle
 fn1<-function(b,h){
   A=0.5*b*h
   print(A)
 }
fn1(12,20) 

#A function to compute histogram for a power of any normal random variable 
m<-rnorm(100,0,1)
fn3<-function(m,n){
  hist(x=m^n)
}
fn3(m,4)#Calling for any power
#Histogram plot for any normal distributed random variable
fn4<-function(a,b){
  hist(rnorm(1000,a,b),xlab = "Values",main = "Histogram",col = "red",breaks = 50)
}
fn4(100,20)#Calling the function for N(20,8)
#User defined function with additional unnamed urguments
fn5<-function(a,b,...){
  hist(rbinom(1000,a,b),xlab = "values",ylab = "Freq",main = "Histogram",...)
}
fn5(15,0.05,col="red",breaks=10,angle=45)

#Function with logical arguments
fn6<-function(a,b,hist=FALSE,...){
  if(hist==TRUE){
    hist(rnorm(1000,a,b),...)
  }else
  {
    plot(1:length(rnorm(1000,a,b)),rnorm(1000,a,b),...)
  }
  
}
fn6(100,25,hist=FALSE,col="red",xlab="Values",ylab="Freq",main="Histogram",
    breaks=50)
#A function to compute population increase
fn7<-function(a,b){
  i=-1
  while(b>a){
    i=i+1
    a=a*2
  }
  i+(b/a)
}
fn7(10000,40000)
