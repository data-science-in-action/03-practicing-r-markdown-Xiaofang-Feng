# The true value for comparison
library(tidyverse)
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)
x=pnorm( t, mean = 0, sd = 1)

n1=10^2
z1=c(rep(0,9))
w1=matrix(0,9,n1)
y1=c(rnorm(n1,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n1)
  {w1[k,j]=sign(y1[j]<=t[k])}
  z1[k]=sum(w1[k,])/n1}

n2=10^3
z2=c(rep(0,9))
w2=matrix(0,9,n2)
y2=c(rnorm(n2,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n2)
  {w2[k,j]=sign(y2[j]<=t[k])}
  z2[k]=sum(w2[k,])/n2}


n3=10^4
z3=c(rep(0,9))
w3=matrix(0,9,n3)
y3=c(rnorm(n3,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n3)
  {w3[k,j]=sign(y3[j]<=t[k])}
  z3[k]=sum(w3[k,])/n3}


tb<-tibble(
  t=t,
  true=x,
  value1=z1,
  value2=z2,
  value3=z3
)

knitr::kable(head(tb), booktabs = TRUE,
             caption = 'The true value for comparison')
			 
			 

# When n=10^2
x=pnorm(  c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72), mean = 0, sd = 1)
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)

n=10^2
z=matrix(0,100,9)
w=matrix(0,9,n)

for(p in 1:100)
{ y=c(rnorm(n,mean=0,sd=1))
  for(k in 1:9)
  {
   for(j in 1:n)
  {w[k,j]=sign(y[j]<=t[k])}
z[p,k]=sum(w[k,])/n}}
z=as.data.frame(z)
r=c(z$V1,z$V2,z$V3,z$V4,z$V5,z$V6,z$V7,z$V8,z$V9)
e=c(rep(0.0,100),rep(0.67,100),rep(0.84,100),rep(1.28,100),rep(1.65,100),rep(2.32,100),rep(2.58,100),rep(3.09,100),rep(3.72,100))
q=data.frame(T=rep(0,100),X=0)
for(s in 1:900)
{q[s,2]=r[s]}
for(s in 1:900)
{q[s,1]=e[s]}

for(a in 1:100)
 { q[a,2]=q[a,2]-x[1]
 q[a+100,2]=q[a+100,2]-x[2]
 q[a+200,2]=q[a+200,2]-x[3]
 q[a+300,2]=q[a+300,2]-x[4]
 q[a+400,2]=q[a+400,2]-x[5]
 q[a+500,2]=q[a+500,2]-x[6]
 q[a+600,2]=q[a+600,2]-x[7]
 q[a+700,2]=q[a+700,2]-x[8]
 q[a+800,2]=q[a+800,2]-x[9]}
library(ggplot2)
ggplot(q,aes(T,X,group=T)) + geom_boxplot()
# When n=10^3
x=pnorm(  c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72), mean = 0, sd = 1)
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)

n=10^3
z=matrix(0,100,9)
w=matrix(0,9,n)

for(p in 1:100)
{ y=c(rnorm(n,mean=0,sd=1))
  for(k in 1:9)
  {
   for(j in 1:n)
  {w[k,j]=sign(y[j]<=t[k])}
z[p,k]=sum(w[k,])/n}}
z=as.data.frame(z)
r=c(z$V1,z$V2,z$V3,z$V4,z$V5,z$V6,z$V7,z$V8,z$V9)
e=c(rep(0.0,100),rep(0.67,100),rep(0.84,100),rep(1.28,100),rep(1.65,100),rep(2.32,100),rep(2.58,100),rep(3.09,100),rep(3.72,100))
q=data.frame(T=rep(0,100),X=0)
for(s in 1:900)
{q[s,2]=r[s]}
for(s in 1:900)
{q[s,1]=e[s]}

for(a in 1:100)
 { q[a,2]=q[a,2]-x[1]
 q[a+100,2]=q[a+100,2]-x[2]
 q[a+200,2]=q[a+200,2]-x[3]
 q[a+300,2]=q[a+300,2]-x[4]
 q[a+400,2]=q[a+400,2]-x[5]
 q[a+500,2]=q[a+500,2]-x[6]
 q[a+600,2]=q[a+600,2]-x[7]
 q[a+700,2]=q[a+700,2]-x[8]
 q[a+800,2]=q[a+800,2]-x[9]}
library(ggplot2)
ggplot(q,aes(T,X,group=T)) + geom_boxplot()
# When n=10^4
x=pnorm(  c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72), mean = 0, sd = 1)
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)

n=10^4
z=matrix(0,100,9)
w=matrix(0,9,n)

for(p in 1:100)
{ y=c(rnorm(n,mean=0,sd=1))
  for(k in 1:9)
  {
   for(j in 1:n)
  {w[k,j]=sign(y[j]<=t[k])}
z[p,k]=sum(w[k,])/n}}
z=as.data.frame(z)
r=c(z$V1,z$V2,z$V3,z$V4,z$V5,z$V6,z$V7,z$V8,z$V9)
e=c(rep(0.0,100),rep(0.67,100),rep(0.84,100),rep(1.28,100),rep(1.65,100),rep(2.32,100),rep(2.58,100),rep(3.09,100),rep(3.72,100))
q=data.frame(T=rep(0,100),X=0)
for(s in 1:900)
{q[s,2]=r[s]}
for(s in 1:900)
{q[s,1]=e[s]}

for(a in 1:100)
 { q[a,2]=q[a,2]-x[1]
 q[a+100,2]=q[a+100,2]-x[2]
 q[a+200,2]=q[a+200,2]-x[3]
 q[a+300,2]=q[a+300,2]-x[4]
 q[a+400,2]=q[a+400,2]-x[5]
 q[a+500,2]=q[a+500,2]-x[6]
 q[a+600,2]=q[a+600,2]-x[7]
 q[a+700,2]=q[a+700,2]-x[8]
 q[a+800,2]=q[a+800,2]-x[9]}
library(ggplot2)
ggplot(q,aes(T,X,group=T)) + geom_boxplot()
