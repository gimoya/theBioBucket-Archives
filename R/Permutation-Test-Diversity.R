# File-Name: Permutation-Test-Diversity.R
# Date: 2011-12-28
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: Permutation test to test differences between diversity of 2 samples with abundance data.
# adapted from the PAST Software (Hammer & Harper, http://folk.uio.no/ohammer/past/diversity.html)
# Packages Used: vegan
# Licence: CC BY-NC-SA

require(vegan)
 
### two samples with abundance data:
### 1st sample of counts (numbers representing species 1-15)
dat1 <- c(1,2,0,3,0,1,4,20,0,2,21,3,15,23,30)
 
### 2nd sample of counts (numbers representing species 1-15)
dat2 <- c(0,1,0,1,0,0,1,10,0,0,29,1,22,30,25)
  
(div1=diversity(t(dat1),"shannon"))
(div2=diversity(t(dat2),"shannon"))
(rich1=sum(dat1>0))
(rich2=sum(dat2>0))
  
(tr.diff.div=abs(div1-div2)) ### observed difference
(tr.diff.rich=abs(rich1-rich2)) ### ...
  
K=2000
  
pop.diff.div <- pop.diff.rich <- rep(NA,K) ### dataframe for null population of differences
pop.diff.div[1]=tr.diff.div
pop.diff.rich[1]=tr.diff.rich
  
for(i in 2:K){ ### loop to generate null pop.diff. of differences
  
  ind1<-sum(dat1) ### sum of individuals sample no.1
  ind2<-sum(dat2) ### sum of ... no.2
  pool<-c(rep(1:length(dat1),dat1), ### pooled sample with numbers representing species
          rep(1:length(dat2),dat2)) ### replicated as often as no. of individuals
  
  temp1=sample(pool,ind1,replace=T) ### resample no.1
  temp2=sample(pool,ind2,replace=T) ### resample no.2
  
  ### calculate diversity:
  div1.temp=diversity(t(tabulate(temp1)),"shannon")
  div2.temp=diversity(t(tabulate(temp2)),"shannon")
  
  rich1.temp=sum(tabulate(temp1)>0)
  rich2.temp=sum(tabulate(temp2)>0)
  
  pop.diff.div[i]=abs(div1.temp-div2.temp)
  pop.diff.rich[i]=abs(rich1.temp-rich2.temp)
}
  
(p.div=sum(pop.diff.div>=abs(tr.diff.div))/K)
(p.rich=sum(pop.diff.rich>=abs(tr.diff.rich))/K)
 
### diagramms to show null-distributions with obs. differences
par(mfrow=c(2,1))
hist(pop.diff.div)
abline(v=tr.diff.div,lty=3,col=2,lwd=2)

hist(pop.diff.rich)
abline(v=tr.diff.rich,lty=3,col=2,lwd=2)