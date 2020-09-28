#BC 2020#

results<-read.csv(file.choose(), header=TRUE) #pick csv file called results
adjustments<-read.csv(file.choose(), header=TRUE) #pick adjustment file. They are in percentages.

n=400 # sample size, corresponding to MoE of 4.8% for a party at 40%. Consistent with historical polling accuracy
N=2000 #number of simulations. My computer has no issue with 2000 but takes about 30s for 10k
nr=87 # number of ridings
pr=c(35.5,43.5,12,6.5) #current voting intentions. BC Lib, NDP, Green and Conservatives. No need for 'others'
sumpr=sum(pr)
pr2=c(pr,100-sumpr) # so that it adds to 100% with others

# Randomize at the provincial level
r=100*t(rmultinom(N, n, prob = pr2))/n

s=c(40.4,40.3,16.8,0.5) # past election results at the aggregate level


swing1=(r[,1]-s[1])
swing2=(r[,2]-s[2])
swing3=(r[,3]-s[3])
swing4=(r[,4]-s[4])

swing=cbind(swing1, swing2, swing3, swing4)

proj1=matrix(0, nrow=nr, ncol=N)
proj2=matrix(0, nrow=nr, ncol=N)
proj3=matrix(0, nrow=nr, ncol=N)
proj4=matrix(0, nrow=nr, ncol=N)
proj5=matrix(0, nrow=nr, ncol=N) #independents



for (i in 1:N) {
  proj1[,i]=results[,1]+swing1[i]+adjustments[,1]*100
}
proj1[proj1<0]=0
proj1[proj1>100]=100

for (i in 1:N) {
  proj2[,i]=results[,2]+swing2[i]+adjustments[,2]*100
}
proj2[proj2<0]=0
proj2[proj2>100]=100

for (i in 1:N) {
  proj3[,i]=results[,3]+swing3[i]+adjustments[,3]*100
}
proj3[proj3<0]=0
proj3[proj3>100]=100

for (i in 1:N) {
  proj4[,i]=results[,4]+swing4[i]+adjustments[,4]*100
}
proj4[proj4<0]=0
proj4[proj4>100]=100

for (i in 1:N) {
  proj5[,i]=adjustments[,5]*100
}



# 2nd randomization, at the riding level (because having correct province-wide % isn't everything)

proj1_2=matrix(0, nrow=nr, ncol=N)
proj2_2=matrix(0, nrow=nr, ncol=N)
proj3_2=matrix(0, nrow=nr, ncol=N)
proj4_2=matrix(0, nrow=nr, ncol=N)
proj5_2=matrix(0, nrow=nr, ncol=N)

n2=200 # sample size for the randomnization at the riding level. Yes it's low because a lot of uncertainty exists empirically


for (i in 1:N) {
  for (j in 1:nr) {
    sumproj=proj1[j,i]+proj2[j,i]+proj3[j,i]+proj4[j,i]+proj5[j,i]
    pr3=c(proj1[j,i]/sumproj, proj2[j,i]/sumproj, proj3[j,i]/sumproj, proj4[j,i]/sumproj, proj5[j,i]/sumproj)
    r2=100*t(rmultinom(1, n2, prob = pr3))/n2
    proj1_2[j,i]=r2[1]+0.00001*runif(1) #small random number added to avoid ties
    proj2_2[j,i]=r2[2]+0.00001*runif(1)
    proj3_2[j,i]=r2[3]+0.00001*runif(1)
    proj4_2[j,i]=r2[4]+0.00001*runif(1)
    proj5_2[j,i]=r2[5]+0.00001*runif(1)
  }
}



#wins

projmax=pmax(proj1_2, proj2_2, proj3_2, proj4_2, proj5_2)

win1<-matrix(as.numeric(proj1_2==projmax),nr,N)
win2<-matrix(as.numeric(proj2_2==projmax),nr,N)
win3<-matrix(as.numeric(proj3_2==projmax),nr,N)
win4<-matrix(as.numeric(proj4_2==projmax),nr,N)
win5<-matrix(as.numeric(proj5_2==projmax),nr,N)



#Sum of wins

sumwin1=rowSums(t(win1))
sumwin2=rowSums(t(win2))
sumwin3=rowSums(t(win3))
sumwin4=rowSums(t(win4))
sumwin5=rowSums(t(win5))


#Outcomes

outcome=matrix(0,4,1)
outcome[1]=sum(sumwin1>=44) # Lib majority
outcome[2]=sum(sumwin1<44 & sumwin1>=sumwin2) # Lib plurality
outcome[3]=sum(sumwin1==sumwin2) # Tie
outcome[4]=sum(sumwin2<44 & sumwin2>=sumwin1) # NDP plurality
outcome[5]=sum(sumwin2>=44) # NDP majority

outcome


#Probabilities to win each riding

for (i in 1:N) {
  sumwin1riding=rowSums(win1)
}

for (i in 1:N) {
  sumwin2riding=rowSums(win2)
}

for (i in 1:N) {
  sumwin3riding=rowSums(win3)
}

for (i in 1:N) {
  sumwin4riding=rowSums(win4)
}

for (i in 1:N) {
  sumwin5riding=rowSums(win5)
}

probwinriding=cbind(sumwin1riding, sumwin2riding, sumwin3riding, sumwin4riding, sumwin5riding)*1/N # chances of winning each riding

