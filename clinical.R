#Start with Fuquene et al pg.22 (http://ba.stat.cmu.edu/journal/2009/vol04/issue04/fuquene.pdf )




# real data 
datasumm1 = function(a1,b1,a2,b2) {
 A <- log(((a1+.5)/(b1+.5))/
        ((a2+.5)/(b2+.5)))


 B <-  sqrt(1/(a1+.5)+1/(b1+.5) +
         1 /(a2+.5)+1/(b2+.5))
 neff = 4/(B*B)
 
 a = c(A,B,neff)
 a
}

prior = datasumm1(a1,b1,a2,b2)
prior


datasumm = function(a1,b1,a2,b2){
  A <-  log((a1/b1)/(a2/b2)) # logodds ratio
  
  B <- sqrt(1/(a1) +1/(b1) +
           1/(a2 )+1/(b2))           # sdodds ratio
  
  neff = 4/(B*B)
  
  a = c(A,B,neff)
  a
  
}



# Data from prior stuy
a1= 8
b1 =1120
N1 = 1128
a2 = 92
b2 = 1023
N2 =1045


ORstudy = logoddrat(a1,b1,a2,b2)

sdstudy = sdlogoddrat(a1,b1,a2,b2)

equivN = 4/sdstudy^2


priorparams = c(ORstudy,sdstudy,equivN)

# Data from new study
c1= 70
d1 =1042
N3 = 1112
c2 = 135
d2 = 960
N4 = 1095


ORstudy2 = logoddrat(c1,d1,c2,d2)

sdstudy2 = sdlogoddrat(c1,d1,c2,d2)
equivM = 4/sdstudy2^2

realparams = c(ORstudy2,sdstudy2,equivM)




# Manual Calculation from Spieghalter book

lastlogOR = (priorparams[3]*priorparams[1]+realparams[3]*realparams[1])/(priorparams[3]+realparams[3])
lastlogOR
lastSD = 2/sqrt(equivN+equivM)
lastSD
lastlogORCIup=lastlogOR+1.96*lastSD
lastlogORCIlow=lastlogOR - 1.96*lastSD
m  = exp(lastlogORCIup)
n = exp(lastlogORCIlow)
finalOR = c(m,n)
finalOR


library(ClinicalRobustPriors)
Berger.Normal (ORstudy,ORstudy2,sdstudy,sdstudy2,equivN,equivM)
Cauchy.Normal (ORstudy,ORstudy2,sdstudy,sdstudy2,equivN,equivM)


# normal/normal gives sup + inf credible intervals similar to spieghalter priors
# Berger/normal or Cauchy/normal are robust priors.
# robust priors superior to spieghalter priors due to heavy tail.

# use Cauchy if sd study(sdstudy2) more than sd of prior(sdstudy)


# Cauchy.binomial for single arm study (beta,binomial) distribution


# calculating distribution from estimate


x1 = 26
x2 = 97

y1 = 13
y2 = 193

# we assume that the normal, Cauchy, and intrinsicsceptical priors have all mean log(OR)=0 and with a 95% interval running from 50% reduction
#in odds of death (log(OR) = -0.69) to a 50% increase (log(OR) = 0.69).
# Assuming skeptical prior(no information) , odds ratio =1 , conf. interval = 0.5 to 2


log(1)
log(2)
(log(0.8) + log(1.1)) /2

# One way to select the mean (m) and standard deviation (s)of the prior log odds ratio is from previously published data.  
#If such data are not available, one can enumerate the limits of one's belief regarding all possible values of the odds
#ratio, convert these values to logarithms, and compute the mean and standard deviation for a 95% confidence interval
#of the associated Gaussian distribution. Thus, if you thinkthe odds ratio must lie between 0.8 (a 20% relative risk
#reduction) and 1.1 (a 10% relative risk increase), thenm = [ ln(0.8) + ln(1.1) ] / 2 = - 0.064
#ands = [ | ln(0.8) | + | ln(1.1) | ] / 3.92 = 0.081

kaul = function(a,b){
  m = (log(a) + log(b))/2 
  sd = (abs(log(a)) + abs(log(b)))/3.92
  neff = 4/(sd*sd)
  param = c(m,sd,neff)
  param
}

kaul(0.75,1.33)

logoddrat(x2,y2,x1,y1)

log(0.2)
log(0.65)
k = sdlogoddrat(x1,y1,x2,y2)
k
nu = 4/(k*k)
nu

Cauchy.Normal(0,-1.35,0.353,0.349,31.9,32.77)

# OR from sd

ORCI =function(OR,sd)
{
  ORl = log(OR)
  ORCIup= ORl + 1.96*sd
  ORCIlow= ORl - 1.96*sd
  m  = exp(ORCIup)
  n = exp(ORCIlow)
  finalOR = c(m,n)
  finalOR
}

uniformprior = ORCI(1,10)
highskepticprior = ORCI(1,0.4)
moderateskepticprior = ORCI(1,0.07)
mildskepticprior = ORCI(1,0.03)
uninformativeprior = ORCI(1,2)


up = exp(19.6)
low = exp(-19.6)
0.081*0.081
4/0.006561



datasumm(13,193,26,97)


logoddrat(13,193,26,97)

# sceptical prior (OR = 1, log(OR) =0 )
Cauchy.Normal(0,-1.6,0.35,0.36,32.3,30.5)
# Enthusiastic  prior (OR = 0.5, log(OR) = -0.69 )
Cauchy.Normal(-0.69,-1.6,0.35,0.36,32.3,30.5)

log(0.32)

kaul(0.5,2)

# computing posterior probability from odds raio (what percent off odds patients in sceptical prior with at least 50% reduction(logOR = -0.69), when OR = -0.77, scale = 0.25)

# normal skeptical prior
pnorm(-0.69,-0.77,0.25)

pnorm(0.32)
pnorm(4.52)

#optmistic prior
pnorm(0,-1.13,0.25)

# cauchy sceptical
pnorm(-0.69,-1.45,0.38)

# cauchy optimistic
pnorm(-0.69,-1.35,0.39)


post prob = (OR-prior) - OR / sd
kaul()



# odds ratio from baseline rate

logHRlow = log(log(0.2)/log(0.15)) # increase survival from baseline 15% to 20%
logHRhigh = log(log(0.25)/log(0.15)) #  increase survival from baseline 15% to 25%

ORlow = exp(logHRlow)
ORhigh = exp(logHRhigh)
ORlow

log(0.2)/log(0.335)
log(1.47)
log(log(0.15)/log(0.2))
log(log(0.15)/log(0.25))

kaul(0.16,0.31)


