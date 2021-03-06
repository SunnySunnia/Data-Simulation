
setwd("C:/HS616/Data-Simulation")
#set.seed(10)

generateData = function(N=412){
  age = sample(c(runif(412,16,36)),N,replace = T)
  mVIQ = sample(c(abs(105-rweibull(376,2, 45))+22, rep(NA,(412-376))),N,replace = T)
  mPIQ = sample(c(rnorm(378, 88.5,14.4), rep(NA,(412-378))),N,replace = T)
  mIQ = sample(c((mVIQ+mPIQ)/2), N,replace = T)
  
  gestation = sample(c(45-rweibull(412,1.6,5)),N,replace = T)
  wt_gain= sample(c(rnorm(366,25.0,14.1),rep(NA,(412-366))),N,replace = T)+0.1*gestation
  phe_sd = sample(c(rweibull(408,2,188),rep(NA,(412-408))),N, replace = T)
  phe_avg = sample(c(rweibull(412,1.8,610)),N,replace = T)
  
  birth_len = sample(c(rnorm(406,45.0,3.1),rep(NA,(412-406))),N,replace = T)+0.1*gestation
  head_circum = sample(c(rnorm(403,32.8,2.0),rep(NA,(412-403))),N, replace = T)+0.1*gestation-0.001*phe_avg-2  #birth_wt = sample(c(rweibull(411,2.7,1700)+1389,rep(NA,(412-411))), N,replace = T)+head_circum
  birth_wt = 38*gestation+7*head_circum+41*wt_gain
  #MDI = sample(c(rnorm(283,100.1, 20.3),rep(NA, (412-283))),N,replace = T)-33+head_circum
  MDI = 1.5*head_circum+3*gestation-0.012*phe_avg-56.5
  #PDI = sample(c(rweibull(263, 3,52)+45,rep(NA,(412-263))),N,replace = T)-33+head_circum
  PDI = 1.5*head_circum+2.5*gestation-0.022*phe_avg-40.5
  result = data.frame(age, mVIQ, mPIQ, mIQ, wt_gain, phe_sd,
                      phe_avg, gestation, birth_len, birth_wt,
                      head_circum, MDI, PDI)
  return(result)
}

result = data.frame(age, mVIQ, mPIQ, mIQ, wt_gain, phe_sd,
                    phe_avg, gestation, birth_len, birth_wt,
                    head_circum, MDI, PDI)


wt_related = data.frame(c(sort(wt_gain),rep(NA,sum(is.na(wt_gain)))), 
                        c(sort(birth_wt),rep(NA,sum(is.na(birth_wt)))))
colnames(wt_related)= c("wt_gain","birth_wt")
wt_related= wt_related[sample(1:N,N,replace = F),]
age_related=data.frame(sort(age),c(sort(phe_sd),rep(NA,sum(is.na(phe_sd)))))
colnames(age_related) = c("age", "phe_sd")
age_related= age_related[sample(1:N,N,replace = F),]
phe_related = data.frame(sort(phe_avg,decreasing = T), 
                         c(sort(MDI),rep(NA,sum(is.na(MDI)))), 
                         c(sort(PDI),rep(NA,sum(is.na(PDI)))))
colnames(phe_related)= c("phe_avg", "MDI", "PDI")
phe_related= phe_related[sample(1:N,N,replace = F),]
result = data.frame(age_related$age, mVIQ, mPIQ, mIQ, wt_related$wt_gain,
                    age_related$phe_sd, phe_related$phe_avg, 
                    gestation, birth_len, wt_related$birth_wt,
                    head_circum, phe_related$MDI, 
                    phe_related$PDI)
colnames(result)=c("age", "mVIQ", "mPIQ","mIQ","wt_gain",
                   "phe_sd", "phe_avg", "gestation","birth_len",
                   "birth_wt", "head_circum", "MDI","PDI")



data= generateData(412)
summary(data)

library(reshape2)
library(ggplot2)
b = melt(data)
head(b)
tail(b)
ggplot(b,aes(x = value)) + 
  # comparing diverse rather than similar data, so scales should range freely to display data best
  facet_wrap(~variable,scales = "free") +  # also try scales = "free"
  geom_histogram(fill="pink",bins = 50)

data = mutate(data, age.level=factor(ifelse(data$age<20, "teens", ifelse(data$age<25,"early.20s",ifelse(data$age<30,"late.20s",ifelse(data$age<35,"early.30s","late.30s")))),levels = c("teens", "early.20s","late.20s", "early.30s", "late.30s"), ordered = T))

lm1 = lm(MDI ~ phe_avg, data =data)
summary(lm1)
plot(lm1,1)

full1 = lm(MDI ~ . -PDI-age.level , data =data)
summary(full1)
plot(full1,1)

data = generateData(1000)
mdi_all = lm(MDI~. -PDI-age.level, data = data)
summary(mdi_all)
pdi_all = lm(PDI~ . -MDI-age.level, data = data)
summary(pdi_all)
bwt_all = lm(birth_wt ~ . -MDI -PDI -age.level, data = data)
summary(bwt_all)
plot(bwt_all,1)  
hcir_all = lm(head_circum ~ . -MDI -PDI -age.level, data = data)
summary(hcir_all)
plot(hcir_all,1)

