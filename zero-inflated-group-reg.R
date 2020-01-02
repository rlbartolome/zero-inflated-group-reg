## DOES NOT CONVERGE
library(Gooogle)
library(zic)
library(splines)

data <- read.csv('spider.csv')
n <- nrow(data)

light<-bs(data$Light..Lux.,3)[1:n,]
int<-bs(data$Interception,3)[1:n,]
spider_size<-bs(data$Spider.Size,3)[1:n,]
web_size<-bs(data$Web.size,3)[1:n,]
hours<-bs(data$Hours,3)[1:n,]
att<-bs(data$Attracted,3)[1:n,]
int_rate<-bs(data$Interception.rate,3)[1:n,]
att_rate<-bs(data$Attraction.rate,3)[1:n,]
cap_eff<-bs(data$Capture.efficiency,3)[1:n,]

doc.spline<-cbind.data.frame(data$Number.of.predation.events, light, int, spider_size, web_size,
                             hours, att, int_rate, att_rate, cap_eff, data$Site, data$Lit, 
                             data$Painted)

names(doc.spline)<-c("pred_events", paste("light",1:3,sep=""),
                           paste("int",1:3,sep=""), paste("spider_size",1:3,sep=""), 
                           paste("web_size",1:3,sep=""), paste("hours",1:3,sep=""), 
                           paste("att",1:3,sep=""), paste("int_rate",1:3,sep=""), 
                           paste("att_rate",1:3,sep=""), paste("cap_eff",1:3,sep=""),
                           "site", "lit", "painted")
doc.spline[10:12] <- as.factor(doc.spline[10:12])
data<-doc.spline %>% as_tibble() %>% na.omit()

group=c(rep(1:9,each=3),(10:12))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="negbin",penalty="gBridge")
fit.gooogle

### Okay, but categorical data, yung response is okay sana

data(ships)
ships <- na.omit(ships)
n <- nrow(ships)
service <- bs(ships$accident,3)[1:n,]

spline<-cbind.data.frame(ships$accident, service, ships$op, ships$co.65.69, ships$co.70.74, ships$co.75.79)

names(spline)[1:4]<-c("accident", paste("service",1:3,sep=""))
#spline[5:8] <- as.factor(spline[5:8])

data<-spline

group=c(rep(1:1,each=3),(2:5))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="negbin",penalty="grLasso")

fit.gooogle

### not converging, okay sana dataset

data(lbw)
lbw <- as.data.frame(lbw)
n <- nrow(lbw)
age<-bs(lbw$age,3)[1:n,]
lwt<-bs(lbw$lwt,3)[1:n,]
bwt<-bs(lbw$bwt,3)[1:n,]

spline<-cbind.data.frame(lbw$ptl[1], age, lwt, bwt)
,
                         lbw$smoke[1], lbw$race[1], lbw$low[1], 
                         lbw$ht[1], lbw$ftv[1], lbw$ui[1]) 

names(spline)<-c("died", paste("age",1:3,sep=""), paste("lwt",1:3,sep=""), paste("bwt",1:3,sep=""))
,
                     "smoke", "race", "low", "ht", "ptl", "ui")
data<-spline

group=c(rep(1:3,each=3))

,(3:9))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="poisson",penalty="grLasso")
fit.gooogle



###

data("docvisits")

n<-nrow(docvisits)
age<-bs(docvisits$age,3)[1:n,]
hlth<-bs(docvisits$health,3)[1:n,]
hdeg<-bs(docvisits$hdegree,3)[1:n,]
schl<-bs(docvisits$schooling,3)[1:n,]
hhin<-bs(docvisits$hhincome,3)[1:n,]

attach(docvisits)
doc.spline<-cbind.data.frame(docvisits$docvisits,age,hlth,hdeg,schl,hhin,docvisits$handicap,docvisits$married,
                             docvisits$children,docvisits$self,docvisits$civil,docvisits$bluec,
                             docvisits$employed,docvisits$public,docvisits$addon)

names(doc.spline)[1:16]<-c("docvisits",paste("age",1:3,sep=""),paste("health",1:3,sep=""),paste("hdegree",1:3,sep=""),paste("schooling",1:3,sep=""),paste("hhincome",1:3,sep=""))
data<-doc.spline

group=c(rep(1:5,each=3),(6:14))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="negbin",penalty="gBridge")
fit.gooogle


### BEST FOR NOW, kaya lang yung deaths is not count data. its al

data(azcabgptca)
n <- nrow(azcabgptca)
age<-bs(azcabgptca$age,3)[1:n,]
los<-bs(azcabgptca$los,3)[1:n,]

doc.spline<-cbind.data.frame(azcabgptca$died, age, los,
                             azcabgptca$procedure, azcabgptca$gender, azcabgptca$type) 

names(doc.spline)<-c("died", paste("age",1:3,sep=""), paste("los",1:3,sep=""),
                     "procedure", "gender", "type")
data<-doc.spline

group=c(rep(1:2,each=3),(3:5))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="negbin",penalty="grLasso")
fit.gooogle



### accidents

data("Fatalities")
n <- nrow(Fatalities)
drinkage<-bs(Fatalities$incom,3)[1:n,]
beertax<-bs(Fatalities$beertax,3)[1:n,]

doc.spline<-cbind.data.frame(Fatalities$nfatal1517, drinkage, beertax, Fatalities$breath, Fatalities$jail)

names(doc.spline)<-c("died", paste("drinkage",1:3,sep=""), paste("beertax",1:3,sep=""),
                     "breath", "jail")
data<-doc.spline %>% as_tibble()

group=c(rep(1:2,each=3),(3:4))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="negbin",penalty="grLasso")
fit.gooogle


# PhDPublications %>% head()

data("PhDPublications")
n <- nrow(PhDPublications)
prestige<-bs(PhDPublications$prestige,3)[1:n,]
mentor<-bs(PhDPublications$mentor,3)[1:n,]
kids <- bs(PhDPublications$kids,3)[1:n,]

doc.spline<-cbind.data.frame(PhDPublications$articles, prestige, mentor, kids,
                             PhDPublications$gender, PhDPublications$married)

names(doc.spline)<-c("articles", paste("prestige",1:3,sep=""),
                     paste("mentor",1:3,sep=""), paste("kids",1:3,sep=""),
                     "gender", "married")
doc.spline$gender <- as.numeric(as.factor(doc.spline$gender)) - 1
doc.spline$married <- as.numeric(as.factor(doc.spline$married)) - 1

data<-doc.spline 

group=c(rep(1:3,each=3),(4:5))

yvar<-names(data)[1]
xvars<-names(data)[-1]
zvars<-xvars

#grLasso, grMCP, grSCAD, gBridge

fit.gooogle <- gooogle(data=data,yvar=yvar,xvars=xvars,zvars=zvars,group=group,dist="poisson",penalty="gBridge")
fit.gooogle$coefficients %>% write.csv('sample.csv')

hist(PhDPublications$articles, breaks = 40, main = "Histogram of PhD Publications",
     xlab = "Number of Articles Published")

