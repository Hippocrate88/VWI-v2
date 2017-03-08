#####################################
# Vessel wall enhancement           #
# Analysis started December 5, 2016 #
#####################################

# Installing necessary packages - One time thing
install.packages('Hmisc')
install.packages('pastecs')
install.packages('gmodels')
install.packages('Quartz')
install.packages('nnet')

# Loading necessary packages - every time you start a session
library(Hmisc)
library(pastecs)
library(gmodels)
library(Quartz)
library(nnet)

#set directory
setwd("~/Dropbox/My Research/Research projects/VWI v2")
setwd ("~/Dropbox/SAH vessel wall enhancement/data.csv")

# Uploading data
data_vwi<- read.csv("datav4.0.csv") # Sam
data_vwi <- read.csv("~/Dropbox/SAH vessel wall enhancement/data.csv") # Guido



# View dataset
View(data_vwi)

# Decriptive stats - Continuous covariates
stat.desc(data_vwi$)

# Decriptive stats - Discrete covariates
CrossTable(data_sah$Patient..ASA)


### before we start we create the needed column for analysis

# multiple aneurysms

data_vwi$multipleaneurysm [data_vwi$Total...aneurysms >1] <- 1
data_vwi$multipleaneurysm [data_vwi$Total...aneurysms ==1] <- 0

#MERGE ASA+PLAVIX --> anti-platelet
data_vwi$antiplat <- rowSums(data_vwi[, c("ASA", "Plavix")], na.rm = T)
data_vwi$antiplat[data_vwi$antiplat==2] <-1

#subgroups of Vessel Wall Enhancement

data_vwi0<-data_vwi[data_vwi$A1_vwi=="0",] ## subsetting no enhancement 

data_vwi1<-data_vwi[data_vwi$A1_vwi=="1",] ## subsetting enhancement
d1<-data_vwi1 ## subset shortcut

data_vwi1thin<-data_vwi[data_vwi$A1_vwi_val=="1",] ## subsetting thin enhancement
dthin<-data_vwi1thin ## subset shortcut


data_vwi1thick<-data_vwi[data_vwi$A1_vwi_val=="2",] ## subsetting thick enhancement
dthick<-data_vwi1thick ## subset shortcut

## Merging no and thin

data_vwi$THINmergeNOvsTHICK[data_vwi$A1_vwi_val==0]<-0
data_vwi$THINmergeNOvsTHICK[data_vwi$A1_vwi_val==1]<-0
data_vwi$THINmergeNOvsTHICK[data_vwi$A1_vwi_val==2]<-1
data_vwi$THINmergeNOvsTHICK = as.factor(data_vwi$THINmergeNOvsTHICK)

### Restriction to <7mm
less7mmA1<- subset(data_vwi, data_vwi$A1_size <7)


## ANALYSIS



##### ALL PATIENTS #######


CrossTable(data_vwi$A1_vwi)       # Enhancement Yes/No
CrossTable(data_vwi$A1_vwi_val)   # Enhancement Thick/Thin/None

# Demographics

stat.desc(data_vwi$AODiscovery)   #Age 
CrossTable(data_vwi$Sex)          #Sex
CrossTable(data_vwi$Hosp)         #Hospitalization

# Comorbidities

CrossTable(data_vwi$HTN)              #Hypertension
CrossTable(data_vwi$DM)               #Diabetes
CrossTable(data_vwi$DLP)              #Dyslipidemia
CrossTable(data_vwi$Smoking.Status)   #Smoking status
stat.desc(data_vwi$BMI)               #BMI
CrossTable(data_vwi$Migraine)         #Migraine
CrossTable(data_vwi$Afib)             #Atrial fibrillation
CrossTable(data_vwi$CAD)              #Coronary arterial disease
CrossTable(data_vwi$Sleep_apnea)      #Sleep apnea
CrossTable(data_vwi$FamH)             #Positive family history of SAH or Aneurysm
CrossTable(data_vwi$anxdep)           #Anxiety/Depression

#Medication
CrossTable(data_vwi$ASA)              # Aspirine
CrossTable(data_vwi$ASA_post)         # Aspirine post treatment of aneurysm
CrossTable(data_vwi$Plavix)           # Plavix
CrossTable(data_vwi$antiplat)         # Antiplatelet


#Clinical details

CrossTable(data_vwi$Grouping)           # Clinical presentation 
CrossTable(data_vwi$Total...aneurysms)  # Total number of aneurysms
CrossTable(data_vwi$multipleaneurysm)   # Multiple aneurysms
CrossTable(data_vwi$A1_loc)             # Anterior location
stat.desc(data_vwi$A1_size)             # size continuous [mm]
CrossTable(data_vwi$A1_size7)           # size less than 7 mm
CrossTable(data_vwi$A1_TTT)             # Treatment modality


########################





##### NO ENHANCEMENT PATIENT ###### 

data_vwi0<-data_vwi[data_vwi$A1_vwi=="0",]

# Demographics

stat.desc(data_vwi0$AODiscovery)    #Age 
CrossTable(data_vwi0$Sex)           #Sex
CrossTable(data_vwi0$Hosp)          #Hospitalization


# Comorbidities

CrossTable(data_vwi0$HTN)             #Hypertension
CrossTable(data_vwi0$DM)              #Diabetes
CrossTable(data_vwi0$DLP)             #Dyslipidemia
CrossTable(data_vwi0$Smoking.Status)  #Smoking
stat.desc(data_vwi0$BMI)              #BMI
CrossTable(data_vwi0$Migraine)        #Migraine
CrossTable(data_vwi0$Afib)            #Afib
CrossTable(data_vwi0$CAD)             #CAD
CrossTable(data_vwi0$Sleep_apnea)     #Sleep apnea
CrossTable(data_vwi0$FamH)            #Positive family history for SAH or Aneurysm
CrossTable(data_vwi0$anxdep)       

#Medication

CrossTable(data_vwi0$ASA)             #Aspirine
CrossTable(data_vwi0$ASA_post)        #ASA_post
CrossTable(data_vwi0$Plavix)          #Plavix
CrossTable(data_vwi0$antiplat)        #Antiplat

#Clinical details

CrossTable(data_vwi0$Grouping)          # Clinical presentation
CrossTable(data_vwi0$Total...aneurysms) # Total number of aneurysms
CrossTable(data_vwi0$multipleaneurysm)  # Multiple aneurysms
CrossTable(data_vwi0$A1_loc)            # Anterior location
stat.desc(data_vwi0$A1_size)            # Size continous [mm]
CrossTable(data_vwi0$A1_size7)          # size less than 7 mm
CrossTable(data_vwi0$A1_TTT)            # Treatment modality



#######################################



##### ENHANCEMENT PATIENT ########


data_vwi1<-data_vwi[data_vwi$A1_vwi=="1",] ## subsetting enhancement 
d1<-data_vwi1 ## subset shortcut

# Demographics

stat.desc(data_vwi1$AODiscovery)    #Age
CrossTable(data_vwi1$Sex)           #Sex
CrossTable(data_vwi1$Hosp)          #Hospitalization


# Comorbidities

CrossTable(data_vwi1$HTN)            # Hypertension
CrossTable(data_vwi1$DM)             # Diabetes
CrossTable(data_vwi1$DLP)            # Dyslipidemia
CrossTable(data_vwi1$Smoking.Status) # Smoking
stat.desc(data_vwi1$BMI)             # BMI
CrossTable(data_vwi1$Migraine)       # Migraine 
CrossTable(data_vwi1$Afib)           # Atrial Fibrillation
CrossTable(data_vwi1$CAD)            # Coronary arterial disease
CrossTable(data_vwi1$Sleep_apnea)    # Sleep apnea
CrossTable(data_vwi1$FamH)           # Positive family history of SAH or aneurysm
CrossTable(data_vwi1$anxdep)        

#Medication
CrossTable(data_vwi1$ASA)             # Aspirine
CrossTable(data_vwi1$ASA_post)        # Aspirine post treatment
CrossTable(data_vwi1$Plavix)          # Plavix
CrossTable(data_vwi1$antiplat)        # Antiplatelet


#Clinical details

CrossTable(data_vwi1$Grouping)          # Clinical presentation
CrossTable(data_vwi1$Total...aneurysms) # Total number of aneurysms
CrossTable(data_vwi1$multipleaneurysm)  # Multiple aneurysm
CrossTable(data_vwi1$A1_loc)            # Anterior location
stat.desc(data_vwi1$A1_size)            # Size continous [mm]
CrossTable(data_vwi1$A1_size7)          # Size less than 7mm
CrossTable(data_vwi1$A1_TTT)            # Treatment modality

####################################

#Enhancement thin

data_vwi1thin<-data_vwi[data_vwi$A1_vwi_val=="1",] ## subsetting thin enhancement
dthin<-data_vwi1thin ## subset shortcut

# Demographics

stat.desc(dthin$AODiscovery)        # Age
CrossTable(dthin$Sex)               # Sex
CrossTable(dthin$Hosp)              # Hospitalization


# Comorbidities

CrossTable(dthin$HTN)           
CrossTable(dthin$DM)            
CrossTable(dthin$DLP)            
CrossTable(dthin$DM)             
CrossTable(dthin$Smoking.Status)
stat.desc(dthin$BMI)            
CrossTable(dthin$Migraine)      
CrossTable(dthin$Afib)          
CrossTable(dthin$CAD)           
CrossTable(dthin$Sleep_apnea)    
CrossTable(dthin$FamH)           
CrossTable(dthin$anxdep)        

#Medication
CrossTable(dthin$ASA)           
CrossTable(dthin$ASA_post)      
CrossTable(dthin$Plavix)         
CrossTable(dthin$antiplat)


#Clinical details

CrossTable(dthin$Grouping)    
CrossTable(dthin$Total...aneurysms)
CrossTable(data_vwi1$multipleaneurysm)
CrossTable(dthin$A1_loc)      
stat.desc(dthin$A1_size)      
CrossTable(dthin$A1_size7)    
CrossTable(dthin$A1_TTT)       
CrossTable(dthin$A1_dys)


#############################


#Enhancement thick

data_vwi1thick<-data_vwi[data_vwi$A1_vwi_val=="2",] ## subsetting thick enhancement
dthick<-data_vwi1thick ## subset shortcut


# Table 1 
CrossTable(dthick$Sex)            
stat.desc(dthick$AODiscovery)     
CrossTable(dthick$Hosp)         


# Demographics
CrossTable(dthick$HTN)           
CrossTable(dthick$DM)            
CrossTable(dthick$DLP)            
CrossTable(dthick$Smoking.Status)
stat.desc(dthick$BMI)            
CrossTable(dthick$Migraine)      
CrossTable(dthick$Afib)          
CrossTable(dthick$CAD)           
CrossTable(dthick$Sleep_apnea)    
CrossTable(dthick$FamH)           
CrossTable(dthick$anxdep)        

#Medication
CrossTable(dthick$ASA)           
CrossTable(dthick$ASA_post)      
CrossTable(dthick$Plavix)     
CrossTable(dthick$antiplat)


#Clinical details

CrossTable(dthick$Grouping)    
CrossTable(dthick$Total...aneurysms)
CrossTable(data_vwi0$multipleaneurysm)
CrossTable(dthick$A1_loc)      
stat.desc(dthick$A1_size)      
CrossTable(dthick$A1_size7)    
CrossTable(dthick$A1_TTT)       
CrossTable(dthick$A1_dys)






##################################################


##Univariate analysis generalize linear regression 



######## Univariate Analysis ##########


### ENHANCEMENT VS. NON ENHANCEMENT ###

# Age

glm.age <- glm(A1_vwi~ AODiscovery, data_vwi, family = "binomial")
summary(glm.age)
confint(glm.age)


# Sex

data_vwi$Sex = as.factor(data_vwi$Sex)

glm.sex <- glm (A1_vwi~Sex, data_vwi, family = "binomial")
summary(glm.sex)
confint(glm.sex)


# Hospitalization

data_vwi$Hosp = as.factor(data_vwi$Hosp)

glm.hosp<- glm(A1_vwi~Hosp, data_vwi, family = "binomial")      #### Hospitalisation OR 1.3 P value 0.0075
summary(glm.hosp)
confint(glm.hosp)



# HTA

data_vwi$HTN = as.factor(HTN)

glm.hta<-glm(A1_vwi~HTN, data_vwi, family = "binomial")
summary(glm.hta)
confint(glm.hta)

  
# DM

data_vwi$DM = as.factor(data_vwi$DM)

glm.dm<-glm(A1_vwi~DM, data_vwi, family = "binomial")
summary(glm.dm)
confint(glm.dm)


# Dyslipidemia

data_vwi$DLP=as.factor(data_vwi$DLP)

glm.dlp<-glm(A1_vwi~DLP, data_vwi, family = "binomial")
summary(glm.dlp)
confint(glm.dlp)


# Smoking

data_vwi$Smoking.Status=as.factor(data_vwi$Smoking.Status)

glm.smoke<-glm(A1_vwi~Smoking.Status, data_vwi, family = "binomial")
summary(glm.smoke)
confint(glm.smoke)


# BMI

glm.bmi <- glm(A1_vwi ~ BMI, data_vwi, family = "binomial")
summary(glm.bmi)
confint(glm.bmi)


# Migraine

data_vwi$Migraine = as.factor(data_vwi$Migraine)

glm.migraine<- glm(A1_vwi~Migraine, data_vwi, family = "binomial")
summary(glm.migraine)
confint(glm.migraine)


# Afib

data_vwi$Afib = as.factor(data_vwi$Afib)

glm.afib<- glm(A1_vwi~Afib, data_vwi, family = "binomial")
summary(glm.afib)
confint(glm.afib)

# CAD

data_vwi$CAD = as.factor(data_vwi$CAD)

glm.cad<-glm(A1_vwi~CAD, data_vwi, family = "binomial")
summary(glm.cad)
confint(glm.cad)

# Sleep apnea

data_vwi$Sleep_apnea = as.factor(data_vwi$Sleep_apnea)

glm.sleepapnea <- glm(A1_vwi~Sleep_apnea, data_vwi, family = "binomial")
summary(glm.sleepapnea)
confint(glm.sleepapnea)


# Family history

data_vwi$FamH = as.factor(data_vwi$FamH)

glm.FamH <- glm(A1_vwi~FamH, data_vwi, family = "binomial")
summary(glm.FamH)
confint(glm.FamH)


# Anxiety depression

data_vwi$anxdep = as.factor(data_vwi$anxdep)

glm.anxdep<- glm(A1_vwi~anxdep, data_vwi, family = "binomial")
summary(glm.anxdep)
confint(glm.anxdep)


# Anti-platelet

data_vwi$antiplat = as.factor(data_vwi$antiplat)

glm.antiplat <- glm(A1_vwi ~ antiplat, data_vwi, family = "binomial" )
summary(glm.antiplat)
confint(glm.antiplat)

# Clinical prensentation

data_vwi$Grouping = as.factor(data_vwi$Grouping)

glm.group<- glm(A1_vwi~Grouping, data_vwi, family = "binomial")
summary(glm.group)
confint(glm.group)

# Size

  # size discrete (<7mm)

data_vwi$A1_size7 = as.factor(data_vwi$A1_size7)

glm.size7<- glm(A1_vwi~A1_size7, data_vwi, family = "binomial")  ### >7mm OR 1.5 P Value 3.8E-E05
summary(glm.size7)
confint(glm.size7)

  # size continuous 

glm.size <- glm(A1_vwi~A1_size, data_vwi, family = "binomial") ##continous size
summary(glm.size)
confint(glm.size)

  # continuous size intragroup <7mm

less7mmA1<- subset(data_vwi, data_vwi$A1_size <7)

glm.sizeifonlylessthan7mm <- glm(A1_vwi ~ A1_size, less7mmA1, family = "binomial")
summary(glm.sizeifonlylessthan7mm)  
confint(glm.sizeifonlylessthan7mm)  



#Localization#

data_vwi$A1_loc = as.factor(data_vwi$A1_loc)

glm.location<- glm(A1_vwi~A1_loc, data_vwi, family = "binomial")
summary(glm.location)
confint(glm.location)

str(data_vwi$A1_loc)

### Multiple aneurysm

data_vwi0$multipleaneurysm = as.factor(data_vwi0$multipleaneurysm)

glm.mult.a <- glm(A1_vwi ~ multipleaneurysm, data_vwi, family = "binomial")
summary(glm.mult.a)
confint(glm.mult.a)


#Treatment#

data_vwi$A1_TTT = as.factor(data_vwi$A1_TTT)

glm.ttt<-glm(A1_vwi~A1_TTT-1, data_vwi, family = "binomial")
summary(glm.ttt)
confint(glm.ttt)




### MULTIVARIATE ANALYSIS### 


# Multivariable analysis: model building

# Step 1: Include covariates with P<0.2 in univariable analysis

# Step 2: force into the model universal confounders

# Step 3: force covariates into the model based on expert knowledge

# Step 4: remove covariates with p<0.1 in multivariable analysis

# Step 5: assess for colinearity based on variance inflation factor


mod1  <- glm(A1_vwi ~ Sex + HTN + Hosp + Migraine + DLP+ A1_loc + Grouping + antiplat  + A1_size  , data_vwi, family = "binomial")

summary(mod1)


### correction with universal confounders and selection p<0.2 #### 

mod2 <- glm(A1_vwi ~ AODiscovery + Sex + HTN + Grouping + A1_size , data_vwi, family = "binomial")

summary(mod2)

### sensitivity analysis

mod2b <- glm(A1_vwi ~ AODiscovery + Sex + HTN + Grouping + A1_size7 , data_vwi, family = "binomial")
mod2c <- glm(A1_vwi ~ AODiscovery + Sex + HTN + Grouping + A1_size , less7mmA1, family = "binomial")

summary(mod2b)
summary(mod2c)

mod2d <- glm(THINmergeNOvsTHICK ~  AODiscovery + Sex + HTN + Grouping + A1_size , data_vwi, family = "binomial")
mod2e <- glm(THINmergeNOvsTHICK ~  AODiscovery + Sex + Grouping + A1_size , less7mmA1, family = "binomial")
mod2f <- glm(THINmergeNOvsTHICK ~  AODiscovery + Sex + Grouping + A1_size , less7mmA1, family = "binomial")

summary(mod2d)
summary(mod2e)



### FIGURES



# Figures
hist(data_vwi$)                      ## histogramms
localisation<-factor(data_vwi$Localisation_A1)  ## need to transform Localisation in factor
plot(localisation)                              ## plotting 
boxplot(data_vwi, data_vwi$) ## How to do it?
boxplot(data_vwi$A1_vwi~data_vwi$Grouping)

par(mar=c(5,5,3,3)) 

#### good margin but might be augmented if needed

plot(glm.group$coefficients~c("G1","G2","G3"), bty='n', cex=2, col="red", pch=16)
### bty box around the plot and suppress it
### cex size of point
### col color of point
### pch is the size of points

#### HISTOGRAM FIG.1

original.parameters <- par()
par(xaxt="n")
par(yaxt="n")
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)


hist(data_vwi$A1_size, xlab = "Size [mm]", ylab = "Number of Aneurysms", main= "Aneurysms repartition by size",
     xlim = c(0,40), ylim = c(0,50),col= "bisque")

axis(1, at=seq(0,40,by=10), labels=FALSE)
text(seq(0,40, by=5), par("usr")[3]-0.2, labels = seq(0,40, by=5), srt=0, pos = 1, xpd=TRUE )


axis(2, at=seq(0,50, by=10), labels=FALSE)
text(y=seq(0,50, by=10),par("usr")[1], labels= seq(0,50, by=10), srt=0, pos=3, xpd=TRUE)

#### HISTOGRAM FIG.1B
original.parameters <- par()
par(xaxt="n")
par(yaxt="n")



data.cut<- cut(data_vwi$A1_size,c(seq(0,18,3), Inf))
test1 <- tapply(data_vwi$A1_vwi,data.cut, 
       function(x) sum(x==0, na.rm = TRUE)/length(x))*50 
test1[is.na(test1)] <- 0
test1 <- tapply(data_vwi$A1_vwi,data.cut, 
                function(x) sum(x==0, na.rm = TRUE)/length(x))*50 
test1 <- tapply(data_vwi$A1_vwi,data.cut, 
                function(x) sum(x==0, na.rm = TRUE)/length(x))*50 
### create vector of the y variable that correspond to the percentage


###
figure1b <-hist(as.numeric(data.cut),breaks= 0:7,
                      xlab = "Size [mm]",
                      ylab = "Number of Aneurysms", 
                      ylim = c(0,50),
                      main= "Aneurysms repartition by size",
                      col= "bisque",
                      las = 2, cex.lab =1.3,
                      xaxt="n",
                      right=TRUE)
axis(1, at=figure1b$mids,labels=c("1-3","4-6","7-9","10-12","13-15","16-18","19+"), cex.axis=0.9, pos = 0)
lines(seq(0.5,6.5, by=1),test1)




axis(1, at=figure1b$mids,labels=FALSE)

text(x=)

axis(2, at=seq(0,50, by=10), labels=FALSE)

text(y=seq(0,50, by=10),par("usr")[1], labels= seq(0,50, by=10), srt=0, pos=3, xpd=TRUE)

####


### BOX PLOT FIG 2.

?boxplot
data_vwi$A1_vwi = as.factor(data_vwi$A1_vwi)
levels(data_vwi$A1_vwi)
data_vwi$A1_vwiforboxplot <- factor(data_vwi$A1_vwi, levels=c("1", "0")) ### if want to switch the plots


boxplot1 <-boxplot(A1_size~A1_vwi, data=data_vwi,
        main= 'Aneurysms Size and Enhancement', cex.main = 2,
        ylab = 'Size [mm]',cex.lab= 1.5,
        ylim = c(0,20), 
        las = 1,
        names=c("No Enhancement","Enhancement"), cex.axis= 1.7,
        col="bisque",
        boxwex=0.93,
        outline=FALSE )
par(yaxt="n")
axis(2, at=seq(0,30, by=10), labels=FALSE, cex=2)
text(y=seq(0,30, by=10),par("usr")[1], labels= seq(0,30, by=10), srt=0, pos=2, xpd=TRUE, cex = 1.8)





