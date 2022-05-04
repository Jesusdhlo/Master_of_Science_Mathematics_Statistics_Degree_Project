rm(list=ls())
setwd("D:/Universidad de Puerto Rico - Mayaguez/5-6 Semestre (Tercer año)/PC UPRM/Trabajo de grado/Trabajo de grado")
dat<-read.csv("sol_3686_basenew.csv", header=TRUE, sep=",", na.strings=" ")
dat2<-read.csv("sol_3686_graduacionnew.csv", header=TRUE, sep=",", na.strings=" ")
dat_gpa<-read.csv("Std_GPA.csv", header=TRUE, sep=",", na.strings=" ") #this file was created using Minitab (see more details below)
#dat2[,7]<-as.character(dat2[,7])
# First a function is created to make packages available. Function provided at https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Now we run the function
packages <- c("tidyverse","rpart","partykit","ROCR","randomForest","gbm","caret","e1071","nnet","ranger","Rcpp","VIM","mice") # este paquete se usan para cargar la data de data.pr.gov al programa R
ipak(packages) #install or activate packages
library(tidyverse) # library(ggplot2)

#filter to create cohort of students that came in no later than 2010 and graduated no later than 2018
dat<-dat%>%filter(Year <= 2010)#dat<-dat[dat$Year<=2010,] #at least 8 years to graduate

who<-which(dat2$GRAD_CONCENTRACION_GPA==0) #multiple students have 0 concentration GPAs. Is that even possible? I think so, i.e. transferred to different program
summary(dat2$PROG_ACAD_GRAD_ABRV[who])

dat$MATE_APR_CEEB<-as.numeric(as.character(dat$MATE_APR_CEEB))
dat$ESPA_APR_CEEB<-as.numeric(as.character(dat$ESPA_APR_CEEB))
dat$INGL_APR_CEEB<-as.numeric(as.character(dat$INGL_APR_CEEB))
dat$GPA_1ER_ANO<-as.numeric(as.character(dat$GPA_1ER_ANO))

dat_gpa$Rel_School_GPA<-as.numeric(as.character(dat_gpa$Rel_School_GPA))

who<-match(dat2$STUDENT_RECORD_KEY,dat$STUDENT_RECORD_KEY)
who<-na.omit(who)
dat$FACULTAD_GRAD<-NA;
dat$PROG_ACAD_GRAD<-NA;
dat$GRAD_GPA<-NA
dat$GRAD_CONC_GPA<-NA
dat$Year_Grad<-NA
dat[who,c("FACULTAD_GRAD","PROG_ACAD_GRAD","GRAD_GPA","GRAD_CONC_GPA","Year_Grad")]<-dat2[,c(2,4,5,6,8)]

##### in Std_GPA I derived relative GPA measures. Rel_Stud_GPA divides each students GPA by mean HS GPA of admitted students. This way, the variable accounts for difficulty of school, 
##### and preparedness of student. If we were to divide student GPA by freshmen GPA, then program difficulty would influence variable. 
# Rel_School_GPA is derived by dividing mean RUM GPA of students from that school by mean HS GPA of admitted students. This measures how good the schools prepare the students
dat$Rel_Stud_GPA<-NA;
dat$Rel_School_GPA<-NA;
who<-match(dat_gpa$Sorted.STUDENT_RECORD_KEY,dat$STUDENT_RECORD_KEY)
who<-na.omit(who)
dat[who,c("Rel_Stud_GPA","Rel_School_GPA")]<-dat_gpa[,c("Rel_Stud_GPA","Rel_School_GPA")]

dat$Year_Grad<-as.numeric(as.character(dat$Year_Grad))
#dat$Grad_GPA<-as.numeric(as.character(dat$GRAD_GPA))





dat<-rename(dat,Major=PROG_ACAD_ADMISION_ABRV)
dat<-rename(dat,Faculty=FACULTAD_ADMISION,Apt_Verbal=VERBAL_APT_CEEB,Apt_Matem=MATE_APT_CEEB,
            Aprov_Espanol=ESPA_APR_CEEB,Aprov_Matem=MATE_APR_CEEB,Aprov_Ingles=INGL_APR_CEEB,
            IGS=IGS_ADMISSION_INDEX,Highschool.GPA=E0_PROMEDIO_ESC_SUP,Gender=STUDENT_GENDER,School_Type=TIPO_DE_ESCUELA)

table(dat$Major) #arbitrarily, any program that does not have 10 students admitted/yr on average will be merged into a "Other" program.

who<-which((as.numeric(dat$Major) %in% which(table(dat$Major)<=10*(max(dat$Year)-min(dat$Year))))==T)
dat$Major<-as.character(dat$Major)
dat$Major[who]<-999 #999 level should occur length(who) times
dat$Major<-as.factor(dat$Major)




#### need to clean up data. For example some students show as not graduating, but have grad Year and grad GPA.
data.dup <- dat[duplicated(dat) | duplicated(dat, fromLast = TRUE), ] #empty
#duplicated ID would imply transfers are treated separately
dat$ID.Code[duplicated(dat$ID.Code) | duplicated(dat$ID.Code, fromLast = TRUE) ] #empty


### clean up parent education variables
#summary(dat$EDUC_MADRE) #below, better categories are created.
# open question. Should I leave None as category (1%), or should it be changed to NA?
dat<-dat %>%  
  mutate(EDUC_MADRE = as.character(EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'Asistio a Universidad, No Termino' |
                                EDUC_MADRE == 'Grado Asociado', 'Assoc or Less', EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'Bachillerato', 'College', EDUC_MADRE),         
         EDUC_MADRE = if_else(EDUC_MADRE == 'Completo Escuela Superior', 'HS', EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'Maestria' |
                                EDUC_MADRE == 'Doctorado'| EDUC_MADRE =='Estudios Graduados', 'Grad', EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'Grados 1-6' |
                                EDUC_MADRE == 'Grados 1 - 9'| EDUC_MADRE =='Grados 7-9'| EDUC_MADRE =='Grados 10-12'| EDUC_MADRE =='Grados 10 - 12', 'LHS', EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'Ninguna', 'None', EDUC_MADRE),
         EDUC_MADRE = if_else(EDUC_MADRE == 'No Suministrado', 'NA', EDUC_MADRE),
         EDUC_MADRE = as.factor(EDUC_MADRE))
levels(dat$EDUC_MADRE)[levels(dat$EDUC_MADRE)=='NA'] <- NA

summary(dat$EDUC_MADRE)

# open question. Should I leave None as category (3% for fathers), or should it be changed to NA?

dat<-dat %>%
  mutate(EDUC_PADRE = as.character(EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'Asistio a Universidad, No Termino' |
                                EDUC_PADRE == 'Grado Asociado', 'Assoc or Less', EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'Bachillerato', 'College', EDUC_PADRE),         
         EDUC_PADRE = if_else(EDUC_PADRE == 'Completo Escuela Superior', 'HS', EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'Maestria' |
                                EDUC_PADRE == 'Doctorado'| EDUC_PADRE =='Estudios Graduados', 'Grad', EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'Grados 1-6' |
                                EDUC_PADRE == 'Grados 1 - 9'| EDUC_PADRE =='Grados 7-9'| EDUC_PADRE =='Grados 10-12'| EDUC_PADRE =='Grados 10 - 12', 'LHS', EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'Ninguna', 'None', EDUC_PADRE),
         EDUC_PADRE = if_else(EDUC_PADRE == 'No Suministrado', 'NA', EDUC_PADRE),
         EDUC_PADRE = as.factor(EDUC_PADRE))
levels(dat$EDUC_PADRE)[levels(dat$EDUC_PADRE)=='NA'] <- NA

summary(dat$EDUC_PADRE)

dat<-dat %>%
  mutate(School_Type = as.character(School_Type),
         School_Type = if_else(School_Type == 'Privada', 'Private', School_Type),
         School_Type = if_else(School_Type == 'Pública', 'Public', School_Type),         
         School_Type = if_else(School_Type == 'Examen de Equiv.' |
                                 School_Type == 'Otra', 'Other', School_Type),
         School_Type = if_else(School_Type == 'No Disponible', 'NA', School_Type),
         School_Type = as.factor(School_Type))
levels(dat$School_Type)[levels(dat$School_Type)=='NA'] <- NA

summary(dat$School_Type)

dat<-dat %>%
  mutate(INGRESO_FAMILIAR = as.character(INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == '$1,000 - $1,499' |
                                      INGRESO_FAMILIAR == '$1,500 - $1,999'| INGRESO_FAMILIAR =='$2,000 - $2,999'| 
                                      INGRESO_FAMILIAR == '$3,000 - $3,999' |
                                      INGRESO_FAMILIAR == '$4,000 - $4,999'| INGRESO_FAMILIAR =='$750 - $999'|
                                      INGRESO_FAMILIAR == 'Menos de $500'| INGRESO_FAMILIAR =='Menos de $7,499'| 
                                      INGRESO_FAMILIAR == '$501 - $749'|INGRESO_FAMILIAR == '$10,000 - $12,499'|
                                      INGRESO_FAMILIAR =='$5,000 - $7,499'|INGRESO_FAMILIAR == '$7,500 - $12,499','I1_L12.5', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == '$12,500 - $14,999' |
                                      INGRESO_FAMILIAR == '$15,000 - $17,499'| INGRESO_FAMILIAR =='$17,500 - $19,999',
                                    'I2_B12.5A20', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == '$20,000 - $29,999',
                                    'I3_B20A30', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == '$30,000 - $39,999'| INGRESO_FAMILIAR =='$40,000 - $49,999',
                                    'I4_B30A50', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == 'Mas de $50,000',
                                    'I5_O50', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == 'No Suministrado', 'NA', INGRESO_FAMILIAR),
         INGRESO_FAMILIAR = as.factor(INGRESO_FAMILIAR))
levels(dat$INGRESO_FAMILIAR)[levels(dat$INGRESO_FAMILIAR)=='NA'] <- NA

summary(dat$INGRESO_FAMILIAR)



#dat<-dat %>%
#  mutate(INGRESO_FAMILIAR = as.character(INGRESO_FAMILIAR),
#         INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == '$1,000 - $1,499' |
#INGRESO_FAMILIAR == '$1,500 - $1,999'| INGRESO_FAMILIAR =='$2,000 - $2,999'| 
#INGRESO_FAMILIAR == '$3,000 - $3,999' |
#INGRESO_FAMILIAR == '$4,000 - $4,999'| INGRESO_FAMILIAR =='$750 - $999'|
#INGRESO_FAMILIAR == 'Menos de $500'| INGRESO_FAMILIAR =='Menos de $7,499'| 
#INGRESO_FAMILIAR == '$501 - $749'|INGRESO_FAMILIAR == '$10,000 - $12,499'|
#INGRESO_FAMILIAR =='$5,000 - $7,499'|INGRESO_FAMILIAR == '$7,500 - $12,499','L12500', INGRESO_FAMILIAR),
#INGRESO_FAMILIAR = if_else(INGRESO_FAMILIAR == 'No Suministrado', 'NA', INGRESO_FAMILIAR),
#INGRESO_FAMILIAR = as.factor(INGRESO_FAMILIAR))
#levels(dat$INGRESO_FAMILIAR)[levels(dat$INGRESO_FAMILIAR)=='NA'] <- NA

#summary(dat$INGRESO_FAMILIAR)

who<-which(dat$GPA_1ER_ANO==0)#those with GPA=0 means they dropped out in first semester, or error. But none of them GRAD, so it does not seem to be an error. 
#dat<-dat[-who,] #I could remove those that did not finish a semester but at the moment I do not see any reason to.

# Those who do not finish the first semester, are a subset of those who do not graduate. Could a model predict that students do not finish first semester?
# if I create a new variable, drop_out (use as.factor(ifelse....)), and build a tree for drop out based on predictors (except freshmen GPA), I currently do not find anything of value (new predictors could change this)
#dat$Apt_Matem[which(dat$GPA_1ER_ANO==0)] #some had good scores!



length(which(dat$GRAD=="N" & dat$GPA_1ER_ANO>3.9))#many students got a high 1st yr GPA and still did not graduate. Likely, these students are not struggling to graduate 
# because they struggle with college life. Maybe transferred to a different institution? (both genders, many study programs) Should these students be kept in the data? Only non-academic predictors would be able to explain 
# students with high GPA not graduating.

# IGS can be modeled almost perfectly since it is a linear fct of college board scores and HS GPA. 
# Thus very high residuals are indicative of an error. They will be removed.
fit<-lm(IGS~Apt_Verbal+Apt_Matem+Highschool.GPA,data=dat)
res<-resid(fit)

#### some IGS =0. Transfers? 
who<-which(abs(res)>2 & dat$IGS!=0)
dat<-dat[-who,]

###  I could estimate IGS using HS.GPA, etc but in the end I will not use IGS for my predictive model

who<-which(dat$Highschool.GPA==0) #Homeschooled? Just one person
dat<-dat[-who,]
#who<-which(dat$GRAD=="N" & (is.na(dat$Year_Grad)==F |is.na(dat$Grad_GPA)==F)) #it is empty now
#dat<-dat[-who,]

di<-dat$Year_Grad-dat$Year #hard to intepret because eng programs are designed for 5 years
summary(di)
hist(di) #some people take a long time to graduate. 

#####check Graduation times
dif<-dat$Year_Grad-dat$Year
summary(dif)
who<-which(dif<=0) #by student ID we can identify these students and try to correct any error.
#dat<-dat[-who,]
#dif<-dat$Year_Grad-dat$Year
who<-which((dif>8 & dat$Faculty=='Facultad de Ingeniería') | (dif>6 & dat$Faculty!='Facultad de Ingeniería')) # looking for ppl graduating in 150% time or less. About 5% of those who graduate take over 150% time
dat<-dat[-who,]

#eliminate students w HS GPA of 0?
#who<-which(dat$Rel_Stud_GPA==0);length(who) # looking for ppl w HS GPA of zero
#dat<-dat[-who,]#very few. home schooled?


#### graduated in time. Should do this separately because cohort can go until admission year 2013
#dif<-dat$Year_Grad-dat$Year
#grad.intime<-ifelse((dif>5 & dat$Faculty=='Facultad de Ingeniería') | (dif>4 & dat$Faculty!='Facultad de Ingeniería')| is.na(dif)==T,"N","Y")
#dat$grad.intime<-as.factor(grad.intime)

#people who didn't graduate should not have a GRAD GPA or COnc GPA
w<-which(dat$GRAD=="N" & (is.na(dat$GRAD_GPA)==F|is.na(dat$GRAD_CONC_GPA)==F))
length(w)

summary(dat)

# 0 Aprov shoud be removed (only 2 students)
w<-which(dat$Aprov_Espanol==0|dat$Aprov_Ingles==0|dat$Aprov_Matem==0)
dat<-dat[-w,]

### remove unnecessary variables
dat2<-dat%>%select(-c(STUDENT_RECORD_KEY,Year_Grad,Highschool.GPA,ANO_ACAD_ADMISION,IGS,GRAD_GPA,GRAD_CONC_GPA,FACULTAD_GRAD,FECHA_DE_NACIM,PROG_ACAD_GRAD, PROG_ACAD_ADMISION, HIGH_SCHOOL)) # should keep Year only if effect is simple. Although likely an important predictor, it's effect should disappear quickly with the intervention program.

### remove unnecessary variables in deep learning
#dat2<-dat%>%select(-c(ANO_ACAD_ADMISION,IGS,GRAD_GPA,GRAD_CONC_GPA,FACULTAD_GRAD,FECHA_DE_NACIM,PROG_ACAD_GRAD)) # should keep Year only if effect is simple. Although likely an important predictor, it's effect should disappear quickly with the intervention program.

#para random forest
dat2$GRAD<-as.factor(dat2$GRAD)
dat2$Major<-as.factor(dat2$Major)
dat2$INGRESO_FAMILIAR<-as.factor(dat2$INGRESO_FAMILIAR)
dat2$EDUC_PADRE<-as.factor(dat2$EDUC_PADRE)
dat2$EDUC_MADRE<-as.factor(dat2$EDUC_MADRE)
dat2$School_Type<-as.factor(dat2$School_Type)
########################

who<-which(dat2$EDUC_PADRE=='None')
dat2$EDUC_PADRE[who]<-NA

who<-which(dat2$EDUC_MADRE=='None')
dat2$EDUC_MADRE[who]<-NA

#partition the data
dat2 <- dat2 %>% select(GRAD, everything()) #Cambiando de posicion una variable
set.seed(123)
who<-sample(1:nrow(dat2),round(.20*nrow(dat2)))
train<-dat2[-who,]
test<-dat2[who,]

################################# Particion para tabnet modelo 2
####################################################################
#Para tabnet tuve que sacar primero las muestras de train, valid and test, correr las lineas de abajo 
#excepto que de ultimas imputo train y lo guardo para cargarlo en python.

dat2 <- dat2 %>% select(GRAD, everything())
who<-sample(1:nrow(dat2),round(.20*nrow(dat2)))
train<-dat2[-who,]
test<-dat2[who,]
who1<-sample(1:nrow(test),round(.50*nrow(test)))
valid<-test[who1,]
test<-test[-who1,]

test<-test%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime
valid<-valid%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime

write.table(train, file="data_imputation_paramodel2_train.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)
write.table(test, file="data_imputation_paramodel2_test.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)
write.table(valid, file="data_imputation_paramodel2_valid.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)


###############################
################################# Particion para tabnet modelo 1
dat2<-dat2%>%select(-c(GPA_1ER_ANO,Rel_School_GPA ))
dat2 <- dat2 %>% select(GRAD, everything())
who<-sample(1:nrow(dat2),round(.20*nrow(dat2)))
train<-dat2[-who,]
test<-dat2[who,]
who1<-sample(1:nrow(test),round(.50*nrow(test)))
valid<-test[who1,]
test<-test[-who1,]

test<-test%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA) #,grad.intime
valid<-valid%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA) #,grad.intime

write.table(train, file="data_imputation_paramodel1_train.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)
write.table(test, file="data_imputation_paramodel1_test.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)
write.table(valid, file="data_imputation_paramodel1_valid.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)

#######################################
#remove rows with NAs
#3701
#train1<-train%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime
#14712
################################# Data imputation usando mice
######################################################
trainsinNA<-train%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA) #,grad.intime
test<-test%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime


#library(mice)
#install.packages('Rcpp')
#library(Rcpp)
#install.packages("VIM")
#library("VIM")

round(sapply(train, function(x) sum(is.na(x))/dim(train)[1])*100,2)# sapply(train, function(x) sum(is.na(x))/dim(train)[1])*100
sapply(train, function(x) sum(is.na(x)))
sum(is.na(train))
md.pattern(train,rotate.names=TRUE)
matrixplot(train_graf, sortby = 2)
train_graf <- train%>%select(-c('GRAD', 'Year', 'Faculty','Major', 'Apt_Verbal', 'Apt_Matem', 'Gender', 'Rel_Stud_GPA', 'Rel_School_GPA' ))
patrones <-aggr(train, 
     col=c('blue','red'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(train), cex.axis=.5,
     gap=3, ylab=c("Proporción valores perdidos","Patrones"))
summary(patrones) #porcentaje de mising 
aggr(train,number=TRUE,prop=FALSE)

nearZeroVar(train, saveMetrics= T)

#flores, Para mi es muy valioso su punto de vista, el aorte que usted me pueda brindar a mi proyecto
# HE venido trabajndo en mi proyecto de grado y me gustaría tener su concepto en cuanto a mi documento,
#Si tiene alguna sugerencia, sería de gran ayuda muy enriquecedor para mi. Mi advisor me indica que hay
#algunas lagunas

imputate_1=mice(train[,2:17], m=3, maxit=3, seed=123)# imputate missing values with 3 iterations
print(imputate_1) # it prints the methods used for imputation
stripplot(imputate_1, pch=20, cex=1.2)# it shows where the imputed points fall for each column
dat_inpu=complete(imputate_1,2)  # the new dat2_1 dataset with imputated missing values.  # and each iteration
dat_inpu$GRAD <- train[,1]
train <- dat_inpu%>%select(GRAD, everything())


################################# Data imputation normalito
######################################################
dat2 <- dat2 %>% select(GRAD, everything()) #Cambiando de posicion una variable
sapply(dat2, function(x) sum(is.na(x))) #contando Na por col
boxplot(dat2$GPA_1ER_ANO)
library(imputeMissings)
dat2<-impute(dat2, object = NULL, method = "median/mode", flag = FALSE) # generalmente crean sesgo
sapply(dat2, function(x) sum(is.na(x)))

dat2.1<-dat2%>%select(-c(GPA_1ER_ANO,Rel_School_GPA ))
write.table(dat2, file="data_imputation.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)
write.table(dat2.1, file="data_sin_imputation_paramodel1.txt", row.names = FALSE, quote = FALSE,sep = ",",col.names = TRUE)

#remove rows with NAs
#dat2<-dat2%>%drop_na(STUDENT_RECORD_KEY,Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,Highschool.GPA,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime

################### Guardando esta base preprocesada.
#write.table(dat2,file = "data_pre.txt", row.names = FALSE,  quote = FALSE, sep = ",", na = "NA", col.names = TRUE)
##############################################

#w<-which(dat2$Year>2007)
#summary(dat2$GRAD[w]) #implies a graduation rate higher than RUM reports
#prop.table(table(dat2$GRAD[w]))


########################################################### Tuning Modelos antes del primer ano
################################################################################################

####Nota algunos modelos requieren que se corra lo de arriba porque por ejemplo en arboles necesita retar 1 a la Y.
####Osea se le hace una modificacion a GRAD

####################################
######################## Decision Trees
###################################


library(rpart)
formJ1 <- as.formula("GRAD ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol +INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")



mod_tree1 <- rpart(formJ1, data = train,control = list(cp = 0, xval = 10), method = "class")
printcp(mod_tree1)
plotcp(mod_tree1)
cp1=mod_tree1$cptable[which.min(mod_tree1$cptable[,"xerror"]),"CP"]

hyper_grid <- expand.grid(
  minsplit = seq(11, 17, 1),
  maxdepth = seq(11, 17, 1))

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = formJ1,
    data    = train,
    method  = "class",
    control = list(minsplit = minsplit, maxdepth = maxdepth, cp=cp1, xval = 10)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    error = map_dbl(models, get_min_error)
  ) %>% 
  arrange(error) %>%  
  top_n(-10, wt = error)

#cp    = map_dbl(models, get_cp),

#####
optimal_tree <- rpart(
  formula = formJ1,
  data    = train,
  method  = "class",
  control = list(minsplit = 17, maxdepth = 12, cp = 0)
)

test <- test %>%
  mutate(grad_dtree = predict(optimal_tree, type = "class",newdata=test))
confusionMatrix(table(predict(optimal_tree, type = "class",newdata=test), test$GRAD))
performance(prediction(predict(optimal_tree,newdata=test,type="prob")[,2], test$GRAD), measure="auc")@y.values

##########importancia de las variables 

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(optimal_tree$variable.importance,horiz=TRUE)

####################################
######################## Random Forest
###################################

#Para este modelo tuve que hacer factor unas variables ver arriba antes d ela imputacion con mice
formJ1_sin_major <- as.formula("GRAD ~ Year + Gender + Faculty + Rel_Stud_GPA +
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol +INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")


hyper_grid <- expand.grid(
  
  mtry= seq(3,5,by=1) ,## number of variables randomly selected at each split.It ## is usually sqrt(#variables)
  ntree= seq(20, 200,by=1), # number of trees to grow in parallel
  node_size = seq(4, 10, by=2), # minimum size of terminal node
#  sampe_size = c(.70, .80, 0.9), # size of samples to draw
  OOB_RMSE   = 0
  
)

for (i in 1:nrow(hyper_grid)){
  
  mod_forest <- ranger(formula=formJ1_sin_major, data = train,
                       mtry=hyper_grid$mtry[i],
                       num.trees=hyper_grid$ntree[i],
                       min.node.size = hyper_grid$node_size[i],
#                       sample.fraction = hyper_grid$sampe_size[i],
                       seed=123
  )                           
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(mod_forest$prediction.error)
  
}

hyper_grid %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10) #needed less than 2,500 trees

mod_forest1 <- randomForest(formJ1_sin_major, data = train, ntree = 169, mtry = 3, nodesize = 10) 
varImpPlot(mod_forest1)

test <- test %>%
  mutate(grad_rf = predict(mod_forest1, type = "class",newdata=test))
confusionMatrix(table(predict(mod_forest1, type = "class",newdata=test), test$GRAD))
performance(prediction(predict(mod_forest1,newdata=test,type="prob")[,2], test$GRAD), measure="auc")@y.values


####################################
######################## Boosting
###################################
library(gbm)
set.seed (123)
train$resp<-ifelse(train$GRAD=="Y",1,0)
formJ1_B <- as.formula("resp ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol +INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

train$Gender=as.factor(train$Gender)
train$Faculty <- as.factor(train$Faculty)

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]
random_train$Gender <- as.factor(random_train$Gender)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formJ1_B,
    distribution = "bernoulli",
    data = random_train,
    n.trees = 2500,#previous hyper grid found could be less than 5000
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}


hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10) #needed less than 2,500 trees


set.seed(123)
test$Gender=as.factor(test$Gender)
test$Faculty <- as.factor(test$Faculty)

# train GBM model based on best model from results above
gbm.fit.final <- gbm(
  formJ1_B,distribution = "bernoulli",
  data = train,
  n.trees = 99,
  interaction.depth = 3,
  shrinkage = 0.10,
  n.minobsinnode = 15,
  bag.fraction = 1, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)




par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

L<-summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
############# Importancia de las variables
names(L)<-c("Predictoras", "Importancia")
#Lo que nos gustaría es que R respetara el orden en data.frame. Para que eso suceda, necesitamos cambiar el orden de los niveles de los factores especificando el orden explícitamente .
L$var <- factor(L$var, levels = L$var[order(L$rel.inf)])
ggplot(L, aes(x=Importancia.porc, y=Predictoras))+ geom_bar(stat = "identity", color="#6B2D46", fill = "#245D81")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=12),
        axis.title=element_text(size=12))

fit_pred<-predict(gbm.fit.final, newdata = test, n.trees = 99, type = "link")
gbm.test = as.factor(ifelse(fit_pred <0.5,"N", "Y"))
confusionMatrix(table(data = gbm.test, reference = test$GRAD))
performance(prediction(predict(gbm.fit.final, newdata = test, n.trees = 99, type = "response"), test$GRAD), measure="auc")@y.values


####################################
######################## Naive bayes
###################################
library(e1071)
formJ1 <- as.formula("GRAD ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol +INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

#caret
mod_nb <- naiveBayes(formJ1, data = train)


#caret
mod_nb <- train(x,y,'nb', trControl= trainControl(method = 'cv', number=10))
confusionMatrix(mod_nb)

test <- test %>% select(-c( Year_Grad)) #grad.intime

##Tuning
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),    #TRUE para la estimación de densidad. Si FALSE se estima una densidad normal.
  fL = 0:5,                      #Factor para la corrección de Laplace, default is 0
  adjust = seq(0, 5, by = 1)
)

x<-train %>% select(-c(GRAD,Rel_School_GPA,GPA_1ER_ANO)) #grad.intime
y<-train[,'GRAD']

mod_nb2 <- train(x,y,'nb', trControl= trainControl(method = 'cv', number=10), tuneGrid = search_grid)
mod_nb2$results %>% top_n(5, wt = Accuracy) %>% arrange(desc(Accuracy))


mod_nb1 <- naiveBayes(formJ1, data = train, usekernel=T, laplace=0, adjust =5)

confusionMatrix(table(predict(mod_nb1, test), test$GRAD))
performance(prediction(predict(mod_nb1,newdata=test,type="raw")[,2], test$GRAD), measure="auc")@y.values

####################################
######################## logistic 
###################################
train$resp<-ifelse(train$GRAD=="Y",1,0)
formJ1_B_lr <- as.formula("resp ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +
                    Apt_Verbal + Aprov_Ingles+Aprov_Matem+Aprov_Espanol +INGRESO_FAMILIAR+(EDUC_PADRE*EDUC_MADRE)+School_Type")
mod_lr1_1 <- glm (formJ1_B_lr, data = train, family = binomial)
pred_lr1_1 <- predict(mod_lr1_1, type = 'response',newdata=test)
pred.test1_1 <- as.factor(ifelse(pred_lr1_1 <0.5,"N", "Y"))
confusionMatrix(table(data = pred.test1_1, reference = test$GRAD))
performance(prediction(predict(mod_lr1_1, type = 'response',newdata=test), test$GRAD), measure="auc")@y.values


########################################################### Tuning Modelos antes despues del first year
################################################################################################

####Nota algunos modelos requieren que se corra lo de arriba porque por ejemplo en arboles necesita retar 1 a la Y.
####Osea se le hace una modificacion a GRAD

####################################
######################## Decision Trees
###################################


library(rpart)
formJ2 <- as.formula("GRAD ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")


mod_tree1 <- rpart(formJ2, data = train,control = list(cp = 0, xval = 10), method = "class")
printcp(mod_tree1)
plotcp(mod_tree1)
cp1=mod_tree1$cptable[which.min(mod_tree1$cptable[,"xerror"]),"CP"]

hyper_grid <- expand.grid(
  minsplit = seq(4, 30, 1),
  maxdepth = seq(2, 30, 1))

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = formJ2,
    data    = train,
    method  = "class",
    control = list(minsplit = minsplit, maxdepth = maxdepth, cp=cp1, xval = 10)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    error = map_dbl(models, get_min_error)
  ) %>% 
  arrange(error) %>%  
  top_n(-10, wt = error)

#cp    = map_dbl(models, get_cp),

#####
optimal_tree <- rpart(
  formula = formJ2,
  data    = train,
  method  = "class",
  control = list(minsplit = 24, maxdepth = 29, cp = 0)
)

test <- test %>%
  mutate(grad_dtree = predict(optimal_tree, type = "class",newdata=test))
confusionMatrix(table(predict(optimal_tree, type = "class",newdata=test), test$GRAD))
performance(prediction(predict(optimal_tree,newdata=test,type="prob")[,2], test$GRAD), measure="auc")@y.values

##########importancia de las variables 

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(optimal_tree$variable.importance,horiz=TRUE)

####################################
######################## Random Forest
###################################

formJ2_sin_major <- as.formula("GRAD ~ Year + Gender + Faculty + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

hyper_grid <- expand.grid(
  
  mtry= seq(3,5,by=1) ,## number of variables randomly selected at each split.It ## is usually sqrt(#variables)
  ntree= seq(20, 220,by=1), # number of trees to grow in parallel
  node_size = seq(4, 10, by=2), # minimum size of terminal node
  #  sampe_size = c(.70, .80, 0.9), # size of samples to draw
  OOB_RMSE   = 0
  
)

for (i in 1:nrow(hyper_grid)){
  
  mod_forest <- ranger(formula=formJ2_sin_major, data = train,
                       mtry=hyper_grid$mtry[i],
                       num.trees=hyper_grid$ntree[i],
                       min.node.size = hyper_grid$node_size[i],
                       #                       sample.fraction = hyper_grid$sampe_size[i],
                       seed=123
  )                           
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(mod_forest$prediction.error)
  
}

hyper_grid %>%
  dplyr::arrange(OOB_RMSE) %>%
  head(10) #needed less than 2,500 trees

mod_forest2 <- randomForest(formJ2_sin_major, data = train, ntree = 187, mtry = 3, nodesize = 4) 
varImpPlot(mod_forest1)

test <- test %>%
  mutate(grad_rf = predict(mod_forest2, type = "class",newdata=test))
confusionMatrix(table(predict(mod_forest2, type = "class",newdata=test), test$GRAD))
performance(prediction(predict(mod_forest2,newdata=test,type="prob")[,2], test$GRAD), measure="auc")@y.values


####################################
######################## Boosting
###################################
library(gbm)
set.seed (123)
train$resp<-ifelse(train$GRAD=="Y",1,0)
formJ2_B <- as.formula("resp ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

train$Gender=as.factor(train$Gender)
train$Faculty <- as.factor(train$Faculty)

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]
random_train$Gender <- as.factor(random_train$Gender)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formJ2_B,
    distribution = "bernoulli",
    data = random_train,
    n.trees = 2500,#previous hyper grid found could be less than 5000
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}


hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10) #needed less than 2,500 trees


set.seed(123)
test$Gender=as.factor(test$Gender)
test$Faculty <- as.factor(test$Faculty)

# train GBM model based on best model from results above
gbm.fit.final <- gbm(
  formJ2_B,distribution = "bernoulli",
  data = train,
  n.trees = 1423,
  interaction.depth = 3,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  bag.fraction = .80, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)




par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

fit_pred<-predict(gbm.fit.final, newdata = test, n.trees = 1423, type = "link")
gbm.test = as.factor(ifelse(fit_pred <0.5,"N", "Y"))
confusionMatrix(table(data = gbm.test, reference = test$GRAD))
performance(prediction(predict(gbm.fit.final, newdata = test, n.trees = 1029, type = "response"), test$GRAD), measure="auc")@y.values


####################################
######################## Naive bayes
###################################
library(e1071)
formJ2 <- as.formula("GRAD ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

#caret
mod_nb <- naiveBayes(formJ1, data = train)


#caret
mod_nb <- train(x,y,'nb', trControl= trainControl(method = 'cv', number=10))
confusionMatrix(mod_nb)

test <- test %>% select(-c( Year_Grad)) #grad.intime

##Tuning
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),    #TRUE para la estimación de densidad. Si FALSE se estima una densidad normal.
  fL = 0:5,                      #Factor para la corrección de Laplace, default is 0
  adjust = seq(0, 5, by = 1)
)

x<-train %>% select(-c(GRAD)) #grad.intime
y<-train[,'GRAD']

mod_nb2 <- train(x,y,'nb', trControl= trainControl(method = 'cv', number=10), tuneGrid = search_grid)
mod_nb2$results %>% top_n(5, wt = Accuracy) %>% arrange(desc(Accuracy))


mod_nb1 <- naiveBayes(formJ2, data = train, usekernel=F, laplace=5, adjust =0)

confusionMatrix(table(predict(mod_nb1, test), test$GRAD))
performance(prediction(predict(mod_nb1,newdata=test,type="raw")[,2], test$GRAD), measure="auc")@y.values

####################################
######################## logistic 
###################################
train$resp<-ifelse(train$GRAD=="Y",1,0)
formJ2_B_lr <- as.formula("resp ~ Year + Gender + Faculty + Major + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")

mod_lr1_1 <- glm (formJ2_B_lr, data = train, family = binomial)
pred_lr1_1 <- predict(mod_lr1_1, type = 'response',newdata=test)
pred.test1_1 <- as.factor(ifelse(pred_lr1_1 <0.5,"N", "Y"))
confusionMatrix(table(data = pred.test1_1, reference = test$GRAD))
performance(prediction(predict(mod_lr1_1, type = 'response',newdata=test), test$GRAD), measure="auc")@y.values


