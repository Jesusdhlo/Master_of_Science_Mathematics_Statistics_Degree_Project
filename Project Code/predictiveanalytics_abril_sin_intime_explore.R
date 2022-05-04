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

############################### Se redefine Rel_Stud_GPA
##########################################################
for (i in 1:dim(dat)[1]) {
  dat$Rel_Stud_GPA[i] <- dat$Highschool.GPA[i]/mean(dat$Highschool.GPA)
}



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
#dat2<-dat%>%select(-c(Year_Grad,ANO_ACAD_ADMISION,IGS,GRAD_GPA,GRAD_CONC_GPA,FACULTAD_GRAD,FECHA_DE_NACIM,PROG_ACAD_GRAD, PROG_ACAD_ADMISION, HIGH_SCHOOL)) # should keep Year only if effect is simple. Although likely an important predictor, it's effect should disappear quickly with the intervention program.
dat2<-dat%>%select(-c(STUDENT_RECORD_KEY,Year_Grad,Highschool.GPA,ANO_ACAD_ADMISION,IGS,GRAD_GPA,GRAD_CONC_GPA,FACULTAD_GRAD,FECHA_DE_NACIM,PROG_ACAD_GRAD, PROG_ACAD_ADMISION, HIGH_SCHOOL)) # should keep Year only if effect is simple. Although likely an important predictor, it's effect should disappear quickly with the intervention program.

who<-which(dat2$EDUC_PADRE=='None')
dat2$EDUC_PADRE[who]<-NA

who<-which(dat2$EDUC_MADRE=='None')
dat2$EDUC_MADRE[who]<-NA

### remove unnecessary variables in deep learning
#dat2<-dat%>%select(-c(ANO_ACAD_ADMISION,IGS,GRAD_GPA,GRAD_CONC_GPA,FACULTAD_GRAD,FECHA_DE_NACIM,PROG_ACAD_GRAD)) # should keep Year only if effect is simple. Although likely an important predictor, it's effect should disappear quickly with the intervention program.
########################
#partition the data
set.seed(123)
who<-sample(1:nrow(dat2),round(.20*nrow(dat2)))
train<-dat2[-who,]
test<-dat2[who,]

trainsinNA<-train%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA) #,grad.intime
train<-train%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA) #,grad.intime
#remove rows with NAs
#dat2<-dat2%>%drop_na(STUDENT_RECORD_KEY,Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,Highschool.GPA,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime
dat2<-dat2%>%drop_na(Year,Faculty,Major,Apt_Verbal, Aprov_Matem,Apt_Matem,Aprov_Espanol,Aprov_Ingles,INGRESO_FAMILIAR, EDUC_PADRE,School_Type,GPA_1ER_ANO,EDUC_MADRE,Gender,GRAD,Rel_Stud_GPA,Rel_School_GPA) #,grad.intime

sapply(dat2, function(x) sum(is.na(x))) 
str(dat2)

################### Guardando esta base preprocesada.
#write.table(dat2,file = "data_pre.txt", row.names = FALSE,  quote = FALSE, sep = ",", na = "NA", col.names = TRUE)
##############################################

#w<-which(dat2$Year>2007)
#summary(dat2$GRAD[w]) #implies a graduation rate higher than RUM reports
#prop.table(table(dat2$GRAD[w]))



################### explore data
#############################################

#########gratution rate per edu padre and madre
theTableP_M <- within(trainsinNA, 
                    EDUC_MADRE <- factor(EDUC_MADRE, 
                                         levels=c('Grad','College','Assoc or Less','HS','LHS','None')))

theTableP_M$EDUC_PADRE <- factor(theTableP_M$EDUC_PADRE, levels=c('Grad','College','Assoc or Less','HS','LHS','None'))

#theTableP <- within(trainsinNA, 
 #                   EDUC_PADRE <- factor(EDUC_PADRE, 
  #                                       levels=c('Grad','Bachillerato','Assoc or Less','HS','LHS','None')))

ggplot(theTableP_M, aes(x=EDUC_PADRE, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Nivel de Educación del Padre") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#B2182B", "Y" = "#2166AC" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

#theTableM <- within(trainsinNA, 
#                    EDUC_MADRE <- factor(EDUC_MADRE, 
#                                         levels=c('Grad','Bachillerato','Assoc or Less','HS','LHS','None')))

ggplot(theTableP_M, aes(x=EDUC_MADRE, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Nivel de Educación de la Madre") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#B2182B", "Y" = "#2166AC" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

################################### Interpretando los porcentajes de las graficas anteriores, lo hace por grupo
summary(trainsinNA$EDUC_PADRE)
py<-which(trainsinNA$EDUC_PADRE=='None' & trainsinNA$GRAD=='Y')
pn<-which(trainsinNA$EDUC_PADRE=='None' & trainsinNA$GRAD=='N')
length(py)/(length(py)+length(pn))  #Este sería de los 411 None el 54% de los estudinates se graduaron  
#0.52, 0.51, 0.50, 0.54 
#      0.5,  0.51, 0.5

#########gratution rate looking interaction with mom and dad
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

#Para que R organice los niveles del factor como yo desee uso 
#factor(,especifico como los quiero en que orden).

#theTableP <- within(trainsinNA, 
#                    EDUC_PADRE <- factor(EDUC_PADRE, 
#                                         levels=c('Grad','Bachillerato','Assoc or Less','HS','LHS','None')))

ggplot(theTableP_M, aes(x=EDUC_PADRE, fill = EDUC_MADRE))+
  geom_bar( aes(y=..count..), position="dodge",
  )+ xlab("Nivel de Educación del Padre") + ylab("Frecuencia")+
  facet_wrap(~GRAD)+ 
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=10),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  guides(fill=guide_legend(title="Nivel de Educación de la Madre"))+
  fill_palette("jco")

#theTableP_M <- within(trainsinNA, 
#                      School_Type <- factor(School_Type, 
#                                            levels=c('Private','Public','Other')))

#theTableP_M$INGRESO_FAMILIAR <- factor(theTableP_M$INGRESO_FAMILIAR, 
#                                  levels=c('I5_O50','I4_B30A50','I3_B20A30','I2_B12.5A20','I1_L12.5'))


#ggplot(theTableP_M, aes(x=School_Type, fill =INGRESO_FAMILIAR ))+
#  geom_bar( aes(y=..count..), position="dodge",
#  )+ xlab("Nivel de Educación del Padre") + ylab("Frecuencia")+
#  facet_wrap(~GRAD)+ 
#  theme(axis.line.y=element_blank(),
#        legend.text=element_text(size=10),
#        axis.text.x=element_text(size=12,angle=90),
#        axis.title=element_text(size=14),)+
#  guides(fill=guide_legend(title="Nivel de Educación de la Madre"))+
#  fill_palette("jco")

#library(ggplot2)
#library(ggpubr)
#theme_set(theme_pubr())


#ggplot(theTableM, aes(x=EDUC_MADRE, fill = EDUC_PADRE))+
#  geom_bar( aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge",
#  )+ xlab("Nivel Educación Madre") + ylab("Frecuencia")+
#  facet_wrap(~GRAD)+ 
#  theme(axis.line.y=element_blank(),
#        legend.text=element_text(size=10),
#        axis.text.x=element_text(size=12,angle=90),
#        axis.title=element_text(size=14),)+
#  guides(fill=guide_legend(title="Nivel Educación Padre"))+
#  fill_palette("jco")


boxplot(Rel_Stud_GPA~GRAD,data=train)
boxplot(Apt_Verbal~GRAD,data=train)
plot(EDUC_PADRE~INGRESO_FAMILIAR,data=train)

ggplot(train, aes(x=INGRESO_FAMILIAR, fill = EDUC_MADRE)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position = "fill")+xlab("Programa") + ylab("Conteo") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
#  scale_fill_manual("legend", values = c("Assoc or Less" = "darkgoldenrod1", "Bachillerato" = "indianred4", "Grad"="#B2182B", ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)


####################Esto no se puede hacer GAP de primer ano es una variable continua.
ggplot(train, aes(x=GPA_1ER_ANO, fill = GRAD)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position = "fill")+xlab("GPA de Primer Año") + ylab("Tasa de Graduación") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_fill_manual("legend", values = c("N" = "#B2182B", "Y" = "darkgoldenrod1"))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)
######################################################################################################3


################################################El Siguiente programa es para graficar solo para programa la graducacion de Y
###################################################
# graduation rate per program
k<-names(sort(table(train$Major), decreasing=TRUE))
p<-names(sort(table(train$GRAD), decreasing=TRUE))
y<-c(); n<-c()

for (i in 1:length(k)) {
  y[i]<-length(which(train$Major==k[i] & train$GRAD==p[1]))/length(which(train$Major==k[i] & (train$GRAD==p[2] | train$GRAD==p[1]))) # % del Yes Esta linea lo que hace es sacar el porcentaje de los que si en ese grupo (cada programa) 
  n[i]<-1-y[i]  # % del N0
  
}

P_P_programa<-data.frame(Major=k,Y=y,N=n)  #data_frame() esta obsoleto
P_P_programa<-P_P_programa[-55,]

#Lo que nos gustaría es que R respetara el orden en data.frame. Para que eso suceda, necesitamos cambiar el orden de los niveles de los factores especificando el orden explícitamente .
P_P_programa$Major <- factor(P_P_programa$Major, levels = P_P_programa$Major[order(P_P_programa$Y)])

ggplot(P_P_programa, aes(x = Major, y = Y)) + geom_bar(stat = "identity", color="#6B2D46", fill = "#245D81")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.title=element_text(size=14),)+
  xlab("Programa") + ylab("Tasa de Graduación")+coord_flip()

########################################################################33, "Y" = 

#graduation per Program http://estudiantes.upr.edu/admisiones/carreras/exploreps.php?a=lc&campus=Mayaguez
theTable <- within(train, 
                   Major <- factor(Major, 
                                   levels=names(sort(table(Major), 
                                                     decreasing=TRUE))))
ggplot(theTable, aes(x=Major, fill = GRAD, order=GRAD)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar()+xlab("Programa") + ylab("Conteo") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_fill_manual("legend", values = c("N" = "darkgoldenrod1", "Y" = "indianred4"))+
  guides(fill=guide_legend(title="Graduación"))

#graduation per Program
ggplot(theTable, aes(x=Major, fill = GRAD))+
  geom_bar(aes( y=..count..), position="dodge" ) +
  xlab("Programa") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "indianred4", "Y" = "darkgoldenrod1" ))+
  guides(fill=guide_legend(title="Graduación"))

# graduation rate per program
ggplot(theTable, aes(x=Major, fill = GRAD, order=GRAD)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position = "fill")+xlab("Programa") + ylab("Tasa de Graduación") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_fill_manual("legend", values = c("N" = "darkgoldenrod1", "Y" = "indianred4"))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

#graduation rate per Program
ggplot(theTable, aes(x=Major, fill = GRAD, order=GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Programa") + ylab("Tasa de Graduación") +
  #geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(round(..count../tapply(..count.., ..x.. ,sum)[..x..], digits=2)), angle =90),
  #  stat="count", position=position_dodge(0.5), vjust=0)+ Esta linea era para poner encima de la barra el porcentaje.
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "indianred4", "Y" = "darkgoldenrod1" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

# graduation rate per Start Year

pp<-function(i=min(train$Year)){
  length(train$GRAD[which(train$GRAD=="Y" & train$Year==i)])/length(train$GRAD[which(train$Year==i)])
  } #this function will find the graduation rate for Year i (1999 to 2010)
p<-NULL;a<-0
#now get graduation by year
for(j in min(train$Year):max(train$Year)){
  p[a+1]<-pp(j)
  a=a+1}


df_t <- data.frame(x = min(train$Year):max(train$Year), y = p) 
ggplot(data = df_t,aes(x, y, group = 1)) + geom_line() +geom_point()+theme_bw() + xlab("Año de Inicio") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90,hjust=1,size=8),axis.ticks = element_blank())+
  ylab("Tasa de Graduación")

#mio
ggplot(data = df_t, aes(x=x, y)) + geom_line(color = "salmon2", lwd=1.5) + xlab("Año de Inicio") + 
  theme(axis.text.x = element_text(hjust=1,size=8),axis.ticks = element_blank(),
        axis.title=element_text(size=14))+
  ylab("Tasa de Graduación")

plot(df_t, xlab="Año de Inicio", ylab="Tasa de Graduación", pch = 21,
     bg = "red",   # Color de fondo
     col = "blue", # Color del borde
     cex = 3,      # Tamaño del símbolo
     lwd = 3)


plot(df_t, col="salmon2", xlab="Año de Inicio", ylab="Tasa de Graduación", type= "l", lwd=3, axes = FALSE) #esta quedo
# Añadir el eje X
axis(1)
# Añadir el eje Y
axis(2)


# graduation per Faculty
theTable <- within(train, 
                   Faculty <- factor(Faculty, 
                                     levels=names(sort(table(Faculty), 
                                                       decreasing=TRUE))))
ggplot(theTable, aes(x=Faculty, fill = GRAD)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar()+xlab("Facultad") + ylab("Conteo") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_fill_manual("legend", values = c("N" = "darkgoldenrod1", "Y" = "indianred4"))+
  guides(fill=guide_legend(title="GRAD"))

ggplot(theTable, aes(x=Faculty, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Facultad") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#B2182B", "Y" = "#2166AC" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

theTable <- within(trainsinNA, 
                   School_Type <- factor(School_Type, 
                                         levels=c('Private','Public','Other')))

#income, seems a bit important w ggplot below, parents educ maybe a bit too
ggplot(theTable, aes(x=School_Type, fill = GRAD)) +
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position = "fill")+xlab("Tipo de Escuela") + ylab("Tasa de Graduación") + #ggtitle("")+
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        #axis.ticks.y=element_blank(),
        #axis.title.y=element_blank(),
        axis.title=element_text(size=14),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())+
  scale_fill_manual("legend", values = c("N" = "darkgoldenrod1", "Y" = "indianred4"))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

ggplot(theTable, aes(x=School_Type, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Tipo de Escuela") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#6B2D46", "Y" = "#245D81" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

theTable <- within(trainsinNA, 
                   INGRESO_FAMILIAR <- factor(INGRESO_FAMILIAR, 
                                              levels=c('I5_O50','I4_B30A50','I3_B20A30','I2_B12.5A20','I1_L12.5')))


ggplot(theTable, aes(x=INGRESO_FAMILIAR, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Ingreso Familiar") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#EB543D", "Y" = "#C03180" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)


theTable <- within(train, 
                   EDUC_PADRE <- factor(EDUC_PADRE, 
                                              levels=names(sort(table(EDUC_PADRE), 
                                                                decreasing=TRUE))))
ggplot(theTable, aes(x=EDUC_PADRE, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Educación del Padre") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#CA3400", "Y" = "#1C7B34" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)

theTable <- within(train, 
                   EDUC_MADRE <- factor(EDUC_MADRE, 
                                        levels=names(sort(table(EDUC_MADRE), 
                                                          decreasing=TRUE))))
ggplot(theTable, aes(x=EDUC_MADRE, fill = GRAD))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  xlab("Educación de la Madre") + ylab("Tasa de Graduación") +
  theme(axis.line.y=element_blank(),
        legend.text=element_text(size=14),
        axis.text.x=element_text(size=12,angle=90),
        axis.title=element_text(size=14),)+
  scale_fill_manual("legend", values = c("N" = "#ED5200", "Y" = "#36A5AA" ))+
  guides(fill=guide_legend(title="Graduación"))+scale_y_continuous(labels = scales::percent)


library(rpart)# to fit recursive partioning decision trees

##### Model for students after 1st year of college
form <- as.formula("GRAD ~ Year + Gender + Major + Rel_Stud_GPA + Rel_School_GPA+
                   Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")
form2 <- as.formula("GRAD ~ Year + Gender + Faculty + Rel_Stud_GPA +Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO+INGRESO_FAMILIAR+EDUC_PADRE+EDUC_MADRE+School_Type")
mod_tree <- rpart(form, data = train,control = rpart.control(cp = 0))#default tuning parameter is 1%
printcp(mod_tree)
plotcp(mod_tree) #suggests 0.0018 based on ?plotcp: "leftmost value for which the mean lies below the horizontal line" and printcp minimum.

#mod_tree <- rpart(form, data = train,control = rpart.control(cp = 0.0018))#
mod_tree <- prune(mod_tree, cp=mod_tree$cptable[which.min(mod_tree$cptable[,"xerror"]),"CP"]) #this command may over simplify though

library(partykit)
plot(as.party(mod_tree))

##########importancia de las variables 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(mod_tree$variable.importance,horiz=TRUE)

library(dplyr)

train <- train %>%
  mutate(grad_dtree = predict(mod_tree, type = "class"))

#two lines below not working Baumer et al.
#confusion <- tally(grad_dtree ~ GRAD, data = train, format = "count")
#confusion

co<-train %>% group_by(grad_dtree, GRAD) %>% tally()
#co[3,3]/(sum(co[,3])) #in sample error of model determining student will graduate and they do not (0.13)

co[1,3]/(co[1,3]+co[3,3])#### sensitivity defined as prob model determines student will not graduate when in fact he does not. (complement is model determines student graduates when in fact he does not, worst kind of error).



##### check with test data
library(ROCR)#performance and its plot
fit_pred<-predict(mod_tree,newdata=test,type="prob")[,2]
pred<-prediction(fit_pred,test$GRAD)
perf<-performance(pred, "tpr", "fpr")
plot(perf)

perf_auc<-performance(pred, measure="auc")
perf_auc@y.values #y.values, 78% chance model will be able to distinguish between positive class and negative class. 50% would represent random guessing. The model is not too bad.

#Usando form da 0.7956866
#Usando form2 da 0.758856

perf_df <- data.frame(perf@x.values, perf@y.values)
names(perf_df) <- c("fpr", "tpr")
roc <- ggplot(data = perf_df, aes(x = fpr, y = tpr)) +
  geom_line(color="blue") + geom_abline(intercept=0, slope=1, lty=3) +
  ylab(perf@y.name) + xlab(perf@x.name)

test <- test %>%
  mutate(grad_dtree = predict(mod_tree, type = "class",newdata=test))
confusionMatrix(table(predict(mod_tree, type = "class",newdata=test), test$GRAD))

co_test<-test %>% group_by(grad_dtree, GRAD) %>% tally()
co_test[1,3]/(co_test[1,3]+co_test[3,3])#### sensitivity defined as prob model determines student will not graduate when in fact he does not. (complement is model determines student graduates when in fact he does not, worst kind of error).



#random Forest    # modificar
#mod_forest <- randomForest(form, data = train, ntree = 201, mtry = 3) #when not aggregating some program codes must use Faculty instead of Program since fct does not handle too many categories
#Error in randomForest.default(m, y, ...) : 
#  Can not handle categorical predictors with more than 53 categories.


library(randomForest)
mod_forest <- randomForest(form, data = train, ntree = 201, mtry = 3) #when not aggregating some program codes must use Faculty instead of Program since fct does not handle too many categories
mod_forest

importance (mod_forest) # higher mean decrease in Gini statistic, the more important the variable
varImpPlot(mod_forest)

test <- test %>%
  mutate(grad_rf = predict(mod_forest, type = "class",newdata=test))

co_test<-test %>% group_by(grad_rf, GRAD) %>% tally()
co_test[1,3]/(co_test[1,3]+co_test[3,3])

# Boosting
library(gbm)
set.seed (123)
train$resp<-ifelse(train$GRAD=="Y",1,0)
form3 <- as.formula("resp ~ Year + Gender + Major + Rel_Stud_GPA + Rel_School_GPA+
                    Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol + GPA_1ER_ANO")
#hyper_grid <- expand.grid(
#  shrinkage = c(.01, .1, .3),
#  interaction.depth = c(1, 3, 5),
#  n.minobsinnode = c(5, 10, 15),
#  bag.fraction = c(.65, .8, 1), 
#  optimal_trees = 0,               # a place to dump results
#  min_RMSE = 0                     # a place to dump results
#)



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
    form3,
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
train$Gender=as.factor(train$Gender)

# train GBM model based on best model from results above
gbm.fit.final <- gbm(
  form3,distribution = "bernoulli",
  data = train,
  n.trees = 483,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 5,
  bag.fraction = .65, 
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
#mod_boost =gbm(form3,data=train, distribution=
#"bernoulli",n.trees =5000 , interaction.depth =4) #see gareth et al's book
#summary(mod_boost)

library(caret)
#best.iter <- gbm.perf(gbm.fit.final) # too small
fit_pred<-predict(gbm.fit.final, newdata = test, n.trees = 2500, type = "link")
gbm.test = as.factor(ifelse(fit_pred <0.5,"N", "Y"))
confusionMatrix(table(data = gbm.test, reference = test$GRAD))

#naive bayes
library(e1071)
#caret
mod_nb <- naiveBayes(form, data = train)

x<-train %>% select(-c(GRAD, Year_Grad)) #grad.intime
y<-train[,'GRAD']
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

mod_nb2 <- train(x,y,'nb', trControl= trainControl(method = 'cv', number=10), tuneGrid = search_grid)
mod_nb2$results %>% top_n(5, wt = Accuracy) %>% arrange(desc(Accuracy))

plot(mod_nb2)

confusionMatrix(table(predict(mod_nb2, test), test$GRAD))

### Recall
re<-confusionMatrix(table(predict(mod_nb2, test), test$GRAD))
recall=re$table[1,1]/(re$table[1,1]+re$table[2,1])


#SVM (not suitable for large data)
svmfit =svm(form, data=train, kernel ="polynomial", cost =1,gamma=1)

train <- train %>%
  mutate(grad_nb = predict(mod_nb, newdata=train))

train %>% group_by(grad_nb, GRAD) %>% tally()


# ANN
library(nnet)
mod_nn <- nnet(form, data = train, size = 5)

train <- train %>%
  mutate(grad_nn = predict(mod_nn, newdata=train,type="class"))

train %>% group_by(grad_nn, GRAD) %>% tally()


test <- test %>%
  mutate(grad_nn = predict(mod_nn, newdata=test,type="class"))

test %>% group_by(grad_nn, GRAD) %>% tally()

########################## Logistic regression
mod_lr <- glm (form, data = train, family = binomial)
summary(mod_lr)

#mod_lr<-update(mod_lr, . ~ . - INGRESO_FAMILIAR )

pred_lr <- predict(mod_lr, type = 'response')

#confusion matrix
table(train$GRAD, pred_lr > 0.5)

library(ROCR)
ROCRpred <- prediction(pred_lr, train$GRAD)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



#test data
pred_lr <- predict(mod_lr, type = 'response',newdata=test)
table(test$GRAD, pred_lr > 0.5)
ROCRpred <- prediction(pred_lr, test$GRAD)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#plot glm
library(ggplot2)
ggplot(train, aes(x=GPA_1ER_ANO, y=GRAD)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)


##### Model for freshmen (Freshmen GPA not used)
form <- as.formula("GRAD ~ Year + Gender + Major + Rel_Stud_GPA +
                   Apt_Verbal + Apt_Matem+Aprov_Ingles+Aprov_Matem+Aprov_Espanol")
mod_tree_hs <- rpart(form, data = train)

library(partykit)
plot(as.party(mod_tree_hs))

printcp(mod_tree)

train <- train %>%
  mutate(grad_dtreehs = predict(mod_tree, type = "class"))

train %>% group_by(grad_dtreehs, GRAD) %>% tally()





