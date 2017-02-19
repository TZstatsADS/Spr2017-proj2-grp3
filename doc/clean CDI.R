cdi <- read.csv("./data/CDI.csv")
questions <- levels(cdi$Question)
related_q <- questions[c(6,30,31,32,40,97,116,117,118,119,120,121,122,164,165,166,167)]
related_q

#All teeth lost among adults aged >= 65 years
a<- cdi[cdi$Question==related_q[1],]
a<-a[!is.na(a$DataValueAlt),] #clean rows that has NA value
a<-a[a$Stratification1=="Overall",] #Get overall data
a<-a[a$DataValueTypeID=="CrdPrev",] #Get CrdPrev data
unique(a$YearEnd)
a2012<- a[a$YearStart==2012,]
a2014<- a[a$YearStart==2014,]
unique(a2012$LocationDesc)

#Cancer of the lung and bronchus, incidence
b<- cdi[cdi$Question==related_q[2],]
b<- b[!is.na(b$DataValueAlt),]
b<- b[b$DataValueTypeID=="AvgAnnNmbr",]

#Cancer of the lung and bronchus, mortality
c<- cdi[cdi$Question==related_q[3],]
c<- c[!is.na(c$DataValueAlt),]
c<- c[c$DataValueTypeID=="AvgAnnNmbr",]

#Cancer of the oral cavity and pharynx, mortality
d<- cdi[cdi$Question==related_q[4],]
d<- d[!is.na(d$DataValueAlt),]
d<- d[d$DataValueTypeID=="AvgAnnNmbr",]

#Current asthma prevalence among adults aged >= 18 years
e<- cdi[cdi$Question==related_q[5],]
e<-e[!is.na(e$DataValueAlt),] #clean rows that has NA value
e<-e[e$Stratification1=="Overall",] #Get overall data
e<-e[e$DataValueTypeID=="CrdPrev",] #Get CrdPrev data
unique(e$YearEnd)
e2012 <- e[e$YearStart==2012,]
e2012 <- e2012[,c(1,2,7,12)]
write.table(e2012,file = "./output/asthma_prevalence_2012.csv")
e2013 <- e[e$YearStart==2013,]
e2013 <- e2013[,c(1,2,7,12)]
write.table(e2013,file = "./output/asthma_prevalence_2013.csv")
e2014 <- e[e$YearStart==2014,]
e2014 <- e2014[,c(1,2,7,12)]
write.table(e2014,file = "./output/asthma_prevalence_2014.csv")

#Invasive cancer of the oral cavity or pharynx, incidence
f <-cdi[cdi$Question==related_q[6],]
f<- f[!is.na(f$DataValueAlt),]
f<- f[f$DataValueTypeID=="AvgAnnNmbr",]


#Mortality from cerebrovascular disease (stroke)
g<- cdi[cdi$Question==related_q[7],]
g<-g[!is.na(g$DataValueAlt),] #clean rows that has NA value
g<-g[g$Stratification1=="Overall",] #Get overall data
g<-g[g$DataValueTypeID=="Nmbr",] #Get CrdPrev data
unique(g$YearStart)
g2010<-g[g$YearStart==2010,]
g2010 <- g2010[,c(1,2,7,12)]
write.table(g2010,file = "./output/mortality_stroke_2010.csv")
g2011<-g[g$YearStart==2011,]
g2011 <- g2011[,c(1,2,7,12)]
write.table(g2011,file = "./output/mortality_stroke_2011.csv")
g2012<-g[g$YearStart==2012,]
g2012 <- g2012[,c(1,2,7,12)]
write.table(g2012,file = "./output/mortality_stroke_2012.csv")
g2013<-g[g$YearStart==2013,]
g2013 <- g2013[,c(1,2,7,12)]
write.table(g2013,file = "./output/mortality_stroke_2013.csv")
g2014<-g[g$YearStart==2014,]
g2014 <- g2014[,c(1,2,7,12)]
write.table(g2014,file = "./output/mortality_stroke_2014.csv")

#Mortality from coronary heart disease
h <- cdi[cdi$Question==related_q[8],]
h <- h[!is.na(h$DataValueAlt),] #clean rows that has NA value
h <- h[h$Stratification1=="Overall",] #Get overall data
h <- h[h$DataValueTypeID=="Nmbr",] #Get CrdPrev data
unique(h$YearStart)
h2010<-h[h$YearStart==2010,]
h2010 <- h2010[,c(1,2,7,12)]
write.table(h2010,file = "./output/mortality_coronary_heart_disease_2010.csv")
h2011<-h[h$YearStart==2011,]
h2011 <- h2011[,c(1,2,7,12)]
write.table(h2011,file = "./output/mortality_coronary_heart_disease_2011.csv")
h2012<-h[h$YearStart==2012,]
h2012 <- h2012[,c(1,2,7,12)]
write.table(h2012,file = "./output/mortality_coronary_heart_disease_2012.csv")
h2013<-h[h$YearStart==2013,]
h2013 <- h2013[,c(1,2,7,12)]
write.table(h2013,file = "./output/mortality_coronary_heart_disease_2013.csv")
h2014<-h[h$YearStart==2014,]
h2014 <- h2014[,c(1,2,7,12)]
write.table(h2014,file = "./output/mortality_coronary_heart_disease_2014.csv")

#Mortality from diseases of the heart
i <- cdi[cdi$Question==related_q[9],]
i <- i[!is.na(i$DataValueAlt),]
i <- i[i$Stratification1=="Overall",]
i <- i[i$DataValueTypeID=="Nmbr",]
unique(i$YearStart)
i2010<-i[i$YearStart==2010,]
i2010 <- i2010[,c(1,2,7,12)]
write.table(i2010,file = "./output/mortality_heart_disease_2010.csv")
i2011<-i[i$YearStart==2011,]
i2011 <- i2011[,c(1,2,7,12)]
write.table(i2011,file = "./output/mortality_heart_disease_2011.csv")
i2012<-i[i$YearStart==2012,]
i2012 <- i2012[,c(1,2,7,12)]
write.table(i2012,file = "./output/mortality_heart_disease_2012.csv")
i2013<-i[i$YearStart==2013,]
i2013 <- i2013[,c(1,2,7,12)]
write.table(i2013,file = "./output/mortality_heart_disease_2013.csv")
i2014<-i[i$YearStart==2014,]
i2014 <- i2014[,c(1,2,7,12)]
write.table(i2014,file = "./output/mortality_heart_disease_2014.csv")

#Mortality from heart failure
j <- cdi[cdi$Question==related_q[10],]
j <- j[!is.na(j$DataValueAlt),]
j <- j[j$Stratification1=="Overall",]
j <- j[j$DataValueTypeID=="Nmbr",]
unique(j$YearStart)
j2010<-j[j$YearStart==2010,]
j2010 <- j2010[,c(1,2,7,12)]
write.table(j2010,file = "./output/mortality_heart_failure_2010.csv")
j2011<-j[j$YearStart==2011,]
j2011 <- j2011[,c(1,2,7,12)]
write.table(j2011,file = "./output/mortality_heart_failure_2011.csv")
j2012<-j[j$YearStart==2012,]
j2012 <- j2012[,c(1,2,7,12)]
write.table(j2012,file = "./output/mortality_heart_failure_2012.csv")
j2013<-j[j$YearStart==2013,]
j2013 <- j2013[,c(1,2,7,12)]
write.table(j2013,file = "./output/mortality_heart_failure_2013.csv")
j2014<-j[j$YearStart==2014,]
j2014 <- j2014[,c(1,2,7,12)]
write.table(j2014,file = "./output/mortality_heart_failure_2014.csv")

#Mortality from total cardiovascular disease
k <- cdi[cdi$Question==related_q[11],]
k <- k[!is.na(k$DataValueAlt),]
k <- k[k$Stratification1=="Overall",]
k <- k[k$DataValueTypeID=="Nmbr",]
unique(k$YearStart)
k2010<-k[k$YearStart==2010,]
k2010 <- k2010[,c(1,2,7,12)]
write.table(k2010,file = "./output/mortality_card_disease_2010.csv")
k2011<-k[k$YearStart==2011,]
k2011 <- k2011[,c(1,2,7,12)]
write.table(k2011,file = "./output/mortality_card_disease_2011.csv")
k2012<-k[k$YearStart==2012,]
k2012 <- k2012[,c(1,2,7,12)]
write.table(k2012,file = "./output/mortality_card_disease_2012.csv")
k2013<-k[k$YearStart==2013,]
k2013 <- k2013[,c(1,2,7,12)]
write.table(k2013,file = "./output/mortality_card_disease_2013.csv")
k2014<-k[k$YearStart==2014,]
k2014 <- k2014[,c(1,2,7,12)]
write.table(k2014,file = "./output/mortality_card_disease_2014.csv")

# Mortality with chronic obstructive pulmonary disease as underlying cause among adults aged >=45 years
l <- cdi[cdi$Question==related_q[12],]
l <- l[!is.na(l$DataValueAlt),]
l <- l[l$Stratification1=="Overall",]
l <- l[l$DataValueTypeID=="Nmbr",]
unique(l$YearStart)
l2010<-l[l$YearStart==2010,]
l2010 <- l2010[,c(1,2,7,12)]
write.table(l2010,file = "./output/mortality_pulmonary_disease_2010.csv")
l2011<-l[l$YearStart==2011,]
l2011 <- l2011[,c(1,2,7,12)]
write.table(l2011,file = "./output/mortality_pulmonary_disease_2011.csv")
l2012<-l[l$YearStart==2012,]
l2010 <- l2010[,c(1,2,7,12)]
write.table(l2010,file = "./output/mortality_pulmonary_disease_2010.csv")
l2013<-l[l$YearStart==2013,]
l2014<-l[l$YearStart==2014,]

# Mortality with chronic obstructive pulmonary disease as underlying or contributing cause among adults aged >=45 years
m <- cdi[cdi$Question==related_q[13],]
m <- m[!is.na(m$DataValueAlt),]
m <- m[m$Stratification1=="Overall",]
m <- m[m$DataValueTypeID=="Nmbr",]
unique(m$YearStart)
m2010<-m[m$YearStart==2010,]
m2011<-m[m$YearStart==2011,]
m2012<-m[m$YearStart==2012,]
m2013<-m[m$YearStart==2013,]
m2014<-m[m$YearStart==2014,]

#Prevalence of chronic obstructive pulmonary disease among adults >= 18
n <- cdi[cdi$Question==related_q[14],]
n <- n[!is.na(n$DataValueAlt),]
n <- n[n$Stratification1=="Overall",]
n <- n[n$DataValueTypeID=="CrdPrev",]
unique(n$YearStart)
n2012<-n[n$YearStart==2012,]
n2013<-n[n$YearStart==2013,]
n2014<-n[n$YearStart==2014,]

#Prevalence of chronic obstructive pulmonary disease among adults >= 45 years
o <- cdi[cdi$Question==related_q[15],]
o <- o[!is.na(o$DataValueAlt),]
o <- o[o$Stratification1=="Overall",]
o <- o[o$DataValueTypeID=="CrdPrev",]
unique(o$YearStart)
o2012<-o[o$YearStart==2012,]
o2013<-o[o$YearStart==2013,]
o2014<-o[o$YearStart==2014,]

#Prevalence of current smoking among adults >= 18 with diagnosed chronic obstructive pulmonary disease
p <- cdi[cdi$Question==related_q[16],]
p <- p[!is.na(p$DataValueAlt),]
p <- p[p$Stratification1=="Overall",]
p <- p[p$DataValueTypeID=="CrdPrev",]
unique(p$YearStart)
p2012<-p[p$YearStart==2012,]
p2013<-p[p$YearStart==2013,]
p2014<-p[p$YearStart==2014,]

#Prevalence of current smoking among adults >= 45 years with diagnosed chronic obstructive pulmonary disease
q <- cdi[cdi$Question==related_q[17],]
q <- q[!is.na(q$DataValueAlt),]
q <- q[q$Stratification1=="Overall",]
q <- q[q$DataValueTypeID=="CrdPrev",]
unique(q$YearStart)
q2012<-q[q$YearStart==2012,]
q2013<-q[q$YearStart==2013,]
q2014<-q[q$YearStart==2014,]












