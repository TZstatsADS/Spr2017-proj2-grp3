pop <- read.csv("./data/population.csv")
pop <- pop[-c(1,2,3,4,5),]
pop <- pop[,c(5,6,9,10,11,12)]
write.table(pop,file = "./output/population20102014.csv",row.names = F)
