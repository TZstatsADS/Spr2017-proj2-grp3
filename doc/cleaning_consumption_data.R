

df1 <- read.csv("./data/Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
df2 <- read.csv("./data/Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")
df2$DisplayOrder <- NULL
df2$SubMeasureID <- NULL

df$TopicType <- NULL

df <- rbind(df1,df2)