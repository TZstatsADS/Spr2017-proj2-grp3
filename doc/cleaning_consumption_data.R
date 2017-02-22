

df1 <- read.csv("./data/Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
df2 <- read.csv("./data/Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")
df2$DisplayOrder <- NULL
df2$SubMeasureID <- NULL

df <- rbind(df1,df2)

df$TopicType <- NULL
df$DataSource <- NULL
df$StratificationID1 <- NULL
df$StratificationID2 <- NULL
df$StratificationID3 <- NULL
df$StratificationID4 <- NULL
df$TopicTypeId <- NULL
df$TopicId <- NULL
df$MeasureId <- NULL
df$GeoLocation <- NULL
df$Sample_Size <- NULL
df$High_Confidence_Limit <- NULL
df$Low_Confidence_Limit <- NULL
df$Data_Value_Std_Err <- NULL
df$Education <- NULL

df <- subset(df, Race=="All Races")
df$Race <- NULL

df1 <- subset(df, TopicDesc=="Cigarette Use (Adults)")

smoking_frequency <- subset(df1, MeasureDesc=="Smoking Frequency")
smoking_status <- subset(df1, MeasureDesc=="Smoking Status")
curently_smoking <- subset(df1, MeasureDesc=="Current Smoking")
