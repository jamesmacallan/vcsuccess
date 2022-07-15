get(wd) #gets working directory
vcsuccess <- readr::read_csv("C:/Users/jmand/OneDrive/Documents/CSVs/vcsuccess.csv") #reads csv, my csv only has two columns
#I plan to import a different csv with the other usable data.
spec(vcsuccess) #specifies how r is viewing the csv, i used this to make sure it was reading the correct columns
class(vcsuccess) #show if it can be seen as dataframe i think

df1<-data.frame(vcsuccess) #creates dataframe
complete.cases(df1) #gives boolean for complete x and y columns
na_vec <- which(!complete.cases(df1)) #specifies non complete columns (notice the exclamation point)
df1 <- df1[-na_vec,] #removes anything with na
#print(df1)
df1$status<-ifelse(df1$status=='operating'||df1$status=='acquired',1,0) #creates value for qualitative data
df1$funding_total_usd<-gsub(",","",as.character(df1$funding_total_usd)) #gets rid of commas in the funding data
df1 <- subset(df1, funding_total_usd != "-") #gets rid of hyphon data in total funding
print(df1)

plot(x = df1$funding_total_usd,
     y = df1$status,
     xlab = "Funding",
     ylab = "Status",
     main = "Funding vs Status")
