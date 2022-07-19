library(dplyr)

get(wd) #gets working directory
vcsuccess <- readr::read_csv("C:filepath")
spec(vcsuccess) #specifies how r is viewing the csv, i used this to make sure it was reading the correct columns
class(vcsuccess) #show if it can be seen as dataframe i think

df1<-data.frame(vcsuccess) #creates dataframe
#print(df1)
#df1$status <- if(df1$status=='acquired', "-")
#df1 <- subset(df1,status != "acquired")#hyphonates any status that's acquired to get rid of later
#complete.cases(df1) #gives boolean for complete x and y columns
#na_vec <- which(!complete.cases(df1)) #specifies non complete columns (notice the exclamation point)
#df1 <- df1[-na_vec,] #removes anything with na
#print(df1)
df1$status <- ifelse(df1$status=='operating',1,0) #creates value for qualitative data ||df1$status=='acquired',
df1$funding_total_usd<-gsub(",","",as.character(df1$funding_total_usd)) #gets rid of commas in the funding data
df1 <- subset(df1, funding_total_usd != "-") #gets rid of hyphon data in total funding
print(df1)
sum(with(df1,status == 1))
sum(with(df1,status == 0))

# truncated_status <- function(df1, status_to_change, n) {
#   df1 %>%
#     filter(status %in% status_to_change) %>%
#     group_by(status) %>%
#     #slice_sample(n = n) %>%
#     ungroup %>%
#     bind_rows(df1 %>% filter(!status %in% status_to_change))
# }
# 
# new_df <- df1 %>% truncated_status('operating',4000)
# new_df %>% count(status)