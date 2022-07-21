#R project - Intro to ML

rm(list=ls())

df_inv <- read.csv(file = 'C:/Users/apurv/OneDrive/Desktop/investments_VC.csv')

head(df_inv,5)

dim(df_inv)

colnames(df_inv)
ncol(df_inv)

select_region <- c("Atlanta","Austin","Boston","Chicago","Los Angeles","New York City","Seattle","SF Bay Area")
df_usa <- subset(df_inv, country_code == "USA" & (region %in% select_region))

dim(df_usa)                                 

#df_usa is a subset of the entire dataset with specific regions within usa.

summary(df_usa)

#Cleaning data by finding null values
unique(df_usa$region) #unique Values of region

na <- sum(is.na(df_usa))
na # Null values in dataset
#apply(X=is.na(df_usa),MARGIN=1,FUN=sum)
#df_usa <-subset(df_usa,!is.na(df_usa))
#df_usa

df_usa <- na.omit(df_usa)

dim(df_usa)

#Checkpoint for Cleaning
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa.csv", row.names = TRUE)

#Data cleaning

df_usa$funding_total_usd <- as.numeric(gsub(",","",df_usa$funding_total_usd))
typeof(df_usa$funding_total_usd)
#df['funding_total_usd']=df['funding_total_usd'].str.replace(',','') # removing commas from funding_total_usd column

names(df_usa) <- gsub(" ", "_", names(df_usa)) #function to replace blanks in column names with an underscore
#df['funding_total_usd']=df['funding_total_usd'].str.replace(' ','')#removing extra space from funding_total_usd column

##Checkpoint for Cleaning
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa1.csv", row.names = TRUE)

df_usa$funding_total_usd[is.na(df_usa$funding_total_usd)] <- 0 # converting Na to 0 as earlier some values were -
#df['funding_total_usd']=df['funding_total_usd'].str.replace('-','0') #removing - from funding_total_usd column and replacing with 0

#df['funding_total_usd'] = pd.to_numeric(df['funding_total_usd'])# turning column to number
summary(df_usa)

#Adding a new column to categorize market as Technology or Non-Technology
#277 market categorized as Technology, rest as Non-Technology

dim(df_usa)

#Checkpoint for Cleaning
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa_labelled.csv", row.names = TRUE)


######### All in Python need to convert to R ########

#turning all date columns in to date

df_usa$founded_at <- as.Date(df_usa$founded_at, "%m/%d/%Y")
#df['founded_at'] =  pd.to_datetime(df['founded_at'], format='%Y-%m-%d', errors = 'coerce') # conveting column into date and ignoring errors

df_usa$first_funding_at <- as.Date(df_usa$first_funding_at,format= "%m/%d/%Y")
#df['first_funding_at'] =  pd.to_datetime(df['first_funding_at'], format='%Y-%m-%d', errors = 'coerce')  # conveting column into date and ignoring errors

df_usa$last_funding_at <- as.Date(df_usa$last_funding_at,format= "%m/%d/%Y")
#df['last_funding_at'] =  pd.to_datetime(df['last_funding_at'], format='%Y-%m-%d', errors = 'coerce')  # conveting column into date and ignoring errors

df_usa$founded_year <- as.Date(df_usa$founded_year, format="%Y")
head(df_usa$founded_at)
head(df_usa$first_funding_at)
head(df_usa$last_funding_at)
head(df_usa$founded_year)
#df['founded_year'] =  pd.to_datetime(df['founded_year'], format='%Y', errors = 'coerce') # conveting column into date and ignoring errors

#df_usa$founded_month <- as.Date(df_usa$founded_month,format= "%Y-%m")
#head(df_usa$founded_month)
#df['founded_month'] =  pd.to_datetime(df['founded_month'], format='%Y-%m', errors = 'coerce') # conveting column into date and ignoring errors

df_usa$market <- trimws(df_usa$market, which = c("both"))
#df.market = df.market.str.strip() #removing space from beginning and end of market column
#add technology column - keep only technology

dim(df_usa)

tech_list <- c("News","Curated Web","Analytics","Software","Electronics","Biotechnology","E-Commerce",
               "Software","Designers","Enterprise Software","Big Data","Hardware + Software","Social Fundraising",
               "Corporate IT","Bitcoin","Mobile","Electronic Health Records","Automotive","Web Hosting","Clean Technology",
               "Web Design","Cloud Computing","Technology","Video","Predictive Analytics","Telecommunications","Consulting",
               "Design","Social Games","Business Productivity","Communications Hardware","Health Care Information Technology",
               "Advertising Platforms","Cloud Infrastructure","Online Travel","Social Media","Cloud Management","Semiconductors",
               "3D","Technical Continuing Education","Internet of Things","Android","Semantic Search","Internet",
               "Business Intelligence","Financial Services","Ad Targeting","Development Platforms","Bio-Pharm","Semantic Web",
               "Assisitive Technology","SaaS","Data Visualization","Application Platforms","Social Commerce","Mobile Advertising",
               "Therapeutics","Wireless","Real Time","Finance Technology","Cloud Data Services","Machine Learning","Twitter Applications",
               "Sensors","Developer APIs","Aerospace","iPhone","Information Services","Consumer Goods","Startups","iPad",
               "Stock Exchanges","Computers","Big Data Analytics","Retail","Social Media Monitoring","Reviews and Recommendations",
               "Broadcasting","Trading","Pharmaceuticals","Smart Grid","Nanotechnology","Chemicals","Mobile Security","Databases",
               "Online Dating","Mobile Enterprise","Online Rental","Apps","Facebook Applications","Mobile Devices","Developer Tools",
               "PaaS","Application Performance Monitoring","Mobile Software Tools","QR Codes","Virtualization","Data Integration",
               "Mobile Games","Mobile Payments","Mobile Commerce","Web Tools","Payments","Video Games","Consumer Electronics",
               "Diabetes","Diagnostics","Networking","Tracking","E-Commerce Platforms","Productivity Software","Home Automation",
               "Email","Artificial Intelligence","Tablets","Industrial Automation","SEO","Computer Vision","Augmented Reality",
               "Life Sciences","Service Providers","Web CMS","Enterprises","Video Conferencing","Video Streaming","Photography",
               "Online Shopping","Email Marketing","Social Search","Cyber Security","Risk Management","Hardware","Social Network Media",
               "Information Technology","Cloud Security","Fantasy Sports","Web Development","IT and Cybersecurity","Open Source",
               "Mobile Health","Social Media Marketing","Infrastructure","Semiconductor Manufacturing Equipment","Robotics",
               "Blogging Platforms","Photo Sharing","Social Media Advertising","Image Recognition","3D Technology","E-Books",
               "Tech Field Support","Defense","File Sharing","Private Social Networking","Printing","Social Media Platforms",
               "Customer Support Tools","Biometrics","Navigation","Banking","Communications Infrastructure","Data Centers",
               "iOS","Project Management","Customer Service","Sales Automation","iPod Touch","Electric Vehicles","Social Bookmarking",
               "Google Apps","IaaS","Online Gaming","Proximity Internet","Browser Extensions","Information Security",
               "Meeting Software","Recipes","Online Scheduling","Embedded Hardware and Software","Product Search","Network Security",
               "Consumer Internet","Energy Management","Data Mining","B2B","Cosmetics","Enterprise Application","Data Security",
               "Video on Demand","App Discovery","Drones","Intelligent Systems","Environmental Innovation","Energy Efficiency",
               "Virtual Workforces","Bioinformatics","Renewable Energies","Internet TV","Business Information Systems",
               "Corporate Wellness","Speech Recognition","Retail Technology","Online Video Advertising","Green Consumer Goods",
               "MicroBlogging","Social CRM","Enterprise Resource Planning","Algorithms","Web Browsers","Presentations",
               "3D Printing","Mobile Coupons","Linux","Audio","Online Reservations","Batteries","App Marketing","Corporate Training",
               "Dental","Biotechnology and Semiconductor","Social + Mobile + Local","Human Resource Automation","Internet Infrastructure",
               "Visualization","Mobile Shopping","Consumer Behavior","Business Analytics","Product Development Services",
               "App Stores","Product Design","Virtual Goods","Cyber","Social Innovation","Enterprise Search","Systems",
               "Energy IT","Social Buying","Mac","Mechanical Solutions","Government Innovation","IT Management","Price Comparison",
               "Enterprise 2.0","Virtual Currency","Photo Editing","Google Glass","High Tech","Innovation Engineering",
               "Mobile Infrastructure","Gps","Mining Technologies","Data Center Infrastructure","Data Center Automation",
               "Social Media Management","Internet Technology","Mobile Video","Social Business","Cosmetic Surgery",
               "Social Opinion Platform","Windows Phone 7","Video Chat","Early-Stage Technology","Fraud Detection",
               "Enterprise Purchasing","Electrical Distribution","Advanced Materials","Mobile Analytics","Enterprise Security",
               "Health Services Industry","Reading Apps","CAD","Human Computer Interaction","Text Analytics","RFID",
               "Material Science","Service Industries","Natural Language Processing")
df_usa$market_type <- ifelse(df_usa$market %in% tech_list,"Technology","Non-Technology")

#Checkpoint
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa_labelled1.csv", row.names = TRUE)

unique(df_usa$market_type)
#Drop rows with market_type - Non- Technology

df_usa <- subset(df_usa, df_usa$market_type == "Technology")

dim(df_usa)

#EDA

df_usa$diff_funding <- df_usa$last_funding_at - df_usa$first_funding_at # finding the difference in days between first and last funding dates
head(df_usa$diff_funding)

#mean(df_usa$diff_funding) # mean is 312 days which is about 1 year
#df_usa$diff_funding_months = (df_usa$last_funding_at - df_usa$first_funding_at)/np.timedelta64(1, 'M') # turning the difference into months

#Dropping Columns
drop <- c('homepage_url', 'category_list','country_code', 'state_code', 'founded_at', 'founded_month', 'founded_quarter', 'founded_year', 
          'diff_first_funding_months', 'diff_funding', 'funding_total_usd', 'first_funding_at', 'last_funding_at')
df_usa = df_usa[,!(names(df_usa) %in% drop)]
dim(df_usa)

#Checkpoint
#write.csv(df_usa,"C:/Users/apurv/OneDrive/Desktop/df_usa_labelled1.csv", row.names = TRUE)
#dropping unecessary columns that I dont plan on using

#df1['diff_funding_year'] = round(df1['diff_funding_months']/12) # making new column that has difference in funding in year

#Histogram -- Will take care of this later as EDA : cut in dplyr
#df1.hist(column=['funding_rounds', 'seed', 'venture', 'equity_crowdfunding',
                 'undisclosed', 'convertible_note', 'debt_financing', 'angel', 'grant',
                 'private_equity', 'post_ipo_equity', 'post_ipo_debt',
                 'secondary_market', 'product_crowdfunding', 'round_A', 'round_B',
                 'round_C', 'round_D', 'round_E', 'round_F', 'round_G', 'round_H',
                 'diff_funding_year', 'total_investment'], bins=100, grid=False, figsize=(20,15), color='#86bf91', zorder=2, rwidth=0.9) 
#Creating histogram of all numerical values, all values are very skewed

#Creating categories of these numerical values based on the output from the describe data. Also creating new column for the categories
cat_invest = pd.cut(df2.total_investment, bins = [-1, 112500, 1400300, 8205200, 40079503000], labels=['low','low_medium','high_medium','high'])
#labeling total investment values as low, low medium, high medium and high based on their descriptive summary. 
df2.insert(0,'cat_total_investment',cat_invest) # creating new column called cat_total_investment
df['total_investment'] = df['seed'] + df['venture'] + df['equity_crowdfunding'] + df['undisclosed'] + df['convertible_note'] + df['debt_financing'] + df['angel'] + df['grant'] + df['private_equity'] + df['post_ipo_equity'] + df['post_ipo_debt'] + df['secondary_market'] + df['product_crowdfunding']
#creating new column for total investment
df['total_investment'].describe() # calculating the total investment for each company

#Creating Bins 
#creating categories of these numerical values based on the output from the describe data. Also creating new column for the categories
cat_invest = pd.cut(df2.total_investment, bins = [-1, 112500, 1400300, 8205200, 40079503000], labels=['low','low_medium','high_medium','high'])
#labeling total investment values as low, low medium, high medium and high based on their descriptive summary. 
df2.insert(0,'cat_total_investment',cat_invest) # creating new column called cat_total_investment

cat_diff_funding_year = pd.cut(df2.diff_funding_year, bins = [-1, 2, 49], labels=['low','high'])
#labeling diff_funding_year as low and high based on their descriptive summary. 
df2.insert(0,'cat_diff_funding_year',cat_diff_funding_year)# creating new column called cat_diff_funding_year

cat_funding_rounds = pd.cut(df2.funding_rounds, bins = [-1, 2, 20], labels=['low','high'])
#labeling funding_rounds as low and high based on their descriptive summary. 
df2.insert(0,'cat_funding_rounds',cat_funding_rounds)# creating new column called cat_funding_rounds

cat_seed = pd.cut(df2.seed, bins = [-1, 28000, 140000000], labels=['low','high'])
#labeling seed as low and high  based on their descriptive summary. 
df2.insert(0,'cat_seed',cat_seed)# creating new column called cat_seed

cat_venture = pd.cut(df2.venture, bins = [-1, 85038.5, 6000000, 2451000000], labels=['low','medium','high'])
#labeling venture as low, medium and high based on their descriptive summary. 
df2.insert(0,'cat_venture',cat_venture) # creating new column called cat_venture

#as a lot of the money columns have 0, we are turning them into new categories of 0 and 1
df_usa$equity_crowdfunding <- ifelse(df_usa$equity_crowdfunding < 1 , 0 , 1)
str(df_usa)
df_usa$undisclosed <- ifelse(df_usa$undisclosed < 1 , 0 , 1)
df_usa$convertible_note <- ifelse(df_usa$convertible_note < 1 , 0 , 1)
df_usa$debt_financing <- ifelse(df_usa$debt_financing < 1 , 0 , 1)
df_usa$angel <- ifelse(df_usa$angel < 1 , 0 , 1)
df_usa$grant <- ifelse(df_usa$grant < 1 , 0 , 1)
df_usa$private_equity <- ifelse(df_usa$private_equity < 1 , 0 , 1)
df_usa$post_ipo_equity <- ifelse(df_usa$post_ipo_equity < 1 , 0 , 1) 
df_usa$post_ipo_debt <- ifelse(df_usa$post_ipo_debt < 1 , 0 , 1)
df_usa$secondary_market <- ifelse(df_usa$secondary_market < 1 , 0 , 1)
df_usa$product_crowdfunding <- ifelse(df_usa$product_crowdfunding < 1 , 0 , 1)
df_usa$round_A <- ifelse(df_usa$round_A < 1 , 0 , 1)
df_usa$round_B <- ifelse(df_usa$round_B < 1 , 0 , 1)
df_usa$round_C <- ifelse(df_usa$round_C < 1 , 0 , 1)
df_usa$round_D <- ifelse(df_usa$round_D < 1 , 0 , 1)
df_usa$round_E <- ifelse(df_usa$round_E < 1 , 0 , 1)
df_usa$round_F <- ifelse(df_usa$round_F < 1 , 0 , 1)
df_usa$round_G <- ifelse(df_usa$round_G < 1 , 0 , 1)
df_usa$round_H <- ifelse(df_usa$round_H < 1 , 0 , 1)

library(tidyverse)

df_usa_v2 = df_usa

df_usa_v3 <- df_usa_v2 %>%
  mutate(status_label = ifelse(df_usa_v2$status =='closed',0, 
                               ifelse(df_usa_v2$status =='operating',1,2)))


df2['cat_total_investment'] = df2['cat_total_investment'].replace(['low','low_medium','high_medium','high'], [0, 1, 2, 3])
df2['cat_diff_funding_year'] = df2['cat_diff_funding_year'].replace(['low', 'high'], [0, 1])
df2['cat_funding_rounds'] = df2['cat_funding_rounds'].replace(['low', 'high'], [0, 1])
df2['cat_seed'] = df2['cat_seed'].replace(['low', 'high'], [0, 1])
df2['cat_venture'] = df2['cat_venture'].replace(['low','medium','high'], [0, 1, 3])

