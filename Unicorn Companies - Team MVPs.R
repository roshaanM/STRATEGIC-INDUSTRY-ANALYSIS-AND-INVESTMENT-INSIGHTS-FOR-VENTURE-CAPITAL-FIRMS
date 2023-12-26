### Load libraries
library(stargazer)
library(tidyverse)
library(lfe) 
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)
library (tidyr)
library(data.table)
library(datasets)
library(lubridate)
library(ggthemes)
library(class)
library(scales)
options(scipen = 999)

# -------------------------- File Paths ---------------------------------------------------------------

#Anjas File Path
#file_path <-"/Users/anjaskapur/Documents/Info Visuals/Group-assig/Unicorn_Companies.csv" 
#unicorn <- tibble(read_csv(file_path))

#Saurabh File Path
file_path <- "/Users/saurabhkadam/Documents/MSBA/Semesters/Spring 2023/MISM 6210 - Information Visualisations and Dashboards for Business/Team Project/Datasets/Unicorn_Companies.csv"
unicorn <- tibble(read_csv(file_path))

#Load your file path here:

#-------------------------- Data Wrangling --------------------------------------------

###Part 1 - Checking Data 

##Rows and column
dim(unicorn)
#1074 Rows and 10 Columns

##Checking NA values
colSums(is.na(unicorn))
#No NA values found

##Checking Null Values
is.null(unicorn)
#No null data

##Structure of Data
str(unicorn)

##Filter Put year from Date Joined to rech $1B
#unicorn$Year_1B<- year(unicorn$`Date Joined`)

#Change column names

colnames(unicorn) <- c("Company","Valuation","Date_Joined","Industry","City","Country","Continent","Year_Founded","Funding","Investors")


#Check correlation between numeric columns

#Cleaning Valuation Column
unicorn$Valuation <- gsub('B','',unicorn$Valuation) #Removes 'B' from Valuation column
unicorn$Valuation <- gsub('[^[:alnum:] ]','',unicorn$Valuation) #Removes special character - '$'
unicorn$Valuation <- as.numeric(unicorn$Valuation)
unicorn$Valuation <- unicorn$Valuation * 1000000000

#remove unknown values from funding 
unicorn <- filter(unicorn, unicorn$Funding != "Unkown")


#Define a function to convert the funding values to numeric
convert_Funding <- function(Funding_str) {
  # Remove the dollar sign and extract the numeric part
  Funding_num <- as.numeric(gsub("\\$|B|M", "", Funding_str))
  #Convert B notation to numeric
  if (grepl("B", Funding_str)) {
    Funding_num <- Funding_num * 1000000000
  }
  #Convert M notation to numeric
  if (grepl("M", Funding_str)) {
    Funding_num <- Funding_num * 1000000
  }
  #Return the numeric funding value
  return(Funding_num)
}

#Apply the function to the funding column
unicorn$Funding_num <- sapply(unicorn$Funding, convert_Funding)

unicorn$Funding <- unicorn$Funding_num
unicorn = subset(unicorn, select = -c(Funding_num) )

#Data Consistency  - Artificial intelligence & Artificial Intelligence
unicorn$Industry <- gsub("Artificial intelligence", "Artificial Intelligence", unicorn$Industry)


#remove unknown values from funding 
unicorn <- filter(unicorn, unicorn$Funding != "Unkown")
# print the converted data frame
unicorn_num <- select_if(unicorn,is.numeric)

# calculate the correlation matrix
corr_matrix <- cor(unicorn_num)

# print the correlation matrix
print(corr_matrix)

##Descriptive statistics
summary(unicorn)

#Group By industry - Valuation
unicorn %>%
  group_by(Industry) %>%
  dplyr::summarize(Sum = sum(Valuation, na.rm=TRUE))

#Visualization for Total Valuation of Industries Globally in USD

ggplot(unicorn, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat="summary", fun="sum") +
  ggtitle("Total Valuation of Industries Globally in USD") + 
  xlab("Industry") +
  ylab("Total Valuation in USD") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Group By industry - Funding 
unicorn %>%
  group_by(Industry) %>%
  dplyr::summarize(Sum = sum(Funding, na.rm=TRUE))

#Visualization for Funding Received by Industries Globally in USD

ggplot(unicorn, aes(y = Funding,x = reorder(as.factor(Industry), Funding, sum))) +
  geom_bar(stat="summary", fun="sum") +
  ggtitle("Funding Received by Industries Globally in USD") + 
  xlab("Industry") +
  ylab("Total Funding in USD") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Group by continent and industry
summary_data <- unicorn %>% 
  group_by(Continent, Industry) %>% 
  summarize(Total_Valuation = sum(Valuation))

Africa_df <- filter(unicorn, unicorn$Continent == 'Africa')
North_America_df <- filter(unicorn, unicorn$Continent == 'North America')
Oceania_df <- filter(unicorn, unicorn$Continent == 'Oceania')
South_America_df <- filter(unicorn, unicorn$Continent == 'South America')
Asia_df <- filter(unicorn, unicorn$Continent == 'Asia')
Europe_df <- filter(unicorn, unicorn$Continent == 'Europe')

#Visualization for North America in Ascending Order of Industry and Total Valuations

ggplot(North_America_df, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Valuations of Industries in North America") +
  ylab("Valuation in USD") +
  xlab("CIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Visualization for South America in Ascending Order of Industry and Total Valuations

ggplot(South_America_df, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Valuations of Industries in South America") +
  ylab("Valuation in USD") +
  xlab("CIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Visualization for Oceania in Ascending Order of Industry and Total Valuations

ggplot(Oceania_df, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Valuations of Industries in Oceania") +
  ylab("Valuation in USD") +
  xlab("CIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Visualization for Africa in Ascending Order of Industry and Total Valuations

ggplot(Africa_df, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Valuations of Industries in Africa") +
  ylab("Valuation in USD") +
  xlab("CIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Visualization for Asia in Ascending Order of Industry and Total Valuations

ggplot(Asia_df, aes(y = Valuation, x = reorder(as.factor(Industry), Valuation, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Valuations of Industries in Asia") +
  ylab("Valuation in USD") +
  xlab("CIndustry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##Convert YYYY/MM/DD to YYYY format
unicorn$Date_Joined <- format(as.Date(unicorn$Date_Joined, format="%Y/%m/%d"), "%Y")

#Time taken to reach unicorn status
unicorn$Year_Taken <- as.numeric(unicorn$Date_Joined) - as.numeric(unicorn$Year_Founded)

#delete nonsense data
unicorn <- unicorn[unicorn$Company != "Yidian Zixun", ]

#unicorn$Investors <- sapply(strsplit(as.character(unicorn$Investors), ","), function(x) paste0(unique(x), collapse = ","))
#unicorn$Investors <- unique(unicorn$Investors)
#unicorn$Investors <- unique(na.omit(unicorn$Investors))
#separates comma separated values in investors

unicorn$Investors <- sapply(strsplit(as.character(unicorn$Investors), ","), function(x) {
  x <- trimws(x)  # remove leading/trailing whitespace
  paste0(unique(x), collapse = ",")
})

unicorn$Investors <- sapply(strsplit(as.character(unicorn$Investors), ","), function(x) paste0(unique(x), collapse = ","))
unicorn <- separate(unicorn, Investors, into = paste0("Investor_", 1:4), sep = ",", fill = "right")

#Replace na values wiht blanks
#unicorn[is.na(unicorn)] <- ""

#count total number of investors

# Define range of columns to count and sum across
Num_Investors <- c("Investor_1", "Investor_2", "Investor_3", "Investor_4")

# Count number of non-NA values across the columns
# Define range of columns to count and sum across

# Count number of non-NA values across the columns
unicorn$Num_Investors <- rowSums(!is.na(unicorn[, Num_Investors]))
unicorn$Date_Joined <- as.numeric(unicorn$Date_Joined)

#convert to excel
library(openxlsx)
wb <- createWorkbook()

# Add a new worksheet to the workbook
addWorksheet(wb, "Unicorn Data by Company")

# Write the data frame to the worksheet
writeData(wb, "Unicorn Data by Company", unicorn)

# Save the workbook as an Excel file
saveWorkbook(wb, "MISM 6210 - MVPs - Unicorn Data - Company.xlsx", overwrite = TRUE)

unicorn_summary <- unicorn %>%
  group_by(Continent, Country, City, Industry) %>%
  dplyr::summarize(Funding = mean(Funding, na.rm=TRUE),
                   Valuation = mean(Valuation, na.rm=TRUE),
                   Founding_Year = mean(Year_Founded, na.rm=TRUE),
                   Date_Joined = mean(as.numeric(Date_Joined), na.rm=TRUE),
                   Years_Taken = mean(Year_Taken, na.rm=TRUE),
                   Num_Investor = mean(Num_Investors, na.rm=TRUE))

unicorn_summary$Valuation - unicorn_summary$Funding = unicorn_summary$Generated_Value
unicorn_summary$Years_Taken <- round(unicorn_summary$Years_Taken)
unicorn_summary$Num_Investor <- round(unicorn_summary$Num_Investor)
unicorn_summary$Founding_Year <- round(unicorn_summary$Founding_Year)
unicorn_summary$Date_Joined <- round(unicorn_summary$Date_Joined)
unicorn_summary$

ggplot(unicorn_summary, aes(y = Funding, x = reorder(as.factor(Country), Funding, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Funding Received by Country") +
  ylab("Funding Received") +
  xlab("Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(unicorn_summary, aes(y = Funding, x = reorder(as.factor(Country), Funding, sum))) +
  geom_bar(stat = "summary", fun = "sum") +
  ggtitle("Funding Received by Country") +
  ylab("Funding Received") +
  xlab("Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

unicorn_visualisation_export <- createWorkbook()

# Add a new worksheet to the workbook
addWorksheet(unicorn_visualisation_export, "Unicorn Summary")

# Write the data frame to the worksheet
writeData(unicorn_visualisation_export, "Unicorn Summary", unicorn_summary)

# Save the workbook as an Excel file
saveWorkbook(unicorn_visualisation_export, "MISM 6210 - MVPs - Unicorn Export.xlsx", overwrite = TRUE)

