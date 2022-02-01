### Load in necessary Libraries
library(dplyr)
library(tidyr)
library(gmodels)
library(rcompanion)
library(readxl)

### Load in the datasets for evaluation
Exiting_FC = read_excel('C:/Users/13612/Desktop/Course Final Project/Assignments/Week 1/Datasets/Children exiting foster care by age group.xlsx')
Juveniles = read_excel('C:/Users/13612/Desktop/Course Final Project/Assignments/Week 1/Datasets/Youth residing in juvenile detention, correctional and_or residential facilities.xlsx')
Juveniles_2 = read.csv('C:/Users/13612/Desktop/Course Final Project/Assignments/Week 1/Datasets/ezacjrp_export.csv')

##### Shared Data wrangling for the Evaluation Questions

## Exiting the Foster Care System dataset
# Changing the Age Group column to AgeGroup to avoid conflicts with codes
Exiting_Age = names(Exiting_FC)[names(Exiting_FC) == 'Age group'] <- "AgeGroup" 

# Getting rid of the percent rows in the DataFormat column
Exiting_FC = filter(Exiting_FC, DataFormat != 'Percent') 

# Changing the Data from a character to a numeric format
Exiting_FC = Exiting_FC %>% mutate(Data = as.numeric(Data)) 

# Filtering out only the 10 years that are needed and dropping the Puerto Rico rows from the Location column
Exiting_FC = Exiting_FC %>% filter((Location != 'Puerto Rico') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))

## Juvenile incarceration dataset

# Dropping the Rate per 100,000 rows in the DataFormat column
Juveniles = filter(Juveniles, DataFormat != 'Rate per 100,000') 

# Changing the Data from a character to a numeric format
Juveniles = Juveniles %>% mutate(Data = as.numeric(Data)) 

# Filtering out only the 10 years that are needed and dropping the Puerto Rico rows from the Location column
Juveniles = Juveniles %>% filter((Location != 'Puerto Rico') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))

### Data Exploration graphs

# Selecting the four columns wanted for the graphs
Tableau_Exiting = select(Exiting_FC, c('Location', 'AgeGroup', 'TimeFrame', 'Data')) 

# Dropping the United States rows from the Location column and dropping the Total rows from the AgeGroup column
Tableau_Exiting_Graph = Tableau_Exiting %>% filter((Location == 'United States') & (AgeGroup != 'Total'))

# Selecting only United States and Age Group Total
Tableau_Exiting_Graph2 =  Tableau_Exiting %>% filter((Location == 'United States') & (AgeGroup == 'Total'))

Tableau_Exiting_Graph3 = Tableau_Exiting %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont') & (AgeGroup != 'Total') & (TimeFrame %in% c('2001', '2019'))))

# Exporting the csv files to be used in Tableau for graphing purposes
write.csv(Tableau_Exiting_Graph, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Exiting USA.csv', row.names=TRUE)
write.csv(Tableau_Exiting_Graph2, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Exiting USA Age Group Total.csv', row.names=TRUE)
write.csv(Tableau_Exiting_Graph3, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Exiting 10 States 01&19.csv', row.names=TRUE)


# Selecting the three columns wanted for the graphs
Tableau_Juveniles = select(Juveniles, c('Location', 'TimeFrame', 'Data')) 

# Dropping the United States rows from the Location column
Tableau_Juveniles_Graph = Tableau_Juveniles %>% filter(Location == 'United States')

# Selecting the Age Groups from the second Juvenile dataset
Tableau_Juveniles_Graph2 = read.csv('C:/Users/13612/Desktop/Course Final Project/Assignments/Week 1/Datasets/ezacjrp_export2.csv')
  
# Selecting the 10 states and selecting only years 2001 and 2019
Tableau_Juveniles_Graph3 = Tableau_Juveniles %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont') & (TimeFrame %in% c('2001', '2019'))))

# Exporting the csv files to be used in Tableau for graphing purposes
write.csv(Tableau_Juveniles_Graph, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Juveniles USA.csv', row.names=TRUE)
write.csv(Tableau_Juveniles_Graph2, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Juveniles Age Groups.csv', row.names=TRUE)
write.csv(Tableau_Juveniles_Graph3, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Juveniles 10 States 01&19.csv', row.names=TRUE)


#### Evaluation Question 1: "Do 10 States with children leaving foster care have an influence on the juvenile incarceration numbers?"

### Data Wrangling

## Exiting the Foster Care System dataset
Exiting_FC1 = select(Exiting_FC, c('Location', 'Data')) # Keeping only two columns: Location and Data

# Selecting the 10 States we chose 
Exiting_States = Exiting_FC1 %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))

# Getting the sum of the Data column for all years combined.  Dropping to just the two needed columns for analysis
Exiting_States1 = aggregate(Data~Location, Exiting_States, sum) 

## Youth Incarceration dataset

# Keeping only two columns
Juveniles_IN = select(Juveniles, c('Location', 'Data')) 

# Selecting the 10 States we chose
Juveniles_States = Juveniles_IN %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))

# Getting the sum of the Data column for all years combined
Juveniles_States1 = aggregate(Data~Location, Juveniles_States, sum) 


### Important Variable Summary Statistics
summary(Exiting_FC1$Data)
summary(Exiting_States1$Data)
summary(Juveniles$Data)
summary(Juveniles_States1$Data)


### Data Analysis

## Testing assumptions

CrossTable(Exiting_States1$Location, Juveniles_States1$Data, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format='SPSS')

## Conclusions
# The p-value is .2313417, which is < .05 making this analysis not significant. No cells have any values < 5, also failing the assumptions
# There is no influence from children leaving foster care by state to juvenile incarceration numbers.



#### Evaluation Question 2: " Does the age group of children exiting foster care have an influence on juvenile incarceration numbers in the United States?"

### Data Wrangling

## Exiting the Foster Care System dataset
# Dropping three unneeded columns: LocationType, TimeFrame, and DataFormat
Exiting_FC2 = select(Exiting_FC, c('Location', 'AgeGroup', 'Data')) 

# Filtering only United States from the Location column and dropping the Total rows from the AgeGroup columns
Exiting_USA = Exiting_FC2 %>% filter((Location == 'United States') & (AgeGroup != 'Total')) 

 # Getting the sum of the Data column for all years combined
Exiting_USA1 = aggregate(Data~Location + AgeGroup, Exiting_USA, sum)

# Exporting the csv file to recode the dataset in Excel
write.csv(Exiting_USA1, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV DataFrames\\Exiting_USA1.csv', row.names=TRUE)

# Reading in the csv file that has been recoded
Exiting_USA2 <- read.csv("C:/Users/13612/Desktop/Course Final Project/Assignments/Week 1/Datasets/Exiting_USA2.csv")

## Youth incarceration dataset

#Keeping only the two needed columns for analysis
Juveniles_USA = select(Juveniles_2, c('AgeGroup', 'Data')) 

# Adding the Location column
Juveniles_USA$Location = 'United States' 

# Dropping any na rows
Juveniles_USA = na.omit(Juveniles_USA) 

# Reorganizing the columns to match Exiting_USA2 dataframe
Juveniles_USA1 = Juveniles_USA[, c(3,1,2)] 

### Important Variable Summary Statistics
summary(Exiting_FC2$Data)
summary(Exiting_USA2$Data)
summary(Juveniles_USA$Data)

### Data Analysis

## Testing assumptions
CrossTable(Exiting_USA2$AgeGroup, Juveniles_USA1$Data, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format='SPSS')

## Conclusions
# The p-value is .1572992, which is < .05, making this analysis not significant.  No cells have any values < 5, also failing the assumptions
# There is no influence from children leaving foster care by age group to juvenile incarceration numbers