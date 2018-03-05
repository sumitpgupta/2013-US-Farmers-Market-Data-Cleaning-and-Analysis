## Question 1

library(openxlsx) # Installing and loading package openxlsx
library(xlsx)
FarmerMarketData<- read.xlsx2("2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", header=TRUE, sheetIndex = 1, startRow = 3, stringsAsFactors=FALSE) # Loading the xlsx2 file
FarmerMarketData <-as.data.frame(FarmerMarketData)
FarmerMarketData

## Question 2
# The Strategy is to first seperate the months given in ranges into two seperate columns(startdate and enddate) and perform looping iterations to assign different seasons to these months
# Adding new columns startDate and endDate
FarmerMarketData$startDate<- 0
FarmerMarketData$endDate<- 0

# Creating new vectors with name of all months, month numbers and seasons
monthName<- c("January","February","March","April","May","June","July","August","September","October","November","December","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
monthNum<-c(1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12)
seasons<-c("Winter","Winter","Spring","Spring","Spring","Summer","Summer","Summer","Fall","Fall","Fall","Winter")

# Reading the data in Season1Date column
# Seperating the data based on string = 'to'
# Storing seperated data in StartDate and endDate columns
for (i in 1:nrow(FarmerMarketData))
{
  len<- nchar(as.character(FarmerMarketData$Season1Date[i]))
  mid<- regexpr(' to ', as.character(FarmerMarketData$Season1Date[i]))
  FarmerMarketData$startDate[i]<- gsub(" ","",substr(FarmerMarketData$Season1Date[i],1,mid-1)) # Remove any extra space by gsub function
  FarmerMarketData$endDate[i]<- gsub(" ","",substr(FarmerMarketData$Season1Date[i],mid+4, len))
  
}

# Assigning numbers to each month
for (i in 1:nrow(FarmerMarketData))
{
  flag1=0 
  flag2=0
  for(j in 1:length(monthName))
  {
    a<- regexpr(monthName[j],FarmerMarketData$startDate[i]) # Chceking if month is present in startdate column
    if(!is.na(a) && a>0)
    {
      flag1=j 
      break
    }
    
  }
  if(flag1==0) #Flag name will be 0 for no Month Name. 
  {
    FarmerMarketData$startDate[i]<- month(as.Date(FarmerMarketData$startDate[i],format="%m/%d/%Y"))
    
  } 
  if(flag1>0) # When greater than zero, flag1 is executed
  {
    FarmerMarketData$startDate[i]<- monthNum[flag1]  
  }
  
  ## Working through same logic of startdate for enddate
  
  for(j in 1:length(monthName))
  {
    b<- regexpr(monthName[j],FarmerMarketData$endDate[i])
    if(!is.na(b) && b>0)
    {
      flag2=j
      break
    }
  }
  if(flag2==0)
  {
    FarmerMarketData$endDate[i]<- month(as.Date(FarmerMarketData$endDate[i],format="%m/%d/%Y"))
    
  } 
  if(flag2>0)
  {
    FarmerMarketData$endDate[i]<- monthNum[flag2]  
  }
}


## Defining functions for Winter, Spring, Summer, Fall, Year Round and Half year
getseason<- function(startmonth=0, endmonth=0)
{
  monthdiff<- (as.numeric(startmonth) - as.numeric(endmonth))*-1
  
  if (monthdiff >= 11) # Year Round for month difference greater than 11
  {
    return("Year Round")
  }
  else if(monthdiff >= 5) # Half year where month difference is 5
  {
    return("Half Year")
  }
  else if(startmonth ==0 && endmonth==0) # 
  {
    return("Dates not given") # Where no date is given
  }
  else 
  {
    if(seasons[startmonth]==seasons[endmonth]) # Season matches when endmonth and start month are same
    {
      return(seasons[startmonth])
    }
    else # When start month and end month are not same then that month lies between two different seasons
    {
      return(paste(seasons[startmonth]," to ",seasons[endmonth]))
    }
  }
}

# Removing NA's by assigning 0
FarmerMarketData$startDate[is.na(FarmerMarketData$startDate)]<-0
FarmerMarketData$endDate[is.na(FarmerMarketData$endDate)]<-0

## Finding the output as season
for (i in 1:nrow(FarmerMarketData))
{
  FarmerMarketData$Season1Date[i] <- getseason(as.numeric(FarmerMarketData$startDate[i]), as.numeric(FarmerMarketData$endDate[i]))
}

FarmerMarketData$Season1Date # Printing the output of Season1Date





## Question 3

acceptsWIC <- function()
{
  NewFarmData <- FarmerMarketData[which(FarmerMarketData$WIC == "Y"),] # Sorting only those rows where WIC = Y
  MarketNameData <- data.frame(NewFarmData$MarketName) # Sorting columns with only Market Names from above obtained data

  return(MarketNameData)

}
acceptsWIC() # Calling the function



