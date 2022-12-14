getwd()
# Set to a directory that has two folders in it that will contain your data
# Here I use Working Directory as "Q2toQ3
# Two subdirectories containing csv files are set a few blocks below in Path

setwd("C:/temp/trash/R/DLLR/QuarterCompares/Q2toQ3")

# Following just lists directories below working directory note ./.Directories are hidden
list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

# below looks for All files ending in Q2.csv ***All files for Q2 should end in Q2.csv or this will not work
Q2files <- list.files(path = ".", pattern = '^.*Q2.csv', all.files = FALSE,
           full.names = FALSE, recursive = TRUE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# below looks for All files ending in Q3.csv ***All files for Q3 should end in Q3.csv or this will not work
Q3files <- list.files(path = ".", pattern = '^.*Q3.csv', all.files = FALSE,
                      full.names = FALSE, recursive = TRUE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

typeof(Q2files) #Character Type
length(Q2files) #How many files (14)
length(Q3files) #How many files (14)

# Are file Names the same besides Quarter?
gsub("Q2","",Q2files) == gsub("Q3","",Q3files) #Should be TRUE

Q2files <- gsub('.*/',"",Q2files)
Q3files <- gsub('*/',"",Q3files)
i<-0
for (i in 1:14){
  cat(Q2files[i],"\t","\t", Q3files[i],"\n")
  }

filetable <- data.frame(seq(1,14,by=1),Q2files,Q3files,stringsAsFactors = TRUE)

View(filetable)
colnames(filetable) <- c("FilePairNo.","Q2","Q3")                       
filetable[7,3]

str(filetable)


# Skip to comparing Data Start with Maryland!
Q2path <- './MD_County_WDA_Q2/'
Q3path <- './MD_County_WDA_Q3/'


# Row 7 is where MD is:
MDQ2 = read.csv(paste(Q2path,filetable[7,2],sep=""))
MDQ3 = read.csv(paste(Q3path,filetable[7,3],sep=""))

str(MDQ2)
length(colnames(MDQ2)) #9 variables in Maryland
View(MDQ2)

MDQ2 <- (MDQ2[-c(1),]) #Drop Header Row
MDQ3 <- (MDQ3[-c(1),]) #Drop Header Row

View(MDQ2)
View(MDQ3)


MDQ2['conc'] <- paste(MDQ2$Location,MDQ2$Time,MDQ2$Indicator,MDQ2$Indicator_Status,MDQ2$Employment_Status,MDQ2$QWI.Status)
MDQ3['conc'] <- paste(MDQ3$Location,MDQ3$Time,MDQ3$Indicator,MDQ3$Indicator_Status,MDQ3$Employment_Status,MDQ3$QWI.Status)

# MDQ3[,10] <- NULL #To remove 10th Row in MD if needed
# MDQ2[,10] <- NULL #TO remove 10th Row in MD if needed


str(MDQ3)
str(MDQ2)

fullMDQ2Q3 <- merge(MDQ3,MDQ2,by.x = "conc",by.y = "conc") 
View(fullMDQ2Q3)



same <- as.integer(nrow(fullMDQ2Q3[c("Amount.x","Amount.y","conc")][(fullMDQ2Q3$Amount.x == fullMDQ2Q3$Amount.y),]))
diff <- as.integer(nrow(fullMDQ2Q3[c("Amount.x","Amount.y","conc")][(fullMDQ2Q3$Amount.x != fullMDQ2Q3$Amount.y),]))
str(same)

# Simple Pie Chart with Percentage
redgreen <- c("red","green")

slices <- c(same,diff)
lbls <- c("Same","Different")
pct <- round(slices/sum(slices)*100)
lbls <- paste(slices,lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(slices, labels = lbls, col=redgreen, main="Maryland Q2 vs Q3")

#Maryland Complete

#Move on to County
#Investigate Strucutre of csv first COUNTY HAS 11 variable
# install.packages("data.table")                     # Install data.table package for %like%
# install data.table                                 # This is a different way to find county files 
library("data.table")                              # Load data.table



filenumber <- as.integer(filetable[1][filetable$Q2 %like% "CountyData", ])     # Extract matching rows with %like%
View(filenumber)

Q2path <- './MD_County_WDA_Q2/'
Q3path <- './MD_County_WDA_Q3/'

CountyQ2 = read.csv(paste(Q2path,filetable[filenumber,2],sep=""))
CountyQ3 = read.csv(paste(Q3path,filetable[filenumber,3],sep=""))

str(CountyQ3)
str(CountyQ2)
length(colnames(CountyQ2)) #11 Variables in County

View(CountyQ2) # No Header Rows?
View(CountyQ3) # No Header Rows?

CountyQ2['conc'] <- paste(CountyQ2$Time,CountyQ2$Indicator,CountyQ2$Employment_Status,CountyQ2$Indicator_Status,CountyQ2$Location)
CountyQ3['conc'] <- paste(CountyQ3$Time,CountyQ3$Indicator,CountyQ3$Employment_Status,CountyQ3$Indicator_Status,CountyQ3$Location)

fullCountyQ2Q3 <- merge(CountyQ3,CountyQ2,by.x = "conc",by.y = "conc") 
View(fullCountyQ2Q3) #Many NA's Note conc is not NA but Amounts are
sum(is.na(fullCountyQ2Q3$Amount.x))
sum(is.na(fullCountyQ2Q3$Amount.y))

fullCountyQ2Q3 <- fullCountyQ2Q3[!is.na(fullCountyQ2Q3$Amount.x),]
# ReRun Counts Above for NAs

sameCounty <- as.integer(nrow(fullCountyQ2Q3[c("Amount.x","Amount.y","conc")][(fullCountyQ2Q3$Amount.x == fullCountyQ2Q3$Amount.y),]))
diffCounty <- as.integer(nrow(fullCountyQ2Q3[c("Amount.x","Amount.y","conc")][(fullCountyQ2Q3$Amount.x != fullCountyQ2Q3$Amount.y),]))
str(sameCounty)
str(diffCounty)

#What 2 out of Curiosity?
View(fullCountyQ2Q3[c("Amount.x","Amount.y","conc")][(fullCountyQ2Q3$Amount.x != fullCountyQ2Q3$Amount.y),])

# Simple Pie Chart with Percentage
slicesCounty <- c(sameCounty,diffCounty)
countsCounty <- c(sameCounty,diffCounty)
lbls <- c("Same","Different")
pct <- round(slicesCounty/sum(slicesCounty)*100)
lbls <- paste(countsCounty, lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(slicesCounty, labels = lbls, col=redgreen, main="County Q2 vs Q3")


#LWDAs

#Investigate Strucutre of csv first LWDAs HAS 11 variable
# install.packages("data.table")                     # Install data.table package
library("data.table")                              # Load data.table

filenumberLWDA <- as.integer(filetable[1][filetable$Q2 %like% "LWDA", ])     # Extract matching rows with %like%

length(filenumberLWDA) # Number of files ...Should be 12
filenumberLWDA[1:12]

Q2path <- './MD_County_WDA_Q2/'
Q3path <- './MD_County_WDA_Q3/'

filenumberLWDA[4]

LWDAQ2file1 = read.csv(paste(Q2path,filetable[filenumberLWDA[1],2],sep=""))
LWDAQ3file1 = read.csv(paste(Q3path,filetable[filenumberLWDA[1],3],sep=""))
LWDAQ2file2 = read.csv(paste(Q2path,filetable[filenumberLWDA[2],2],sep=""))
LWDAQ3file2 = read.csv(paste(Q3path,filetable[filenumberLWDA[2],3],sep=""))
LWDAQ2file3 = read.csv(paste(Q2path,filetable[filenumberLWDA[3],2],sep=""))
LWDAQ3file3 = read.csv(paste(Q3path,filetable[filenumberLWDA[3],3],sep=""))
LWDAQ2file4 = read.csv(paste(Q2path,filetable[filenumberLWDA[4],2],sep=""))
LWDAQ3file4 = read.csv(paste(Q3path,filetable[filenumberLWDA[4],3],sep=""))
LWDAQ2file5 = read.csv(paste(Q2path,filetable[filenumberLWDA[5],2],sep=""))
LWDAQ3file5 = read.csv(paste(Q3path,filetable[filenumberLWDA[5],3],sep=""))
LWDAQ2file6 = read.csv(paste(Q2path,filetable[filenumberLWDA[6],2],sep=""))
LWDAQ3file6 = read.csv(paste(Q3path,filetable[filenumberLWDA[6],3],sep=""))
LWDAQ2file7 = read.csv(paste(Q2path,filetable[filenumberLWDA[7],2],sep=""))
LWDAQ3file7 = read.csv(paste(Q3path,filetable[filenumberLWDA[7],3],sep=""))
LWDAQ2file8 = read.csv(paste(Q2path,filetable[filenumberLWDA[8],2],sep=""))
LWDAQ3file8 = read.csv(paste(Q3path,filetable[filenumberLWDA[8],3],sep=""))
LWDAQ2file9 = read.csv(paste(Q2path,filetable[filenumberLWDA[9],2],sep=""))
LWDAQ3file9 = read.csv(paste(Q3path,filetable[filenumberLWDA[9],3],sep=""))
LWDAQ2file10 = read.csv(paste(Q2path,filetable[filenumberLWDA[10],2],sep=""))
LWDAQ3file10 = read.csv(paste(Q3path,filetable[filenumberLWDA[10],3],sep=""))
LWDAQ2file11 = read.csv(paste(Q2path,filetable[filenumberLWDA[11],2],sep=""))
LWDAQ3file11 = read.csv(paste(Q3path,filetable[filenumberLWDA[11],3],sep=""))
LWDAQ2file12 = read.csv(paste(Q2path,filetable[filenumberLWDA[12],2],sep=""))
LWDAQ3file12 = read.csv(paste(Q3path,filetable[filenumberLWDA[12],3],sep=""))



length(colnames(LWDAQ2file1)) #8 Variables in LWDA
colnames(LWDAQ2file1)

View(LWDAQ2file10) # No Header Rows?
View(LWDAQ2file11) # No Header Rows?

#Combine first 6 of 8 Rows
LWDAQ2file1['conc']  <- paste(LWDAQ2file1$Measure, LWDAQ2file1$Location, LWDAQ2file1$Indicator, LWDAQ2file1$Time, LWDAQ2file1$Indicator_Status, LWDAQ2file1$Indicator_Value)
LWDAQ3file1['conc']  <- paste(LWDAQ3file1$Measure, LWDAQ3file1$Location, LWDAQ3file1$Indicator, LWDAQ3file1$Time, LWDAQ3file1$Indicator_Status, LWDAQ3file1$Indicator_Value)
LWDAQ2file2['conc']  <- paste(LWDAQ2file2$Measure, LWDAQ2file2$Location, LWDAQ2file2$Indicator, LWDAQ2file2$Time, LWDAQ2file2$Indicator_Status, LWDAQ2file2$Indicator_Value)
LWDAQ3file2['conc']  <- paste(LWDAQ3file2$Measure, LWDAQ3file2$Location, LWDAQ3file2$Indicator, LWDAQ3file2$Time, LWDAQ3file2$Indicator_Status, LWDAQ3file2$Indicator_Value)
LWDAQ2file3['conc']  <- paste(LWDAQ2file3$Measure, LWDAQ2file3$Location, LWDAQ2file3$Indicator, LWDAQ2file3$Time, LWDAQ2file3$Indicator_Status, LWDAQ2file3$Indicator_Value)
LWDAQ3file3['conc']  <- paste(LWDAQ3file3$Measure, LWDAQ3file3$Location, LWDAQ3file3$Indicator, LWDAQ3file3$Time, LWDAQ3file3$Indicator_Status, LWDAQ3file3$Indicator_Value)
LWDAQ2file4['conc']  <- paste(LWDAQ2file4$Measure, LWDAQ2file4$Location, LWDAQ2file4$Indicator, LWDAQ2file4$Time, LWDAQ2file4$Indicator_Status, LWDAQ2file4$Indicator_Value)
LWDAQ3file4['conc']  <- paste(LWDAQ3file4$Measure, LWDAQ3file4$Location, LWDAQ3file4$Indicator, LWDAQ3file4$Time, LWDAQ3file4$Indicator_Status, LWDAQ3file4$Indicator_Value)
LWDAQ2file5['conc']  <- paste(LWDAQ2file5$Measure, LWDAQ2file5$Location, LWDAQ2file5$Indicator, LWDAQ2file5$Time, LWDAQ2file5$Indicator_Status, LWDAQ2file5$Indicator_Value)
LWDAQ3file5['conc']  <- paste(LWDAQ3file5$Measure, LWDAQ3file5$Location, LWDAQ3file5$Indicator, LWDAQ3file5$Time, LWDAQ3file5$Indicator_Status, LWDAQ3file5$Indicator_Value)
LWDAQ2file6['conc']  <- paste(LWDAQ2file6$Measure, LWDAQ2file6$Location, LWDAQ2file6$Indicator, LWDAQ2file6$Time, LWDAQ2file6$Indicator_Status, LWDAQ2file6$Indicator_Value)
LWDAQ3file6['conc']  <- paste(LWDAQ3file6$Measure, LWDAQ3file6$Location, LWDAQ3file6$Indicator, LWDAQ3file6$Time, LWDAQ3file6$Indicator_Status, LWDAQ3file6$Indicator_Value)
LWDAQ2file7['conc']  <- paste(LWDAQ2file7$Measure, LWDAQ2file7$Location, LWDAQ2file7$Indicator, LWDAQ2file7$Time, LWDAQ2file7$Indicator_Status, LWDAQ2file7$Indicator_Value)
LWDAQ3file7['conc']  <- paste(LWDAQ3file7$Measure, LWDAQ3file7$Location, LWDAQ3file7$Indicator, LWDAQ3file7$Time, LWDAQ3file7$Indicator_Status, LWDAQ3file7$Indicator_Value)
LWDAQ2file8['conc']  <- paste(LWDAQ2file8$Measure, LWDAQ2file8$Location, LWDAQ2file8$Indicator, LWDAQ2file8$Time, LWDAQ2file8$Indicator_Status, LWDAQ2file8$Indicator_Value)
LWDAQ3file8['conc']  <- paste(LWDAQ3file8$Measure, LWDAQ3file8$Location, LWDAQ3file8$Indicator, LWDAQ3file8$Time, LWDAQ3file8$Indicator_Status, LWDAQ3file8$Indicator_Value)
LWDAQ2file9['conc']  <- paste(LWDAQ2file9$Measure, LWDAQ2file9$Location, LWDAQ2file9$Indicator, LWDAQ2file9$Time, LWDAQ2file9$Indicator_Status, LWDAQ2file9$Indicator_Value)
LWDAQ3file9['conc']  <- paste(LWDAQ3file9$Measure, LWDAQ3file9$Location, LWDAQ3file9$Indicator, LWDAQ3file9$Time, LWDAQ3file9$Indicator_Status, LWDAQ3file9$Indicator_Value)
LWDAQ2file10['conc']  <- paste(LWDAQ2file10$Measure, LWDAQ2file10$Location, LWDAQ2file10$Indicator, LWDAQ2file10$Time, LWDAQ2file10$Indicator_Status, LWDAQ2file10$Indicator_Value)
LWDAQ3file10['conc']  <- paste(LWDAQ3file10$Measure, LWDAQ3file10$Location, LWDAQ3file10$Indicator, LWDAQ3file10$Time, LWDAQ3file10$Indicator_Status, LWDAQ3file10$Indicator_Value)
LWDAQ2file11['conc']  <- paste(LWDAQ2file11$Measure, LWDAQ2file11$Location, LWDAQ2file11$Indicator, LWDAQ2file11$Time, LWDAQ2file11$Indicator_Status, LWDAQ2file11$Indicator_Value)
LWDAQ3file11['conc']  <- paste(LWDAQ3file11$Measure, LWDAQ3file11$Location, LWDAQ3file11$Indicator, LWDAQ3file11$Time, LWDAQ3file11$Indicator_Status, LWDAQ3file11$Indicator_Value)
LWDAQ2file12['conc']  <- paste(LWDAQ2file12$Measure, LWDAQ2file12$Location, LWDAQ2file12$Indicator, LWDAQ2file12$Time, LWDAQ2file12$Indicator_Status, LWDAQ2file12$Indicator_Value)
LWDAQ3file12['conc']  <- paste(LWDAQ3file12$Measure, LWDAQ3file12$Location, LWDAQ3file12$Indicator, LWDAQ3file12$Time, LWDAQ3file12$Indicator_Status, LWDAQ3file12$Indicator_Value)

# Running merge for each file LOOP THOROUGH THIS FOR EACH OF 12 FILES
# Relpace file names above in next line LWDAQ2file<filenumber>, LWDAQ3file<filenumber+1>

# for (f in 1:length(filenumberLWDA)) {
# 
#   View(gsub(" ", "", paste("LWDAQ2file",as.character(filenumberLWDA[f],sep=""))))
# }

LWDAvlookup <- merge(LWDAQ2file12,LWDAQ3file12,by.x = "conc",by.y = "conc")
  
  cat("This if for LWDA: ", LWDAvlookup[1,"Location.x"])
  
  # View(LWDAvlookup) #Manual check for NA's Note conc is not NA but Amounts are for COUNTY DATA
  sum(is.na(LWDAvlookup$Amount.x)) #Should be Zero
  sum(is.na(LWDAvlookup$Amount.y)) #Should be Zero
  
  # fullCountyQ2Q3 <- fullCountyQ2Q3[!is.na(fullCountyQ2Q3$Amount.x),] #Not needed if no NAs
  
  
  sameLWDA <- as.integer(nrow(LWDAvlookup[c("Amount.x","Amount.y","conc")][(LWDAvlookup$Amount.x == LWDAvlookup$Amount.y),]))
  diffLWDA <- as.integer(nrow(LWDAvlookup[c("Amount.x","Amount.y","conc")][(LWDAvlookup$Amount.x != LWDAvlookup$Amount.y),]))
  str(sameLWDA)
  str(diffLWDA)
  
  # Simple Pie Chart with Percentage
  slicesLWDA <- c(sameLWDA,diffLWDA)
  lbls <- c("Same","Different")
  pct <- round(slicesLWDA/sum(slicesLWDA)*100)
  lbls <- paste(slicesLWDA, lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  
  pie(slicesLWDA, labels = lbls, col=redgreen, main=LWDAvlookup[1,"Location.x"])

