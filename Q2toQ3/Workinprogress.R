getwd()
setwd("C:/temp/trash/R/DLLR/QuarterCompares/Q2toQ3")
list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
Q2files <- list.files(path = ".", pattern = '^.*Q2.csv', all.files = FALSE,
           full.names = FALSE, recursive = TRUE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
Q3files <- list.files(path = ".", pattern = '^.*Q3.csv', all.files = FALSE,
                      full.names = FALSE, recursive = TRUE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

typeof(Q2files) #Character Type
length(Q2files) #How many files (14)
length(Q3files) #How many files (14)

# Are file Names the same besides Quarter?
gsub("Q2","",Q2files) == gsub("Q3","",Q3files) #Should be TRUE

Q2files <- gsub('.*/',"",Q2files)
Q3files <- gsub('.*/',"",Q3files)

cat(Q2files[1],Q3files[1])

filetable <- data.frame(seq(1,14,by=1),Q2files,Q3files,stringsAsFactors = TRUE)

View(filetable)
colnames(filetable) <- c("FilePairNo.","Q2","Q3")                       
filetable[7,3]

str(filetable)


# Skip to comparing Data Start with Maryland!
Q2path <- './MD_County_WDA_Q2/'
Q3path <- './MD_County_WDA_Q3/'

MDQ2 = read.csv(paste(Q2path,filetable[7,2],sep=""))
MDQ3 = read.csv(paste(Q3path,filetable[7,3],sep=""))

str(MDQ2)
length(colnames(MDQ2)) #9 variables in Maryland

MDQ2 <- (MDQ2[-c(1),]) #Drop Header Row
MDQ3 <- (MDQ3[-c(1),]) #Drop Header Row

View(MDQ2)
View(MDQ3)


MDQ2['conc'] <- paste(MDQ2$Location,MDQ2$Time,MDQ2$Indicator,MDQ2$Indicator_Status,MDQ2$Employment_Status,MDQ2$QWI.Status)
MDQ3['conc'] <- paste(MDQ3$Location,MDQ3$Time,MDQ3$Indicator,MDQ3$Indicator_Status,MDQ3$Employment_Status,MDQ3$QWI.Status)

# MDQ3[,10] <- NULL #To remove 10th Row in MD if needed
# MDQ2[,10] <- NULL #TO remove 10th Row in MD if needed


help("merge")
str(MDQ3)
str(MDQ2)

fullMDQ2Q3 <- merge(MDQ3,MDQ2,by.x = "conc",by.y = "conc") 
View(fullMDQ2Q3)

same <- as.integer(count(fullMDQ2Q3[c("Amount.x","Amount.y","conc")][(fullMDQ2Q3$Amount.x == fullMDQ2Q3$Amount.y),]))
diff <- as.integer(count(fullMDQ2Q3[c("Amount.x","Amount.y","conc")][(fullMDQ2Q3$Amount.x != fullMDQ2Q3$Amount.y),]))
str(same)
# Simple Pie Chart with Percentage
slices <- c(same,diff)
lbls <- c("Same","Different")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(slices, labels = lbls, col=rainbow(length(lbls)), main="Maryland Q2 vs Q3")

#Maryland Complete

#Move on to County
#Investigate Strucutre of csv first COUNTY HAS 11 variable
install.packages("data.table")                     # Install data.table package
library("data.table")                              # Load data.table

filenumber <- as.integer(filetable[1][filetable$Q2 %like% "CountyData", ])     # Extract matching rows with %like%


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


sameCounty <- as.integer(count(fullCountyQ2Q3[c("Amount.x","Amount.y","conc")][(fullCountyQ2Q3$Amount.x == fullCountyQ2Q3$Amount.y),]))
diffCounty <- as.integer(count(fullCountyQ2Q3[c("Amount.x","Amount.y","conc")][(fullCountyQ2Q3$Amount.x != fullCountyQ2Q3$Amount.y),]))
str(sameCounty)
# Simple Pie Chart with Percentage
slicesCounty <- c(sameCounty,diffCounty)
lbls <- c("Same","Different")
pct <- round(slicesCounty/sum(slicesCounty)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(slices, labels = lbls, col=rainbow(length(lbls)), main="County Q2 vs Q3")
