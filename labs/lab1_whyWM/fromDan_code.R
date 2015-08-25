#First, set your working directory.
#This is the directory you'll
#Keep all your files.
#Note we put two "\" after the H, this is because
#"\" means something special in R.
setwd("H:\\")

#Now, lets load our data!  Make sure the CSV
#is in your working directory.
#In some cases you may have to specify the full "path" to the
#file, for example "H:\\myFile.CSV"
#Note you will also need to change this
#CSV name to your own that you download, and make
#sure to unzip the file!!

collegeData <- read.csv("H:\\CSV_8132015-669")

#Here is an example from a Linux computer: 
#collegeData <- read.csv("/home/aiddata/Desktop/R_Repo/COLL_100/labs/lab_1_1_basics/BackupData/CSV_8132015-669.csv")

#Take a look at the dataset to make sure it
#looks like it loaded correctly
View(collegeData)

#Try out some commands to see what they do:
names(collegeData)
dim(collegeData)
nrow(collegeData)
ncol(collegeData)
#Let's run a quick analysis - how many colleges are in each state?
#when you type in names(collegeData), you'll notice one of the
#names is "HD2013.State.abbreviation" - one of the columns you
#requested when you did the download.  To see how many
#colleges are in each state:
table(collegeData$HD2013.State.abbreviation)

#Now, let's use a different command to identify the average tuition across all colleges:
summary(collegeData$DRVIC2013.Tuition.and.fees..2013.14)

#Now let's do the same thing, but only looking at Virginia.
#Does Virginia have a higher average tuition or lower than
#the rest of the country?:
VA_colleges <- collegeData[which(collegeData$HD2013.State.abbreviation=="Virginia"),]
summary(VA_colleges$DRVIC2013.Tuition.and.fees..2013.14)

#What if we want to get really clever, and identify only those institutions in VA that are
#above the national mean?  Is William and Mary above the national mean?
AboveUSMeanVA <- collegeData[which(collegeData$HD2013.State.abbreviation=="Virginia"& collegeData$DRVIC2013.Tuition.and.fees..2013.14 > 14140),]
View(AboveUSMeanVA)

#You can read through each entry to figure out what William and Mary's tuition is, or
#you could type this:
collegeData[which(collegeData$institution.name=="College of William and Mary"),]$DRVIC2013.Tuition.and.fees..2013.14

#You can use similar approaches to find the cheapest and most expensive schools in the country:
min(collegeData$DRVIC2013.Tuition.and.fees..2013.14, na.rm=TRUE)
max(collegeData$DRVIC2013.Tuition.and.fees..2013.14, na.rm=TRUE)
minColCost <- min(collegeData$DRVIC2013.Tuition.and.fees..2013.14, na.rm=TRUE)
maxColCost <- max(collegeData$DRVIC2013.Tuition.and.fees..2013.14, na.rm=TRUE)
collegeData[which(collegeData$DRVIC2013.Tuition.and.fees..2013.14 == minColCost),]$institution.name
collegeData[which(collegeData$DRVIC2013.Tuition.and.fees..2013.14 == maxColCost),]$institution.name

#Now, on to some basic analysis.  How does the percentage increase of W&M tuition
#compare to all schools and VA school sform 2010 to the 2013-2014 shcool year?
#First, calculate the change for W&M:
WMTuition_2014 <- collegeData[which(collegeData$institution.name=="College of William and Mary"),]$DRVIC2013.Tuition.and.fees..2013.14
WMTuition_2010 <- collegeData[which(collegeData$institution.name=="College of William and Mary"),]$DRVIC2013.Tuition.and.fees..2010.11
WM_change <- WMTuition_2014 / WMTuition_2010
WM_change

#Now, let's do it for every other school.
#To do this, we're first going to add our own new "column"
#of data.  To see how this works, first type in:
names(collegeData)
#which shows you all of the current columns of data.

#Now, let's add a new one.  This is the same thing we did for William 
#and Mary, but for *every* school:
collegeData$All_change <- collegeData$DRVIC2013.Tuition.and.fees..2013.14 / collegeData$DRVIC2013.Tuition.and.fees..2010.11

#We'll do names again, and now you'll see your new data column!
#That column is the change for every school.
names(collegeData)

#Let's summarize that new column of data - anything surprising?
summary(collegeData$All_change)

#Now, let's do a comparison between the tuition in 2014 and the
#percent of that money spent on instructors salary.
#First, let's look it up for William and Mary:
collegeData[which(collegeData$institution.name=="College of William and Mary"),]$DRVF2013.Salaries..wages..and.benefit.expenses.for.instruction.as.a.percent.of.total.expenses.for.instruction..GASB.

#Second, let's compare that to the national average:
summary(collegeData$DRVF2013.Salaries..wages..and.benefit.expenses.for.instruction.as.a.percent.of.total.expenses.for.instruction..GASB.)

#Now, let's plot out all schools tuition in 2014 contrasted to the 
#percent of tuition they spend on instructors salary
plot(collegeData$DRVF2013.Salaries..wages..and.benefit.expenses.for.instruction.as.a.percent.of.total.expenses.for.instruction..GASB., collegeData$DRVIC2013.Tuition.and.fees..2010.11)

#That's not particularly helpful, as it's hard to see patterns with
#outliers and without knowing which dot is W&M.  It's also probably
#more fair to compare to only Virginia schools, so let's use the earlier
#Virginia dataset:
plot(VA_colleges$DRVF2013.Salaries..wages..and.benefit.expenses.for.instruction.as.a.percent.of.total.expenses.for.instruction..GASB., VA_colleges$DRVIC2013.Tuition.and.fees..2010.11)

#Now, let's label William and Mary!
WM_plot <- collegeData[which(collegeData$institution.name=="College of William and Mary"),]
points(WM_plot$DRVF2013.Salaries..wages..and.benefit.expenses.for.instruction.as.a.percent.of.total.expenses.for.instruction..GASB., WM_plot$DRVIC2013.Tuition.and.fees..2010.11, col="green")


#This is the end of the R guided tutorial, but you still have more to do!
#Reference back to the lab assignment for your instructions.