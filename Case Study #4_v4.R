library(XML)

#Establish the View Page Source of the Web Site 
ubase = "http://www.cherryblossom.org/"
url = paste(ubase, "results/2012/2012cucb10m-m.htm", sep = "")
doc = htmlParse(url)

preNode = getNodeSet(doc, "//pre")
txt = xmlValue(preNode[[1]])
nchar(txt)

#Modified function that works with year 1999 now.
extractResTable =

  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  
function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm", year = 1999, sex = "female", file = NULL)
{
    doc = htmlParse(url)
	
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
     else if (year == 1999) {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]  
     } 
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (sex == "male") {
        subDir <- "MenTxt"
    } else {
        subDir <- "WomenTxt"
    }
    
    if (!(is.null(file))) {
        if(!(dir.exists(subDir))) {dir.create(subDir)} 
        writeLines(els, file.path(subDir, file))
    }
    return(els)
}


#Setting a vector of all the URLS for each year 
ubase = "http://www.cherryblossom.org/"
urls = paste(ubase, "results/", 1999:2012, "/",
             1999:2012, "cucb10m-m.htm", sep = "")
years <- 1999:2012

# Apply the extractRestTable() to "urls" 
menTables = lapply(urls, extractResTable)

#Resolving the error message
options(error = recover)
menTables = lapply(urls, extractResTable) #Choose Selection 2

# After choosing Selection 2, enter ls() in the console below
# The list should display: [1] "doc"     "preNode" "url"
# Proceed if so by:
# 1. Enter url in the console. 
  # Output: [1] "http://www.cherryblossom.org/results/1999/1999cucb10m-m.htm"
# 2. Enter length(preNode)
  # Output: [1] 0

# Gather the URLs for Female Results into a character vector, womURLS 

womURLS <- c("/results/1999/cb99f.html", "/results/2000/Cb003f.htm", "results/2001/oof_f.html", 
              "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
              "results/2004/women.htm", "results/2005/CB05-F.htm",
              "results/2006/women.htm", "results/2007/women.htm", 
              "results/2008/women.htm", "results/2009/09cucb-F.htm",
              "results/2010/2010cucb10m-f.htm", 
              "results/2011/2011cucb10m-f.htm",
              "results/2012/2012cucb10m-f.htm")

# Reconstruct the urls vector to contain the proper Web Addresses
urls = paste(ubase, womURLS, sep = "")
urls[1:3]

# Call extractResTable() for the female results of each year
years = 1999:2012
womTables <- mapply(extractResTable, url = urls, year = years, sex = "female",
                    file = paste(years, ".txt", sep = ""))

###############################################################################
## Working with the Text Files ##

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))

  else return(c(0, spaceLocs))
}

selectCols =
  function(colNames, headerRow, searchLocs)
  {
    sapply(colNames,
           function(name, headerRow, searchLocs)
           {
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1)
               return( c(NA, NA) )
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1])
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }

extractVariables <- function(file, varNames = c("name", "home", "ag", "gun", "net", "time"),
                             sex = "W", year) {
  
  #Find the index of the footer row
  footIndex <- grep("^[[:blank:]]*[#|*]", file)
  
  #Find the index of rows that are completely blank
  blankIndex <- grep("^[[:blank:]]*$", file)
  
  if(sex == "W" & year == 2001){
    #women's file for 2001 does not contain spacer or header rows
    body <- file[-c(footIndex, blankIndex)]
    locCols<-matrix(c(13, 34, 38, 56, 35, 37, 65, 72, 57, 64, NA, NA), nrow = 2)
    colnames(locCols) <- varNames
    
  } else {
    #Find the index of the row with equal signs
    eqIndex <- grep("^===", file)    
    
    #Extract the two key rows and the data (fix men 2006 spacer row)
    spacerRow <- file[eqIndex]
    headerRow <- tolower(file[eqIndex - 1])
    
    if (year == 2006){
      locNetTime <- regexpr("net", headerRow)
      spacerRow <- paste(substr(spacerRow, 1, locNetTime - 2), 
                         substr(spacerRow, locNetTime, nchar(spacerRow)), "")
    }
    
    
    body <- file[-c(1:eqIndex, footIndex, blankIndex)]
    
    #Obtain the starting and ending positions of variables
    searchLocs <- findColLocs(spacerRow)
    locCols <- selectCols(varNames, headerRow, searchLocs)
  }
  
  Values <- mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
  colnames(Values) <- varNames
  
  invisible(Values)
}

#Read the lines into tables in R 
wfilenames = paste("WomenTxt/", 1999:2012, ".txt", sep = "")
womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012
years = 1999:2012 

#Obtain a list of character matrices 
#womenResMat = lapply(womenFiles, extractVariables, year = years)
womenResMat <- mapply(extractVariables, womenFiles, sex = "W", year = 1999:2012)
length(womenResMat)

sapply(womenResMat, nrow)



##########################################################################

## Data Cleaning & Reformatting Variables 

#Check for why there are so many NA's in women's 2006 file for runtime
#We  fix this in the extractVariables function
fileWomen2006 <- womenFiles$`2006`
head(fileWomen2006, 30)


#Create numeric variable age with as.numeric
age <- as.numeric(womenResMat$`2012`[, "ag"])
age <- sapply(womenResMat, function(x) as.numeric(x[ , "ag"]))
# Box Plot to show errors/NAs
boxplot(age, ylab = "Age", xlab = "Year")


#Reviewing Years' Head
head(womenFiles[['2003']])
womenFiles[['2006']][2200:2205]

# Getting count of NAs by Year 
sapply(age, function(x) sum(is.na(x)))
#Women
age <- as.numeric(womenResMat$`2012`[, "ag"])
age <- sapply(womenResMat, function(x) as.numeric(x[ , "ag"]))
boxplot(age, ylab = "Age", xlab = "Year")

sapply(age, function(x) sum(is.na(x)))

age1999 <- age$`1999`

badAgeIndex <- which(is.na(age1999)) + grep("^===", womenFiles[["1999"]])
womenFiles$`1999`[badAgeIndex]

age2002 <- age$`2002`
badAgeIndex <- which(is.na(age2002)) + grep("^===", womenFiles[["2002"]])
womenFiles$`2002`[badAgeIndex]

age2005 <- age$`2005`
badAgeIndex <- which(is.na(age2005)) + grep("^===", womenFiles[["2005"]])
womenFiles$`2005`[badAgeIndex]

age2001 <- age$`2001`
min(age2001)
zeroAgeIndex <- which(age2001 == 0)
womenFiles$`2001`[zeroAgeIndex + 3]

age2009 <- age$`2009`
min(age2009, na.rm = T)
ageSevenIndex <- which(age2009 == 7) + grep("^===", womenFiles[["2009"]])
womenFiles$`2009`[ageSevenIndex]

convertTime <- function(charTime){
  #takes time in h:mm:ss format and converts it to minutes
  #if time is invalid, it forces it to NA
  
  timePieces <- strsplit(charTime, ":")
  timePieces <- sapply(timePieces, as.numeric)
  
  #Fix to account for times that are of incorrect format, e.g. "1:30:" 
  nbrColons <- lapply(charTime, 
                      function(x) {
                        length(gregexpr(":", x)[[1]])
                      })
  
  runTime <- mapply(function(x, y, z){
    nbrTimePieces <- length(x)
    if (nbrTimePieces <= y) {
      return(NA)}
    else if (nbrTimePieces == 2) {
      return(x[1] + x[2]/60)}
    else {
      return(60*x[1] + x[2] + x[3]/60)}
  }, 
  timePieces, 
  nbrColons,
  charTime)
  
}

createDF <- function(Res, year, sex){
  #Determine which time to use
  useTime <- if(!is.na(Res[1, "net"])) {
    Res[, "net"]
  } else if(!is.na(Res[1, "gun"])) {
    Res[, "gun"]
  } else {
    Res[, "time"]}
  
  #Remove # and * and blanks from time
  useTime <- gsub("[#\\*[:blank:]]", "", useTime)
  
  #Drop rows with no time
  Res <- Res[useTime != "", ]
  
  runTime <- convertTime(useTime[useTime != ""])
  
  #convertTime returns NA for invalid run times; drop these records and print
  #message about record(s) dropped
  if(sum(is.na(runTime)) > 0){
    print(paste("Dropping the following records in year", year, "for", 
                ifelse(sex == "M", "Men", "Women"), 
                "due to invalid times", sep = " "))
    
    print(Res[is.na(runTime), ])     
  }
  
  
  Results <- data.frame(year = rep(year, nrow(Res)),
                        sex = rep(sex, nrow(Res)),
                        name = Res[ , "name"],
                        home = Res[ , "home"],
                        age = as.numeric(Res[ , "ag"]), 
                        runTime = runTime,
                        stringsAsFactors = F)
  
  invisible(Results)
  
}

womenDF <- mapply(createDF, womenResMat, year = 1999:2012, sex = "W", SIMPLIFY = F)


#check NA values for runTime
sapply(womenDF, function(x) sum(is.na(x$runTime)))

#check NA values for age
sapply(womenDF, function(x) sum(is.na(x$age)))

#check NA values for year
sapply(womenDF, function(x) sum(is.na(x$year)))

sum(sapply(womenDF, nrow))
       
