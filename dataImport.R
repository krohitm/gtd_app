
#function to import and clean data
dataImport <- function() {
  #input data
  terr <- read.csv(
    './data/globalterrorismdb_0616dist.csv',
    check.names = FALSE,
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  #data cleaning
  #rename columns for easier understanding
  terr <- rename(
    terr,
    id = eventid,
    year = iyear,
    nation = country_txt,
    Region = region_txt,
    attack = attacktype1_txt,
    target = targtype1_txt,
    weapon = weaptype1_txt,
    Killed = nkill,
    wounded = nwound
  )
  
  #transform killed and wounded columns to integer type
  terr$Killed = as.integer(terr$Killed)
  terr$wounded = as.integer(terr$wounded)
  
  #change NA killed/wounded to 0
  terr$Killed[which(is.na(terr$Killed))] = 0
  terr$wounded[which(is.na(terr$wounded))] = 0
  
  #create derived column casualties
  terr$casualties = as.integer(terr$Killed + terr$wounded)
  
  #update nation names for JOINS with maps later
  terr$nation[terr$nation == "United States"] <- "USA"
  terr$nation[terr$nation == "United Kingdom"] <- "UK"
  terr$nation[terr$nation == "People's Republic of the Congo"] <-
    "Republic of Congo"
  terr$nation[terr$nation == "Bosnia-Herzegovina"] <-
    "Bosnia and Herzegovina"
  terr$nation[terr$nation == "Slovak Republic"] <- "Slovakia"
  
  return(terr)
}