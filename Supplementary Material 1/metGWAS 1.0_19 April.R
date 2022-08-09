##### metGWAS 1.0

##### FUNCTIONS #####

# Set Up
check_install <- function(reqPackages){
  # This function checks if the desired packages are installed, and installs them only if they are not.
  #
  # Parameters:
  # reqPackages - vector of package names
  # 
  # Returns:
  # NULL
  
  presPackages <- rownames(installed.packages())
  for(p in reqPackages){
    if(!(p %in% presPackages)){
      install.packages(p)
    }
  }
}
startUp <- function(reqPackages){
  # This function gives users the welcome prompts of the workflow, prompts them to have their R object loaded if they wish to use it and controls installation of packages.
  #
  # Parameters:
  # reqPackages - character vector of packages required for the workflow
  #
  # Returns:
  # NULL
  
  okInput <- c("q", "Q", "c", "C")
  
  # R object information for user
  cat("This workflow requires data input. Generally, the user is given the choice between inputting R objects or excel csv files. If inputting R objects, these objects must be loaded into the environment before starting the workflow.\n")
  cat("For more details see the walkthrough tutorial.\n\n")
  
  # Obtain user input
  cat("Do you intend to use R objects as input at any point (y or n)? ")
  useRObj <- readline()
  useRObj <- evalProceed(useRObj)
  while(useRObj == "i"){
    cat("Invalid input.\nDo you intend to use R objects at any point (y or n)? ")
    useRObj <- readline()
    useRObj <- evalProceed(useRObj)
  }
  
  if(useRObj == "y"){
    # Ensure the R object is loaded
    cat("\nPlease ensure the R objects are already loaded in your environment.\n")
    cat("If it is not, please enter quit, load the R objects, and restart (source) this workflow.\n")
    cat("Quit = q\n")
    cat("Continue = c\n")
    cat("Quit or continue: ")
    
    # Obtain user input
    loaded <- readline()
    while(!(loaded %in% okInput)){
      cat("Invalid input. \nPlease enter q or c: ")
      loaded <- readline()
    }
    
    # Quit if necessary
    if(loaded %in% c("q", "Q")){
      stop("Workflow quit.\n")
    }else if(loaded %in% c("c", "C")){
      # Obtain name of R object
      proceed <- "n"
      while(proceed == "n"){
        cat("If using a MetaboAnalyst R object, please enter the name now (otherwise press enter):\n")
        objName <- readline()
        if(objName == ""){
          cat("Not using a MetaboAnalyst R object. Confirm (y or n)? ")
        }else{
          cat("You entered ", objName, " as the name of your MetaboAnalyst object. \nConfirm (y or n)? ", sep="")
        }
        proceed <- readline()
        proceed <- evalProceed(proceed)
        while(proceed == "i"){
          cat("Invalid input.\nConfirm (y or n)? ")
          proceed <- readline()
          proceed <- evalProceed(proceed)
        }
      }
      assign("mSet", objName)
    }
  }
  
  # Installation information for user
  cat("This workflow requires the following packages to be installed:\n")
  cat(reqPackages, sep="\n")
  cat("\nIf you would like to manually install these packages, please enter quit, install the packages, and restart (source) this workflow.\n")
  cat("If you would like to allow the workflow to install any missing packages (or you've already installed the packages yourself), please enter continue.\n")
  cat("Quit = q\n")
  cat("Continue = c\n")
  cat("Quit or continue: ")
  
  # Obtain user input
  autoInstall <- readline()
  while(!(autoInstall %in% okInput)){
    cat("Invalid input. \nPlease enter q or c: ")
    autoInstall <- readline()
  }
  
  # Install if desired and necessary
  if(autoInstall %in% c("q", "Q")){
    stop("Workflow quit.\n")
  }else if(autoInstall %in% c("c", "C")){
    check_install(reqPackages)
    cat("\nPackages present/installed.\n")
  }
}
setChrome <- function(){
  # This function instructs the user on how to select the appropriate chrome driver for RSelenium and obtains their input.
  #
  # Parameters:
  # none
  #
  # Returns:
  # chromeVersion = character representing the chrome driver the user wants us to use
  
  # Instructions for user
  cat("Some modules in this workflow will need to interact with your chrome browser.\n")
  cat("Please check what version your chrome driver is (and press enter to continue).\n")
  readline()
  cat("The following are different versions of chrome drivers that this workflow can use:\n")
  
  # Display available drivers for RSelenium
  okDrivers <- binman::list_versions("chromedriver")
  for(i in 1:length(okDrivers)){
    cat("For ", names(okDrivers)[[i]], ":\n   ", sep="")
    cat(okDrivers[[i]], sep="\n   ")
  }
  
  # Instructions for user
  cat("Please select a driver from the above list that most closely matches your current chrome driver. The left numbers in each version are more important, the numbers after later decimals do not need to match exactly. \nIf none of these work, you may need to update your chrome driver first.\n")
  cat("Please enter the matching driver from the above list (enter the full version even if it doesn't match exactly: ")
  
  # Obtain user input and check it is valid
  chromeVersion <- readline()
  
  okInput <- FALSE
  while(!okInput){
    for(i in (okDrivers)){
      if(chromeVersion %in% i){
        okInput <- TRUE
      }
    }
    if(!okInput){
      cat("Invalid input. \nPlease ensure your input exactly matches one of the driver versions listed below (include all decimals):\n")
      for(i in 1:length(okDrivers)){
        cat("For ", names(okDrivers)[[i]], ":\n   ", sep="")
        cat(okDrivers[[i]], sep="\n   ")
      }
      cat("Enter driver: ")
      chromeVersion <- readline()
    }
  }
  
  return(chromeVersion)
}

evalProceed <- function(answer){
  # This function takes strings and determines if they represent the input "yes", "no" or are invalid strings. It then outputs 'y', 'n', or 'i' respectively.
  #
  # Parameters:
  # answer = a character to evalutate
  #
  # Returns:
  # proceed = character: 'y', 'n' or 'i'
  
  yes <- c("y", "Y", "yes", "Yes", "YES",
           "YEs", "YeS", "yEs", "yES", "yeS")
  no <- c("n", "N", "no", "No", "NO")
  if(answer %in% yes){
    proceed <- "y"
  }else if(answer %in% no){
    proceed <- "n"
  }else{
    proceed <- "i"
  }
  return(proceed)
}
setDir <- function(){
  # This function asks user if and where they want to save results. It ensures the file path the user input is valid.
  #
  # Parameters:
  # None
  # Returns:
  # savePath = string representing the directory where results should be saved
  
  # Obtain user input
  cat("If you would like the workflow to automatically save its results, please enter a file path.\nIf you would like to save results yourself after the workflow is complete, press enter without supplying a file path.\n")
  cat("File path: ")
  savePath <- readline()    #if user inputs \, it will automatically be \\
  proceed <- "n"
  
  # Check input
  while(proceed == "n"){
    if(savePath == ""){
      # Ensure user doesn't want to save
      cat("The workflow will not save results. Proceed (y or n)? ")
      proceed <- readline()
      proceed <- evalProceed(proceed)
      while(proceed == "i"){
        cat("Invalid input.\nProceed (y or n)? ")
        proceed <- readline()
        proceed <- evalProceed(proceed)
      }
      if(proceed == "n"){
        cat("Please enter a file path: ")
        savePath <- readline()
      }
    }else{
      # Ensure file path is valid (by testing if setwd works)
      pathValid <- tryCatch(
        {
          setwd(savePath)
          pathValid <- TRUE
        },
        error=function(cond){
          message("The path provided cannot be set as a working directory. Below is the error message:")
          message(cond)
          message("\n")
          
          pathValid <- FALSE
          return(pathValid)
        }
      )
      
      #if invalid, prompt for path again
      if(!pathValid){
        cat("The path entered was not valid, please try again:\n")
        savePath <- readline()
        #proceed is still unaltered (its "n") at this point
      }else{
        #if valid, check user meant it
        cat("The path you entered was:\n")
        cat(savePath)
        cat("\nProceed (y or n)? ")
        proceed <- readline()
        proceed <- evalProceed(proceed)
        while(proceed == "i"){
          cat("Invalid input.\nProceed (y or n)? ")
          proceed <- readline()
          proceed <- evalProceed(proceed)
        }
        if(proceed == "n"){
          cat("Please enter a file path: ")
          savePath <- readline()
        }
      }
    }
  }
  
  return(savePath)
}

# Functions called in multiple modules
setThresh <- function(disp){
  # This function converts an inputted string to a numeric value and determines if it is appropriate (ie between 0 and 1)
  #
  # Parameters:
  # disp = string to display to user to ask threshold
  # 
  # Returns:
  # thresh = a number between 0 and 1
  
  # Obtain user input
  cat(disp)
  thresh <- readline()
  
  # Ensure input is valid
  inBounds <- FALSE
  while(!inBounds){
    threshNumeric <- FALSE
    while(!threshNumeric){
      threshNumeric <- tryCatch(
        {
          as.numeric(thresh)
          threshNumeric <- TRUE
        },
        warning=function(cond){
          message("You did not provide a proper number.\n")
          threshNumeric <- FALSE
          return(threshNumeric)
        }
      )
      if(!threshNumeric){
        cat("Please enter a threshold (between 0 and 1):\n")
        thresh <- readline()
      }
      
    }
    thresh <- as.numeric(thresh)
    inBounds <- (thresh >= 0 & thresh <= 1)
    if(!inBounds){
      cat("The threshold entered is out of bounds.\n")
      cat("Please enter a threshold (between 0 and 1):\n")
      thresh <- readline()
    }
  }
  
  return(thresh)
}
loadDiscoverable <- function(){
  # This function asks users for a pathway to their discoverable gene set and evaluates if the pathway exists.
  #
  # Parameters:
  # none
  #
  # Returns:
  # dataPath = string representing a valid path to the discoverable gene set
  
  cat("Please enter a file path to your discoverable gene set (an RData file):\n")
  dataPath <- readline()
  validPath <- file.exists(dataPath)
  while(!validPath){
    cat("Invalid file path.\n")
    cat("Please enter a file path to your discoverable gene set (RData file):\n")
    dataPath <- readline()
    validPath <- file.exists(dataPath)
  }
  
  return(dataPath)
}

# Module 1
cNumber <- function(id){
  # This function tests if string id fits the format C#####.
  #
  # Parameters:
  # id = string to test
  #
  # Returns:
  # isCode = boolean (TRUE/FALSE) indicating if id is of the form C#####
  
  isCode <- FALSE
  if(str_sub(id, start=1, end=1) == "C"){
    nums <- str_sub(id, start=2)
    if(typeof(as.integer(nums))=="integer"){
      isCode <- TRUE
    }
  }
  return(isCode)
}
check_na <- function(strVec){
  # This function looks at a vector of strings and determines which indices are NA.
  #
  # Parameters:
  # strVec = character vector
  #
  # Returns:
  # ind = vector of indices
  
  ind <- which(is.na(strVec))
  naNames <- c("na", "Na", "NA")
  ind <- c(ind, which(strVec %in% naNames))
  
  return(ind)
}
appendMet_Robj<- function(invesPaths, altNames, hits, chromeVersion){
  # This function appends the metabolites of interest from each pathway onto the appropriate rows of invesPaths.
  # Assumes your organism of interest is homo sapiens. This can be altered by setting orgCode.
  #
  # Parameters:
  # invesPaths = data frame of pathways with our metabolites enriched, obtained from mSet (rownames are path ids)
  # altNames = data frame of metabolites of interest and other names and codes that match it, made from mSet
  # hits = list where elements are named after pathways and each element contains the metabolites of interest that were found in the pathway
  # chromeVersion = string representing the driver version chrome should be running
  # 
  # Returns:
  # pathsAndMet = list containing 3 elements (in the following order):
  #     invesPaths (same inputted data frame but with metabolites appended)
  #     responsibleMetabolites (vector of the metabolites appended)
  #     missingMet (vector of the metabolites that don't have a correpsonding KEGG C number (id))
  
  newcol <- ncol(invesPaths)+1
  responsibleMetabolites <- c()
  orgCode <- "hsa"
  
  # Determine how many metabolites don't have a KEGG C number (in which case, they may not be searched for)
  #missingInd <- which(is.na(altNames$KEGG))
  missingInd <- check_na(altNames$KEGG)
  missingMet <- altNames$Query[missingInd]
  
  # Open browser
  cat("\n Chrome will pop up and the workflow will automatically control it.\n")
  cat("DO NOT INTERFER WITH THE BROWSER.\n")
  cat("Press enter to continue...")
  readline()
  driver <- rsDriver(browser = "chrome", chromever=chromeVersion)
  keggBrowser <- driver[["client"]]
  
  # For each pathway...
  for(i in 1:nrow(invesPaths)){
    curPathway <- rownames(invesPaths)[i]
    
    # Find metabolites of interest in the pathway
    hitsInd <- which(names(hits)==curPathway)
    toAppend <- hits[[hitsInd]]
    namesInd <- which(altNames$KEGG %in% toAppend)
    toAppend <- altNames$Query[namesInd]
    
    # Append the metabolites to the overall vector of metabolites and to the invesPath table
    already <- which(toAppend %in% responsibleMetabolites)
    if(length(already) == 0){
      responsibleMetabolites <- c(responsibleMetabolites, toAppend)
    }else{
      responsibleMetabolites <- c(responsibleMetabolites, toAppend[-already])
    }
    
    toAppend <- str_c(toAppend, collapse=";")
    invesPaths[i, newcol] <- toAppend
    
    # Change rowname from code to name
    keggBrowser$navigate("https://www.genome.jp/kegg/pathway.html")
    orgBox <- keggBrowser$findElement(using = 'css selector', "#main > div.searchbox > form > div:nth-child(1) > input[type=text]:nth-child(3)")
    orgBox$clearElement()
    orgBox$sendKeysToElement(list(orgCode))
    searchBox <- keggBrowser$findElement(using = 'css selector', "#main > div.searchbox > form > div:nth-child(2) > input[type=text]:nth-child(3)")
    searchBox$clearElement()
    searchBox$sendKeysToElement(list(curPathway, "\uE007"))
    keggScrape <- read_html(keggBrowser$getPageSource()[[1]])
    correspondingPathway <- keggScrape %>% html_nodes("tr:nth-child(2) .data1:nth-child(3)") %>% html_text()
    rownames(invesPaths)[i] <- correspondingPathway
    
  }
  
  # Close browser
  keggBrowser$close()
  driver[["server"]]$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  colnames(invesPaths)[newcol] <- "hitNames"
  
  pathsAndMet <- list(invesPaths=invesPaths, responsibleMetabolites=responsibleMetabolites, missingMet=missingMet)
  return(pathsAndMet)
}
appendMet_file <- function(invesPaths, altNames, chromeVersion){
  # This function appends the metabolites of interest from each pathway onto the appropriate rows of invesPaths
  # Assumes your organism of interest is homo sapiens. This can be altered by setting orgCode.
  #
  # Parameters:
  # invesPaths = data frame of pathways with our metabolites enriched (rownames are names of paths)
  # altNames = data frame of metabolites of interest and other names that match it
  # chromeVersion = string denoting the driver version chrom should be running
  #
  # Returns:
  # pathsAndMet = list containing 4 elements (in the following order):
  #     invesPaths (data frame, similar to the inputted invesPaths but with metabolites appended)
  #     responsibleMetabolites (vector of the metabolites appended) 
  #     missingMet (vector of metabolite names that didn't have a corresponding KEGG C number (id))
  #     moreHits (vector of pathway names that have more hits than MetaboAnalyst first identified)
  
  
  newcol <- ncol(invesPaths)+1
  responsibleMetabolites <- c()
  moreHits <- c()
  orgCode <- "hsa"
  
  # Determine how many metabolites don't have a KEGG C number and can't be searched for in KEGG
  #missingInd <- which(is.na(altNames$KEGG))
  missingInd <- check_na(altNames$KEGG)
  missingMet <- altNames$Query[missingInd]
  
  # Open browser
  cat("\n Chrome will pop up and the workflow will automatically control it.\n")
  cat("DO NOT INTERFER WITH THE BROWSER.\n")
  cat("Press enter to continue...")
  readline()
  driver <- rsDriver(browser = "chrome", chromever=chromeVersion)
  keggBrowser <- driver[["client"]]
  
  #For each pathway...
  for(i in 1:nrow(invesPaths)){
    curPathway <- rownames(invesPaths)[i]
    
    # Find the pathway on KEGG
    keggBrowser$navigate("https://www.genome.jp/kegg/pathway.html")
    #orgBox <- keggBrowser$findElement(using = 'css selector', "#main > div.searchbox > form > div:nth-child(1) > input[type=text]:nth-child(3)") #OLD
    orgBox <- keggBrowser$findElement(using= 'css selector', "#content > div.searchbox > form > div:nth-child(1) > input[type=text]:nth-child(3)") #NEW (website updated)
    orgBox$clearElement()
    orgBox$sendKeysToElement(list(orgCode))
    #searchBox <- keggBrowser$findElement(using = 'css selector', "#main > div.searchbox > form > div:nth-child(2) > input[type=text]:nth-child(3)") #OLD
    searchBox <- keggBrowser$findElement(using = 'css selector', "#content > div.searchbox > form > div:nth-child(2) > input[type=search]:nth-child(3)")
    searchBox$clearElement()
    searchBox$sendKeysToElement(list(curPathway, "\uE007"))
    keggScrape <- read_html(keggBrowser$getPageSource()[[1]])
    linkResult <- keggScrape %>% html_nodes("tr:nth-child(2) td:nth-child(1) a") %>% html_attr(name = "href")
    keggBrowser$navigate(linkResult)
    
    # Get all compounds of the pathway
    keggScrape <- read_html(keggBrowser$getPageSource()[[1]])
    res <- keggScrape %>% html_nodes("table") %>% .[[1]] %>% html_table(fill=TRUE)
    res <- res[,1:2]    #only first two columns are relevant
    colnames(res) <- c("code", "name")
    res <- res[(which(res$code == "Compound")+1):nrow(res),] #the header compound and everything above is irrelevant
    #now get rid of everything below the last C##### entry as it is not a compound (assume there's at least 1 compound)
    j <- 0
    isCode <- TRUE
    while(j < nrow(res) & isCode){
      j <- j + 1
      isCode <- cNumber(res$code[j])
    }
    if(j == nrow(res) & isCode){
      res <- res[1:(j-1),]
    }else{
      res <- res[1:(j-1),] 
    }
    
    # Find metabolites of interest from all compounds
    metPresent <- which(altNames$KEGG %in% res$code)
    toAppend <- c(str_trim(altNames$Query[metPresent]))
    
    # Append the metabolites to the overall vector of metabolites
    already <- which(toAppend %in% responsibleMetabolites)
    if(length(already) == 0){
      responsibleMetabolites <- c(responsibleMetabolites, toAppend)
    }else{
      responsibleMetabolites <- c(responsibleMetabolites, toAppend[-already])
    }
    
    # Update the hits column if necessary
    trueHits <- length(toAppend)
    if(trueHits != invesPaths$Hits[i]){
      invesPaths$Hits[i] <- trueHits
      moreHits <- c(moreHits, curPathway)
    }
    
    # Append the metabolites to the invesPath table
    toAppend <- str_c(toAppend, collapse=";")
    invesPaths[i, newcol] <- toAppend
    
  }
  
  # Close browser
  keggBrowser$close()
  driver[["server"]]$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) #frees all ports (required for updated RSelenium)
  
  colnames(invesPaths)[newcol] <- "hitNames"
  pathsAndMet <- list(invesPaths=invesPaths, responsibleMetabolites=responsibleMetabolites, missingMet=missingMet, moreHits=moreHits)
  
  return(pathsAndMet)
}
standardizeColTypes <- function(invesPaths){
  # This function takes the data frame invesPaths and forces its columns into certain data types.
  # Using downloaded files vs R objects give the same results but the format of the data is slightly different and this function standardizes the format.
  #
  # Parameters:
  # invesPaths = data frame (the pathway results with the appended metabolites)
  # 
  # Returns:
  # invesPaths = standardized (in terms of column types) version of input
  
  invesPaths$Total.Cmpd <- as.integer(invesPaths$Total.Cmpd)
  invesPaths$Hits <- as.integer(invesPaths$Hits)
  invesPaths$Raw.p <- as.numeric(invesPaths$Raw.p)
  invesPaths$neg.log.p <- as.numeric(invesPaths$neg.log.p)
  invesPaths$FDR <- as.numeric(invesPaths$FDR)
  invesPaths$Impact <- as.numeric(invesPaths$Impact)
  invesPaths$Holm.adjust <- as.numeric(invesPaths$Holm.adjust)
  invesPaths$hitNames <- as.character(invesPaths$hitNames)
  
  return(invesPaths)
}
conversion <- function(responsibleMetabolites, chromeVersion){
  # This function takes a vector of metabolite names and converts them to HMDB ids that will be accepted by MBRole.
  #
  # Parameters:
  # responsibleMetabolites = vector of metabolites names
  # chromeVeresion = string representing the driver version chrome should be running
  # 
  # Returns:
  # responsibleMetabolites_df = dataframe of where each row is an input metabolite name (alphabetically ordered) and its corresponding HMDB code
  
  metInput <- str_c(responsibleMetabolites, collapse="\n")
  
  driver <- rsDriver(browser = "chrome", chromever=chromeVersion)
  convertBrowser <- driver[["client"]]
  
  convertBrowser$navigate("http://csbg.cnb.csic.es/mbrole2/conversion.php")
  compBox <- convertBrowser$findElement(using = 'css selector', "#content > form > div:nth-child(1) > div.input > textarea")
  compBox$clearElement()
  compBox$sendKeysToElement(list(metInput))
  dbBox <- convertBrowser$findElement(using = 'css selector', "#t7 > label > input[type=checkbox]")
  dbBox$clickElement()
  searchButton <- convertBrowser$findElement(using = 'css selector', "#send > button:nth-child(1)")
  searchButton$clickElement()
  
  convertScrape <- read_html(convertBrowser$getPageSource()[[1]])
  correspondingResults <- convertScrape %>% html_nodes("#results") %>% .[[1]] %>% html_table()
  correspondingResults <- correspondingResults[which(correspondingResults$`Output ID source`=="HMDB"),]
  
  responsibleMetabolites <- data.frame(metNames=correspondingResults$`Input ID`, HMDBcode=correspondingResults$`Output ID`, stringsAsFactors = FALSE)
  
  convertBrowser$close()
  driver[["server"]]$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  responsibleMetabolites <- responsibleMetabolites[order(responsibleMetabolites$metNames), ]
  return(responsibleMetabolites)
}
check_dups <- function(responsibleMetabolites){
  # This function takes a table of metabolites and codes and determines which metabolites appear more than once (have more than 1 code).
  #
  # Parameters:
  # responsibleMetabolites = dataframe containing two columns of metabolite names and of corresponding HMDB codes
  # 
  # Returns:
  # duplicateMetabolites = dataframe that is a subset of responsibleMetabolites, containing only metabolites that appear more than once
  
  occurences <- table(responsibleMetabolites$metNames)
  dups <- names(which(occurences > 1))
  dupsRows <- which(responsibleMetabolites$metNames %in% dups)
  duplicatedMetabolites <- responsibleMetabolites[dupsRows,]
  
  return(duplicatedMetabolites)
}
pathOverRep <- function(){
  # This function determines if the pathway over-representation module should be run and then runs it.
  #
  # Parameters:
  # None (but note that calling variable mSet accesses object mSet defined in the main environment and calling variable savePath accesses object savePath in the main environment)
  # 
  # Returns:
  # NULL if the user doesn't want to run the module
  # otherwise
  # results = list of the following elements (not all will be present depending on the input data):
  #     invesPaths = data frame of pathways with our metabolites enriched (rownames are names of paths)
  #     responsibleMetabolites = data frame with column of metabolite names and column of corresponding code names
  #     dups = subset of responsibleMatbolites that contains only metabolites that appear more than once
  #     missingMet = vector of metabolites that user inputed and didn't have a C code
  #     moreHits = vector of pathways where more metabolites of interest were found than expected based on MetaboAnalyst
  
  # Determine if user wants to run module
  cat("Do you wish to run the \"Pathways of Over-Representation\" module (y or n)? ")
  proceed <- readline()
  proceed <- evalProceed(proceed)
  while(proceed == "i"){
    cat("Invalid input.\nProceed (y or n)? ")
    proceed <- readline()
    proceed <- evalProceed(proceed)
  }
  
  # Run module
  if(proceed == "y"){
    
    cat("This module uses results from MetaboAnalyst's pathway analysis module (see manual for more details).\n\n")
    
    # Determine type of data and format of input
    cat("Was your input into MetaboAnalyst qualitative (a list of differentially expressed metabolites) or quantitative (a concentration table)?\n")
    cat("1 - qualitative \n")
    cat("2 - quantitative \n")
    cat("Enter a number: ")
    dataType <- readline()
    while(dataType != "1" & dataType != "2"){
      cat("Invalid input. Enter a number: ")
      dataType <- readline()
    }
    
    cat("\nWhat format are your results?\n")
    cat("1 - R object created using package MetaboAnalystR.\n")
    cat("2 - Excel files created and downloaded from MetaboAnalyst online.\n")
    cat("Enter a number: ")
    formatOp <- readline()
    while( formatOp != "1" & formatOp != "2"){
      cat("Invalid input. Enter a number: ")
      formatOp <- readline()
    }
    
    # Determine desired thresholds
    disp <- "What significance threshold do you want to filter the FDR adjusted p values by?\n"
    p_thresh <- setThresh(disp)
    disp <- "What significance threshold do you want to filter the impact factor by?\n"
    i_thresh <- setThresh(disp)
    
    if(formatOp == 1){
      cat("Using R object...")
      
      # Obtain and format pathway results
      if(dataType == "1"){
        paths <- as.data.frame(mSet$analSet$ora.mat)
        paths <- subset(paths, select=-c(2))
        hits <- mSet$analSet$ora.hits
      }else{
        paths <- as.data.frame(mSet$analSet$qea.mat)
        hits <- mSet$analSet$qea.hits
      }
      colnames(paths) <- c("Total.Cmpd", "Hits", "Raw.p", "neg.log.p", "Holm.adjust", "FDR", "Impact")
      
      # Apply thresholds to pathways
      invesPaths <- paths[which((paths$FDR <= p_thresh) & (paths$Impact >= i_thresh)),]
      
      # Obtain name mappings of input metabolites
      altNames <- as.data.frame(mSet$dataSet$map.table, stringsAsFactors = FALSE)
      
      # Append the metabolites of interest present in each pathway to the appropriate row
      pathsAndMet <- appendMet_Robj(invesPaths, altNames, hits, chromeVersion)
      
      # Create return object
      invesPaths <- pathsAndMet$invesPaths
      invesPaths <- standardizeColTypes(invesPaths)    
      responsibleMetabolites <- pathsAndMet$responsibleMetabolites
      if(sum(str_detect(responsibleMetabolites, "HMDB")) == 0){
        #only apply conversion function if the input in MA (the query) is not already HMDB ids
        responsibleMetabolites <- conversion(responsibleMetabolites, chromeVersion)
      }else{
        #if input (the query) was already HMDB ids, just turn it into a dataframe for check_dups to work
        responsibleMetabolites <- data.frame(metNames=responsibleMetabolites, HMDBcode=responsibleMetabolites, stringsAsFactors = FALSE)
      }
      dups <- check_dups(responsibleMetabolites)
      missingMet <- pathsAndMet$missingMet
      
      results <- list(invesPaths=invesPaths, responsibleMetabolites=responsibleMetabolites, dups=dups, missingMet=missingMet)
    }else{
      cat("Using excel file...")
      
      # Obtain pathway analysis results table
      cat("Please enter a file path to your pathway analysis results table:\n")
      dataPath <- readline()
      validPath <- file.exists(dataPath)
      while(!validPath){
        cat("Invalid file path.\n")
        cat("Please enter a file path to your results table:\n")
        dataPath <- readline()
        validPath <- file.exists(dataPath)
      }
      paths <- read.csv(dataPath, header=TRUE, row.names=1, stringsAsFactors=FALSE)
      
      # Obtain name mappings of input metabolites
      cat("Please enter a file path to the name map generated by pathway analysis:\n")
      dataPath <- readline()
      validPath <- file.exists(dataPath)
      while(!validPath){
        cat("Invalid file path.\n")
        cat("Please enter a file path to your name map:\n")
        dataPath <- readline()
        validPath <- file.exists(dataPath)
      }
      altNames <- read.csv(dataPath, header=TRUE, stringsAsFactors=FALSE)
      
      # Format the file 
      if(dataType == "1"){
        paths <- subset(paths, select=-c(2))
      }
      colnames(paths) <- c("Total.Cmpd", "Hits", "Raw.p", "neg.log.p", "Holm.adjust", "FDR", "Impact")
      
      # Apply thresholds to pathways
      invesPaths <- paths[which((paths$FDR <= p_thresh) & (paths$Impact >= i_thresh)),]
      
      # Append the metabolites of interest present in each pathway to the appropriate row
      pathsAndMet <- appendMet_file(invesPaths, altNames, chromeVersion)
      
      # Create return object
      invesPaths <- pathsAndMet$invesPaths
      invesPaths <- standardizeColTypes(invesPaths)
      responsibleMetabolites <- pathsAndMet$responsibleMetabolites
      if(sum(str_detect(responsibleMetabolites, "HMDB")) == 0){
        #only apply conversion function if the input in MA (the query) is not already HMDB ids
        responsibleMetabolites <- conversion(responsibleMetabolites, chromeVersion)
      }else{
        #if input (the query) was already HMDB ids, just turn it into a dataframe for check_dups to work
        responsibleMetabolites <- data.frame(metNames=responsibleMetabolites, HMDBcode=responsibleMetabolites, stringsAsFactors = FALSE)
      }
      dups <- check_dups(responsibleMetabolites)
      missingMet <- pathsAndMet$missingMet
      moreHits <- pathsAndMet$moreHits
      
      results <- list(invesPaths=invesPaths, responsibleMetabolites=responsibleMetabolites, dups=dups, missingMet=missingMet, moreHits=moreHits)
    }
    
    cat("\nThe following metabolites found in the over-represented pathways had more than one HMDB id:\n")
    print(results$dups)
    cat("\nPathway Over-Representation Analysis module complete!\n")
    
    # Save results if desired
    if(savePath != ""){
      cat("Do you wish to save the results of this module (y or n)?\n")
      saveChoice <- readline()
      saveChoice <- evalProceed(saveChoice)
      while(saveChoice == "i"){
        cat("Invalid input. Do you wish to save the results of this module (y or n)?\n")
        saveChoice <- readline()
        saveChoice <- evalProceed(saveChoice)
      }
      
      if(saveChoice == "y"){
        setwd(savePath)
        cat("How do you wish to save the results?\n")
        cat("1 - as R objects in an RData file\n")
        cat("2 - as excel files\n")
        cat("3 - both\n")
        cat("4 - neither\n")
        cat("Enter a number: ")
        saveChoice <- readline()
        while( saveChoice != "1" & saveChoice != "2" & saveChoice != "3" & saveChoice != "4"){
          cat("Invalid input. Enter a number: ")
          saveChoice <- readline()
        }
        
        if(saveChoice == "1" | saveChoice == "3"){
          # save RData file
          res_pathOverRep <- results
          save(res_pathOverRep, file="mod1_res_pathOverRep.RData")
          rm(res_pathOverRep)
        }
        if(saveChoice == "2" | saveChoice == "3"){
          # save excel files
          write.csv(results$invesPaths, file="mod1_pathways_with_metabolites.csv", row.names=TRUE)
          write.csv(results$responsibleMetabolites, file="mod1_metabolitesNames_HMDBcodes.csv", row.names=FALSE)
          write.csv(results$dups, file="mod1_metabolites_with_multipleHMDBcodes.csv", row.names=FALSE)
          write.table(results$missingMet, file="mod1_metabolites_cIdMissing.csv", row.names=FALSE, col.names=c("metabolites_with_no_c_number_annotation"))
          if("moreHits" %in% names(results)){
            write.table(results$moreHits, file="mod1_pathways_with_extraHits.csv", row.names=FALSE, col.names=c("pathways_with_extra_hits_found"))
          }
        }
        
        cat("Results were saved in ", savePath, "\n", sep="")
      }
    }
    
    return(results)
  }
}

# Module 2
findProteinsGenes <- function(compoundCodes, chromeVersion){
  # This function takes a set of metabolites (as HMDB codes) and finds enzymes that interact with the metabolites (using HMDB).
  # It also identifies the genes for those enymes (also from HMDB) and ensures gene names are primary names from UniProt.
  # 
  # Parameters:
  # compoundCodes = vector of metabolite HMDB ids (strings)
  # chromeVersion = string representing the driver version chrome should be running
  # Returns:
  # EDIT RESULTS
  # results = list of the following elements:
  #   priMetCodes = dataframe with a column of input metabolite HMBD ids and the primary metabolite HMDB ids
  #   proteins = dataframe with columns for enzyme names, corresponding UniProt id and EC id, type according to UniProt (enzyme or unknown), and metabolite HMDB ids which the enzyme was found for
  #   noHMDBPage = vector (of strings) of metabolite HMDB ids that didn't have a HMDB webpage (NULL if all metabolites were have webpages)
  #   noProteins = vector (of strings) of metabolite HMDB ids that didn't have proteins associated
  
  
  # Obtain primary accession number for metabolites
  priMetCodes <- data.frame(input=compoundCodes, primary=compoundCodes, stringsAsFactors=FALSE) #intitalize map of codes to primary codes
  
  cat("\n Chrome will pop up and the workflow will automatically control it.\n")
  cat("DO NOT INTERFER WITH THE BROWSER.\n")
  cat("Press enter to continue...")
  readline()
  
  driver<- rsDriver(browser = "chrome", platform = "WINDOWS", chromever=chromeVersion)  # Open browser
  myBrowser <- driver[["client"]]
  for(i in 1:nrow(priMetCodes)){
    # Go to HMDB page and get new url (which contains primary HMDB id)
    metSiteURL <- paste("https://hmdb.ca/metabolites/", priMetCodes[i,1], sep="")
    myBrowser$navigate(metSiteURL)
    new <- myBrowser$getCurrentUrl()
    # Put new url in priMetCodes
    new <- str_split(new[[1]], "/")
    new <- new[[1]][length(new[[1]])]
    priMetCodes[i,2] <- new
  }
  myBrowser$close()
  driver[["server"]]$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  # Find interacting proteins on HMDB
  firstMet <- TRUE
  noHMDBPage <- c()
  noProteins <- c()
  for(metCode in priMetCodes$primary){
    # Navigate to appropriate HMDB page
    metSiteURL <- paste("https://hmdb.ca/metabolites/", metCode, "/metabolite_protein_links", sep="")
    pageExists <- tryCatch(
      {
        metSite <- read_html(metSiteURL)
      },
      error=function(cond){
        metSite <<- NULL
      }
    )
    if(!is.null(metSite)){
      # If the webpage exists for the metabolite:
      
      # Grab proteins for this metabolite
      #pages <- metSite %>% html_nodes("div") %>% .[[16]] %>% html_text()       #OLD
      pages <- metSite %>% html_nodes("div") %>% .[[17]] %>% html_text()        #NEW (site updated)
      if(startsWith(pages, "No metabolite protein links found")){
        noProteins <- c(noProteins, metCode)
      }else{
        if(startsWith(pages, "Displaying all") | startsWith(pages, "Displaying 1 metabolite protein link")){
          #All enzymes on one page, grab everything and format
          protForMet <- (metSite %>% html_nodes("table") %>% html_table(fill=TRUE))[[1]]
          protForMet <- select(protForMet, c("Name", "UniProt ID", "Gene Name", "Type"))
          # rightType <- which(protForMet$Type %in% c("Enzyme", "Unknown"))
          # protForMet <- protForMet[rightType,]
          protForMet$metCode <- rep(metCode, nrow(protForMet))
        }else{
          #There are multiple pages of results, calculate how many
          pages <- str_split(pages, " ")[[1]]
          resPerPage <- str_split(pages[5], "-")[[1]]
          resPerPage <- as.integer(str_trim(resPerPage[2]))
          resTotal <- as.integer(pages[7])
          pages <- ceiling(resTotal/resPerPage)
          
          #Append the results from all pages
          protForMet <- (metSite %>% html_nodes("table") %>% html_table(fill=TRUE))[[1]]
          
          for(i in 2:pages){
            metSiteURL_n <- paste(metSiteURL, "?c=hmdb_id&d=up&page=", i, sep="")
            metSite <- read_html(metSiteURL_n)
            protForMet_n <- (metSite %>% html_nodes("table") %>% html_table(fill=TRUE))[[1]]
            protForMet <- rbind(protForMet, protForMet_n)
          }
          
          protForMet <- select(protForMet, c("Name", "UniProt ID", "Gene Name", "Type"))
          # rightType <- which(protForMet$Type %in% c("Enzyme", "Unknown"))
          # protForMet <- protForMet[rightType,]
          protForMet$metCode <- rep(metCode, nrow(protForMet))
        }
        
        # Append enzymes for current metabolite to table for all metabolites
        if(firstMet){
          protForAllMet <- protForMet
          firstMet <- FALSE
        }else{
          protForAllMet <- merge(protForAllMet, protForMet, by=c("Name", "UniProt ID", "Gene Name", "Type"), all.x=TRUE, all.y=TRUE)
          protForAllMet$metCode <- paste(str_replace_na(protForAllMet$metCode.x, ""), 
                                        str_replace_na(protForAllMet$metCode.y, ""),
                                        sep="\t ")
          protForAllMet <- select(protForAllMet, -c("metCode.x", "metCode.y"))
          protForAllMet$metCode <- str_trim(protForAllMet$metCode)
        }
      }
    }else{
      # Note that the metabolite didn't have a HMDB page and let loop proceed to next metabolite
      noHMDBPage <- c(noHMDBPage, metCode)
      print("No HMDB page")
    }
  }
  
  
  #enzForAllMet <- enzForAllMet[order(enzForAllMet$"UniProt ID"),]
  
  results <- list(priMetCodes=priMetCodes, proteins=protForAllMet, noHMDBPage=noHMDBPage, noProteins=noProteins)
  
  return(results)
}
standardizeGeneNames <- function(proteins){
  # This function ensures that the gene names listed in the protein table from findProteinsGenes results are primary gene names.
  # The discoverable gene set is used to replace synonyms with their primary gene name.
  # If a gene is neither a primary name or synonym, it is removed. If a synonym maps to more than 3 primary names, it is removed.
  # If a gene maps to two or three primary names, all are used.
  #
  # Parameters:
  # proteins = dataframe of proteins (part of results returned by findProteinsGenes)
  # 
  # Returns:
  # proteins = the same dataframe but with genes names in their primary format
  
  
  # Load discoverable gene set (which maps gene synonyms to a primary gene name)
  dataPath <- loadDiscoverable()
  load(dataPath)
  
  synSplit <- str_split(discGenes$SynonymGeneNames, " ")
  synSplit2 <- c()
  for(i in 1:length(synSplit)){
    synSplit2 <- c(synSplit2, synSplit[[i]])
  }
  synSplit2 <- synSplit2[-which(synSplit2 == "")]
  syn_wManyPri <- table(synSplit2)[which(table(synSplit2) > 1)]
  
  toRemove_ind <- c()
  finalRow_ind <- nrow(proteins)
  for(i in 1:finalRow_ind){
    if(!(proteins$`Gene Name`[i] %in% discGenes$PrimaryGeneNames)){
      #if gene is not primary, it needs to be dealt with (primary genes don't need anything done to them)
      
      if(!(proteins$`Gene Name`[i] %in% synSplit2)){
        #gene is not a synonym either, so remove
        toRemove_ind <- c(toRemove_ind, i)
      }else{
        #does the synonym map to more than one gene?
        if(proteins$`Gene Name`[i] %in% names(syn_wManyPri)){
          #there's more than one primary gene
          
          #if there's more than 3 primary names remove gene
          if(proteins$`Gene Name`[i] %in% names(syn_wManyPri[which(syn_wManyPri > 3)])){
            toRemove_ind <- c(toRemove_ind, i)
          }else{
            #grab all primary names (2 or 3 of them)
            
            curGene <- proteins$`Gene Name`[i]
            k <- 1
            foundCount <- 0
            goalCount <- as.numeric(syn_wManyPri[which(names(syn_wManyPri) == curGene)])
            
            while(foundCount < goalCount){
              if(curGene %in% synSplit[[k]]){
                foundCount <- foundCount + 1
                #for the first primary gene, replace the current synonym, for the others, append
                if(foundCount == 1){
                  proteins$`Gene Name`[i] <- discGenes$PrimaryGeneNames[k]
                  proteins <- rbind(proteins, proteins[i,], stringsAsFactors=FALSE)
                }else{
                  proteins$`Gene Name`[nrow(proteins)] <- discGenes$PrimaryGeneNames[k]
                  if(foundCount < goalCount){
                    proteins <- rbind(proteins, proteins[nrow(proteins),], stringsAsFactors=FALSE)
                  }
                }
              }
              k <- k + 1
            }
          }
        }else{
          #there's only one primary gene, grab it
          notFound <- TRUE
          k <- 1
          while(notFound){
            if(proteins$`Gene Name`[i] %in% synSplit[[k]]){
              #replace synonym with primary
              proteins$`Gene Name`[i] <- discGenes$PrimaryGeneNames[k]
              notFound <- FALSE
            }else{
              #search next element
              k <- k + 1
            }
          }
        }
      }
    }
  }
  if(length(toRemove_ind) > 0){
    proteins <- proteins[-toRemove_ind,] 
  }
  
  return(proteins)
}
interProtGenes <- function(){
  # This function determines if the interacting proteins module should be run and then runs it.
  #
  # Parameters:
  # none (but note that if res_pathOverRep is called, it is accessing object res_pathOverRep from main environment and calling variable savePath accesses object savePath in the main environment)
  #
  # Returns:
  # NULL if user doesn't want to run the module
  # otherwise
  # results = list of the following elements:
  #   priMetCodes = matrix with a column of input metabolite HMBD ids and the primary metabolite HMDB ids
  #   proteins = dataframe with columns for protein names, corresponding UniProt id, primary gene name according to UniProt, type, and metabolite HMDB ids which the enzyme was found for
  #   noHMDBPage = vector (of strings) of metabolite HMDB ids that didn't have a HMDB webpage (NULL if all metabolites were have webpages)
  #   noProteins = vactor (of strings) of metabolite HMDB ids that didn't have proteins associated
  
  cat("Do you wish to run the \"Interacting Proteins and Genes\" module (y or n)? ")
  proceed <- readline()
  proceed <- evalProceed(proceed)
  while(proceed == "i"){
    cat("Invalid input.\nProceed (y or n)? ")
    proceed <- readline()
    proceed <- evalProceed(proceed)
  }
  if(proceed == "y"){
    cat("Running Interacting Proteins and Genes module...\n ")
    
    cat("\nAre you running this module immediately after module 1 (Pathways of Over-Representation)? In other words, was this workflow uninterrupted (y or n)?\n")
    unint <- readline()
    unint <- evalProceed(unint)
    while(unint == "i"){
      cat("Is this workflow uninterupted? Please enter y or n: ")
      unint <- readline()
      unint <- evalProceed(unint)
    }
    
    if(unint == "y"){
      responsibleMetabolites <- res_pathOverRep$responsibleMetabolites
    }else{
      # Determine format of responsibleMetabolites
      cat("This module uses a table with the same format as the responsibleMetabolites table created by the Pathway Over-Representation module (ie a column of metabolite names named \"metNames\" and a column of corresponding HMDB codes named \"HMDBcode\".\n\n")
      cat("Where is your table of metabolites and codes coming from?\n")
      cat("1 - R obj (data frame) created by module 1 \n")
      cat("2 - csv file\n")
      formatOp <- readline()
      while( formatOp != "1" & formatOp != "2"){
        cat("Invalid input. Enter a number: ")
        formatOp <- readline()
      }
      
      if(formatOp == "1"){
        # Ensure R object loaded
        cat("Please ensure your R object is loaded in the environment. Ensure the table is an object on it own, and doesn't need to be accessed with indexing or with $. Ensure the column names are metNames and HMDBcode.\n")
        cat("If you need to load or alter the R object, please enter quit, load the R object/rename it, and restart (source) this workflow.\n")
        cat("Quit = q\n")
        cat("Continue = c\n")
        cat("Quit or continue: ")
        loaded <- readline()
        while(!(loaded %in% c("c", "C", "q", "Q"))){
          cat("Invalid input. \nPlease enter q or c: ")
          loaded <- readline()
        }
        
        if(loaded %in% c("q", "Q")){
          stop("Workflow quit.\n")
        }
        
        # Get name of R object
        proceed <- "n"
        while(proceed == "n"){
          cat("What is the name of your R object containing metabolites and codes?\n")
          objName <- readline()
          cat("You entered ", objName, " as the name of your object. \n Confirm (y or n)?", sep="")
          proceed <- readline()
          proceed <- evalProceed(proceed)
          while(proceed == "i"){
            cat("Invalid input.\nConfirm (y or n)? ")
            proceed <- readline()
            proceed <- evalProceed(proceed)
          }
        }
        assign("responsibleMetabolites", get(objName))
      }else{
        cat("Using csv file...")
        
        # Obtain responsible metabolites table
        cat("Before proceeding, please ensure the column names in the csv file are metNames and HMDBcode. Press enter to continue. \n")
        readline()
        cat("Please enter a file path to your responsible metabolites table:\n")
        dataPath <- readline()
        validPath <- file.exists(dataPath)
        while(!validPath){
          cat("Invalid file path.\n")
          cat("Please enter a file path to your results table:\n")
          dataPath <- readline()
          validPath <- file.exists(dataPath)
        }
        responsibleMetabolites <- read.csv(dataPath, header=TRUE, stringsAsFactors=FALSE)
      }
    }
    
    # Search metabolites on HMDB
    compoundCodes <- responsibleMetabolites$HMDBcode
    proteinsOutput <- findProteinsGenes(compoundCodes, chromeVersion)
    proteins <- proteinsOutput$proteins
    proteins <- standardizeGeneNames(proteins)
    colnames(proteins) <- c("Name", "UniProt ID", "Primary Gene Name", "Type", "Primary metCode")
    priMetCodes <- proteinsOutput$priMetCodes #this includes any metabolite codes that would be removed due to gene name issues
    priMetCodes <- cbind(responsibleMetabolites$metNames, priMetCodes)
    
    results <- list(priMetCodes=priMetCodes, proteins=proteins, noHMDBPage=proteinsOutput$noHMDBPage, noProteins=proteinsOutput$noProteins)
    cat("Interacting Proteins and Genes module complete!\n")
    
    # Save object if desired
    if(savePath != ""){
      cat("Do you wish to save the results of this module (y or n)?\n")
      saveChoice <- readline()
      saveChoice <- evalProceed(saveChoice)
      while(saveChoice == "i"){
        cat("Invalid input. Do you wish to save the results of this module (y or n)?\n")
        saveChoice <- readline()
        saveChoice <- evalProceed(saveChoice)
      }
      
      if(saveChoice == "y"){
        setwd(savePath)
        cat("How do you wish to save the results?\n")
        cat("1 - as R objects in an RData file\n")
        cat("2 - as csv files\n")
        cat("3 - both\n")
        cat("4 - neither\n")
        cat("Enter a number: ")
        saveChoice <- readline()
        while( saveChoice != "1" & saveChoice != "2" & saveChoice != "3" & saveChoice != "4"){
          cat("Invalid input. Enter a number: ")
          saveChoice <- readline()
        }
        
        if(saveChoice == "1" | saveChoice == "3"){
          # save RData file
          res_interProtGenes <- results
          save(res_interProtGenes, file="mod2_res_interProtGenes.RData")
          rm(res_interProtGenes)
        }
        if(saveChoice == "2" | saveChoice == "3"){
          # save excel files
          write.csv(proteins, file="mod2_proteinsGenes.csv", row.names=FALSE)
          #write.csv(proteinsOutput$priMetCodes, file="mod3_primaryMetaboliteCodes.csv", row.names=FALSE)
          write.csv(priMetCodes, file="mod2_primaryMetaboliteCodes.csv", row.names=FALSE)
          if(length(proteinsOutput$noProteins) > 0){
            write.csv(proteinsOutput$noProteins, file="mod2_metabolitesWithoutProteins.csv", row.names=FALSE)
          }
          if(length(proteinsOutput$noHMDBPage) > 0){
            write.csv(proteinsOutput$noHMDBPage, file="mod2_checkMetaboliteHMDB.csv", row.names=FALSE)
          }
        }
        
        cat("Results were saved in ", savePath, "\n", sep="")
      }
    }
    
    return(results)
  }
}

# Module 3
discoverableGenes <- function(){
  # This function finds all genes that code for metabolite-interacting proteins (those present on HMDB) based on the UniProt database.
  # It automatically saves results.
  # The user will be asked for a ".tab" file representing the human proteome downloaded from UniProt. See tutorial for required columns
  # The user will also be asked for a ".xml" file representing the proteins present in HMDB.
  # (note that savePath will be used from the main environment)
  #
  # Parameters
  # none
  #
  # Returns
  # discGenes = dataframe with two columns: PrimaryGeneNames  and SynonymGeneNames
  #
  # Saves
  # discGenes into an .RData file
  
  # Obtain UniProt data table
  cat("Please enter a file path to your UniProt data table.\n")
  cat("Data table can be downloaded from https://www.uniprot.org/uniprot/?query=proteome:UP000005640 for human data.\n")
  cat("Ensure the correct columns are downloaded (see tutorial).\n")
  cat("File path:\n")
  uniprotFilePath <- readline()
  validPath <- file.exists(uniprotFilePath)
  while(!validPath){
    cat("Invalid file path.\n")
    cat("Please enter a file path to your UniProt data table:\n")
    uniprotFilePath <- readline()
    validPath <- file.exists(uniprotFilePath)
  }
  
  UniProt <- read.delim(uniprotFilePath, stringsAsFactors = FALSE)
  colnames(UniProt) <- c("UPid", "EntryName", "Status", "ProteinName", "GeneNames", "Org", "Length", 
                         "PrimaryGeneNames", "SynonymGeneNames")
  
  cat("Please enter a file path to your HMDB data table.\n")
  cat("Data table can be downloaded from https://hmdb.ca/downloads .\n")
  cat("Download the XML file 'All Proteins' (under section Metabolite and Protein Data). \n")
  cat("File path:\n")
  HMDBFilePath <- readline()
  validPath <- file.exists(HMDBFilePath)
  while(!validPath){
    cat("Invalid file path.\n")
    cat("Please enter a file path to your UniProt data table:\n")
    HMDBFilePath <- readline()
    validPath <- file.exists(HMDBFilePath)
  }
  
  HMDB_proteins <- XML::xmlToDataFrame(HMDBFilePath)

  #Filter data for proteins present in HMDB
  keepInd <- which(UniProt$UPid %in% HMDB_proteins$uniprot_id)
  UniProt_genesInfo <- UniProt[keepInd, c("GeneNames","PrimaryGeneNames","SynonymGeneNames")]
  
  #Re-format entries with more than one primary gene (so that each primary gene gets its own row)
  twoPri_ind <- str_detect(UniProt_genesInfo$GeneNames, ";")
  
  UniProt_genesInfo_onePri <- UniProt_genesInfo[!twoPri_ind,]
  UniProt_genesInfo_onePri <- UniProt_genesInfo_onePri[-which(UniProt_genesInfo_onePri$GeneNames == ""),] #remove entries with no genes
  
  UniProt_genesInfo_twoPri <- UniProt_genesInfo[twoPri_ind,]
  d <- str_split(UniProt_genesInfo_twoPri$GeneNames, ";", simplify=TRUE)
  e <- str_split(UniProt_genesInfo_twoPri$PrimaryGeneNames, ";", simplify=TRUE)
  f <- str_split(UniProt_genesInfo_twoPri$SynonymGeneNames, ";", simplify=TRUE)
  new_GeneNames <- c()
  new_PrimaryGeneNames <- c()
  new_SynonymGeneNames <- c()
  for(i in 1:ncol(d)){
    new_GeneNames <- c(new_GeneNames, d[,i])
    new_PrimaryGeneNames <- c(new_PrimaryGeneNames, e[,i])
    new_SynonymGeneNames <- c(new_SynonymGeneNames, f[,i])
  }
  UniProt_genesInfo_twoPri <- data.frame(GeneNames=new_GeneNames, PrimaryGeneNames=new_PrimaryGeneNames, 
                                         SynonymGeneNames=new_SynonymGeneNames, stringsAsFactors=FALSE)
  UniProt_genesInfo_twoPri <- UniProt_genesInfo_twoPri[(UniProt_genesInfo_twoPri$GeneNames != ""), ] #remove entries with no genes (caused by splitting)
  
  #amalgamate the discoverable gene set
  UniProt_geneSet <- rbind(UniProt_genesInfo_onePri, UniProt_genesInfo_twoPri, stringsAsFactors=FALSE)
  UniProt_geneSet$GeneNames <- str_trim(UniProt_geneSet$GeneNames) #remove leading/trailing whitespace
  UniProt_geneSet$PrimaryGeneNames <- str_trim(UniProt_geneSet$PrimaryGeneNames)
  UniProt_geneSet$SynonymGeneNames <- str_trim(UniProt_geneSet$SynonymGeneNames)
  
  #clean discoverable gene set
  if(sum(UniProt_geneSet$PrimaryGeneNames == "") > 0){
    #putative genes are somtimes in GeneNames but PrimaryGeneNames remains blank (ignore putative genes)
    UniProt_geneSet <- UniProt_geneSet[-which(UniProt_geneSet$PrimaryGeneNames == ""),]
  }
  UniProt_geneSet <- unique(UniProt_geneSet) #remove duplicate rows
  #amalgamate synonyms (sometimes GeneNames has synonyms not present in SynonymGeneNames, ignore these)
  priGeneCounts <- table(UniProt_geneSet$PrimaryGeneNames)
  for(i in 1:length(priGeneCounts)){
    if(priGeneCounts[i] > 1){
      indInterest <- which(UniProt_geneSet$PrimaryGeneNames == names(priGeneCounts)[i])
      #amalgamate synonyms
      allSynonyms <- UniProt_geneSet$SynonymGeneNames[indInterest]
      allSynonyms <- unique(allSynonyms)
      if("" %in% allSynonyms){
        allSynonyms <- allSynonyms[-which(allSynonyms == "")]
      }
      allSynonyms <- paste(allSynonyms, collapse=" ")
      #put all synonyms in first entry of primary gene
      UniProt_geneSet$SynonymGeneNames[(indInterest[1])] <- allSynonyms
      #mark other entries of primary gene for removal
      UniProt_geneSet$SynonymGeneNames[(indInterest[-1])] <- "remove"
    }
  }
  discGenes <- UniProt_geneSet[-which(UniProt_geneSet$SynonymGeneNames == "remove"),-1]
  
  # Let user know where the discoverable gene set will be saved
  setwd(savePath)
  today <- Sys.Date()
  today <- str_replace_all(today, "-", "_")
  newDGfile <- paste("discGenes_", today, ".RData", sep="")
  cat("The updated discoverable gene set will be saved as ", newDGfile, " in directory:\n", sep="")
  cat(getwd(), "\n\n")
  cat("Press enter to continue...\n")
  readline()
  
  save(discGenes, file=newDGfile)
  
  cat("Network updated and saved.\n")
  
  return(discGenes)
} #UniProt and HMDB (proteins)
updateNetwork_discGenes <- function(discGenes){
  # This function takes a downloaded GWAS catalogue and creates an adjacency matrix to represent a network where nodes are a study and trait combo, and the attributes of the nodes are the genes found to associate with the trait according to that study (limited to discoverable genes). If and only if there is significant overlap (according to a p value of a Jaccard coefficient) between the genes of two nodes, than there is a link between those nodes.
  #
  # Parameters
  # discGenes = dataframe of gene names (1 column with the primary name and the other with synonyms)
  # (note that savePath will be used from the main environment)
  #
  # Returns
  # results = list containing:
  #     all_genes = vector of factors representing all genes in the GWAS catalogue network (discoverable only)
  #     GWAS_cat = list where elements are named after study+trait names and contain vectors of factors representing genes
  #     adj_matrix_stat = matrix representing the GWAS catalogue where rows/col are nodes and elements are the centered Jaccard coefficients (which represents the overlap in genes between two nodes); elements on the diagonal are set to 0 as we are not interested in a node's overlap with itself
  #     adj_matrix_expec = matrix representing the GWAS catalogue where rows/col are nodes and elements are the expected Jaccard coefficients (which are used to center the original Jaccard coefficient); elements on the diagonal are set to 0 as we are not interested in a node's overlap with itself
  #     adj_matrix_pval = matrix representing the GWAS catalogue where rows/col are nodes and elements are the p values of the Jaccard coefficients (which represents how significant overlap in genes between two nodes is); elements on the diagonal are set to 1 as we are not interested in a node's overlap with itself
  #     perc_disc = estimated percentage of genes in GWAS catalogue that were discoverable and were kept in network (not returned by function updateNetwork_allGenes; estimated because a few gene synonyms map to multiple primary genes)
  #
  # Saves:
  # all_genes, GWAS_cat, adj_matrix_stat, adj_matrix_expec, adj_matrix_pval, and perc_disc (as separate objects, not part of a list) into an RData file
  
  #Determine file path to catalogue
  cat("Please ensure you have downloaded the GWAS catalogue. \n")
  proceed <- "n"
  while(proceed == "n"){
    cat("Please enter a file path to the downloaded .tsv file:\n")
    catFile <- readline()
    cat("You have entered the following file path as your GWAS catalogue:\n")
    cat(catFile, "\n")
    cat("Confirm (y or n)?\n")
    proceed <- readline()
    proceed <- evalProceed(proceed)
    while(proceed == "i"){
      cat("Invalid input.\nConfirm (y or n)? ")
      proceed <- readline()
      proceed <- evalProceed(proceed)
    }
    if(proceed == "y"){
      validPath <- file.exists(catFile)
      while(!validPath){
        cat("Invalid path.\n")
        proceed <- "n"
      }
    }
  }
  
  # Load the GWAS catalogue
  GWAS_cat <- read_delim(catFile, "\t", escape_double = FALSE, trim_ws = TRUE)
  # Get rid of irrelevant columns and rows with missing genes
  GWAS_cat <- select(GWAS_cat, "PUBMEDID", "STUDY", "DISEASE/TRAIT", "MAPPED_GENE")
  GWAS_cat <- GWAS_cat %>% filter(!is.na(MAPPED_GENE))
  # Ensure all genes are primary genes in the discoverable gene set (ie replace synonyms with primary)
  
  
  # Concatenate genes for each unique pubmed id + disease/trait row (new column is "x")
  GWAS_cat <- (aggregate(GWAS_cat$MAPPED_GENE, GWAS_cat[c('PUBMEDID', "STUDY", 'DISEASE/TRAIT')], paste, collapse=','))
  GWAS_cat <- mutate(GWAS_cat, x= str_replace_all(x, " - ", ","))   #note that "-" (no spaces) is valid
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, " ", ","))
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, ",,,", ","))
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, ",,", ","))
  
  # Replace synonyms with primary gene name, get rid of genes that are not discoverable and duplicate apperances, and accumulate all genes into a master vector
  pre_all_genes <- c()
  all_genes <- c()
  synSplit <- str_split(discGenes$SynonymGeneNames, " ")
  synSplit2 <- c()
  for(i in 1:length(synSplit)){
    synSplit2 <- c(synSplit2, synSplit[[i]])
  }
  synSplit2 <- synSplit2[-which(synSplit2 == "")]
  syn_wManyPri <- table(synSplit2)[which(table(synSplit2) > 1)]
  
  for(i in 1:nrow(GWAS_cat)){
    genes <- unique(str_split(GWAS_cat$x[i], ",")[[1]]) #genes in the row
    pre_all_genes <- c(pre_all_genes, genes) #master vector with the original genes 
    
    #discoverable genes
    original_length <- length(genes)
    for(j in 1:original_length){
      if(!(genes[j] %in% discGenes$PrimaryGeneNames)){
        #if gene is not primary, it needs to be dealt with (primary genes don't need anything done to them)
        
        if(!(genes[j] %in% synSplit2)){
          #if gene is not a synonym, it needs to be removed
          genes[j] <- "remove"
        }else{
          #does the synonym map to more than one gene?
          if(genes[j] %in% names(syn_wManyPri)){
            #if there's more than 3 primary names remove gene
            if(genes[j] %in% names(syn_wManyPri[which(syn_wManyPri > 3)])){
              genes[j] <- "remove"
            }else{
              #grab all primary names (2 or 3 of them)
              k <- 1
              foundCount <- 0
              
              curGene <- genes[j]
              while(foundCount < as.numeric(syn_wManyPri[which(names(syn_wManyPri) == curGene)])){
                if(curGene %in% synSplit[[k]]){
                  foundCount <- foundCount + 1
                  #for the first primary gene, replace the current synonym, for the others, append
                  if(foundCount == 1){
                    genes[j] <- discGenes$PrimaryGeneNames[k]
                  }else{
                    genes <- c(genes, discGenes$PrimaryGeneNames[k])
                  }
                }
                k <- k + 1
              }
            }
          }else{
            #grab the primary name (there's only 1)
            notFound <- TRUE
            k <- 1
            while(notFound){
              if(genes[j] %in% synSplit[[k]]){
                #replace synonym with primary
                genes[j] <- discGenes$PrimaryGeneNames[k]
                notFound <- FALSE
              }else{
                #search next col
                k <- k + 1
              }
            }
          }
        }
      }
    }
    
    genes <- genes[-which(genes == "remove")]
    all_genes <- c(all_genes, genes) #master vector of discoverable genes
    GWAS_cat$x[i] <- paste(genes, collapse=",")
  }
  rm(genes)
  
  pre_all_genes <- as.factor(unique(pre_all_genes))   #all genes in catalogue (including non-discoverable ones removed from network)
  all_genes <- as.factor(unique(all_genes))   #all discoverable genes in network
  perc_disc <- length(all_genes) / length(pre_all_genes) * 100 #an estimate (a few genes map to more than one synonym so discoverable genes may be slightly increased)
  noGenes <- which(GWAS_cat$x == "")
  GWAS_cat <- slice(GWAS_cat, -noGenes)
  
  # Turn dataframe of strings into list of factors
  GWAS_cat <- unite(GWAS_cat, node_name, c(PUBMEDID, STUDY, 'DISEASE/TRAIT'), sep="_", remove=TRUE)
  nn <- GWAS_cat$node_name
  GWAS_cat <- select(GWAS_cat, x)
  GWAS_cat <- str_split(GWAS_cat$x, ",")
  GWAS_cat <- lapply(GWAS_cat, as.factor)
  names(GWAS_cat) <- nn
  
  # Replace genes with binary vector representing presence/absence of all genes
  GWAS_cat2 <- GWAS_cat
  explainError <- tryCatch(
    {
      #try if GWAS_cat list has elements (ie there was overlap between discoverable genes and the catalogue)
      GWAS_cat2[[1]]
    },
    error=function(cond){
      print("Error (displayed below) occured because GWAS catalogue does not contain discoverable genes.")
      print("Module will be terminated.")
    }
  )
  for(i in 1:length(GWAS_cat2)){
    GWAS_cat2[[i]] <- as.numeric(all_genes %in% GWAS_cat2[[i]])
  }
  
  # Create adjacency matrix (one for stats (centered), one for expected stat, and one for p value)
  adj_matrix_stat <- adj_matrix_expec <- adj_matrix_pval <- matrix(ncol=length(GWAS_cat2), nrow=length(GWAS_cat2))
  rownames(adj_matrix_stat) <- rownames(adj_matrix_expec) <- rownames(adj_matrix_pval) <- nn
  colnames(adj_matrix_stat) <- colnames(adj_matrix_expec) <- colnames(adj_matrix_pval) <- nn
  
  pb <- txtProgressBar(min = 0, max = length(GWAS_cat2), style = 3)
  for(i in 1:length(GWAS_cat2)){
    for(j in i:length(GWAS_cat2)){
      if(i==j){
        # nodes have perfect overlap with themselves, we don't want these links
        adj_matrix_stat[i,j] <- 0
        adj_matrix_expec[i,j] <- 0
        adj_matrix_pval[i,j] <- 1
      }else{
        vals <- jaccard.test.mca(GWAS_cat2[[i]], GWAS_cat2[[j]])
        adj_matrix_stat[i,j] <- adj_matrix_stat[j,i] <- vals$statistics
        adj_matrix_expec[i,j] <- adj_matrix_expec[j,i] <- vals$expectation
        adj_matrix_pval[i,j] <- adj_matrix_pval[j,i] <- vals$pvalue
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Let user know where the adjacency matrix will be saved
  setwd(savePath)
  today <- Sys.Date()
  today <- str_replace_all(today, "-", "_")
  newCatFile <- paste("GWAScat_networkAdjMat_discGenes_", today, ".RData", sep="")
  cat("The updated GWAS catalogue network will be saved as ", newCatFile, " in directory:\n", sep="")
  cat(getwd(), "\n\n")
  cat("Press enter to continue...\n")
  readline()
  
  save(all_genes, GWAS_cat, adj_matrix_stat, adj_matrix_expec, adj_matrix_pval, perc_disc, file=newCatFile)
  results = list(all_genes=all_genes, GWAS_cat=GWAS_cat, adj_matrix_stat=adj_matrix_stat, adj_matrix_expec=adj_matrix_expec, adj_matrix_pval=adj_matrix_pval, perc_disc=perc_disc)
  
  cat("Network updated and saved.\n")
  
  return(results)
}
updateNetwork_allGenes <- function(){
  # This function takes a downloaded GWAS catalogue and create an adjacency matrix to represent a network where nodes are a study and trait combo, and the attributes of the nodes are the genes found to associate with the trait according to that study. If and only if there is significant overlap (according to a p value of a Jaccard coefficient) between the genes of two nodes, than there is a link between those nodes.
  # Note that gene names are not set to primary gene names as all genes are being used, not the discoverable ones.
  #
  # Parameters
  # none (note that savePath will be used from the main environment)
  #
  # Returns
  # results = list containing:
  #     all_genes = vector of factors representing all genes in the GWAS catalogue
  #     GWAS_cat = list where elements are named after study+trait names and contain vectors of factors representing genes
  #     adj_matrix_stat = matrix representing the GWAS catalogue where rows/col are nodes and elements are the centered Jaccard coefficients (which represents the overlap in genes between two nodes); elements on the diagonal are set to 0 as we are not interested in a node's overlap with itself
  #     adj_matrix_expec = matrix representing the GWAS catalogue where rows/col are nodes and elements are the expected Jaccard coefficients (which are used to center the original Jaccard coefficient); elements on the diagonal are set to 0 as we are not interested in a node's overlap with itself
  #     adj_matrix_pval = matrix representing the GWAS catalogue where rows/col are nodes and elements are the p values of the Jaccard coefficients (which represents how significant overlap in genes between two nodes is); elements on the diagonal are set to 1 as we are not interested in a node's overlap with itself
  #
  # Saves:
  # all_genes, GWAS_cat, adj_matrix_stat, adj_matrix_expec, and adj_matrix_pval (as separate objects, not part of a list) into an RData file
  
  #Determine file path to catalogue
  cat("Please ensure you have downloaded the GWAS catalogue. \n")
  proceed <- "n"
  while(proceed == "n"){
    cat("Please enter a file path to the downloaded .tsv file:\n")
    catFile <- readline()
    cat("You have entered the following file path as your GWAS catalogue:\n")
    cat(catFile, "\n")
    cat("Confirm (y or n)?\n")
    proceed <- readline()
    proceed <- evalProceed(proceed)
    while(proceed == "i"){
      cat("Invalid input.\nConfirm (y or n)? ")
      proceed <- readline()
      proceed <- evalProceed(proceed)
    }
    if(proceed == "y"){
      validPath <- file.exists(catFile)
      while(!validPath){
        cat("Invalid path.\n")
        proceed <- "n"
      }
    }
  }

  # Load the GWAS catalogue
  GWAS_cat <- read_delim(catFile, "\t", escape_double = FALSE, trim_ws = TRUE)
  # Get rid of irrelevant columns and rows with missing genes
  GWAS_cat <- select(GWAS_cat, "PUBMEDID", "STUDY", "DISEASE/TRAIT", "MAPPED_GENE")
  GWAS_cat <- GWAS_cat %>% filter(!is.na(MAPPED_GENE))

  # Concatenate genes for each unique pubmed id + disease/trait row (new column is "x")
  GWAS_cat <- (aggregate(GWAS_cat$MAPPED_GENE, GWAS_cat[c('PUBMEDID', "STUDY", 'DISEASE/TRAIT')], paste, collapse=','))
  GWAS_cat <- mutate(GWAS_cat, x= str_replace_all(x, " - ", ","))   #note that "-" (no spaces) is valid
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, " ", ","))
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, ",,,", ","))
  GWAS_cat <- mutate(GWAS_cat, x=str_replace_all(x, ",,", ","))
  
  # Get rid of genes that show up more than once in a row and accumulate all genes into a master vector
  all_genes <- c()
  for(i in 1:nrow(GWAS_cat)){
    genes <- unique(str_split(GWAS_cat$x[i], ",")[[1]])
    all_genes <- c(all_genes, genes)
    GWAS_cat$x[i] <- paste(genes, collapse=",")
  }
  rm(genes)
  all_genes <- as.factor(unique(all_genes))
  
  # Turn dataframe of strings into list of factors
  GWAS_cat <- unite(GWAS_cat, node_name, c(PUBMEDID, STUDY, 'DISEASE/TRAIT'), sep="_", remove=TRUE)
  nn <- GWAS_cat$node_name
  GWAS_cat <- select(GWAS_cat, x)
  GWAS_cat <- str_split(GWAS_cat$x, ",")
  GWAS_cat <- lapply(GWAS_cat, as.factor)
  names(GWAS_cat) <- nn
  
  # Replace genes with binary vector representing presence/absence of all genes
  GWAS_cat2 <- GWAS_cat
  for(i in 1:length(GWAS_cat2)){
    GWAS_cat2[[i]] <- as.numeric(all_genes %in% GWAS_cat2[[i]])
  }
  
  # Create adjacency matrix (one for stats (centered), one for expected stat, and one for p value)
  adj_matrix_stat <- adj_matrix_expec <- adj_matrix_pval <- matrix(ncol=length(GWAS_cat2), nrow=length(GWAS_cat2))
  rownames(adj_matrix_stat) <- rownames(adj_matrix_expec) <- rownames(adj_matrix_pval) <- nn
  colnames(adj_matrix_stat) <- colnames(adj_matrix_expec) <- colnames(adj_matrix_pval) <- nn
  
  pb <- txtProgressBar(min = 0, max = length(GWAS_cat2), style = 3)
  for(i in 1:length(GWAS_cat2)){
    for(j in i:length(GWAS_cat2)){
      if(i==j){
        # nodes have perfect overlap with themselves, we don't want these links
        adj_matrix_stat[i,j] <- 0
        adj_matrix_expec[i,j] <- 0
        adj_matrix_pval[i,j] <- 1
      }else{
        vals <- jaccard.test.mca(GWAS_cat2[[i]], GWAS_cat2[[j]])
        adj_matrix_stat[i,j] <- adj_matrix_stat[j,i] <- vals$statistics
        adj_matrix_expec[i,j] <- adj_matrix_expec[j,i] <- vals$expectation
        adj_matrix_pval[i,j] <- adj_matrix_pval[j,i] <- vals$pvalue
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Let user know where the adjacency matrix will be saved
  setwd(savePath)
  today <- Sys.Date()
  today <- str_replace_all(today, "-", "_")
  newCatFile <- paste("GWAScat_networkAdjMat_allGenes_", today, ".RData", sep="")
  cat("The updated GWAS catalogue network will be saved as ", newCatFile, " in directory:\n", sep="")
  cat(getwd(), "\n\n")
  cat("Press enter to continue...\n")
  readline()
  
  save(all_genes, GWAS_cat, adj_matrix_stat, adj_matrix_expec, adj_matrix_pval, file=newCatFile)
  results = list(all_genes=all_genes, GWAS_cat=GWAS_cat, adj_matrix_stat=adj_matrix_stat, adj_matrix_expec=adj_matrix_expec, adj_matrix_pval=adj_matrix_pval)
  
  cat("Network updated and saved.\n")
  
  return(results)
} #not converted to primary names as many genes may not be in discoverable gene set anyway
pAdjust_network <- function(adj_matrix, correction="fdr"){
  # This function applies a p adjustment to the network of p values. Note, because the network undirected (its adjacency matrix is symmetric) and the diagonal is irrelevant (nodes have perfect overlap with themselves) the p value is adjusted assuming the number of tests is equal to the number of elements in only the upper triangle of the adjacency matrix
  # 
  # Parameters
  # adj_matrix = an adjacency matrix of raw jaccard coefficient p values (lower/higher p values representing presence/absence of links respectively)
  #
  # Returns
  # adj_matrix = the same adjacency matrix but with adjusted p values (default is fdr)
  
  adj_matrix[upper.tri(adj_matrix)] <- p.adjust(adj_matrix[upper.tri(adj_matrix)], method=correction)
  adj_matrix[lower.tri(adj_matrix)] <- t(adj_matrix)[lower.tri(t(adj_matrix))]
  
  return(adj_matrix)
}
genesOfNodes <- function(GWAS_cat, nodes){
  # This function takes the supplied set of nodes and finds the genes present in that set.
  #
  # Parameters
  # GWAS_cat = list where elements are named after nodes and contain a vector of factors representing genes belonging to that node.
  # nodes = character vector of node names
  #
  # Returns
  # rel_genes = character vector of genes belonging to the set of nodes (genes only appear once)
  
  listInd <- which(names(GWAS_cat) %in% nodes)
  rel_genes <- c()
  for(i in listInd){
    rel_genes <- c(rel_genes, as.character(GWAS_cat[[i]]))
  }
  rel_genes <- unique(rel_genes)
  
  return(rel_genes)
}
shinyExplore <- function(nn_df){
  # This function takes a dataframe and opens an interactive shiny window for the user to search the dataframe. When the user clicks the quit button, the app is stopped.
  #
  # Parameters:
  # nn_df = dataframe
  #
  # Results:
  # NULL
  
  app=shinyApp(
    ui <- fluidPage(
      navbarPage(title="Search GWAS Catalogue", id="navbar",
                 tabPanel(title="Data", DT::dataTableOutput("mytable")),
                 tabPanel(title = "Quit", value="stop", icon=icon("cicle-o-notch")))
    ),
    server <- function(input, output){
      output$mytable = DT::renderDataTable(
        nn_df,
        filter="top",
        options=list(searchHighlight = TRUE, searchDelay = 1000)
      )
      observe({
        if (input$navbar == "stop") 
          stopApp()
      })
    }
  )
  runApp(app)
}
exploratorySearch <- function(nodes){
  # This function allows the user to explore what nodes would be returned with certain search words.
  #
  # Parameters
  # nodes = vector of node names (strings with the format PMID_TITLE_TRAIT)
  #
  # Returns
  # NULL
  
  # Split the node names back into their constituent parts (pubmed id, study title, disease/trait annotation)
  nn_df <- str_split(nodes, pattern="_")
  pmid <- c()
  title <- c()
  trait <- c()
  for(i in 1:length(nn_df)){
    pmid <- c(pmid, nn_df[[i]][1])
    title <- c(title, nn_df[[i]][2])
    trait <- c(trait, nn_df[[i]][length(nn_df[[i]])])
  }
  nn_df <- data.frame(pmid=pmid, title=title, trait=trait, stringsAsFactors = FALSE)
  
  # Let the user interactively search the table
  cat("Another R window (called Shiny) will pop up. Use the search bar at the top right to try out search terms (note that capitalization is ignored). You can also use the search bars at the top of the columns to search a specific column. The number of results is mentioned at the bottom of the table. When you are finished, click the Quit button NOT the x button that closes the window.\n")
  cat("Please press enter to continue...\n")
  readline()
  
  shinyExplore(nn_df)
  # ui <- basicPage(
  #   h2("Search GWAS Catalogue"),
  #   DT::dataTableOutput("mytable")
  # )
  # server <- function(input, output){
  #   output$mytable = DT::renderDataTable(
  #     nn_df,
  #     filter="top",
  #     options=list(searchHighlight = TRUE, searchDelay = 1000)
  #   )
  # }
  # 
  # shinyApp(ui, server)
  
}
nameCluster <- function(nodes){
  # This function takes node names of clusters and finds patterns in the node names.
  #
  # Parameters
  # nodeNames = vector of strings
  #
  # Returns
  # hits = data frame with columns "word" and "freq" denoting the words found and their frequencies (rows are ordered from most frequent to least)
  
  # Break node names into individual words
  nodeWords <- str_split(nodes, "_")
  nodeWords <- lapply(nodeWords, "[", -1)        #get rid of PUBMED ID
  nodeWords <- unlist(nodeWords)    
  nodeWords <- unlist(str_split(nodeWords, " "))  #accumulate all nodes
  
  # Format nodeWords to allow wordcloud functions to work
  docs <- Corpus(VectorSource(nodeWords))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "\\(")
  docs <- tm_map(docs, toSpace, "\\)")
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))        #convert to lower case
  # Remove common words
  scienceWords <- c("human", "humans", "mouse", "mice",
                     "disease", "diseases", "pathology", "pathologies",
                     "study", "studies", "investigate", "investigation", "investigations",
                     "identify", "identifies", "association", "associations", "genome-wide", 
                     "genome", "genomes", "genomics", "genetic",
                     "locus", "loci", "variant", "variants", "SNP", "SNPs", 
                     "polymorphism", "polymorphisms", "nucleotide", "nucleotides",
                    "new", "novel", "discover", "discovers", "discovery", "discoveries", "confirm", "confirms",
                    "\\.", "-", "&")
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, scienceWords)
  
  # Find frequent words
  dtm <- TermDocumentMatrix(docs, control=list(wordLengths=c(1, Inf), bounds=list(global = c(2, Inf))))
  v <- sort(rowSums(as.matrix(dtm)),decreasing=TRUE)
  hits <- data.frame(word = names(v),freq=v, stringsAsFactors = FALSE)
  
  return(hits)
}
c_table <- function(c_list, max_n){
  # This function takes a list where elements represent cluster and creates a table, where each cluster is represented by a column. The strings stored in the columns are the same strings that the original list stores in the elements. Rows do not represent specific strings. Since all columns need to be the same length, when there are too few strings in a column, the remaining rows are filled with NA.
  #
  # Parameters
  # c_list <- list where each element is a cluster and contains a vector of strings (eg node names or gene names)
  # max_n <- size of the largest cluster (ie how many strings are in the largest element of the list)
  #
  # Returns
  # tab <- data frame where columns represent clusters (numbered) and columns are filled with strings 
  
  tab <- data.frame(matrix(nrow=max_n, ncol=length(c_list)))
  for(i in 1:length(c_list)){
    tab[,i] <- c(c_list[[i]], rep(NA, (max_n-length(c_list[[i]]))))
  }
  
  return(tab)
}
graphSubnetwork <- function(adj_matrix, rel_nodeNames, searchTerm, n_all_genes){
  # This function creates a subnetwork object, clusters it, graphs the clustered network, and names the clusters based on common patterns in the node names.
  #
  # Parameters:
  # adj_matrix = matrix representing full GWAS catalogue (weights are assumed to be negative log of adjusted p values)
  # rel_nodeNames = data frame, consisting of column "nodes" which contains names (as strings) of nodes that should be included in the subnetwork, and a second column "hitType" indicating if the corresponding node is a primary hit (ie was found through the original search; denoted as 1) or was a neighbor (denoted as 2)
  # (and note that savePath will be used from the main environment)
  # 
  # Returns:
  # results = list containing:
  #     relNet = network object of the subnetwork
  #     c_relNet = clusterings of relNet
  #     c_summary = data frame summarizing the clusters found with columns representing cluster number, cluster name, total number of nodes in cluster, total number of primary hits in cluster, percentage of nodes in the cluster that are primary hits, percentage of primary hits in the cluster
  #     c_patterns = list where elements are either data frames containing frequencies of words within node names of that cluster, or elements are strings representing the single node in a cluster
  #     c_nodes = data frame where columns represent clusters and which nodes are in the cluster (rows do not represent specific nodes, and are filled with NA to keep dimensions of columns consistent)
  #     subnetwork_graph = graphical representation of relNet

  # Create subnetwork
  #rnames <- rownames(adj_matrix)  #DEBUG
  #nNames <- rel_nodeNames$nodes #DEBUG
  nodeInd <- which(rownames(adj_matrix) %in% rel_nodeNames$nodes)
  rel_adj_matrix <- adj_matrix[nodeInd, nodeInd]
  #save(rnames, nNames, nodeInd, rel_adj_matrix, file="temp_relAdjMat.RData")  #DEBUG
  relNet <- graph_from_adjacency_matrix(rel_adj_matrix, mode="undirected", weight=TRUE)

  # Find clusters of subnetwork
  cat("Clustering...\n")
  c_relNet <- cluster_fast_greedy(relNet, weights = E(relNet)$weight)
  num_c <- length(unique(c_relNet$membership))

  # Begin summary of clusters
  c_summary <- table(c_relNet$membership)     #sorts clusters from 1 to n (not by frequency)
  c_summary <- data.frame(c_summary)
  colnames(c_summary) <- c("cluster", "total_nodes")
  c_summary$cluster <- as.numeric(c_summary$cluster)
  c_summary$name <- rep(NA, nrow(c_summary))
  c_summary <- c_summary[,c("cluster", "name", "total_nodes")]
  c_summary$primary_hits <- rep(0, nrow(c_summary))
  c_nodes <- list()

  # Name clusters based on common traits within cluster
  c_patterns <- list()
  c_relNet$c_names <- character(length(c_relNet$membership))    #add a cluster name element to the communities object
  max_n_nodes <- 0
  for(i in 1:num_c){  
    # For nodes belonging to a certain cluster grab the node names
    cInd <- which(c_relNet$membership == i)
    if(length(cInd) > max_n_nodes){
      max_n_nodes <- length(cInd)
    } #find largest cluster
    
    # Search for patterns in the node names
    c_nodes[[i]] <- c_relNet$names[cInd]
    clusterName <- nameCluster(c_nodes[[i]])
    c_patterns[[i]] <- clusterName
    
    # Use the top 5 patterns to generate a name for the cluster
    if(nrow(clusterName) < 5){
      c_relNet$c_names[cInd] <- paste(c(i, clusterName$word), collapse="_")
      c_summary$name[i] <- paste(c(i, clusterName$word), collapse="_")        #update summary
    }else{
      c_relNet$c_names[cInd] <- paste(c(i, clusterName$word[1:5]), collapse="_")
      c_summary$name[i] <- paste(c(i, clusterName$word[1:5]), collapse="_")   #update summary
    }
  }
  
  # Set colours of nodes
  cat("Graphing...\n")
  c_relNet$palette <- c_relNet$membership       #add a palette element to the communities object
  c_relNet$frame_palette <- c_relNet$membership
  total_primary <- 0
  for(i in 1:length(c_relNet$palette)){
    #if the node is a primary hit, give it a special colour, not the cluster colour
    if(rel_nodeNames$hitType[which(rel_nodeNames$nodes == c_relNet$names[i])] == 1){
      c_relNet$palette[i] <- num_c + 1
      memb <- c_relNet$membership[i]
      c_summary$primary_hits[memb] <- c_summary$primary_hits[memb] + 1    #update summary
      total_primary <- total_primary + 1
    }else{
      c_relNet$frame_palette[i] <- num_c + 1
    }
  }
  
  my_palette <- rainbow(num_c)
  {
    
    if(length(my_palette) >5){
      my_palette2 <- c()      #was outside of if
      my_palette2 <- c(my_palette2, my_palette[seq(1, length(my_palette), 5)])
      my_palette2 <- c(my_palette2, my_palette[seq(2, length(my_palette), 5)])
      my_palette2 <- c(my_palette2, my_palette[seq(3, length(my_palette), 5)])
      my_palette2 <- c(my_palette2, my_palette[seq(4, length(my_palette), 5)])
      my_palette2 <- c(my_palette2, my_palette[seq(5, length(my_palette), 5)])
      my_palette <- my_palette2             #was outside of if
      rm(my_palette2)             #was outside of if
    }
    
  }   #mix order of palette if palette isn't too small
  my_palette <- c(my_palette, 'black')
  
  # Set legend
  legend_df <- select(c_summary, c("name", "total_nodes"))
  legend_df <- data.frame(legend_df, node_colour=my_palette[1:(length(my_palette)-1)], stringsAsFactors=FALSE)
  legend_df <- slice(legend_df, which(legend_df$total_nodes > 1))
  legend_df <- rbind(legend_df, c(paste((num_c+1), "primary hits", sep=" "), total_primary, my_palette[length(my_palette)]))
  colnames(legend_df) <- c("name", "total_nodes", "node_colour")
  
  # Set layouts
  layout(matrix(c(1,1,2), nrow=1, byrow=TRUE))    #layout network over 2/3rds and legend on right most third
  weights<-ifelse(crossing(c_relNet, relNet), 1, 100)
  lay <- layout_nicely(relNet, weights=weights/100)   #layout of nodes within network
  
  # Graph (in pdf/png and in RStudio)
  filePrefix <- paste(searchTerm, "from", n_all_genes, sep="_")
  setwd(savePath)
  
  cat("Do you wish to save the graph as a PNG (y or n)?\n")
  saveChoice <- readline()
  saveChoice <- evalProceed(saveChoice)
  while(saveChoice == "i"){
    cat("Invalid input. Do you wish to save the graph as a PNG (y or n)?\n")
    saveChoice <- readline()
    saveChoice <- evalProceed(saveChoice)
  }
  if(saveChoice == "y"){
    png(paste("mod3_", filePrefix, "_subnetwork.png"), width=10, height=7, units="in", res=300)
    
    par(mfrow=c(1,2))
    # plot network
    plot(relNet, vertex.label=NA, vertex.color=alpha(my_palette[c_relNet$palette], 0.75), vertex.size=5, vertex.frame.color=my_palette[c_relNet$frame_palette] , edge.color="grey80", layout=lay)
    # plot legend
    plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)  #blank plot (can't plot legend alone)
    legend("center", title="Clusters (with more than 1 node)", legend=legend_df$name, fill=alpha(legend_df$node_colour, 0.5), cex=0.75)
    
    dev.off()
    par(mfrow=c(1,1))
  }
  
  cat("Do you wish to save the graph as a PDF (y or n)?\n")
  saveChoice <- readline()
  saveChoice <- evalProceed(saveChoice)
  while(saveChoice == "i"){
    cat("Invalid input. Do you wish to save the graph as a PDF (y or n)?\n")
    saveChoice <- readline()
    saveChoice <- evalProceed(saveChoice)
  }
  if(saveChoice == "y"){
    pdf(paste("mod3_", filePrefix, "_subnetwork.pdf"), width=10, height=7)
    
    # plot network
    plot(relNet, vertex.label=NA, vertex.color=alpha(my_palette[c_relNet$palette], 0.75), vertex.size=5, vertex.frame.color=my_palette[c_relNet$frame_palette] , edge.color="grey80", layout=lay)
    # plot legend
    plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)  #blank plot (can't plot legend alone)
    legend("center", title="Clusters (with more than 1 node)", legend=legend_df$name, fill=alpha(legend_df$node_colour, 0.5), cex=0.75)
    
    dev.off()
  }
  
  par(mfrow=c(1,2))
  # plot network
  plot(relNet, vertex.label=NA, vertex.color=alpha(my_palette[c_relNet$palette], 0.75), vertex.size=5, vertex.frame.color=my_palette[c_relNet$frame_palette] , edge.color="grey80", layout=lay)
  # plot legend
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)  #blank plot (can't plot legend alone)
  legend("center", title="Clusters (with more than 1 node)", legend=legend_df$name, fill=alpha(legend_df$node_colour, 0.5), cex=0.75)
  subnetwork_graph <- recordPlot()
  
  # Finish summary of clusters
  c_summary$perc_of_c <- c_summary$primary_hits/c_summary$total_nodes * 100
  c_summary$perc_of_primary <- c_summary$primary_hits/total_primary * 100
  c_nodes <- c_table(c_nodes, max_n_nodes)
  colnames(c_nodes) <- legend_df$name[1:(length(legend_df$name)-1)]
  
  # Reset layout
  par(mfrow=c(1,1))
 
  results <- list(relNet=relNet, c_relNet=c_relNet, c_summary=c_summary, c_patterns=c_patterns, c_nodes=c_nodes, subnetwork_graph=subnetwork_graph)
}
gwasGenes <- function(){
  # This function (if the module is run) will take data from the GWAS catalogue, create a representative network, look at nodes related to the search term as well as 1st order neighbors of those nodes, and return all the relevant genes.
  #
  # Parameters:
  # none (but note that if variable savePath is called, it accesses object savePath in the main environment)
  #
  # Returns:
  # results = a list, 
  #   always containing:
  #   n_all_genes = the total number of genes in the full GWAS catalogue
  #   searchTerm = a string representing what search term was used to create the subnetwork
  #   rel_nodeNames = a data frame with column "node" containing node names and column "hitType" containing 1s and 2s representing respectively if the node was found via the search term or if it is a neighbor to such a node 
  #   rel_genes = a vector of genes accumulated from all nodes in rel_nodeNames (genes only appear once)
  #   and containg the following elements only when clustering/graphing is run:
  #   relNet = a network object representing the subnetwork of nodes in rel_nodeNames
  #   c_relNet = a communities object resulting from clustering relNet
  #   c_summary = a summary of the clusters of relNet
  #   c_patterns = a list where each element is a data frame representing a cluster and the frequency of words found within its nodes 
  #   c_nodes = data frame where columns represent clusters and which nodes are in the cluster (rows do not represent specific nodes, and are filled with NA to keep dimensions of columns consistent)
  #   c_genes = data frame where columns represent clusters and which genes are in the cluster (rows do not represent specific genes, and are filled with NA to keep dimensions of columns consistent)
  #   subnetwork_graph = a graph of the subnetwork generated by the search term
  
  cat("Do you wish to run the \"GWAS Genes\" module (y or n)? ")
  proceed <- readline()
  proceed <- evalProceed(proceed)
  while(proceed == "i"){
    cat("Invalid input.\nProceed (y or n)? ")
    proceed <- readline()
    proceed <- evalProceed(proceed)
  }
  
  if(proceed == "y"){
    cat("Running GWAS Genes module...\n ")
    
    # Determine if user wants to update GWAS catalogue network or load an existing one
    cat("\nDo you wish to update the full GWAS Catalog network (y or n)?\n")
    proceed <- readline()
    proceed <- evalProceed(proceed)
    while(proceed == "i"){
      cat("Invalid input.\nUpdate full GWAS Catalog network (y or n)? ")
      proceed <- readline()
      proceed <- evalProceed(proceed)
    }
    if(proceed =="y"){
      #warn that it will take a long time
      cat("Note that updating the GWAS Catalog network will take a long time (over 3 hours for the discoverable network and over 30 hours when not limiting to discoverable genes). Are you sure you want to update the network (y or n)?\n")
      proceed <- readline()
      proceed <- evalProceed(proceed)
      while(proceed == "i"){
        cat("Invalid input.\nUpdate GWAS Catalog network (y or n)? ")
        proceed <- readline()
        proceed <- evalProceed(proceed)
      }
      if(proceed == "y"){
        
        cat("Do you wish to create a network with all genes or  with only discoverable genes (see user manual for more information)?\n")
        cat("1 - all genes\n")
        cat("2 - discoverable genes\n")
        formatOp <- readline()
        while( formatOp != "1" & formatOp != "2"){
          cat("Invalid input. Enter a number: ")
          formatOp <- readline()
        }
        
        if(formatOp == 1){
          # Update the network, do not use discoverable genes
          res_upNet <- updateNetwork_allGenes()
          all_genes <- res_upNet$all_genes
          GWAS_cat <- res_upNet$GWAS_cat
          adj_matrix_pval <- res_upNet$adj_matrix_pval
          adj_matrix_stat <- res_upNet$adj_matrix_stat
        }else{
          # Determine if user wants to update discoverable genes
          cat("\nDo you wish to update the set of discoverable genes (y or n)?\n")
          proceed2 <- readline()
          proceed2 <- evalProceed(proceed2)
          while(proceed2 == "i"){
            cat("Invalid input.\nUpdate set of discoverable genes (y or n)? ")
            proceed2 <- readline()
            proceed2 <- evalProceed(proceed2)
          }
          if(proceed2 =="y"){
            cat("Are you sure you want to update (y or n)?\n")
            proceed2 <- readline()
            proceed2 <- evalProceed(proceed2)
            while(proceed2 == "i"){
              cat("Invalid input.\nUpdate set of discoverable genes (y or n)? ")
              proceed2 <- readline()
              proceed2 <- evalProceed(proceed2)
            }
            if(proceed2 == "y"){
              # Update the discoverable gene set
              res_upDiscGenes <- discoverableGenes()
              all_DiscGenes <- res_upDiscGenes
            }
          }
          if(proceed2 == "n"){ #not an else statment because user could have changed their mind
            # Load the discoverable gene set
            cat("An existing discoverable gene set will be loaded (for more details, see the user manual).\n")
            proceed3 <- "n"
            while(proceed3 == "n"){
              cat("Please enter a file path to an .RData file with the gene set:\n")
              setFile <- readline()
              cat("You have entered the following file path as your discoverable gene set:\n")
              cat(setFile, "\n")
              cat("Confirm (y or n)?\n")
              proceed3 <- readline()
              proceed3 <- evalProceed(proceed3)
              while(proceed3 == "i"){
                cat("Invalid input.\nConfirm (y or n)? ")
                proceed3 <- readline()
                proceed3 <- evalProceed(proceed3)
              }
              if(proceed3 == "y"){
                validPath <- file.exists(setFile)
                while(!validPath){
                  cat("Invalid path.\n")
                  validPath <- TRUE
                  proceed3 <- "n"
                }
              }
            }
            cat("Loading...\n")
            load(setFile)     #will load dataframe discGenes
            all_DiscGenes <- discGenes #NEW
          }
          
          #all_DiscGenes object exists, use it to update catalogue network
          # Update the network and use discoverable genes
          
          res_upNet <- updateNetwork_discGenes(all_DiscGenes) #need to get this function going
          all_genes <- res_upNet$all_genes
          GWAS_cat <- res_upNet$GWAS_cat
          adj_matrix_pval <- res_upNet$adj_matrix_pval
          adj_matrix_stat <- res_upNet$adj_matrix_stat
        }
      }
    }
    if(proceed == "n"){ #not an else statement because user was given the option to change their mind
      # Load the existing adjacency matrix
      cat("An existing full GWAS Catalog network will be loaded (for more details, see the user manual).\n")
      proceed <- "n"
      while(proceed == "n"){
        cat("Please enter a file path to an .RData file with a network:\n")
        netFile <- readline()
        cat("You have entered the following file path as your GWAS Catalog network:\n")
        cat(netFile, "\n")
        cat("Confirm (y or n)?\n")
        proceed <- readline()
        proceed <- evalProceed(proceed)
        while(proceed == "i"){
          cat("Invalid input.\nConfirm (y or n)? ")
          proceed <- readline()
          proceed <- evalProceed(proceed)
        }
        if(proceed == "y"){
          validPath <- file.exists(netFile)
          while(!validPath){
            cat("Invalid path.\n")
            validPath <- TRUE
            proceed <- "n"
          }
        }
      }
      cat("Loading...\n")
      load(netFile)     #will load objects all_genes, GWAS_cat, adj_matrix_pval, adj_matrix_stat, and adj_matrix_expec (though this last object is unused)
    }
    
    # Ensure very significant p values are not exactly 0
    minP <- min(adj_matrix_pval[which(adj_matrix_pval != 0)])
    magOrder <- floor(log(minP, base=10))     #order of magnitude of the smallest p value
    newMin <- 0.1*10^magOrder
    adj_matrix_pval[which(adj_matrix_pval == 0)] <- newMin
    
    # Correct the p values
    adj_matrix_pval <- pAdjust_network(adj_matrix_pval)
    
    # Set links to exist only if Jaccard coefficient is significant
    disp <- "Links are present between nodes of the network if there is significant overlap in genes (see user manual for more detail).\nWhat significance threshold do you want to filter the FDR adjusted p values by?\n "
    jp_thresh <- setThresh(disp)
    adj_matrix_pval[which(adj_matrix_pval > jp_thresh)] <- 1
    
    # Transform p values to weights (these are used in the network)
    adj_matrix_pval <- -log(adj_matrix_pval, base=10)
    
    disp <- "Small overlap (represented by small Jaccard coefficients) may be significant but irrelevant. What threshold do you want to filter the Jaccard coefficients (from 0 to 1) by?\n"
    jstat_thresh <- setThresh(disp)
    adj_matrix_stat[which(adj_matrix_stat >= jstat_thresh)] <- TRUE
    adj_matrix_stat[which(adj_matrix_stat < jstat_thresh)] <- FALSE
    adj_matrix <- adj_matrix_pval*adj_matrix_stat     #keep the transformed p values that have a large J coeff, set the rest to 0 (insignificant p values were already turned to 0)
    
    # Create network object representing full GWAS catalogue
    catNet <- graph_from_adjacency_matrix(adj_matrix, mode="undirected", weight=TRUE)
    
    # Select nodes with certain names and select their neighbors       #CURRENTLY ONLY TAKES ONE SEARCH TERM

        # explore search term results
    cat("Do you want to explore search terms before selecting a final one (y or n)?\n")
    proceed <- readline()
    while(proceed == "i"){
      cat("Invalid input. Explore search terms (y or n)?\n")
      proceed <- readline()
      proceed <- evalProceed(proceed)
    }
    if(proceed == "y"){
      exploratorySearch(rownames(adj_matrix))
    }
    
        # enter a final search term
    proceed <- "n"
    while(proceed == "n"){
      cat("Please enter your chosen search term (note that capitalization is ignored):\n")
      
      searchTerm <- readline()
      
      #find nodes with search term and ignore result if term is preceded or followed by a letter (eg purine if search is urine)
      rowInd <- str_detect(rownames(adj_matrix), regex(searchTerm, ignore_case=TRUE))
      ignore1 <- str_detect(rownames(adj_matrix), regex(paste("[:alpha:]", searchTerm, sep=""), ignore_case=TRUE))
      ignore2 <- str_detect(rownames(adj_matrix), regex(paste(searchTerm, "[:alpha:]", sep=""), ignore_case=TRUE))
      rowInd <- which(rowInd & !ignore1 & !ignore2)
      
    
      if(length(rowInd)==0){
        #search term was not found
        cat("The search term entered was not found. Please enter another search term.\n")
      }else{
        proceed <- "y"
      }
    }
    rel_nodeNames <- rownames(adj_matrix)[rowInd]
    
    # Also select neighbors of selected nodes
    searchedNeighbors <- catNet[[rel_nodeNames]]    #This part requires you to create a network of the full catalogue
    for(i in searchedNeighbors){
      rel_nodeNames <- c(rel_nodeNames, names(i))
    }
    rel_nodeNames <- unique(rel_nodeNames)
    rel_nodeNames <- data.frame(nodes=rel_nodeNames, hitType=c(rep(1, length(rowInd)), rep(2, (length(rel_nodeNames)-length(rowInd)))), stringsAsFactors = FALSE)
    
    # Determine genes of the selected nodes
    rel_genes <- genesOfNodes(GWAS_cat, rel_nodeNames$nodes)
    
    # Determine if user wants to graph the subnetwork related to the search term
    if(nrow(rel_nodeNames) == 1){
      cat("\nThe filtered subnetwork consists of only one node. Graphs/clusters will not be generated.\n")
      proceed <- "n"
    }else{
      cat("\nDo you wish to graph/find clusters of the search terms' subnetwork (y or n)?\n")
      proceed <- readline()
      proceed <- evalProceed(proceed)
      while(proceed == "i"){
        cat("Invalid input.\nGraph/cluster subnetwork (y or n)? ")
        proceed <- readline()
        proceed <- evalProceed(proceed)
      }
      if(proceed =="y"){
        
        # Graph and cluster subnetwork
        subnetwork_results <- graphSubnetwork(adj_matrix, rel_nodeNames, searchTerm, length(all_genes))
        
        # Summary of genes in each cluster
        c_genes <- list()
        max_n_genes <- 0
        for(i in 1:length(subnetwork_results$c_relNet)){
          c_genes[[i]] <- genesOfNodes(GWAS_cat, subnetwork_results$c_relNet[[i]])
          if(length(c_genes[[i]]) > max_n_genes){
            max_n_genes <- length(c_genes[[i]])
          }
        }
        c_genes <- c_table(c_genes, max_n_genes)
        colnames(c_genes) <- names(subnetwork_results$c_nodes)
        
        # Accumulate results
        results <- list(n_all_genes = length(all_genes), searchTerm = searchTerm, rel_nodeNames = rel_nodeNames, rel_genes = rel_genes, relNet = subnetwork_results$relNet, c_relNet = subnetwork_results$c_relNet, c_summary=subnetwork_results$c_summary, c_patterns=subnetwork_results$c_patterns, c_nodes=subnetwork_results$c_nodes, c_genes=c_genes, subnetwork_graph=subnetwork_results$subnetwork_graph)
      }
    }
    
    if(proceed == "n"){
      # If graphs were not generated accumulate results
      results <- list(n_all_genes = length(all_genes), searchTerm = searchTerm, rel_nodeNames = rel_nodeNames, rel_genes = rel_genes)
    }
    
    cat("GWAS Genes module complete!\n")
    
    # Save object if desired
    if(savePath != ""){
      cat("Do you wish to save the results of this module (y or n)?\n")
      saveChoice <- readline()
      saveChoice <- evalProceed(saveChoice)
      while(saveChoice == "i"){
        cat("Invalid input. Do you wish to save the results of this module (y or n)?\n")
        saveChoice <- readline()
        saveChoice <- evalProceed(saveChoice)
      }
      
      if(saveChoice == "y"){
        setwd(savePath)
        cat("If you prefer to save your data as excel files, it is recommended for this module that you also save the data in an RData file (ie option 3) should you need to re-graph the network.\nHow do you wish to save the results?\n")
        cat("1 - as R objects in an RData file\n")
        cat("2 - as excel files\n")
        cat("3 - both\n")
        cat("4 - neither\n")
        cat("Enter a number: ")
        saveChoice <- readline()
        while( saveChoice != "1" & saveChoice != "2" & saveChoice != "3" & saveChoice != "4"){
          cat("Invalid input. Enter a number: ")
          saveChoice <- readline()
        }
        
        if(saveChoice == "1" | saveChoice == "3"){
          # save RData file
          res_GWAS <- results
          save(res_GWAS, file="mod3_res_GWAS.RData")
          rm(res_GWAS)
        }
        if(saveChoice == "2" | saveChoice == "3"){
          # save excel files
          filePrefix <- paste(results$searchTerm, "from", results$n_all_genes, sep="_")
          write.csv(results$rel_nodeNames, file=paste("mod3_", filePrefix, "_netNodeNames.csv", sep=""), row.names=FALSE)
          write.csv(results$rel_genes, file=paste("mod3_", filePrefix, "_netGenes.csv", sep=""), row.names=FALSE)
          if(length(names(results)) > 4){
            write.csv(results$c_summary, file=paste("mod3_", filePrefix, "_clusterSummary.csv", sep=""), row.names=FALSE)
            write.xlsx(results$c_patterns, file=paste("mod3_", filePrefix, "_clusterNamePatterns.xlsx", sep=""))
            write.csv(results$c_nodes, file=paste("mod3_", filePrefix, "_clusterNodes.csv", sep=""), row.names=FALSE, na="")
            write.csv(results$c_genes, file=paste("mod3_", filePrefix, "_clusterGenes.csv", sep=""), row.names=FALSE, na="")
          }
        }
        
        cat("Results were saved in ", savePath, "\n", sep="")
      }
    }
    
    return(results)
  }
}

# Module 4
diseaseOverRep <- function(){
  # This function determines if the disease genes (from the res_GWAS object created by module 3) are over-represented in the metabolite genes (in the proteins dataframe created by module 2)
  #
  # Parameters
  # none (but note that if variable savePath is called, it accesses object savePath in the main environment)
  #
  # Returns
  # results = a list, always containing:
  #   overlap_genes = character vector of the genes that overlap between metabolite and disease gene sets
  #   overrep_stat = p value (upper tail) of a hypergeometric test for overlap between the metabolite and disease gene sets
  #   n_background = the number of genes in the discoverable gene set
  
  
  cat("Do you wish to run the \"Disease Over-Representation\" module (y or n)? ")
  proceed <- readline()
  proceed <- evalProceed(proceed)
  while(proceed == "i"){
    cat("Invalid input.\nProceed (y or n)? ")
    proceed <- readline()
    proceed <- evalProceed(proceed)
  }
  
  if(proceed == "y"){
    cat("Running Disease Over-Representation module...\n ")
    
    # Detmerine what type of object will be used
    cat("\nAre you running this module immediately after modules 2 (Interacting Proteins and Genes) and 3 (GWAS Genes)? In other words, was this workflow uninterrupted (y or n)?:\n")
    unint <- readline()
    unint <- evalProceed(unint)
    while(unint == "i"){
      cat("Is this workflow uninterupted? Please enter y or n: ")
      unint <- readline()
      unint <- evalProceed(unint)
    }
    if(unint == "y"){
      # Assume obj res_enzGenes and res_GWAS objects are present in environment
      met_genes <- unique(res_interProtGenes$proteins$`Primary Gene Name`)     #ensure that genes are always unique and don't have more than one code
      disease_genes <- unique(res_GWAS$rel_genes)
      
    }else{

      # Determine what format metabolite associated genes will be loaded in as.
      cat("This module uses a vector of gene names associated with metabolites of interest. This can either be the dataframe object produced by module 2 (Interacting Proteins and Genes) or an excel table (with headers). In both cases, the first column should contain the genes.\n\n")
      cat("Where is your set of metabolite genes coming from?\n")
      cat("1 - R obj (character vector)\n")
      cat("2 - excel file\n")
      formatOp <- readline()
      while( formatOp != "1" & formatOp != "2"){
        cat("Invalid input. Enter a number: ")
        formatOp <- readline()
      }
      if(formatOp == 1){
        
        # Ensure R object is loaded
        cat("Please ensure your R object is loaded in the environment. Ensure the dataframe is an object on its own, and doesn't need to be accessed with indexing or with $.\n")
        cat("If you need to load or alter the R object, please enter quit, load the R object/format it, and restart (source) this workflow.\n")
        cat("Quit = q\n")
        cat("Continue = c\n")
        cat("Quit or continue: ")
        loaded <- readline()
        while(!(loaded %in% c("c", "C", "q", "Q"))){
          cat("Invalid input. \nPlease enter q or c: ")
          loaded <- readline()
        }
        
        if(loaded %in% c("q", "Q")){
          stop("Workflow quit.\n")
        }
        
        # Get name of R object
        proceed <- "n"
        while(proceed == "n"){
          cat("What is the name of your R object containing the metabolite associated genes?\n")
          objName <- readline()
          cat("You entered ", objName, " as the name of your object. \n Confirm (y or n)?", sep="")
          proceed <- readline()
          proceed <- evalProceed(proceed)
          while(proceed == "i"){
            cat("Invalid input.\nConfirm (y or n)? ")
            proceed <- readline()
            proceed <- evalProceed(proceed)
          }
        }
        assign("met_genes", get(objName))
      }else{
        cat("Using excel file... \n")
        
        # Obtain table of metabolite associated genes
        cat("Before proceeding, please ensure the excel table has genes in its first column and headers (such that data starts in row 2). Press enter to continue. \n")
        readline()
        cat("Please enter a file path to your metabolite associated genes excel file:\n")
        dataPath <- readline()
        validPath <- file.exists(dataPath)
        while(!validPath){
          cat("Invalid file path.\n")
          cat("Please enter a file path to your excel file:\n")
          dataPath <- readline()
          validPath <- file.exists(dataPath)
        }
        met_genes <- (read.csv(dataPath, header=TRUE, stringsAsFactors=FALSE))[,1]
      }
      
      # Determine what format disease genes will be loaded in as.
      cat("This module also uses a vector of gene names associated with disease. This can either be the vector object produced by module 3 (GWAS Genes) or an excel table (with headers) with the first column containing genes.\n\n")
      cat("Where is your set of disease genes coming from?\n")
      cat("1 - R obj (character vector)\n")
      cat("2 - excel file\n")
      formatOp <- readline()
      while( formatOp != "1" & formatOp != "2"){
        cat("Invalid input. Enter a number: ")
        formatOp <- readline()
      }
      if(formatOp == 1){
        
        # Ensure R object is loaded
        cat("Please ensure your R object is loaded in the environment. Ensure the vector is an object on its own, and doesn't need to be accessed with indexing or with $.\n")
        cat("If you need to load or alter the R object, please enter quit, load the R object/format it, and restart (source) this workflow.\n")
        cat("Quit = q\n")
        cat("Continue = c\n")
        cat("Quit or continue: ")
        loaded <- readline()
        while(!(loaded %in% c("c", "C", "q", "Q"))){
          cat("Invalid input. \nPlease enter q or c: ")
          loaded <- readline()
        }
        
        if(loaded %in% c("q", "Q")){
          stop("Workflow quit.\n")
        }
        
        # Get name of R object
        proceed <- "n"
        while(proceed == "n"){
          cat("What is the name of your R object containing the disease genes?\n")
          objName <- readline()
          cat("You entered ", objName, " as the name of your object. \n Confirm (y or n)?", sep="")
          proceed <- readline()
          proceed <- evalProceed(proceed)
          while(proceed == "i"){
            cat("Invalid input.\nConfirm (y or n)? ")
            proceed <- readline()
            proceed <- evalProceed(proceed)
          }
        }
        assign("disease_genes", get(objName))
        
      }else{
        cat("Using excel file... \n")
        
        # Obtain disease associated genes
        cat("Before proceeding, please ensure the excel table has genes in its first column and headers (such that data starts in row 2). Press enter to continue. \n")
        readline()
        cat("Please enter a file path to your disease genes excel file:\n")
        dataPath <- readline()
        validPath <- file.exists(dataPath)
        while(!validPath){
          cat("Invalid file path.\n")
          cat("Please enter a file path to your disease genes excel file:\n")
          dataPath <- readline()
          validPath <- file.exists(dataPath)
        }
        disease_genes <- (read.csv(dataPath, header=TRUE, stringsAsFactors=FALSE))[,1]
        
        
      }
      met_genes <- unique(met_genes)
      disease_genes <- unique(disease_genes)
    }
    
    #Determine the size of the background gene set
    dataPath <- loadDiscoverable()
    load(dataPath)
    
    n_total <- length(unique(discGenes$PrimaryGeneNames))
    overlap <- met_genes[which(met_genes %in% disease_genes)]
    overrep_stat <- phyper((length(overlap)-1), length(disease_genes), (n_total-length(disease_genes)), length(met_genes), lower.tail=FALSE)
    
    results = list(overlap_genes=overlap, overrep_stat=overrep_stat, n_background=n_total)
  
    cat("Disease Over-Representation module complete!\n")

    # Save object if desired
    if(savePath != ""){
      cat("Do you wish to save the results of this module (y or n)?\n")
      saveChoice <- readline()
      saveChoice <- evalProceed(saveChoice)
      while(saveChoice == "i"){
        cat("Invalid input. Do you wish to save the results of this module (y or n)?\n")
        saveChoice <- readline()
        saveChoice <- evalProceed(saveChoice)
      }
      
      if(saveChoice == "y"){
        setwd(savePath)
        cat("How do you wish to save the results?\n")
        cat("1 - as R objects in an RData file\n")
        cat("2 - as excel files\n")
        cat("3 - both\n")
        cat("4 - neither\n")
        cat("Enter a number: ")
        saveChoice <- readline()
        while( saveChoice != "1" & saveChoice != "2" & saveChoice != "3" & saveChoice != "4"){
          cat("Invalid input. Enter a number: ")
          saveChoice <- readline()
        }
        
        if(saveChoice == "1" | saveChoice == "3"){
          # save RData file
          res_diseaseOverRep <- results
          save(res_diseaseOverRep, file="mod4_res_diseaseOverRep.RData")
          rm(res_diseaseOverRep)
        }
        if(saveChoice == "2" | saveChoice == "3"){
          # save excel files
          if(length(results$overlap_genes) != 0){
            write.table(results$overlap_genes, file="mod4_overlap_genes.csv", row.names=FALSE, col.names=c("genes_metResults_and_diseaseResults"))
          }
          stats_overview <- matrix(c(results$n_background, results$overrep_stat),  nrow=1)
          write.table(stats_overview, file="mod4_overrep_pval.csv", row.names=FALSE, col.names = c("background_Genes", "overRepresentation_pValue_upperTail"))
          rm(stats_overview)
        }
        
        cat("Results were saved in ", savePath, "\n", sep="")
      }
    }
    
    return(results)
  }
}



##### MAIN #####

### Welcome and package set up

cat("\nWelcome.\n\n")

reqPackages <- c("XML", "RSelenium", "rvest", "tidyverse", "igraph", "jaccard", "tm", "wordcloud", "scales", "shiny", "DT", "openxlsx")
startUp(reqPackages)

cat("Loading libraries... \n(Do not worry if output shows up on the console.)\n")
Sys.sleep(3)
library(XML)
library(RSelenium)
library(rvest)
library(tidyverse)
library(igraph)
library(jaccard)
library(tm) 
library(wordcloud)
library(scales)
library(shiny)
library(DT)
library(openxlsx)
cat("\nLibraries have been loaded.\n\n")

chromeVersion <- setChrome()

### Set where to save results

savePath <- setDir()

### Run modules

cat("\nWorkflow beginning.\n\n")

res_pathOverRep <- pathOverRep()

res_interProtGenes <- interProtGenes()

res_GWAS <- gwasGenes()

res_diseaseOverRep <- diseaseOverRep()

cat("\nWorkflow over.\n\n")
