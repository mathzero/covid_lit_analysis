
# Function takes a list of tables and saves them in a nicely formatted excel workbook, with 
# one named sheet per table
savePrettyExcelWorkbook <- function(listOfTables, workbookName = "myworkbook",outpath,
                                    noDecimalsColumns=NULL,numFmt = "0.00"){
  
  # first open a workbook
  wb <- openxlsx::createWorkbook()
  i <- 1
  # then loop over list of tables adding worksheets
  for(i in 1:length(listOfTables)){

    tab=listOfTables[[i]]
    tabName=names(listOfTables)[[i]]


    # define columns for rounding and whole numbers
    wholeNumberColumns=which(colnames(tab)%in% c("Positive", "Total",noDecimalsColumns))
    decimalNumberColumns=which(colnames(tab) %in% c("Prevalence","Lower","Upper" ))
    
    
    # First we delete all the repeating variable names
    dupes=duplicated(tab$Variable)
    if(length(dupes)>0){
      tab$Variable[dupes] <-  NA_character_
    }
    
    # now replace all NaNa
    is.nan.data.frame <- function(x) do.call(cbind, lapply(x,is.nan))
    tab[is.nan(tab)] <- NA
    
    # Open a new worksheet
    openxlsx::addWorksheet(wb,tabName)
    
    # define style
    rowShadeStyle = openxlsx::createStyle(fontColour = "black",fontSize = 9,numFmt = numFmt,
                                fgFill = "#F5F5F5", halign= "left", valign = "center")
    rowWhiteStyle = openxlsx::createStyle(fontColour = "black",fontSize = 9,numFmt = numFmt,
                                fgFill = "#FFFFFF", halign= "left", valign = "center")
    dashedRowBorder=openxlsx::createStyle(border = "top",borderStyle = "dotted",
                                borderColour = "grey70",numFmt = numFmt)
    headerStyleLeft = openxlsx::createStyle(fontSize = 9,
                              valign = "center",
                              fgFill = "white",
                              halign = "left",
                              # bgFill = "none",
                              border = "bottom",
                              borderStyle = "thin")
    headerStyleRight = openxlsx::createStyle(fontSize = 9,
                                  valign = "center",
                                  fgFill = "white",
                                  halign = "right",
                                  # bgFill = "none",
                                  border = "bottom",
                                  borderStyle = "thin")
    
    # col styles for numbers
    wholeNumberColStyle =openxlsx::createStyle(numFmt = "0",halign= "right")
    numberColStyle =openxlsx::createStyle(numFmt = numFmt,halign= "right")
    
    
    colourRows=2:(nrow(tab)+1)
    dashRows=which(!dupes)+1
    
    # apply styles
    openxlsx::addStyle(wb = wb,sheet = tabName,style = rowWhiteStyle,
             cols = 1:ncol(tab),
             rows = colourRows, 
             gridExpand = T,stack = T)
    
    openxlsx::addStyle(wb = wb,sheet = tabName,style = rowShadeStyle,
             cols = 1:ncol(tab),
             rows = colourRows[which(colourRows%%2==1)], 
             gridExpand = T,stack = T)
    
    openxlsx::addStyle(wb = wb,sheet = tabName,style = dashedRowBorder,
             cols = 1:ncol(tab),
             rows = dashRows, 
             gridExpand = T,stack = T)
    
    ## add column number styles
    openxlsx::addStyle(wb = wb,sheet = tabName,
             style = wholeNumberColStyle,
             cols = wholeNumberColumns,
             rows = 2:(nrow(tab)+1), 
             gridExpand = T,stack = T)
    openxlsx::addStyle(wb = wb,sheet = tabName,
             style = numberColStyle,
             cols = decimalNumberColumns,
             rows = 2:(nrow(tab)+1), 
             gridExpand = T,stack = T)

      # add header styles
    openxlsx::addStyle(wb = wb,sheet = tabName,
             style = headerStyleRight,
             cols = which(as.logical(lapply(tab,is.numeric))),
             rows = 1, 
             gridExpand = T,stack = T)
    openxlsx::addStyle(wb = wb,sheet = tabName,
             style = headerStyleLeft,
             cols = which(!as.logical(lapply(tab,is.numeric))),
             rows = 1, 
             gridExpand = T,stack = T)
    
    
    # set column widths
    openxlsx::setColWidths(wb = wb,sheet = tabName,
                 cols = 1:ncol(tab),
                 widths = "auto")
    
    
    
    openxlsx::writeDataTable(wb = wb,sheet = tabName,
                             x = tab,
                             startCol = 1,startRow = 1,
                             tableStyle = "none",
                             bandedRows = F,
                             withFilter = F,
                             na.string = "-",
                             stack = T)
    
  }
  openxlsx::saveWorkbook(wb = wb,file = paste0(outpath,workbookName,".xlsx"),overwrite = T)
  
  
}
