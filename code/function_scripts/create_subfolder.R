createMySubfolder <- function(subfolderName="example"){
  
  outpath<<-paste0(outpath,subfolderName,"/")
  figpath<<-paste0(figpath,subfolderName,"/")
  
  dir.create(outpath,showWarnings = F)
  dir.create(figpath,showWarnings = F)
  
}