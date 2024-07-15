#  install automatically packages if necessary
instal.import.package <- function(package){
  if(!(package %in% installed.packages()[,1]))
    install.packages(package)
  if(!(paste0("package:", package) %in% search()))
    library(package, character.only=TRUE, quietly = T)
}
