#-------------- Wczytanie pakietów --------------

install.packages("xml2")
library(xml2)

library(dplyr)

#-------------------------------------------------

#ROOT_PATH <- "C:\\Users\\aga71\\OneDrive\\studia\\SEMESTRY\\MGR\\I\\inne\\pd3_PADR_PADPy\\PADR\\"
ROOT_PATH <- "C:\\Users\\aga71\\OneDrive\\studia\\SEMESTRY\\MGR\\I\\inne\\pd3_PADR_PADPy\\serwisy_xml\\"

xml_to_df <- function(xml_path){
  path <- paste(ROOT_PATH, xml_path, sep = "")
  doc <- read_xml(path)
  data.frame(bind_rows(lapply(xml_find_all(doc, "//row"), function(x) {
    data.frame(as.list(xml_attrs(x)), stringsAsFactors = FALSE)
  })))
}

#-------------------------------------------------

#BadgesF <- xml_to_df("Badges.xml")
BadgesF <- xml_to_df("fitness_stackexchange_com\\Badges.xml")
typeof(BadgesF)
BadgesF.head()


CommentsF <- xml_to_df("Comments.xml")
CommentsF.head()