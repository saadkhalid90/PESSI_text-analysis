## importing relevant libraries
library(readr)
library(dplyr)
library(stringdist)

## read in the files and selecting relevant features

PESSI <- read.csv('PESSI_Multan.csv', encoding = 'UTF-8') %>% select(Unit.Name, Tehsil, Nature.of.Business, Address.of.Establishment)
## converting factors to character variables
PESSI <- data.frame(sapply(X = PESSI, FUN = as.character), stringsAsFactors=FALSE)

str(PESSI)

PVT_list <- read_csv('Pvt_schools_Multan_dir.csv') %>% select(SchoolName, TehsilName, SchoolAddress)

PESSI$Unit.Name <- gsub(pattern = '[Mm]/( )?[Ss](.)?', replacement = "", x = PESSI$Unit.Name)


PESSI$Unit.Name <- toupper(trimws(PESSI$Unit.Name, which = 'both'))
PESSI <- data.frame(sapply(X = PESSI, FUN = function(x) return(toupper(trimws(x, which = 'both')))), stringsAsFactors=FALSE)


logical <- grepl(pattern = "SCHOOL|ACAD|EDUCATOR|COLLEGE", x = PESSI$Unit.Name) | PESSI$Nature.of.Business == "SCHOOLS"

sum(logical)
PESSI_schools <- data.frame(PESSI[logical, ])

PVT_list$TehsilName <- gsub(pattern = "MULTAN (.)+", replacement = "MULTAN", x = PVT_list$TehsilName)
PVT_list$TehsilName <- gsub(pattern = " ABAD", replacement = "BAD", x = PVT_list$TehsilName)

## checking if the tehsils in both datasets are the same:
## adjusting indices as PESSI data also has blank Tehsil fields
sort(unique(PVT_list$TehsilName)) == sort(unique(PESSI$Tehsil))[2:10]


tehsils <- sort(unique(PVT_list$TehsilName))

consol <- data.frame(matrix(vector(), 0, 12,
                            dimnames=list(c(), c("PESSI_schools", "Address", paste0("BM", 1:5), paste0("Address_BM", 1:5)))),
                     stringsAsFactors=F)


for (i in tehsils){
  print(i)
  ## i <- "MULTAN"
  ## subset based on tehsil
  PESSI_schools_subset <- PESSI_schools %>% filter(Tehsil == i)
  PVT_schools_list <- PVT_list %>% filter(TehsilName == i) 
  

  dist_matrix_q2 <- stringdistmatrix(a = PESSI_schools_subset$Unit.Name, b = PVT_schools_list$SchoolName, method = "cosine", q = 2)
  dist_matrix_q3 <- stringdistmatrix(a = PESSI_schools_subset$Unit.Name, b = PVT_schools_list$SchoolName, method = "cosine", q = 3)
  dist_matrix_q4 <- stringdistmatrix(a = PESSI_schools_subset$Unit.Name, b = PVT_schools_list$SchoolName, method = "cosine", q = 4)
  
  dist_matrix <- (dist_matrix_q2 + dist_matrix_q3 + dist_matrix_q4)/ 3
  
  min_5_idx <- apply(X = dist_matrix, MARGIN = 1, FUN = function(a){return(c(which(a == sort(a)[1]),
                                                                             which(a == sort(a)[2]),
                                                                             which(a == sort(a)[3]),
                                                                             which(a == sort(a)[4]),
                                                                             which(a == sort(a)[5]))[1:5])})
  min_5_idx <- t(min_5_idx)
  
  school_match <- as.data.frame(matrix(PVT_schools_list$SchoolName[min_5_idx], dim(dist_matrix)[1], 5))
  school_address <- as.data.frame(matrix(PVT_schools_list$SchoolAddress[min_5_idx], dim(dist_matrix)[1], 5))
  PESSI_schools_units <- PESSI_schools_subset$Unit.Name
  Address <- PESSI_schools_subset$Address.of.Establishment
  comp_matrix <- cbind(PESSI_schools_units, Address, school_match, school_address)
  
  ##print(consol)
  print(comp_matrix)
  
  consol <- rbind(consol, comp_matrix)

}

names(consol) <- c("PESSI_schools", "Address", paste0("BM", 1:5), paste0("Address_BM", 1:5))
consol <- consol %>% select_(.dots = c("PESSI_schools", "Address", paste0(c("BM", "Address_BM"), rep(x = 1:5, each = 2))))

write.csv(x = consol, file = 'comparison.csv', row.names = FALSE)



