##########################################################################
# THIS CODE MAKES SOME QUALITY CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#

library(tidyverse)

data_dir <- "C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2022/pekka2022"
data_dir_old <- "C:/Users/OMISTAJA/Documents/repos/kilpisjarvi_microclimate/data"

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files(data_dir, pattern = "binary_", recursive = T, full.names = T),
       list.files(data_dir, pattern = "command_", recursive = T, full.names = T))

for(i in f){ 
  if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
    unlink(i)
  } else {
    print(paste0("DATA FILE MISSING!!! ", i))
  } 
}
# If no printed messages then no problems

###########################################################################
# Change names of the saana/mikkuna directories

dirs <- list.dirs(data_dir, recursive = F)

add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

for(i in dirs){
  id <- as.numeric(tail(strsplit(i, "/")[[1]], 1))
  if(!is.na(id)){
    file.rename(from = i,
                to = paste0(paste(head(strsplit(i, "/")[[1]], -1), collapse = "/"),
                            "/",paste0("SAA", add_zeros(id))))
  }
}


# # Haxo
# 
# f <- list.files("data", pattern = "-2021.ltd$", recursive = T, full.names = T)
# 
# for(i in f){ if(file.exists(gsub("-2021.ltd","-2021.csv",i))){
#   unlink(i)
# } else {
#   print(paste0("DATA FILE MISSING!!! ", i))
# } 
# }

###########################################################################
# Check Tomst ID-numbers from last year data
maxdt <- read_csv("data/reading_times_2021.csv")
maxdt20 <- bind_rows(read_csv("data/reading_times_2020_AIL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("AIL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_MAL.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("MAL", add_zeros(x))))),
                   read_csv("data/reading_times_2020_SAA.csv") %>% mutate(site = unlist(lapply(site, function(x) paste0("SAA", add_zeros(x))))))

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

# No problems
# maxdt %>% filter(tomst_id == "94194078")
# maxdt %>% filter(tomst_id == "94194080")
###########################################################################
# Update the file list

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

#######################################################################
# Check if missing sites in 2021 data
all <- full_join(fi, maxdt)

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

maxdt20 %>% filter(tomst_id %in% c(94214996, 94214997, 94181612, 94181602) )

# Sites SAA895 and SAA1195 have previous data from different logger
f2 <- list.files(data_dir_old,
                 pattern = "data_", recursive = T, full.names = T)

# Copy site SAA1195 data from last year data
f2[grepl("94181612", f2)]
file.copy(f2[grepl("94181612", f2)],
          paste0(data_dir, "/SAA1195/data_94181612_0.csv"))

# Copy site SAA895 data from last year data
f2[grepl("94181602", f2)]
file.copy(f2[grepl("94181602", f2)],
          paste0(data_dir, "/SAA895/data_94181602_0.csv"))

# Non-matching sites
all %>% filter(!complete.cases(.))

# No sites that occur only in 2021 data

maxdt20 %>% filter(tomst_id == 94194157) # 
all %>% filter(tomst_id == 94194157) # 

# For site 39 find 2021 data and copy to repository
f2 <- list.files(data_dir_old,
                 pattern = "data_", recursive = T, full.names = T)

# Copy site AIL105 data from last year data
f2[grepl("94194176", f2)]
dir.create(paste0(data_dir, "/AIL105"))
file.copy(f2[grepl("94194176", f2)],
          paste0(data_dir, "/AIL105/data_94194176_0.csv"))

# Copy site AIL108 data from last year data
f2[grepl("94194028", f2)]
dir.create(paste0(data_dir, "/AIL108"))
file.copy(f2[grepl("94194028", f2)],
          paste0(data_dir, "/AIL108/data_94194028_0.csv"))

# Copy site MAL073 data from last year data
f2[grepl("94194135", f2)]
dir.create(paste0(data_dir, "/MAL073"))
file.copy(f2[grepl("94194135", f2)],
          paste0(data_dir, "/MAL073/data_94194135_0.csv"))

# Copy site MAL092 data from last year data
f2[grepl("94194157", f2)]
dir.create(paste0(data_dir, "/MAL092"))
file.copy(f2[grepl("94194157", f2)],
          paste0(data_dir, "/MAL092/data_94194157_0.csv"))

# Copy site RA061 data from last year data
f2[grepl("94204782", f2)]
dir.create(paste0(data_dir, "/RA061"))
file.copy(f2[grepl("94204782", f2)],
          paste0(data_dir, "/RA061/data_94204782_0.csv"))

# Copy site RA062 data from last year data
f2[grepl("94204778", f2)]
dir.create(paste0(data_dir, "/RA062"))
file.copy(f2[grepl("94204778", f2)],
          paste0(data_dir, "/RA062/data_94204778_0.csv"))

# Copy site RA080 data from last year data
f2[grepl("94204796", f2)]
dir.create(paste0(data_dir, "/RA080"))
file.copy(f2[grepl("94204796", f2)],
          paste0(data_dir, "/RA080/data_94204796_0.csv"))

# Copy site RA082 data from last year data
f2[grepl("94204747", f2)]
dir.create(paste0(data_dir, "/RA082"))
file.copy(f2[grepl("94204747", f2)],
          paste0(data_dir, "/RA082/data_94204747_0.csv"))

# Copy site SAA11231 data from last year data
f2[grepl("94181611", f2)]
dir.create(paste0(data_dir, "/SAA11231"))
file.copy(f2[grepl("94181611", f2)],
          paste0(data_dir, "/SAA11231/data_94181611_0.csv"))

# Copy site SAA367 data from last year data
f2[grepl("94181630", f2)]
dir.create(paste0(data_dir, "/SAA367"))
file.copy(f2[grepl("94181630", f2)],
          paste0(data_dir, "/SAA367/data_94181630_0.csv"))

# Copy site SAA583 data from last year data
f2[grepl("94181605", f2)]
dir.create(paste0(data_dir, "/SAA583"))
file.copy(f2[grepl("94181605", f2)],
          paste0(data_dir, "/SAA583/data_94181605_0.csv"))

# Copy site SAA751 data from last year data
f2[grepl("94181607", f2)]
dir.create(paste0(data_dir, "/SAA751"))
file.copy(f2[grepl("94181607", f2)],
          paste0(data_dir, "/SAA751/data_94181607_0.csv"))


maxdt20 %>% filter(tomst_id == 94181607) # 
all %>% filter(tomst_id == 94181607) # 


########################################################################################
# Update file list

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# This site 39 is like it should be

#######################################################################
# Check if Tomst ids match between years
all <- full_join(fi, maxdt %>% rename(tomst_id_21 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # These are fine

all %>% filter(tomst_id == tomst_id_21)
all %>% filter(tomst_id != tomst_id_21)
# All seems to match nicely!!!!!!!!!!


# Good to go and read the data


