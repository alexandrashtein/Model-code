library(magrittr)

# 1
# Create file: ~/.netrc
# Contents: machine urs.earthdata.nasa.gov login <uid> password <password>

# fileConn<-file("~/netrc")
# writeLines("machine urs.earthdata.nasa.gov login u password p", fileConn)
# close(fileConn)

# 2
# Create file: ~/.urs_cookies
# Contents: EMPTY

# fileConn<-file("~/.urs_cookies")
# writeLines("", fileConn)
# close(fileConn)


setwd("/media/qnap/Projects/P028.IL.Israel.MAIAC.PM.V2/raw/OMI_AAI_data")

# Base URL
dir = "https://acdisc.gesdisc.eosdis.nasa.gov/data/Aura_OMI_Level3/OMTO3d.003/2005"

# Get list of files
x = system('wget -q -nH -nd "%s" -O - | grep "he5" | cut -f4 -d\\"' %>% sprintf(dir), intern = TRUE)

# Download one file
file = x[1]
"wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies %s/%s" %>% 
  sprintf(dir, file) %>% 
  system

# Doenload all 'he5' files
'wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -r -c -nH -nd -np -A he5,xml "%s"' %>% 
  sprintf(dir) %>% 
  system



