library(stringr)

colleges <- c("Reed College", "University of Arizona", "Harvard University", "Yale")

append_names <- function(string) {
  
  string <- str_remove(string, " College")
  string <- str_remove(string, "University of ")
  string <- str_remove(string, " University")
  string <- str_remove(string, "Pennsylvania State-")
  string <- str_remove(string, "Institute of Technology")
  string <- str_remove(string, "Liberal Arts and Sciences")
  string
}

append_names(colleges)

names <- colleges_mrc$school_name
append_names(names)
