# Based on code from https://rpubs.com/ZakWitkower/ManuscriptPDFtoWordCloud


library(pdftools)
library(tm)
library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud2)
library(randomcoloR)

file_path = getwd()

# List PDF files
pdf_files <- list.files(path = "papers", pattern = "pdf$", full.names = T)


# Function to specify final text being analyzed
extract_text_before_method <- function(file_path) {
  
  # Extract all text from the PDF
  full_text <- paste(pdf_text(file_path), collapse = "\n")
  
  # Retain text up until "References"
  extracted_text <- sub("(.*?)References\\n.*", "\\1", full_text)
  
  # Retain text up until "Results"
  # extracted_text <- sub("(.*?)Results\\n.*", "\\1", extracted_text)
  
  return(full_text)
}

# Create uninitialized list
pdf.files<-list()

# Extract text from files
for (file in pdf_files) {
  print(file)
  pdf_text <- extract_text_before_method(file)
  
  # Clean data by removing new lines
  pdf_text <- gsub("\n", "", pdf_text)
  
  pdf.files[[file]] <- pdf_text
}


pdf.files<-TermDocumentMatrix(pdf.files, # data run through the function will go here.
                              control = 
                                list(removePunctuation = TRUE, # Remove punctuation
                                     stopwords = TRUE,         # Remove stopwords 
                                     tolower = TRUE,           # Converts to lowercase
                                     stemming = F,             # Retain full words
                                     removeNumbers = TRUE))    # Remove numbers

print(pdf.files)

#Clean the data
pdf.files.cleaned<-tidy(pdf.files) %>% 
  group_by(term) %>%  # For each term
  summarise(count,count = sum(count)) %>% # Count the number of times it appears in the TDM
  unique()%>%  
  ungroup() %>%
  arrange(desc(count)) # And sort the TDM in descending order


# Manual curation
filter.list <- c("plant","–","using","can","used","figure","different","system","crop","growth","analysis","high",
                 "plants","conditions","also","based","results","use","methods","−","fig","information","method","study",
                 "values,","may","wheat","table","low","shown","one","however","obtained","two","number","research",
                 "current","content","developed","applications","found","sens","crossref","review","will","sci","due",
                 "zhang","set","wang","wiley","maize","new","per","many","change","large","well","within","three","high","highthroughput",
                 "see")
pdf.files.cleaned.manual <- pdf.files.cleaned[!pdf.files.cleaned$term %in% filter.list, ]

print(pdf.files.cleaned.manual,n=110)

# Manual merge
sum_delete_replace <- function(df, str_array) {
  values <- df[df$term %in% str_array, ]
  new_df <- df[!df$term %in% str_array, ] %>%
    add_row(term = str_array[1], count = sum(values$count)) %>%
    arrange(desc(count))
  
  return(new_df)
}

pdf.files.cleaned.manual = sum_delete_replace(pdf.files.cleaned.manual, c("image","images","imagery","imaging"))
pdf.files.cleaned.manual = sum_delete_replace(pdf.files.cleaned.manual, c("sensor","sensors","sensing"))
pdf.files.cleaned.manual = sum_delete_replace(pdf.files.cleaned.manual, c("uav","uavs"))

print(pdf.files.cleaned.manual,n=50)

# Trim to most frequently used words
pdf.files.trimmed<-head(pdf.files.cleaned.manual, 100)
write.csv(pdf.files.trimmed,file="word_frequency.csv",row.names = F)

# Word cloud
wordcloud2(pdf.files.trimmed, 
           fontWeight = "bold", 
           rotate = 0, 
           size = 0.8, 
           fontFamily = "Tahoma", 
           color = randomColor(nrow(pdf.files.trimmed), #Using the randomcoloR package
                               hue = "random", 
                               luminosity = "dark"))

