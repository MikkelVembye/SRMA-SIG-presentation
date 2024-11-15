#-------------------------------------------------------------------------------
# Install and load relevant package
#-------------------------------------------------------------------------------

# Install from CRAN
# install.packages("AIscreenR")

# If development version is wanted
# install.packages("devtools")
# devtools::install_github("MikkelVembye/AIscreenR")

# Load relevant package

library(AIscreenR) # Used to screen titles and abstracts with gpt 
library(revtools)  # Used to load and convert RIS file data to a data.frame
library(tibble)    # Used to work with tibbles, i.e., lazy data.frames 
library(dplyr)     # Used for data manipulation
library(purrr)     # For loop
library(future)    # Used to run screenings in parallel
library(readxl)    # Use if you want to save the screen data to an excel file

options(scipen = 100)

#-------------------------------------------------------------------------------
# Ensure correct working directory
#-------------------------------------------------------------------------------

# Ensure the working directory points to the folder containing the RIS file(s)
getwd()
# Alternatively set working directory or work in an R project
#setwd("C:/Users/B199526/Desktop/GitHub repos/SRMA-SIG-presentation")


#-------------------------------------------------------------------------------
# Load and convert RIS file to data frames
#-------------------------------------------------------------------------------

# Load excluded references
ris_dat_excl <- revtools::read_bibliography("friends_excl.ris") |> 
  suppressWarnings()

set.seed(20241113)

# Sample 100 irrelevant reference for the the test data
excluded_sample <- 
  ris_dat_excl |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>  # Selecting relevant variables only
  mutate(
    human_code = 0,                                      # Track human decision
    across(c(author, title, abstract), ~ na_if(., "NA")) # Handle missing titles and abstracts
  ) |> 
  dplyr::filter(!is.na(abstract)) |>                     # Remove references with no abstract
  AIscreenR::sample_references(n = 100)                  # Sample references 

View(excluded_sample)

# Load included references
ris_dat_incl <- revtools::read_bibliography("friends_incl.ris") |> 
  suppressWarnings()

# Sample 50 irrelevant references for the the test data
included_sample <- 
  ris_dat_incl |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>  # Selecting relevant variables only
  mutate(
    human_code = 1,                                      # Track human decision
    across(c(author, title, abstract), ~ na_if(., "NA")) # Handle missing titles and abstracts
  ) |> 
  dplyr::filter(!is.na(abstract)) |>                     # Remove references with no abstract
  AIscreenR::sample_references(n = 50)                   # Sample references

# Construct test dataset
test_dat <- 
  bind_rows(excluded_sample, included_sample) |> 
  mutate(studyid = 1:n())
View(test_dat)


#-------------------------------------------------------------------------------
# Write prompt(s)
#-------------------------------------------------------------------------------

prompt <- "We are screening studies for a systematic literature review. 
The topic of the systematic review is the effect of the FRIENDS preventive programme
on reducing anxiety symptoms in children and adolescents. The FRIENDS programme is a 
10-session manualised  cognitive behavioural therapy (CBT) programme which can be 
used as both prevention and  treatment of child and youth anxiety. The study should 
focus exclusively on this topic  and we are exclusively searching  for studies with 
a treatment and a comparison group. For each study, I would like you to assess:  
1) Is the study about the FRIENDS preventive programme? 
2) Is the study estimating an effect between a treatment and control/comparison group?"


#-------------------------------------------------------------------------------
# # Handle your API key 
#-------------------------------------------------------------------------------

# Get from https://platform.openai.com/settings/organization/api-keys 
#set_api_key()

# To permanently added to you R environment use
# See https://mikkelvembye.github.io/AIscreenR/articles/Using-GPT-API-Models-For-Screening.html#permanent-solution for how to do so
#usethis::edit_r_environ()

# To see how the AIscreenR function handle your key
?tabscreen_gpt

#-------------------------------------------------------------------------------
# Inspect model access
#-------------------------------------------------------------------------------

# Get you info about your model access and rate limits
rate_limit <- AIscreenR::rate_limits_per_minute() 
rate_limit

# See across all available models
rlpm_all_models <- rate_limits_per_minute(model = model_prizes$model)
View(rlpm_all_models)


#-------------------------------------------------------------------------------
# Run first screening
#-------------------------------------------------------------------------------

# Set parallel plan
plan(multisession)

result_obj <-
  AIscreenR::tabscreen_gpt(
    data = test_dat,                       # The data
    prompt = prompt,                       # The prompt(s) - can take multiple inputs
    studyid = studyid,                     # Variable name containing the study IDs
    title = title,                         # Variable name containing the titles
    abstract = abstract,                   # Variable name containing abstracts
    model = "gpt-4o-mini",                 # Model - can take multiple inputs
    reps = 1,                              # The number of screenings you want to run
    rpm = rate_limit$requests_per_minute   # Requests per minute
  )

# Back the sequential plan
plan(sequential)

result_obj

result_obj$answer_data |> View()

#saveRDS(result_obj, file = "res_obj_4o-mini.rds")
# Worst case
#result_obj <- readRDS("res_obj_4o-mini.rds")

#-------------------------------------------------------------------------------
# Analyze screening performance
#-------------------------------------------------------------------------------
performance <- 
  result_obj |> 
  AIscreenR::screen_analyzer(
    human_decision = human_code, # Name variable with human decisions
    key_result = TRUE # # State whether only key results should be printed. Default is true
  )

performance 

#-------------------------------------------------------------------------------
# Using multiple screening - this will a take a couple of minutes
#-------------------------------------------------------------------------------

plan(multisession)

result_obj_reps10 <-
  tabscreen_gpt(
    data = test_dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    reps = 10,
    rpm = rate_limit$requests_per_minute
  )

plan(sequential)

#saveRDS(result_obj_reps10, file = "res_obj_4o-mini_reps10.rds")
# I have cheated from home
result_obj_reps10 <- readRDS("res_obj_4o-mini_reps10.rds")
result_obj_reps10


performance10 <- 
  result_obj_reps10 |> screen_analyzer()

performance10


performance_dist <- attr(performance10, "p_incl_data")
performance_dist |> View()


#-------------------------------------------------------------------------------
# Check disagreement between human and GPT
# Remember also to check studies included by GPT and excluded by humans. 
#-------------------------------------------------------------------------------

disagree_dat <- 
  result_obj_reps10$answer_data_aggregated |> 
  filter(human_code == 1, incl_p == 0) 
  
plan(multisession)

dis_res <-
  tabscreen_gpt(
    data = disagree_dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    decision_description = TRUE, # Set TRUE to get detailed responses
    reps = 10, 
    rpm = rate_limit$requests_per_minute
  )

plan(sequential)


#saveRDS(dis_res, file = "disagreement_description_res_obj_4o-mini_10reps.rds")
# Worst case
#dis_res <- readRDS("disagreement_description_res_obj_4o-mini_10reps.rds")

dis_res$answer_data_aggregated |> 
  select(author, abstract, final_decision_gpt, longest_answer) |> 
  View()

#-------------------------------------------------------------------------------
# Report and comment on disaggreements
#-------------------------------------------------------------------------------

# Report function - will eventually be added to AIscreenR --------
report <- 
  function(
    data, studyid, title, abstract, answer, 
    file = "StudyReport_AIscreenR.Rmd", format = "html_document", ...){
    
    studyid <- data |> dplyr::pull({{ studyid }})
    title <- data |> dplyr::pull({{ title }})
    abstract <- data |> dplyr::pull({{ abstract }})
    
    studyid_txt <- paste0("**STUDY-ID: ", studyid, ":**", "\n\n")
    title_text <- paste0("-- *Title:* '", title, "'", "\n\n")
    abs_txt <- paste0("-- *Abstract*: ", abstract, "\n\n")
    
    if (missing(answer)){
      answer_txt <- NULL
    } else {
      answer <- data |> dplyr::pull( {{answer}} )
      answer_txt <- paste0("-- *Answer (GPT)*: ", answer, "\n\n")
    }
    
    comment_text <- paste0("*Please add a comment on whether and why you agree with the GPT decision or not:*\n\n \n\n")
    
    print_test <- paste0(studyid_txt, title_text, abs_txt, answer_txt, comment_text)
    
    cat("---
title: \"Study Report - Disagreement Explanations\"
subtitle: \"Included by humans, excluded by GPT\"
output: html_document
---
           
\`\`\`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
\`\`\`


\`\`\`{r, echo=FALSE, results='asis', include=TRUE}

base::cat(print_test)

\`\`\`", 
        file = file
    )
    message(paste0("Saving to", getwd(), "/", file)) 
    message(paste0("Rendering ", file))  
    file_out <-  rmarkdown::render(file, output_format = format, quiet = TRUE, ...) 
    
    message(paste0("Opening ", file))  
    shell.exec(file_out)
    
    invisible(file_out)
    
  }

# Get a report with screening disagreements 
report(
  data = dis_res$answer_data_aggregated, 
  studyid = studyid, 
  title = title, 
  abstract = abstract, 
  answer = longest_answer, 
  format = "word_document"
)


#-------------------------------------------------------------------------------
# Approximate price 
#-------------------------------------------------------------------------------

# Approximate price of full screening (2862 refs)
all_dat <- bind_rows(ris_dat_excl, ris_dat_incl) |> as_tibble() 
nrow(all_dat)

app_price <- 
  approximate_price_gpt(
    data = all_dat,
    prompt = prompt,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini",
    reps = 10
  )

app_price$price_data
app_price$price_dollar 
