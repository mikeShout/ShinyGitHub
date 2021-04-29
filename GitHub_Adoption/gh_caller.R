
library(tidyverse)
library(gh)

# This function calls the GitHub API to get data and then cleans the data into a simple summary table..

sum_data <- function(uName){
  err = FALSE
  # Get the json from various endpoints...
  repos <- tryCatch(
    gh(paste("GET /users/",uName, "/repos", sep=""), username = uName, per_page = 100),
    error = function(e) "User not found"
  )
  
  if(repos == "User not found") {
    # alert user of problem
    err = TRUE
    sum_table <- as_tibble(rbind(
      cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Repos"), 
      cbind(forked="Forked_Yes", count = 0, first_created = "", last_updated = "", Feature = "Pages"),
      cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Gists"),
      cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Proj"),
      cbind(forked="User not found!", count = 0, first_created = "", last_updated = "", Feature = "user"))) %>%
      mutate(first_created= as.Date(first_created), last_updated = as.Date(last_updated), count = as.numeric(count))
      
    

    
  } else {

    uType <- ifelse(first(sapply(sapply(repos, "[", "owner"), "[[", "type")) =="Organization", "orgs", "users")
    
    gists <- tryCatch(
      gh(paste("GET /", uType, "/", uName, "/gists", sep=""), username = uName),
      error = function(e) NULL
    )
    
    proj <- tryCatch(
      gh(paste("GET /", uType, "/", uName, "/projects", sep=""), username = uName, .accept = "application/vnd.github.inertia-preview+json"),
      error = function(e) NULL
    )
    
  }
  
  # parse the JSON and create a simple summary table...
  if(err == FALSE) {
    
  if(length(repos) > 0) {
    repos <- as_tibble(cbind(name = sapply(repos, "[[", "name"), url = sapply(repos, "[[", "html_url"), forked = sapply(repos, "[[", "fork"), created = sapply(repos, "[[", "created_at"), updated = sapply(repos, "[[", "updated_at"), has_pages = sapply(repos, "[[", "has_pages")))
    repos <- repos %>% mutate(
      created = as.Date(created, "%Y-%m-%d"), 
      updated = as.Date(updated, "%Y-%m-%d"), 
      has_pages = if_else(has_pages==TRUE, 1, 0),
      forked = if_else(forked=="FALSE", "Forked_No", "Forked_Yes"))
    
    sum_table <- repos %>% group_by(forked) %>% summarize(count = n(), first_created = max(created), last_updated = max(updated)) %>% add_column(Feature = "Repos")
    
    if(sum(repos$has_pages>0)) {
      sum_table <- sum_table %>% add_row(repos[repos$has_pages==1,]%>% group_by(forked) %>% summarize(count = n(), first_created = max(created), last_updated = max(updated)) %>% add_column(Feature = "Pages"))
    } else {
      sum_table <- sum_table %>% add_row(forked="Forked_No", count = 0, first_created = NULL,last_updated = NULL, Feature = "Pages")
    }
  } else {
    
    sum_table <- as_tibble(rbind(
      cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Repos"), 
      cbind(forked="Forked_Yes", count = 0, first_created = "", last_updated = "", Feature = "Pages"))) %>% 
      mutate(first_created= as.Date(first_created), last_updated = as.Date(last_updated), count = as.numeric(count))
    
  }
  
  if(length(gists) > 0) {
    gists <- as_tibble(cbind(url = sapply(gists, "[[", "html_url"), created = sapply(gists, "[[", "created_at"), updated = sapply(gists, "[[", "updated_at")))
    gists <- gists %>% mutate(created = as.Date(created, "%Y-%m-%d"), updated = as.Date(updated, "%Y-%m-%d"))
    sum_table <- sum_table <- sum_table %>%
      add_row(forked="Forked_No", count = nrow(gists), first_created = min(gists$created),last_updated = max(gists$updated), Feature = "Gists")
  } else {
    sum_table <- sum_table %>%
      add_row(forked="Forked_No", count = 0, first_created = NULL,last_updated = NULL, Feature = "Gists")
    
  }
  
  if(length(proj) > 0) {
    proj <- as_tibble(cbind(name = sapply(proj, "[[", "name"), created = sapply(proj, "[[", "created_at"), updated = sapply(proj, "[[", "updated_at")))
    proj <- proj %>% mutate(created = as.Date(created, "%Y-%m-%d"), updated = as.Date(updated, "%Y-%m-%d"))
    sum_table <- sum_table %>%
      add_row(forked="Forked_No", count = nrow(proj), first_created = min(proj$created),last_updated = max(proj$updated), Feature = "Proj")
  } else {
    sum_table <- sum_table %>%
      add_row(forked="Forked_No", count = 0, first_created = NULL,last_updated = NULL, Feature = "Proj")
  }
  
  sum_table <- sum_table %>% add_row(forked=uName, count = 0, first_created = NULL, last_updated = NULL, Feature = "user")
  

  }
  sum_table
} # function



process_name <- function(uName){
  if(nchar(uName)<1 || str_count(uName, " ")>0) {
    "Please enter a valid GitHub username (no spaces)!"
  } else {
    #paste(uName, "is the user!!")
    as.data.frame(cbind(Name=uName, stats="test"))
    
  }
  
}