library(tidyverse)
library(gh)
library(plotly)

# Usfeul JSON Viewing tool...
#   library(jsonview)
#   json_tree_view(gists)

# A GitHub color palette:
# https://www.schemecolor.com/github.php#:~:text=GitHub%20Colors%20with%20Hex%20%26%20RGB%20Codes%20,RGB%3A%20%285%20...%20%202%20more%20rows%20

# API Notes:
# Will only use endpoints and queries that do not require authentication
# The repos endpoint does not need to know the user type (org or user), but other endpoints do... 

# Getting Data Process
#   1. Provide look-up user (org) and retrieve API results using repos endpoint first
#   2. If error, then user/org not found and stop looking for other data
#   3. If found, determine the user type for use with other endpoints

# user names to test with...
# uName <- "mikeShout" # User
uName <- "hadley"
uName <- "spring-projects" # Org
#uName <- "github" # Org
#uName <- "erronpurpose" # non-existant GitHub user
uName <- "bigbangdata" # using pages
uName <- "fabpot" #active user

# 1. get repos or determine user is not found...
repos <- tryCatch(
  gh(paste("GET /users/",uName, "/repos", sep=""), username = uName, per_page = 100),
  error = function(e) "User not found"
)

if(repos == "User not found") {
  # 2. Stop processing and alert user of problem
  
} else {
  #3. determine the user type and get additional data...
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

# The returned data will be either an empty variable or json data
# If there is json data, parse out selected elements into a tibble, 
# otherwise create an empty tibble

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

#Charts
#repos %>% group_by(forked) %>% summarize(count = n()) %>% mutate(forked = if_else(forked=="FALSE", "Not Forked", "Forked")) %>% ggplot(aes(y=count)) + geom_bar(aes(fill = forked), position = position_stack(reverse = TRUE))

# Days since last update = 

repo_chart <- repos %>% 
  group_by(forked) %>% 
  summarize(count = n()) %>% 
  mutate(forked = if_else(forked=="FALSE", "Not Forked", "Forked")) %>% 
  pivot_wider(names_from=forked,values_from=count) %>%
  plot_ly(x = ~Forked, y = "", type = 'bar', orientation = 'h', name = 'Forked',
          marker = list(
            color = 'rgba(65, 131, 196, 0.6)',
            line = list(color = 'rgba(46, 92, 137, 1.0)', width = 3))) %>%
  add_trace(x = ~`Not Forked`, name = 'Not Forked',
            marker = list(
              color = 'rgba(102, 102, 102, 0.6)',
              line = list(color = 'rgba(51, 51, 51, 1.0)', width = 3))) %>%
  layout(legend = list(orientation = "h", yanchor = "top", y=1, xanchor="center", x=.5), barmode = 'stack',
         xaxis = list(title = "Repo Count"),
         yaxis = list(title =""))

gists_chart <- plot_ly(x=nrow(gists), y="", type='bar', orientation='h', 
                       marker = list(
                         color = 'rgba(65, 131, 196, 0.6)',
                         line = list(color = 'rgba(46, 92, 137, 1.0)', width = 3))) %>%
  layout(xaxis = list(title = "Gist Count", range=c(0,40)), yaxis = list(title =""))



combo_chart <- repos %>% 
  group_by(forked) %>% 
  summarize(count = n()) %>% 
  mutate(forked = if_else(forked=="FALSE", "Not Forked", "Forked")) %>% 
  pivot_wider(names_from=forked,values_from=count) %>%
  plot_ly(x = ~Forked, y = "Repos", type = 'bar', orientation = 'h', name = 'Forked',
          marker = list(
            color = 'rgba(65, 131, 196, 0.6)',
            line = list(color = 'rgba(46, 92, 137, 1.0)', width = 3))) %>%
  add_trace(x = ~`Not Forked`, name = 'Not Forked',
            marker = list(
              color = 'rgba(102, 102, 102, 0.6)',
              line = list(color = 'rgba(51, 51, 51, 1.0)', width = 3))) %>%
  add_trace(x = gists_count, y = "Gists",
            marker = list(
              color = 'rgba(102, 102, 102, 0.6)',
              line = list(color = 'rgba(51, 51, 51, 1.0)', width = 3))) %>%
  add_trace(x = proj_count, y = "Projects",
            marker = list(
              color = 'rgba(102, 102, 102, 0.6)',
              line = list(color = 'rgba(51, 51, 51, 1.0)', width = 3))) %>%
  add_trace(x = pages_count, y = "Pages",
            marker = list(
              color = 'rgba(102, 102, 102, 0.6)',
              line = list(color = 'rgba(51, 51, 51, 1.0)', width = 3))) %>%
  layout(legend = list(orientation = "h", yanchor = "top", y=1, xanchor="center", x=.5), barmode = 'stack',
         xaxis = list(title = "Count"),
         yaxis = list(title =""))




# Time bar chart...

time_chart <- sum_table %>% group_by(Feature) %>% summarize(count = sum(count), first_created=min(first_created), last_updated = max(last_updated)) %>% ggplot(aes(x=first_created, xend=last_updated, y=Feature)) + geom_dumbbell(size=1.3, color="#007FA9",colour_x = "#007FA9",colour_xend = "#007FA9",size_x = 5,size_xend = 5,dot_guide=FALSE, dot_guide_size=0.25)+ 
  labs(x=NULL, y=NULL,
       title="GitHub Feature Usage",
       subtitle="From first creation to latest update",
       caption="Source: GitHub API") + 
  theme_classic() + 
  geom_stripes(odd = "#11111111", even = "#00000000") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        panel.grid.major.x = element_line(color = "gray", size = .5),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank()
  ) + coord_cartesian(xlim = c(as.Date("2008-01-01"), as.Date("2021-12-31")))

# A plotly chart...

mikeshout %>% filter(forked=="Forked_No") %>% 
  plot_ly(x=~count, y=~Feature, type="bar", orientation="h", name="Not Forked",
          marker = list(color = 'rgba(65, 131, 196, 0.6)', line = list(color = 'rgba(46, 92, 196, 1.0)', width = 3))) %>% 
  add_trace(x = mikeshout %>% filter(forked=="Forked_Yes"), name = 'Forked',
            marker = list(color = 'rgba(102, 102, 102, 0.6)', line = list(color = 'rgba(102, 102, 102, 1.0)', width = 3))) %>% 
  layout(barmode = 'stack',
         xaxis = list(title = "Count"),
         yaxis = list(title ="Features"),
         title="Features Used")
