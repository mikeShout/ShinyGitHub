#
# GitHub Feature Adoption
# How to get the most out of your GitHub account
# This is a shiny app dashboard that summarizes GitHub feature adoption for GitHub 
# users with recommendations to get more out of their service.
#

library(shiny)
library(plotly)
library(tidyverse)
library(shiny.semantic)
library(ggalt)
library(ggforestplot)
library(shinybusy)

source("gh_caller.R")

ui <- semanticPage(
    
    title = "GitHub Adoption Dashboard",
    
    div(class = "ui middle aligned doubling grid",
        div(class = "three wide column",
            div(class = "image", img(src = "https://octodex.github.com/images/Fintechtocat.png", height="125", width="125"))),
        div(class = "thirteen wide column",
            h2(class = "ui header", 
               div(class = "content",style = "color: #333333", "Github Feature Adoption",
                   div(class = "sub header", style = "color: #999999", "How to get the most out of your GitHub account"))))
    ),
    
    div(class = "ui triple grid raised segment",
        div(class = "seven wide column",
            textInput("user_name", label = "Enter a GitHub username", type = "text", 
                      placeholder = "MikeShout",
                      ),

            action_button("Go", "Go!", class = "green")
        ),
        
        div(class = "two wide column",
            add_busy_gif(src = "http://nathanlaredo.com/github_files/octocat-spinner-128.gif", height = 50, width = 50)
        ),
        
        div(class = "seven wide column",
            p(icon("idea"), "Examples to try:", 
              list_container(list(list(header = "hadley"), 
                                  list(header = "bodil"), 
                                  list(header = "juliasilge")), 
                             is_divided = FALSE)
              )
        )
    ),
    
    h2(class = "ui header", 
       div(class = "content",textOutput("user"))),
    
    div(class = "ui four column stackable grid container",
        div(class = "column",
            div(class = "ui icon small blue message",
                icon("github"),
                div(class = "content",
                    div(class = "header", textOutput("rCount")),
                    p("Repositories")
                )
            ),
            h6("* Last 100 retreived (for now)")
        ),
        
        div(class = "column",
            div(class = "ui icon small blue message",
                icon("bolt"),
                div(class = "content",
                    div(class = "header", textOutput("gCount")),
                    p("Gists")
                )
            )
        ),
        div(class = "column",
            div(class = "ui icon small blue message",
                icon("project diagram"),
                div(class = "content",
                    div(class = "header", textOutput("prCount")),
                    p("Projects")
                )
            )
        ),
        
        div(class = "column",
            div(class = "ui icon small blue message",
                icon("blog"),
                div(class = "content",
                    div(class = "header", textOutput("pCount")),
                    p("Pages")
                )
            )
        
        )
    ),
    
    div(class = "ui two column stackable grid container",
        div(class = "column",
            plotOutput("timePlot")
                ),
        div(class = "column",
            plotOutput("countPlot") #Chart goes here
        )
    ),
    
    div(class = "ui horizontal divider", icon("user ninja"), 
        "Recommendations"),
    br(), br(),    
    textOutput("recos"),
    
    div(class = "ui horizontal divider", icon("info circle icon"), 
        "About"),
    br(),
    p(tags$b("GitHub"), " offers a fantastic service extending git where users can organize, collaborate, and share their code. Not to mention it is free and GitHub continues to develop new features. Users can share gists (useful code snippets), publish a page/blog, and create projects."),
    p("This Shiny app is a simple", tags$b(" dashboard "), "analyzing a given user's usage of their GitHub service. Based on that, it recommends others features that may be valuable for that user."),
    br(),
    p("This app was created by ", tags$b(" Mike Wehinger")),
    
    tags$a(href="https://twitter.com/mwehinger", "@mwehinger"),
    br(),
    tags$a(href="http://www.myshout.io", "myshout.io"),
    br(), br(),
    p("Of course you can find this code on GitHub ",tags$a(href="https://github.com/mikeShout/ShinyGitHub", "here")),
    br(),
    p(tags$i("Thank you for visiting"))
        
)


server <- function(input, output, session) {

        suppliedUser <- eventReactive(input$Go, {
        uName = trimws(input$user_name)
        
        if(nchar(uName)<1 || str_count(uName, " ")>0) {

            #Create a 'blank' table..
            sum_table <- as_tibble(rbind(
                cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Repos"), 
                cbind(forked="Forked_Yes", count = 0, first_created = "", last_updated = "", Feature = "Pages"),
                cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Gists"),
                cbind(forked="Forked_No", count = 0, first_created = "", last_updated = "", Feature = "Proj"),
                cbind(forked="Please enter a valid GitHub username (no spaces)!", count = 0, first_created = "", last_updated = "", Feature = "user"))) %>%
                mutate(first_created= as.Date(first_created), last_updated = as.Date(last_updated), count = as.numeric(count))
            
            
        } else {
            #paste(uName, "is the user!!")
            sum_data(uName)
            
        }
        
        
        
    })
    
    output$rCount <- renderText({
        #sum(suppliedUser()[suppliedUser$Feature=="Repos"]$count)
        sum(suppliedUser()[suppliedUser()$Feature=="Repos", "count"])
    })
    
    output$gCount <- renderText({
        #sum(suppliedUser()[suppliedUser$Feature=="Repos"]$count)
        sum(suppliedUser()[suppliedUser()$Feature=="Gists", "count"])
    })
    
    output$prCount <- renderText({
        #sum(suppliedUser()[suppliedUser$Feature=="Repos"]$count)
        sum(suppliedUser()[suppliedUser()$Feature=="Proj", "count"])
    })
    
    output$pCount <- renderText({
        #sum(suppliedUser()[suppliedUser$Feature=="Repos"]$count)
        sum(suppliedUser()[suppliedUser()$Feature=="Pages", "count"])
    })
    
    output$timePlot <- renderPlot({
        
        suppliedUser() %>% filter(Feature != "user") %>% group_by(Feature) %>% summarize(count = sum(count), first_created=min(first_created), last_updated = max(last_updated)) %>% ggplot(aes(x=first_created, xend=last_updated, y=Feature)) + geom_dumbbell(size=1.3, color="#007FA9",colour_x = "#007FA9",colour_xend = "#007FA9",size_x = 5,size_xend = 5,dot_guide=FALSE, dot_guide_size=0.25)+ 
            labs(x=NULL, y=NULL,
                 title="GitHub Usage Timeline",
                 subtitle="From first creation to latest update",
                 caption="Source: GitHub API") + 
            theme_classic() + 
            geom_stripes(odd = "#11111111", even = "#00000000") +
            theme(plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid.major.x = element_line(color = "gray", size = .5),
                  axis.ticks.x=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.line.x = element_blank()) + 
            coord_cartesian(xlim = c(as.Date("2008-01-01"), as.Date("2021-12-31")))
        
    })
    
    output$countPlot <- renderPlot({
        
        suppliedUser() %>% filter(Feature != "user") %>% mutate(forked = if_else(forked=="Forked_No", "Not Forked", "Forked")) %>% 
            ggplot(aes(x=count,y=Feature)) + 
            geom_bar(aes(fill=forked),stat="identity", color="#333333",position = position_stack(reverse = TRUE)) +
            scale_fill_manual(values = c("Forked" = "#4183C4", "Not Forked" = "#999999")) + 
            labs(x="Count", y="Feature",
                 title="GitHub Feature Usage",
                 subtitle="Count of each feature") + 
            theme_classic() +
            theme(plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid.major.x = element_line(color = "gray", size = .5),
                  axis.ticks.x=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.line.x = element_blank(),
                  legend.title = element_blank()
            )
    })
    
        output$recos <- renderText({
            comment <- paste("User ", as.character(suppliedUser()[suppliedUser()$Feature=="user", "forked"]), sep="")
            comment <- paste(comment, " has ", sum(suppliedUser()[suppliedUser()$Feature=="Repos", "count"]), " repos, ", sum(suppliedUser()[suppliedUser()$Feature=="Repos" & suppliedUser()$forked=="Forked_No", "count"]), " of which are not forked.", sep="")

            if (sum(suppliedUser()[suppliedUser()$Feature=="Repos" & suppliedUser()$forked=="Forked_No", "count"])<2) {
                comment <- paste(comment, " Get more practice and build out a portfolio by creating and sharing more repositories", sep="")
            } else {
                comment <- paste(comment, " That is a solid portfolio, well done.", sep="")
            }
            
            if (sum(suppliedUser()[suppliedUser()$Feature=="Pages", "count"])<1) {
                comment <- paste(comment, " Now you can set up a page to showcase your skills and projects.", sep="")
            } else {
                comment <- paste(comment, " Whats better is that you also have a GitHub page to better articulate your skills and interests.", sep="")
            }
            
            if (sum(suppliedUser()[suppliedUser()$Feature=="Gists", "count"])<1) {
                comment <- paste(comment, " Do you have a snippet of code that you find really useful? Then try sharing it as a gist.", sep="")
            } else {
                comment <- paste(comment, " WIth ", sum(suppliedUser()[suppliedUser()$Feature=="Gists", "count"]), " gists, you are well on your way to sharing best practices and code with others.", sep="")
            }
            
    })
        output$user <- renderText({
            as.character(suppliedUser()[suppliedUser()$Feature=="user", "forked"])
            })
    
#    output$test <- renderTable({
#        suppliedUser()
#    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
