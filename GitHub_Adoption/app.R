#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(shiny.semantic)
library(ggalt)
library(ggforestplot)

source("gh_caller.R")


# Define UI for application that draws a histogram
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
    
    div(class = "ui doubling grid raised segment",
        div(class = "eight wide column",
            textInput("user_name", label = "Enter a GitHub username", type = "text", 
                      placeholder = "MikeShout",
                      ),

            action_button("Go", "Go!", class = "green")
        ),
        div(class = "eight wide column",
            p(icon("idea"), "Examples to try:", 
              list_container(list(list(header = "hadley"), 
                                  list(header = "AnotherPerson"), 
                                  list(header = "GitHub")), 
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
        textOutput("recos"),
        tableOutput("test")
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
                 title="GitHub Feature Usage",
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
            "A bunch of text will go here eventually..."
    })
        output$user <- renderText({
            as.character(suppliedUser()[suppliedUser()$Feature=="user", "forked"])    
        })
    
    output$test <- renderTable({
        suppliedUser()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
