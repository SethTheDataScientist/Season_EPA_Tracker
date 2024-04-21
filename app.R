

theme_reach <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0, 
                                       lineheight = 0.9),
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1.5), family = '' , 
                                       face = 'bold', hjust = -0.05, 
                                       vjust = 1.5, colour = '#3B3B3B'),
      axis.text =         element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14)
    )
}

#regular libraries
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(knitr)
library(stringr)
library(shiny)
library(DT)
library(DescTools)



NetEPA <- read_rds("data/NetEPA.rds") %>% 
  arrange(desc(season)) %>% 
  arrange(week) %>% 
  arrange(home_team) 


EPAData <- read_rds("data/EPA_data.rds") %>% 
    arrange(posteam)

QBname <- EPAData %>%
  group_by(QBname) %>% 
  mutate(LastSeason = max(season),
         SumCount = sum(Count)) %>% 
  filter(SumCount >= 600,
         LastSeason >= 2015)

Types = c("Include all plays", "Exclude QB Rushes", "Exclude Turnovers")

process_data <- function(df, Recency){
  df = df %>% 
    group_by() %>%
    mutate(MaxWeek = case_when(max(Week) > 18 & season >= 2021 ~ 18,
                               max(Week) > 18 ~ 17,
                               T ~ as.double(max(Week))),
           WeeksBack = MaxWeek - Week) %>%
    group_by(posteam) %>%
    mutate(AvgFinEPA.x = mean(FinEPA.x),
           FinEPA.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinEPA.x,
             (FinEPA.x + (WeeksBack * AvgFinEPA.x))/
               (WeeksBack + 1)),
           
           FinEPA.x = if_else(Recency == "None",  mean(FinEPA.x), mean(FinEPA.xAdj)),
           
           AvgFinPassEPA.x = mean(FinPassEPA.x),
           FinPassEPA.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinPassEPA.x,
             (FinPassEPA.x + (WeeksBack * AvgFinPassEPA.x))/
               (WeeksBack + 1)),
           
           FinPassEPA.x = if_else(Recency == "None",  mean(FinPassEPA.x), mean(FinPassEPA.xAdj)),
           
           AvgFinRushEPA.x = mean(FinRushEPA.x),
           FinRushEPA.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinRushEPA.x,
             (FinRushEPA.x + (WeeksBack * AvgFinRushEPA.x))/
               (WeeksBack + 1)),
           
           FinRushEPA.x = if_else(Recency == "None",  mean(FinRushEPA.x), mean(FinRushEPA.xAdj)),
           
           AvgFinEPA.y = mean(FinEPA.y),
           FinEPA.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinEPA.y,
             (FinEPA.y + (WeeksBack * AvgFinEPA.y))/
               (WeeksBack + 1)),
           
           FinEPA.y = if_else(Recency == "None",  mean(FinEPA.y), mean(FinEPA.yAdj)),
           
           
           AvgFinPassEPA.y = mean(FinPassEPA.y),
           FinPassEPA.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinPassEPA.y,
             (FinPassEPA.y + (WeeksBack * AvgFinPassEPA.y))/
               (WeeksBack + 1)),
           
           FinPassEPA.y = if_else(Recency == "None",  mean(FinPassEPA.y), mean(FinPassEPA.yAdj)),
           
           AvgFinRushEPA.y = mean(FinRushEPA.y),
           FinRushEPA.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinRushEPA.y,
             (FinRushEPA.y + (WeeksBack * AvgFinRushEPA.y))/
               (WeeksBack + 1)),
           
           FinRushEPA.y = if_else(Recency == "None",  mean(FinRushEPA.y), mean(FinRushEPA.yAdj)),
           
           AvgFinSuccessRate.x = mean(FinSuccessRate.x),
           FinSuccessRate.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinSuccessRate.x,
             (FinSuccessRate.x + (WeeksBack * AvgFinSuccessRate.x))/
               (WeeksBack + 1)),
           
           FinSuccessRate.x = if_else(Recency == "None",  mean(FinSuccessRate.x), mean(FinSuccessRate.xAdj)),
           AvgFinPassSuccessRate.x = mean(FinPassSuccessRate.x),
           FinPassSuccessRate.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinPassSuccessRate.x,
             (FinPassSuccessRate.x + (WeeksBack * AvgFinPassSuccessRate.x))/
               (WeeksBack + 1)),
           
           FinPassSuccessRate.x = if_else(Recency == "None",  mean(FinPassSuccessRate.x), mean(FinPassSuccessRate.xAdj)),
           
           AvgFinRushSuccessRate.x = mean(FinRushSuccessRate.x),
           FinRushSuccessRate.xAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinRushSuccessRate.x,
             (FinRushSuccessRate.x + (WeeksBack * AvgFinRushSuccessRate.x))/
               (WeeksBack + 1)),
           
           FinRushSuccessRate.x = if_else(Recency == "None",  mean(FinRushSuccessRate.x), mean(FinRushSuccessRate.xAdj)),
           
           
           AvgFinSuccessRate.y = mean(FinSuccessRate.y),
           FinSuccessRate.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinSuccessRate.y,
             (FinSuccessRate.y + (WeeksBack * AvgFinSuccessRate.y))/
               (WeeksBack + 1)),
           
           FinSuccessRate.y = if_else(Recency == "None",  mean(FinSuccessRate.y), mean(FinSuccessRate.yAdj)),
           AvgFinPassSuccessRate.y = mean(FinPassSuccessRate.y),
           FinPassSuccessRate.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinPassSuccessRate.y,
             (FinPassSuccessRate.y + (WeeksBack * AvgFinPassSuccessRate.y))/
               (WeeksBack + 1)),
           
           FinPassSuccessRate.y = if_else(Recency == "None",  mean(FinPassSuccessRate.y), mean(FinPassSuccessRate.yAdj)),
           
           AvgFinRushSuccessRate.y = mean(FinRushSuccessRate.y),
           FinRushSuccessRate.yAdj = if_else(
             (season >= 2021 & Week > 18) | 
               (season < 2021 & Week > 17), FinRushSuccessRate.y,
             (FinRushSuccessRate.y + (WeeksBack * AvgFinRushSuccessRate.y))/
               (WeeksBack + 1)),
           
           FinRushSuccessRate.y = if_else(Recency == "None",  mean(FinRushSuccessRate.y), mean(FinRushSuccessRate.yAdj)),
           
    ) %>% 
    slice_tail(n = 1)
}

add_slope_lines <- function(plot, slope, intercept_range, linetype = "dashed", alpha = 0.5) {
  intercepts <- seq(from = intercept_range[1], to = intercept_range[2], by = 0.05)
  
  for (intercept in intercepts) {
    plot <- plot + geom_abline(slope = slope, intercept = intercept, linetype = linetype, alpha = alpha)
  }
  
  return(plot)
}



slope <- -2
intercept_range <- c(-0.75, 0.75)


# Define UI for application that draws a histogram
ui <- fluidPage(

    
    
    titlePanel("Opponent & WP Adjusted Season EPA Plots"),
    
    fluidRow( 
        tabsetPanel(
          tabPanel("Intro Page",
                   fluidRow(
                     column(width = 10, offset = 1,
                            helpText(HTML("This shiny app is designed for viewing how teams are doing from a production standpoint. The methodology is using Expected points added adjusted for team strength at time of matchup. It breaks down into season trends, team tiers, offense and defense splits, and net success rate.</br>
                        </br>
                        
                                   To check out my other shiny apps, follow one of the following links.</br>
                                   </br>

        <a href='https://seththedatascientist.shinyapps.io/QB_Bayesian_Updating/'>Bayesian Updating of composite metrics for Quarterback play for NFL and College QBs</a></br>

        This shiny app displays both college and pro QBs in two composite metrics that show not only their relative playstyles, but also how those values change over their careers. These values have a high correlation from college to pro for predicting playstyle once in the NFL</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/General_Manager_Toolbox/'>General Manager's Toolbox: A collection of tools to help analyze an NFL Team's Offseason moves.</a></br>

        This shiny app goes over a handful of useful data points that I have found very helpful for analyzing a team's offseason moves, including draft trade calculators (with some linear programming to try and ensure extra value by comparing the Jimmy Johnson trade chart to the Wins Above Replacement values), created metrics to analyze draft prospects in further detail, and team breakdowns of their effective cap and team structure over the coming years.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Offense_And_Defense_Metrics/'>Collection of Offense & Defense efficiency and playstyle metrics for the NFL</a></br>

        This shiny app includes a number of metrics used to understand Offense and Defense in further detail including down conversion values of how often you are allowing a first down or touchdown based on what down it currently is, explosive play rates, big throws and misses by quarterbacks, and more. Most metrics include a feature to isolate a playcaller's history of that metric across all teams they were the playcaller for.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Season_EPA_Tracker/'>Timeline of play measuring efficiency metrics, team season-long rankings, and team tier plots</a></br>

        This shiny app includes many iterations of breaking down expected points added (EPA) adjusted based on opponent strength and situation. Season long graphs to see individual team or starting quarterback trends, team plots for offense and defense including splits for passing and rushing, and a metric for team strength based on the relative margin of domination during the game as well as opponent strength.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/WAR_positiongroup/'>Position Wins Above Replacement graphs by team and Watch Index</a></br>

        This shiny app shows Wins Above Replacement (WAR) values and plots for both college and pro broken down into many useful facets like by position group, team, and individual player. Includes receiver custom metrics plotted to compare players both within college and pro, as well as a customizable Watch Index which assigns a values based on relative values of excitement and closeness.</br>
        </br>
                  
                                       To check some of my other work, check out my <a href='https://twitter.com/SethDataScience'>Twitter</a>, <a href='https://www.linkedin.com/in/sethlanza/'>Linkedin</a>, or <a href='https://sites.google.com/view/seth-lanza-portfolio'>Portfolio</a>")),
                     )
                   ),
                   fluidRow(
                     column(width = 10, offset = 1,
                            textOutput("lastDeploymentTime")
                     )
                   )
          ),
        tabPanel("Season EPA Tracker", 
                 # Sidebar with a slider input for number of bins 
                 fluidRow(
                     column(width = 5,
                            sliderInput(inputId = "seasons1",
                                        label = "Team 1 Season Selection",
                                        min = min(EPAData$season),
                                        max = max(EPAData$season),
                                        value = c(2022,2023),
                                        step = 1,
                                        round = T,
                                        sep = ""),
                            
                            selectizeInput(inputId = "team1",
                                           label = "Team 1 Namecode",
                                           choices = unique(EPAData$posteam),
                                           selected = "BUF"),
                            
                            selectizeInput(inputId = "QB1",
                                           label = "Starting QB Filter (Ignores Above Selections)",
                                           choices = c("", sort(unique(QBname$QBname))))
                            ),
                     
                     column(width = 5,
                            sliderInput(inputId = "seasons2",
                                        label = "Team 2 Season Selection",
                                        min = min(EPAData$season),
                                        max = max(EPAData$season),
                                        value = c(2022,2023),
                                        step = 1,
                                        round = T,
                                        sep = ""),
                            
                            selectizeInput(inputId = "team2",
                                           label = "Team 2 Namecode",
                                           choices = unique(EPAData$posteam),
                                           selected = "KC"),
                            
                            selectInput(inputId = "QB2",
                                        label = "Starting QB Filter (Ignores Above Selections)",
                                        choices = c("", sort(unique(QBname$QBname))))
                            ),
                     
                     column(width = 4, submitButton())  
                     
                     ),
                 
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("Team1Plot", height = 700),
                             width = 10, offset = 1
                             )
                         ), 
                     
                     fluidRow(
                         column(
                            plotOutput("Team2Plot", height = 700),
                            width = 10, offset = 1
                            )
                        )
                 ),
        
        
        
        tabPanel("Season Team Tiers", 
                 
                 fluidRow(
                     column(width = 3,
                            checkboxGroupInput(inputId = "seasonstier",
                                        label = "Season(s) Selection",
                                        choices = unique(EPAData$season),
                                        selected = c(2023),
                                        inline = T)
                            ),
                     
                     
                     column(width = 3,
                            checkboxGroupInput(inputId = "weektier",
                                               label = "Week(s) Selection",
                                               choices = sort(unique(EPAData$Week)),
                                               selected = c(1:22),
                                               inline = T)
                     ),
                     
                     column(width = 3,
                            selectizeInput(inputId = "Typetier",
                                           label = "Filter Selection",
                                           choices =  Types,
                                           selected =  Types[1]), 
                            helpText(HTML("This selection either takes all the data, filters out QB rushes, or filters out plays with Turnovers"))
                     ),
                     
                     column(width = 3,
                            selectizeInput(inputId = "RecentWeek",
                                               label = "Recency Weighting",
                                               choices =  c("None", "Yes"),
                                               selected = "Yes"), 
                            helpText(HTML("Recency Weighting applies a weighting toward the most recent week (up to the playoffs) available with more weight closer to that week. This will also smooth out the numbers and bring them down a bit. If None is selected then it is the average of all week's EPA."))
                     ),
                 ),
                 fluidRow(
                     
                     column(width = 4, submitButton())                     
                 ),
                 
                 
                 
                 # Show a plot of the generated distribution
                 fluidRow(
                     column(
                         plotOutput("TeamTierPlot", height = 700),
                         width = 10, offset = 1
                     ),
                     
                     column(
                       dataTableOutput("Tiertable"),
                       width = 10, offset = 1
                     )
                 )
        ),
        tabPanel("Season Team Offense", 
                     
                     fluidRow(
                         column(width = 3,
                                checkboxGroupInput(inputId = "seasonsoff",
                                                   label = "Season(s) Selection",
                                                   choices = sort(unique(EPAData$season)),
                                                   selected = c(2023),
                                                   inline = T)
                         ),
                         
                         
                         column(width = 3,
                                checkboxGroupInput(inputId = "weekoff",
                                                   label = "Week(s) Selection",
                                                   choices = sort(unique(EPAData$Week)),
                                                   selected = c(1:22),
                                                   inline = T)
                         ),
                         
                         
                         column(width = 3,
                                selectizeInput(inputId = "Typeoff",
                                               label = "Filter Selection",
                                               choices =  Types,
                                               selected =  Types[1]), 
                                helpText(HTML("This selection either takes all the data, filters out QB rushes, or filters out plays with Turnovers"))
                         ),
                         
                         column(width = 3,
                                selectizeInput(inputId = "RecentWeekoff",
                                               label = "Recency Weighting",
                                               choices =  c("None", "Yes"),
                                               selected = "Yes"), 
                                helpText(HTML("Recency Weighting applies a weighting toward the most recent week available with more weight closer to that week. This will also smooth out the numbers and bring them down a bit. If None is selected then it is pure EPA."))
                         ),
                     ),
                 fluidRow(
                         
                         column(width = 4, submitButton())                     ),
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("TeamOffensePlot", height = 700),
                             width = 10, offset = 1
                         ),
                         
                         column(
                           dataTableOutput("Offtable"),
                           width = 10, offset = 1
                         )
                     )
                ),
        tabPanel("Season Team Defense", 
                     
                     fluidRow(
                         column(width = 3,
                                checkboxGroupInput(inputId = "seasonsdef",
                                                   label = "Season(s) Selection",
                                                   choices = sort(unique(EPAData$season)),
                                                   selected = c(2023),
                                                   inline = T)
                         ),
                         
                         
                         column(width = 3,
                                checkboxGroupInput(inputId = "weekdef",
                                                   label = "Week(s) Selection",
                                                   choices = sort(unique(EPAData$Week)),
                                                   selected = c(1:22),
                                                   inline = T)
                         ),
                         column(width = 3,
                                selectizeInput(inputId = "Typedef",
                                                   label = "Filter Selection",
                                                   choices = Types,
                                                   selected = Types[1]),
                                helpText(HTML("This selection either takes all the data, filters out QB rushes, or filters out plays with Turnovers"))
                         ),
                         
                         column(width = 3,
                                selectizeInput(inputId = "RecentWeekdef",
                                               label = "Recency Weighting",
                                               choices =  c("None", "Yes"),
                                               selected = "Yes"), 
                                helpText(HTML("Recency Weighting applies a weighting toward the most recent week available with more weight closer to that week. This will also smooth out the numbers and bring them down a bit. If None is selected then it is pure EPA."))
                         ),
                     ),
                 fluidRow(
                         
                         column(width = 4, submitButton())
                     ),
                     
                     
                     
                     # Show a plot of the generated distribution
                     fluidRow(
                         column(
                             plotOutput("TeamDefensePlot", height = 700),
                             width = 10, offset = 1
                         ),
                         
                         column(
                           dataTableOutput("Deftable"),
                           width = 10, offset = 1
                         )
                     )
            ),
        tabPanel("Net Success Rate", 
                 
                 fluidRow(
                   column(width = 3,
                          selectInput(inputId = "seasonNet",
                                     label = "Season Selection",
                                     choices = sort(unique(NetEPA$season)),
                                     selected = 2023)
                   ),
                   
                   
                   column(width = 3,
                          selectInput(inputId = "weekNet",
                                             label = "Week Selection",
                                             choices = unique(NetEPA$week),
                                             selected = max(NetEPA$week[NetEPA$season == 2023]))
                   ),
                   column(width = 3,
                          selectInput(inputId = "teamNet",
                                      label = "Team Selection",
                                      choices = unique(NetEPA$home_team),
                                      selected = "BUF")
                   ),
                   
                   
                   column(width = 3, submitButton())
                 ),
                 
                 
                 
                 # Show a plot of the generated distribution
                 fluidRow(
                   column(
                     plotOutput("NetPlot", height = 700),
                     width = 10, offset = 1
                   ),
                   column(
                     plotOutput("NetPlot2", height = 700),
                     width = 10, offset = 1
                   )
              )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  getLastDeploymentTime <- function() {
    timestamp <- tryCatch(
      readLines("deployment_timestamp.txt"),
      error = function(e) NA
    )
    if (!is.na(timestamp)) {
      as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA
    }
  }
  
  # Display the last deployment time
  output$lastDeploymentTime <- renderText({
    lastDeploymentTime <- getLastDeploymentTime()
    if (!is.na(lastDeploymentTime)) {
      paste("Last Deployment Time: ", format(lastDeploymentTime, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Deployment time not available."
    }
  })
  
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")

    output$Team1Plot <- renderPlot({
        
        EPAData %>% 
            group_by(QBname) %>% 
            mutate(QBTime = seq(1,n(),1),
                   QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
                   QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5)),
                   QBByeWeek = case_when(is.na(ByeWeek) == 0 ~ QBTime-0.5))%>% 
            group_by() %>% 
            filter(Type == "Include all plays",
              between(season, 
                           case_when(input$QB1 == "" ~ input$seasons1[1],
                                     T ~ as.integer(0)),
                           case_when(input$QB1 == "" ~ input$seasons1[2],
                                    T ~ as.integer(3000))), 
                   posteam == case_when(input$QB1 == "" ~ input$team1,
                                        T ~ posteam),
                   QBname == case_when(input$QB1 == "" ~ QBname,
                                       T ~ input$QB1)) %>% 
            ggplot(aes(x = case_when(input$QB1 == "" ~ Time,
                                     T ~ as.integer(QBTime)), y = FinEPA.x))+
            geom_point(color = "red")+
            geom_line(color = "red", group = 1)+
            geom_point(aes(y = -1*(FinEPA.y), color = "blue"))+
            geom_line(aes(y = -1*(FinEPA.y), color = "blue"), group = 1)+
            geom_point(aes(y = EarlyDown, color = "orange"))+
            geom_line(aes(y = EarlyDown, color = "orange"), group = 1)+
            scale_color_identity(aesthetics = "color")+
            geom_text(aes(y = -0.6, label = Opponent, color = Win))+
            geom_text(aes(y = -0.52, label = "Alt QB", 
                          alpha = Starter, color = "red"))+
            geom_hline(yintercept = head(EPAData$AvgEarly, 1), color = "orange")+
            geom_hline(yintercept = 0, alpha = 0.75)+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ ByeWeek,
                                                  T ~ QBByeWeek),
                           alpha = ByeWeekAlpha,
                           color = "red"), linetype = "dashed")+
            scale_alpha_identity()+
            theme_reach()+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ Season_Change_variable,
                                                  T ~ QBSeason_Change_variable),
                           color = "black"))+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ Playoffs_variable,
                                                  T ~ QBPlayoffs_variable),
                           color = "purple"))+
            geom_smooth(se = F, color = "red")+
            geom_smooth(aes(y = -1*FinEPA.y), se = F, color = "blue")+
            geom_smooth(aes(y = EarlyDown), se = F, color = "orange")+
            labs(title = paste0(
                case_when(input$QB1 == "" ~ input$seasons1[1],
                          T ~ as.integer(
                              min(EPAData$season[EPAData$QBname == input$QB1]))),
                "-", 
                case_when(input$QB1 == "" ~ input$seasons1[2],
                          T ~ as.integer(
                              max(EPAData$season[EPAData$QBname == input$QB1]))),
                " ",
                case_when(input$QB1 != "" ~ EPAData$posteam[EPAData$QBname == input$QB1],
                          T ~ ""),
                case_when(input$QB1 == "" ~  input$team1,
                          T ~ ""),
                " ", "Season(s)"),
                 subtitle = "Red is Opponent & WP Adjusted Offensive EPA, Blue is inverted Opponent & WP Adjusted Defensive EPA (higher = better),
           Orange is Early Down Passing Rate with League Average as line, Vertical Lines to Represent Season Changes and Playoffs (Purple)",
                 y = "EPA or (Early Down Passing Rate)",
                x = "Number of Games",
                 caption = "@SethDataScience")
        
    })
    
    output$Team2Plot <- renderPlot({
        
        EPAData %>% 
            group_by(QBname) %>% 
            mutate(QBTime = seq(1,n(),1),
                   QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
                   QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5)),
                   QBByeWeek = case_when(is.na(ByeWeek) == 0 ~ QBTime-0.5))%>% 
            group_by() %>% 
            filter(Type == "Include all plays",
                   between(season, 
                           case_when(input$QB2 == "" ~ input$seasons2[1],
                                     T ~ as.integer(0)),
                           case_when(input$QB2 == "" ~ input$seasons2[2],
                                     T ~ as.integer(3000))), 
                   posteam == case_when(input$QB2 == "" ~ input$team2,
                                        T ~ posteam),
                   QBname == case_when(input$QB2 == "" ~ QBname,
                                       T ~ input$QB2)) %>% 
            ggplot(aes(x = case_when(input$QB1 == "" ~ Time,
                                     T ~ as.integer(QBTime)), y = FinEPA.x))+
            geom_point(color = "red")+
            geom_line(color = "red", group = 1)+
            geom_point(aes(y = -1*(FinEPA.y), color = "blue"))+
            geom_line(aes(y = -1*(FinEPA.y), color = "blue"), group = 1)+
            geom_point(aes(y = EarlyDown, color = "orange"))+
            geom_line(aes(y = EarlyDown, color = "orange"), group = 1)+
            scale_color_identity(aesthetics = "color")+
            geom_text(aes(y = -0.6, label = Opponent, color = Win))+
            geom_text(aes(y = -0.52, label = "Alt QB", 
                          alpha = Starter, color = "red"))+
            geom_hline(yintercept = head(EPAData$AvgEarly, 1), color = "orange")+
            geom_hline(yintercept = 0, alpha = 0.75)+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ ByeWeek,
                                                  T ~ QBByeWeek),
                           alpha = ByeWeekAlpha,
                           color = "red"), linetype = "dashed")+
            scale_alpha_identity()+
            theme_reach()+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ Season_Change_variable,
                                                  T ~ QBSeason_Change_variable),
                           color = "black"))+
            geom_vline(aes(xintercept = case_when(input$QB1 == "" ~ Playoffs_variable,
                                                  T ~ QBPlayoffs_variable),
                           color = "purple"))+
            geom_smooth(se = F, color = "red")+
            geom_smooth(aes(y = -1*FinEPA.y), se = F, color = "blue")+
            geom_smooth(aes(y = EarlyDown), se = F, color = "orange")+
            labs(title = paste0(
                case_when(input$QB2 == "" ~ input$seasons2[1],
                          T ~ as.integer(
                              min(EPAData$season[EPAData$QBname == input$QB2]))),
                "-", 
                case_when(input$QB2 == "" ~ input$seasons2[2],
                          T ~ as.integer(
                              max(EPAData$season[EPAData$QBname == input$QB2]))),
                " ",
                case_when(input$QB2 != "" ~ EPAData$posteam[EPAData$QBname == input$QB2],
                          T ~ ""),
                case_when(input$QB2 == "" ~  input$team2,
                          T ~ ""),
                " ", "Season(s)"),
                subtitle = "Red is Opponent & WP Adjusted Offensive EPA, Blue is inverted Opponent & WP Adjusted Defensive EPA (higher = better),
           Orange is Early Down Passing Rate with League Average as line, Vertical Lines to Represent Season Changes and Playoffs (Purple)",
                y = "EPA or (Early Down Passing Rate)",
                x = "Number of Games",
                caption = "@SethDataScience")
        
    })
    
    
    output$TeamTierPlot <- renderPlot({
        
     
      
        FilteredEPAData <- EPAData %>% 
            filter(Week %in% input$weektier,
                season %in% input$seasonstier,
                Type == input$Typetier) 
        
        TierEPAData <- process_data(FilteredEPAData,
                                    input$RecentWeek)
        
            base_plot <- ggplot(TierEPAData,
                   aes(x = FinEPA.x, y = FinEPA.y))+
            geom_image(aes(image = url))+
            geom_hline(aes(yintercept = 0))+
            geom_vline(aes(xintercept = 0))+
            scale_color_identity(aesthetics = "color")+
            scale_y_reverse()+
            theme_reach()+
            labs(title = paste0(
                min(input$seasonstier), "-", max(input$seasonstier),
                " Team Tiers of Opponent & WP Adjusted EPA/Play"),
                subtitle = paste0("Adjusts raw EPA/P by opponent faced and pre-play WP, Slope of dashed lines is -2 to represent defensive Tiers with coverage being twice as valuable as run defense,
    Axes flipped as negative EPA/P is good from the defense's perspective, Filtered to ",input$Typetier),
                x = "Offensive EPA/Play",
                y = "Defensive EPA/Play",
                caption = "Seasons listed in Title are Min/Max Seasons selected. @SethDataScience")
            
            add_slope_lines(base_plot, slope, intercept_range)
        
    })
    
   
    
    output$Tiertable = DT::renderDataTable({
      
      
      TierFilter <- EPAData %>% 
        filter(season %in% input$seasonstier,
               Week %in% input$weektier,
               Type == input$Typetier) 
      
      TierFilter <- process_data(TierFilter,
                      input$RecentWeek) %>% 
        select(season, posteam, FinPassEPA.x, FinRushEPA.x, FinEPA.x, FinPassSuccessRate.x, FinRushSuccessRate.x, FinSuccessRate.x,
               FinPassEPA.y, FinRushEPA.y, FinEPA.y, FinPassSuccessRate.y, FinRushSuccessRate.y, FinSuccessRate.y) %>% 
        group_by(posteam) %>% 
        summarise(OffEPA = round(mean(FinEPA.x), 3),
                  OffSuccessRate = round(round(mean(FinSuccessRate.x), 4)*100,2),
                  DefEPA = round(mean(FinEPA.y), 3),
                  DefSuccessRate = round(round(mean(FinSuccessRate.y), 4)*100, 2)) %>%   
        group_by() %>% 
        arrange(desc(OffEPA)) %>% 
        mutate(Rank = seq(1, n(), 1)) %>% 
        select(Rank, everything())
      
      
      
      OffEPAbrks <- quantile(TierFilter$OffEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      OffEPAbrksy <- round(seq(255, 40, length.out = length(OffEPAbrks) + 1), 0)
      OffEPAbrksclrs <- paste0("rgb(", OffEPAbrksy, "," , 255-OffEPAbrksy , ",", 0, ")")
      
      OffSuccessRatebrks <- quantile(TierFilter$OffSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      OffSuccessRatebrksy <- round(seq(255, 40, length.out = length(OffSuccessRatebrks) + 1), 0)
      OffSuccessRatebrksclrs <- paste0("rgb(", OffSuccessRatebrksy, "," , 255-OffSuccessRatebrksy , ",", 0, ")")
      
      DefEPAbrks <- quantile(TierFilter$DefEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      DefEPAbrksy <- round(seq(255, 40, length.out = length(DefEPAbrks) + 1), 0)
      DefEPAbrksclrs <- paste0("rgb(", 255-DefEPAbrksy, "," , DefEPAbrksy , ",", 0, ")")
      
      DefSuccessRatebrks <- quantile(TierFilter$DefSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      DefSuccessRatebrksy <- round(seq(255, 40, length.out = length(DefSuccessRatebrks) + 1), 0)
      DefSuccessRatebrksclrs <- paste0("rgb(", 255-DefSuccessRatebrksy, "," , DefSuccessRatebrksy , ",", 0, ")")
      
      
      datatable(TierFilter, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')), 
                callback = JS(js))%>%
        formatStyle("OffEPA", 
                    backgroundColor = styleInterval(OffEPAbrks,OffEPAbrksclrs)
        )%>%
        formatStyle("OffSuccessRate", 
                    backgroundColor = styleInterval(OffSuccessRatebrks,OffSuccessRatebrksclrs)
        )%>%
        formatStyle("DefEPA", 
                    backgroundColor = styleInterval(DefEPAbrks,DefEPAbrksclrs)
        )%>%
        formatStyle("DefSuccessRate", 
                    backgroundColor = styleInterval(DefSuccessRatebrks,DefSuccessRatebrksclrs)
        )
      
      
    })
    
    output$TeamOffensePlot <- renderPlot({
        
        FilteredEPAData <-  EPAData %>% 
            filter(Week %in% input$weekoff,
                   season %in% input$seasonsoff,
                   Type == input$Typeoff)
        
        OffEPAData <- process_data(FilteredEPAData,
                                   input$RecentWeekoff)
        
        
            base_plot <- ggplot(OffEPAData,
                   aes(x = FinPassEPA.x, y = FinRushEPA.x))+
            geom_image(aes(image = url))+
            geom_hline(aes(yintercept = 0))+
            geom_vline(aes(xintercept = 0))+
            scale_color_identity(aesthetics = "color")+
            theme_reach()+
            labs(title = paste0(
                min(input$seasonsoff), "-", max(input$seasonsoff),
                " Team Offensive Tiers of Opponent & WP Adjusted EPA/Play"),
                subtitle =paste0("Adjusts raw EPA/P by opponent faced and pre-play WP, Slope of dashed lines is -2 to represent defensive Tiers with coverage being twice as valuable as run defense,
    Axes flipped as negative EPA/P is good from the defense's perspective, Filtered to ",input$Typeoff),
                x = "Offensive Passing EPA/Play",
                y = "Offensive Rushing EPA/Play",
                caption = "Seasons listed in Title are Min/Max Seasons selected. @SethDataScience")
            
            add_slope_lines(base_plot, slope, intercept_range)
        
    })
    
    
    output$Offtable = DT::renderDataTable({
      
      FilteredEPAData <-  EPAData %>% 
        filter(Week %in% input$weekoff,
               season %in% input$seasonsoff,
               Type == input$Typeoff)
      
      OffFilter <- process_data(FilteredEPAData,
                                 input$RecentWeekoff) %>% 
        select(season, posteam, FinPassEPA.x, FinRushEPA.x, FinEPA.x, FinPassSuccessRate.x, FinRushSuccessRate.x, FinSuccessRate.x,
               FinPassEPA.y, FinRushEPA.y, FinEPA.y, FinPassSuccessRate.y, FinRushSuccessRate.y, FinSuccessRate.y) %>% 
        group_by(posteam) %>% 
        summarise(OffEPA = round(mean(FinEPA.x), 3),
                  OffSuccessRate = round(round(mean(FinSuccessRate.x), 4)*100,2),
                  PassEPA = round(mean(FinPassEPA.x), 3),
                  PassSuccessRate = round(round(mean(FinPassSuccessRate.x), 4)*100,2),
                  RushEPA = round(mean(FinRushEPA.x), 3),
                  RushSuccessRate = round(round(mean(FinRushSuccessRate.x), 4)*100,2)) %>%         group_by() %>% 
        arrange(desc(OffEPA)) %>% 
        mutate(Rank = seq(1, n(), 1)) %>% 
        select(Rank, everything())
      
      
      
      
      OffEPAbrks <- quantile(OffFilter$OffEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      OffEPAbrksy <- round(seq(255, 40, length.out = length(OffEPAbrks) + 1), 0)
      OffEPAbrksclrs <- paste0("rgb(", OffEPAbrksy, "," , 255-OffEPAbrksy , ",", 0, ")")
      
      OffSuccessRatebrks <- quantile(OffFilter$OffSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      OffSuccessRatebrksy <- round(seq(255, 40, length.out = length(OffSuccessRatebrks) + 1), 0)
      OffSuccessRatebrksclrs <- paste0("rgb(", OffSuccessRatebrksy, "," , 255-OffSuccessRatebrksy , ",", 0, ")")
      
      PassEPAbrks <- quantile(OffFilter$PassEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      PassEPAbrksy <- round(seq(255, 40, length.out = length(PassEPAbrks) + 1), 0)
      PassEPAbrksclrs <- paste0("rgb(", PassEPAbrksy, "," , 255-PassEPAbrksy , ",", 0, ")")
      
      PassSuccessRatebrks <- quantile(OffFilter$PassSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      PassSuccessRatebrksy <- round(seq(255, 40, length.out = length(PassSuccessRatebrks) + 1), 0)
      PassSuccessRatebrksclrs <- paste0("rgb(", PassSuccessRatebrksy, "," , 255-PassSuccessRatebrksy , ",", 0, ")")
      
      RushEPAbrks <- quantile(OffFilter$RushEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      RushEPAbrksy <- round(seq(255, 40, length.out = length(RushEPAbrks) + 1), 0)
      RushEPAbrksclrs <- paste0("rgb(", RushEPAbrksy, "," , 255-RushEPAbrksy , ",", 0, ")")
      
      RushSuccessRatebrks <- quantile(OffFilter$RushSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      RushSuccessRatebrksy <- round(seq(255, 40, length.out = length(RushSuccessRatebrks) + 1), 0)
      RushSuccessRatebrksclrs <- paste0("rgb(", RushSuccessRatebrksy, "," , 255-RushSuccessRatebrksy , ",", 0, ")")
      
      
      datatable(OffFilter, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')), 
                callback = JS(js))%>%
        formatStyle("OffEPA", 
                    backgroundColor = styleInterval(OffEPAbrks,OffEPAbrksclrs)
        )%>%
        formatStyle("OffSuccessRate", 
                    backgroundColor = styleInterval(OffSuccessRatebrks,OffSuccessRatebrksclrs)
        )%>%
        formatStyle("PassEPA", 
                    backgroundColor = styleInterval(PassEPAbrks,PassEPAbrksclrs)
        )%>%
        formatStyle("PassSuccessRate", 
                    backgroundColor = styleInterval(PassSuccessRatebrks,PassSuccessRatebrksclrs)
        )%>%
        formatStyle("RushEPA", 
                    backgroundColor = styleInterval(RushEPAbrks,RushEPAbrksclrs)
        )%>%
        formatStyle("RushSuccessRate", 
                    backgroundColor = styleInterval(RushSuccessRatebrks,RushSuccessRatebrksclrs)
        )
      
      
    })
    
    output$TeamDefensePlot <- renderPlot({
        
      FilteredEPAData <-  EPAData %>% 
        filter(Week %in% input$weekdef,
               season %in% input$seasonsdef,
               Type == input$Typedef)
      
      DefEPAData <- process_data(FilteredEPAData,
                                 input$RecentWeekdef)
      
      
      base_plot <- ggplot(DefEPAData,
             aes(x = FinPassEPA.y, y = FinRushEPA.y))+
            geom_image(aes(image = url))+
            geom_hline(aes(yintercept = 0))+
            geom_vline(aes(xintercept = 0))+
            scale_color_identity(aesthetics = "color")+
            scale_y_reverse()+
            scale_x_reverse()+
            theme_reach()+
            labs(title = paste0(
                min(input$seasonsdef), "-", max(input$seasonsdef),
                " Team Defensive Tiers of Opponent & WP Adjusted EPA/Play"),
                subtitle = paste0("Adjusts raw EPA/P by opponent faced and pre-play WP, Slope of dashed lines is -2 to represent defensive Tiers with coverage being twice as valuable as run defense,
    Axes flipped as negative EPA/P is good from the defense's perspective, Filtered to ",input$Typedef),
                x = "Defensive Passing EPA/Play (Allowed)",
                y = "Defensive Rushing EPA/Play (Allowed)",
                caption = "Seasons listed in Title are Min/Max Seasons selected. @SethDataScience")
      
      add_slope_lines(base_plot, slope, intercept_range)
        
    })
    
    
    output$Deftable = DT::renderDataTable({
      
      FilteredEPAData <-  EPAData %>% 
        filter(Week %in% input$weekdef,
               season %in% input$seasonsdef,
               Type == input$Typedef)
      
      DefFilter <- process_data(FilteredEPAData,
                                 input$RecentWeekdef)%>% 
        select(season, posteam, FinPassEPA.x, FinRushEPA.x, FinEPA.x, FinPassSuccessRate.x, FinRushSuccessRate.x, FinSuccessRate.x,
               FinPassEPA.y, FinRushEPA.y, FinEPA.y, FinPassSuccessRate.y, FinRushSuccessRate.y, FinSuccessRate.y) %>% 
        group_by(posteam) %>% 
        summarise(DefEPA = round(mean(FinEPA.y), 3),
                  DefSuccessRate = round(round(mean(FinSuccessRate.y), 4)*100,2),
                  PassEPA = round(mean(FinPassEPA.y), 3),
                  PassSuccessRate = round(round(mean(FinPassSuccessRate.y), 4)*100,2),
                  RushEPA = round(mean(FinRushEPA.y), 3),
                  RushSuccessRate = round(round(mean(FinRushSuccessRate.y), 4)*100,2)) %>%         group_by() %>% 
        arrange((DefEPA)) %>% 
        mutate(Rank = seq(1, n(), 1)) %>% 
        select(Rank, everything())
      
      
      
      
      DefEPAbrks <- quantile(DefFilter$DefEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      DefEPAbrksy <- round(seq(255, 40, length.out = length(DefEPAbrks) + 1), 0)
      DefEPAbrksclrs <- paste0("rgb(", 255-DefEPAbrksy, "," , DefEPAbrksy , ",", 0, ")")
      
      DefSuccessRatebrks <- quantile(DefFilter$DefSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      DefSuccessRatebrksy <- round(seq(255, 40, length.out = length(DefSuccessRatebrks) + 1), 0)
      DefSuccessRatebrksclrs <- paste0("rgb(", 255-DefSuccessRatebrksy, "," , DefSuccessRatebrksy , ",", 0, ")")
      
      PassEPAbrks <- quantile(DefFilter$PassEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      PassEPAbrksy <- round(seq(255, 40, length.out = length(PassEPAbrks) + 1), 0)
      PassEPAbrksclrs <- paste0("rgb(", 255-PassEPAbrksy, "," , PassEPAbrksy , ",", 0, ")")
      
      PassSuccessRatebrks <- quantile(DefFilter$PassSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      PassSuccessRatebrksy <- round(seq(255, 40, length.out = length(PassSuccessRatebrks) + 1), 0)
      PassSuccessRatebrksclrs <- paste0("rgb(", 255-PassSuccessRatebrksy, "," , PassSuccessRatebrksy , ",", 0, ")")
      
      RushEPAbrks <- quantile(DefFilter$RushEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      RushEPAbrksy <- round(seq(255, 40, length.out = length(RushEPAbrks) + 1), 0)
      RushEPAbrksclrs <- paste0("rgb(", 255-RushEPAbrksy, "," , RushEPAbrksy , ",", 0, ")")
      
      RushSuccessRatebrks <- quantile(DefFilter$RushSuccessRate, probs = seq(.05, .95, .01), na.rm = TRUE)
      RushSuccessRatebrksy <- round(seq(255, 40, length.out = length(RushSuccessRatebrks) + 1), 0)
      RushSuccessRatebrksclrs <- paste0("rgb(", 255-RushSuccessRatebrksy, "," , RushSuccessRatebrksy , ",", 0, ")")
      
      
      datatable(DefFilter, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')), 
                callback = JS(js))%>%
        formatStyle("DefEPA", 
                    backgroundColor = styleInterval(DefEPAbrks,DefEPAbrksclrs)
        )%>%
        formatStyle("DefSuccessRate", 
                    backgroundColor = styleInterval(DefSuccessRatebrks,DefSuccessRatebrksclrs)
        )%>%
        formatStyle("PassEPA", 
                    backgroundColor = styleInterval(PassEPAbrks,PassEPAbrksclrs)
        )%>%
        formatStyle("PassSuccessRate", 
                    backgroundColor = styleInterval(PassSuccessRatebrks,PassSuccessRatebrksclrs)
        )%>%
        formatStyle("RushEPA", 
                    backgroundColor = styleInterval(RushEPAbrks,RushEPAbrksclrs)
        )%>%
        formatStyle("RushSuccessRate", 
                    backgroundColor = styleInterval(RushSuccessRatebrks,RushSuccessRatebrksclrs)
        )
      
      
    })
    
   
    output$NetPlot <- renderPlot({
      
      
      NetEPA %>% 
        group_by() %>% 
        filter(season == input$seasonNet, 
               week == input$weekNet) %>% 
        ggplot(aes(x = reorder(Game, WinNetSuccess), y = WinNetSuccess))+
        geom_col(aes(fill = primary, color = secondary, alpha = 0.5))+
        geom_image(aes(image = url.x))+
        geom_image(aes(image = url.y, y = 0))+
        geom_col(aes(y = WinNetEPA, fill = primary, color = secondary, alpha = 0.2))+
        geom_image(aes(image = url.x, y = WinNetEPA))+
        theme_reach()+
        theme(axis.text.x = element_text(size = 8, angle = 30)) +
        scale_color_identity(aesthetics = c("color", "fill")) +
        labs(
          y= "Net Success/EPA Rate",
          x= "Game",
          title= paste0(input$seasonNet, " ",
            case_when(input$weekNet == "" ~  "",
                      T ~ paste0("Week ", input$weekNet)), 
            " Net Success Rate and Net EPA by Game"),
          subtitle = "Winner is on the actual value, Loser is on 0. Net Success rate is the Higher Alpha Value, Net EPA is the more transparent",
          caption = "@SethDataScience"
        ) 
    })
    
    output$NetPlot2 <- renderPlot({
      
      
      NetEPA %>% 
        group_by() %>% 
        filter(season == input$seasonNet,
               home_team == case_when(input$teamNet ==  "" ~ home_team,
                                      T ~ input$teamNet) | 
                 away_team == case_when(input$teamNet == "" ~ away_team,
                                        T ~ input$teamNet)) %>% 
        ggplot(aes(x = reorder(Game, WinNetSuccess), y = WinNetSuccess))+
        geom_col(aes(fill = primary, color = secondary, alpha = 0.5))+
        geom_image(aes(image = url.x))+
        geom_image(aes(image = url.y, y = 0))+
        geom_col(aes(y = WinNetEPA, fill = primary, color = secondary, alpha = 0.2))+
        geom_image(aes(image = url.x, y = WinNetEPA))+
        theme_reach()+
        theme(axis.text.x = element_text(size = 8, angle = 30)) +
        scale_color_identity(aesthetics = c("color", "fill")) +
        labs(
          y= "Net Success/EPA Rate",
          x= "Game",
          title= paste0(input$seasonNet, " ", 
                        case_when(input$teamNet == "" ~  "",
                                  T ~ input$teamNet), 
                        " Net Success Rate and Net EPA by Game"),
          subtitle = "Winner is on the actual value, Loser is on 0. Net Success rate is the Higher Alpha Value, Net EPA is the more transparent",
          caption = "@SethDataScience"
        ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
