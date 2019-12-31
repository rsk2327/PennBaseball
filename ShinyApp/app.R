library(shiny)
library(tidyverse)
library(DT)
library(zoo)
library(scales)
library(stringr)
library(shinydashboard)
library(shinythemes)
library(lubridate)
library(plotly)
library(ggridges)
library(scales)



source('./utils.R')

print(pitcherCols)

ui <- navbarPage(
  "Penn Baseball",
  
  tabPanel("Data",
           
           sidebarLayout(
             sidebarPanel(
               width = 2,
               uiOutput(outputId = "DT_gameSelection"),
               uiOutput(outputId = "DT_homeTeamSelection"),
               uiOutput(outputId = "DT_awayTeamSelection"),
               hr(),
               uiOutput(outputId = "DT_colSelection"),
               uiOutput(outputId = "DT_pitcherSelection"),
               uiOutput(outputId = "DT_batterSelection")
               
               
               
             ),
             mainPanel(
               width = 10,
               shinydashboard::box(
                 width = NULL,
                 status = "primary",
                 div(style = 'overflow-x: scroll', dataTableOutput("datatable"))
               )
             )
           )),
  
  tabPanel("Pitcher",
           sidebarLayout(
             sidebarPanel(
               width = 2,
               uiOutput(outputId = "PT_teamSelection"),
               uiOutput(outputId = "PT_pitcherSelection"),
               uiOutput(outputId = "PT_comparePitcherSelection"),
               uiOutput(outputId = "PT_interactSelection"),
               hr(),
               uiOutput("PT_Plot_xaxis"),
               uiOutput("PT_Plot_yaxis"),
               uiOutput("PT_PitchType")
             ),
             mainPanel(width = 10,
                       tabsetPanel(id='PT_Tabset',
                         tabPanel("Plots",
                                  fluidRow(plotlyOutput('PT_Plot')),
                                  fluidRow(column(
                                    width = 6, htmlOutput("PT_Stats")
                                  ))),
                         tabPanel("Compare", 
                                  fluidRow(
                                    column(width=6,
                                           uiOutput("PT_comparePlot1")),
                                    column(width=6,
                                           uiOutput("PT_comparePlot2"))
                                  ))
                       ))
           )),
  tabPanel("Batter")
)


server <- function(input, output, session) {
  ############################
  ##### DATA TABLE ###########
  ############################
  
  output$DT_gameSelection <- renderUI({
    gameID_options <- sort(unique(baseDf$gameID))
    selectizeInput(
      inputId = "DT_gameSelection_input",
      label = "Game",
      choices = c("All", gameID_options),
      selected = "All"
    )
  })
  
  output$DT_homeTeamSelection <- renderUI({
    homeTeam_options <- sort(unique(baseDf$Home_Team))
    selectizeInput(
      inputId = "DT_homeTeamSelection_input",
      label = "Home Team",
      choices =  c("All", homeTeam_options),
      selected = input$DT_homeTeamSelection_input
    )
  })
  
  output$DT_awayTeamSelection <- renderUI({
    awayTeam_options <- sort(unique(baseDf$Away_Team))
    selectizeInput(
      inputId = "DT_awayTeamSelection_input",
      label = "Away Team",
      choices =  c("All", awayTeam_options),
      selected = input$DT_awayTeamSelection_input
    )
  })
  
  
  output$DT_pitcherSelection <- renderUI({
    selectizeInput(
      inputId = "DT_pitcherSelection_input",
      label = "Pitcher",
      choices =  c("All", sort(unique(
        game_data()$Pitcher
      ))),
      selected = input$DT_pitcherSelection_input
    )
  })
  
  output$DT_batterSelection <- renderUI({
    selectizeInput(
      inputId = "DT_batterSelection_input",
      label = "Batter",
      choices =  c("All", sort(unique(
        game_data()$Batter
      ))),
      selected = input$DT_batterSelection_input
    )
  })
  
  output$DT_colSelection <- renderUI({
    selectizeInput(
      inputId = "DT_colSelection_input",
      label = "Columns",
      choices =  c("All", names(baseDf)),
      selected = "All",
      multiple = TRUE
    )
  })
  
  
  
  game_data <- reactive({
    req(input$DT_gameSelection_input)
    df  =  baseDf
    
    if (input$DT_gameSelection_input != "All") {
      df <- df %>% filter(gameID == input$DT_gameSelection_input)
    }
    
    if (input$DT_homeTeamSelection_input != "All") {
      df <- df %>% filter(Home_Team == input$DT_homeTeamSelection_input)
    }
    
    if (input$DT_awayTeamSelection_input != "All") {
      df <- df %>% filter(Away_Team == input$DT_awayTeamSelection_input)
    }
    
    df
    
  })
  
  DT_data <- reactive({
    req(input$DT_gameSelection_input)
    
    df = game_data()
    
    if (isTruthy(input$DT_pitcherSelection_input != "All")) {
      df <- df %>% filter(Pitcher == input$DT_pitcherSelection_input)
    }
    
    if (isTruthy(input$DT_batterSelection_input != "All")) {
      df <- df %>% filter(Batter == input$DT_batterSelection_input)
    }
    
    if (!("All" %in% input$DT_colSelection_input)) {
      df = df[, input$DT_colSelection_input]
    }
    
    df
  })
  
  
  output$datatable <- renderDataTable({
    req(input$DT_gameSelection_input)
    selectDf <- DT_data()
    datatable(selectDf, options = list(paging = TRUE, selection = "none"))
  })
  
  
  
  ############################
  ##### PITCHER ##############
  ############################
  
  
  #### UI OUTPUTS ############
  output$PT_teamSelection <- renderUI({
    team_options <- sort(unique(baseDf$Pitcher_Team))
    selectizeInput(
      inputId = "PT_teamSelection_input",
      label = "Team",
      choices = c("All", team_options),
      selected = "All"
    )
  })
  
  output$PT_pitcherSelection <- renderUI({
    pitcher_options <- sort(unique(team_data()$Pitcher))
    selectizeInput(
      inputId = "PT_pitcherSelection_input",
      label = "Pitcher",
      choices = pitcher_options,
      selected = input$PT_pitcherSelection_input
    )
  })
  
  output$PT_comparePitcherSelection <- renderUI({
    pitcher_options <- sort(unique(team_data()$Pitcher))
    
    if (input$PT_Tabset=='Compare'){
      
      selectizeInput(
        inputId = "PT_comparePitcherSelection_input",
        label = "Compare with",
        choices = pitcher_options,
        selected = input$PT_comparePitcherSelection_input
      )
      
    }
    
    
  })
  
  
  output$PT_Plot_yaxis <- renderUI({
    options = c("None",
                "Pitch_Speed",
                "Pitch_Spin",
                "Pitch_Release_Height",
                "Pitch_Time")
    options = c("None", pitcherCols)
    selectizeInput(
      inputId = "PT_Plot_yaxis_input",
      label = "Y Axis",
      choices = options,
      selected = input$PT_PT_plot_yaxis_input
    )
    
  })
  
  output$PT_Plot_xaxis <- renderUI({
    options = c("Pitch_Speed",
                "Pitch_Spin",
                "Pitch_Release_Height",
                "Pitch_Time")
    options = pitcherCols
    selectizeInput(
      inputId = "PT_Plot_xaxis_input",
      label = "X Axis",
      choices = options,
      selected = input$PT_pitcherSelection_input
    )
    
  })

  output$PT_PitchType <- renderUI({

    df <- pitcher_data()
    options = c("All",names(table(df$Pitch_Type)))
    selectizeInput(
      inputId = "PT_PitchType_input",
      label = "Pitch Type",
      choices = options,
      selected = input$PT_PitchType_input
    )
    
  }) 
  
  output$PT_interactSelection <- renderUI({
    
    
    if (input$PT_Tabset=='Compare'){
      
      checkboxInput(
        inputId = "PT_interactSelection_input",
        label = "Interactive Plot"
        
      )
      
    }
    
  })
  
  output$PT_Stats <- renderUI({
    
    df =pitcher_filter_data()
    
    xaxis = input$PT_Plot_xaxis_input
    xaxis_ = invColMapping[xaxis]
    
    
    games = n_distinct(df$gameID)
    pitches = nrow(df)
    mean = round(mean(df[, xaxis_], na.rm = TRUE),2)
    median = round(median(df[, xaxis_],na.rm = TRUE),2)
    sd = round(sd(df[, xaxis_], na.rm = TRUE),2)
    
    p1 = "<table style='width:50%; text-align:center; align:center;'>
      <tbody style='width:50%; text-align:center; align:center;'>"
    
    p = sprintf("<tr>
      <td> &nbsp;</td>
      </tr>
      <tr>
      <td style='text-align:left;font-weight:bold;'>Games &nbsp;</td>
      <td style='text-align:right;'>%s&nbsp;</td>
      </tr>
      <tr>
      <td style='text-align:left;font-weight:bold;'>Pitches &nbsp;</td>
      <td style='text-align:right;'>%s&nbsp;</td>
      </tr>
      <tr>
      <td style='text-align:left;font-weight:bold;'>Mean &nbsp;</td>
      <td style='text-align:right;'>%s&nbsp;</td>
      </tr>
      <tr>
      <td style='text-align:left;font-weight:bold;'>Median &nbsp;</td>
      <td style='text-align:right;'>%s&nbsp;</td>
      </tr>
      <tr>
      <td style='text-align:left;font-weight:bold;'>Deviation &nbsp;</td>
      <td style='text-align:right;'>%s&nbsp;</td>
      </tr>
      </tbody>
      </table>",games, pitches, mean, median, sd)
    
    p = paste0(p1,p)
    
    HTML(p)
    
    
  })
  
  
  
  
  
  
  
  output$PT_PlayerStats <- renderTable({
    rowNames = c("Name", "Hit %", "Games")
    
    df = pitcher_filter_data()
    values = c(df$Pitcher[1],
               round(mean(df$hit), 3),
               length(unique(df$gameID)))
    
    d = data.frame(rowNames, values)
    
    d
  },
  colnames = FALSE,  align = 'lr', striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$PT_Name <- renderText({
    req(input$PT_pitcherSelection_input)
    paste0("Name : ", pitcher_data()$Pitcher[1])
  })
  output$PT_HitPercent <- renderText({
    req(input$PT_pitcherSelection_input)
    paste0("Hit % : ", mean(pitcher_data()$hit))
  })
  
  
  
  
  
  output$PT_Plot <- renderPlotly({
    req(input$PT_Plot_xaxis_input)
    
    df = pitcher_filter_data()

    xaxis = input$PT_Plot_xaxis_input
    yaxis = input$PT_Plot_yaxis_input
    
    xaxis_ = invColMapping[xaxis]
    yaxis_ = invColMapping[yaxis]
    
    xmin = min(df[, xaxis_])
    xmax = max(df[, xaxis_])
    
    xminLimit = xmin - 0.1*xmin
    xmaxLimit = xmax + 0.1*xmax
    
    if (yaxis == 'None') {
      p = ggplot(df) + geom_histogram(aes(x = df[, xaxis_]), binwidth = 1, fill =
                                        "black", col = "white") +
        xlab(xaxis) + xlim(xminLimit,xmaxLimit)
      
    } else{
      tooltip_text = "%s : %f <br />%s : %f <br />"
      
      p =  ggplot(df) + geom_point(aes(
        x = df[, xaxis_],
        df[, yaxis_],
        color = df$Pitch_Type,
        text = sprintf(tooltip_text, xaxis, df[, xaxis_], yaxis, df[, yaxis_])
      )) +
        xlab(xaxis) + ylab(yaxis)  + theme(legend.title = element_blank()) +
        scale_color_manual(values = pitchTypeScale)
      
      p = p + labs(fill = "Pitch Type")
      
      
      
    }
    
    
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE)
    
  })
  
  
  
    PT_comparePlot1 <- reactive({
    
    req(input$PT_Plot_xaxis_input)
    
    df = pitcher_filter_data()
    
    xaxis = input$PT_Plot_xaxis_input
    yaxis = input$PT_Plot_yaxis_input
    
    xaxis_ = invColMapping[xaxis]
    yaxis_ = invColMapping[yaxis]
    
    xmin = min(df[, xaxis_])
    xmax = max(df[, xaxis_])
    
    xminLimit = xmin - 0.1*xmin
    xmaxLimit = xmax + 0.1*xmax
    
    if (yaxis == 'None') {
      p = ggplot(df) + geom_histogram(aes(x = df[, xaxis_]), binwidth = 1, fill =
                                        "black", col = "white") +
        xlab(xaxis) + xlim(xminLimit,xmaxLimit)
      
    } else{
      tooltip_text = "%s : %.2f <br />%s : %.2f <br />%s : %s"
      
      p =  ggplot(df, aes(
        x = df[, xaxis_],
        y = df[, yaxis_],
        color = df$Pitch_Type,
        text = sprintf(tooltip_text, xaxis, df[, xaxis_], yaxis, df[, yaxis_], "Pitch Type",df$Pitch_Type)
      )) + geom_point() +
        xlab(xaxis) + ylab(yaxis)  + theme(legend.title = element_blank(), legend.position="bottom") +
       scale_color_manual(values = pitchTypeScale)
      
      # p = p + labs(fill = "Pitch Type")
      
      p = p + theme(legend.title = element_blank(), legend.position="bottom")
      
      
      
    }
    
    # renderPlot(p)
    
    
  })

  
  
    PT_comparePlot2 <- reactive({
    
    req(input$PT_Plot_xaxis_input)
    
    df = pitcher_filter_data2()
    
    xaxis = input$PT_Plot_xaxis_input
    yaxis = input$PT_Plot_yaxis_input
    
    xaxis_ = invColMapping[xaxis]
    yaxis_ = invColMapping[yaxis]
    
    xmin = min(df[, xaxis_])
    xmax = max(df[, xaxis_])
    
    xminLimit = xmin - 0.1*xmin
    xmaxLimit = xmax + 0.1*xmax
    
    if (yaxis == 'None') {
      p = ggplot(df) + geom_histogram(aes(x = df[, xaxis_]), binwidth = 1, fill =
                                        "black", col = "white") +
        xlab(xaxis) + xlim(xminLimit,xmaxLimit)
      
    } else{
      tooltip_text = "%s : %f <br />%s : %f <br />%s : %s"
      
      p =  ggplot(df) + geom_point(aes(
        x = df[, xaxis_],
        df[, yaxis_],
        color = df$Pitch_Type,
        text = sprintf(tooltip_text, xaxis, round(df[, xaxis_],2), yaxis, round(df[, yaxis_],2), "Pitch Type",df$Pitch_Type)
      )) + scale_color_manual(values = pitchTypeScale) +
        xlab(xaxis) + ylab(yaxis)  + theme(legend.title = element_blank(), legend.position="bottom") 
      
      
    }
    
    
  })
    
  output$PT_comparePlot2_ggplot <- renderPlot({
    p = PT_comparePlot2()
    p
  })
  
  output$PT_comparePlot1_ggplot <- renderPlot({
    p = PT_comparePlot1()
    p
  })
  
  output$PT_comparePlot1_plotly <- renderPlotly({
    p = PT_comparePlot1()
    ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE) %>% layout(showlegend = FALSE)
    
  })
  
  output$PT_comparePlot2_plotly <- renderPlotly({
    p = PT_comparePlot2()
    p = ggplotly(p, tooltip = "text") %>% config(displaylogo = FALSE) %>% layout(showlegend = FALSE)
    
    p.layout.hover.align = 'left'
    
    p
    
    
    
  })
    
    
    output$PT_comparePlot1 <- renderUI({
      
      if (input$PT_interactSelection_input ==TRUE)
      {
        plotlyOutput('PT_comparePlot1_plotly')
      }else{
        plotOutput("PT_comparePlot1_ggplot")
      }
      
      
    })
    
    
  output$PT_comparePlot2 <- renderUI({
    
    if (input$PT_interactSelection_input ==TRUE)
    {
      plotlyOutput('PT_comparePlot2_plotly')
    }else{
      plotOutput("PT_comparePlot2_ggplot")
    }
    
  })
  
  
  
  
  
  
  ### REACTIVES #######
  
  
  team_data <- reactive({
    df = baseDf
    req(input$PT_teamSelection_input)
    if (input$PT_teamSelection_input != "All") {
      df <- df %>% filter(Pitcher_Team == input$PT_teamSelection_input)
    }
    print(nrow(df))
    df
  })
  
  
  pitcher_data <- reactive({
    
    req(input$PT_pitcherSelection_input)
    df = team_data()
    
    df <- df %>% filter(Pitcher == input$PT_pitcherSelection_input)
    
  })
  
  pitcher_filter_data <- reactive({
    
    req(input$PT_PitchType_input)
 
    df = pitcher_data()
    
    
    if (input$PT_PitchType_input!='All')
    {
      print("Here")
      print(input$PT_PitchType_input)
      df = df %>% filter(Pitch_Type == input$PT_PitchType_input)
    }
    
    df
    
  })
  
  
  pitcher_data2 <- reactive({
    
    req(input$PT_comparePitcherSelection_input)
    df = team_data()
    
    df <- df %>% filter(Pitcher == input$PT_comparePitcherSelection_input)
    
  })
  
  pitcher_filter_data2 <- reactive({
    
    req(input$PT_PitchType_input)
    
    df = pitcher_data2()
    
    
    if (input$PT_PitchType_input!='All')
    {
      print("Here")
      print(input$PT_PitchType_input)
      df = df %>% filter(Pitch_Type == input$PT_PitchType_input)
    }
    
    df
    
  })
  
  
  
}

shinyApp(ui, server)



