# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)


Data1 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "Jabatan")
Data2 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "SPU")
Data3 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "Unit")
Data4 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "F_Pemeriksa")
Data5 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "Golongan")
Data6 <- read_excel("~/dashboard R/aset/Datapegawaikaltara.xlsx",sheet= "Pangkat")

#judul Dashboard
header <- dashboardHeader(title = "Dashboard Kepegawaian") 

#Bagian menu dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Kepegawaian", tabName = "dashboard", icon = icon("dashboard",lib='glyphicon')),
    menuItem("Portal BPK", icon = icon("send",lib='glyphicon'), 
             href = "https://portal.bpk.go.id/sites/kaltara/Pages/Newlandingpage.aspx")
  )
)

# bagian isi diagram
body <- dashboardBody(
  # Baris pertama berisi 3 box 
  fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")),
  # Baris kedua berisi 8 box 7 donut chart dan 1 bar chart
  fluidRow(
    box(
      title = "Komposisi Pegawai"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie3", height = "400px")
    ),
    box(
      title = "Pendidikan Formal"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie2", height = "400px")
    ),
    box(
      title = "Golongan"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie6", height = "400px")
    ),
    box(
      title = "Usia"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("bar", height = "400px")
    ),
    box(
      title = "Jabatan Fungsional Pemeriksa"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie5", height = "400px")
    ),
    box(
      title = "Pangkat"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie7", height = "400px")
    ),
    box(
      title = "Unit Penempatan Kerja"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie4", height = "400px")
    ),
    box(
      title = "Jabatan"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("pie1", height = "400px")
    )
  )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Dashboard Kepegawaian', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(97,format="d", big.mark='')
      ,paste('Jumlah Pegawai')
      ,icon = icon("list",lib='glyphicon')
      ,color = "teal")
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(62,format="d", big.mark='')
      ,paste('Laki-Laki')
      ,icon = icon("user",lib='glyphicon')
      ,color = "lime")
    
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(35, format="d", big.mark='')
      ,paste('Perempuan')
      ,icon = icon("user",lib='glyphicon')
      ,color = "fuchsia")
  })
  
  
  # creating the plotOutput content
  output$pie1 <- renderPlotly({
    Data11 <- Data1 %>% group_by(Jabatan)
    Data11 <- Data11 %>% summarize(count = n())
    fig1 <- plot_ly(Data11, labels = ~Jabatan, values = ~count,
                    marker = list(colors = c("#CC6699","#FFCC99","#CCCCCC","#CC99FF","#9999CC")), type ="pie"
                    )
    fig1
  })
  
  output$pie2 <- renderPlotly({
    Data22 <- Data2 %>% group_by(Pendidikan)
    Data22 <- Data22 %>% summarize(count = n())
    fig2 <- plot_ly(Data22, labels = ~Pendidikan, values = ~count,
                    marker = list(colors = c("#CC6699","#FFCC99","#CCCCCC","#CC99FF","#9999CC"))
                    )
    fig2 <- fig2 %>% add_pie(hole = 0.55)
    fig2
  })
  
  output$pie3 <- renderPlotly({
    Data222 <- Data2 %>% group_by(Status_Kepegawaian)
    Data222 <- Data222 %>% summarize(count = n())
    fig3 <- plot_ly(Data222, labels = ~Status_Kepegawaian, values = ~count,
                    marker = list(colors = c("#CC6699","#FFCC99","#CCCCCC"))
                    )
    fig3 <- fig3 %>% add_pie(hole = 0.55)
    fig3
  })
  
  output$pie4 <- renderPlotly({
    Data33 <- Data3 %>% group_by(Unit_Kerja)
    Data33 <- Data33 %>% summarize(count = n())
    fig9 <- plot_ly(Data33, y = ~count, x = ~Unit_Kerja, color= ~Unit_Kerja, 
                    type = "bar")
    fig9 <- fig9 %>% layout(title=" ", 
                            xaxis = list(title=" ",showticklabels=FALSE), 
                            yaxis = list(title="Jumlah Pegawai",showticklabels=TRUE))
    fig9
    #fig4 <- plot_ly(Data33, labels = ~Unit_Kerja, values = ~count)
    #fig4 <- fig4 %>% add_pie(hole = 0.55)
  }) 
  
  output$pie5 <- renderPlotly({
    Data44 <- Data4 %>% group_by(Jabatan_Pemeriksa)
    Data44 <- Data44 %>% summarize(count = n())
    fig5 <- plot_ly(Data44, labels = ~Jabatan_Pemeriksa, values = ~count,
                    marker = list(colors = c("#CC6699","#FFCC99","#CCCCCC"))
                    )
    fig5 <- fig5 %>% add_pie(hole = 0.55)
    fig5
  })
  
  output$pie6 <- renderPlotly({
    Data55 <- Data5 %>% group_by(Golongan)
    Data55 <- Data55 %>% summarize(count = n())
    fig6 <- plot_ly(Data55, labels = ~Golongan, values = ~count,
                    marker = list(colors = c("#FF9966","#99CCFF","#CCCCCC","#CC6699","#FFCC99","#CCCCCC","#CC99FF","#9999CC"))
                    )
    fig6 <- fig6 %>% add_pie(hole = 0.55)
    fig6
  })
  
  output$pie7 <- renderPlotly({
    Data66 <- Data6 %>% group_by(Pangkat)
    Data66 <- Data66 %>% summarize(count = n())
    fig7 <- plot_ly(Data66, labels = ~Pangkat, values = ~count,
                    marker = list(colors = c("#FF9966","#99CCFF","#CCCCCC","#CC6699","#FFCC99","#CCCCCC","#CC99FF","#9999CC"))
                    )
    fig7 <- fig7 %>% add_pie(hole = 0.55)
    fig7
  })
  
  output$bar <- renderPlotly({
    value = c(43,34,14,6)
    labels = c("22-30 Tahun","31-39 Tahun","40-49 Tahun"," > 49 Tahun")
    fig8 <- plot_ly(data, y = value, x = labels, name = labels, color=labels, 
                    type = "bar")
    fig8 <- fig8 %>% layout(title=" ", 
                            xaxis = list(title="Usia",showticklabels=TRUE), 
                            yaxis = list(title="Jumlah Pegawai",showticklabels=TRUE))
    fig8
  })
}

shinyApp(ui, server)