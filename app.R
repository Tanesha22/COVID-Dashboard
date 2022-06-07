#school dashboard#
library(shiny, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(reactable, warn.conflicts = FALSE)
library(rsconnect, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(tidyquant, warn.conflicts = FALSE)
library(htmlwidgets, warn.conflicts = FALSE)

options(shiny.sanitize.errors = FALSE)

#BRHD Color Palette, from BRHD Style Guide
BRHD_cols = list(rgb(0, 141, 168, maxColorValue = 255), rgb(241, 227, 197, maxColorValue = 255),
                 rgb(212, 69, 29, maxColorValue = 255), rgb(102, 51, 52, maxColorValue = 255),
                 rgb(255, 206, 113, maxColorValue = 255), rgb(109, 39, 106, maxColorValue = 255),
                 rgb(231, 65, 122, maxColorValue = 255))

besd_react <- read.csv("besd_react.csv", header = T, colClasses = c("NULL","character",rep("numeric",5)))
ccsd_react <- read.csv("ccsd_react.csv", header = T, colClasses = c("NULL","character",rep("numeric",5)))
lcsd_react <- read.csv("lcsd_react.csv", header = T, colClasses = c("NULL","character",rep("numeric",5)))
rsd_react <- read.csv("rsd_react.csv", header = T, colClasses = c("NULL","character",rep("numeric",5)))
co_react <- read.csv("co_react.csv", header = T, colClasses = c("NULL","character",rep("numeric",5)))

k12_besd <- read.csv("k12_besd.csv", header = T, colClasses = c("NULL","Date","character","numeric","character",rep("numeric",17)))
k12_ccsd <- read.csv("k12_ccsd.csv", header = T, colClasses = c("NULL","Date","character","numeric","character",rep("numeric",17)))
k12_lcsd <- read.csv("k12_lcsd.csv", header = T, colClasses = c("NULL","Date","character","numeric","character",rep("numeric",17)))
k12_rsd <- read.csv("k12_rsd.csv", header = T, colClasses = c("NULL","Date","character","numeric","character",rep("numeric",17)))
k12_co <- read.csv("k12_co.csv", header = T, colClasses = c("NULL","Date","character","numeric","character",rep("numeric",17)))

besd_grouped <- read.csv("besd_grouped.csv", header = T, colClasses = c("NULL","Date","numeric","numeric","character",rep("numeric",12)))
ccsd_grouped <- read.csv("ccsd_grouped.csv", header = T, colClasses = c("NULL","Date","numeric","numeric","character",rep("numeric",12)))
lcsd_grouped <- read.csv("lcsd_grouped.csv", header = T, colClasses = c("NULL","Date","numeric","numeric","character",rep("numeric",12)))
rsd_grouped <- read.csv("rsd_grouped.csv", header = T, colClasses = c("NULL","Date","numeric","numeric","character",rep("numeric",12)))
co_grouped <- read.csv("co_grouped.csv", header = T, colClasses = c("NULL","Date","numeric","numeric","character",rep("numeric",12)))

colnames(besd_grouped)

todays_date = Sys.Date()

#Create BESD Table
besd_table <- reactable(data = besd_react,
                        details = function(index){
                            besd_data <- bind_rows(dplyr::select(k12_besd,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases),
                                                   dplyr::select(besd_grouped,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases))
                            expanded_plot = ggplot(data = filter(besd_data, School == besd_react$School[index]) %>% 
                                rename(`Active Cases` = Active.Cases, `Total Cases` = Total.Cases,
                                       `Active Cases from School Transmission` = Active.Cases.from.School.Transmission)) +
                                geom_area(aes(x = Date, y = `Active Cases`, color = "Active Cases"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[1]], alpha = 0.33)) +
                                geom_area(aes(x = Date, y = `Active Cases from School Transmission`, color = "Active Cases from School Transmission"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[7]], alpha = 0.33)) +
                                ggtitle(paste("COVID-19 Progression at", besd_react$School[index])) +
                                theme_classic() + xlab("Date") + ylab("COVID-19 Cases (n)") +
                                scale_color_manual("",breaks = c("Active Cases", "Active Cases from School Transmission", "Recovered"),
                                                   values = c(BRHD_cols[[1]],BRHD_cols[[7]])) +
                                scale_x_date(breaks = "1 month", date_labels = "%b") +
                                scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                                theme(text = element_text(family = "EB Garamond, serif"))
                            
                            htmltools::div(style = "padding: 25px", plotly::ggplotly(expanded_plot, tooltip = c("x","y")))
                        },
                        fullWidth = F, bordered = T, highlight = T,
                        rowStyle = function(index){
                            if(besd_react$School[index] == "Box Elder School District"){
                                list(fontWeight = "bold")
                            }
                        },
                        #Define reactable Columns
                        columns = list(
                            School = colDef(width = 200),
                            New.Student.Cases = colDef(defaultSortOrder = "desc", width = 140, name = "New Student Cases"),
                            Total.Cases = colDef(defaultSortOrder = "desc", width = 90, name = "Total Cases"),
                            Enrollment = colDef(width = 90, name = "Enrollment"),
                            Current.Threshold.Cases = colDef(width = 170, name = "Current Threshold Cases"),
                            Percent.of.Enrollment = colDef(defaultSortOrder = "desc", width = 150, name = "Percent of Enrollment")),
                        theme = reactableTheme(borderColor = "black",
                                               style = list(fontFamily = "EB Garamond, serif", fontSize = "14px")),
                        searchable = TRUE)

#Create CCSD Table
ccsd_table <- reactable(data = ccsd_react,
                        details = function(index){
                            ccsd_data <- bind_rows(dplyr::select(k12_ccsd,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases),
                                                   dplyr::select(ccsd_grouped,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases))
                            expanded_plot = ggplot(data = filter(ccsd_data, School == ccsd_react$School[index]) %>% 
                                                       rename(`Active Cases` = Active.Cases, `Total Cases` = Total.Cases,
                                                              `Active Cases from School Transmission` = Active.Cases.from.School.Transmission)) +
                                geom_area(aes(x = Date, y = `Active Cases`, color = "Active Cases"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[1]], alpha = 0.33)) +
                                geom_area(aes(x = Date, y = `Active Cases from School Transmission`, color = "Active Cases from School Transmission"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[7]], alpha = 0.33)) +
                                ggtitle(paste("COVID-19 Progression at", ccsd_react$School[index])) +
                                theme_classic() + xlab("Date") + ylab("COVID-19 Cases (n)") +
                                scale_color_manual("",breaks = c("Active Cases", "Active Cases from School Transmission", "Recovered"),
                                                   values = c(BRHD_cols[[1]],BRHD_cols[[7]])) +
                                scale_x_date(breaks = "1 month", date_labels = "%b") +
                                scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                                theme(text = element_text(family = "EB Garamond, serif"))
                            
                            htmltools::div(style = "padding: 25px", plotly::ggplotly(expanded_plot, tooltip = c("x","y")))
                        },
                        fullWidth = F, bordered = T, highlight = T,
                        rowStyle = function(index){
                            if(ccsd_react$School[index] == "Cache County School District"){
                                list(fontWeight = "bold")
                            }
                        },
                        #Define reactable Columns
                        columns = list(
                            School = colDef(width = 200),
                            New.Student.Cases = colDef(defaultSortOrder = "desc", width = 140, name = "New Student Cases"),
                            Total.Cases = colDef(defaultSortOrder = "desc", width = 90, name = "Total Cases"),
                            Enrollment = colDef(width = 90, name = "Enrollment"),
                            Current.Threshold.Cases = colDef(width = 170, name = "Current Threshold Cases"),
                            Percent.of.Enrollment = colDef(defaultSortOrder = "desc", width = 150, name = "Percent of Enrollment")),
                        theme = reactableTheme(borderColor = "black",
                                               style = list(fontFamily = "EB Garamond, serif", fontSize = "14px")),
                        searchable = TRUE)

#Create LCSD Table
lcsd_table <- reactable(data = lcsd_react,
                        details = function(index){
                            lcsd_data <- bind_rows(dplyr::select(k12_lcsd,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases),
                                                   dplyr::select(lcsd_grouped,School,Date,New.Student.Cases,Total.Student.Cases,
                                                                 Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                                 Active.Cases.from.School.Transmission,Total.Cases))
                            expanded_plot = ggplot(data = filter(lcsd_data, School == lcsd_react$School[index]) %>% 
                                                       rename(`Active Cases` = Active.Cases, `Total Cases` = Total.Cases,
                                                              `Active Cases from School Transmission` = Active.Cases.from.School.Transmission)) +
                                geom_area(aes(x = Date, y = `Active Cases`, color = "Active Cases"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[1]], alpha = 0.33)) +
                                geom_area(aes(x = Date, y = `Active Cases from School Transmission`, color = "Active Cases from School Transmission"), outline.type = "upper",
                                          show.legend = F, fill = alpha(BRHD_cols[[7]], alpha = 0.33)) +
                                ggtitle(paste("COVID-19 Progression at", lcsd_react$School[index])) +
                                theme_classic() + xlab("Date") + ylab("COVID-19 Cases (n)") +
                                scale_color_manual("",breaks = c("Active Cases", "Active Cases from School Transmission", "Recovered"),
                                                   values = c(BRHD_cols[[1]],BRHD_cols[[7]])) +
                                scale_x_date(breaks = "1 month", date_labels = "%b") +
                                scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                                theme(text = element_text(family = "EB Garamond, serif"))
                            
                            htmltools::div(style = "padding: 25px", plotly::ggplotly(expanded_plot, tooltip = c("x","y")))
                        },
                        fullWidth = F, bordered = T, highlight = T,
                        rowStyle = function(index){
                            if(lcsd_react$School[index] == "Logan City School District"){
                                list(fontWeight = "bold")
                            }
                        },
                        #Define reactable Columns
                        columns = list(
                            School = colDef(width = 200),
                            New.Student.Cases = colDef(defaultSortOrder = "desc", width = 140, name = "New Student Cases"),
                            Total.Cases = colDef(defaultSortOrder = "desc", width = 90, name = "Total Cases"),
                            Enrollment = colDef(width = 90, name = "Enrollment"),
                            Current.Threshold.Cases = colDef(width = 170, name = "Current Threshold Cases"),
                            Percent.of.Enrollment = colDef(defaultSortOrder = "desc", width = 150, name = "Percent of Enrollment")),
                        theme = reactableTheme(borderColor = "black",
                                               style = list(fontFamily = "EB Garamond, serif", fontSize = "14px")),
                        searchable = TRUE)

#Create RSD Table
rsd_table <- reactable(data = rsd_react,
                       details = function(index){
                           rsd_data <- bind_rows(dplyr::select(k12_rsd,School,Date,New.Student.Cases,Total.Student.Cases,
                                                               Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                               Active.Cases.from.School.Transmission,Total.Cases),
                                                 dplyr::select(rsd_grouped,School,Date,New.Student.Cases,Total.Student.Cases,
                                                               Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                               Active.Cases.from.School.Transmission,Total.Cases))
                           expanded_plot = ggplot(data = filter(rsd_data, School == rsd_react$School[index]) %>% 
                                                      rename(`Active Cases` = Active.Cases, `Total Cases` = Total.Cases,
                                                             `Active Cases from School Transmission` = Active.Cases.from.School.Transmission)) +
                               geom_area(aes(x = Date, y = `Active Cases`, color = "Active Cases"), outline.type = "upper",
                                         show.legend = F, fill = alpha(BRHD_cols[[1]], alpha = 0.33)) +
                               geom_area(aes(x = Date, y = `Active Cases from School Transmission`, color = "Active Cases from School Transmission"), outline.type = "upper",
                                         show.legend = F, fill = alpha(BRHD_cols[[7]], alpha = 0.33)) +
                               ggtitle(paste("COVID-19 Progression at", rsd_react$School[index])) +
                               theme_classic() + xlab("Date") + ylab("COVID-19 Cases (n)") +
                               scale_color_manual("",breaks = c("Active Cases", "Active Cases from School Transmission", "Recovered"),
                                                  values = c(BRHD_cols[[1]],BRHD_cols[[7]])) +
                               scale_x_date(breaks = "1 month", date_labels = "%b") +
                               scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                               theme(text = element_text(family = "EB Garamond, serif"))
                           
                           htmltools::div(style = "padding: 25px", plotly::ggplotly(expanded_plot, tooltip = c("x","y")))
                       },
                       fullWidth = F, bordered = T, highlight = T,
                       rowStyle = function(index){
                           if(rsd_react$School[index] == "Rich School District"){
                               list(fontWeight = "bold")
                           }
                       },
                       #Define reactable Columns
                       columns = list(
                           School = colDef(width = 200),
                           New.Student.Cases = colDef(defaultSortOrder = "desc", width = 140, name = "New Student Cases"),
                           Total.Cases = colDef(defaultSortOrder = "desc", width = 90, name = "Total Cases"),
                           Enrollment = colDef(width = 90, name = "Enrollment"),
                           Current.Threshold.Cases = colDef(width = 170, name = "Current Threshold Cases"),
                           Percent.of.Enrollment = colDef(defaultSortOrder = "desc", width = 150, name = "Percent of Enrollment")),
                       theme = reactableTheme(borderColor = "black",
                                              style = list(fontFamily = "EB Garamond, serif", fontSize = "14px")),
                       searchable = TRUE)

#Create Charter/Other Table
co_table <- reactable(data = co_react,
                      details = function(index){
                          co_data <- bind_rows(dplyr::select(k12_co,School,Date,New.Student.Cases,Total.Student.Cases,
                                                             Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                             Active.Cases.from.School.Transmission,Total.Cases),
                                               dplyr::select(co_grouped,School,Date,New.Student.Cases,Total.Student.Cases,
                                                             Active.Cases,Current.Threshold.Cases,Percent.of.Enrollment,
                                                             Active.Cases.from.School.Transmission,Total.Cases))
                          expanded_plot = ggplot(data = filter(co_data, School == co_react$School[index]) %>% 
                                                     rename(`Active Cases` = Active.Cases, `Total Cases` = Total.Cases,
                                                            `Active Cases from School Transmission` = Active.Cases.from.School.Transmission)) +
                              geom_area(aes(x = Date, y = `Active Cases`, color = "Active Cases"), outline.type = "upper",
                                        show.legend = F, fill = alpha(BRHD_cols[[1]], alpha = 0.33)) +
                              geom_area(aes(x = Date, y = `Active Cases from School Transmission`, color = "Active Cases from School Transmission"), outline.type = "upper",
                                        show.legend = F, fill = alpha(BRHD_cols[[7]], alpha = 0.33)) +
                              ggtitle(paste("COVID-19 Progression at", co_react$School[index])) +
                              theme_classic() + xlab("Date") + ylab("COVID-19 Cases (n)") +
                              scale_color_manual("",breaks = c("Active Cases", "Active Cases from School Transmission", "Recovered"),
                                                 values = c(BRHD_cols[[1]],BRHD_cols[[7]])) +
                              scale_x_date(breaks = "1 month", date_labels = "%b") +
                              scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                              theme(text = element_text(family = "EB Garamond, serif"))
                          
                          htmltools::div(style = "padding: 25px", plotly::ggplotly(expanded_plot, tooltip = c("x","y")))
                      },
                      fullWidth = F, bordered = T, highlight = T,
                      rowStyle = function(index){
                          if(co_react$School[index] == "Charter/Other Schools"){
                              list(fontWeight = "bold")
                          }
                      },
                      #Define reactable Columns
                      columns = list(
                          School = colDef(width = 200),
                          New.Student.Cases = colDef(defaultSortOrder = "desc", width = 140, name = "New Student Cases"),
                          Total.Cases = colDef(defaultSortOrder = "desc", width = 90, name = "Total Cases"),
                          Enrollment = colDef(width = 90, name = "Enrollment"),
                          Current.Threshold.Cases = colDef(width = 170, name = "Current Threshold Cases"),
                          Percent.of.Enrollment = colDef(defaultSortOrder = "desc", width = 150, name = "Percent of Enrollment")),
                      theme = reactableTheme(borderColor = "black",
                                             style = list(fontFamily = "EB Garamond, serif", fontSize = "14px")),
                      searchable = TRUE)


# Define UI for application
ui <- fluidPage(
    titlePanel("BRHD School COVID-19 Cases"),
    mainPanel(
        HTML(
            paste(
                h5("Data represented on this page are specific to lab-confirmed COVID-19 cases 
                for K-12 students and staff currently living within the Bear River Health District. Data are updated
                weekly on Monday at approximately 11:00 a.m."),
                br(),
                h5("Last updated 6/7/2022 at 10:00 a.m."),
                br(),
                
                br(),
                h2("Box Elder School District")
            )),
        reactableOutput("chart1"),
        br(),
        br(),
        br(),
        br(),
        HTML(
            paste(
                h2("Cache County School District"))),
        reactableOutput("chart2"),
        br(),
        br(),
        br(),
        br(),
        HTML(
            paste(
                h2("Logan City School District"))),
        reactableOutput("chart3"),
        br(),
        br(),
        br(),
        br(),
        HTML(
            paste(
                h2("Rich School District"))),
        reactableOutput("chart4"),
        br(),
        br(),
        br(),
        br(),
        HTML(
            paste(
                h2("Charter/Other Schools"))),
        reactableOutput("chart5"),
        br(),
        br(),
        br(),
        br(),
    ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$chart1 <- renderReactable({
        besd_table
    })
    
    output$chart2 <- renderReactable({
        ccsd_table
       
    })
    
    output$chart3 <- renderReactable({
        lcsd_table
       
    })
    
    output$chart4 <- renderReactable({
        rsd_table
       
    })
    
    output$chart5 <- renderReactable({
        co_table
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)