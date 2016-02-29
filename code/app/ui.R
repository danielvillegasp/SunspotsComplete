library(shiny)

shinyUI(fluidPage(
  navbarPage("Sunspots",
             tabPanel("Sun View",
                      sidebarPanel(
                        dateInput('date.sun.view',label = 'Date', max = "2016-01-01",
                                  min="1875-01-01",value = "2015-12-31"), 
                        width=3
                        ),
                      mainPanel(h2("Sun View"),
                                p("The figure above shows the sunspots and their area relative to the
                                  solar disk for a certain date."),
                        plotOutput('sun.view.plot'),
                        textOutput('date.v'),
                        width = 9
                      )),
             tabPanel("Sunspots and Temperature",
                      sidebarPanel(
                        dateInput('date.begin',label = 'From', max = "2016-01-01",
                                  min="1875-01-01",value = "1875-01-01"),
                        dateInput('date.end',label = 'To', max = "2016-01-01",
                                  min="1875-01-01",value = "2015-12-31"), 
                        numericInput('step', 'Step Size', min=1, max=132, step = 1, value = 1),
                        width=3
                      ),
                      mainPanel(h2("Sunspots and Land-Ocean Temperature Index in Time"),
                                p("The set of figures above show the LOTI vs. the 
                                  number of sunspots between two dates."),
                        fluidRow(
                          column(12,
                            plotOutput('sp.time.plot')
                          )
                        ),
                        fluidRow(
                          column(6,
                                 plotOutput('sp.hist.plot')
                          ),
                          column(6,
                                 plotOutput('temp.hist')
                          )
                        ),
                        fluidRow(
                          column(12,
                                 plotOutput('temp.vs.sp')
                          )
                        )
                      ),
                      width = 9)
             )
))