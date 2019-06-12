

library(shiny)
library(timevis)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(zoo)
library(DT)

###############################################################################

### Read and clean data

###############################################################################


gs_auth()
sheets <- gs_ls()


### read data


data              <- gs_read(gs_key("1PEELI0uD9U18pfenk0_z92MI0Y8ZJxliREHStRQiAM8"))
term.dates        <- gs_read(gs_key("1OG9gUB_feyplcQhDpVdXjiMaKJ60IE7J8SwDdIrD9a8"))

data$start        <- as.Date(data$start, pattern =  '%Y-%m-%d')
data$end          <- as.Date(data$end, pattern = '%Y-%m-%d')

term.dates$start  <- as.Date(term.dates$start, pattern =  '%Y-%m-%d')
term.dates$end    <- as.Date(term.dates$end, pattern = '%Y-%m-%d')
term.dates        <- term.dates[, -c(19)]


###############################################################################

### Function to create icalendar files

###############################################################################


create_ical <- function(start, end, summary, domain="kcl.ac.uk") {
  
  
  ### load dependency
  
  require(uuid, quietly = TRUE, warn.conflicts = FALSE)

  
  ### check function inputs in order
  
  if(length(start) != length(end) | length(summary) != length(end)) {
    
    stop('Error: ical inputs start, end and summary must be the same length')
  }
  
  t <- list()
  
  
  ### Define function to create ical files
  
  for (i in 1:length(start)) {
  
  t[[i]] <- sprintf(
    
    "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//rstats//NONSGML v1.0//EN
BEGIN:VEVENT
UID:%s@%s
DTSTAMP:%s
DTSTART:%s
DTEND:%s
SUMMARY:%s
END:VEVENT
END:VCALENDAR
", uuid::UUIDgenerate(),
    domain, 
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz="GMT"), 
    format(start[i], "%Y%m%dT%H%M%SZ", tz="GMT"), 
    format(end[i], "%Y%m%dT%H%M%SZ", tz="GMT"), 
    summary[i]
  )
    
  }
  
  return(t)
}






###############################################################################

### APP

###############################################################################


ui <- fluidPage(

  
    tags$body(
    tags$style(HTML("
                    
                    #Warning {
                    font-weight: 500;
                    font-size: 14pt;
                    color: #FF0000;
                    }

                    #Warning2 {
                    font-weight: 200;
                    font-size: 14pt;
                    }

                    #Warning3 {
                    font-weight: 200;
                    font-size: 14pt;
                    }

                    
                    "))
    ),
  
  
    h1('This is a module selection tool'),
    
    h4('Select your modules, check you are happy with the deadlines, then download your new calendar'),

    ### Module Choice Inputs
    
    fluidRow(
      
      # Select Year
      column(4, checkboxGroupInput('a',label = "1st Year Modules",
                            choices = data$Module_Name[!duplicated(data$Module_Name)][contains('4SS', vars = data$Code[!duplicated(data$Module_Name)])],
                            selected = data$Module_Name[!duplicated(data$Module_Name)][contains('4SS', vars = data$Code[!duplicated(data$Module_Name)])][1])),
      
      column(4, checkboxGroupInput('b',label = "2nd Year Modules",
                            choices = data$Module_Name[!duplicated(data$Module_Name)][contains('5SS', vars = data$Code[!duplicated(data$Module_Name)])],
                            selected = data$Module_Name[!duplicated(data$Module_Name)][contains('5S', vars = data$Code[!duplicated(data$Module_Name)])][1])),
      
      column(4, checkboxGroupInput('c',label = "3rd Year Modules",
                            choices = data$Module_Name[!duplicated(data$Module_Name)][contains('6SS', vars = data$Code[!duplicated(data$Module_Name)])],
                            selected = data$Module_Name[!duplicated(data$Module_Name)][contains('6SS', vars = data$Code[!duplicated(data$Module_Name)])][1]))

      

    ),
    
    hr(),

    ### Download ICAL button
    
    fluidRow(
      
      column(6, align="center", downloadButton(outputId = 'ical_download', label = 'Download my module deadlines as a calendar')),
      column(6, align = 'center', actionButton('newmodules', label = 'Update module data with new modules'))

      
    ),
    
    hr(),
    
    
    ### Data table UI
    
    fluidRow(
    column(12, DT::dataTableOutput(outputId = 'Schedule'))
    
    ),
    
    hr(),
    
    ### Timevis UI
    
    fluidRow(
      column(12, timevisOutput(outputId = "Timeline"))

    ), 
    
    hr(), 
    
    fluidRow(
      column(12, plotOutput('histogram'))
    )

  )



server <- function(input, output, session) {
  
  ### Filter available module choices by Year
  
  # Select modules
  output$a <- renderUI ({
    
    checkboxGroupInput("a")

  
  })
  
  output$b <- renderUI ({
    
    checkboxGroupInput("b")
    
  })
  
  output$c <- renderUI ({
    
    checkboxGroupInput("c")
    
  })
  

  
  ### Filter data frame by module choices
  
  data_sample <- reactive({

    data.frame(data %>% filter(Module_Name %in% c(input$a, input$b, input$c)))
    
  })
  
  
  ### Filter data frame by module choices
  
  data_sample_table <- reactive({

    filt.dat <- data
      
    dat.split <- nrow(filt.dat) / 2
    
    filt.dat$start[c(1:dat.split)]      <- filt.dat$start[c((dat.split+1):nrow(filt.dat))]
    
    data.frame(filt.dat %>% filter(Module_Name %in% c(input$a, input$b, input$c)))

  })

  
  ### Create .ICS file for download
  
  icals  <- reactive({
    
    temp       <- create_ical(as.POSIXct(paste(data_sample()$start, '12:30:00'), origin = "1970-01-01 00:00:00"),
                    as.POSIXct(paste(data_sample()$end, '12:30:00'), origin = "1970-01-01 00:00:00"),
                      paste(data_sample()$Module_Name, data_sample()$Assessment, sep = ', '))
    
    temp_body  <- lapply(temp[-c(1, length(temp))], function(x) {substr(x, 64, nchar(x) - 15)})
    temp_start <- substr(temp[[1]], 1, nchar(temp[[1]]) - 15)
    temp_end   <- substr(temp[[length(temp)]], 64, nchar(temp[[length(temp)]]))
    
    temp       <- unlist(list(temp_start, temp_body, temp_end))
    
    temp
    
  })


  ### data table output: should this be all deadlines or just bunches / pinch points?
  
  output$Schedule <- DT::renderDataTable({

    temp                    <- data_sample_table()
    temp                    <- temp[!duplicated(temp$id), ]
    temp$`CW#`              <- substr(temp$groups, nchar(temp$groups), nchar(temp$groups))

    colnames(temp)[c(3:4)] <- c('Student Deadline / Exam period start', 'Staff Post Date / Exam period finish')

    ##### NB data is currently for all modules rather than those that clash. 
    DT::datatable(temp %>% select(c(11, 9, 7,ncol(temp), 3,4)))

  })
    
  ### timevis output
  
  output$Timeline   <- renderTimevis({
    
    vis.dat         <- data_sample()
    vis.dat         <- vis.dat %>% filter(start > '2017-09-01' & end < '2018-07-31')
    
    ### group data for TVis by module
    vis.dat         <- rbind(vis.dat, term.dates)
    vis.dat$CWnumb  <- vis.dat$groups
    vis.dat         <- vis.dat[, -c(1)]
    vis.dat$group   <- vis.dat$Code
    
    #vis.dat         <- vis.dat %>% filter((duplicated(id) & content == 'Exam'))
    
    if(length(which(!duplicated(vis.dat$id) & vis.dat$content == 'Exam')) != 0) {
      
      vis.dat         <- vis.dat[-c(which(!duplicated(vis.dat$id) & vis.dat$content == 'Exam')), ]
      
    }
    
    #vis.dat         <- vis.dat[-c(which(duplicated(vis.dat$id) & vis.dat$content == 'Exam')), ]
    vis.dat$id      <- c(1:nrow(vis.dat))
    vis.dat[vis.dat$type == 'background',]$group   <- 'Term Dates'
    
    ### Display information for timevis: module code and CWnumber
    
    vis.dat$content <- ifelse(vis.dat$IsExam == 0,
                              paste0(vis.dat$content, '_CW', substr(vis.dat$CWnumb, nchar(vis.dat$CWnumb), nchar(vis.dat$CWnumb))),
                              vis.dat$content)
    
    timevis(data = vis.dat, groups = data.frame(id = vis.dat$group[!duplicated(vis.dat$group)], content = vis.dat$group[!duplicated(vis.dat$group)]))
    
  })
  
  output$histogram <- renderPlot({
    
    temp            <- data_sample()
    temp$bool       <- 1
    term.dates$bool <- 0
    temp            <- rbind(temp, term.dates)
    temp            <- temp %>% filter(start > '2017-09-01' & end < '2018-07-31')
    
    
    temp$week      <- cut.Date(temp$start, breaks = '1 week', labels = FALSE)
    
    temp           <- temp %>% group_by(week) %>% summarise('number of deadlines' = sum(bool))
    
    ggplot(temp, aes(x = as.numeric(week),y = `number of deadlines`)) + 
      geom_col(fill = 'red', colour = 'black') +
      theme_classic() + ggtitle('Distribution of your deadlines') + xlab('Week of academic year') +
      ylab('Deadlines & Post dates') + scale_y_continuous(limits = c(0, max(temp$`number of deadlines`)), 
                                         breaks = c(seq.int(from = 0, to = max(temp$`number of deadlines`), by = 1)))
    
                                     #+ scale_x_discrete(limits = c())))
    
  })
  
  ### ICAL download manager
  
  output$ical_download <- downloadHandler(
    
   filename    = 'My_modules.ics',
   content     = function(file) {writeLines(icals(), file)}, 
   contentType = 'text/calendar'
     
  )
  
  ### File updater
  
  update.sheet <- function() {

    ### remove modules already copied across
    data.gs <- gs_read(gs_key("1RQ5lB3Oqp-eoJwe7lkomy15NBJcmKTxnBhyFhAuPTp8"))
    data.gs <- data.gs[!data.gs[, 2] %in% data$Module_Name, ]
    data.gs <- data.gs[!data.gs[, 4] %in% data$Code, ]
    
    
    ### move new modules over
    newrows                <- sum(data.gs$`How many marked assessments does your module have?`, na.rm = TRUE) ### new rows
    base                   <- nrow(data)+1
    end                    <- as.numeric(nrow(data)+newrows)
    data[c(base:end), ]    <- NA
    
    data.gs.long           <- gather(data.gs)
    
    
    ###module info
    
    newmodules                 <- as.numeric(lapply(sapply(data.gs$`How many marked assessments does your module have?`, 
                                                    function(x){seq.int(1, x, by = 1)}), max, na.rm = TRUE))
    data[c(base:end),]$groups  <- paste0('Deadline.', newmodules)
    
    data[c(base:end),]$content <- 'Student Deadline'
    
    
    ### deadlines
    
    newdeadlines               <- data.gs.long[grep("assessment's deadline", data.gs.long$key), 2]
    newdeadlines               <- newdeadlines$value[!is.na(newdeadlines$value)]
    newdeadlines               <- ifelse(substr(newdeadlines, 3, 3) != '/', paste0('0', newdeadlines), 
                                         newdeadlines)
    
    newdeadlines               <- ifelse(substr(newdeadlines, 6, 6) != '/', paste0(substr(newdeadlines, 1, 3),'0', substr(newdeadlines, 4, nchar(newdeadlines))), 
                                         newdeadlines)
    
    
    newdeadlines               <- base::as.Date(newdeadlines, format = "%m/%d/%Y")
    
    data[base:end, 3]          <- newdeadlines
    
    
    ### post date
    
    postdate                  <- data.gs.long[grep("post date", data.gs.long$key), 2]
    postdate                  <- postdate$value[!is.na(postdate$value)]
    postdate                  <- ifelse(substr(postdate, 3, 3) != '/', paste0('0', postdate), 
                                        postdate)
    
    postdate                  <- ifelse(substr(postdate, 6, 6) != '/', paste0(substr(postdate, 1, 3),'0', substr(postdate, 4, nchar(postdate))), 
                                        postdate)
    
    
    postdate                  <- base::as.Date(postdate, format = "%m/%d/%Y")
    
    
    exam                      <- as.character(data.gs.long[grep('form', data.gs.long$key), 2]$value)
    exam                      <- exam[!is.na(exam)]
    end.date                  <- ifelse(exam == 'Exam', as.character(postdate), as.character(newdeadlines))
    end.date                  <- as.Date(end.date)
    
    data[base:end, 4]         <- end.date
    
    
    ### timevis type
    
    data[base:end, 5]         <- ifelse(exam == 'Exam', 'range', 'point')
    
    ### id
    new.ids                   <- seq.int(max(data$id, na.rm = TRUE)+1, max(data$id, na.rm = TRUE)+as.numeric(newrows), 1)
    data[base:end, 6]         <- new.ids
    
    ### Assessment type
    data[base:end, 7]         <- exam
    
    ### Percentage
    Percentages               <- as.numeric(data.gs.long[grep('percentage of the module', data.gs.long$key), 2]$value)
    Percentages               <- Percentages[!is.na(Percentages)]
    data[base:end, 8]         <- Percentages
    
    ### Codes
    Codes                     <- rep(as.character(unlist(data.gs.long[grep('module code', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    data[base:end, 9]         <- Codes
    
    ### Compulsory?
    
    Compulsory                <- rep(as.character(unlist(data.gs.long[grep('module compulsory', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    YearGroup                 <- rep(as.character(unlist(data.gs.long[grep('year group', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    
    YearGroup                 <- ifelse(YearGroup == 'UG1', 1, 
                                        ifelse(YearGroup == 'UG2', 2, 
                                               ifelse(YearGroup == 'UG3', 3, 
                                                      'PGT')))
    
      
    data[base:end, 10]        <- ifelse(Compulsory == 'No', NA, YearGroup)
    
    ### YearGroup
    data[base:end, 17]        <- YearGroup
    
    ### Module name
    Module.names              <- rep(as.character(unlist(data.gs.long[grep('name of your new module', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    data[base:end, 11]        <- Module.names
    
    ### Timevis style
    data[base:end, 12]        <- ifelse(data[base:end, 8] >= 50,'font-weight:bold' ,'font-weight:normal')
    
    ### Is Exam?
    data[base:end, 13]        <- ifelse(exam == 'Exam', 1, 0)
    
    ### Yearmon
    Ym                        <- as.yearmon(as.Date(data[base:end, 3]$start, format = '%Y-%m-%d')) 
    data[base:end, 14]        <- as.character(Ym)
    
    ### week
    data$week                 <- cut.Date(data$start, breaks = '1 week', labels = FALSE)
    
    ### optional?
    optional                  <- rep(as.character(unlist(data.gs.long[grep('Is your module compulsory', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    data[base:end, 16]        <- ifelse(optional == 'No', 0, 1)
    
    ### credits
    
    credits                   <- rep(as.numeric(unlist(data.gs.long[grep('How many credits are awarded for your module', data.gs.long$key), 2])),
                                     times =  c(as.numeric(data.gs$`How many marked assessments does your module have?`)))
    
    data[base:end, 18]        <- credits

    ### add post dates
    
    for(i in base:end) {
      
      if(data[i, 2]$content == 'Student Deadline') {
        
    data[i+length(base:end), ]  <- data[i, ]
    data[i+length(base:end), 2] <- 'Post Date'

    
      }
    }
    
    data[(base+length(base:end)):nrow(data), 3] <- postdate
    data[(base+length(base:end)):nrow(data), 4] <- postdate

    ### return
    return(data[base:nrow(data), ])
    
    }
  
  observeEvent(input$newmodules, {
    
    temp  <- update.sheet()

    gs_add_row(gs_key("1PEELI0uD9U18pfenk0_z92MI0Y8ZJxliREHStRQiAM8"), input = temp)

    session$reload()
    
  })
  

}






### Run App


shinyApp(ui = ui, server = server)
