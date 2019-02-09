library(shiny)
# remotes::install_github("ColinFay/nessy")
library(nessy)
library(dplyr)
library(purrr)

tweets <- readRDS(here::here("data", "djt_joined.rds")) %>% 
  filter(grepl(" ", text)) %>% 
  group_by(top_category) %>% 
  mutate(i = row_number()) %>% 
  filter(i %in% sample(1:n(), min(n(), 50)) | top_category == "Executive Time") %>% 
  ungroup()
  
event_types <- c("Executive Time" = "Executive Time",
                 "Travel" = "Travel",
                 "Unknown" = "Unknown Time at \"Work\"",
                 "Meeting" = "A Meeting",
                 "Lunch" = "Lunch",
                 "Event" = "An Event",
                 "Weekend" = "The Weekend")

`%??%` <- function(x, y) if (is.na(x)) y else x
`%???%` <- function(x, y) if (!is.na(x)) y
`%||%` <- function(x, y) if (is.null(x)) y else x
`%>>%` <- function(x, .f) if (!is.null(x)) .f(x)

N_ANSWER_OPTIONS <- 3L

ui <- cartridge(
  "Trump Tweet Time",
  tags$style(HTML(
    "body {max-width: 900px; margin: auto;}
    @media only screen and (max-width:800px) {
      body {
        font-size: 10px;
      }
      html {
        font-size: 10px;
      }
      h1 {
        font-size: 19px;
      }
    }
    "
  )),
  tags$head(HTML(
  '
  <title>Trump Twitter Time!</title>
  <meta property="og:title" content="Trump Twitter Time!">
  <meta property="og:description" content="A simple game for your Executive Time">
  <meta property="og:url" content="https://apps.garrickadenbuie.com">
  <meta name="twitter:card" content="summary">
  <meta name="twitter:creator" content="@grrrck">
  <meta name="twitter:site" content="https://garrickadenbuie.com">
  <link rel="apple-touch-icon" sizes="57x57" href="apple-icon-57x57.png">
  <link rel="apple-touch-icon" sizes="60x60" href="apple-icon-60x60.png">
  <link rel="apple-touch-icon" sizes="72x72" href="apple-icon-72x72.png">
  <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76.png">
  <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-114x114.png">
  <link rel="apple-touch-icon" sizes="120x120" href="apple-icon-120x120.png">
  <link rel="apple-touch-icon" sizes="144x144" href="apple-icon-144x144.png">
  <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152.png">
  <link rel="apple-touch-icon" sizes="180x180" href="apple-icon-180x180.png">
  <link rel="icon" type="image/png" sizes="192x192"  href="android-icon-192x192.png">
  <link rel="icon" type="image/png" sizes="32x32" href="favicon-32x32.png">
  <link rel="icon" type="image/png" sizes="96x96" href="favicon-96x96.png">
  <link rel="icon" type="image/png" sizes="16x16" href="favicon-16x16.png">
  <link rel="manifest" href="manifest.json">
  <meta name="msapplication-TileColor" content="#ffffff">
  <meta name="msapplication-TileImage" content="ms-icon-144x144.png">
  <meta name="theme-color" content="#ffffff">
  <link rel="icon" href="favicon.ico" type="image/x-icon">
  ')),
  container_with_title(
    title = "When did Trump tweet...?",
    uiOutput("tweet_balloon")
  ),
  uiOutput("score"),
  container_with_title(
    title = "During...",
    uiOutput("ask_answer_ui")
  ),
  tags$p(),
  tags$p(
    style = "color: #999;",
    "Based on White House schedules", 
    tags$a(href = "http://bit.ly/2UGM0fw", "released by Axios.")
  ),
  tags$p(
    style = "color: #999;",
    "App by",
    tags$a(href = "https://twitter.com/grrrck", HTML("&commat;grrrck")),
    "using",
    HTML(paste0(
      # tags$a(href = "https://twitter.com/search?q=%23rstats", "#rstats"), ", ",
      tags$a(href = "https://rtweet.info", "rtweet"), ", ", 
      tags$a(href = "https://shiny.rstudio.com", "Shiny"), ", and ",
      tags$a(href = "https://github.com/ColinFay/nessy", "nessy"), ".")
    )
  )
)

server <- function(input, output, session) {
  SESSION_ID <- paste0(sample(c(0:9, letters[1:3]), 16, replace = TRUE))
  tweet <- reactiveVal(tweets %>% sample_n(1))
  
  output$tweet_balloon <- renderUI({
    tagList(
      balloon(HTML(tweet()$text), style = "margin-left: 40px;"),
      tags$br(),
      tags$img(src = "trump.png")
    )
  })
  
  ask_mode <- reactiveVal(TRUE)
  answer_was_correct <- reactiveVal(FALSE)
  score <- reactiveVal(0)
  
  labels <- reactive({
    req(tweet())
    required <- tweet() %>% pull(top_category)
    others <- setdiff(names(event_types), required)
    labels <- c(required, sample(others, N_ANSWER_OPTIONS - 1))
    sample(labels, N_ANSWER_OPTIONS)
  })
  
  output$ask_answer_ui <- renderUI({
    s_tweet <- isolate(tweet())
    if (ask_mode()) {
      r_labels <- labels()
      tagList(
        button_primary("answer_1", r_labels[1]),
        if (N_ANSWER_OPTIONS >= 2) button_warning("answer_2", r_labels[2]),
        if (N_ANSWER_OPTIONS >= 3) button_success("answer_3", r_labels[3]),
        if (N_ANSWER_OPTIONS >= 4) button_error(  "answer_4", r_labels[4])
      )
    } else {
      tagList(
        div(
          style="display: block;float: left;padding-top: 3px;",
          if (answer_was_correct()) nessy::star("medium") else nessy::close("medium")
        ),
        div(
          style = "padding-left: 70px;",
          event_types[s_tweet$top_category] %>% tags$h2(),
          s_tweet$created_at %>% strftime("%A %b, %e at %l:%M%P") %>% tags$h3(),
          s_tweet$listed_title %???% if (
            tolower(s_tweet$listed_title) != tolower(s_tweet$top_category)
          ) tags$h4(s_tweet$listed_title),
          s_tweet$listed_location %???% tags$h4(s_tweet$listed_location, style = "color: #555;"),
          s_tweet$time_start %???% tags$p(
            style = "color: #555;",
            strftime(s_tweet$time_start, "Sched: %l:%M%P"), "to", 
            strftime(s_tweet$time_end, "%l:%M%P"),
            (s_tweet$notes %??% NULL) %>>% tags$em
          )
        ),
        tags$br(),
        button("next_tweet", if (answer_was_correct()) "Do Another!" else "Try Again!")
      )
    }
  })
  
  log_answer <- function(label, status_id, file = "responses.log") {
    cat(SESSION_ID, strftime(Sys.time(), ',"%F %T %Z",'), 
        status_id, ',"', label, '"\n', 
        sep = "", file = file, append = TRUE)
  }
  
  observeEvent(input$next_tweet, {
    if (input$next_tweet > 0) {
      ask_mode(TRUE)
      tweet(tweets %>% sample_n(1))
    }
  })
  
  observe({
    if (!isolate(ask_mode())) return()
    s_tweet <- isolate(tweet())
    s_labels <- isolate(labels())
    
    btn_ids <- paste0("answer_", 1:N_ANSWER_OPTIONS)
    btns <- map_int(btn_ids, ~ input[[.]] %||% 0L)
    # cat("", capture.output(str(btns)), sep = "\n")
    btn_picked <- which(btns != rep(0L, N_ANSWER_OPTIONS))
    if (!length(btn_picked)) return(NULL)
    s_label <- s_labels[btn_picked]
    
    answer_was_correct(FALSE)
    answer_was_correct(s_label == s_tweet$top_category)
    # cat("\npicked:", s_label, "\twas:", s_tweet$top_category)
    
    log_answer(s_label, s_tweet$status_id)
    ask_mode(FALSE)
  }, priority = -1000)
  
  observeEvent(answer_was_correct(), {
    if (answer_was_correct()) score(score() + 1L)
  })
  
  output$score <- renderUI({
    if (score() >= 10) {
      tags$h3(
        tags$span(class = "icon star small"),
        "You Win!",
        tags$span(class = "icon star small")
      )
    } else if (score() > 0) {
      tags$h3("Your Score:", score())
    } 
  })
}

shinyApp(ui = ui, server = server)
