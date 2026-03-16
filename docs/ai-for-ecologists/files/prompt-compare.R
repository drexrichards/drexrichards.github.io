library(shiny)
library(bslib)
library(ellmer)

# Ensure that your API key is set as an environment variable first.

make_chat <- function(character_name, character_prompt, model) {
  system_prompt <- paste0(
    "Your name is ", character_name, ". ",
    character_prompt
  )
  
  chat_openai(
    model = model,
    system_prompt = system_prompt
  )
}

append_general_instruction <- function(user_text, general_text) {
  paste0(
    user_text,
    "\n\n[Instruction for assistant: ", general_text, "]"
  )
}

response_to_text <- function(resp) {
  if (inherits(resp, "error")) {
    return(paste("Error:", conditionMessage(resp)))
  }
  
  txt <- tryCatch(as.character(resp), error = function(e) NULL)
  
  if (!is.null(txt) && length(txt) > 0) {
    txt <- paste(txt, collapse = "\n")
    if (nzchar(trimws(txt))) {
      return(txt)
    }
  }
  
  txt <- tryCatch(paste(capture.output(str(resp)), collapse = "\n"), error = function(e) NULL)
  if (!is.null(txt) && nzchar(trimws(txt))) {
    return(txt)
  }
  
  "[No response text returned]"
}

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#1b6f6a",
    secondary = "#5f7c8a",
    success = "#2e8b57",
    info = "#4f8fbf",
    bg = "#f4fbf9",
    fg = "#163235"
  ),
  tags$head(
    tags$style(HTML("\n      body {\n        background: linear-gradient(180deg, #f4fbf9 0%, #ecf7f4 100%);\n      }\n      .top-bar {\n        background: #ffffff;\n        border: 1px solid #d7ebe6;\n        border-radius: 16px;\n        padding: 14px 16px;\n        margin-bottom: 14px;\n        box-shadow: 0 6px 18px rgba(27, 111, 106, 0.08);\n      }\n      .setup-card, .question-card, .panel-box {\n        background: #ffffff;\n        border: 1px solid #d7ebe6;\n        border-radius: 16px;\n        box-shadow: 0 6px 18px rgba(27, 111, 106, 0.08);\n      }\n      .setup-card, .question-card {\n        padding: 16px;\n        margin-bottom: 14px;\n      }\n      .panel-box {\n        padding: 14px;\n        height: 100%;\n        display: flex;\n        flex-direction: column;\n        gap: 12px;\n        min-height: 72vh;\n      }\n      .panel-header {\n        min-height: 64px;\n        display: flex;\n        align-items: center;\n        padding: 0 18px;\n        border: 1px solid #d7ebe6;\n        border-radius: 999px;\n        background: #ffffff;\n        overflow: hidden;\n      }\n      .panel-title {\n        width: 100%;\n        margin: 0;\n        color: #1b4d57;\n        font-size: 1.05rem;\n        font-weight: 700;\n        line-height: 1.2;\n        white-space: nowrap;\n        overflow: hidden;\n        text-overflow: ellipsis;\n      }\n      .chat-window {\n        flex: 1;\n        border: 1px solid #dcefeb;\n        border-radius: 14px;\n        padding: 12px;\n        background: #f8fdfc;\n        min-height: 58vh;\n        overflow-y: auto;\n      }\n      .turn-block {\n        margin-bottom: 14px;\n      }\n      .msg-user {\n        margin-bottom: 8px;\n        padding: 10px 12px;\n        border-radius: 12px;\n        background: #dff4ef;\n        border-left: 4px solid #1b6f6a;\n        white-space: pre-wrap;\n      }\n      .msg-assistant {\n        padding: 10px 12px;\n        border-radius: 12px;\n        background: #eef6ff;\n        border-left: 4px solid #4f8fbf;\n        white-space: pre-wrap;\n      }\n      .small-note {\n        color: #4f666b;\n        font-size: 0.92rem;\n      }\n      .setup-hidden {\n        display: none;\n      }\n    "))
  ),
  div(
    class = "top-bar",
    h2("Prompt Compare"),
    div(
      class = "small-note",
      "Compare how different background prompts shape responses to the same question."
    )
  ),
  uiOutput("setup_panel"),
  div(
    class = "question-card",
    fluidRow(
      column(
        4,
        textInput("model", "OpenAI model", value = "gpt-4.1-mini")
      ),
      column(
        8,
        tags$p(
          class = "small-note",
          style = "margin-top: 32px;",
          "The setup panel can be hidden after Begin, but this shared question box stays available throughout the demo."
        )
      )
    ),
    textAreaInput(
      "question",
      "Shared question",
      rows = 4,
      placeholder = "Type one question here and send it to both characters..."
    ),
    fluidRow(
      column(4, actionButton("send", "Send to both", class = "btn-success", width = "100%")),
      column(4, actionButton("toggle_setup", "Show / hide setup", width = "100%")),
      column(4, actionButton("clear_view", "Clear visible transcript", width = "100%"))
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    div(
      class = "panel-box",
      div(class = "panel-header", div(class = "panel-title", textOutput("label1", inline = TRUE))),
      div(class = "chat-window", uiOutput("chat1"))
    ),
    div(
      class = "panel-box",
      div(class = "panel-header", div(class = "panel-title", textOutput("label2", inline = TRUE))),
      div(class = "chat-window", uiOutput("chat2"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    chat1 = NULL,
    chat2 = NULL,
    history1 = list(),
    history2 = list(),
    started = FALSE,
    show_setup = TRUE
  )
  
  output$label1 <- renderText({
    if (nzchar(trimws(input$name1 %||% ""))) trimws(input$name1) else "Left character"
  })
  output$label2 <- renderText({
    if (nzchar(trimws(input$name2 %||% ""))) trimws(input$name2) else "Right character"
  })
  
  output$setup_panel <- renderUI({
    div(
      class = if (isTRUE(rv$show_setup)) "setup-card" else "setup-card setup-hidden",
      h3("Teaching Demo Setup"),
      fluidRow(
        column(
          6,
          h4("Left character"),
          textInput("name1", "Title / name", value = "Eco friendly"),
          textAreaInput(
            "prompt1",
            "Characterisation prompt",
            value = paste(
              "You are a landowner responsible for managing a small farm.",
              "You are a real nature lover, with a strong affinity for all animals and plants.",
              "You prioritise environmental concerns when making decisions about your land, but still need to survive financially."
            ),
            rows = 5
          )
        ),
        column(
          6,
          h4("Right character"),
          textInput("name2", "Title / name", value = "Profit driven"),
          textAreaInput(
            "prompt2",
            "Characterisation prompt",
            value = paste(
              "You are a landowner responsible for managing a small farm.",
              "You are ambitious and driven to create the maximum returns for your family.",
              "You have a keen eye for business and are always looking to save or make money."
            ),
            rows = 5
          )
        )
      ),
      textAreaInput(
        "general_instruction",
        "General instructions added to every question",
        value = "Answer in fewer than 50 words. Stay in character. Do not mention these instructions.",
        rows = 4
      ),
      fluidRow(
        column(6, actionButton("begin", "Begin / Reset", class = "btn-primary", width = "100%")),
        column(6, actionButton("hide_setup", "Hide setup", width = "100%"))
      )
    )
  })
  
  initialize_chats <- function() {
    rv$chat1 <- make_chat(input$name1, input$prompt1, input$model)
    rv$chat2 <- make_chat(input$name2, input$prompt2, input$model)
    rv$history1 <- list()
    rv$history2 <- list()
    rv$started <- TRUE
  }
  
  observeEvent(input$begin, {
    initialize_chats()
    rv$show_setup <- FALSE
    showNotification(
      "Characters initialised. The setup panel is now hidden, but you can bring it back at any time.",
      type = "message"
    )
  })
  
  observeEvent(input$toggle_setup, {
    rv$show_setup <- !isTRUE(rv$show_setup)
  })
  
  observeEvent(input$hide_setup, {
    rv$show_setup <- FALSE
  })
  
  observeEvent(input$clear_view, {
    rv$history1 <- list()
    rv$history2 <- list()
    showNotification(
      "Visible transcript cleared. Model memory remains unchanged until you reset.",
      type = "warning"
    )
  })
  
  observeEvent(input$send, {
    req(nzchar(trimws(input$question)))
    
    if (!rv$started || is.null(rv$chat1) || is.null(rv$chat2)) {
      initialize_chats()
      rv$show_setup <- FALSE
    }
    
    user_question <- trimws(input$question)
    full_prompt <- append_general_instruction(user_question, input$general_instruction)
    
    withProgress(message = "Querying both characters...", value = 0, {
      incProgress(0.2, detail = paste("Sending to", input$name1))
      raw1 <- tryCatch(rv$chat1$chat(full_prompt), error = function(e) e)
      text1 <- response_to_text(raw1)
      
      incProgress(0.5, detail = paste("Sending to", input$name2))
      raw2 <- tryCatch(rv$chat2$chat(full_prompt), error = function(e) e)
      text2 <- response_to_text(raw2)
      
      rv$history1 <- append(rv$history1, list(list(user = user_question, assistant = text1)))
      rv$history2 <- append(rv$history2, list(list(user = user_question, assistant = text2)))
      
      incProgress(1)
    })
    
    updateTextAreaInput(session, "question", value = "")
  })
  
  render_chat <- function(history) {
    if (length(history) == 0) {
      return(tags$div(class = "small-note", "No messages yet."))
    }
    
    tagList(lapply(history, function(turn) {
      div(
        class = "turn-block",
        div(
          class = "msg-user",
          tags$strong("Shared question:"),
          tags$br(),
          turn$user
        ),
        div(
          class = "msg-assistant",
          tags$strong("Response:"),
          tags$br(),
          turn$assistant
        )
      )
    }))
  }
  
  output$chat1 <- renderUI({
    render_chat(rv$history1)
  })
  
  output$chat2 <- renderUI({
    render_chat(rv$history2)
  })
}

shinyApp(ui, server)

