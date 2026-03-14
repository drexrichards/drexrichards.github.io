#### Session 4 - Chatbots continued ####
require(ellmer)
require(ollamar)
require(shinychat)
require(shiny)
require(ragnar)
require(bslib)

#### 1. Initial  ####
# This session focuses on user interface and usability issues for chatbots
# Let's make a simple example and then polish it up
# For the experiment, we will make a simple chatbot, not using RAG. But the same methods will apply to the RAG models from the previous session.
# Set up a user interface with a 'chat_ui' from the shinychat package
ui <- bslib::page_fluid(
  chat_ui("chat")
)

# Set up a server with an OpenAI -powered LLM inside
server <- function(input, output, session) {
  chat <- ellmer::chat_openai(  model = "gpt-4o",        
                                system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital.")
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
#shinyApp(ui, server)


#### 2. Add a welcome message from the AI ####
# Welcome messages can be added when creating the user interface using chat_ui
ui <- bslib::page_fluid(
  chat_ui("chat",
          messages = list(
            "**Hi!** 
              Please let me know how I can help with your queries about ecosystem services in Singapore."
          ))
)

# Set up a server with an OpenAI -powered LLM inside
server <- function(input, output, session) {
  chat <- ellmer::chat_openai(  model = "gpt-4o",        
                                system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital.")
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
# shinyApp(ui, server)

#### 3. Add a customised icon ####
# Custom icons can be added when creating the user interface using chat_ui
# In Shiny, images must be served from a folder called "www" in the same folder as the .R script
# Because our chatbot is about Singapore, let's use a photo of a local bird
# The photo provided is of a Pink-necked green pigeon (Treron vernans) 
# The photo was taken by Fung Tze Kwan, a brilliant ecologist at the National University of Singapore
# Tze Kwan's web page: https://www.nus.edu.sg/cncs/fung-tze-kwan/
addResourcePath("assets", file.path(getwd(), "www"))

ui <- page_fillable(
  chat_ui(
    "chat",
    messages = list(
      list(
        role = "assistant",
        content = "**Hi!** Please let me know how I can help with your queries about ecosystem services in Singapore."
      )
    ),
    icon_assistant = tags$img(
      src = "assets/pngp.png",
      width = "32px",
      height = "32px",
      style = "border-radius: 50%; object-fit: cover;"
    )
  )
)

# Set up a server with an OpenAI -powered LLM inside
server <- function(input, output, session) {
  chat <- ellmer::chat_openai(
    model = "gpt-4o",
    system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital."
  )
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
# shinyApp(ui, server)

#### 4. Change the colour scheme ####
# Changing the colour scheme can also be done when creating the user interface using chat_ui
# This requires making some changes to the HTML styling, so is a bit more involved - we can't just use R
# HTML can be fiddly, as the formatting is critical
# The colours are given as hexadecimal colour value codes
addResourcePath("assets", file.path(getwd(), "www"))

ui <- bslib::page_fluid(
  tags$style(HTML("
    /* Whole page background */
    body {
      background-color: #f0fdf4;
    }

    /* Assistant message bubble */
    #chat .shinychat-assistant .shinychat-message-body {
      background-color: #bbf7d0;
    }

    /* User message bubble */
    #chat .shinychat-user .shinychat-message-body {
      background-color: #dbeafe;
    }
  ")),
  
  chat_ui(
    "chat",
    messages = list(
      list(
        role = "assistant",
        content = "**Hi!** Please let me know how I can help with your queries about ecosystem services in Singapore."
      )
    ),
    icon_assistant = tags$img(
      src = "assets/pngp.png",
      width = "32px",
      height = "32px",
      style = "border-radius: 50%; object-fit: cover;"
    )
  )
)

server <- function(input, output, session) {
  chat <- ellmer::chat_openai(
    model = "gpt-4o",
    system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital."
  )
  
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
# shinyApp(ui, server)


#### 5. Add a disclaimer ####
# Chatbots can often get things wrong, so we may want to add a disclaimer
# The following code inserts a small disclaimer box with custom text, into the chatbot
disclaimer_text <- "This chatbot is for educational demonstration only and may produce incomplete or incorrect answers.
Do not rely on it for professional or policy decisions."

addResourcePath("assets", file.path(getwd(), "www"))

ui <- bslib::page_fluid(
  # Colour customisation
  tags$style(HTML("
    /* Whole page background */
    body {
      background-color: #f0fdf4;  
    }

    /* Assistant message bubble */
    #chat .shinychat-assistant .shinychat-message-body {
      background-color: #bbf7d0;  
    }

    /* User message bubble */
    #chat .shinychat-user .shinychat-message-body {
      background-color: #dbeafe; 
    }
  ")),
  
  # Chat interface
  chat_ui("chat",
          messages = list(
            "**Hi!** 
              Please let me know how I can help with your queries about ecosystem services in Singapore."
          ),
          icon_assistant = tags$img(
            src   = "assets/pngp.png",
            width = 32,
            height = 32,
            style = "border-radius: 50%;"
          )),
  
  # Disclaimer box
  div(
    style = "
      margin-top: 15px;
      padding: 10px;
      border-left: 4px solid #f59e0b;
      background-color: #fff8e1;
      border-radius: 6px;
      max-width: 500px;
    ",
    strong("Disclaimer: "),
    disclaimer_text  
  )
)


# Set up a server with an OpenAI -powered LLM inside
server <- function(input, output, session) {
  chat <- ellmer::chat_openai(  model = "gpt-4o",        
                                system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital.")
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
# shinyApp(ui, server)



