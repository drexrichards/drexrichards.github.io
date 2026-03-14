#### Session 3 - Chatbots ####
require(ellmer)
require(ollamar)
require(shinychat)
require(shiny)
require(ragnar)
require(bslib)

#### 1. Simple chatbot using ChatGPT ####
# Set up a user interface with a 'chat_ui' from the shinychat package
ui <- bslib::page_fluid(
  chat_ui("chat")
)

# Set up a server with an OpenAI -powered LLM inside
server <- function(input, output, session) {
  chat <- ellmer::chat_openai(  model = "gpt-4o",        
                                system_prompt = "You are an expert on global sustainability.")
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This line opens the Shiny App
# shinyApp(ui, server)


#### 2. Retrieval augemented generation ####
# For a better response in a chatbot, we could use RAG
# This allows us to provide a dataset of reference material, which can be retrieved
# The chatbot will thus have specific information, and can provide supporting references

# Let's import a dataset of summaries of academic papers published about ecosystem services in Singapore
ragin<- read.csv("Data/sgliterature2.csv")
# The dataset has 3 columns; a bibliographic reference, a doi, and some summary text describing the key findings
ragin[1,]

# RAG uses a data store. We will set up a duckDB data store using the ragnar package
# Set up store file path
store_location <- "Data/sglit.duckdb" 

# # Create store using embeddings. This is fiddly and best started from scratch.
# # If you need to re-start, it is best to manually delete the whole duckDB file and start from scratch
# embedder <- \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")  
# store <- ragnar_store_create(
#   store_location,
#   embed = embedder,
#   version = 1,
#   name  = "sg_literature",
#   title = "Singapore ecosystem services",
#   extra_cols = data.frame(
#     reference    = character(),  
#     doi_url     = character(),
#     text         = character())
# )
# # Ingest data
# ragnar_store_insert(store, ragin)   
# # Build index
# ragnar_store_build_index(store)

# Now, we have built the store! 
# If we want to come back again later, the store is saved on our hard drive.
# So, we can skip to just reconnect to it, e.g.
store<- ragnar_store_connect(store_location)

# Now, we can create a Chat and give it the 'tool" of accessing our data store
# Create client model and system prompt
client <- chat_openai(model = "gpt-4.1", 
                      system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital. You have access to one knowledge store tool. 
  The 'SGES' knowledge store contains summaries of relevant scientific publications.
  If no relevant results are found, inform the user and do not attempt to answer the question.
  If the user request is ambiguous, perform at least one search first, then ask a clarifying question.
  For referencing the SGES knowledge store, use a hyperlink to the provided 'doi_url' data field. 
  You can also provide a bibliography from the full citation details provided in the 'reference' field.
"
                      )
# Register retrieval tool on client
ragnar_register_tool_retrieve(
  client, store,
  top_k = 20,
  name ="SGES",
  store_description = "Use for published scientific literature about ecosystem services and biodiversity in Singapore."
)

# Set up the server as before
server <- function(input, output, session) {
  chat <- client
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This commented out line line opens the Shiny App 
# shinyApp(ui, server)


#### 3. Open-source version ####
# We need an embedding model to build the RAG data store, on top of our LLM used for conversation
ollamar::pull("nomic-embed-text")

# We are also going to need a different LLM, because the very minimal tinyllama we used previously doesn't support tool use
# Let's use a very small version of Qwen2.5, an open-source model from Alibaba
ollamar::pull("qwen2.5:0.5b")

# Then, most of the steps are the same
Ostore_location <- "Data/sglitOPEN.duckdb"

# # Create store using embeddings. This is fiddly and best started from scratch.
# # If you need to re-start, it is best to manually delete the whole duckDB file and start from scratch
# embedderOPEN <- \(x) ragnar::embed_ollama(x, model = "nomic-embed-text")
# Ostore <- ragnar_store_create(
#   Ostore_location,
#   embed = embedderOPEN,
#   version = 1,
#   name  = "sg_literature_open",
#   title = "Singapore ecosystem services",
#   extra_cols = data.frame(
#     reference    = character(),
#     doi_url     = character(),
#     text         = character())
# )
# # Ingest data
# ragnar_store_insert(Ostore, ragin)
# # Build index
# ragnar_store_build_index(Ostore)

# Now, we have built the store!
# If we want to come back again later, the store is saved on our hard drive.
# So, we can skip to just reconnect to it, e.g.
Ostore<- ragnar_store_connect(Ostore_location)

# Now, we can create a Chat and give it the 'tool" of accessing our data store
# Create client model and system prompt
Oclient <- chat_ollama(model = "qwen2.5:0.5b",
                      system_prompt = "You are an expert on nature in Singapore, especially ecosystem services and natural capital. You have access to one knowledge store tool.
  The 'SGES' knowledge store contains summaries of relevant scientific publications.
  If no relevant results are found, inform the user and do not attempt to answer the question.
  If the user request is ambiguous, perform at least one search first, then ask a clarifying question.
  For referencing the SGES knowledge store, use a hyperlink to the provided 'doi_url' data field.
  You can also provide a bibliography from the full citation details provided in the 'reference' field.
"
)
# Register retrieval tool on client
ragnar_register_tool_retrieve(
  Oclient, Ostore,
  top_k = 20,
  name ="SGES",
  store_description = "Use for published scientific literature about ecosystem services and biodiversity in Singapore."
)

# Set up the server as before
Oserver <- function(input, output, session) {
  chat <- Oclient
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

# This commented out line line opens the Shiny App
# shinyApp(ui, Oserver)
