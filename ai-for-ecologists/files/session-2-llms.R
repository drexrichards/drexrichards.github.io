#### Session 2 - LLMs ####
#require(httr)
#require(jsonlite)
require(ellmer)
require(ollamar)
library(pdftools)

#### 1. Accessing the OpenAI API ####
# We are going to use the ellmer package to access LLMs
# First we will use a commercial model provided by OpenAI.
# Let's ask for an opinion about New Zealand's sustainability.

# To access an OpenAI model through the OpenAI, you will need an API key.
# A safe way to do this is to do it from outside this script, setting as an environment variable
# For example, Sys.setenv(OPENAI_API_KEY = "insert your key here)

# Open a chat using the 'chat_openai' function
newchat <- chat_openai(
  model = "gpt-4o",        
  system_prompt = "You are an expert on global sustainability.")
# Send question to the chat and print the last message
res <- newchat$chat("How sustainable is New Zealand's economy?" , echo = "none")
print(res)

# Let's try to access an open-source alternative via the ollamar package
# There is a bit more set-up to do for ollamar because we need to install the model we want to run locally
# First let's see if we have any models installed
list_models()
# The 'pull' function can install a specific model from the archive
pull("tinyllama:1.1b") # This is a relatively small model which can run on a reasonably powerful laptop
# Once we have a model, we can use it through ellmer using a similar approach
# Open a chat using 'chat_ollama' function
newchat <- chat_ollama(
  model = "tinyllama:1.1b",        
  system_prompt = "You are an expert on global sustainability.")
# Send question to the chat and print the last message
res <- newchat$chat("How sustainable is New Zealand's economy?" , echo = "none")
print(res)


#### 2. Asking a series of questions ####
# In environmental social science we might be interested in asking land use questions to some virtual "Agents"
# This is increasingly common in simulation studies and agent-based modelling
# LLMs are a powerful tool for this, as we can repeatedly send prompts to a model and save the responses.

# Let's consider a situation where a farmer might change part of their farm from sheep to native forestry.
# Whether or not a farmer will plant trees will depend on lots of factors, 
# including the size of the financial incentive available. 
# We can make a loop to test the response of different models; e.g. the two we used earlier.


# Let's test different incentive levels, ranging from $0 to $500
dollars<- seq(0,500, by = 40)
# Make a dataframe to save the responses
agentq <- data.frame(dollars = dollars, 
                        chatgpt = NA,
                        open = NA)
# The system text is background information for the LLM
  systemtext<-"You are a New Zealand sheep farmer making a land use decision."
  
# The prompts should all be the same, only varying in the $ amount of th eincentive 
  qulist<- paste0("You are a sheep farmer in New Zealand, with 150 hectares of land. You make an average profit of $438 per hectare from farming per year. ",
                  " You are making a decision about whether to plant native trees on your land instead of farming sheep next year. ",
                  "The cost of planting native trees is $100 per hectare and you would lose the annual profit from sheep farming. ",
                  "The government has offered to pay you $",
                  dollars,
                  " per hectare year if you plant the native trees. Please make this decision while thinking about your livelihood. ",
                  "Please choose how many hectares of land to convert to native trees by indicating the number of hectares.",
                  "Do not provide any other text or response, only a number between 0 and 150 indicating the number of hectares")
# Note that the prompt includes specifications on HOW the LLM should give the response back (just a number, no extra text)
  
# Send the loop through, calling the OpenAI API  
  for( j in 1:length(qulist)){
    messages_df <- data.frame(
      role = c("system", "user"),
      content = c(systemtext, qulist[j]),
      type = c("text", "text"),
      stringsAsFactors = FALSE
    )

    # Open a chat
    farmerchat <- chat_openai(
      model = "gpt-4o",        
      system_prompt = systemtext)
    # Send question to the chat and receive last message
    res <- farmerchat$chat(qulist[j], echo = "none")
    agentq[j,2]  <- as.numeric(res)
}

# Let's see what the LLM came up with
plot(chatgpt ~ dollars,pch=16, col="red", data = agentq, xlab= "Incentive ($)", ylab = "Forested area (ha)")
m1<- glm(cbind(chatgpt, 150-chatgpt) ~ dollars, data = agentq, family= "binomial")
lines(1:500, predict(m1, newdata = data.frame(dollars = 1:500), type = "response")*150, lty = 3)
# We plot the raw numbers reported by the LLM, alongside a fitted regression line


#### 3. Repeat the survey with an open-source model ####
  for( j in 1:length(qulist)){
    messages_df <- data.frame(
      role = c("system", "user"),
      content = c(systemtext, qulist[j]),
      type = c("text", "text"),
      stringsAsFactors = FALSE
    )
    
    # Open a chat
    farmerchat <- chat_ollama(
      model = "tinyllama:1.1b",        
      system_prompt = systemtext)
    # Send question to the chat and receive last message
    res <- farmerchat$chat(qulist[j], echo = "none")
    agentq[j,3]  <- as.numeric(res)
}
# We have a problem! The loop works but seems to give no data responses, look;
agentq$open

# We can inspect one response from the LLM, to get a better idea of what is happening;
res

# The response from the LLM has not followed our request to only return a single number. 
# Depending on how the LLM responds you might gain some insight into whether it is capable of answering this type of relatively complex request.
# Not all models are useful for the task at hand, perhaps we should try a different one in future.


#### 4. Press release example ####
# Let's use an LLM for something different - writing a press release for a paper
# First we will extract all the text from a .pdf
pdf_path<- "Data/Richards et al 2024.pdf" # You may use any paper that you like
papertext<- pdftools::pdf_text(pdf_path)

# Open a chat using the 'chat_openai' function
newchat <- chat_openai(
  model = "gpt-4o",        
  system_prompt = "You are an expert in writing press releases for scientific papers.")
# Add a background message, paste the paper text, and request for a press release
res<- newchat$chat(paste0("Please read the following paper and prepare a press release;\n\n",
                    papertext,
                    "\n The press release should be less than 500 words."), echo = "none")
print(res)

# We can also ask a follow-up request, e.g. to write a social media post
res2<- newchat$chat(paste0("Thanks, please could you now write a suitable social media post e.g. for LinkedIn?"), echo = "none")
print(res2)



