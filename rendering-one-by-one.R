system("quarto render index.qmd")
system("quarto render research.qmd")
system("quarto render methods.qmd")
system("quarto render singapore.qmd")

system("quarto render ai-for-ecologists/index.qmd")
system("quarto render ai-for-ecologists/session-2-llms.qmd") # slowest one, invloves runnign code
system("quarto render ai-for-ecologists/session-1-started.qmd")
system("quarto render ai-for-ecologists/session-3-chatbots.qmd")
system("quarto render ai-for-ecologists/session-4-chatbots-continued.qmd")