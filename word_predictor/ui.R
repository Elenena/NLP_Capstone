library(shiny)
shinyUI(fluidPage(
    titlePanel("English 4-gram word predictor"),
    p("Type a sentence of any length in the input box. Up to the last 4 words will be used for prediction."),
    p("Prediction is better with English words, but the app also knows a lot of German and Finnish words."),
    p("Punctuation is handled."),
    p("Find more details about this app at",
      a("RPubs",href="https://rpubs.com/Elenena/WordPredictor", target="_blank")),
        mainPanel(
            textInput("sentence", "Type here:", width="150%"),
            p("Next word is:", h3(textOutput("word")))
            
        )

))
