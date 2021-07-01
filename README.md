# Natural Language Prediction Shiny App
## Coursera's Data Science Specialisation CAPSTONE Project

### Overview
This project is based on a corpus called HC Corpora containing sentences from blogs, tweets and online newspapers in 4 languages (English, German, Finnish and Russian). Data in the corpus are already divided by language, even if some words or parts of sentences in a foreign language can appear in a language database.  
The aim of the project is to build a Shiny app taking as input a sequence of English words and giving as output a meaningful prediction of the next English word. Optionally, prediction for other languages can be included. 
Among the main issues to be handled, the more relevant are:  
-    **Accuracy**: it obviously tends to increase with the number of previous words used for prediction and with the number of words included in the dictionary used by the algorithm.    
-    **Speed**: I wanted a prediction time small enough to be unnoticeable  when using the app, so that prediction could be done real-time while the user is typing. As prediction is essentially a search in the rows of a dataframe, the time needed increases when the dataframe is longer, or heavier (objects in rows requiring more bytes of memory), or when the dataframe has to be loaded in RAM. Conversely, time needed for prediction decreases when the dataframe is already loaded in memory, when a smart search algorithm (e.g. binary search) is used and when the data table is split in several smaller data tables.    
-    **Memory limits**: Currently, Shiny free server allows a max of 1Gb of allocated RAM for each app. This limits the possible size of data tables as well as the maximum number of objects that can be loaded at the same time. Also, the type of object stored in the cells of the tables heavily impacts on memory: an R character vector is way larger than an R integer vector.

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/dict.png)
