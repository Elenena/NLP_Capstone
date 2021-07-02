# Natural Language Prediction Shiny App
## hosted at https://elenena810.shinyapps.io/word_predictor/
## Coursera's Data Science Specialisation CAPSTONE Project

### Overview
This project is based on a corpus called HC Corpora containing sentences from blogs, tweets and online newspapers in 4 languages (English, German, Finnish and Russian). Data in the corpus are already divided by language, even if some words or parts of sentences in a foreign language can appear in a language database.  
The aim of the project is to write R code and finally build a Shiny app taking as input a sequence of English words and giving as output a meaningful prediction of the next English word. Optionally, prediction for other languages can be included. 
Among the main issues to be handled, the more relevant are:  
-    **Accuracy**: it obviously tends to increase with the number of previous words used for prediction and with the number of words included in the dictionary used by the algorithm. In any case, it's impossible to reach an accuracy near to 100%, because there is no "right answer" in this kind of prediction.    
-    **Speed**: I wanted a prediction time small enough to be unnoticeable  when using the app, so that prediction could be done real-time while the user is typing. As prediction is essentially a search in the rows of a dataframe, the time needed increases when the dataframe is longer, or has a bigger size (objects in rows requiring more bytes of memory), or when the dataframe has to be loaded in RAM (loading speed depending also on file format). Conversely, time needed for prediction decreases when the dataframe is already loaded in memory, when a smart search algorithm (e.g. binary search) is used and when the data table is split in several smaller data tables.    
-    **Memory limits**: Currently, Shiny free server allows a max of 1Gb of allocated RAM for each app. This limits the possible size of data tables as well as the maximum number of objects that can be loaded at the same time. Also, the type of object stored in the cells of the tables heavily impacts on memory: an R character vector is way larger than an R integer vector.  Finally, memory employed depends on the level of compression of files.      
-    **Special tokens**: in a text, there are parts that is probably best to regroup in some kind of special tokens; for example, it doesn't make much sense to consider every different weblink as a different word. But when the algorithm predicts a weblink, how to choose which link to dispay? (I adressed the problem, but my solution doesn't fully satisfy me).      
-    **Out of dictionary words**: if a word is not present in the training dataset, the algorithm must know what to do with it.    
-    **Upper/lowercase words**: the algorithm can be case insensitive with the words used for prediction, but on the other hand the predicted word has to be correctly capitalized when necessary. 

### My solutions 
My model, for **English words**, will perform a prediction based on the **4 previous words**. Basically, I built dataframes containing all the sequences of 4 words found in the training dataset and I chose, for each one, the most frequent 5th word, that will be the result of prediction. When the exact sequence is not found in the dataframe (or when only 3 words are typed), the first word will be ignored and the search will be done in a dataframe with 3 words as input and the most frequent 4th word as output, and so on for prediction with 2 words and ultimately with one word.

When the last typed word is not in my English dictionary, a dataframe with **German and Finnish** words will be used, associating each input word the following most frequent word (prediction with **1 previous word**). 

If the last typed word is not found even in foreign dictionary, the output will be the most frequent English word (i.e. *"the"*). 

For computation, I used base R and 3 packages: dplyr, tidyr and data.table. Final app requires only two libraries: shiny and data.table.

My training set was a random sample containing the 70% of the English corpus and consisted of nearly 3 millions of sentences of various length. 
Sentences were split based on whitespaces and English punctuation rules, obtaining nearly **790000 different words**, including 6 special tokens I introduced: \<BADWORD\> (for profanity filtered words), \<WEBLINK\>, \<MAIL_ADRESS\>, \<NUMBER\>, \<PRICE\>, \<DATE\>. I used all these words to build a frequency-sorted dictionary where each word is associated with an integer number. The dictionary has 3 columns:    
-    code: integers corresponding to the frequency ranking of the word (1 for the most frequent).    
-   words: the word in its most frequent upper/lowercase version (e.g., "I" and not "i", "London" and not "london").  
-   capital: the word in uppercase (this column is needed to speed up the search during the  prediction process).    
Here are the first lines of the dictionary (line 11 is one of my custom tags):

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/dict.png)

Next step was to convert each splitted sentence into a "coded" version, i.e. **each word was replaced with the integer in the code column of the dictionary**. This step was time-consuming, but it allowed me to speed up all the following computation and to work with large dataframes that otherwise my laptop (and, ultimately, Shiny server) couldn't handle. In fact, size of later created dataframes was reduced of 12 times with respect to dataframes containing words as character vectors.  

After that, I repeatedly looped over each sentence to find all possible 2-grams, 3-grams, 4-grams and 5-grams, and built dataframes with 1 column for each position. This is how the dataframe for pentagrams looked like:

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/penta.png)

Then, for each sequence of n-1 words, I selected only the n word with the max frequency. So, for example, in the previous image, only the first row is been kept, meaning that when the user types the words corresponding to the code *1, 217, 5, 1* ("the", "end", "of", "the"), the word corresponding to the code *73* ("day") will be predicted.

Eventually, to save as much memory as possible, those 4 **dataframes were joined** based on all the existing colums obtaining a unique dataframe with nearly 50 millions of rows. An integer column called "best" was added to denote if that row contains, besides the best prediction with 4 words, the best prediction with a lower number of words. So for example, in this image:

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/df4.png)

-   all the rows show the best prediction with 4 words;    
-   the row with "best" equal to 20 also contains the best prediction for the code *6, 64012*, which is *15*, and doesn't contain best prediction with 1 or 3 words    
-   the first row, were "best" is equal to 23, contains the best prediction for *6, 63820* (*10*) and also the best prediction for *6, 63820, 10*, which is *11755*.

At this point, I had to face memory limitation of Shiny server and also find a way to speed up the dataframe filtering. For those reasons, direct use of this last dataframe was unfeasible. So, I split it in **225 dataframes** based on the first word (column V1) as follows:   
-   The first 25 are one for each of the first words (or tokens) in the dictionary. Column V1 was removed from them, as it became unuseful.   
- The last 200 contain each various V1 codes, and another light dataframe was added to tell the app in which dataframe to look based on the V1 input word, like this:

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/number.png)

All those dataframes, and also the dictionary, were converted to **data tables with appropriate keys for binary searching** (much faster than dplyr filter() function) and **saved as RDS files**. This is the ideal format according to my trials, because it is compressed but at the same time can be loaded in a really fast way. 

The first **25** dataframes, statistically, will be used very often in prediction, as words from 1 to 25 account for 30% of the cumulative frequency of words in the training dataset. Also, they are the largest and so the slowest to load. For those reasons, they are **preloaded in the app** (Shiny will load all of them at the beginning of each session). Conversely, each of the other **200 will be loaded only when needed**, i.e. when the first word to be used in prediction is in column V1 of that dataframe.

**Foreign languages**

For German and Finnish, I built a unique very simple data table, containing in the first column the words covering the 90%  of cumulative frequency of a 200000 sentences sample for **German** (nearly **24000** words), and words covering 80% of cumulative frequency of a 200000 sentences sample for **Finnish** (approx **47000** words). The priority I used for words appearing in more than a language was English > German > Finnish (so for example "die" is treated as an English word, not as a German one).   
In the second column of this dataframe, there is simply the most frequent following word, based on my 200000 sentences samples.


Final steps were:   
-   Creation of a function to divide in words the input sentence, also performing **profanity filtering** and conversion of special tokens.  
-   Creation of a function assigning the most frequent value when prediction was one of my custom tokens (e.g. all predicted numbers are "2"). For date, as the corpora is quite old, after testing I substituted predicted date with the date of the current day. For profane words, "\<badword\>" will be displayed.   
-   Creation of a function to **capitalize** predicted word when preceded by a period, a question mark and other relevant punctuation.

### Model performance

The total size of the files used by the app is **308Mb**.
On Shiny server, the app loads in 10-15 seconds and allocated RAM (based on shinyapp.io metrics) is about **800Mb** (it can reach 900Mb when typing or erasing very fast, but as far as I tested it, it never crashed).

For speed and accuracy test, the workhorse function (which has embedded all the accessory functions) was tested against a sample containing the 10% of the HC Corpora that wasn't used to train the model (853900 sentences). The test was run in a local non-interactive R session and the goal was the prediction of the final word of each sentence.  
**Mean time** for a prediction was **0.0982 seconds**.   
**Mean accuracy** (percentage of words correctly predicted) was **46.52%**. Measured accuracy was case-insensitive, i.e. if correct word is "Word" and predicted word is "word", the prediction is considered correct. *This was necessary due to an oversight at the very beginning of the job, as I'll explain later in this file*.    
As the sentences used have various lengths, but most of them are 5 words or longer, this percentage reflects mostly prediction when 4 words were typed. So, I subsetted the results to determine accuracy with each number of predictors:     
-   with 1 word: accuracy= 10.48%   
-   with 2 words: accuracy= 22.66%    
-   with 3 words: accuracy= 35.87%  
-   with 4 words: accuracy= 49.20%  

Here are some examples of test results (the first 2 columns have been previously converted to uppercase):

![Alt text](https://github.com/Elenena/NLP_Capstone/blob/main/screenshots/test.png)

Some final statistics:  
-   **82%** of the correct words to predict can be found in my English dictionary   
-   **1.45%** of the correct words to predict can be found in my foreign languages table    
-   There were 82059 different correct words in the the testing dataset, while my algorithm predicted 50062 different words.  
-   As expected, in a certain amount of sentences the app didn't know which word to predict and so used the word *"the"*. In fact, this word was predicted 56456 times, while it appears only 1049 times in the "correct" column.

### Note about code and reproducibility

In this repo, you can find all the code needed to generate the files used by the app (code_for_tables_creation.R), the server.R and ui.R functions (in the word_predictor folder) and code for testing (code_for_testing.R). You can also download the dataframe with test results in the validationresults folder, if you want to examine it in detail.  The screenshots folder is used to build this file, while the Milestone.Rmd file contains an initial approach to the project.

Unfortunately, I forgot to set the seed at the very beginning of the code_for_tables_creation.R, when sampling training, validation and testing datasets, and realized it after more than 1 month of work. This means that if you decide to run my code, you will obtain an app analogous but not identical to mine. This had also an impact on the way I handled upper/lowercase problem. In fact, I initially worked only on uppercase words, and when I adressed case sensitivity I wasn't able to retreive the original version of the uncapitalized training set (this is explained in the R file).  

Also note that running this code could take a very long time and requires at minimum a machine with 8Gb of RAM that is not doing other computations in the meanwhile. With R 4.0.2 on my Windows10 X64 machine with 8Gb of RAM, in several points I had to close RStudio and then open it again to avoid going out of memory (cleaning environment and forcing garbage collection didn't work for me). That's why I often saved files and loaded them again after a few lines.

**I HOPE YOU WILL LIKE MY JOB!**
