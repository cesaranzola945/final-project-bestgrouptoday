# text analysis of reports

rm(list = ls())
cat("\014")
# ----------Packages this code needs ----------#
# install.packages("pdftools")

#libraries
library(tidyverse)
library(pdftools)
library(textdata)
library(tidytext)
library(wordcloud)
library(tm)
library(ggthemes)

# ---------- path for this code --------------#

setwd("/Users/Agha/Documents/GitHub/final-project-bestgrouptoday")
#setwd("D:/Users/ASUS/Documents/GitHub/Data and programing for PP 2/Final Project")
reports = c("Exxon", "Gazprom", "Petrochina", "Shell", "BritishPetrolum")
n_rep   = length(reports)

# ---------- Most used words on each report and in All the reports ----#

# FUNCTION - Obtain words from a pdf file
    pdf_words<- function(sel_rep) {
        pdf_file <- str_c("sustainability_reports/", sel_rep,".pdf")  
        pdf_text_file <- pdftools::pdf_text(pdf_file)
        pdf_lines <- 
          pdf_text_file %>% 
           read_lines()
           
        pdf_sq <-
        pdf_lines %>% 
          str_squish() %>% 
             unlist()
        
        all_words    <- str_c(pdf_sq, collapse = "|")
        text_df      <- tibble(text = all_words)
        words_report <- unnest_tokens(text_df, word_tokens,  text, token = "words")
        words_report <- anti_join(words_report, stop_words, by = c("word_tokens" = "word"))
    }


# Get the words from each sustainability report
word_rep <- tibble()

  for (r in seq_along(reports)){    
    words_r  <- pdf_words(reports[r])
    words_r["report"]  <- reports[r]
    word_rep <- rbind(word_rep,words_r)
  }

word_rep_all <- as_tibble(word_rep)

#keep just the numbers
numb_rep<-
  word_rep_all %>% 
  filter(str_detect(word_tokens,"[0-9]"))

# Keep just the words
word_rep<-
word_rep_all %>% 
  filter(str_detect(word_tokens,"[a-zA-Z]"))

# top 50 words on every report 
top_50w<-   
word_rep %>% 
  group_by(report) %>%
   count(word_tokens) %>% 
   arrange(report,desc(n)) %>%
    summarise(report = report,word_tokens = word_tokens,n = n, prop = n/sum(n)) %>% 
    slice(1:50)


# Create cloud words for each report
cloud_names <- str_c(c("images/worldCloud"), reports,".png") 

for (j in seq_along(reports)){
  filt <-
    word_rep %>% 
    filter(report == reports[j])
  
  text_corpus <- Corpus(VectorSource(filt$word_tokens))
  png(cloud_names[j])
  wordcloud(text_corpus, max.words = 50, colors = brewer.pal(8, "Dark2"),
            rot.per=0)
  dev.off()
}

# Which words all the reports used in common?
list_top <- list()
    for (t in seq_along(reports)){
      list_top[[t]] = filter(top_50w,report == reports[t])$word_tokens
    }

word_int <- Reduce(intersect,list_top)

# Filter the words in common and plot them in a graph
topw_all <-
top_50w %>% 
  filter(word_tokens %in% word_int) %>% 
    arrange(word_tokens)

png("images/words_in_common.png",width = 650, height = 450)
ggplot(topw_all)+
  geom_bar(aes(x = word_tokens, y = prop*100,fill = report),position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("green2","red3","blue3","yellow2","purple")) +
  labs(title = "Figure 2: Words Used in ALL the reports",
       subtitle = "Percent of times this word appear on each report",
       caption = "Note: calculated as the number of times this word appears over the total number of words in each report. This theme belongs to The economist.") +
  theme_economist() +
  xlab("") +
  ylab("Percent(%)")
dev.off()
  
# ----------Text sentiment analysis----#
word_sent <- word_rep %>%
  left_join(get_sentiments("nrc"), by = c("word_tokens" = "word")) %>%
  plyr::rename(replace = c(sentiment = "nrc", value = "nrc"), warn_missing = FALSE)

word_sent_c <-
  word_sent %>% 
  group_by(report) %>%
  count(nrc,sort = TRUE) %>% 
  arrange(report) %>% 
  summarise(nrc = nrc,n = n, prop = n/sum(n)) %>% 
    arrange(nrc)


png("images/reports_sentiment_NRC.png",width = 650, height = 450)
ggplot(data = filter(word_sent_c, !is.na(nrc)))+
  geom_bar(aes(x = nrc, y =prop*100,fill = report),position = "dodge",stat = "identity") +
  scale_fill_manual(values = c("green2","red3","blue3","yellow2","purple")) +
  labs(title = "Figure 3: Sentiment Analysis (NRC Emoticon Lexicon)",
       subtitle = "Percent of times this sentiment appear on each report",
       caption = "Note: calculated as the number of times this sentiment appears over the total number of words in each report. The NRC Emotion Lexicon is a list of English words and their associations with ten basic emotions. This theme belongs to The economist.")+
  theme_economist() +
  xlab("") +
  ylab("Percent(%)") +
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 7))


dev.off()