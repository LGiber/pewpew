library(tm)
library(wordcloud)
library(memoise)

stopwords <- c("�", "���", "��", "��", "�", "��", "��", "�", "��", "�����", "���", "�����", "��", "���", "��", "���", "���", "���", "������", "������", "�����", "�����", "�����", "������", "�������", "�����", "�����������", "� ����", "� ����������� ��", "����", "������", "�� ������", "�����", "�� ������", "� �������", "���������", "�������� ��", "������", "� ", "��-���", "��-��", "��-���", "� ������� ��", "� �����", "���", "������", "��� ���", "��� ���� �����", "����", "����", "�����", "�����", "������ ���", "� ", "� ", "���", "���", "��", "������", "�����", "����", "����", "���", "����", "������", "�������", "���������", "���", "�� ����� �������", "�� ����", "�����", "��� �����", "�����", "�����", "��� ��","����", "���� ��", "����", "�����", "�������� �� ��", "����", "����", "������", "����", "� ��� �����", "��� ���", "��", "��", "�����")

# The list of valid scenarios
scenarios <<- list("golosa" = "golosa",
                   "Jenwini protiv Muwin" = "Jenwini protiv Muwin",
                   "Mafia" = "mafia")

getTermMatrix <- memoise(function(scenario) {
  
  if (!(scenario %in% scenarios))
    stop("Unknown scenario")
  
  text <- readLines(sprintf("C:/Users/home/Desktop/cinemarketing/%s.txt", scenario),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation(enc2utf8(text)))
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords (enc2utf8(text), stopwords))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})



clean_text <- function(doc) {
  # ���������� ������
  require(stringr)
  require(tm)
  # ������ ������ ��������� ����
  stopwords <- c("�", "���", "��", "��", "�", "��", "��", "�", "��", "�����", "���", "�����", "��", "���", "��", "���", "���", "���", "������", "������", "�����", "�����", "�����", "������", "�������", "�����", "�����������", "� ����", "� ����������� ��", "����", "������", "�� ������", "�����", "�� ������", "� �������", "���������", "�������� ��", "������", "� ", "��-���", "��-��", "��-���", "� ������� ��", "� �����", "���", "������", "��� ���", "��� ���� �����", "����", "����", "�����", "�����", "������ ���", "� ", "� ", "���", "���", "��", "������", "�����", "����", "����", "���", "����", "������", "�������", "���������", "���", "�� ����� �������", "�� ����", "�����", "��� �����", "�����", "�����", "��� ��","����", "���� ��", "����", "�����", "�������� �� ��", "����", "����", "������", "����", "� ��� �����", "��� ���", "��", "��", "�����")
  doc <- tolower(doc)                               # �������� ��� � ������ ��������
  doc <- enc2utf8(doc)                              # ������ ���������
  doc <- removeWords(doc, stopwords)                # ������� ��������� �����, ����� �������� � ���������� � CP1251
  doc <- removePunctuation(doc)                     # ������� ����� ����������, ����� �������� � ���������� � CP1251
  doc <- str_replace_all(doc, "\\s+", " ")          # ������� ������ �������
  doc <- str_trim(doc, side = "both")               # ������� ������� � ������ � � ����� ������
  doc
}

clean_text(scenarios)

library(wordcloud)
wordcloud(scenario, random.order=F, max.words=80, 
          colors=brewer.pal(6,"Oranges"))



#1 ������

golosa <- readLines(sprintf("C:/Users/home/Desktop/cinemarketing/golosa.txt"),
                  encoding="UTF-8")

clean_text <- function(doc) {
  # ���������� ������
  require(stringr)
  require(tm)
  # ������ ������ ��������� ����
  stopwords <- c("�", "���", "��", "��", "�", "��", "��", "�", "��", "�����", "���", "�����", "��", "���", "��", "���", "���", "���", "������", "������", "�����", "�����", "�����", "������", "�������", "�����", "�����������", "� ����", "� ����������� ��", "����", "������", "�� ������", "�����", "�� ������", "� �������", "���������", "�������� ��", "������", "� ", "��-���", "��-��", "��-���", "� ������� ��", "� �����", "���", "������", "��� ���", "��� ���� �����", "����", "����", "�����", "�����", "������ ���", "� ", "� ", "���", "���", "��", "������", "�����", "����", "����", "���", "����", "������", "�������", "���������", "���", "�� ����� �������", "�� ����", "�����", "��� �����", "�����", "�����", "��� ��","����", "���� ��", "����", "�����", "�������� �� ��", "����", "����", "������", "����", "� ��� �����", "��� ���", "��", "��", "�����")
  doc <- tolower(doc)                               # �������� ��� � ������ ��������
  doc <- enc2utf8(doc)                              # ������ ���������
  doc <- removeWords(doc, stopwords)                # ������� ��������� �����, ����� �������� � ���������� � CP1251
  doc <- removePunctuation(doc)                     # ������� ����� ����������, ����� �������� � ���������� � CP1251
  doc <- str_replace_all(doc, "\\s+", " ")          # ������� ������ �������
  doc <- str_trim(doc, side = "both")               # ������� ������� � ������ � � ����� ������
  doc
}

clean_text(golosa)


library(wordcloud)
wordcloud(golosa, random.order=F, max.words=80, 
          colors=brewer.pal(6,"Oranges"))




# 2 ������


golosa2 <- readLines(sprintf("C:/Users/home/Desktop/cinemarketing/golosa.txt"),
                    encoding="UTF-8")

myCorpus = Corpus(VectorSource(golosa2))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation(enc2utf8(golosa2)))
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords (enc2utf8(text), stopwords))

myDTM = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))

m = as.matrix(myDTM)

sort(rowSums(m), decreasing = TRUE)

library(wordcloud)
wordcloud(golosa2, random.order=F, max.words=80, 
          colors=brewer.pal(8,"Dark2"))
          

          
          #�����������


          install.packages("stringr")
          library("stringr")
          
          install.packages("plyr")
          library(plyr)
          
          library(stringr)

pos.words = scan('C:/Users/home/Desktop/cinemarketing/positive.txt', what='character', comment.char=';')

neg.words = scan('C:/Users/home/Desktop/cinemarketing/negative.txt', what='character', comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

analysis = score.sentiment(golosa2, pos.words, neg.words) 
table(analysis$score)


mean(analysis$score)
#0.002


hist(analysis$score)

