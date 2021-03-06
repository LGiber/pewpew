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