library(tm)
library(wordcloud)
library(memoise)

stopwords <- c("в", "без", "до", "из", "к", "на", "по", "о", "от", "перед", "при", "через", "за", "над", "об", "под", "про", "для", "вблизи", "вглубь", "вдоль", "возле", "около", "вокруг", "впереди", "после", "посредством", "в роли", "в зависимости от", "путём", "насчёт", "по поводу", "ввиду", "по случаю", "в течение", "благодаря", "несмотря на", "спустя", "с ", "из-под", "из-за", "по-над", "в отличие от", "в связи", "как", "словно", "так как", "для того чтобы", "тоже", "зато", "чтобы", "также", "потому что", "и ", "а ", "что", "или", "но", "однако", "когда", "лишь", "едва", "где", "куда", "откуда", "столько", "настолько", "так", "до такой степени", "до того", "такой", "как будто", "будто", "точно", "как бы","если", "если бы", "коли", "ежели", "несмотря на то", "хотя", "хоть", "пускай", "дабы", "с тем чтобы", "так что", "ли", "не", "какой")

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