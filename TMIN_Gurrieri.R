#LIBRERIE USATE-----------------------------------------------------------------

# Creo un vettore chiamato "packs" che contiene i nomi dei pacchetti da caricare
packs <- c("dplyr", "tidyr", "readr", "ggplot2", "tidytext", "tidyverse", "widyr",
           "ggraph", "igraph", "ggthemes", "tm", "textclean", "textdata", "topicmodels",
           "wordcloud", "reshape2", "sentimentr", "stringr", "udpipe", "stopwords",
           "syuzhet", "BTM", "textplot", "concaveman")

# Carico i pacchetti usando la funzione lapply, che itera su ciascun elemento del vettore "packs"
# La funzione require viene utilizzata per caricare i pacchetti
# L'argomento "character.only = TRUE" specifica che solo caratteri (nomi dei pacchetti) devono essere passati alla funzione require.
lapply(packs, require, character.only = TRUE)

#PRE-ELABORAZIONE DEI DATI------------------------------------------------------

# Aggiungo una nuova colonna chiamata "combined_rating" a ScholarsEn utilizzando la funzione mutate
# La colonna combinata è ottenuta attraverso la funzione coalesce che seleziona il valore non nullo tra le colonne Rating, Rating.1, Rating.2, Rating.3, Rating.4
ScholarsEn <- ScholarsEn %>%
  mutate(combined_rating = coalesce(Rating, Rating.1, Rating.2, Rating.3, Rating.4))

# Rimuovo le colonne 6:10 da ScholarsEn, rinomino alcune colonne
ScholarsEn <- ScholarsEn %>%
  select(-c(6:10)) %>%
  rename("Username" = "ï..Username", "Rating" = "combined_rating")

# Rimuovo la stringa "Date of visit: " dalla colonna Date di ScholarsEn
ScholarsEn$Date <- gsub("Date of visit: ", "", ScholarsEn$Date)

# Rimuovo le stringhe " reviews" e " review" dalla colonna Review.Number di ScholarsEn
ScholarsEn$Review.Number <- gsub(" reviews", "", ScholarsEn$Review.Number)
ScholarsEn$Review.Number <- gsub(" review", "", ScholarsEn$Review.Number)

# Copio ScholarsEn nel dataframe df
df <- ScholarsEn

# Mantengo solo gli ultimi 4 caratteri nella colonna Date di df
df$Date <- substr(df$Date, nchar(df$Date) - 3, nchar(df$Date))

# Converto le colonne Text e Title di df in caratteri
df$Text <- as.character(df$Text)
df$Title <- as.character(df$Title)

# Aggiungo una nuova colonna Id che va da 1 a 1725 a df
df$Id = (1:1725)

# Estrapolo le colonne Rating, Text, Date e Id da df e creo un nuovo dataframe chiamato IRDT
Rating <- df$Rating
Text <- df$Text
Date <- df$Date
Id <- df$Id
IRDT <- data.frame(Id, Rating, Date, Text)

# Estrapolo le colonne Id, Rating, Date e Title da df e creo un nuovo dataframe chiamato IRDT2
Title <- df$Title
IRDT2 <- data.frame(Id, Rating, Date, Title)

# Tokenizzo la colonna Review di IRDT e filtro i token che contengono solo caratteri minuscoli o apostrofi
# Rimuovo anche i token presenti nella lista di stop_words
tex_words <- IRDT %>%
  unnest_tokens(word, Text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

# Tokenizzo la colonna Title di IRDT2 e filtro i token che contengono solo caratteri minuscoli o apostrofi
# Rimuovo anche i token presenti nella lista di stop_words
tit_words <- IRDT2 %>%
  unnest_tokens(word, Title) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

#ANALISI PRELIMINARI------------------------------------------------------------

# Creazione di un oggetto ggplot con i dati del dataframe df e mappando la variabile Rating sull'asse x
ggplot(df, aes(x = Rating)) +
  # Aggiunta di un layer di barre al grafico utilizzando i valori della variabile Rating
  geom_bar() +
  # Aggiunta delle etichette agli assi x e y del grafico
  labs(x = "Stelle", y = "Numero di recensioni") +
  # Applicazione di uno stile minimale al grafico
  theme_minimal()+
  # Inversione degli assi x e y, visualizzando le barre in orizzontale
  coord_flip()

# Creazione di un oggetto ggplot con i dati del dataframe df e mappando la variabile Date sull'asse x
ggplot(df, aes(x = Date)) +
  # Aggiunta di un layer di barre al grafico utilizzando i valori della variabile Date
  geom_bar() +
  # Aggiunta delle etichette agli assi x e y del grafico
  labs(x = "Anno", y = "Numero di recensioni") +
  # Applicazione di uno stile minimale al grafico
  theme_minimal()+
  # Inversione degli assi x e y, visualizzando le barre in orizzontale
  coord_flip()

# Calcolo la frequenza delle parole nella colonna word del dataframe tex_words, ordinandole in modo decrescente
freq <- tex_words %>%
  count(word, sort = TRUE)

# Filtraggio delle parole con una frequenza superiore a 100 dal dataframe freq
# Riordino le parole in base alla frequenza
# Creo un grafico a barre utilizzando ggplot, con le frequenze sulle ordinate e le parole sulle ascisse
freq %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, x=NULL) +
  labs(title = "Frequenza delle parole di Text")

# Calcolo la frequenza delle parole nella colonna word del dataframe tit_words, ordinandole in modo decrescente
freq2 <- tit_words %>%
  count(word, sort = TRUE)

# Filtraggio delle parole con una frequenza superiore a 25 dal dataframe freq2
# Riordino le parole in base alla frequenza
# Creo un grafico a barre utilizzando ggplot, con le frequenze sulle ordinate e le parole sulle ascisse
freq2 %>%
  filter(n > 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, x=NULL) +
  labs(title = "Frequenza delle parole di Title")


# Calcolo la frequenza delle parole nella colonna word del dataframe tex_words
# Creo una nuvola di parole utilizzando la funzione wordcloud, limitando il numero massimo di parole a 100
tex_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Calcolo la frequenza delle parole nella colonna word del dataframe tex_words, raggruppate per Rating
words_by_stars <- tex_words %>%
  count(Rating, word, sort = TRUE) %>%
  ungroup()

# Calcolo la frequenza delle parole nella colonna word del dataframe tex_words, raggruppate per Date
words_by_year <- tex_words %>%
  count(Date, word, sort = TRUE) %>%
  ungroup()

# Calcolo la frequenza delle parole nella colonna word del dataframe tit_words, raggruppate per Rating
words_by_stars2 <- tit_words %>%
  count(Rating, word, sort = TRUE) %>%
  ungroup()

#TF-IDF-------------------------------------------------------------------------

# Calcolo il valore tf-idf per le parole raggruppate per Rating nel dataframe words_by_stars
# Utilizzo la funzione bind_tf_idf per legare il tf-idf alla parola, al Rating e alla frequenza n
# Ordino il dataframe in modo decrescente in base al valore di tf-idf
tf_idf_stars <- words_by_stars %>%
  bind_tf_idf(word, Rating, n) %>%
  arrange(desc(tf_idf))

# Ordino il dataframe tf_idf_stars in modo decrescente in base al valore di tf-idf
tf_idf_stars %>%
  arrange(desc(tf_idf))

# Raggruppo il dataframe tf_idf_stars per Rating
# Seleziono le parole con il valore di tf-idf più alto per ogni Rating (le prime 5 parole)
# Rimuovo il raggruppamento
# Creo un grafico a barre utilizzando ggplot, con tf-idf sulle ascisse, le parole riordinate in base al tf-idf sulle ordinate e il Rating come colore delle barre
# Creo una griglia di faccette separate per ogni Rating, con 2 colonne, scalando gli assi liberamente e senza legenda per il colore
# Assegno una etichetta all'asse x e lascio l'asse y senza etichetta
tf_idf_stars %>%
  group_by(Rating) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Rating)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Rating, ncol = 2, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "TF-IDF di Text")

# Calcolo il valore tf-idf per le parole raggruppate per Rating nel dataframe words_by_stars2
# Utilizzo la funzione bind_tf_idf per legare il tf-idf alla parola, al Rating e alla frequenza n
# Ordino il dataframe in modo decrescente in base al valore di tf-idf
tf_idf_stars2 <- words_by_stars2 %>%
  bind_tf_idf(word, Rating, n) %>%
  arrange(desc(tf_idf))

# Ordino il dataframe tf_idf_stars2 in modo decrescente in base al valore di tf-idf
tf_idf_stars2 %>%
  arrange(desc(tf_idf))

# Raggruppo il dataframe tf_idf_stars2 per Rating
# Seleziono le parole con il valore di tf-idf più alto per ogni Rating (le prime 6 parole)
# Rimuovo il raggruppamento
# Creo un grafico a barre utilizzando ggplot, con tf-idf sulle ascisse, le parole riordinate in base al tf-idf sulle ordinate e il Rating come colore delle barre
# Creo una griglia di faccette separate per ogni Rating, con 2 colonne, scalando gli assi liberamente e senza legenda per il colore
# Assegno una etichetta all'asse x e lascio l'asse y senza etichetta
tf_idf_stars2 %>%
  group_by(Rating) %>%
  slice_max(tf_idf, n = 6) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Rating)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Rating, ncol = 2, scales = "free") +
  labs(x = NULL, y = NULL) +
  labs(title = "TF-IDF di Title")

#TOPIC MODELLING----------------------------------------------------------------

# Seleziona le colonne "Id" e "word" dal dataframe tex_words e crea un nuovo dataframe chiamato BTM_words
BTM_words <- tex_words[,c("Id", "word")]

# Imposta il seed del generatore di numeri casuali su 123
set.seed(123)

# Crea un modello BTM utilizzando i dati di BTM_words con 7 topic, 1000 iterazioni, background=TRUE e trace=100
model <- BTM(BTM_words, k=7, iter=1000, background=TRUE, trace=100)

# Visualizza il plot del modello BTM, mostrando i top 10 termini per ogni topic e con un titolo "BTM model"
plot(model, top_n = 10, title = "BTM model")

# Restituisce la matrice di distribuzione dei topic (theta) del modello BTM
model$theta

# Ottiene i termini più rilevanti per ogni topic del modello BTM, selezionando i top 10 termini
topicterms <- terms(model, top_n = 10)

# Restituisce i termini più rilevanti per ogni topic del modello BTM
topicterms

##

# Unisco le colonne Rating e Id del dataframe tex_words in una nuova colonna chiamata document
# Conto le occorrenze di document e word nel dataframe risultante
# Creo una Document-Term Matrix (DTM) utilizzando la funzione cast_dtm, specificando document come documento, word come termine e n come conteggio
tex_dtm <- tex_words %>%
  unite(document, Rating, Id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

# Applico il modello LDA (Latent Dirichlet Allocation) alla DTM tex_dtm, con 6 topic e il seed 2410
tex_lda <- LDA(tex_dtm, k = 6, control = list(seed = 2410))

# Ottengo i topic dal modello LDA e li converto in un formato tabulare utilizzando la funzione tidy
tex_topics <- tidy(tex_lda, matrix = "beta") 

# Per ogni topic, seleziono i termini con il valore di beta più alto (le prime 10)
# Rimuovo il raggruppamento
# Ordino i risultati per topic in ordine crescente e per beta in ordine decrescente
tex_top_terms <- tex_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Riordino i termini all'interno di ogni topic in base al valore di beta
# Creo un grafico a barre utilizzando ggplot, con beta sulle ascisse, i termini riordinati in base a beta sulle ordinate e il topic come colore delle barre
# Creo una griglia di faccette separate per ogni topic, con gli assi scalati liberamente
# Assicuro che l'asse delle ordinate sia riordinato in base a beta
tex_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#SENTIMENT ANALYSIS-------------------------------------------------------------

# Calcolo il valore di sentimento medio per ciascuna valutazione (Rating)
rating_sentiments <- words_by_stars %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Rating) %>%
  summarize(value = sum(value * n) / sum(n))

# Riordino le valutazioni (Rating) in base al valore di sentimento medio
# Creo un grafico a barre utilizzando ggplot, con il valore di sentimento medio sulle ascisse, le valutazioni riordinate sulle ordinate e il colore delle barre in base al valore di sentimento (positivo o negativo)
rating_sentiments %>%
  mutate(rating = reorder(Rating, value)) %>%
  ggplot(aes(value, rating, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Valore medio del sentiment", y = NULL) +
  labs(title = "Sentiment medio delle valutazioni basato sui testi")

# Calcolo il valore di sentimento medio per ciascuna valutazione (Rating)
rating_sentiments2 <- words_by_stars2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Rating) %>%
  summarize(value = sum(value * n) / sum(n))

# Riordino le valutazioni (Rating) in base al valore di sentimento medio
# Creo un grafico a barre utilizzando ggplot, con il valore di sentimento medio sulle ascisse, le valutazioni riordinate sulle ordinate e il colore delle barre in base al valore di sentimento (positivo o negativo)
rating_sentiments2 %>%
  mutate(rating = reorder(Rating, value)) %>%
  ggplot(aes(value, rating, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Valore medio del sentiment", y = NULL) +
  labs(title = "Sentiment medio delle valutazioni basato sui titoli")

# Calcolo il numero di occorrenze e la contribuzione per ogni parola nel dataframe tex_words
contributions <- tex_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

# Calcolo il numero di occorrenze e la contribuzione per ogni parola nel dataframe tit_words
contributions2 <- tit_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

# Seleziono le prime 25 parole con la contribuzione assoluta più alta
# Riordino le parole in base alla contribuzione
# Creo un grafico a barre utilizzando ggplot, con la contribuzione sulle ascisse, le parole riordinate sulle ordinate e il colore delle barre in base alla contribuzione (positiva o negativa)
contributions %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL) +
  labs(title = "Parole che influenzano un sentiment più positivo o negativo nei testi")

contributions2 %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL) +
  labs(title = "Parole che influenzano un sentiment più positivo o negativo nei titoli")

# Calcolo la contribuzione di sentimento per ciascuna parola nel dataframe words_by_stars
top_sentiment_words <- words_by_stars %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

# Per ogni valutazione (Rating), seleziono le prime 15 parole con la contribuzione assoluta più alta
# Riordino i termini all'interno di ogni valutazione (Rating) in base alla contribuzione
# Creo un grafico a barre utilizzando ggplot, con la contribuzione sulle ascisse, le parole riordinate in base alla contribuzione sulle ordinate e il colore delle barre in base alla valutazione (Rating)
top_sentiment_words %>%
  group_by(Rating) %>%
  slice_max(abs(contribution), n = 15) %>%
  mutate(term = reorder_within(word, contribution, Rating)) %>%
  ggplot(aes(contribution, word, fill = factor(Rating))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Rating, scales = "free") +
  scale_y_reordered() +
  labs(title = "Parole che influenzano un sentiment più positivo o negativo nei testi in base alle valutazioni")


# Calcolo la contribuzione di sentimento per ciascuna parola nel dataframe words_by_stars2
top_sentiment_words2 <- words_by_stars2 %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n / sum(n))

# Per ogni valutazione (Rating), seleziono le prime 15 parole con la contribuzione assoluta più alta
# Riordino i termini all'interno di ogni valutazione (Rating) in base alla contribuzione
# Creo un grafico a barre utilizzando ggplot, con la contribuzione sulle ascisse, le parole riordinate in base alla contribuzione sulle ordinate e il colore delle barre in base alla valutazione (Rating)
top_sentiment_words2 %>%
  group_by(Rating) %>%
  slice_max(abs(contribution), n = 15) %>%
  mutate(term = reorder_within(word, contribution, Rating)) %>%
  ggplot(aes(contribution, word, fill = factor(Rating))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Rating, scales = "free") +
  scale_y_reordered() +
  labs(title = "Parole che influenzano un sentiment più positivo o negativo nei titoli in base alle valutazioni")

# Calcolo il conteggio delle parole per ogni sentimento nel dataframe tex_words
bing_word_counts <- tex_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Per ogni sentimento, seleziono le prime 10 parole con il conteggio più alto
# Riordino le parole in base al conteggio
# Creo un grafico a barre utilizzando ggplot, con il conteggio sulle ascisse, le parole riordinate sulle ordinate e il colore delle barre in base al sentimento
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Sentiment",y = NULL) +
  labs(title = "Parole positive/negative nei testi delle recensioni")

# Calcolo il conteggio delle parole per ogni sentimento nel dataframe tit_words
bing_word_counts2 <- tit_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Per ogni sentimento, seleziono le prime 10 parole con il conteggio più alto
# Riordino le parole in base al conteggio
# Creo un grafico a barre utilizzando ggplot, con il conteggio sulle ascisse, le parole riordinate sulle ordinate e il colore delle barre in base al sentimento
bing_word_counts2 %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Sentiment",y = NULL) +
  labs(title = "Parole positive/negative nei titoli delle recensioni")

# Calcolo il conteggio delle parole per ogni sentimento nel dataframe tex_words
# Creo una nuvola di parole utilizzando comparison.cloud, con le parole riordinate in base al conteggio
tex_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

#ANALISI N-GRAMMI--------------------------------------------------------------- 

# Crea un nuovo dataframe tex_bigrams utilizzando il dataframe df, suddividendo il testo in bigrammi utilizzando la funzione "unnest_tokens"
tex_bigrams <- df %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

# Calcola il conteggio dei bigrammi nel dataframe tex_bigrams, ordinandoli in modo decrescente
tex_bigram_counts <- tex_bigrams %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtra i bigrammi nel dataframe tex_bigram_counts, escludendo le parole presenti nel dataframe stop_words nella colonna "word1" e "word2"
bigrams_filtered <- tex_bigram_counts %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Definizione di una lista di parole negate, buone e problematiche
negate_words <- c('not', 'no', "don't")
good_words <- c('good','great','nice','perfect')
problems_words <- c('beer','staff', 'food','music')

# Filtra i bigrammi nel dataframe tex_bigram_counts, includendo solo quelli che hanno la parola di "word1" presente nella lista di parole negate
# Calcola il conteggio dei bigrammi e la loro sentiment value utilizzando il dataframe "afinn"
# Calcola la "contribution" moltiplicando il valore del sentimento per il conteggio del bigramma
# Raggruppa per "word1" e seleziona i 10 bigrammi con la maggior contribuzione assoluta
# Riordina i bigrammi in base alla loro contribuzione all'interno di ogni "word1"
# Crea un grafico ggplot con i bigrammi filtrati e le rispettive contribuzioni di sentimento
# Utilizza facet_wrap per suddividere il grafico per ogni "word1"
# Aggiunge etichette agli assi x e y del grafico
tex_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 4) +
  scale_y_reordered() +
  labs(x = "Sentiment value",
       y = "Word checked")

# Esegue operazioni simili al blocco di codice precedente, ma filtrando i bigrammi per "word1" presente nella lista di parole buone
tex_bigram_counts %>%
  filter(word1 %in% good_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value",
       y = "Word checked")

# Esegue operazioni simili al blocco di codice precedente, ma filtrando i bigrammi per "word1" presente nella lista di parole problematiche
tex_bigram_counts %>%
  filter(word1 %in% problems_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value",
       y = "Word checked")

# Filtra i bigrammi nel dataframe bigrams_filtered che hanno un conteggio maggiore di 2
# Crea un grafo basato sui bigrammi filtrati
bigram_graph <- bigrams_filtered %>%
  filter(n > 2) %>%
  graph_from_data_frame()

# Imposta il seed del generatore di numeri casuali su 1223
set.seed(1223)

# Crea un oggetto arrow per rappresentare le frecce nel grafo
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Crea un grafico ggraph utilizzando il grafo bigram_graph e il layout "fr"
# Aggiunge i collegamenti tra i nodi con frecce, utilizzando l'attributo "n" per l'opacità degli archi
# Aggiunge i punti dei nodi e le relative etichette
# Applica un tema senza elementi di sfondo
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

