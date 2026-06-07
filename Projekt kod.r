#' ---
#' title: "Klasyfikacja"
#' author: " "
#' date:   " "
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show    
#'     number_sections: false 
#' ---
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#' # KROK  0 Wymagane pakiety
# KROK 0 Wymagane pakiety ----

library(tm)      # Przetwarzanie tekstu
library(tidyverse)  # Praca nad tekstem
library(tidytext) # Analiza tekstu
library(rvest)       # Web scraping
library(syuzhet)    # Analiza sentymentu
library(e1071)    # Uczenie maszynowe
library(ggplot2)     # Wykresy
library(readxl)     # Czytanie plików z excela
library(SnowballC)    # Stemming
library(wordcloud)      # Chmury słów
library(RColorBrewer)  # Palety kolorystyczne

#' # KROK 1. Pobranie danych treningowych
# KROK 1. Pobranie danych treningowych ----


#' # 1A: POBIERANIE ARTYKUŁÓW Z INTERNETU 
# 1A: POBIERANIE ARTYKUŁÓW Z INTERNETU (kod z tego kroku został napisany wykorzystując LLM) ----


# Funkcja do pobierania linków do artykułów z podanej strony docelowej

get_latest_links <- function(main_url, base_domain, regex_pattern, limit = 40) {
  page <- tryCatch(read_html(main_url), error = function(e) return(NULL))
  if(is.null(page)) return(character(0))
  wszystkie_linki <- page %>% html_nodes("a") %>% html_attr("href")
  wszystkie_linki <- wszystkie_linki[!is.na(wszystkie_linki)]
  linki_artykulow <- wszystkie_linki[grepl(regex_pattern, wszystkie_linki)]
  linki_artykulow <- ifelse(grepl("^http", linki_artykulow), linki_artykulow, paste0(base_domain, linki_artykulow))
  unikalne_linki <- head(unique(linki_artykulow), limit)
  return(unikalne_linki)
}

# Funkcja do ekstrakcji czystego tekstu z zawartości pojedynczego artykułu
scrape_article <- function(url) {
  Sys.sleep(1.5)
  result <- tryCatch({
    page <- read_html(url)
    text_nodes <- page %>% html_nodes("p") %>% html_text()
    full_text <- paste(text_nodes, collapse = " ")
    return(full_text)
  }, error = function(e) { return(NA) })
  return(result)
}
#Zdefiniowanie źródła artykułów
linki_cnn <- get_latest_links("https://www.cnn.com/politics", "https://www.cnn.com", "/202", 30)
linki_fox <- get_latest_links("https://www.foxnews.com/politics", "https://www.foxnews.com", "/politics/.*-", 30)

#Przetwarzanie zbioru danych
# Undersampling: wyrównujemy ilość artykułów
ile_max <- min(length(linki_cnn), length(linki_fox))
linki_cnn <- head(linki_cnn, ile_max)
linki_fox <- head(linki_fox, ile_max)
# --- BEZPIECZNIK ---
MIN_WYMAGANYCH_ARTYKULOW <- 10

if (ile_max < MIN_WYMAGANYCH_ARTYKULOW) {
  stop(paste("BŁĄD KRYTYCZNY: Za mało danych do uczenia!",
             "CNN pobrało:", length(linki_cnn), 
             "| FOX pobrało:", length(linki_fox)))
}


#Stworzenie ramki danych z artykułami z sieci
dane_artykuly <- data.frame(
  url = c(linki_cnn, linki_fox),
  Partia = c(rep("Democrat", length(linki_cnn)), rep("Republican", length(linki_fox))),
  stringsAsFactors = FALSE
)

# Pobieranie treści artykułów
dane_artykuly$Tekst <- sapply(dane_artykuly$url, scrape_article)
dane_artykuly <- dane_artykuly %>% filter(!is.na(Tekst) & nchar(Tekst) > 200)

# Standaryzacja ramki dla artykułów
ramka_artykuly <- data.frame(Tekst = dane_artykuly$Tekst, Partia = dane_artykuly$Partia, Zrodlo = "Artykuł")


#' # 1B: POBIERANIE PRZEMÓWIEŃ PREZYDENTÓw
# 1B: POBIERANIE PRZEMÓWIEŃ PREZYDENTÓw ----

# Pobieramy przemówienia ostatnich 15 prezydentó USA z pliku xlsx, gdzie w sheet = 2 znajduje się 60 przemóień (po 4 każdego z nich)
dane_prezydenci <- read_excel("prezydenci.xlsx", sheet = 2) # W sheet = 1 znajdują się jeszcze bardziej archiwalne przemóienia, jednak nie są one obiektem zainteresowania niniejszego projektu
dane_prezydenci$Partia <- factor(dane_prezydenci$Partia, levels = c("Democrat", "Republican"))

# Porządkujemy przemówienia ze względu na prezydenta
dane_prezydenci <- dane_prezydenci %>%
  group_by(Prezydent) %>%
  mutate(Numer_Przemowienia = row_number()) %>%
  ungroup()
# Podział na zbiór treningowy i tekstowy
# do trenigu wybieramy co drugie przemówienie (1 i 3). 
dane_train <- dane_prezydenci %>% 
  filter(Numer_Przemowienia %% 2 != 0)

# do testu zostawiamy resztę, czyli parzyste (2 i 4).
dane_test <- dane_prezydenci %>% 
  filter(Numer_Przemowienia %% 2 == 0) # To zostawiamy na sam koniec!

# Standaryzacja ramki dla przemówień 
ramka_przemowienia <- data.frame(Tekst = dane_train$Speech, Partia = dane_train$Partia, Zrodlo = "Przemówienie")


#' # 1C: Połączenie danych treningowych
# 1C: Połączenie danych treningowych ----

wspolne_dane_treningowe <- rbind(ramka_artykuly, ramka_przemowienia)

print(table(wspolne_dane_treningowe$Zrodlo, wspolne_dane_treningowe$Partia))

#' # KROK 2: BUDOWA PRZESTRZENI WEKTOROWEJ
# KROK 2: BUDOWA PRZESTRZENI WEKTOROWEJ ----

# Dodajemy unikalne ID dla każdego dokumentu, aby wiedzieć, skąd pochodzą słowa
wspolne_dane_treningowe <- wspolne_dane_treningowe %>%
  mutate(doc_id = row_number())

# Definiujemy własne stop słowa jako ramkę danych
custom_stop_words <- tibble(word = c("cnn", "fox", "news", "subscribe", 
                                     "newsletter", "advertisement", "share"))

# 2A Przetwarzanie, oczyszczanie i stemming tekstu
tidy_train <- wspolne_dane_treningowe %>%
  # Rozbijamy tekst na pojedyncze słowa (od razu zmienia na małe litery i usuwa interpunkcję!)
  unnest_tokens(word, Tekst) %>%
  # Usuwamy liczby i linki przy pomocy wyrażeń regularnych
  filter(!str_detect(word, "^http"),
         !str_detect(word, "[0-9]")) %>%
  # Usuwamy standardowe stop words (korzystamy ze wbudowanego słownika 'stop_words')
  anti_join(stop_words, by = "word") %>%
  # Usuwamy nasze niestandardowe słowa
  anti_join(custom_stop_words, by = "word") %>%
  # Wykonujemy stemming (przy użyciu pakietu SnowballC)
  mutate(word = wordStem(word, language = "en"))

# 2B Budowa macierzy i inżynieria cech
# Obliczamy TF-IDF
tidy_tfidf <- tidy_train %>%
  count(doc_id, word) %>%
  bind_tf_idf(word, doc_id, n)

# Przekształcamy dane z powrotem do macierzy dokument-słowo
train_data_wide <- tidy_tfidf %>%
  select(doc_id, word, tf_idf) %>%
  pivot_wider(names_from = word, values_from = tf_idf, values_fill = list(tf_idf = 0))

# Budujemy finalną ramkę do uczenia (łączymy z docelową Partią i dodajemy Sentyment)
train_data <- train_data_wide %>%
  left_join(wspolne_dane_treningowe %>% select(doc_id, Partia, Tekst), by = "doc_id") %>%
  mutate(
    Political_Orientation = factor(Partia, levels = c("Democrat", "Republican")),
    EXTRA_Sentiment = get_sentiment(Tekst, method = "syuzhet")
  ) %>%
  # Pozbywamy się kolumn pomocniczych, by zostawić tylko cechy do modelu
  select(-doc_id, -Partia, -Tekst)

# Zapisujemy wektor unikalnych słów ("zamrożony słownik") na potrzeby nowych danych
zamrozony_slownik <- colnames(train_data_wide)[-1] # Pomijamy kolumnę doc_id

# Obliczamy sentyment (krytyczny vs pozytywny)
train_data$EXTRA_Sentiment <- get_sentiment(wspolne_dane_treningowe$Tekst, method = "syuzhet") 
train_data$Political_Orientation <- factor(wspolne_dane_treningowe$Partia, levels = c("Democrat", "Republican"))
zamrozony_slownik_idf <- tidy_tfidf %>% distinct(word, idf)
zamrozony_slownik     <- zamrozony_slownik_idf$word



#' # KROK 3: TRENOWANIE MODELU SVM 
# KROK 3: TRENOWANIE MODELU SVM ----


svm_model <- svm(Political_Orientation ~ ., 
                 data = train_data, 
                 kernel = "linear", 
                 probability = TRUE)

#' # KROK 4: TESTOWANIE MODELU
# KROK 4: TESTOWANIE MODELU ----


# Obliczamy TF-IDF dla tokenów z danych testowych
test_tfidf <- dane_test %>%
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word, Speech) %>%
  filter(!str_detect(word, "^http"), 
         !str_detect(word, "[0-9]")) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  mutate(word = wordStem(word, language = "en")) %>%
  count(doc_id, word) %>%
  # Łączymy z wagami IDF z treningu
  inner_join(zamrozony_slownik_idf, by = "word") %>%
  mutate(tf_idf = n * idf) %>%
  select(doc_id, word, tf_idf)

# Używamy complete(), aby upewnić się, że KAŻDY doc_id (od 1 do nrow(dane_test))
# oraz KAŻDE słowo z zamrożonego słownika znajdzie się w macierzy (puste wartości wypełniamy 0)
test_data_complete <- test_tfidf %>%
  complete(doc_id = 1:nrow(dane_test), word = zamrozony_slownik, fill = list(tf_idf = 0))

# Przekształcamy do szerokiej macierzy
test_data_wide <- test_data_complete %>%
  pivot_wider(names_from = word, values_from = tf_idf)

# Porządkujemy wiersze i kolumny, aby idealnie pasowały do modelu SVM
test_data <- test_data_wide %>%
  arrange(doc_id) %>%
  select(all_of(zamrozony_slownik)) %>%
  # Dodajemy sentyment wyliczony dla tekstów testowych
  mutate(EXTRA_Sentiment = get_sentiment(dane_test$Speech, method = "syuzhet"))

# Prognoza modelu
predykcje_test <- predict(svm_model, newdata = test_data)

#' # KROK 5: WYNIKI
# KROK 5: WYNIKI ----

# Wymuszamy, aby model pamiętał o obu partiach,
# nawet jeśli przypisał wszystkim dokumentom tylko jeden rodzaj partii.
predykcje_test <- factor(predykcje_test, levels = c("Democrat", "Republican"))
prawdziwe_partie <- factor(dane_test$Partia, levels = c("Democrat", "Republican"))

#' # 5A Wyniki liczbowe i tabelarycznie
# 5A Wyniki liczbowe i tabelarycznie ----

wyniki_koncowe <- data.frame(
  Prezydent = dane_test$Prezydent,
  Prawdziwa_Partia = dane_test$Partia,
  Werdykt_Modelu = predykcje_test
)
print(wyniki_koncowe)

# Macierz pomyłek 
confusion_matrix  <- table(Przewidziane = predykcje_test, Prawdziwe = prawdziwe_partie)
print(confusion_matrix)

TP <- confusion_matrix["Democrat", "Democrat"]
TN <- confusion_matrix["Republican", "Republican"]
FP <- confusion_matrix["Democrat", "Republican"]
FN <- confusion_matrix["Republican", "Democrat"]

cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")

# Obliczenia metryk
accuracy <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)

# Zabezpieczenie przed dzieleniem przez zero
if(is.na(precision)) precision <- 0 
if(is.na(recall)) recall <- 0
if(is.na(specificity)) specificity <- 0

f1_score <- 2 * (precision * recall) / (precision + recall)
if(is.na(f1_score)) f1_score <- 0

cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla 'Democrat'):", round(precision, 2),
    "\nRecall (dla 'Democrat'):", round(recall, 2),
    "\nSpecificity (dla 'Democrat'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")

#' # 5B Wyniki na wykresach
# 5B Wyniki na wykresach ----
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)
)

ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryki Klasyfikacji", y = "Wartość", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

confusion_df <- as.data.frame(as.table(confusion_matrix))

confusion_df$Label <- c("True Democrat (TP)", "False Republican (FN)", 
                        "False Democrat (FP)", "True Republican (TN)")

ggplot(confusion_df, aes(x = Prawdziwe, y = Przewidziane, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Count") +
  labs(title = "Confusion Matrix", fill = "Count") +
  theme_minimal(base_size = 14)

#' # KROK 6: FUNKCJA (ANALIZA PLIKÓW TXT) 
# KROK 6: FUNKCJA (ANALIZA PLIKÓW TXT) ----

analizuj_plik_txt <- function(sciezka_do_pliku) {
  
  # Wczytanie pliku przy użyciu tryCatch.
  tekst_surowy <- tryCatch({
    paste(readLines(sciezka_do_pliku, warn = FALSE), collapse = " ")
  }, error = function(e) {
    warning(paste("Nie można otworzyć lub znaleźć pliku:", sciezka_do_pliku))
    return(NULL)
  })
  
  # Jeśli wczytywanie się nie powiodło, przerywamy działanie funkcji i zwracamy NULL
  if (is.null(tekst_surowy)) return(NULL)
  
  # Sprawdzenie długości tekstu
  if(nchar(tekst_surowy) < 50) {
    warning(paste("Tekst w pliku", sciezka_do_pliku, "jest za krótki do rzetelnej analizy!"))
    return(NULL)
  }
  
  # Obliczamy sentyment
  sentyment_wynik <- syuzhet::get_sentiment(tekst_surowy, method = "syuzhet")
  
  # Tworzymy jednoelementową ramkę danych do obróbki Tidytext
  df_txt_raw <- tibble(doc_id = 1, Tekst = tekst_surowy)
  
  # Przetwarzanie i nakładanie zamrożonego słownika oraz wag IDF
  df_txt_wide <- df_txt_raw %>%
    unnest_tokens(word, Tekst) %>%
    filter(!str_detect(word, "^http"), 
           !str_detect(word, "[0-9]")) %>%
    anti_join(stop_words, by = "word") %>%
    anti_join(custom_stop_words, by = "word") %>%
    mutate(word = SnowballC::wordStem(word, language = "en")) %>%
    count(doc_id, word) %>%
    # Dopasowujemy tylko te słowa, które model poznał podczas treningu
    inner_join(zamrozony_slownik_idf, by = "word") %>%
    mutate(tf_idf = n * idf) %>%
    select(doc_id, word, tf_idf) 
  
  # Jeśli w tekście nie ma ANI JEDNEGO słowa ze słownika treningowego
  if(nrow(df_txt_wide) == 0) {
    return(list(Werdykt = "Nieznany (brak pasujących słów)"))
  }
  
  # Transponujemy do formatu szerokiego (1 wiersz)
  df_txt_wide <- df_txt_wide %>%
    pivot_wider(names_from = word, values_from = tf_idf, values_fill = list(tf_idf = 0))
  
  # Uzupełniamy brakujące kolumny ze słownika treningowego wartościami 0
  missing_cols_fn <- setdiff(zamrozony_slownik, colnames(df_txt_wide))
  for(col in missing_cols_fn) {
    df_txt_wide[[col]] <- 0
  }
  
  # Sortujemy kolumny, aby ich układ był identyczny jak w modelu SVM i dodajemy sentyment
  df_txt <- df_txt_wide %>%
    select(all_of(zamrozony_slownik)) %>%
    mutate(EXTRA_Sentiment = sentyment_wynik)
  
  # Wyznaczenie werdyktu przez model SVM
  werdykt <- predict(svm_model, newdata = df_txt)
  
  return(list(Werdykt = as.character(werdykt)))
}
#' # KROK 7: Analiza historycznych wypowiedzi prezydentów
# KROK 7: Analiza historycznych wypowiedzi prezydentów ----

#chemy zobaczyć jak ocenianie sa historyczni prezydenci na przestrzeni lat zgodnie z obecnymi standardami

#Wgranie danych
dane_historyczne <- data.frame(
  Plik = paste0("prezydent", 1:10, ".txt"),
  Prezydent = c("G. Washington", "T. Jefferson", "A. Lincoln", "T. Roosevelt", 
                "W. Wilson", "F.D. Roosevelt", "D. Eisenhower", "J.F. Kennedy", 
                "R. Reagan", "B. Obama"),
  Rok = c(1790, 1801, 1861, 1901, 1913, 1933, 1953, 1961, 1981, 2009), 
  Prawdziwa_Partia = c("Niezależny", "Dem-Rep", "Republican", "Republican", 
                       "Democrat", "Democrat", "Republican", "Democrat", 
                       "Republican", "Democrat"),
  Werdykt_Modelu = NA,
  stringsAsFactors = FALSE
)

#Użycie funkcji z punktu 6
for (i in 1:nrow(dane_historyczne)) {
  
  wynik <- analizuj_plik_txt(dane_historyczne$Plik[i])
  
  # Jeśli plik istniał i funkcja zwróciła dane, zapisujemy je do tabeli
  if(!is.null(wynik)) {
    dane_historyczne$Werdykt_Modelu[i] <- wynik$Werdykt
  }
}


# Wizualizacja w czasie ----
ggplot(dane_historyczne, aes(x = Rok)) +
  geom_point(aes(y = Prawdziwa_Partia, color = "Prawdziwa Partia"), size = 8, alpha = 0.4) +
  geom_point(aes(y = Werdykt_Modelu, color = "Werdykt Modelu"), size = 4) +
  geom_text(aes(y = Prawdziwa_Partia, label = Prezydent), vjust = -1.5, size = 3.5) +
  labs(title = "Rzeczywistość a werdykt algorytmu w czasie", x = "Rok", y = "Partia") +
  theme_minimal()

#' # KROK 8: Chmury słów  oparte na TF-IDF
# KROK 8: Chmury słów oparte na TF-IDF ----

message("Generowanie chmur słów charakterystycznych dla partii...")
#' # 8A Przygotowanie danych do chmur 
# 8A Przygotowanie danych do chmur ----

party_word_tfidf <- tidy_tfidf %>%
  # Dołączamy informację o partii z oryginalnych danych treningowych
  left_join(wspolne_dane_treningowe %>% select(doc_id, Partia), by = "doc_id") %>%
  # Agregujemy: obliczamy średnie TF-IDF dla każdego słowa w ramach partii.
  # Dzięki temu słowa unikalne dla kilku tekstów danej partii zyskają na wadze.
  group_by(Partia, word) %>%
  summarise(mean_tfidf = mean(tf_idf), .groups = 'drop') %>%
  # Sortujemy od najwyższego TF-IDF
  arrange(desc(mean_tfidf))

#' # 8B Generowanie wizualizacji
# 8B Generowanie wizualizacji ----

# --- GRAFIKA 1: Chmura dla Demokratów ---
df_dem <- party_word_tfidf %>% filter(Partia == "Democrat") %>% head(100)
pal_dem <- brewer.pal(8, "Blues")[4:8]

par(mar = c(1, 1, 3, 1))

wordcloud(words = df_dem$word, 
          freq = df_dem$mean_tfidf, 
          scale = c(2.5, 0.4),     
          max.words = 60,          
          random.order = FALSE,     
          rot.per = 0.15,          
          colors = pal_dem)
title("Demokraci: Specyficzne słowa", col.main="#2171b5", cex.main=1.4, line = 1)


# --- GRAFIKA 2: Chmura dla Republikanów ---
df_rep <- party_word_tfidf %>% filter(Partia == "Republican") %>% head(100)
pal_rep <- brewer.pal(8, "Reds")[4:8]

par(mar = c(1, 1, 3, 1))

wordcloud(words = df_rep$word, 
          freq = df_rep$mean_tfidf, 
          scale = c(2.5, 0.4),    
          max.words = 60,          
          random.order = FALSE, 
          rot.per = 0.15, 
          colors = pal_rep)
title("Republikanie: Specyficzne słowa", col.main="#cb181d", cex.main=1.4, line = 1)

# Chmury słów generowane są m.in. na bazie najnowszych artykułów z stron internetowych CNN i Fox News
# Z tego powodu pojawiają się tam nagłówki bądź tematy wiadomości, które niewiele wspólnego mają z analizowanymi przemówieniami prezydentów
# Pozwalają za to łatwo odczytać, jaki temat obecnie jest często poruszany w danym źródle