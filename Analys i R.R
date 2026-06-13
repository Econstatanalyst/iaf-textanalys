# ============================================================
# IAF Textanalys - Komplett skript
# Syfte: Analysera hur IAF:s granskningsfokus förändrats
# över tid genom analys av 223 granskningsrapporter
# ============================================================

# ------------------------------------------------------------
# STEG 0: Ladda paket
# ------------------------------------------------------------
if (!require("pdftools")) install.packages("pdftools")
if (!require("tidytext")) install.packages("tidytext")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("topicmodels")) install.packages("topicmodels")

library(pdftools)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(topicmodels)

# ------------------------------------------------------------
# STEG 1: Läs in PDF:er (kör bara EN gång per session)
# OBS: Sätt working directory till mappen med PDF-filerna
# ------------------------------------------------------------
pdf_filer <- list.files("iaf_rapporter", pattern = "\\.pdf$", full.names = TRUE)
message("Antal PDF-filer hittade: ", length(pdf_filer))

# Funktion för att extrahera år från filnamnet
extrahera_ar <- function(filnamn) {
  ar <- str_extract(basename(filnamn), "20[0-9]{2}")
  return(ar)
}

# Läs in alla PDF:er och bygg dataframe
message("Läser in PDF:er...")
iaf_data <- data.frame()

for (i in seq_along(pdf_filer)) {
  message("[", i, "/", length(pdf_filer), "] ", basename(pdf_filer[i]))
  tryCatch({
    text <- pdf_text(pdf_filer[i]) %>% paste(collapse = " ")
    ar <- extrahera_ar(pdf_filer[i])
    iaf_data <- rbind(iaf_data, data.frame(
      filnamn = basename(pdf_filer[i]),
      ar = ar,
      text = text,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    message("  Kunde inte läsa: ", basename(pdf_filer[i]))
  })
}
message("Klart! Antal rapporter inlästa: ", nrow(iaf_data))




# ============================================================
# KÖR OM HÄRIFRÅN varje gång du uppdaterar stoppord
# ============================================================


# ------------------------------------------------------------
# STEG 2: Definiera stoppord (komplett lista - ändra här)
# ------------------------------------------------------------
stoppord <- c(
  # Svenska grundord
  "och", "att", "är", "det", "en", "av", "för", "som", "på",
  "med", "de", "till", "den", "har", "inte", "om", "vi", "kan",
  "ska", "ett", "sig", "men", "också", "när", "från", "så", "vid",
  "var", "han", "hon", "efter", "under", "över", "mot", "utan",
  "samt", "eller", "vilket", "vilka", "denna", "detta", "dessa",
  "inom", "genom", "enligt", "dock", "redan", "även", "alla",
  "hur", "där", "här", "mer", "än", "per", "varav", "hos",
  "jag", "vad", "dels", "bör", "hade", "vara", "finns",
  
  # Siffror och förkortningar
  "bl.a", "dvs", "t.ex", "m.m", "s.k", "ca", "dvs.",
  
  # Icke-informativa ord
  "iaf", "iaf:s", "rapport", "sidan", "tabell", "mellan",
  "antal", "antalet", "procent", "kassan", "kassorna",
  "lämna", "arbete", "personer", "sökande", "inskriven",
  "programdeltagare", "ersättningstagare", "arbetslös",
  
  # Städade efter ordfrekvensanalys
  "fick", "lämpligt", "första", "andel", "tid", "kassor",
  "uppgifter", "utbildning", "digitala", "leverantörer",
  "får", "stöd", "grund", "enheten", "visar", "exempel",
  "meddelande", "ärendet", "mån", "skäl", "annat",
  "närliggande", "skulle", "bli",
  
  # Städade efter TF-IDF analys
  "olof", "dahlgren", "fea", "krom", "ggr", "tusen",
  "lät", "svikligt", "hände", "astat", "avst", "vidgat", "kris",
  "vederbörande", "initierad", "påminner", "södra", "mälardalen",
  "norrland", "las", "meny", "användaren", "kognitiv", "vårdat",
  "begripligt", "upptäckta", "bortom", "trappsteg", "kartlagda",
  "skogs", "jämsides", "påpekandet", "huvudsysslan", "kartläggningen",
  "götaland", "bisysslan", "mnkr", "norra", "jan", "arb",
  
  # Städa efter LDA
  "övre", "låter", "denne", "yttrande"
)

# ------------------------------------------------------------
# STEG 3: Definiera ordformsregler (slå ihop varianter)
# ------------------------------------------------------------
normalisera_ord <- function(ord) {
  case_when(
    str_detect(ord, "^arbetsförmedling") ~ "arbetsförmedlingen",
    str_detect(ord, "^arbetslöshetskass") ~ "arbetslöshetskassorna",
    str_detect(ord, "^arbetssökand")      ~ "arbetssökande",
    str_detect(ord, "^ersättning")        ~ "ersättning",
    str_detect(ord, "^underrättels")      ~ "underrättelser",
    str_detect(ord, "^beslut")            ~ "beslut",
    str_detect(ord, "^sanktion")          ~ "sanktioner",
    str_detect(ord, "^granskning")        ~ "granskning",
    str_detect(ord, "^arbetslöshetsersättning") ~ "ersättning",
    str_detect(ord, "^arbetslöshetsförsäkring") ~ "arbetslöshetsförsäkringen",
    str_detect(ord, "^arbetsförmedl")     ~ "arbetsförmedlingen",
    
    # Nya sammanslagningar baserade på TF-IDF analys
    str_detect(ord, "^skyddsregel")       ~ "skyddsreglerna",
    str_detect(ord, "^systemutveckling")  ~ "systemutvecklingsprocess",
    str_detect(ord, "^täckningsgr")       ~ "täckningsgraden",
    str_detect(ord, "^jämställdhet")      ~ "jämställdhet",
    str_detect(ord, "^arbetsmarknadsanställ") ~ "arbetsmarknadsanställningar",
    str_detect(ord, "^geograf") ~ "geografiskt",
    str_detect(ord, "^utbetalning") ~ "utbetalningar",
    str_detect(ord, "^avvikelserapportering") ~ "avvikelserapportering",
    str_detect(ord, "^medlem") ~ "medlemmar",
    str_detect(ord, "^handlingsplan") ~ "handlingsplanen",
    str_detect(ord, "^återkall") ~ "återkallanden",
    str_detect(ord, "^sökaktivitet") ~ "sökaktiviteter",
    str_detect(ord, "^skyddsregel") ~ "skyddsreglerna",
    str_detect(ord, "^marknadsområd") ~ "marknadsområden",
    str_detect(ord, "^påanmäl") ~ "påanmälningar",
    
    TRUE ~ ord
  )
}

# ------------------------------------------------------------
# STEG 4: Tokenisera och filtrera (ett rent flöde)
# ------------------------------------------------------------
iaf_ord_filtrerat <- iaf_data %>%
  filter(!is.na(ar)) %>%                        # Ta bort PDF-filer där år saknas i filnamnet
  unnest_tokens(ord, text) %>%                  # Tokenisera till ett ord per rad
  filter(!ord %in% stoppord) %>%                # Ta bort stoppord
  filter(nchar(ord) > 3 | ord == "män") %>%     # Ta bort ord kortare än 4 bokstäver, med undantag för "män"
  filter(!str_detect(ord, "[0-9]")) %>%         # Ta bort ord med siffror
  filter(str_detect(ord, "^[a-zåäö]")) %>%      # Ta bort PDF-skräp som börjar med icke-bokstav
  filter(!str_detect(ord, "-$")) %>%            # Ta bort trasiga ord med bindestreck i slutet
  filter(!str_detect(ord, "-")) %>%             # Ta bort alla ord med bindestreck
  mutate(ord = normalisera_ord(ord))            # Normalisera ordformer

message("Klart! Antal ord: ", nrow(iaf_ord_filtrerat))

# ============================================================
# ANALYSER
# ============================================================

# ------------------------------------------------------------
# ANALYS 1: Ordfrekvensanalys
# ------------------------------------------------------------

# Vanligaste orden per år
# Lägg till normalisering
iaf_per_ar_norm <- iaf_ord_filtrerat %>%
  group_by(ar) %>%
  mutate(totalt = n()) %>%
  group_by(ar, ord) %>%
  summarise(andel = n() / first(totalt) * 1000, .groups = "drop") %>%
  group_by(ar) %>%
  slice_max(andel, n = 10)

ggplot(iaf_per_ar_norm, aes(x = reorder_within(ord, andel, ar), 
                            y = andel, fill = ar)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ar, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Vanligaste orden i IAF:s rapporter per år",
    subtitle = "Förekomster per 1 000 ord",
    x = NULL,
    y = "Förekomster per 1 000 ord"
  ) +
  theme_minimal()

ggsave("ordfrekvens_per_ar.png", width = 14, height = 10, dpi = 300, bg = "white")

# Nyckelordsutveckling över tid
nyckelord <- c("felaktiga", "kvinnor", "återkrav",
               "sanktioner", "underrättelser", "män")

trend_data <- iaf_ord_filtrerat %>%
  group_by(ar) %>%
  mutate(totalt_ord = n()) %>%
  filter(ord %in% nyckelord) %>%
  group_by(ar, ord, totalt_ord) %>%
  summarise(antal = n(), .groups = "drop") %>%
  mutate(andel = antal / totalt_ord * 1000)

ggplot(trend_data, aes(x = ar, y = andel, color = ord, group = ord)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Nyckelords utveckling i IAF:s rapporter 2015–2026",
    subtitle = "Förekomster per 1000 ord",
    x = NULL,
    y = "Förekomster per 1000 ord",
    color = "Nyckelord"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.key.size = unit(1.5, "cm")
  )

ggsave("nyckelord_trend.png", width = 12, height = 7, dpi = 300, bg = "white")

# ------------------------------------------------------------
# ANALYS 2: TF-IDF
# ------------------------------------------------------------
iaf_tfidf <- iaf_ord_filtrerat %>%
  count(ar, ord, sort = TRUE) %>%
  bind_tf_idf(ord, ar, n) %>%
  arrange(desc(tf_idf))

iaf_tfidf %>%
  group_by(ar) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(ord, tf_idf, ar),
             y = tf_idf, fill = ar)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ar, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Mest unika ord per år i IAF:s rapporter (TF-IDF)",
    x = NULL,
    y = "TF-IDF värde"
  ) +
  theme_minimal()

ggsave("tfidf_per_ar.png", width = 14, height = 10, dpi = 300, bg = "white")

# ------------------------------------------------------------
# ANALYS 3: LDA - Ämnesmodellering
# ------------------------------------------------------------

# Skapa Document Term Matrix
iaf_dtm <- iaf_ord_filtrerat %>%
  count(filnamn, ord) %>%
  cast_dtm(filnamn, ord, n)

message("DTM skapad: ", nrow(iaf_dtm), " dokument, ", ncol(iaf_dtm), " unika ord")

# Kör LDA med k antal tema.
set.seed(123)
iaf_lda <- LDA(iaf_dtm, k = 4, control = list(seed = 123))
message("LDA klar!")

# Extrahera beta (ord per tema)
iaf_topics <- tidy(iaf_lda, matrix = "beta")

# 
# ------------------------------------------------------------
# STEG 1: Unicitetsanalys - hitta ord som särskiljer teman
# Omforma data så att varje tema får en egen kolumn med beta-värden
# ------------------------------------------------------------
iaf_topics_wide <- iaf_topics %>%
  mutate(topic = paste0("tema_", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter_all(any_vars(!is.na(.))) %>%
  replace(is.na(.), 0)

# ------------------------------------------------------------
# STEG 2: Beräkna unicitet för varje ord i varje tema
# Unicitet = hur mycket vanligare ordet är i detta tema jämfört med övriga teman
# medel_andra = genomsnittligt beta för ordet i alla andra teman
# unicitet = ordets beta i detta tema / genomsnittligt beta i andra teman
# ------------------------------------------------------------

iaf_topics_unikt <- iaf_topics %>%
  group_by(term) %>%
  mutate(
    medel_andra = (sum(beta) - beta) / (n() - 1), # Snitt för övriga teman
    unicitet = beta / (medel_andra + 0.0001) # + för att undvika division med 0. 
  ) %>%
  ungroup()


iaf_topics_unikt %>%
  group_by(topic) %>%
  slice_max(unicitet, n = 10) %>% # Välj top 10 per tema
  ungroup() %>%
  ggplot(aes(x = reorder_within(term, unicitet, topic),
             y = unicitet, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Mest unika ord per tema (jämfört med övriga teman)",
    subtitle = "Unicitet = hur mycket vanligare ordet är i detta tema vs andra",
    x = NULL,
    y = "Unicitet"
  ) +
  theme_minimal()

ggsave("lda_unicitet.png", width = 12, height = 8, dpi = 300, bg = "white")

# Temafördelning över tid
# Extrahera gamma-värden – hur stor andel av varje dokument som tillhör varje tema
# Gamma är ett mått per dokument och tema, och summerar alltid till 1 per dokument
iaf_gamma <- tidy(iaf_lda, matrix = "gamma")
iaf_gamma <- iaf_gamma %>%
  mutate(ar = str_extract(document, "20[0-9]{2}")) # Extrahera år från filnamnet

# Namnge temana manuellt utifrån unicitetsanalysen
tema_namn <- c(
  "1" = "Arbetsförmedlingens uppföljning av arbetssökande",
  "2" = "A-kassornas interna styrning och ekonomi",
  "3" = "Granskning av felaktiga utbetalningar och bidragsbrott",
  "4" = "Likabehandling och rättssäkerhet vid sanktionsprövningar"
)
iaf_gamma <- iaf_gamma %>%
  mutate(tema = tema_namn[as.character(topic)])
# Beräkna genomsnittligt gamma per år och tema
# Detta visar hur stor andel av rapporterna ett givet år som tillhör varje tema
iaf_tema_ar <- iaf_gamma %>%
  group_by(ar, tema) %>%
  summarise(gamma = mean(gamma), .groups = "drop")

# Visualisera temafördelning
ggplot(iaf_tema_ar, aes(x = ar, y = gamma, fill = tema)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "#FA8072", "#8DB600", "#00CED1", "#9370DB"
  )) +
  labs(
    title = "Temafördelning i IAF:s rapporter per år",
    subtitle = "Baserat på LDA med 4 teman",
    x = NULL,
    y = "Andel",
    fill = "Tema"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  ) +
  guides(fill = guide_legend(ncol = 2))
ggsave("lda_temafordelning.png", width = 12, height = 7, dpi = 300, bg = "white")

# ------------------------------------------------------------
# Räkna antal ord efter rensning
# ------------------------------------------------------------
message("Totalt antal ord efter rensning: ", nrow(iaf_ord_filtrerat))

