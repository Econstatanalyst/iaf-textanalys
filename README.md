# Textanalys av IAF:s granskningsrapporter 2015–2026

## Om projektet
Detta projekt analyserar hur IAF:s (Inspektionen för arbetslöshetsförsäkringen) 
granskningsfokus förändrats över tid genom analys av 223 publicerade 
granskningsrapporter mellan 2015 och 2026.

## Forskningsfråga
Hur har de teman och nyckelord som dominerar IAF:s granskningsrapporter 
förändrats över tid?

## Metod
Analysen genomförs med tre kvantitativa textanalysmetoder:
- **Ordfrekvensanalys** – identifierar de vanligaste orden per år
- **TF-IDF** – identifierar ord som är unika och karakteristiska för varje år
- **LDA** – automatisk ämnesmodellering som identifierar dolda teman

All analys är genomförd i R med paketen tidytext, topicmodels och ggplot2.

## Resultat
Analysen indikerar ett tydligt skifte i IAF:s granskningsfokus från underrättelser 
och sanktioner mot felaktiga utbetalningar och bidragsbrott, ett mönster som 
är konsistent across TF-IDF- och LDA-analyserna och som sammanfaller med 
ett explicit regeringsuppdrag från 2021.

## Filer
- `Analys i R.R` – komplett R-skript för all analys, inklusive nedladdning av rapporter
- `ordfrekvens_per_ar.png` – vanligaste ord per år
- `nyckelord_trend.png` – nyckelordsutveckling över tid
- `tfidf_per_ar.png` – TF-IDF analys
- `lda_unicitet.png` – LDA unicitetsanalys
- `lda_temafordelning.png` – temafördelning över tid

## Rapport
En fullständig rapport med analys och slutsatser finns tillgänglig i repositoryt.
