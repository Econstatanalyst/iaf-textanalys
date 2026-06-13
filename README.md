# Textanalys av IAF:s granskningsrapporter 2015–2026

## Om projektet
Detta projekt analyserar hur IAF:s (Inspektionen för arbetslöshetsförsäkringen) 
granskningsfokus förändrats över tid genom analys av 223 publicerade 
granskningsrapporter mellan 2015 och 2026.

## Fråga som besvaras i rapporten
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
- `iaf_rapporter` - samtliga rapporter som laddades ned med hjälp av R skript.
- `Analys i R.R` – komplett R-skript för all analys, inklusive nedladdning av rapporter.
- `Rapport.pdf` – rapport i text.

## Rapport
En fullständig rapport med analys och slutsatser finns tillgänglig i repositoryt.
