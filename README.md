# Analiza podatkov s programom R, 2015/16

Avtor: Andraž Pirnovar

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

## Analiza delovnih mest v Sloveniji

Ker je v današnjih časih velik problem zaposlitev, sploh za nas, mlade, bom analiziral, na katerih področjih gospodarstva je največ prostih delovnih mest, v katerem sektorju je največ zaposlenih, ter kakšna je zaposlenost po regijah.

Večino virov bom pridobil iz Statističnega urada republike Slovenije, bodo pa v oblikah .csv , .xml.
http://pxweb.stat.si/pxweb/Database/Dem_soc/07_trg_dela/30_07146_prosta_mesta/30_07146_prosta_mesta.asp

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
