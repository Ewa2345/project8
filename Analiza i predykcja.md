---
title: "Analiza i predykcja wieku w zależności od czynników wpływających na kondycje serca"
author: "Ewa Bojke"
date: "25 stycznia 2020"
output: html_document
---

#Zbiór danych

\newline
W naszym projekcie będziemy używali danych ze strony https://kaggle.com.
\newline
Ta baza danych zawiera 76 atrybutów, ale wszystkie opublikowane eksperymenty odnoszą się do korzystania z podzbioru 14 z nich. W szczególności baza danych Cleveland jest jedyną, z której do tej pory korzystali naukowcy ML.  

**Zmiennne**

zmienna       opis
--------      ----
`age`          wiek
`sex`          płeć
`cp`           indykator bólu w klatce piersiowej(od 0 do 1 wartości)
`trestbps`     spoczynkowe ciśnienie krwi
`chol`         surowica cholestoral w mg / dl
`fbs`          indykator czy cukier na czczo(0=nie, 1=tak)  
`restecg`      indykator spoczynkowych wyników elektrokar(0,1)
`thalach`      maksymalne osiągnięte tętno
`exang`        indykator dusznicy bolesnej wywołanej wysiłkiem                          fizycznym(0=nie,1=tak)
`oldpeak`      depresja ST wywołana wysiłkiem w stosunku do odpoczynku

      
\newline 
**Sformułowanie problemu badawczego oraz agenda do zadań **\newline
\newline

W naszym projekcie 

\newline

- przygotujemy dane do interpretacji tzw. czyszczenie danych i ich          wczytanie

- przeanalizujemy zmienne i zależności między nimi tzw. eksploracyjna       analiza danych\newline

- stworzymy model predykcyjny wieku osób chorych \newline

- sprawdzimy jaki model najbardziej pasuje do danych

\newline 

#Wczytywanie danych

```{r}
heart = read.csv("C:/Users/Ewci/Desktop/wnioskowanie II/projekt/heart.csv")
colnames(heart)=c('wiek', 'plec', 'bol', 'cisnienie','cholesterol', 'cukier','spo.wyn','max tetno','dusznica','depresja ST')
```

\newline 

#Zamiana odpowiednich zmiennych na czynniki

```{r}
library(dplyr)
heart <- heart %>% mutate(plec = factor(plec, levels = c(man=0, woman = 1), labels = c("man","woman"))) %>% mutate(cukier = factor(cukier, levels = c(cukrzyk= 1, zdrowy = 0), labels = c("cukrzyk", "zdrowy")))%>% mutate(dusznica = factor(dusznica, levels = c(dusznica= 1, `bez dusznicy` = 0),labels = c("dusznica", "bez dusznicy")))%>% mutate(spo.wyn = factor(spo.wyn, levels = c(duze= 1, `male` = 0),labels = c("duze", "male")))%>% mutate(bol = factor(bol, levels = c(bol= 1, `brak bolu` = 0),labels = c("bol", "brak bolu")))
```

Wnioski i obserwacje:

Zamieniliśmy zmienne numeryczne plec, cukier, dusznica, bol, spo.wyn na zmienne kategoryczne. Użyjemy ich do analizy wariancji w kolejnych etapach projektu.

#Podstawowe statystyki dla zmiennych w naszym zbiorze danych

```{r}
lapply(heart,summary)
```
Wnioski i obserwacje:

Możemy zaobserwować, że wiek badanych ludzi waha się od 29 do 77 lat.
Przy czym połowa ludzi ma wiek w przedziale 47.5 - 61 lat z
medianą 55 lat. W badaniach mamy więcej kobiet niż mężczyzn.

Jeśli chodzi o ciśnienie skurczowe  krwi, to możemy zauważyć, że wahania występują między 94mm a 200mm. Większa część badanych ma wyniki ciśnienia skurczowego między 120 a 140mm z medianą 130mm. Ogólnie O nadciśnieniu mówimy już w momencie, gdy ciśnienie skurczowe przekracza 140 mm .

Natomiast wyniki cholesterolu u zdrowego człowieka nie powinny przekraczać 190mg. W naszych danych wahania występują między 126mg a 409mg. Większość badanych ma wyniki w przedziale 211 mg - 274.5mg. 

Ostatnią zmienną, którą zbadamy to tętno. Wahania występują między 71 a 202 uderzeń serca na minutę. Większa część osób ma tętno w  przedziale (134,166) z medianą równą 153. Wynikiem zagrażającym zdrowiu jest przekroczenie 100 uderzeń serca na minutę.



#Korelacje między zmiennymi
```{r}
cor(heart$cisnienie,heart$cholesterol)
cor(heart$cisnienie,heart$`max tetno`)
cor(heart$cholesterol,heart$`max tetno`)
cor(heart$cisnienie,heart$`depresja ST`)
cor(heart$cholesterol,heart$`depresja ST`)
cor(heart$`max tetno`,heart$`depresja ST`)

```

Wnioski i obserwacje:

Większość zmiennych nie jest skorelowana. Jedynie mamy większą korelację przy zmiennych ciśnienie i depresja ST, przy zmiennych ciśnienie i cholesterol oraz przy zmiennych max tetno i depresja ST. Korelacja wynosi odpowiednio około 0.2, 0.14 i -0.34.

#Przykładowe zależności między zmiennymi i modele liniowe dla zmiennych

```{r}
mod1=lm(heart$cholesterol~heart$cisnienie)
plot(heart$cisnienie,heart$cholesterol, xlab="Cisnienie krwi", ylab="Cholesterol")
abline(mod1,lwd=4,col='red')


mod2=lm(heart$`max tetno`~heart$cisnienie)
plot(heart$cisnienie, heart$`max tetno`, xlab="Cisnienie ", ylab="Max tetno")
abline(mod2,lwd=4,col="red")

mod3=lm(heart$`max tetno`~heart$cholesterol)
plot(heart$cholesterol,heart$`max tetno`, xlab="Cholesterol", ylab="Max tetno")
abline(mod3,lwd=4,col="red")

mod4=lm(heart$`depresja ST`~heart$cisnienie)
plot(heart$cisnienie,heart$`depresja ST`, xlab="Cisnienie",ylab="Depresja ST")
abline(mod4,lwd=4,col="red")

mod5=lm(heart$`depresja ST`~heart$cholesterol)
plot(heart$cholesterol,heart$`depresja ST`, xlab="Cholesterol", ylab="Depresja ST")
abline(mod5,lwd=4,col='red')

mod6=lm(heart$`depresja ST`~heart$`max tetno`)
plot(heart$`max tetno`,heart$`depresja ST`, xlab="Max tetno", ylab="Depresja ST")
abline(mod6,lwd=4,col='red')


```


Wnioski i obserwacje:

Widzimy,że jest zależność między zmiennymi, ale bardzo mała.
Największą zależność możemy zauważyć między zmiennymi depresja ST i max tetno.

\newpage
#Badanie zależności
```{r}
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)
```

Wnioski i obserwacje:

Jak widać większość zmiennych jest słabo zależne i  są słabo skorelowane.
Jedynie w ostanim modelu widzimy pewną zależność. Błąd standardowy reszt wynosi w tym przypadku tylko 1.141. Mod6 wyjaśnia około 11.5% wariancji depresji ST biorąc pod uwagę maksymalne tętno.


#Modele w regresji wieloliniowej

Regresja wieloliniowa jest to wyjaśnianie wartości zmiennej zależnej za pomocą więcej niż jednego predyktora. 
Stworzymy model nie biorący pod uwagę zależności pomiędzy zmiennymi 

```{r}
model = lm(heart$wiek~+cholesterol+`max tetno`+cisnienie, heart)
summary(model)
```

Wnioski i obserwacje:

Każdy wzrost o 1 mg cholesterolu sprawia, że średnia wartość wieku   będzie wzrastała o 0.03 lat, czyli niecałe 11 dni.(zakładając, że pozostałe zmienne się nie zmieniają)

Każdy wzrost o 1mm ciśnienia krwi sprawia, że średnia wartość wieku będzie wzrastała o 0.12 lat, czyli niecałe 44 dni.(zakładając, że pozostałe zmienne się nie zmieniają)

Każdy wzrost o 1 uderzenie serca na minutę sprawia, że średnia wartość wieku będzie malała o 0.15lat, czyli niecałe 54 dni.(zakładając, że pozostałe zmienne się nie zmieniają)

Współczynnik determinacji wynosi 0.2464. Natomiast błąd standardowy reszt 7.884. Model wyjaśnia 24% wariancji wieku. W większości wyników mamy taką sytuację, że osoby mające coraz słabszą kondycję serca są starsze. Jeżeli osoby badane mają podwyższone tętno to wystąpi to przeważnie w młodszym wieku. 
 

Dodajemy zmienną depresja ST i tworzymy model.
```{r}
model2ST = lm(heart$wiek~+cholesterol+`max tetno`+cisnienie+`depresja ST`, heart)
summary(model2ST)
```

Wnioski i obserwacje:

Tak jak wcześniej widzimy przeważnie średni wzrost zmiennej wiek. Zauważmy że nowa zmienna zmniejszyła współczynnik determinacji, który teraz wynosi  0.2445 i zwiększyła błąd standardowy reszt, który teraz wynosi 7.894. Możemy powiedzieć, że ten model nie jest lepszy od poprzedniego. Depresja ST jest skorelowana z ciśnieniem i z max tetno i dlatego model zbytnio się nie różni chociaż błąd standardowy reszt jest większy. Informacje, które są przechowywane w zmiennej ciśnienie i max tetno zawiera również zmienna z nimi skorelowana depresja ST.

#Model nie biorący pod uwagę zależności pomiędzy wszystkimi zmiennymi
```{r}
model.model= lm(heart$wiek~`max tetno`+cholesterol, heart)
summary(model.model)
```

Wnioski i obserwacje:

Błąd standardowy reszt jest mniejszy w pierwszym modelu o nazwie "model" i wynosi 7.884 lat, natomiast w modelu "model.model" błąd standardowy reszt wynosi 8.159 lat i współczynnik determinacji jest sporo mniejszy w drugim modelu. Możemy stwierdzić, że model o nazwie "model.model" nie jest lepszy. Szukamy dalej bardziej dopasowanego modelu do naszych danych. 

#Analiza wariancji

Możemy starać się przewidywać wartość zmiennej wiek za pomocą zmiennej
cukier. Może doprowadzić nas to do pewnych wniosków, np takich ,że im człowiek jest starszy tym jego podatność na cukrzyce wzrasta. 

Zwróćmy jednak uwagę na istotną różnicę w stosunku do regresji prostej. Mianowicie, zmienna cukier nie jest liczbowa. W takiej sytuacji, gdy zmienną numeryczną przewidujemy za pomocą zmiennej kategorycznej, mówimy, że mamy do czynienia z analizą wariancji(ANOVA).

Hipoteza Ho:

Model i model.model nie różnią się statystycznie.
```{r}
model1 = lm(heart$wiek~cholesterol+`max tetno`+cisnienie, heart)
model.model1 = lm(heart$wiek~cholesterol*`max tetno`*cisnienie, heart)
anova(model1,model.model1)

```

Wnioski i obserwacje:

P-value jest małe więc nie akceptujemy hipotezy Ho.

#Analiza kowariancji

Zmienną numeryczną możemy wyjaśniać za pomocą innej zmiennej
numerycznej i zmiennej kategorycznej jednocześnie.

Jakie mamy predyktory?

Jednym z dobrych predyktorów wieku osoby jest cisnienie. Rzecz jasna, że osoby  starsze mają większe ciśnienie niż osoby młodsze.


```{r}
library(ggplot2)
ggplot(heart, aes(x =cisnienie,y = wiek)) + geom_point() 

heart %>% filter(!is.na(cisnienie) & !is.na(wiek)) %>%
summarise(correlation = cor(cisnienie, wiek))
```

Wnioski i obserwacje:

Widzimy, że współczynnik korelacji jest istotny i wynosi około 0.3, jednakże nie jest bardzo duży, co sugeruje, że na wiek wpływają również inne czynniki.


# Korelacje wieku z innymi zmiennymi numerycznymi

```{r}
library(GGally)
ggpairs(heart %>% select_if(is.numeric))

```

Wnioski i obserwacje:

Możemy zauważyć, że dobrym predyktorem jest również cholesterol (współczynnik korelacji ponad 0.2), depresja ST (wspołczynik korelacji 0.21) i max tetno (wspolczynnik około -0.4).

#Dwa modele liniowe i i ich porównanie
```{r}
model.cisnienie<-lm(wiek~cisnienie,heart)
model.cukier<-lm(wiek~cukier,heart)
model.cs<-lm(wiek~cukier+cisnienie,heart)
summary(model.cisnienie)
summary(model.cukier)
summary(model.cs)

```

Wnioski i obserwacje:

Jak można było oczekiwać model zawierający dwie zmienne niezależne jest
lepiej dopasowany od modeli z pojedynczymi zmiennymi. Współczynnik determinacji w modelu z dwoma zmiennymi niezależnymi wynosi około 8%. W modelach z pojedyńczą zmienną dużo mniej.

#Jaki procent wariancji wyjaśnia model?

```{r}
ggplot(heart %>% filter(!is.na(cukier)), aes(x = cukier, y = wiek)) +
geom_boxplot(notch = TRUE)
```

Wnioski i obserwacje:

Model z dwoma zmiennymi wyjaśnia około 8% wariancji wieku osoby badanej. Widzimy, że to czy ktoś jest cukrzykiem czy nie zależy od wieku.


#Model o największej zdolności predykcji

Założenia: Staramy się, by model był jak najprostszy, gdyż nie
zależy nam by dopasował się za dobrze do naszych danych.

Jednym z typowych kryteriów używanych do wyboru modelu jest zmodyfikowany współczynnik determinacji. Zawiera on w sobie
informację o liczbie parametrów. Nie zawsze rośnie przy bardziej skomplikowanych modelach. Lepszą metodą jest tzw. kryterium
informacyjne. AIC sprawdza się dobrze w modelach predykcyjnych.

Rozważymy pełny model, z wszystkimi możliwymi interakcjami.


```{r}
model.full<-lm(wiek~bol*cisnienie*cholesterol*`max tetno`*`depresja ST`+cukier*dusznica,na.omit(heart))
summary(model.full)
```

Wnioski i obserwacje:

Większość zmiennych jest nieistotna statystycznie. Jest tak dlatego, że informacja została podzielona pomiędzy mnóstwo predyktorów i interakcji pomiędzy nimi.

Stwórzmy sensowniejszy model.
Dodajemy wszystkie dostępne zmienne. A zwłaszcza zmienną cisnienie, zmienną max tetno i zmienną cholesterol. Są one najbardziej skorelowane ze zmienną wiek.

```{r}
model.zlozony <-lm(wiek~cholesterol+cisnienie+spo.wyn+`depresja ST`+`max tetno`+bol+dusznica+cukier:cisnienie,na.omit(heart))
summary(model.zlozony)


```


Uprośćmy teraz model przy użyciu AIC.
```{r}
model.zlozony.aic <- step(model.zlozony)
```

Wnioski i obserwacje:

Jak widzimy aby model był dokładniejszy wymagane było usunięcie zmiennych depresja ST, cukier i spo.wyn. Ponadto wybrany model okazał się mieć niższe AIC niż model pełny.

```{r}
extractAIC(model.full)
extractAIC(model.zlozony.aic)
```

Wnioski i obserwacje:

Znaleźliśmy najlepiej dopasowany model do naszych danych. Jest to model.zlozony.aic.

#Wykresy diagnostyczne najlepszego modelu

Pierwszy wykres przedstawia reszty wykreślone wględem wartości dopasowanych. Punkty powinny być równomiernie rozrzucone względem osi `x`. Czerwona linia (wygładzenie) wskazuje na lekkie niedopasowane modelu dla mniejszych wartości. 

Drugim wykresem jest wykres kwantyl-kwantyl standaryzowanych reszt. Reszty standaryzowane, są to reszty podzielone przez odchylenie standardowe:
$$t_i = \frac{e_i}{s \sqrt{1 - h_{ii}} }$$
Wykres kwantyl-kwantyl powinien przypominać linię prostą. Tutaj widzimy pewne niedopasowanie w przypadku bardziej ekstremalnych obserwacji. 

Trzeci wykres jest wygodny do badania homoskedastyczności (czerwona krzywa powinna być jak najbardziej pozioma).

Czwarty wykres służy do badania, które obserwacje są wpływowe. Uwidoczniona jest na nim odległość Cooka, która bada różnicę predykcji modelu wyjściowego z modelem po usunięciu obserwacji. Można udowodnić, że wyraża się ona wzorem
$$C_i = e_i \left( \frac{n-p}{p} \cdot\frac{h_{ii}}{1 - h_{ii}} \right)^{1/2} $$

```{r}
plot(model.zlozony.aic)
```



