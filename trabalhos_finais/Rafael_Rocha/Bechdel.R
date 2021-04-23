

#### Exercício Final-------------------------------------------------

### Data de entrega 12/04/21

## Instruções

## O resultado deverá ser entregue em um relatório ou apresentação à sua escolha. A submissão deve ser feita pelo 
## classroom, subindo um arquivo .zip contendo:
#       Arquivo .Rmd (gostaríamos de conseguir rodá-lo, ou seja, se ele for reprodutível é melhor)
#       Códigos auxiliares em R (se existirem)
#       O output final (em HTML, PDF, Word, etc). Você pode escolher o formato do trabalho final.
#       Não é necessário que a base movies esteja no repositório, já que ela pode ser lida diretamente da internet.

## O relatório/apresentação deve conter:
#       uma breve descrição da base de dados. 
#       os resultados da sua análise, com explicações/interpretações das visualizações construídas. Os resultados devem 
# conter pelo menos um gráfico em {ggplot2}, de preferência estilizado com um tema legal
#       uma conclusão tirada a partir da análise

## Dica 1: Procure não só impressionar, mas também produzir algo que considere útil para seu dia-a-dia. Afinal, nosso 
## objetivo com o curso é que você consiga usar mais o R/RMarkdown na sua vida!

##  Dica 2: Tente vender bem seu trabalho. Use sua criatividade! Os 3 melhores trabalhos receberão bolsas da Curso-R. 
## Os melhores trabalhos serão escolhidos com a ajuda de todas as pessoas que dão aula pela Curso-R.

##-------------------------------------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(waffle)
library(ggtext)
library(hrbrthemes)
library(viridis)

## Importar bancos de dados

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

saveRDS(movies, "movies.rds")
saveRDS(raw_bechdel, "raw_bechdel.rds")

movies <- readRDS("movies.rds")
raw_bechdel <- readRDS("raw_bechdel.rds")


## Mesclar bancos para termos um banco de APENAS DE bechel de 1888 a 2020

filmes_bechel_full <- left_join(raw_bechdel, movies, by="imdb_id") %>% 
  janitor::clean_names() 

#Vector of colors and font family
cores <-c(
  "fail" = "#B22222",
  "pass" = "#008080",
  "text" = "#454536",
  "title_fam" = "IBM Plex Sans SemiBold",
  "text_fam" = "IBM Plex Sans"
) 



## Análise dos filmes produzidos por gênero e década, e do $ investido e arrecadado, classificação etária, metascore e imdb, duração


movies_ano <- movies %>%
  count(year, binary) %>%
  group_by(year)

movies$periodo <- cut(movies$year, breaks=c(1970,1975,1980,1985,1990,1995,2000,2005,2010,2013), labels=c(1970,1975,1980,1985,1990,1995,2000,2005,2010), 
                      include.lowest=TRUE, right=FALSE)

movies_periodo <- movies %>%
  count(periodo, binary) %>%
  group_by(periodo)

movies_genero_ano <- movies %>%
  filter(!is.na(genre)) %>% 
  mutate(genre = str_extract(genre, "\\w+"), 
         genre = fct_lump_min(genre, 30)) %>% 
  count(year, genre)

f <- factor(c("1970","1975","1980","1985","1990","1995","2000","2005","2010","2013"), 
            levels = c("1970","1975","1980","1985","1990","1995","2000","2005","2010","2013"))


movies %>% 
  group_by(year, binary) %>% 
  summarise(num_filmes = n()) %>% 
  ggplot() +
  geom_line(aes(x = year, y = num_filmes, color = binary))+
  theme_ipsum()



movies_genero_ano %>% 
  ggplot(aes(x=year, y=n, group=genre, fill=genre)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Número de filmes lançados por gênero 1970-2013") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 9),
    axis.text.y=element_text(size=rel(0.5)),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~genre)


ggplot(movies_ano, aes(x=year, y=n, fill=binary)) + 
  geom_area()+
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Número de filmes lançados por gênero 1970-2013") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 9),
    plot.title = element_text(size=14))


ggplot(movies_ano, aes(x=year, y=n, fill=binary)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("Produção de filmes que passam ou não no teste de Bechdel por ano - 1970-2013")

ggplot(movies_periodo, aes(fill = binary, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~fct_relevel(periodo, "2005", after = 7), nrow = 1, strip.position = "bottom") +
  scale_x_continuous(breaks = seq(1970,2010,10), expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(x) x * 10, # multiplicar para a escala de Y refletir o número de filmes
                     expand = c(0,0)) +
  scale_fill_manual(values = c(cores[["fail"]], cores[["pass"]])) +
  coord_equal() +
  labs(
    title = "Filmes por período de 5 anos e o teste de Bechdel-Wallace (1970 - 2013)",
    x = NULL,
    y = "Número de filmes\n")+
  theme_minimal(base_family = "Roboto") + 
  theme(axis.ticks.y = element_line(),
        plot.title = element_text(color = cores[['text']], 
                                  family = cores[['title_fam']],
                                  size = 18),
        axis.title = element_text(color = cores[['text']],
                                  family = cores[['text_fam']]),
        axis.text = element_text(color = cores[['text']],
                                 family = cores[['text_fam']]),
        panel.grid = element_blank()
  )

ggplot(movies_periodo, aes(fill = binary, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~fct_relevel(periodo, "2005", after = 7), nrow = 1, strip.position = "bottom") +
  scale_x_continuous(breaks = seq(1970,2010,10), expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = function(x) x * 10, # multiplicar para a escala de Y refletir o número de filmes
                     expand = c(0,0)) +
  scale_fill_manual(values = c(cores[["fail"]], cores[["pass"]])) +
  coord_equal() +
  labs(
    title = "Filmes por período de 5 anos e o teste de Bechdel-Wallace (1970 - 2013)",
    x = NULL,
    y = "Número de filmes\n")+
  theme_minimal(base_family = "Roboto") + 
  theme(axis.ticks.y = element_line(),
        plot.title = element_text(color = cores[['text']], 
                                  family = cores[['title_fam']],
                                  size = 18),
        axis.title = element_text(color = cores[['text']],
                                  family = cores[['text_fam']]),
        axis.text = element_text(color = cores[['text']],
                                 family = cores[['text_fam']]),
        panel.grid = element_blank()
  )



movies_genero_ano <- movies %>%
  filter(!is.na(genre)) %>% 
  mutate(genre = str_extract(genre, "\\w+"), 
         genre = fct_lump_min(genre, 50)) %>% 
  count(year, genre)



library(ggridges)


ggplot(movies_genero_ano, aes(x = year, y = genre, fill = genre)) +
  scale_x_continuous(breaks = seq(1970,2010,5), expand = c(0.01, 0.01))+
  scale_y_discrete(expand = expand_scale(mult = c(0.01, .01))) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  theme_ridges() + 
  theme(legend.position = "none")

 


## Análise dos filmes que passaram ou não no teste por década, classificação etária, por duração e por gênero

movies_ano <- movies %>%
  count(year, binary) %>%
  group_by(year) %>%
  mutate(perc_rating = n / sum(n))


# Conta os filmes que passaram no teste por gênero com mais de 10 entradas

movies_genero <- movies %>%
  filter(!is.na(genre)) %>% 
  filter(!country=="USA") %>% 
  mutate(genre = str_extract(genre, "\\w+"), 
         genre = fct_lump_min(genre, 20)) %>% 
  count(genre, binary)



ggplot(movies_genero, aes(fill = binary, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~fct_reorder(genre, -n), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # multiplicar para a escala de Y refletir o número de filmes
                     expand = c(0,0)) +
  scale_fill_manual(values = c(cores[["fail"]], cores[["pass"]])) +
  coord_equal() +
  labs(
    title = "Filmes por gênero e o teste de Bechdel (1970 - 2013)",
    x = NULL,
    y = "Número de filmes por gênero\n")+
  theme_minimal(base_family = "Roboto") + 
  theme(axis.ticks.y = element_line(),
        plot.title = element_text(color = cores[['text']], 
                                  family = cores[['title_fam']],
                                  size = 18),
        axis.title = element_text(color = cores[['text']],
                                  family = cores[['text_fam']]),
        axis.text = element_text(color = cores[['text']],
                                 family = cores[['text_fam']]),
        panel.grid = element_blank()
  )


## Filmes não-americanos

movies_genero_semeua <- movies %>%
  filter(!is.na(genre)) %>% 
  filter(!country=="USA") %>% 
  mutate(genre = str_extract(genre, "\\w+"), 
         genre = fct_lump_min(genre, 10)) %>% 
  count(genre, binary)

#Vector of colors and font family
cores <-c(
  "fail" = "#B22222",
  "pass" = "#008080",
  "text" = "#454536",
  "title_fam" = "IBM Plex Sans SemiBold",
  "text_fam" = "IBM Plex Sans"
) 

ggplot(movies_genero_semeua, aes(fill = binary, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE, show.legend = FALSE) +
  facet_wrap(~fct_reorder(genre, -n), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # multiplicar para a escala de Y refletir o número de filmes
                     expand = c(0,0)) +
  scale_fill_manual(values = c(cores[["fail"]], cores[["pass"]])) +
  coord_equal() +
  labs(
    title = "Filmes por gênero e o teste de Bechdel (Filmes feitos fora dos EUA, 1970 - 2013)",
    x = NULL,
    y = "Número de filmes por gênero\n")+
  theme_minimal(base_family = "Roboto") + 
  theme(axis.ticks.y = element_line(),
        plot.title = element_text(color = cores[['text']], 
                                  family = cores[['title_fam']],
                                  size = 18),
        axis.title = element_text(color = cores[['text']],
                                  family = cores[['text_fam']]),
        axis.text = element_text(color = cores[['text']],
                                 family = cores[['text_fam']]),
        panel.grid = element_blank()
  )



## Batalha de franquias!
# Usando a base de raw_bechdel, comparar franquias: 
# Star Wars x Star trek, 
# Velozes e Furiosos e Transformers,
# Harry potter e Senhor dos anéis,
# Predador x Aliens, 
# Sexta feira 13, Hora do pesadelo 
# marvel e DC,



batalha_franquias <- raw_bechdel %>% 
  mutate(franquia = case_when(str_detect(title, "Harry Potter") ~ "Harry Potter"))

batalha_franquias <-batalha_franquias %>% 
  mutate(franquia = case_when(
    title %in% c("Fantastic Beasts: The Crimes of Grindelwald", 
                 "Fantastic Beasts and Where to Find Them",
                 "Harry Potter and the Sorcerer's Stone",
                 "Harry Potter and the Chamber of Secrets",
                 "Harry Potter and the Prisoner of  Azkaban",
                 "Harry Potter and the Goblet of Fire",
                 "Harry Potter and the Order of the Phoenix",
                 "Harry Potter and the Half-Blood Prince",
                 "Harry Potter and the Deathly Hallows: Part 1",
                 "Harry Potter and the Deathly Hallows: Part 2") ~ "Harry Potter"))

batalha_franquias <-batalha_franquias %>% 
  mutate(franquia = case_when(
    title %in% c("The Lord of the Rings: The Fellowship of the Ring",
                 "The Lord of the Rings: The Two Towers",
                 "The Lord of the Rings: The Return of the King",
                 "The Hobbit: An Unexpected Journey",
                 "The Hobbit: The Desolation of Smaug",
                 "The Hobbit: The Battle of the Five Armies") ~ "Lord of the Rings",
    TRUE ~ as.character(franquia)))

hp_lotr <- batalha_franquias %>%
  filter(franquia %in% c('Lord of the Rings', "Harry Potter"))

hp_lotr %>% 
  group_by(franquia) %>% 
  summarize(media_teste = mean(rating, na.rm = TRUE))

hp_lotr %>% 
  ggplot(aes(x=title, y=rating)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=title, 
                   xend=title, 
                   y=0, 
                   yend=rating)) + 
  labs(title="Lollipop Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

theme_set(theme_classic())

hp_lotr %>% 
  ggdotchart(x = "title", y = "rating",
             color = "franquia",                                # Color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
             sorting = "descending",                       # Sort value in descending order
             add = "segments",                             # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically
             group = "franquia",                                # Order by groups
             dot.size = 6,                                 # Large dot size
             label = round(hp_lotr$rating),                        # Add mpg values as dot labels
             font.label = list(color = "white", size = 9, 
                               vjust = 0.5),               # Adjust label parameters
             ggtheme = theme_pubr() ,                       # ggplot2 theme
  )


ggdotchart(hp_lotr, x = "title", y = "rating",
           color = "franquia",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 8,                                 # Large dot size
           group = "franquia", 
           y.text.col = TRUE,
           label = round(hp_lotr$rating),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 10, 
                             vjust = 0.4),             # Color y text by groups
           ggtheme = theme_pubr())+
  theme_cleveland()                                      # Add dashed grids

ggplot(hp_lotr, aes(x=title, y=rating)) +
  geom_segment( aes(x=title, xend=title, y=0, yend=rating), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+
  facet_wrap(~franquia)

## Análise dos filmes de nuvem de palavras dos filmes que passaram ou não



## Análise dos filmes premiados com o Oscar

oscars = movies %>% 
  mutate(awards = na_if(awards, "N/A")) %>% 
  select(year, imdb_id, title, clean_test, binary, awards) %>% 
  separate(awards,
           sep = "\\. ",
           into = c("big_wins_nominations", "wins_nominations"),
           fill = "left",
           remove = T) %>% 
  filter(str_detect(big_wins_nominations, "Oscar")) %>% 
  mutate(
    oscar_win = ifelse(str_detect(big_wins_nominations, "Won"), T, F),
    oscar_nom = ifelse(str_detect(big_wins_nominations, "Nominated"), T, F),
    oscar_no = as.numeric(str_extract(big_wins_nominations, "\\d+"))
  ) %>% 
  mutate(
    clean_test = factor(
      clean_test,
      levels =  c('ok', 'dubious', 'men', 'notalk', 'nowomen'),
      labels = c("Pass", "Dubious", "Women only talk about men",
                 "Women don't talk to each other",
                 "Fewer than two named women")
    )
  ) 
oscars_summary = oscars %>% 
  group_by(clean_test, oscar_win, oscar_nom) %>% 
  summarize(
    count_movies = n(),
    count_oscars = sum(oscar_no),
    binary = first(binary)
  ) %>% 
  ungroup() %>% 
  group_by(oscar_win) %>% 
  mutate(perc_movies = count_movies/sum(count_movies)) %>% 
  ungroup() %>% 
  mutate(oscars = case_when(oscar_win~"Wins",TRUE~"Nominations"))


## Plot

# Load fonts
extrafont::loadfonts('win')
# Oscars color palette
colors = c("#BD0628","#800155","#8C416B","#AF89AD","#FF9400")
p = ggplot(oscars_summary) +
  geom_vline(aes(xintercept = 0.5), linetype = "longdash", 
             size = 0.8, color = "grey50") +
  geom_col(
    aes(y = oscars, x = perc_movies, fill = clean_test), 
    alpha = 0.85, show.legend = T, color = "white"
  ) +
  scale_x_continuous(
    expand = c(0,0),
    labels = scales::percent,
    sec.axis = dup_axis(trans = rev)
  ) +
  scale_y_discrete(expand = c(0.3, 0.2)) +
  scale_fill_manual(
    "",
    values = rev(colors)
  ) +
  labs(
    title = "The Oscars and the Bechdel Test",
    subtitle = "Percentage of films between 1971 and 2013 that were nominated or won\nan academy award and their Bechdel Test result.",
    caption = "Data: FiveThirtyEight. Visualization: @loreabad6"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 9),
    text = element_text(family = "Futura LT", size = 13), 
    panel.background = element_rect(
      fill = "grey80", color = "transparent"
    ),
    plot.background = element_rect(
      fill = "grey80", color = "transparent"
    ),
    plot.subtitle = element_text(size = 12), 
    plot.caption = element_text(hjust = 0, size = 8),
    panel.grid = element_blank()
  )

