remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))

download.file(url = "https://github.com/youngwoos/Doit_R/raw/master/Data/scala-library-2.11.8.jar", destfile = paste0(.libPaths()[1], "KoNLP/Java/scala-library-2.11.8.jar"))

library(showtext)
library(KoNLP)
useNIADic()

library(stringr)
library(dplyr)
library(ggplot2)
library(ggwordcloud)
library(tidytext)
library(tidyr)

text1 <- readLines("president_inaugurationspeech_leej.txt")
text2 <- readLines("president_inaugurationspeech_yoon.txt")
text3 <- readLines("president_inaugurationspeech_moon.txt")
text4 <- readLines("president_inaugurationspeech_park.txt")
text5 <- readLines("president_inaugurationspeech_leem.txt")
text6 <- readLines("president_inaugurationspeech_noh.txt")

text1 <- text1 %>% as_tibble() %>% mutate(president = "lee")
text2 <- text2 %>% as_tibble() %>% mutate(president = "yoon")
text3 <- text3 %>% as_tibble() %>% mutate(president = "moon")
text4 <- text4 %>% as_tibble() %>% mutate(president = "park")
text5 <- text5 %>% as_tibble() %>% mutate(president = "mb")
text6 <- text6 %>% as_tibble() %>% mutate(president = "noh")

bind_speeches <- bind_rows(text1, text2, text3, text4, text5, text6) %>% select(president, value)

speeches <- bind_speeches %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value= str_squish(value))

speeches <- speeches %>% unnest_tokens(input = value, output = word, token = extractNoun)

speeches <- speeches %>% count(president, word) %>% filter(str_count(word) > 1)

speeches <- speeches %>% filter(word != "있습니")

top10 <- speeches %>% group_by(president) %>% slice_max(n, n = 10, with_ties = F)

ggplot(top10, aes(x = reorder_within(word, n , president), y = n, fill = president)) + geom_col() + geom_text(aes(label = n), hjust = -0.3) + coord_flip() + labs(title = "speech compare", x = NULL, y = NULL) + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ president, scales = "free_y") + scale_x_reordered()

df_wide <- speeches %>%  pivot_wider(names_from = president, values_from = n, values_fill = list(n=0))

# 기준: 이재명
others_lee <- rowSums(select(df_wide, -word, -lee))
df_wide <- df_wide %>% 
  mutate(ratio_lee = (lee + 1) / sum(lee + 1),
         ratio_others_lee = (others_lee + 1) / sum(others_lee + 1),
         odds_lee = ratio_lee / ratio_others_lee)
# 기준: 윤석열
others_yoon <- rowSums(select(df_wide, -word, -yoon))
df_wide <- df_wide %>%
  mutate(ratio_yoon = (yoon + 1) / sum(yoon + 1),
    ratio_others_yoon = (others_yoon + 1) / sum(others_yoon + 1),
    odds_yoon = ratio_yoon / ratio_others_yoon)
# 기준: 문재인
others_moon <- rowSums(select(df_wide, -word, -moon))
df_wide <- df_wide %>%
  mutate(ratio_moon = (moon + 1) / sum(moon + 1),
         ratio_others_moon = (others_moon + 1) / sum(others_moon + 1),
         odds_moon = ratio_moon / ratio_others_moon)
# 기준: 박근혜
others_park <- rowSums(select(df_wide, -word, -park))
df_wide <- df_wide %>%
  mutate(ratio_park = (park + 1) / sum(park + 1),
         ratio_others_park = (others_park + 1) / sum(others_park + 1),
         odds_park = ratio_park / ratio_others_park)
# 기준: 이명박
others_mb <- rowSums(select(df_wide, -word, -mb))
df_wide <- df_wide %>%
  mutate(ratio_mb = (mb + 1) / sum(mb + 1),
         ratio_others_mb = (others_mb + 1) / sum(others_mb + 1),
         odds_mb = ratio_mb / ratio_others_mb)
# 기준: 노무현
others_noh <- rowSums(select(df_wide, -word, -noh))
df_wide <- df_wide %>%
  mutate(ratio_noh = (noh + 1) / sum(noh + 1),
         ratio_others_noh = (others_noh + 1) / sum(others_noh + 1),
         odds_noh = ratio_noh / ratio_others_noh)
# 확인
df_wide %>% 
  select(word, starts_with("odds_")) %>% 
  head(10)


# 각 대통령별 Top10 + Bottom10 단어 추출
top10_lee  <- bind_rows(
  df_wide %>% slice_max(order_by = odds_lee,  n = 10, with_ties = FALSE) %>% 
    select(word, odds_lee) %>% rename(odds_ratio = odds_lee) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_lee,  n = 10, with_ties = FALSE) %>% 
    select(word, odds_lee) %>% rename(odds_ratio = odds_lee) %>% mutate(rank_type = "bottom")
)

top10_yoon <- bind_rows(
  df_wide %>% slice_max(order_by = odds_yoon, n = 10, with_ties = FALSE) %>% 
    select(word, odds_yoon) %>% rename(odds_ratio = odds_yoon) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_yoon, n = 10, with_ties = FALSE) %>% 
    select(word, odds_yoon) %>% rename(odds_ratio = odds_yoon) %>% mutate(rank_type = "bottom")
)

top10_moon <- bind_rows(
  df_wide %>% slice_max(order_by = odds_moon, n = 10, with_ties = FALSE) %>% 
    select(word, odds_moon) %>% rename(odds_ratio = odds_moon) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_moon, n = 10, with_ties = FALSE) %>% 
    select(word, odds_moon) %>% rename(odds_ratio = odds_moon) %>% mutate(rank_type = "bottom")
)

top10_park <- bind_rows(
  df_wide %>% slice_max(order_by = odds_park, n = 10, with_ties = FALSE) %>% 
    select(word, odds_park) %>% rename(odds_ratio = odds_park) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_park, n = 10, with_ties = FALSE) %>% 
    select(word, odds_park) %>% rename(odds_ratio = odds_park) %>% mutate(rank_type = "bottom")
)

top10_mb   <- bind_rows(
  df_wide %>% slice_max(order_by = odds_mb,   n = 10, with_ties = FALSE) %>% 
    select(word, odds_mb)   %>% rename(odds_ratio = odds_mb) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_mb,   n = 10, with_ties = FALSE) %>% 
    select(word, odds_mb)   %>% rename(odds_ratio = odds_mb) %>% mutate(rank_type = "bottom")
)

top10_noh  <- bind_rows(
  df_wide %>% slice_max(order_by = odds_noh,  n = 10, with_ties = FALSE) %>% 
    select(word, odds_noh)  %>% rename(odds_ratio = odds_noh) %>% mutate(rank_type = "top"),
  df_wide %>% slice_min(order_by = odds_noh,  n = 10, with_ties = FALSE) %>% 
    select(word, odds_noh)  %>% rename(odds_ratio = odds_noh) %>% mutate(rank_type = "bottom")
)


top10_all <- bind_rows(
  top10_lee  %>% mutate(president = "lee"),
  top10_yoon %>% mutate(president = "yoon"),
  top10_moon %>% mutate(president = "moon"),
  top10_park %>% mutate(president = "park"),
  top10_mb   %>% mutate(president = "mb"),
  top10_noh  %>% mutate(president = "noh")
)


top10_all %>% filter(president %in% c("lee", "yoon", "moon")) %>% ggplot(aes(x = reorder_within(word, odds_ratio, president), y = odds_ratio,
fill = rank_type)) + geom_col() + coord_flip() + geom_text(aes(label = round(odds_ratio, 2)), hjust = -0.3, size = 3) + labs(title = "Speech Odds Ratio by President", x = NULL, y = "Odds Ratio (vs others)") + theme_minimal(base_family = "a") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.spacing = unit(1.5, "lines"), plot.margin = margin(5, 40, 5, 5)) + facet_wrap(~president, scales = "free_y") + scale_x_reordered()
