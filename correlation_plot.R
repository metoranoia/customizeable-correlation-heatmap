library(ggplot2)
library(magrittr)
library(dplyr)
library(knitr)
library(tidyverse, warn.conflict=F)

LungCap_data <- read.csv("~/Learn R/bivariate analysis/LungCapData2.csv")
str(LungCap_data)

LungCap_data[sapply(LungCap_data, is.character)]=lapply(LungCap_data[sapply(LungCap_data, is.character)], as.factor)
str(LungCap_data)
summary(LungCap_data)

LungCap_data[sapply(LungCap_data, is.factor)]=lapply(LungCap_data[sapply(LungCap_data, is.factor)], as.numeric)

lungcap_data_cor <- Hmisc::rcorr(as.matrix(LungCap_data))
data.frame(lungcap_data_cor$r) %>% head() %>% kable()
data.frame(lungcap_data_cor$P) %>% head() %>% kable()
data.frame(lungcap_data_cor$n) %>% head(n=3) %>% kable()
cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  # turn all three matrices (r, n, and P into a data frame)
  Mdf <- map(M, ~data.frame(.x))
  # return the three data frames in a list
  return(Mdf)
}
cors(LungCap_data) %>% first() %>% head() %>% kable()

cors(LungCap_data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # look at the first element of the list (r)
  first() %>%
  head() %>%
  kable()

cors(LungCap_data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # format each data set (r,P,n) long
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  # look at the first element of the list (r)
  first() %>%
  head() %>%
  kable()

cors(LungCap_data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # format each data set (r,P,n) long
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  # merge our three list elements by binding the rows
  bind_rows(.id = "id") %>%
  head() %>%
  kable()

cors(LungCap_data) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # format each data set (r,P,n) long
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  # merge our three list elements by binding the rows
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  # change so everything is lower case
  rename(p = P) %>%
  mutate(sig_p = ifelse(p < .05, T, F),
         p_if_sig = ifelse(sig_p, p, NA),
         r_if_sig = ifelse(sig_p, r, NA)) %>%
  head() %>% 
  kable()

formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(sig_p, p, NA),
           r_if_sig = ifelse(sig_p, r, NA)) 
}
formatted_cors(LungCap_data) %>% head() %>% kable()

formatted_cors(LungCap_data) %>%
  ggplot(aes(x = measure1, y = measure2, fill = r)) +
  geom_tile()

formatted_cors(LungCap_data) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in LungCap Data",
       subtitle="Only significant Pearson's correlation coefficients shown") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))
