setwd("C:/Users/User/Documents/2019 Datafit/2019 Blogovi i kolumne/2020_Trump_debata")
library(tidyverse)
library(rvest)
library(stringi)
library(hms)
library(naniar)
library(patchwork)
library(ggdark)
library(rtrek)
library(janitor)
library(scales)

# age of elected presidents

age<-"https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age"

age_2<-read_html(age) %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
  html_table(fill = T)
age_2


# wrangle the df
age_2<-age_2 %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  slice(c(2:46 )) %>% 
  select(president, age_atstart_of_presidency)%>% 
  mutate(dob_pocetak=str_extract(age_atstart_of_presidency,"(\\d\\d)")) %>% 
  select(president, dob_pocetak) %>% 
  mutate(dob_pocetak=as.numeric(dob_pocetak),
         president=as.factor(president))

# plot the data
age_2 %>% 
  dplyr::mutate(president=fct_reorder(president, dob_pocetak)) %>% 
  tail(5) %>% 
  ggplot(aes(x=president,y=dob_pocetak)) + 
  geom_point(aes(fill="red", color="red", size=6))+
  geom_segment(aes(x=president, xend=president, y=40, yend=dob_pocetak, color="red", size=5))+
  geom_text(aes(label=dob_pocetak), hjust=-0.3, size=7)+
  guides(fill=F, size=F, colour=F)+
  coord_flip()+
  labs(title = "Age of last five US presidents on presidential inauguration",
       caption = "Age")+
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20),
        plot.caption = element_text(hjust = 0.5, size = 20))+
  scale_x_discrete(name='president',
                   breaks=c("Donald Trump","George H. W. Bush", "George W. Bush",
                            "Barack Obama", "Bill Clinton"),
                   labels=c("Donald\nTrump","George H. W.\nBush", "George W.\nBush",
                            "Barack\nObama", "Bill\nClinton"))
