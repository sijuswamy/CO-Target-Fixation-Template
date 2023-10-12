---
title: | 
    | SAINTGITS COLLEGE OF ENGINEERING, KOTTAYAM (Autonomous)
    | First Year CO Attainment Target Fixation 2022-23
author: "Internal Quality Assurance Cell"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: 
  bookdown::html_document2:
    df_print: paged 
    toc: TRUE
    toc_float: TRUE
    number_sections: true
    fig_width: 7
    fig_height: 6
    fig_caption: true
css: styles.css
editor_options: 
  chunk_output_type: console
params:
  Programme: "ECE"
---
  
<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Lato" />
  
```{r rmarkdown-setup, echo=FALSE}
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(echo = FALSE)
```
```{r, setup, include=FALSE}
knitr::opts_chunk$set(error=FALSE)
```
```{r r-setup, echo=FALSE}
library(tidyverse)
library(knitr)
library(grid)
library(gridExtra)
library(formattable)
library(kableExtra)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#library(urbnthemes)

#set_urban_defaults(style = "print")
```
```{r,echo=F}
df=read.csv("MPCE_data.csv",header = TRUE)
df$Programme=as.factor(df$Programme)
ph=paste0("IQAC/",params$Programme)
```
```{r,fig.align='right',out.width = "15%"}
library(qrcode)
p=qr_code(ph)
plot(p)
```

# CO Attainment Target Fixation of  `r params$Programme` 

## Background

The education system in India over past few years has adopted Outcome Based Education (OBE) model since the model has potential to measure the learning outcomes. Accreditation is a *derivative* of outcome based education. In view of employment becoming more and more challenging, attributes such as knowledge, skill, values, attitude must be given due importance. This calls for design of the whole educational process that enables graduates to meet the set goals. The students should successfully demonstrate these attributes at the end of program which comprise of many
courses. Quality of teaching must be judged from quality of learning. Hence each program must have intended outcomes. For each course, course outcome statements are formed
which have linking with program outcomes. There must be a systematic and documented process in place for attainment of course outcomes which further contributes to attainment of
program outcomes. Stages in outcome based education are:

1. Creation of Goals, Objectives and Outcomes

2. Definition of outcome based process

3. Design of outcome based curriculum

4. Outcome based content delivery and learning

5. Assessment and evaluation of predefined outcomes

6. Reflections on outcome assessments and formulation of action points for improvements

## Significance of CO attainment target fixation

The Engineering admission is based on the rank in KEEEM / marks in higher secondary examinations. Even though the NEP recommends competency based education, presently it is not reflected in the higher secondary level. At present a student's expected level of performance in an Engineering programme can be estimated with his/her score in Physics, Chemistry and Mathematics. One of the key steps in attainment of course outcomes is setting the target value. So it is reasonable to design a PCM score based CO attainment target fixation mechanism for the first year academics.This fact sheet presents the CO attainment target fixation for the first year courses based on the PCME data from `r params$Programme` programme.

### Context

From 2020 onwards the Accreditation agencies have endeavoured to fix the CO attainment target before starting the OBE process of first year Engineering programmes. Scores in Physics, Chemistry and Mathematics courses is collected through a google form for the purpose of calculating the target score. Students expectation about the programme is also collected for setting the short term training goals. 


### Process

The first semester students are chosen for the census. Data is collected through a tested questionnaire.

### Target score calculation

The target score is calculated as the weighted average of Physics, Chemistry and Mathematics scores in the higher secondary board examination with weight distribution $W=(1, 1, 2)$. Since Mathematics is the essential tool for all the analytical subjects in the study, weight 2 is used.   

### Analysis of the open-ended response

The students are asked to give their expectations about the engineering programme they opted in Saintgits. Analysis would be carried out by aggregating the most occurring expectations in the student responses. This would provide an idea of the most general expectations of the first year students in the selected programme.


Here are some observations from the PCME survey response of `r params$Programme` programme.

```{r chart}

titler <- function(title) {
  textGrob(title, 
           x = unit(0, "npc"), 
           hjust = 0, 
           vjust = 0,
           gp = gpar(fontsize = 12, fontfamily = "Lato"))

}

subtitler <- function(subtitle) {
  textGrob(subtitle, 
           x = unit(0, "npc"), 
           hjust = 0, 
           vjust = 0,
           gp = gpar(fontsize = 9.5, fontfamily = "Lato"))
}
  
sourcer <- function(source) {
  grobTree(
    textGrob("Source: ", 
             name = "source1",
             x = unit(0, "npc"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato", fontface = "bold")),
    textGrob(source, 
             x = unit(0, "npc") + grobWidth("source1"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato"))
  )
}

noter <- function(endnote) {
  grobTree(
    textGrob("Notes: ", 
             name = "note1",
             x = unit(0, "npc"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato", fontface = "bold")),
    textGrob(endnote, 
             x = unit(0, "npc") + grobWidth("note1"), 
             hjust = 0, 
             vjust = 0,
             gp = gpar(fontsize = 8, fontfamily = "Lato"))
  )
}

caption <- grobTree(
  gp = gpar(fontsize = 7, hjust = 1), 
  textGrob(label = "I Q A C", 
           name = "caption1",
           x = unit(1, "npc"),  
           y = unit(0, "npc"),
           hjust = 1, 
           vjust = 0),
  textGrob(label = "S A I N T G I T S  ", 
           x = unit(1, "npc") - grobWidth("caption1") - unit(0.01, "lines"),         
           y = unit(0, "npc"), 
           hjust = 1, 
           vjust = 0, 
           gp = gpar(col = "#1696d2"))
)

plotr <- function(plot, title = NULL, subtitle = NULL, 
                  source = "library(fivethirtyeight)",
                  endnote = "") {
  grid.arrange(titler(title), 
              subtitler(subtitle), 
              plot, 
              caption, 
              sourcer(source), 
              noter(endnote),
              heights = c(1.5, 1, 25, 1, 1, 1)) 
}

```

```{r results='hide',error=FALSE}
df1=df %>%filter(Programme ==params$Programme)
#df1$Target_score=apply(df1[,c(5:24)],1,sum)
df1$Target_score=2*df1$Maths+df1$Physics+df1$Chemistry
df1$Target_score_index=(df1$Target_score-min(df1$Target_score))/(max(df1$Target_score)-min(df1$Target_score))
a=mean(df1$Target_score)
```
```{r, error=FALSE}
df1$Target_cat=cut(df1$Target_score,breaks=c(-Inf,240,384,Inf), labels=c("Low","Medium","High"))
```
## Classification of CO target levels

Students with Target score more than 80% is considered as the high level (level 3). Students with target score less than 50% is considerd as the low level (level 1). Those students with target score between 50 and 80 percentages are considered as medium level (level 2).



###  Mean target score

The mean target score of  `r params$Programme` students is `r round(a,2)`.


The Gender-wise mean and standard deviation of the target score in `r params$Programme` is shown in Table \@ref(tab:index-disc).

```{r ,index-disc}
## descriptive measures of content perception
FSa=group_by(df1, Gender) %>%
  summarise(
    mean = mean(Target_score
, na.rm = TRUE),
    sd = sd(Target_score
, na.rm = TRUE)
  )
kable(FSa,digits = 1,align = 'c', booktabs = TRUE,caption=paste0("Descriptive summary of gender-wise target score in ", params$Programme))  %>%kable_styling(bootstrap_options = c("striped", "hover"),full_width = T,position = "center")
#xtable::xtable(Fsa)
```


A percentage analysis  of Target score levels of respondents in  `r params$Programme` programme is shown in Table  \@ref(tab:category-disc).

```{r category-disc}
CatTab=prop.table(table(df1$Target_cat))*100
h=round(as.numeric(CatTab[3]),2)
kable(CatTab,digits = 1,align = 'lc', booktabs = TRUE,caption=paste0("A percentage analysis  of Target score Levels of respondents in  ", params$Programme),col.names = c('Target score Level','Percentage'))  %>%kable_styling(bootstrap_options = c("striped", "hover"),full_width = T,position = "center",)
h=round(as.numeric(CatTab[3]),2)
```
### CO attainment target fixing

From Table \@ref(tab:category-disc), it is clear that at present based on the higher secondary PCM score, there are `r h`% of the students in the `r params$Programme ` B.Tech programme is at the high level (level 3). Our ultimate goal is to elevate all the students to level 3 at the end of each course. So the CO attainment target is that at least 75% of the students in the `r params$Programme ` will be at level 3 while winding-up the first year courses. Also try to reduce the percentage in level 1 (low level) to zero.

**Note:**In Outcome Based Education, every student should attain at least a level 2 in all course outcomes. Formal instruction will create the knowledge base. Problem solving sessions with collaborative approach will strengthen the procedural aspects and thereby cultivate skills in the learner. Tutorials and group assignments with case studies sharpen the skills and build-up competency. Project based learning approach enable the student to solve real life problems. Thus knowledge-skill-competency-ability-attitude/behavior chain in outcome attainment will be completed. Keep this precedence rule in mind while planning and delivering a session.

## Indicators of Academic Strategic Plan

The Course faculties are expected to formulate an academic strategic plan based on following observations.

1. Use Table \@ref(tab:index-disc) to identify the category with higher standard deviation. This category in  `r params$Programme` deserves special attention since they may easily deviated from the mean performance.

2. Based on the gender-wise mean target score the department shall create skill development action plan for the `r params$Programme` B.Tech programme.

3. If the percentage in low target level is more than 5 (Table \@ref(tab:category-disc)), then special care must be given to the students in this category. Weekly remedial classes are strongly recommended. More practice problems shall be given to the medium target level students to uplift them into high target level.



## Analysis of the Students Expectation

*observations/suggestions to improve the overall teaching-learning experience*: A word cloud is created from the open ended question is shown bellow.


```{r}
docs <- Corpus(VectorSource(df1$Expectations))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case

#docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2","nil","nill","etc","nothing"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v,row.names = NULL)
```


```{r}
par(bg="white")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.1,col=brewer.pal(8, "Dark2"))
colors=terrain.colors(length(d$word), alpha=0.94)
```

A frequency distribution of the most repeated words in the open ended response from the `r params$Programme ` programme is shown bellow:

```{r bardbn, results='hide',fig.caption="Frequency distribution of words listed in the wordcloud"}
bpl=barplot(d[1:50,]$freq, las = 2, names.arg = d[1:50,]$word,
        col ="lightblue",
        ylab = "Word frequencies")
bpl
```

The most repeated phrases in the response are shown in the word cloud. The words with larger font size are most repeated in the open ended questions. The bar plot shows the frequency of each word represented in the word cloud. From the bar plot, most common expectation in the responses of `r params$Programme` programme are  shown in the highest bars in plot.

### Sentiment analysis of the open ended question 

The text mining and natural language processing tools are used to conduct a qualitative analysis of the expectations text. The Stanford sentiment analysis toolkit is used for this purpose. Ten dominant human emotions are filtered-out from the word stem. The percentage of emotions in the expectations about the course selected is shown in the figure below.

```{r}
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
```
```{r}
Open_response <- iconv(df1$Expectations)
```

```{r}
s <- get_nrc_sentiment(Open_response)
#head(s)
```
```{r}
#barplot(colSums(s),
#        las = 2,
#        col = rainbow(10),
#        ylab = 'Count',
#        main = 'Sentiment Scores of Open Response')

```

```{r}
#transpose
##td<-data.frame(t(s))
#The function rowSums computes column sums across rows for each level of a grouping variable.
##td_new <- data.frame(rowSums(td))
#Transformation and cleaning
##names(td_new)[1] <- "count"
##td_new <- cbind("sentiment" = rownames(td_new), td_new)
##rownames(td_new) <- NULL
##td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
##quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
```


```{r fig:sentiment,fig.caption="Sentiment analysis of student's expectations about the course"}
#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(s[, 1:10]))*100), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Open-ended Responses", xlab="Percentage"
)
```



This bar plot shows the comparison of the proportion of words associated with each emotion in the student's expectations. The emotion “positive” has the longest bar and shows that words associated with this positive emotion constitute more of all the meaningful words in this text. Overall, words associated with the positive emotions of “trust” and “joy” account for generative energy , which can be interpreted as a good sign of team health. If there are higher percentage in the negative emotions, then the training strategies should incorporate good support mechanism to develop trust, hope and confidence.


## Conclusion

Considering all these suggestions, all supporting departments handling courses in `r params$Programme` B.Tech Programme are advised to take necessary creative steps to improve the Students CO attainment of first year courses.




*DISCLAIMER:*`This is a system generated automated analysis report from the data collected through Google form.`
`Hence seal and signature not required`.

©*IQAC. Data from PCME_data 2022, Internal Quality Assurance Cell, SAINTGITS College of Engineering (Autonomous), Kottayam , Kerala- 686572. All data accessed through `rcurl`. *






