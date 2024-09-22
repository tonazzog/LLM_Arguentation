library(DBI)
library(odbc)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(tidyverse)
library(lsr)
library(FSA)
library(dunn.test)

options(scipen = 999)

# Utility functions ------------------------------------------------------------------------------
create_box_plot <- function(plot, theme, xlab, ylab, title, palette, save_as){
  
  p <- plot + 
    theme +
    geom_boxplot() + 
    labs(x = xlab, y = ylab) +
    theme(legend.position="none") +
    scale_fill_manual(values = palette) +
    ggtitle(title) +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5))
  p
  
  ggsave(save_as, plot=p, device='svg', width=10.667, height = 8)
  
  return(p)
  
}

create_bar_plot_mean <- function(plot, theme, xlab, ylab, title, palette, save_as){
  
  p <- plot +
    theme +
    geom_bar(stat='summary', fun='mean') +
    stat_summary(fun = mean, geom = "bar") + 
    stat_summary(fun.data = "mean_sdl", geom = "errorbar", fun.args = list(mult = 1),
                 position = position_dodge(width = 0.90), width = 0.1) +
    labs(x = xlab, y = ylab) +
    theme(legend.position = 'none') +
    scale_fill_manual(values = palette) +
    ggtitle(title) +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5))
    
    ggsave(save_as, plot=p, device='svg', width=10.667, height = 8)
    
    return(p)
    
}

create_bar_plot_identity <- function(plot, theme, xlab, ylab, labs_fill, title, palette, save_as){
  
  p <- plot +
    theme +
    geom_bar(stat = "identity", width=0.7) + 
    labs(x = xlab, y = ylab, fill = labs_fill) +
    theme(legend.position = 'bottom') +
    scale_fill_manual(values = palette) +
    ggtitle(title) +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5))
  
  ggsave(save_as, plot=p, device='svg', width=10.667, height = 8)
  
  return(p)
  
}

create_bar_plot_dodged_identity <- function(plot, theme, xlab, ylab, labs_fill, title, palette, save_as){
  
  p <- plot +
    theme +
    geom_bar(stat = "identity", width=0.7, position = position_dodge2(preserve = "single")) + 
    labs(x = xlab, y = ylab, fill = labs_fill) +
    theme(legend.position = 'bottom') +
    scale_fill_manual(values = palette) +
    ggtitle(title) +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5))
  
  ggsave(save_as, plot=p, device='svg', width=10.667, height = 8)
  
  return(p)
  
}

basic_statistics<- function(variable, xlab, description) {
  
  summary(variable)
  sd(variable)
  IQR(variable)
  hist(variable, main = paste("Histogram ", description),
       xlab = xlab)
  plot(density(variable), main =paste("Density plot ", description), 
       xlab = xlab)
  qqnorm(variable)
  qqline(variable)
  shapiro.test(variable)
  
}

# Palettes ---------------------------------------------------------------------
brewer.pal(n=6,"Set1")

palette_adv <- c("#ffa600", "#ff7c43", "#f95d6a", "#d45087" ,"#a05195", "#665191")
palette_adv <- c("#0fb5ae", "#4046ca", "#72e06a", "#de3d82" ,"#7e84fa", "#f7d046")
palette_adv <- c("#5884b0", "#97d788", "#e56264", "#fbc6d8", "#a9d1ec", "#f3d46e")
palette_adv <- c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7")
palette_adv <- c("#0070a3", "#6b72bf", "#bf69bb", "#fd6194", "#ff7759", "#ffa600")
palette_adv <- c('#ffa600', '#ff7c5d', '#ff6aa0', '#db78d3', '#868ae6', '#0091d4')

# color brewer accent
palette_adv <- c("#7FC97F", "#BEAED4", "#FDC086", "#ffee65", "#386CB0", "#F0027F")

palette_llm <- c('#10a37f', '#f0eee5','#e95169','#416ff8','#d3e3fd','#ffa600','lightgray')


llm_colors = c("ChatGPT" = palette_llm[1], "Claude" = palette_llm[2],  "Gemini"= palette_llm[3], "Gemma" = palette_llm[4], "Llama" = palette_llm[5], "Mistral" = palette_llm[6], "Student" = palette_llm[7])

llm_colors

palette_2_col <- c("#bf69bb",'lightgray')

palette_open_closed <- c("#f95d6a","#72e06a",'lightgray')

theme_custom <- theme_bw(base_size = 20)

closed <- c("ChatGPT","Claude","Gemini")

# DB connection ----------------------------------------------------------------
conn <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = ".",
  Database = "argumentation",
  Trusted_connection = TRUE
)


# Data -----------------------------------------------------------------------

## Essays ====================================================================
essays <- dbGetQuery(conn, "SELECT * FROM [dbo].[FILES_ARGUMENT]")

## TTR
query = "SELECT 
  	F.file_id, 
  	F.model,
  	COUNT(DISTINCT text) / CAST(COUNT(text) AS FLOAT) AS ttr
  FROM [dbo].[PARSED_ARGUMENT] AS P
  INNER JOIN [dbo].[FILES_ARGUMENT] AS F
  	ON F.file_id = P.file_id
  WHERE lemma != 'PUNCT'
  GROUP BY F.file_id, F.model
"
ttr <- dbGetQuery(conn, query)

## Adverbials=================================================================
adverbials <- dbGetQuery(conn, "SELECT * FROM [dbo].[LINKING_ADVERBIALS]")

## Adverbials frequencies ====================================================
query = "  
  SELECT A.category, A.construction_type, A.marker, (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN (
  	SELECT 
  		Category,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
  	GROUP BY Category
  ) W ON W.category = A.category
  GROUP BY A.marker_id, A.category, A.construction_type, A.marker, W.word_count
"
adverbials_freq <- dbGetQuery(conn, query)

## Adverbials by file ========================================================
query = "  
  SELECT F.model, F.file_id, COUNT(*) AS adverbials_count, F.word_count,
    (COUNT(*) /  CAST(F.word_count AS FLOAT)) * 1000 AS adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN FILES_ARGUMENT AS F
  	ON F.file_id = A.file_id 
  GROUP BY F.model, F.file_id, F.word_count
"
adv_by_file <- dbGetQuery(conn, query)

adv_by_file$author  <- ifelse(adv_by_file$model == "Student", "Student", "LLM")

adv_by_file$author_type  <- ifelse(adv_by_file$model == "Student", "Student", ifelse(adv_by_file$model  %in% closed, "Closed", "Open"))


## Adverbials by category ====================================================
query = "
  SELECT F.category, 
    COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
    COUNT(A.marker_id) AS adverbials_count, W.word_count, 
    (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN FILES_ARGUMENT AS F
  	ON F.file_id = A.file_id
  INNER JOIN (
  	SELECT 
  		category,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
  	GROUP BY category
  ) W ON F.category = W.category
  GROUP BY F.category, W.word_count
"
adv_by_category <- dbGetQuery(conn, query)

## Adverbials by model ====================================================
query = "
  SELECT F.model, COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
  COUNT(A.marker_id) AS adverbials_count, W.word_count, 
  (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN FILES_ARGUMENT AS F
  	ON F.file_id = A.file_id
  INNER JOIN (
  	SELECT 
  		model,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
  	GROUP BY model
  ) W ON W.model = F.model
  GROUP BY F.model, W.word_count
  ORDER BY F.Model
"
adv_by_model <- dbGetQuery(conn, query)

## Adverbials by LLM type =================================================
query = "
  SELECT A.LLM_type, 
  COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
  COUNT(A.marker_id) AS adverbials_count, W.word_count, 
  (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 AS  adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN (
  	SELECT 
  		LLM_type,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
	WHERE LEN(ISNULL(LLM_type, '')) > 0
  	GROUP BY LLM_type
  ) W ON W.LLM_type = A.LLM_type
  GROUP BY A.LLM_type, W.word_count"

adv_by_LLM_TYPE <- dbGetQuery(conn, query)


## Constructions by category ==============================================
query = "
  SELECT F.category, 
  A.construction_type, 
  COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
  COUNT(A.marker_id) AS adverbials_count, W.word_count, 
  (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 AS  adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN FILES_ARGUMENT AS F
  	ON F.file_id = A.file_id
  INNER JOIN (
  	SELECT 
  		category,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
  	GROUP BY category
  ) AS W ON F.category = W.category
  GROUP BY F.category, A.construction_type, W.word_count
"
constr_by_category <- dbGetQuery(conn, query)

## Constructions by model ==============================================
query = "
  SELECT F.model, A.construction_type, 
  COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, COUNT(A.marker_id) AS adverbials_count, W.word_count, 
  (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN FILES_ARGUMENT AS F
  	ON F.file_id = A.file_id
  INNER JOIN (
  	SELECT 
  		model,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
  	GROUP BY model
  ) AS W ON F.model = W.model
  GROUP BY F.model, A.construction_type, W.word_count
"
constr_by_model <- dbGetQuery(conn, query)

## Constructions by file ==============================================
query = "
  SELECT F.file_id, F.model, M.construction_type, 
  (COUNT(A.marker_id) / CAST(F.word_count AS FLOAT)) * 1000 construction_frequency
  FROM FILES_ARGUMENT AS F
  CROSS JOIN  (
  	SELECT DISTINCT construction_type 
  	FROM MARKERS AS M
  	WHERE M.theory = 'Biber'
  ) AS M	
  LEFT JOIN LINKING_ADVERBIALS AS A
  ON A.file_id = F.file_id
  AND A.construction_type = M.construction_type 
  GROUP BY F.file_id, F.model, M.construction_type, F.word_count
"

constr_by_file <- dbGetQuery(conn, query)

constr_by_file$author  <- ifelse(constr_by_file$model == "Student", "Student", ifelse(constr_by_file$model  %in% closed, "Closed", "Open"))

## Constructions by LLM Type ==============================================
query = "
  SELECT A.LLM_type, A.construction_type, COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
  COUNT(A.marker_id) AS adverbials_count, W.word_count, 
  (COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 AS  adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN (
  	SELECT 
  		LLM_type,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT]
	WHERE LEN(ISNULL(LLM_type, '')) > 0
  	GROUP BY LLM_type
  ) W ON W.LLM_type = A.LLM_type
  GROUP BY A.LLM_type, W.word_count, A.construction_type
"
constr_by_LLM_type <- dbGetQuery(conn, query)

## Constructions by Topic Type ==============================================
query = "
 SELECT A.topic_type, A.construction_type, 
	COUNT(DISTINCT A.marker_id) AS distinct_adverbials_count, 
	COUNT(A.marker_id) AS adverbials_count, W.word_count, 
	(COUNT(A.marker_id) / CAST(W.word_count AS FLOAT)) * 1000 AS  adverbials_frequency
  FROM LINKING_ADVERBIALS AS A
  INNER JOIN (
  	SELECT 
		F.category,
  		T.type AS topic_type,
  		SUM(word_count) as word_count
  	FROM [dbo].[FILES_ARGUMENT] AS F
	INNER JOIN [dbo].[TOPIC_ARGUMENT] AS T
		ON T.topic = F.topic
	WHERE F.category = 'LLM'
  	GROUP BY F.category, T.type
  ) W 
  ON W.topic_type = A.topic_type
  AND W.category = A.category
  GROUP BY A.topic_type, W.word_count, A.construction_type
"
constr_by_topic_type <- dbGetQuery(conn, query)

# Essay statistics -----------------------


## Category =====================================================================

### Essay length ############################# 
essays |> 
  summarise(
    mean_w = mean(word_count), 
    sd_w = sd(word_count), 
    n = n(), 
    .by = category
  ) |> 
  arrange(category)

plt <- ggplot(essays, aes(category, word_count, fill = category))

# Box plot of essay length by category
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of words", 
                title = "Essay Length by Author", 
                palette = palette_2_col, 
                save_as = "essay_length_category_boxplot.svg")


# Bar plot of essay length by category
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average number of words", 
                title = "Average Essay Length by Author", 
                palette = palette_2_col, 
                save_as = "essay_length_category_barplot.svg") 


### Number of sentences #############################
plt <- ggplot(essays, aes(category, sentence_count, fill = category))

# Box plot
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of sentences", 
                title = "Essay Number of Sentences by Author", 
                palette = palette_2_col, 
                save_as = "sentence_count_category_boxplot.svg")

# Bar plot
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of sentences", 
                title = "Average Essay Number of Sentences by Author", 
                palette = palette_2_col, 
                save_as = "sentence_count_category_barplot.svg") 

### Words per sentence #############################
plt <- ggplot(essays, aes(category, word_count / sentence_count, fill = category)) 

create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of words", 
                title = "Essay Number of Words per Sentence by Author", 
                palette = palette_2_col, 
                save_as = "words_per_sentece_category_boxplot.svg")

# Bar plot
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average Words per Sentence", 
                title = "Average Number of Words per Sentence", 
                palette = palette_2_col, 
                save_as = "words_per_sentence_category_barplot.svg") 

### ttr #############################
plt <- ggplot(ttr, aes(model, ttr, fill = model)) 

# Bar plot
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Model", ylab = "Average TTR", 
                     title = "Average Type/Token Ratio by Model", 
                     palette = palette_llm, 
                     save_as = "avg_ttr_model.svg") 


## Model =====================================================================

### Essay length ############################# 
essays |> 
  summarise(
    mean_w = mean(word_count), 
    sd_w = sd(word_count), 
    n = n(), 
    .by = model
  ) |> 
  arrange(model)

plt <- ggplot(essays, aes(model, word_count, fill = model))

# Box plot of essay length by model
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of words", 
                title = "Essay Length by Author", 
                palette = palette_llm, 
                save_as = "essay_length_model_boxplot.svg")

# Bar plot of essay length by model
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average number of words", 
                title = "Average Essay Length by Author", 
                palette = palette_llm, 
                save_as = "essay_length_model_barplot.svg") 

### Number of sentences #############################
plt <- ggplot(essays, aes(model, sentence_count, fill = model))

# Box plot
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of sentences", 
                title = "Essay Number of Sentences by Author", 
                palette = palette_llm, 
                save_as = "sentence_count_model_boxplot.svg")

# Bar plot
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of sentences", 
                title = "Average Essay Number of Sentences by Author", 
                palette = palette_llm, 
                save_as = "sentence_count_model_barplot.svg") 

### Words per sentence #############################
plt <- ggplot(essays, aes(model, word_count / sentence_count, fill = model)) 

create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of words", 
                title = "Essay Number of Words per Sentence by Author", 
                palette = palette_llm, 
                save_as = "words_per_sentece_model_boxplot.svg")

# Bar plot
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average words", 
                title = "Average Number of Words per Sentence", 
                palette = palette_llm, 
                save_as = "words_per_sentence_model_barplot.svg") 


# Adverbials statistics --------------------------------------------------------


## Category ====================================================================

### Number of adverbials ############################# 
plt <- ggplot(adv_by_category, aes(category, adverbials_count, fill = category, label=adverbials_count))

# Bar plot of number of adverbials by category
p1 = create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Number of adverbials", 
                #title = "Number of Adverbials by Author", 
                title = "", 
                palette = palette_2_col, 
                save_as = "adverbials_count_category_barplot.svg") 
p1 <- p1 + geom_text(size = 5, position = position_stack(vjust = 0.5))
p1

### Frequency of adverbials ############################# 
# Bar plot of frequency of adverbials by category
plt <- ggplot(adv_by_category, aes(category, adverbials_frequency, fill = category, label = round(adverbials_frequency, digits = 2)))
p2 = create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Frequency of adverbials", 
                #title = "Frequency of Adverbials by Author",
                title = "",
                palette = palette_2_col, 
                save_as = "adverbials_freq_category_barplot.svg") 

p2 <- p2 + geom_text(size = 5, position = position_stack(vjust = 0.5))
p2
p3 <- p1 + p2 + 
  plot_layout(ncol = 2) +
  plot_annotation(title = "Adverbials by Author",
  theme = theme(plot.title = element_text(size = 28, hjust  = 0.5 )))

ggsave("adverbials_freq_category.svg", plot=p3, device='svg', width=10.667, height = 8)

p3



## Model ====================================================================

### Number of adverbials ############################# 
plt <- ggplot(adv_by_model[adv_by_model$model != "Student", ], aes(model, adverbials_count, fill = model))

# Bar plot of number of adverbials by category
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "LLM", ylab = "Number of adverbials", 
                title = "Number of Adverbials by LLM", 
                palette = palette_llm, 
                save_as = "adverbials_count_model_barplot.svg") 

plt <- ggplot(adv_by_model[adv_by_model$model != "Student", ], aes(reorder(model, distinct_adverbials_count), distinct_adverbials_count, fill = model))
# Bar plot of number of distinct adverbials by category
p = create_bar_plot_mean(plot = plt, 
                     theme = theme_custom,
                     xlab = "LLM", ylab = "Number of different adverbials", 
                     title = "Number of Different Adverbials by LLM", 
                     palette = palette_llm, 
                     save_as = "adverbials_count_distinct_model_barplot.svg")

p <- p + coord_flip()
ggsave("adverbials_count_distinct_model_barplot.svg", plot=p, device='svg', width=10.667, height = 8)

### Frequency of adverbials ############################# 
# Bar plot of frequency of adverbials by model
plt <- ggplot(adv_by_model, aes(model, adverbials_frequency, fill = model))
create_bar_plot_mean(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Frequency of adverbials", 
                title = "Frequency of Adverbials by Author", 
                #title = "", 
                palette = palette_llm, 
                save_as = "adverbials_freq_model_barplot.svg")

# Constructions statistics --------------------------------------------------------


## Category ====================================================================

### Number of adverbials #############################
plt <- ggplot(constr_by_category, aes(category, adverbials_count, fill = construction_type))
# Bar plot of number of adverbials by category
p1 <- create_bar_plot_identity(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Author", ylab = "Number of adverbials", 
                     labs_fill = "Category",
                     #title = "Number of Adverbials for each Category", 
                     title = "", 
                     palette = palette_adv,
                     save_as = "adverbials_count_constr_category_barplot.svg")

### Frequency of adverbials ############################# 
# Bar plot of frequency of adverbials by category
plt <- ggplot(constr_by_category, aes(category, adverbials_frequency, fill = construction_type))
p2 <- create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Author", ylab = "Frequency of adverbials", 
                         labs_fill = "Category",
                         #title = "Frequency of Adverbials for each Category", 
                         title = "",
                         palette = palette_adv,
                         save_as = "adverbials_freq_constr_category_barplot.svg")

p3 <- p1 + p2 + 
  plot_annotation(title = "Adverbial Categories by Author",
                  theme = theme(plot.title = element_text(size = 28, hjust  = 0.5 ))) +
  plot_layout(ncol = 2,guides = "collect") & theme(legend.position = "bottom")
  
ggsave("adverbials_freq_constr_category.svg", plot=p3, device='svg', width=10.667, height = 8)

plt <- ggplot(constr_by_category, aes(category, adverbials_frequency, fill = construction_type))
p <- create_bar_plot_dodged_identity(plot = plt, 
                               theme = theme_custom, 
                               xlab = "Author", ylab = "Frequency", 
                               labs_fill = "",
                               title = "Frequency of Adverbials for each Category", 
                               #title = "",
                               palette = palette_adv,
                               save_as = "adverbials_freq_constr_category_barplot_dodged.svg")
p <- p + facet_wrap("construction_type") + theme(legend.position="none") #+ scale_x_discrete(limits = c("Student", "LLM"))

ggsave("adverbials_freq_constr_category_barplot_faceted.svg", plot=p, device='svg', width=10.667, height = 8)

## Model ====================================================================

### Number of adverbials ############################# 
plt <- ggplot(constr_by_model[constr_by_model$model != "Student", ], aes(model, adverbials_count, fill = construction_type))
# Bar plot of number of adverbials by category
create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Model", ylab = "Number of Adverbials", 
                         labs_fill = "Category",
                         title = "Number of Adverbials for each Category", 
                         palette = palette_adv,
                         save_as = "adverbials_count_constr_model_barplot.svg")

### Frequency of adverbials ############################# 
# Bar plot of frequency of adverbials by category
plt <- ggplot(constr_by_model, aes(model, adverbials_frequency, fill = construction_type))
create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Author", ylab = "Frequency of adverbials", 
                         labs_fill = "Category",
                         title = "Frequency of Adverbials for each Category", 
                         palette = palette_adv,
                         save_as = "adverbials_freq_constr_model_barplot.svg")
## Open vs closed LLM  ----------------------------------------

### Number of adverbials ############################# 
plt <- ggplot(adv_by_LLM_TYPE, aes(LLM_type, adverbials_count, fill = LLM_type))

# Bar plot of number of adverbials by LLM type
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "LLM Type", ylab = "Number of adverbials", 
                     title = "Number of Adverbials by LLM Type", 
                     palette = palette_open_closed, 
                     save_as = "adverbials_count_LLMtype_barplot.svg") 


# Bar plot of frequency of adverbials by LLM type
plt <- ggplot(adv_by_LLM_TYPE, aes(LLM_type, adverbials_frequency, fill = LLM_type))
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "LLM Type", ylab = "Frequency of adverbials", 
                     title = "Frequency of Adverbials by LLM Type", 
                     palette = palette_open_closed, 
                     save_as = "adverbials_freq_LLMtype_barplot.svg") 


plt <- ggplot(constr_by_LLM_type, aes(LLM_type, adverbials_count, fill = construction_type))

### Frequency of adverbials #############################
# Bar plot of number adverbials by category
create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Author", ylab = "Number of adverbials", 
                         labs_fill = "Category",
                         title = "Number of Adverbials for each Category", 
                         palette = palette_adv,
                         save_as = "adverbials_count_constr_LLMtype_barplot.svg")


# Bar plot of frequency of adverbials by category
plt <- ggplot(constr_by_LLM_type, aes(LLM_type, adverbials_frequency, fill = construction_type))
create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Author", ylab = "Frequency", 
                         labs_fill = "Category",
                         title = "Frequency of Adverbials for each Category", 
                         palette = palette_adv,
                         save_as = "adverbials_freq_constr_LLMtype_barplot.svg")



# Bar plot of frequency of adverbials by topic type
plt <- ggplot(constr_by_topic_type, aes(topic_type, adverbials_frequency, fill = construction_type))
create_bar_plot_identity(plot = plt, 
                         theme = theme_custom, 
                         xlab = "Topic type", ylab = "Frequency of adverbials", 
                         labs_fill = "Category",
                         title = "Frequency of Adverbials for each Category", 
                         palette = palette_adv,
                         save_as = "adverbials_freq_constr_topic_type_barplot.svg")




# Use of adverbials ------------------------------------------------------------

## Consistency in the use of adverbials by model ===============================

### Number of adverbials #############################
plt <- ggplot(adv_by_file, aes(model, adverbials_count, fill = model))
# Bar plot
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Model", ylab = "Number of adverbials", 
                     title = "Average Number of Adverbials", 
                     palette = palette_llm, 
                     save_as = "adverbials_count_file_model_barplot.svg") 
### Box plot  #############################
create_box_plot(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Model", ylab = "Number of adverbials", 
                     title = "Average Number of Adverbials", 
                     palette = palette_llm, 
                     save_as = "adverbials_count_file_model_boxplot.svg") 

### Frequency of adverbials #############################
plt <- ggplot(adv_by_file, aes(model, adverbials_frequency, fill = model))
# Bar plot
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Model", ylab = "Average Frequency", 
                     title = "Average Frequency of Adverbials", 
                     palette = palette_llm, 
                     save_as = "adverbials_freq_file_model_barplot.svg") 
# Box plot
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average Frequency", 
                title = "Average Frequency of Adverbials", 
                palette = palette_llm, 
                save_as = "adverbials_freq_file_model_boxplot.svg") 

### Frequency of adverbials by author #############################
plt <- ggplot(adv_by_file, aes(author, adverbials_frequency, fill = author))
# Bar plot
create_bar_plot_mean(plot = plt, 
                     theme = theme_custom, 
                     xlab = "Author", ylab = "Frequency", 
                     title = "Average Frequency", 
                     palette = palette_2_col, 
                     save_as = "adverbials_freq_file_llmstu_barplot.svg") 
# Box plot
create_box_plot(plot = plt, 
                theme = theme_custom, 
                xlab = "Author", ylab = "Average Frequency", 
                title = "Average Frequency of Adverbials", 
                palette = palette_2_col, 
                save_as = "adverbials_freq_file_llmstu_boxplot.svg") 

## Most frequent adverbials by category ===============================
## Most frequent marker by category ===================================

constructions <- c('Apposition', 'Contrast/Concession', 'Enumeration and Addition', 'Result/Inference', 'Summation', 'Transition')


# using a for loop

for (constr in constructions){
  
  fill_color = palette_adv[match(constr, constructions)]
  file_name = paste('most_freq_adv_', gsub("/","", constr), '.svg', sep = '')
  print(file_name)
  
  df1 <- adverbials[adverbials$construction_type == constr  & adverbials$category == 'LLM',]  %>%
    group_by(category, marker) %>%
    summarise(count = n())
  
  df2 <- adverbials[adverbials$construction_type == constr  & adverbials$category == 'Student',]  %>%
    group_by(category, marker) %>%
    summarise(count = n()) 
  
  max_count = max(df1$count, df2$count)
  
  len1 = length(df1$marker)
  len2 = length(df2$marker)
  
  if (len1 > len2){
    y = len1 - len2
    for (x in 1 : y) {
      dummy <- paste(replicate(x, " "), collapse = "")
      df2[nrow(df2) + 1, ] <- list('Student', dummy, 0)
    }
    adv_count= len1
    
  }  else {
    y = len2 - len1
    for (x in 1 : y) {
      dummy <- paste(replicate(x, " "), collapse = "")
      df1[ nrow(df1) + 1, ] <- list('LLM', dummy, 0)
    }
    adv_count = len2
    
  }
  
  adv_count <- min(adv_count, 15)
  
  df1 <- df1 %>%
    top_n(n = adv_count, wt = count)  %>% arrange(category, count)
  
  df2 <- df2 %>%
    top_n(n = adv_count, wt = count)  %>% arrange(category, count)
  
  plt <- ggplot(df1, aes(reorder(marker, count), count, fill = category)) 
  p1 <- plt +
    theme_custom +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(x = "", y = "Occurences") +
    coord_flip(ylim = c(0, max_count)) +
    scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
    ggtitle('LLM') +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), 
          legend.position = "none")
  
  plt <- ggplot(df2, aes(reorder(marker, count), count, fill = category)) 
  p2 <- plt +
    theme_custom +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(x = "", y = "Occurences") +
    coord_flip(ylim = c(0, max_count)) +
    scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
    ggtitle('Student') +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), 
          legend.position = "none") 

  # patchwork
  p3 <- p1 + p2 + 
    plot_layout(ncol = 2, heights = c(length(df1$marker), length(df2$marker))) +
    plot_annotation(title = constr,
                    theme = theme(plot.title = element_text(size = 28, hjust  = 0.5 ))) 
  
  ggsave(file_name, plot=p3, device='svg', width=10.667, height = 2 +  (0.8 * adv_count))
    
}

for (constr in constructions){
  
  fill_color = palette_adv[match(constr, constructions)]
  file_name = paste('freq_adv_', gsub("/","", constr), '.svg', sep = '')
  print(file_name)
  
  plt <- ggplot(adverbials_freq[adverbials_freq$construction_type == constr,], aes(marker, adverbials_frequency, fill = category))
  p <- create_bar_plot_dodged_identity(plot = plt, 
                                       theme = theme_custom, 
                                       xlab = "", ylab = "Frequency", 
                                       labs_fill = "Author",
                                       title = "Frequency of Adverbials for each Category", 
                                       #title = "",
                                       palette = palette_2_col,
                                       save_as = "adverbials_freq_constr_category_barplot_dodged.svg")
  p <- p + facet_wrap("construction_type") + theme(legend.position="bottom",axis.text.x=element_text(angle=45,hjust=1)) #+ scale_x_discrete(limits = c("Student", "LLM"))
  
  ggsave(file_name, plot=p, device='svg', width=10.667, height = 8)
}


## Reuse of adverbials by model ===============================

for (constr in constructions){
  

  df <- adverbials[adverbials$construction_type == constr  & adverbials$category == 'LLM',]  %>%
    group_by(model, category, marker) %>%
    summarise(count = n())  
  
  df1 <- df %>%
    group_by(marker) %>%
    summarise(count = sum(count)) %>% arrange(count) %>%
    top_n(n = 10, wt = count) %>% pull(marker) 
  
  adv_count = length(df1) 
  
  df <- df[df$marker %in% df1, ]
  
  plt <- ggplot(df, aes(fct_relevel(df$marker, as.character(df1)), count, fill = model, label = count))
  
  p <- plt +
    theme_custom +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(size = 5, position = position_stack(reverse = TRUE, vjust = 0.5)) +
    labs(x = "", y = "Occurences", fill = "Model") +
    coord_flip() +
    scale_fill_manual(values = palette_llm) +
    ggtitle(paste('Reuse of', constr,  'Adverbials')) +
    theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), 
          legend.position = "none") 
  
  p
  file_name = paste('reuse_by_model_', gsub("/","", constr), '.svg', sep = '')
  ggsave(file_name, plot=p, device='svg', width=8, height = 3 + (0.5 * adv_count))

  }

reuses <- c('in conclusion', 'as a result', 'thus', 'consequently', 'firstly', 'secondly', 'finally', 
            'lastly', 'in contrast', 'on the other hand', 'specifically')
reuses <- c('firstly', 'secondly', 'finally','lastly')

df <- adverbials[adverbials$marker %in% reuses & adverbials$category == 'LLM',]  %>%
  group_by(model, category, marker) %>%
  summarise(count = n())  


df1 <- df %>%
  group_by(marker) %>%
  summarise(count = sum(count)) %>% arrange(count) %>%
  top_n(n = 10, wt = count) %>% pull(marker) 

adv_count = length(df1)

plt <- ggplot(df, aes(fct_relevel(df$marker, as.character(df1)), count, fill = model, label = count))

p <- plt +
  theme_custom +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(size = 5, position = position_stack(reverse = TRUE, vjust = 0.5)) +
  labs(x = "", y = "Occurences", fill = "Model") +
  coord_flip() +
  scale_fill_manual(values = llm_colors) +
  ggtitle(paste('Pattern of use of adverbials')) +
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") 

p
file_name = paste('reuse_by_model.svg', sep = '')
ggsave(file_name, plot=p, device='svg', width= 10.667, height = 3 + (0.5 * adv_count))

# Statistically different constructions ----------------------------------------

## General ====================================================
adv_llm <- adv_by_file[adv_by_file$model != "Student", ]
adv_stu <- adv_by_file[adv_by_file$model == "Student", ]

basic_statistics(adv_llm$adverbials_frequency, "Freq of Adverbials", "LLM")
basic_statistics(adv_stu$adverbials_frequency, "Freq of Adverbials", "Student")

boxplot(adv_llm$adverbials_frequency, adv_stu$adverbials_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Adverbials",
        xlab = "Author", ylab = "Frequency")

t.test(adv_llm$adverbials_frequency, adv_stu$adverbials_frequency)

wilcox.test(adv_llm$adverbials_frequency, adv_stu$adverbials_frequency)

kruskal.test(adverbials_frequency ~ model, data = adv_by_file)
dunn.test(adv_by_file$adverbials_frequency, adv_by_file$model, method = "Bonferroni")

## Open/Closed/Student ====================================================

kruskal.test(adverbials_frequency ~ author_type, data = adv_by_file)
dunn.test(adv_by_file$adverbials_frequency, adv_by_file$author_type, method = "Bonferroni")

kruskal.test(construction_frequency ~ author, data = constr_by_file)
dunn.test(constr_by_file$construction_frequency, constr_by_file$author, method = "Bonferroni")

## LLM vs Student --------------------------------------------------------------
adv_llm <- constr_by_file[constr_by_file$model != "Student", ]
adv_stu <- constr_by_file[constr_by_file$model == "Student", ]

basic_statistics(adv_llm$construction_frequency, "Freq of Adverbials", "LLM")
basic_statistics(adv_stu$construction_frequency, "Freq of Adverbials", "Student")

boxplot(adv_llm$construction_frequency, adv_stu$construction_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Adverbials",
        xlab = "Author", ylab = "Frequency")

t.test(adv_llm$construction_frequency, adv_stu$construction_frequency)

wilcox.test(adv_llm$construction_frequency, adv_stu$construction_frequency)

kruskal.test(construction_frequency ~ model, data = constr_by_file)
dunn.test(constr_by_file$construction_frequency, constr_by_file$model, method = "Bonferroni")

## ChatGPT vs Gemma ====================================================
adv_chatg <- constr_by_file[constr_by_file$model == "ChatGPT", ]
adv_gemma <- constr_by_file[constr_by_file$model == "Gemma", ]

basic_statistics(adv_chatg$construction_frequency, "Freq of Adverbials", "ChatGPT")
basic_statistics(adv_gemma$construction_frequency, "Freq of Adverbials", "Gemma")

boxplot(adv_chatg$construction_frequency, adv_gemma$construction_frequency, 
        names = c("ChatGPT", "Gemma"), main = "Box plots of average frequency Adverbials",
        xlab = "Author", ylab = "Frequency")

t.test(adv_chatg$construction_frequency, adv_gemma$construction_frequency)

## Apposition ====================================================
appos_llm <- constr_by_file[constr_by_file$model != "Student" & constr_by_file$construction_type == "Apposition", ]
appos_stu <- constr_by_file[constr_by_file$model == "Student" & constr_by_file$construction_type == "Apposition", ]

basic_statistics(appos_llm$construction_frequency, "Freq of Apposition", "LLM")
basic_statistics(appos_stu$construction_frequency, "Freq of Apposition", "Student")

boxplot(appos_llm$construction_frequency, appos_stu$construction_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Apposition",
        xlab = "Author", ylab = "Frequency")

t.test(appos_llm$construction_frequency, appos_stu$construction_frequency) 

appos <- constr_by_file[constr_by_file$construction_type == "Apposition", ]

kruskal.test(construction_frequency ~ model, data = appos)

print("Apposition")
dunn.test(appos$construction_frequency, appos$model, method = "Bonferroni")

## Enumeration and Addition ====================================================
enum_llm <- constr_by_file[constr_by_file$model != "Student" & constr_by_file$construction_type == "Enumeration and Addition", ]
enum_stu <- constr_by_file[constr_by_file$model == "Student" & constr_by_file$construction_type == "Enumeration and Addition", ]

basic_statistics(enum_llm$construction_frequency, "Freq of Enum and Addition", "LLM")
basic_statistics(enum_stu$construction_frequency, "Freq of Enum and Addition", "Student")

boxplot(enum_llm$construction_frequency, enum_stu$construction_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Enumeration and Addition",
        xlab = "Author", ylab = "Frequency")

t.test(enum_llm$construction_frequency, enum_stu$construction_frequency) 

enum <- constr_by_file[constr_by_file$construction_type == "Enumeration and Addition", ]

kruskal.test(construction_frequency ~ model, data = enum)

print("Enumeration and Addition")
dunn.test(enum$construction_frequency, enum$model, method = "Bonferroni")

## Result/Inference ====================================================
inf_llm <- constr_by_file[constr_by_file$model != "Student" & constr_by_file$construction_type == "Result/Inference", ]
inf_stu <- constr_by_file[constr_by_file$model == "Student" & constr_by_file$construction_type == "Result/Inference", ]

basic_statistics(inf_llm$construction_frequency, "Freq of Result Inf", "LLM")
basic_statistics(inf_stu$construction_frequency, "Freq of Result Inf", "Student")

boxplot(inf_llm$construction_frequency, inf_stu$construction_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Result/Inference",
        xlab = "Author", ylab = "Frequency")


t.test(inf_llm$construction_frequency, inf_stu$construction_frequency)
wilcox.test(inf_llm$construction_frequency, inf_stu$construction_frequency)

cohensD(inf_llm$construction_frequency, inf_stu$construction_frequency)

t2 <- (-5.5496)^2
df <- 107.47
err_size <- sqrt(t2 / (t2 + df))
err_size

inf <- constr_by_file[constr_by_file$construction_type == "Result/Inference", ]

kruskal.test(construction_frequency ~ model, data = inf)

print("Result/Inference")
dunn.test(inf$construction_frequency, inf$model, method = "bonferroni")

dunn.test(inf$construction_frequency, inf$model)

help(p.adjust.methods)



## Summation ====================================================
sum_llm <- constr_by_file[constr_by_file$model != "Student" & constr_by_file$construction_type == "Summation", ]
sum_stu <- constr_by_file[constr_by_file$model == "Student" & constr_by_file$construction_type == "Summation", ]

basic_statistics(inf_llm$construction_frequency, "Freq of Summation", "LLM")
basic_statistics(inf_stu$construction_frequency, "Freq of Summation", "Student")


boxplot(sum_llm$construction_frequency, sum_stu$construction_frequency, 
        names = c("LLM", "Student"), main = "Box plots of average frequency Summation",
        xlab = "Author", ylab = "Frequency")

t.test(sum_llm$construction_frequency, sum_stu$construction_frequency) 

sum <- constr_by_file[constr_by_file$construction_type == "Summation", ]

kruskal.test(construction_frequency ~ model, data = sum)

print("Summation")
dunn.test(sum$construction_frequency, sum$model, method = "Bonferroni")




# Adverbials positions



query = "
  SELECT m.marker, a.start_pos
  FROM MATCHES_ARGUMENT AS A
  INNER JOIN MARKERS AS M
  	ON M.id = A.marker_id 
  INNER JOIN FILES_ARGUMENT AS F
  	ON A.file_id = F.file_id
  WHERE model != 'Student'
  AND marker IN ('firstly', 'first', 'to begin with', 'futhermore', 
    'conversely', 'additionally','secondly','second','third','thirdly','in conclusion','finally',
    'on the other hand', 'consequently','lastly')

"

query = "
  SELECT m.marker, a.start_pos
  FROM MATCHES_ARGUMENT AS A
  INNER JOIN MARKERS AS M
  	ON M.id = A.marker_id 
  INNER JOIN FILES_ARGUMENT AS F
  	ON A.file_id = F.file_id
  WHERE model != 'Student'
  AND marker IN ('firstly', 'first', 'to begin with', 'futhermore', 
    'conversely', 'secondly','second','third','thirdly','in conclusion','finally',
    'consequently','lastly')

"

query = "
  SELECT m.marker, a.start_pos
  FROM MATCHES_ARGUMENT AS A
  INNER JOIN MARKERS AS M
  	ON M.id = A.marker_id 
  INNER JOIN FILES_ARGUMENT AS F
  	ON A.file_id = F.file_id
  WHERE model != 'Student'
  AND marker IN ('firstly', 'first', 'secondly','second','third','thirdly','in conclusion','finally', 'lastly')

"

distribution <- dbGetQuery(conn, query)

distribution <- distribution %>%
  mutate(marker = recode(marker, 
                         'first' = 'first-firstly', 'firstly' = 'first-firstly', 
                         'second' = 'second-secondly', 'secondly' = 'second-secondly',
                         'third' = 'third-thirdly', 'thirdly' = 'third-thirdly',
                         'finally' = 'finally-lastly', 'lastly' = 'finally-lastly')
         )



#distribution %>%  mutate(marker = fct_reorder(marker, start_pos, .fun = min))

#distribution$marker <- factor(distribution$marker, levels = distribution$marker)

adv_sort <- distribution %>% 
  group_by(marker) %>% 
  summarize(sort = min(start_pos)) %>% 
  arrange(sort)

adv_sorted <- adv_sort[['marker']]
adv_sorted


adv_colors <- c("first" = '#58EFECff', 
                "firstly" = '#66E0E3ff',
                "second" = '#75D2DAff',
                "to begin with" = '#83C3D0ff',
                "conversely"  = '#92B4C7ff',
                "secondly" = '#A0A6BEff',
                "thirdly" = '#AE97B5ff',
                "lastly" ='#BD88ACff',
                "finally" = '#CB79A2ff',
                "in conclusion" = '#DA6B99ff',
                "consequently" = '#E85C90ff')

adv_colors <- c("first-firstly" = 'yellow', 
                "second-secondly" = 'orange',
                "third-thirdly" = 'red',
                "to begin with" = '#83C3D0ff',
                "conversely"  = 'steelblue',
                "lastly" ='#BD88ACff',
                "finally" = '#CB79A2ff',
                "in conclusion" = '#DA6B99ff',
                "consequently" = 'blue')

distribution$"Linking adverbial"  <- distribution$"marker"

adv_colors <- c(brewer.pal(n = (length(adv_sorted)), "Set2"))

graph <- ggplot(distribution, aes(x = start_pos, color = `Linking adverbial`))

graph <- graph +  
  theme_custom +
  geom_density(size=1) +
  scale_color_manual(values = adv_colors, breaks = adv_sorted) +
  labs(x = "Position", y = "Density") +
  ggtitle("Positioning of adverbials by LLMs\n") +
  theme(plot.title.position = 'plot', plot.title = element_text(hjust = 0.5), 
        legend.position = "right")
graph
ggsave('positioning.svg', plot=graph, device='svg', width=10.667, height = 8)
