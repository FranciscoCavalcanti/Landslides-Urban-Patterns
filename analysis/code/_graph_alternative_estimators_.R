library(dplyr)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)


path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

dados <- readRDS(paste0(path_output,"database_panel.rds"))
psm <- readRDS(paste0(path_output,"restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

dados2 <- merge(dados, psm[,c("code","weights")], by="code")

#### Alternative Estimators ####
ggplot_paper <- function(x, estimator){
  
  print(x)
  print(estimator)
  
  ## No Paired results
  mw.dyn_np  <- event_study(
    data=dados,
    yname=x,
    idname="code",
    gname="first_year_landslide",
    tname="year",
    weights = NULL,
    estimator = estimator)
  
  ## Avg effect
  mw.dyn_np <- mw.dyn_np %>% 
    rename(overall.att = estimate,
           overall.se = std.error,
           event.time = term) %>% 
    mutate(conf.low  = overall.att - (1.96 * overall.se),
           conf.high = overall.att + (1.96 * overall.se)) %>% 
    select(c(event.time, overall.att,overall.se,conf.low,conf.high)) %>% 
    mutate(est = 'Broader Sample')

  ## Paired results  
  mw.dyn_p  <- event_study(
    data=dados2,
    yname=x,
    idname="code",
    gname="first_year_landslide",
    tname="year",
    weights = NULL,
    estimator = estimator)

  ## Avg effect
  mw.dyn_p <-  mw.dyn_p %>% 
    rename(overall.att = estimate,
           overall.se = std.error,
           event.time = term) %>% 
    mutate(conf.low  = overall.att - (1.96 * overall.se),
           conf.high = overall.att + (1.96 * overall.se)) %>% 
    select(c(event.time, overall.att,overall.se,conf.low,conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  
  # Graph
  est <- bind_rows(mw.dyn_p,mw.dyn_np)
  
  title <- ifelse(grepl('urban_size',x),"A) Effect of Landslides on Urban Size",
           ifelse(grepl('sprawl',x),"B) Effect of Landslides on Sprawl Index",
           ifelse(grepl('water_size',x),"A) Effect of Landslides on Water Surface",
           ifelse(grepl('forest_size',x),"B) Effect of Landslides on Forest Formation",
           ifelse(grepl('natural_size',x),"C) Effect of Landslides on Other Natural Land Uses",NA)))))
  

  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.8,0.1)
  
  # Criando a tabela como um grob
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",round(mw.dyn_p$overall.se,4),")"))
  )
  
  
  # Gerar tabela
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 20)), # Tamanho da fonte para o conteúdo da tabela
                                                  colhead = list(fg_params = list(fontsize = 20, fontface = "bold")), # Tamanho da fonte e negrito para o cabeçalho
                                                  rowhead = list(fg_params = list(fontsize = 20)))) # Tamanho da fonte para os cabeçalhos das linhas, se houver
  
  
  
  graph <- ggplot(data=est, aes(y = event.time, x = overall.att, color = est, linetype = est)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 ,position = position_dodge(width=0.5), linetype = 'blank'
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 , width = 0.5, position = position_dodge(width=0.5)
    ) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=-1) +
    labs( x='Coefficient', y='Period',
          color = "",linetype = "",
          title = "") +
    scale_color_manual(name ="", values = c("black","grey20"),
                       labels = c('Broader Sample','Matched Sample')) +
    scale_linetype_manual(name ="", values = c("solid","dashed"),
                          labels = c('Broader Sample','Matched Sample')) + 
    scale_y_continuous(breaks=seq(-16,16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 20),
          legend.text = element_text(size = 18), # Ajusta o tamanho do texto da legenda
          legend.title = element_text(size = 18), # Ajusta o tamanho do título da legenda
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1, "cm"),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combinando o gráfico e a tabela
  graph <- graph + annotation_custom(grob=tabela_grob,
                                     xmin=table_pos_y,
                                     xmax=table_pos_y,
                                     ymin=-15, ymax=-11)

  return(graph)
  
}

# Using loop
output_lurban_size <- Map(x         = c('lurban_size'),
                          estimator = c('did','TWFE','did2s','impute'),
                          ggplot_paper)

output_sprawl_index <- Map(x         = c('sprawl_index.x'),
                          estimator = c('did','TWFE','did2s','impute'),
                          ggplot_paper)



# Callaway and Sant'Anna (2020)
# Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_callaway.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[1]], width = 20, height = 10, units = "in", dpi = 300)

# Sprawl_index
sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_callaway.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[1]], width = 20, height = 10, units = "in", dpi = 300)




# TWFE Model
# Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_twfe.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[2]], width = 20, height = 10, units = "in", dpi = 300)

# Sprawl_index
sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_twfe.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[2]], width = 20, height = 10, units = "in", dpi = 300)




# Gardner (2021)
# Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_gardner.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[3]], width = 20, height = 10, units = "in", dpi = 300)

# Sprawl_index
sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_gardner.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[3]], width = 20, height = 10, units = "in", dpi = 300)






# Borusyak, Jaravel, Spiess (2021)
# Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_borusyak.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[4]], width = 20, height = 10, units = "in", dpi = 300)

# Sprawl_index
sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_borusyak.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[4]], width = 20, height = 10, units = "in", dpi = 300)
