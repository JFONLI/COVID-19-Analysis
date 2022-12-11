


db1_cor_fun <- function(db, sex_choices, race_choices, age_choices) {
  db <- db %>% filter(age_group %in% age_choices)
  diseases <- colnames(db)[c(8,9,12:23)]
  rename_diseases <- c("All Causes", "Natural Causes", "Septicemia", "Maligant neoplasms", "Diabetes mellitus", "Alzheimer disease",
                       "Influenza and pneumonia", "Chronic lower respiratory", "Respiratory system", "Nephritis", "Symtoms signs", "Diseases of heart",
                       "Cerebrovascular", "COVID-19")
  
  db <- db %>% filter(sex %in% sex_choices, race %in% race_choices)
  db <- db[, diseases]
  
  colnames(db) <- rename_diseases
  
  cor.mat = cor(db, method = "spearman")
  corrplot(cor.mat,  type='full',  method = 'color',
           col.lim = c(0.1,1),is.corr = FALSE,tl.col = 'black', diag = TRUE, 
           addCoef.col = 'black', col = COL2('RdBu', 10),  
           tl.cex = 0.8, cl.cex = 0.70, number.cex = 0.7, tl.srt= 390)#, 
}

