


db1_cor_fun <- function(db, sex, race) {
  diseases <- colnames(db)[c(8,9,12:23)]
  rename_diseases <- c("All Causes", "Natural Causes", "Septicemia", "Maligant neoplasms", "Diabetes mellitus", "Alzheimer disease",
                       "Influenza and pneumonia", "Chronic lower respiratory", "Respiratory system", "Nephritis", "Symtoms signs", "Diseases of heart",
                       "Cerebrovascular", "COVID-19")
  
  db <- db %>% filter(sex %in% sex)
  db <- db[, diseases]
  
  colnames(db) <- rename_diseases
  correlation <- cor(db)
  corrplot(correlation)
  
}
