# libraries----
pacman::p_load(stm, tictoc)

# Data----
mystm <- import("Clean_Data/mystm.rda")

# topic modeling----
set.seed(7)
tic()
topic_16 <- stm(mystm$documents, 
                mystm$vocab, 
                K = 16, 
                prevalence = ~ factor(year) + category,
                data = mystm$meta)
toc()

save(topic_16, file = "Clean_Data/topic_16.rda")
