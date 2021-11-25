clean_my_data <- function(my_data) {
  
  my_data %>%
    rowwise() %>%
    mutate(anxdep = sum(c(cbcl14, cbcl29, cbcl30, cbcl31, cbcl32, cbcl33, 
                          cbcl35, cbcl45, cbcl50, cbcl52, cbcl71, cbcl91, cbcl112))) %>%
    mutate(withdep = sum(c(cbcl5, cbcl42, cbcl65, cbcl69, cbcl75, 
                           cbcl102, cbcl103, cbcl111))) %>%
    mutate(soma = sum(c(cbcl47, cbcl49, cbcl51, cbcl54, cbcl56a, 
                        cbcl56b, cbcl56c, cbcl56d, cbcl56e, cbcl56f, 
                        cbcl56g))) %>%
    mutate(social = sum(c(cbcl11, cbcl12, cbcl25, cbcl27, cbcl34, 
                          cbcl36, cbcl38, cbcl48, cbcl62, cbcl64, 
                          cbcl79))) %>%
    mutate(thought = sum(c(cbcl9, cbcl18, cbcl40, cbcl46, cbcl58, 
                           cbcl59, cbcl60, cbcl66, cbcl70, cbcl76, 
                           cbcl83, cbcl84, cbcl85, cbcl92, cbcl100))) %>%
    mutate(attention = sum(c(cbcl1, cbcl4, cbcl8, cbcl10, cbcl13, 
                             cbcl17, cbcl41, cbcl61, cbcl78, cbcl80))) %>%
    mutate(rulebreak = sum(c(cbcl2, cbcl26, cbcl28, cbcl39, cbcl43, 
                             cbcl63, cbcl67, cbcl72, cbcl73, cbcl81, 
                             cbcl82, cbcl90, cbcl96, cbcl99, cbcl101, 
                             cbcl105, cbcl106))) %>%
    mutate(agg = sum(c(cbcl3, cbcl16, cbcl19, cbcl20, cbcl21, 
                       cbcl22, cbcl23, cbcl37, cbcl57, cbcl68, 
                       cbcl86, cbcl87, cbcl88, cbcl89, cbcl94, 
                       cbcl95, cbcl97, cbcl104))) %>%
    mutate(other = sum(c(cbcl6, cbcl7, cbcl15, cbcl24, cbcl44, 
                         cbcl53, cbcl55, cbcl56h, cbcl74, cbcl77, 
                         cbcl93, cbcl98, cbcl107, cbcl108, cbcl109, 
                         cbcl110, cbcl113))) %>%
    select(id, anxdep, withdep, soma, social, thought, attention, rulebreak, agg, other) %>% as.data.frame()
  
}


### other stuff

set.seed(12345)

### Sample Data

cbcl <- c("id", "cbcl1", 	"cbcl2", 	"cbcl3", 	"cbcl4", 	"cbcl5", 	"cbcl6", 	"cbcl7", 	"cbcl8", 	
          "cbcl9", 	"cbcl10", 	"cbcl11", 	"cbcl12", 	"cbcl13", 	"cbcl14", 	"cbcl15", 	
          "cbcl16", 	"cbcl17", 	"cbcl18", 	"cbcl19", 	"cbcl20", 	"cbcl21", 	"cbcl22", 	
          "cbcl23", 	"cbcl24", 	"cbcl25", 	"cbcl26", 	"cbcl27", 	"cbcl28", 	"cbcl29", 	
          "cbcl30", 	"cbcl31", 	"cbcl32", 	"cbcl33", 	"cbcl34", 	"cbcl35", 	"cbcl36", 	
          "cbcl37", 	"cbcl38", 	"cbcl39", 	"cbcl40", 	"cbcl41", 	"cbcl42", 	"cbcl43", 	
          "cbcl44", 	"cbcl45", 	"cbcl46", 	"cbcl47", 	"cbcl48", 	"cbcl49", 	"cbcl50", 	
          "cbcl51", 	"cbcl52", 	"cbcl53", 	"cbcl54", 	"cbcl55", 	"cbcl56a", 	"cbcl56b", 	
          "cbcl56c", 	"cbcl56d", 	"cbcl56e", 	"cbcl56f", 	"cbcl56g", 	"cbcl56h", 	"cbcl57", 	
          "cbcl58", 	"cbcl59", 	"cbcl60", 	"cbcl61", 	"cbcl62", 	"cbcl63", 	"cbcl64", 	
          "cbcl65", 	"cbcl66", 	"cbcl67", 	"cbcl68", 	"cbcl69", 	"cbcl70", 	"cbcl71", 	
          "cbcl72", 	"cbcl73", 	"cbcl74", 	"cbcl75", 	"cbcl76", 	"cbcl77", 	"cbcl78", 	
          "cbcl79", 	"cbcl80", 	"cbcl81", 	"cbcl82", 	"cbcl83", 	"cbcl84", 	"cbcl85", 	
          "cbcl86", 	"cbcl87", 	"cbcl88", 	"cbcl89", 	"cbcl90", 	"cbcl91", 	"cbcl92", 	
          "cbcl93", 	"cbcl94", 	"cbcl95", 	"cbcl96", 	"cbcl97", 	"cbcl98", 	"cbcl99", 	
          "cbcl100", 	"cbcl101", 	"cbcl102", 	"cbcl103", 	"cbcl104", 	"cbcl105", 	"cbcl106", 	
          "cbcl107", 	"cbcl108", 	"cbcl109", 	"cbcl110", 	"cbcl111", 	"cbcl112", 	"cbcl113")

rng <- data.frame(score = sample(x = c(1,2,3), size=120, replace=TRUE))
rng <- data.frame(t(rng))
rng$id <- "A"
rng <- rng[, c("id", sort(setdiff(names(rng), "id")))]
colnames(rng) <- cbcl
rownames(rng) <- NULL

rng2 <- data.frame(sample(x = c(1,2,3), size=120, replace=TRUE))
rng2 <- data.frame(t(rng2))
rng2$id <- "B"
rng2 <- rng2[, c("id", sort(setdiff(names(rng2), "id")))]
colnames(rng2) <- cbcl
rownames(rng2) <- NULL

sample_data <- rbind(rng, rng2)

rng3 <- data.frame(sample(x = c(1,2,3), size=120, replace=TRUE))
rng3 <- data.frame(t(rng3))
rng3$id <- "C"
rng3 <- rng3[, c("id", sort(setdiff(names(rng3), "id")))]
colnames(rng3) <- cbcl
rownames(rng3) <- NULL

sample <- rbind(sample_data, rng3)

### Template Data

cbcl <- c("id", "cbcl1", 	"cbcl2", 	"cbcl3", 	"cbcl4", 	"cbcl5", 	"cbcl6", 	"cbcl7", 	"cbcl8", 	
          "cbcl9", 	"cbcl10", 	"cbcl11", 	"cbcl12", 	"cbcl13", 	"cbcl14", 	"cbcl15", 	
          "cbcl16", 	"cbcl17", 	"cbcl18", 	"cbcl19", 	"cbcl20", 	"cbcl21", 	"cbcl22", 	
          "cbcl23", 	"cbcl24", 	"cbcl25", 	"cbcl26", 	"cbcl27", 	"cbcl28", 	"cbcl29", 	
          "cbcl30", 	"cbcl31", 	"cbcl32", 	"cbcl33", 	"cbcl34", 	"cbcl35", 	"cbcl36", 	
          "cbcl37", 	"cbcl38", 	"cbcl39", 	"cbcl40", 	"cbcl41", 	"cbcl42", 	"cbcl43", 	
          "cbcl44", 	"cbcl45", 	"cbcl46", 	"cbcl47", 	"cbcl48", 	"cbcl49", 	"cbcl50", 	
          "cbcl51", 	"cbcl52", 	"cbcl53", 	"cbcl54", 	"cbcl55", 	"cbcl56a", 	"cbcl56b", 	
          "cbcl56c", 	"cbcl56d", 	"cbcl56e", 	"cbcl56f", 	"cbcl56g", 	"cbcl56h", 	"cbcl57", 	
          "cbcl58", 	"cbcl59", 	"cbcl60", 	"cbcl61", 	"cbcl62", 	"cbcl63", 	"cbcl64", 	
          "cbcl65", 	"cbcl66", 	"cbcl67", 	"cbcl68", 	"cbcl69", 	"cbcl70", 	"cbcl71", 	
          "cbcl72", 	"cbcl73", 	"cbcl74", 	"cbcl75", 	"cbcl76", 	"cbcl77", 	"cbcl78", 	
          "cbcl79", 	"cbcl80", 	"cbcl81", 	"cbcl82", 	"cbcl83", 	"cbcl84", 	"cbcl85", 	
          "cbcl86", 	"cbcl87", 	"cbcl88", 	"cbcl89", 	"cbcl90", 	"cbcl91", 	"cbcl92", 	
          "cbcl93", 	"cbcl94", 	"cbcl95", 	"cbcl96", 	"cbcl97", 	"cbcl98", 	"cbcl99", 	
          "cbcl100", 	"cbcl101", 	"cbcl102", 	"cbcl103", 	"cbcl104", 	"cbcl105", 	"cbcl106", 	
          "cbcl107", 	"cbcl108", 	"cbcl109", 	"cbcl110", 	"cbcl111", 	"cbcl112", 	"cbcl113")

template <- data.frame(t(cbcl))
colnames(template) <- cbcl
template <- template[0,]

rm(rng)
rm(rng2)
rm(rng3)
rm(cbcl)
rm(sample_data)