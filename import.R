library(readr)
library(dplyr)
motrgage <- read_delim("motrgage.csv", ";", 
                       escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
View(motrgage)

end1chunk <-        max(which(motrgage[1e6:(1e6 + 100), 3] == motrgage[[1e6, 3]])) + 1e6 -1
end2chunk <-dim(motrgage)[1]

motrgage<- motrgage %>% mutate(status=ifelse(X5==1,91,X4))
write.table(as.data.frame(motrgage[1:end1chunk,]),
            "1chunk.csv",
            dec = "," ,
            sep = ";", row.names=F)
write.table(as.data.frame(motrgage[(end1chunk - 1):end2chunk,]),
            "2chunk.csv",
            dec = "," ,
            sep = ";", row.names=F)