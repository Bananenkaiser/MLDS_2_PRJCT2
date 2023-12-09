{rm(list=ls(all=TRUE))   
graphics.off()
setwd("C:/Users/dtisl/OneDrive - studmail.w-hs.de/Studium/3. Semester/MLDS 2/Projekte/Projekt 2")
getwd()


library(ggplot2)
library(readxl)
library(dplyr)

df <- read_excel("Referat II __ Student II.xls", skip = 2)

df_list <- split(df, df$'Artikel Nr')

df_list <- df_list[[7]]

colnames(df_list) <- gsub(" ", "_", colnames(df_list))

df_sorted <- df_list[order(df_list$Jahr, df_list$KW), ]
#colnames(df_sorted) <- gsub(" ", "_", colnames(df_sorted))

df <- ts(df_list$Abverkauf, frequency = 1)
dfv <- as.vector(df)
dfv <- as.numeric(dfv)


plot(dfv, type = "l", col = "blue", xlab = "Zeit", ylab = "Abverkauf")
title(main = df_sorted$Artikel_Nr[1], col.main = "black", font.main = 4, cex.main = 1.2)

length(df_sorted[[1]])
}

##################################################################

{
# Überprüfe auf doppelte Werte in der Kombination Jahr und KW
duplicates <- df_sorted[duplicated(df_sorted[c("Jahr", "KW")]) | duplicated(df_sorted[c("Jahr", "KW")], fromLast = TRUE), ]

# Überprüfe, ob bei einem doppelten Paar der Wert bei Abverkauf gleich null ist
duplicates_with_zero_abverkauf <- duplicates[duplicates$Abverkauf == 0, ]

if (nrow(duplicates_with_zero_abverkauf) > 0) {
  cat("Es gibt doppelte Werte in der Kombination Jahr und KW, bei denen Abverkauf gleich null ist.\n")
  
  # Lösche die Zeilen mit doppelten Werten und Abverkauf gleich null
  df_sorted <- df_sorted[!(duplicated(df_sorted[c("Jahr", "KW")]) & df_sorted$Abverkauf == 0), ]
  
  cat("Die entsprechenden Zeilen wurden gelöscht.\n")
} else {
  cat("Die KW-Werte sind entweder lückenlos fortgesetzt oder bei doppelten Werten ist Abverkauf ungleich null.\n")
}


length(df_sorted[[1]])
}
##################################################################


{
artikel_nr <- df_list$Artikel_Nr[1]
start_jahr <- df_list$Jahr[1]
start_kw <- df_list$KW[1]
end_jahr <- df_list$Jahr[length(df_sorted[[1]])]
end_kw <- df_list$KW[length(df_sorted[[1]])]



df_filtered <- df_sorted %>% 
  filter((Jahr > start_jahr | (Jahr == start_jahr & KW >= start_kw)) & 
           (Jahr < end_jahr | (Jahr == end_jahr & KW <= end_kw)))

}


{
expected_kw <- start_kw
expected_jahr <- start_jahr
for (i in 1:nrow(df_filtered)) {
  while (df_filtered$Jahr[i] != expected_jahr || df_filtered$KW[i] != expected_kw) {
    cat("Lücke gefunden in Jahr", df_filtered$Jahr[i], ", erwartet:", expected_jahr, "-", expected_kw, ", tatsächlich:", df_filtered$Jahr[i], "-", df_filtered$KW[i], "\n")
    
    # Fülle die Lücke mit 0 für "Abverkauf"
    new_row <- data.frame(
      Artikel_Nr = artikel_nr,
      Jahr = expected_jahr,
      KW = expected_kw,
      Abverkauf = 0
    )
    
    # Füge die neue Zeile an der richtigen Position ein
    df_sorted <- bind_rows(df_sorted[1:(i-1), ], new_row, df_sorted[i:nrow(df_sorted), ])
    cat("Zeile hinzugefügt \n")
    
    # Aktualisiere die erwarteten Werte
    expected_kw <- (expected_kw %% 52) + 1  # Modulo 52, da es 52 Kalenderwochen im Jahr gibt
    expected_jahr <- ifelse(expected_kw == 1, expected_jahr + 1, expected_jahr)
  }
  
  expected_kw <- (df_filtered$KW[i] %% 52) + 1  # Modulo 52, da es 52 Kalenderwochen im Jahr gibt
  expected_jahr <- ifelse(expected_kw == 1, expected_jahr + 1, expected_jahr)
}

df_sorted <- df_sorted[order(df_sorted$Jahr, df_sorted$KW), ]
length(df_sorted[[1]])

}
##################################################################

{
df <- ts(df_sorted$Abverkauf, frequency = 1)
dfv <- as.vector(df)
dfv <- as.numeric(dfv)


plot(dfv, type = "l", col = "blue", xlab = "Zeit", ylab = "Abverkauf")
title(main = artikel_nr, col.main = "black", font.main = 4, cex.main = 1.2)
}

print(df_sorted)
##################################################################

