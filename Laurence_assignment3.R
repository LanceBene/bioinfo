## Die Library dplyr wird aufgerufen, da die "recode" Funktion sehr komfortabel ist
library(dplyr)

PatternToNumber <- function(Pattern) {
SplitPattern <- unlist(strsplit(Pattern, split = ""))
BaseToNumber <- recode(SplitPattern, "A"=0, "C"=1, "G"=2, "T"=3)
Prefix <- {}
for (i in 0:length(BaseToNumber)) {
  Prefix[i] <- (BaseToNumber[i])*(4^(length(BaseToNumber)-i))
  BaseAsNumber <- sum(Prefix)
}
print(BaseAsNumber, digits = 20)
}

PatternToNumber("AGT")
PatternToNumber("CTTCTCACGTACAACAAAATC")

NumberToPattern <- function(Number, k){

Quotient <- {}
Remainder <- {}
Quotient[1] <- Number%/%4
Remainder[1] <- Number%%4


for (i in 1:(k-1)) {
  Quotient[i+1] <- Quotient[i]%/%4
  Remainder[i+1] <- Quotient[i]%%4
}

Reversed <- rev(Remainder)
NumberAsPattern <- recode(Reversed, "0" = "A", "1" = "C", "2" = "G", "3" = "T")
NumberAsPattern <- paste(NumberAsPattern, collapse="")
print(NumberAsPattern)
}

NumberToPattern(0, 2)

text <- c("ACGCGGCTCTGAAA")
k <- 2

## Diese Funktion schneidet leider immer das erste kmer (AA) ab, alle anderen stimmen aber. 
# Ausserdem gibt sie deutlich mehr aus als den gewünschten Dataframe, was vor allem bei der nächsten Funktion nervt.
ComputingFrequencies <- function(text, k) {
kmer <- {}
Index <- {}
Frequency <- {}

for(i in 0:(4^k)) {

  kmer[i] <- NumberToPattern(i, k)

  Index[i] <- i

  Frequency[i] <- 0
}


for (i in 0:(nchar(text)-k)) {
  Pattern2 <- {}
  Pattern2 <- substr(text, i+1, i + k)
  j <- {}
  j <- PatternToNumber(Pattern2)
  Frequency[j] <- (Frequency[j] +1)
}

FrequencyArray <- tibble(kmer, Index, Frequency)
print(FrequencyArray)
}
ComputingFrequencies("ACGCGGCTCTGAAA", 2)

## Diese Funktion gibt immer den ganzen Output von ComputingFrequencies mit an. 
# Ausserdem wird das Ergebnis doppelt ausgegeben.
Fasterfrequentwords <- function(text, k){
FrequentPatterns <- {}  
FrequencyArray2 <- ComputingFrequencies(text, k)
maxCount <- max(FrequencyArray2$Frequency)
for(i in 1:((4^k)-1)) {
  if (FrequencyArray2$Frequency[i]==maxCount){
  Pattern<-{}
  Pattern <- NumberToPattern(i, k)
  FrequentPatterns <- append(FrequentPatterns, Pattern)
  }}
  UniqueFrequentPatterns <- unique(FrequentPatterns)
  print(UniqueFrequentPatterns)
}

Fasterfrequentwords("AAGCAAAGGTGGG", 2)
