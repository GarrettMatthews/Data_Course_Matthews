library(annotate)
library(ape)
library(rentrez)
library(taxize)
names = read.delim("./oak_seqnames_clean.txt", header = FALSE)
name = c()
x = 1

for(i in(names$V1)){
  name[x] = i
  x = x + 1
}

options(max.print = 1100)
print(name)
## To do: Use ape (read.Genbank?) to access scientific names from accession numbers (try it)

read.GenBank(name)
