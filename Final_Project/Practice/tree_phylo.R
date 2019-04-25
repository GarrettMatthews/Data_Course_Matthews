# Load libraries
library(msa)
library(phangorn)
library(ggtree)
library(treeio)
library(seqinr)
library(expss)
library(ape)
library(tidyverse)
# Align and save the data, so there is no need to repeat the msa later
# Commenting these out because the data has been saved
# oak = msa("./oak18s_clean.fasta", type = "dna")
# saveRDS(oak,"./oak18s_alignment.RDS")
# Generating a list of sequence names
names = read.delim("./oak_seqnames_clean.txt", header = FALSE)
name = c()
x = 1
for(i in(names$V1)){
  name[x] = i
  x = x + 1
}
# Reading data frame of Species name and accession numbers
nm_ac = read.delim("./oak_acc.csv", header = FALSE)
# For some reason the way nm_ac was read in it had 36 columns, so this next line just gets rid
# of the excess columns
nm_ac = nm_ac[,1:2]
# Reading the data back in
oak = readRDS("./oak18s_alignment.RDS")
names = read.csv("./oakacc_names.csv", header = FALSE, sep = " ")
names = names[,1:2]

## Building the tree ##

phang.align = as.phyDat(oak, type = "DNA")

dml = dist.ml(phang.align)

oakNJ = NJ(dml)

lookup = oakNJ$tip.label



oakNJ$tip.label <- c(1:1006)
names(phang.align) <-c(1:1006)
fit = pml(oakNJ, data = phang.align)




write.tree(fit$tree, file = "./oak_phylo_numb.nwk")
otree = read.tree("oak_phylo_names.nwk")

ggtree(otree) 
