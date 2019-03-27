# Load libraries
library(msa)
library(phangorn)
library(ggtree)
library(treeio)
library(seqinr)
# Align and save the data, so there is no need to repeat the msa later
# Commenting these out because the data has been saved
# oak = msa("./oak18s_clean.fasta", type = "dna")
# saveRDS(oak,"./oak18s_alignment.RDS")
ofas = read.fasta("./oak18s_clean.fasta")
seq_names = names(ofas)
# Reading the data back in
oak = readRDS("./oak18s_alignment.RDS")
## Building the tree ##
phang.align = as.phyDat(oak, type = "DNA")

dml = dist.ml(phang.align)

oakNJ = NJ(dml)
oakNJ$tip.label <- seq_names
fit = pml(oakNJ, data = phang.align)
