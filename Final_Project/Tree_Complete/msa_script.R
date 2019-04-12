# Importing libraries
library(msa)
# Reading the fasta file
data = msa("./FASTA_Files/fasta_clean.fasta", type = "dna")
# Saving to a RDS
saveRDS(data,"./Tree_Complete/data_msa.RDS")
