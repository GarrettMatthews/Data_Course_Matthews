# Import libraries
library(phangorn)
library(treeio)
library(ggtree)
library(tidyverse)
# Reading the data
data = readRDS("./Tree_Complete/data_msa.RDS")
names = read.csv("./FASTA_Files/name_list.csv", sep = " ")
names = names[,1:4]
labels= read.csv("./edge_label.csv", sep = " ")
# Building the tree

phang.align = as.phyDat(data, type = "DNA")

dml = dist.ml(phang.align)

treeNJ = NJ(dml)

treeNJ$tip.label <- names$Individual
names(phang.align) <-names$Individual

fit = pml(treeNJ, data = phang.align)


write.tree(fit$tree, "./Tree_Complete/tree.nwk")
tree = read.tree("./Tree_Complete/tree.nwk")


nme = as.character((rep(names$Genus,2)))
nme = nme[1:460]

clr = c("#ef2009","#085b03","#60453a","#7c7a79","#26ed23","#7c0947","#d88af2","#012602",
        "#ccc42e","#0fc677","#0d0042","#b24f03","#05478e")

tr = ggtree(tree, layout = "circular", branch.length = 'none')
tr = tr + geom_tiplab2(aes(color = labels$Genus), size = 4) + theme(legend.position = "right") + 
  scale_color_manual(labels= c("Maple", "Maidenhair Fern", "Dingo", "Cat", "Apple", "Trout",
                               "Slime Mold", "Pine", "Gilled Mushrooms", "Oak",
                               "Yeast", "Tulip", "Moss") , values = clr)
tr = tr + labs(title = expression(paste(italic("Quercus")) ~ "Species 18s Sequences Compared with other Eukaryotes"),
          color = "Group") + theme(title = element_text(size = 20), legend.text = element_text(size = 15))

tr
ggsave("Tree_Complete/quercus_tree.pdf",tr, width = 50, height = 50, units = "cm", dpi = 1000, 
       limitsize = FALSE)                  

factor(fit$tree$edge)

