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


levels(names$Species)


phang.align = as.phyDat(data, type = "DNA")

dml = dist.ml(phang.align)

treeNJ = NJ(dml)

treeNJ$tip.label <- names$Individual
names(phang.align) <-names$Individual

fit = pml(treeNJ, data = phang.align)


write.tree(fit$tree, "./Tree_Complete/tree.nwk")
tree = read.tree("./Tree_Complete/tree.nwk")


clr = c("#ff6a00","#015600","#ff0000","#635c2b","#7a1c00","#7c0947","#a3007f","#ff4c7f",
          "#0ab5b5","#08166d","#000000","#b24f03","#00a055")

tr = ggtree(tree, layout = "circular", branch.length = 'none')
tr = tr + geom_tiplab2(aes(color = labels$Genus), size = 4) + theme(legend.position = "right") + 
  scale_color_manual(labels= c("Maple", "Maidenhair Fern", "Dingo", "Cat", "Apple", "Trout",
                               "Slime Mold", "Pine", "Gilled Mushrooms", "Oak",
                               "Yeast", "Tulip", "Moss") , values = clr)
tr = tr + labs(title = expression(paste(italic("Quercus")) ~ "Species 18s Sequences Compared with other Eukaryotes"),
          color = "Group") + theme(title = element_text(size = 20), legend.text = element_text(size = 15)) + guides(color = guide_legend(override.aes = list(size = 10)))

tr
ggsave("Tree_Complete/quercus_tree.pdf",tr, width = 50, height = 50, units = "cm", dpi = 1000, 
       limitsize = FALSE)                  


