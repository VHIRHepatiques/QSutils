DottedAlignmentPlot <-
function(d.alignment)
{ mut=NULL;letter=NULL;haplotypes=NULL;x=NULL;
  p<-data.frame(d.alignment,stringsAsFactors=FALSE)
  aln = data.frame(
    letter = unlist(strsplit(p$d.alignment,"")), 
    haplotypes = rep(rownames(p),nchar(p$d.alignment)),
    x = rep(1:nchar(p$d.alignment[1]), nrow(p))
  )
  aln$mut<- as.factor(aln$letter)
  aln$haplotypes<-factor(aln$haplotypes, levels=rev(unique(aln$haplotypes)))
  if(max(aln$x)>100){
    aln$line <- as.factor(rep(1:(max(aln$x)/100),each=100))
    aln$x<-rep(1:100, (max(aln$x)/100))
    levels(aln$line)<- rep(1:(max(aln$x)/100))
  }else{
    aln$line <- "1"
  } 
  
p2 = ggplot(aln, aes(x, haplotypes)) +
    geom_text(aes(label=letter, color=mut)) + 
    scale_x_continuous(breaks=1:10, expand = c(0.105, 0)) + xlab('') + 
    theme_logo() +
    theme(legend.position = 'none', axis.text.x = element_blank(),strip.text.y = element_blank())+ 
    facet_grid(line ~ .) +
    scale_color_manual(values=c('black', 'red','green','darkgoldenrod1','blue','darkorange2',
                                'darkorchid4','darkred','goldenrod3','darkcyan','green4',
                                'hotpink','navy','burlywood2','darkolivegreen3','mediumpurple1',
                                'tan4','lightcoral','forestgreen','lightpink4','mediumorchid2')) 
  
  return(p2)
}
