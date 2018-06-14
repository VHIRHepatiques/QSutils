ICposition <-
function (seqs,nr=NULL)
{ IC=NULL; pos=NULL;
  dplot <- data.frame(IC=GetInfProfile(seqs,nr),pos=1:width(seqs)[1])
  plot<-ggplot(dplot, aes(x=pos, y=IC)) + geom_point() +
  scale_x_continuous(minor_breaks = 1:nrow(dplot), breaks = 1:nrow(dplot))
  theme(axis.text.x = element_text(angle=45))
  return(plot)
}
