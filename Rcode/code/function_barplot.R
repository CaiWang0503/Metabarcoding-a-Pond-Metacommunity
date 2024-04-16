#function for figure 3
coef.figure <- function(summary.p) {
  effect <- data.frame(summary.p$coefmat)
  effect$rownames=rownames(effect)
  effect<-separate(data = effect, col = rownames, into = c("species", "coef"), sep = " ")
  effect<-effect %>% filter(coef != "(Intercept)")
  effect<-effect %>% dplyr::select(-Z.value)
  effect$coef<-as.factor(effect$coef)
  effect$max<-NA
  effect$p<-NA
  
  n=nrow(summary.p$sigma) # number of species
  m=ncol(data.frame(coef(model)$env))-1
  
  for (i in 1:n) {
    max_effects = as.numeric(apply(data.frame(effect[((m*i-(m-1)):(m*i)),1]),2, function(e) which.max(abs(e)))) 
    max_effects=max_effects + ((i-1)*m)
    effect$max[max_effects]= "max"
  }
  effect$max[is.na(effect$max)] <- " "
  
  for (i in 1:(m*n)) {
    if(is.na(effect$Pr...z..)[i]){effect$Pr...z..[i]=0}
    if(effect$Pr...z..[i]<0.001){effect$p[i]="***"}
    else if (0.001<effect$Pr...z..[i] & effect$Pr...z..[i]<0.01){effect$p[i]="**"}
    else if (0.01<effect$Pr...z..[i] & effect$Pr...z..[i]<0.05){effect$p[i]="*"}
    else if (0.05<effect$Pr...z..[i] & effect$Pr...z..[i]<0.1){effect$p[i]="."}
    else {effect$p[i]=""}
  }
  effect$group=NA
  effect$group[grep("Acti*",effect$species)]="Fish"
  effect$group[grep("Amph*",effect$species)]="Amphibians"
  effect$group[grep("Mamm*",effect$species)]="Mammal"
  effect$group[grep("Aves*",effect$species)]="Perching birds"
  effect$group[grep("Dmst",effect$species)]="Domestic animals"
  effect$group[grep("Wafl",effect$species)]="Waterfowl"

  effect$coef=as.factor(effect$coef)
 #levels species 
  effect.species<-separate(effect,species,c("o","g","species"),sep= "_")
  effect$species<-paste0(effect.species$o," ",effect.species$g," ",effect.species$species)

  spnames3<-separate(spnames,colnames.Y.,c("o","g","species"),sep= "_")
  spnames3<-paste0(spnames3$o," ",spnames3$g," ",spnames3$species)
  
  effect$species=factor(effect$species,levels = rev(spnames3)) 
#levels coef 

effect$coef=factor(effect$coef,levels = c('HSI2_Pond_area','HSI3_Pond_drying','HSI4_Water_quality','HSI5_Shade','HSI10_Macrophytes','agriculture.urban','grassland','woodland')) 

effect$group=factor(effect$group,levels = c('Fish','Amphibians','Perching birds','Domestic animals','Mammal','Waterfowl')) 
  
t=-0.98
p=ggplot(effect,aes(x = species, y = Estimate, fill = group)) +
    geom_bar(position = position_dodge(0.6), stat="identity", width = 0.5)+
    scale_fill_viridis(discrete=T)+
    guides(fill = guide_legend(reverse=F))+
    xlab("Species") + 
    ylab("Environmental covariate") + 
    labs(fill="Species grouping") + 
    coord_flip(expand=F) + 
    geom_hline(aes(yintercept = 0),linetype="dashed",linewidth=1) +
    ggplot2::theme_classic()+ facet_wrap(~coef, ncol = 8)+
    geom_text(aes(y= t, label = max), position = position_dodge(0.6), 
              size = 2, fontface = "bold")+ 
    geom_text(aes(y= t+0.17, label = p), position = position_dodge(0.3), 
              size = 2, fontface = "bold")+
    geom_errorbar(aes(ymax = Estimate + Std.Err, ymin = Estimate - Std.Err), width = 0.2)+ scale_y_continuous(limits = c(-1.6,1.5))+ theme(axis.text.x = element_text( size=5))+theme(axis.text.y = element_text(family = "Times", face = "italic",lineheight = .9))

coef=p
return(coef)
}

