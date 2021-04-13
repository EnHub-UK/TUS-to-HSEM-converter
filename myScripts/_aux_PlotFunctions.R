#' ----------------------------------------------------------------------------
#' TUS Converter                                                   {graphics}
#'
#' This file contains auxiliary functions to produce some charts
#'
#' ----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'


cbbPalette <- c('#606561','#58CEBC','#9E9AED','#FF6E5D','#CEF457','#CC4F4D',
'#7F58DE','#E0614D','#8AF97C','#502CBC')

theme.global <- theme(
  legend.position = "right",
  legend.background = element_rect(fill="transparent"),
  legend.key = element_rect(fill = "#ffffff", color = "#ffffff"),
  legend.key.width = unit(0.1, "in"),
  legend.key.height = unit(0.25, "in"),
  legend.title=element_blank(),
  plot.background = element_blank(),
  panel.background = element_rect(fill = "#F9F9F9"),
  axis.ticks = element_line(colour = "#b8b8b8"),
  axis.ticks.x = element_line(size = rel(4)),
  axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
)

figActivitiesBars <- function(dtaToPlot, varX, varW){
  dtaToPlot <- dtaToPlot[,c(varX, varW)]
  colnames(dtaToPlot) <- c('varX', 'varW')

  p <- ggplot(dtaToPlot, aes(varX)) +
    geom_bar()
  p <- p + facet_wrap(~ varW)
  p <- p + guides(fill=guide_legend(title=NULL, reverse=T,
																		label.position = "right",
																		keywidth = 0.5, keyheight = 2))
  p <- p + xlab("") + ylab("")
  p <- p + coord_flip() + theme_minimal()
  print(p)
}

figActivitiesPanel <- function(dtaToPlot, varX, varY, varZ, varW,
                               type="split"){

  dtaToPlot <- dtaToPlot[,c(varX, varY, varZ, varW)]
  colnames(dtaToPlot) <- c('varX', 'varY', 'varZ', 'varW')

  dtaToPlot$varX <- as.numeric(hm(gsub("(*.)-(.*)","\\1",dtaToPlot$varX)))
  dtaToPlot$varW <- factor(paste0("p.", dtaToPlot$varW))

  p <- ggplot(dtaToPlot, aes(varX, varY, color=varZ))
  if(type=="split"){
    p <- p + geom_point(shape=18, show.legend = F)
    p <- p + facet_wrap(~ varW + varZ, ncol = 2)
  }else{
    p <- p + geom_jitter(aes(colour=varW), shape=15)
    p <- p + facet_wrap(~ varZ)
  }
  p <- p + guides(fill=guide_legend(title=NULL, reverse=T,
																		label.position = "right",
																		keywidth = 0.5, keyheight = 2))
  p <- p + xlab("") + ylab("")
  p <- p + scale_fill_manual(values=cbbPalette)
  p <- p + scale_x_time()
  p <- p + theme_minimal()
  p <- p + theme(legend.position = "bottom")

  print(p)

}

figTransMatrixPlot <- function(dtaToPlot, type="area", ...){
  t <- scale_fill_brewer(palette = "Spectral")

  if(type=="area"){
    gp <-  ggplot(dtaToPlot,
                  aes(y = value, x = as.numeric(tid), fill = variable)) +
      geom_area() + xlab("time slot") + ylab("probability") + t
  }else{
    gp <-  ggplot(dtaToPlot,
                  aes(y = value, x = as.numeric(tid), fill = variable)) +
      geom_bar(stat="identity") + xlab("time slot") + ylab("events") + t
  }
  return(gp)
}

figActivityChains <- function(dtaToPlot, varToPlot="value",
                              tyLine="line", tyPlot="group", no.periods){

  typeRequest <- paste(tyPlot,tyLine,sep="-")

  no.periods <- 1:no.periods

  dtaToPlot <- dtaToPlot[,c('tid','day','idx','variable',varToPlot)]
  colnames(dtaToPlot) <- c('tid','day','idx','variable','value')
  dtaToPlot <- subset(dtaToPlot, day %in% no.periods)

  pg <- ggplot(dtaToPlot,
               aes(x = as.integer(tid), y = value,
                   group = variable, color = variable)) +
    xlab("time slot resolution") + ylab("") +
    scale_color_manual(values=cbbPalette) +
    theme(legend.position="bottom") +
    scale_x_continuous()

  pq <- ggplot(dtaToPlot,
               aes(x = as.integer(tid), y = value,
                   group = as.factor(day), color = variable)) +
    xlab("time slot resolution") + ylab("") +
    scale_color_manual(values=cbbPalette) +
    theme(legend.position="bottom") +
    scale_x_continuous()

  pp <- dtaToPlot %>%
    group_by(variable, value) %>%
    summarise(n = n()) %>%
    ggplot(., aes(x = value, y = n, group = variable, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.6)) +
    theme_bw() + scale_fill_manual(values = cbbPalette) +
    theme(legend.position="bottom")

  pt <- ggplot(dtaToPlot,
               aes(x = as.integer(idx), y = value,
                   group = idx, color = variable)) +
    xlab("time slot resolution") + ylab("") +
    scale_color_manual(values=cbbPalette) +
    theme(legend.position="bottom") +
    scale_x_continuous()

  pg = switch(typeRequest,
    "group-line"={pg + geom_line(size=0.75) +
        facet_wrap(~ variable, ncol = 1, strip.position = "right", as.table=F)},
    "group-step"={pg + geom_step(size=0.75) +
        facet_wrap(~ variable, ncol = 1, strip.position = "right", as.table=F)},
    "single-line"={pq + geom_line(size=0.05) +
        facet_wrap(~ variable, ncol = 1, strip.position = "right", as.table=F)},
    "single-step"={pq + geom_step(size=0.05) +
        facet_wrap(~ variable, ncol = 1, strip.position = "right", as.table=F)},
    "polar-line"={pp + coord_flip()},
    "polar-step"={pp + geom_step(size=0.05) +
        facet_wrap(~ variable) + coord_polar()},
    "cycle-line"={pt + geom_jitter(size=0.75, width = 0.9, height = 0.1) +
        facet_wrap(~ variable, ncol = 1, strip.position = "right", as.table=F)},
    "cycle-step"={pt + geom_jitter(size=0.75, width = 0.9, height = 0.1)}
  )
  p.print <- pg + theme_minimal()
  print(p.print)
}
