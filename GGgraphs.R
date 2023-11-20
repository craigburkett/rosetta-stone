# margin unit is (top, right, bottom, left)
library(tidyverse)
library(scales, warn.conflicts = F) # For pretty breaks, mainly
library(ggthemes) # For better palettes
library(gghighlight) # Highlighting of graphs
library(rlang) # For sym() parsing text programmatically
library(stringi) # To convert to ASCII text
 
# Resolution - lower this to make smaller size files
# 600 is certainly OK for printing but makes file sizes that are a bit large 
dpiRes <- 300

# Plot dimensions (mm)
plotWidth <- 304.8
plotHeight <- 120

# Main colour for plots 
fillColour <- "darkred"
coloursBSC <- c("darkred", "steelblue", "darkgreen", "purple", "darkblue", "darkorange", "red", "orange", "green", "cyan", "blue", "violet")
colFunc <- colorRampPalette(solarized_pal()(8))

### Main theme for plots ###
theme_BSC <- function(size_base= 16, size_tit= 20, position_leg= "right", border=c(0.25,0.25,0.25,0.25)) {
  theme(
    text =              element_text(size=size_base, colour="black"),
    axis.line.x =       element_line(colour="black"),
    axis.line.y =       element_line(colour="black"), # Ubuntu needs both
    axis.text =         element_text(size=size_base, colour="gray40"),
    axis.ticks =        element_line(colour="black"),
    
    legend.key =        element_blank(),
    legend.position =   position_leg,
    
    panel.background =  element_blank(),
    panel.border =      element_blank(),
    panel.grid.major.x= element_blank(),
    panel.grid.major.y= element_line(colour="grey"),
    panel.grid.minor =  element_blank(),
    
    plot.title =        element_text(size = size_tit, colour = "black"),
    plot.margin =       unit(border, "cm")
    # margin unit is (top, right, bottom, left)
  )
}

# Optional transparent background theme
theme_BSC_trans <- function() {
  theme(
    legend.background = element_rect(fill = "transparent", colour = NA),
    panel.background =  element_rect(fill = "transparent", colour = NA),
    plot.background =   element_rect(fill = "transparent", colour = NA)
  )
}



gg_count <- function(dd, type="Bar", filename="", tit="", x.lab="", y.lab="", maxLevels=8, maxGroups=4, maxLabelL = 20, 
                     iLog=F, iLabel=F, iPareto=T, iKeepFac=F, iRotateX=F, iCoordFlip=F, write=F, echo=T, trans=T) {
  ### Creates a barplot of a single variable, or two variables with grouping
  
  ## Inputs:
  # dd[1] (req): factor/char
  # dd[2] (opt): numeric or factor/char or grouping factor
  # dd[3] (opt): numeric
  
  if(type == "Jitter") return(NULL)
  stopifnot(type %in% c("Bar", "Circle", "Tile"))
  
  if(ncol(dd) > 3 | ncol(dd) < 1) return(NULL)
  
  # Single col: barplot (no grouping) - aggregate and go to next block
  if(ncol(dd) == 1){
    names(dd) = c("var")
    dd = dd %>% group_by(var) %>% summarize(count = n())
  } 
  
  # Two cols: colplot (no grouping) - draw plot
  if(ncol(dd) == 2 & lapply(dd, class)[2] %in% c("integer", "numeric")) {
    names(dd) <- c("var", "count")
    dd = dd %>% mutate(var = abbreviate(stri_enc_toascii(var), maxLabelL))
    dd = dd %>% arrange(desc(count)) %>% slice(1:maxLevels)
    
    b = ggplot(dd, aes(x= var, y= count))
    if(iPareto) b = ggplot(dd, aes(x= reorder(var, -count), y= count))
    
    b = b + geom_col(position= "dodge", col= "black", fill= fillColour) 
  }
  
  # Two cols: barplot (w. grouping) - aggregate and go to next block
  else if(ncol(dd) == 2 & lapply(dd, class)[2] %in% c("character", "factor")) {
    names(dd) <- c("var", "group")
    
    if(!iKeepFac){
      dd = dd %>%
        mutate(
          group = fct_lump(group, n = maxGroups)
          , var = fct_lump(var,   n = maxLevels)
        )
    }
    
    ddOrig = dd
    dd = dd %>% group_by(var, group) %>% summarize(count = n()) %>% ungroup()
  }
  
  # Three cols: colplot (w. grouping) - draw plot
  if(ncol(dd) == 3){
    names(dd) <- c("var", "group", "count")
    dd = dd %>% ungroup()
    
    if(!iKeepFac){
      dd = dd %>% 
      # TODO: Max groups/levels doesn't work here
      # mutate(
      #   group = fct_lump(factor(group), n = maxGroups)
      #   , var = fct_lump(factor(var),   n = maxLevels)
      # ) %>%
      mutate(
        group = abbreviate(stri_enc_toascii(group), maxLabelL)
        , var = abbreviate(stri_enc_toascii(var), maxLabelL)
      )
      }
    dd = dd %>%
      group_by(var, group) %>% 
      summarize(count = sum(count)) %>% 
      ungroup()
    
    b = ggplot(dd, aes(x= var, y= count, fill= group)) 
    if(iPareto) b = ggplot(dd, aes(x= reorder(var, -count), y= count, fill= group))
    
    colourCount = length(unique(dd$group))
    
    if(type == "Bar") b = b + geom_col(position= "dodge", col= "black") + scale_fill_manual(values = colFunc(colourCount))
    else if(type == "Circle" & exists("ddOrig")) b = ggplot(ddOrig, aes(x = var, y = group)) + geom_count()
    else if(type == "Tile") b = ggplot(dd, aes(x = var, y = group)) + geom_tile(aes(fill = count))
    # scale_fill_brewer(palette="Set1") #+ scale_fill_manual(values = coloursBSC)
  }
  
  ## General theme and lables
  b = b + labs(x=x.lab, y=y.lab, title=tit) + theme_BSC() 
  
  if(iLog & type == "Bar") b = b + scale_y_log10()
  if(iLabel) b = b + geom_label(aes(label = count))
  if(iRotateX) b = b + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  if(iCoordFlip) b = b + coord_flip()
  if(trans) b = b + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent") 
  if(echo) b
} 


gg_hist <- function(dd, dashPt = NA, iFreqPoly=F, iDensity=F, filename="", tit="", x.lab="", y.lab="", fillCol = fillColour, myBins=30, maxGroups=3, 
                    cutoff=Inf, iLog=F, iCoordFlip=F, iFacet = F, iNoLegend=F, write=F, echo=T, trans=T){
  ### Creates a histogram of a single variable, or two variables with grouping
  
  
  ## Inputs:
  # dd[1] (req): numeric or date 
  # dd[2] (opt): factor/char
  dd = as.data.frame(dd)
  
  # if(ncol(dd) > 2 | ncol(dd) < 1) stop("Incorrect number of columns in data frame. Should be 1-2")
  if(ncol(dd) > 2 | ncol(dd) < 1) return(NULL)
  
  # Single col: no grouping
  if(lapply(dd, class)[[1]] == "Date") iDate = TRUE else iDate = FALSE
  
  if(ncol(dd) == 2) names(dd) = c("num", "group")
  if(ncol(dd) == 1) names(dd) = c("num")
  
  # dd = dd[(dd$num < cutoff & dd$num > -cutoff),] # Might return vector
  dd = filter(dd, num < cutoff & num > -cutoff)
  
  if(ncol(dd) == 1){
    if(iFreqPoly) h = ggplot(dd, aes(x= num)) + geom_freqpoly()
    else if(iDensity) h = ggplot(dd, aes(x= num, y = after_stat(density))) + geom_histogram(bins = myBins, colour= "black", fill= fillCol)
    else          h = ggplot(dd, aes(x= num)) + geom_histogram(bins = myBins, colour= "black", fill= fillCol) 
  }    
  
  # Two cols: grouping
  # if(ncol(dd) == 2 & lapply(dd, class)[2] %in% c("factor", "character", "logical")) {
  if(ncol(dd) == 2) {
    dd$group = factor(dd$group)
    dd$group = fct_lump(dd$group, n = maxGroups)
    colourCount = length(unique(dd$group))
    
    h = ggplot(dd, aes(x= num))
    if(iFacet) {
      h = h + geom_histogram(bins = myBins, colour= "black", position = "dodge") +
        facet_wrap(~ group)
    }
    else if(iFreqPoly){
      h = h + geom_freqpoly(aes(y= after_stat(density), colour= group), bins = myBins, size= 1.5) +
        scale_colour_manual(values = colFunc(colourCount)) #+ theme(axis.text.x= element_blank())
    } 
    else{
      h = h + geom_histogram(aes(fill= group), bins = myBins, colour= "black", position = "dodge") +
        scale_fill_manual(values = colFunc(colourCount))
    }
    # scale_fill_brewer(palette="Set1") #+ scale_fill_manual(values = coloursBSC)
  }
  
  ## General theme and lables
  h = h + labs(x=x.lab, y=y.lab, title=tit) + theme_BSC() 
  
  if(!is.na(dashPt)) h = h + geom_vline(xintercept = dashPt, col = "blue", lty = 2)
  if(iLog)  h = h + scale_y_log10()
  if(iCoordFlip) h = h + coord_flip()
  if(iNoLegend) h = h + guides(fill="none", color="none")
  if(trans) h = h + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent")
  if(echo) h
}


gg_box <- function(dd, filename="", tit="", x.lab="", y.lab="", maxBoxes=8, maxGroups=6, maxLabelL = 20, cutoff=Inf, 
                   iLog=F, iPareto=T, iFacet=F, facetScales="fixed", iRotateX=F, write=F, echo=T, trans=T){
  ### Creates a boxplot with factor along X, numeric along Y
  
  ## Inputs:
  # dd[1] (req): factor/char
  # dd[2] (req): numeric or difftime 
  # dd[3] (opt): factor/char
  
  maxBoxes = min(maxBoxes, nrow(dd))
  
  if(ncol(dd) > 3 | ncol(dd) < 2) stop("Incorrect number of columns in data frame. Should be 2 or 3")
  if(nrow(dd) == 0) return(NULL)
  
  names(dd)[1:2] = c("var", "num")
  dd = dd %>% mutate(var = abbreviate(var, maxLabelL))
  
  if(maxBoxes < Inf & nlevels(dd$var)  < nrow(dd)) dd$var = fct_lump(dd$var, n = maxBoxes)
  if(maxBoxes < Inf & nlevels(dd$var) == nrow(dd)) dd = dd %>% arrange(desc(num)) %>% slice(1:maxBoxes)
  
  dd = dd %>% filter(num < cutoff & num > -cutoff) # Filter before deciding pareto order
  
  if(iPareto){
    # dd %>% group_by(var) %>% summarize(med = median(num)) %>% arrange(desc(med))
    index <- order(with(dd, tapply(num, var, median, na.rm= T)))
    dd$var <- factor(dd$var,rev(levels(dd$var)[index]))
  }
  
  ## 2 columns, no grouping
  if(ncol(dd) == 2) {
    grpLabel = ""
    
    ## Create boxplot
    b = ggplot(dd, aes(x= var, y= as.numeric(num))) +
      geom_boxplot(position="dodge", width=0.75, col=fillColour) 
  }
  
  
  
  if(ncol(dd) == 3) {
    grpLabel = names(dd)[3]
    names(dd)[3] = "group"
    dd = dd %>% mutate(group = abbreviate(group, maxLabelL))
    
    b = ggplot(dd, aes(x= var, y= as.numeric(num), col= fct_lump(group, maxGroups))) + 
      geom_boxplot(position="dodge", width=0.75)
    
    if(iFacet) b = ggplot(dd, aes(x= var, y= as.numeric(num))) + 
      geom_boxplot(position="dodge", width=0.75) + facet_wrap(~fct_lump(group, maxGroups), scales = facetScales)
    
  }
  
  ## General theme and lables
  b = b + expand_limits(y = 0) +
    scale_y_continuous(expand = c(0,0), limits=c(min(dd$num), max(dd$num))) +
    labs(x=x.lab, y=y.lab, title=tit, col = grpLabel) + 
    theme_BSC()
  
  if(iLog) b = b + scale_y_log10()
  if(iRotateX) b = b + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  if(trans) b = b + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent") 
  if(echo) b
}


gg_polar_time <- function(dd, filename="", tit="", x.lab="", y.lab="", write=F, echo=T, trans=T){
  ### Creates a polar histogram of times from a single column DF
  
  if(nrow(dd) == 0 | all(is.na(dd))) return(NULL)
  names(dd) = c("time")
  
  if(is.character(dd$time)) dd$time <- as.POSIXct(dd$time, format = "%H:%M")
  dd$time <- as.numeric(format(dd$time, "%H"))
  
  labs <- c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
            "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
            "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00")
  ## Create circular plot
  p <- ggplot(dd, aes(x= time)) +
    geom_histogram(binwidth= 1, fill= fillColour) +
    coord_polar() +
    scale_x_continuous(breaks = seq(0, 23, 1), labels = labs) + 
    labs(x=x.lab, y=y.lab, title=tit) +
    theme_BSC() + theme(axis.line = element_blank())
  
  if(trans) p = p + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent")
  if(echo) p
}


gg_scatter <- function(dd, filename="", tit="", x.lab="", y.lab="", cutoff=Inf, maxGroups=4, maxPoints = 10000, iSmooth=F, 
                       iXY=F, iJitter=F, iLog=F, iFacet=F, facetScales="fixed", write=F, echo=T, trans=T) {
  ### Creates a scatter plot of two numeric variables
  stopifnot(facetScales %in% c("fixed", "free", "free_x", "free_y"))
  
  ## Inputs:
  # dd[1] (req): numeric or difftime
  # dd[2] (req): numeric or difftime
  # dd[3] (opt): factor/character
  
  if(ncol(dd) < 2) stop("Incorrect number of columns in data frame. Should be 2 or 3.")
  names(dd)[1:2] = c("x", "y")
  
  ## Jitterbug
  if(iJitter) thisPosition = "jitter" else thisPosition = "identity"
  
  ## Cutoffs
  dd = dd %>% filter(x < cutoff & x > -cutoff)
  dd = dd %>% filter(y < cutoff & y > -cutoff)
  
  if(ncol(dd) == 2) {
    gName = ""
    s = ggplot(dd, aes(x= as.numeric(x), y= as.numeric(y))) + geom_point(shape=1, size=1, colour= fillColour, position= thisPosition) 
  } 
  else if(ncol(dd) == 3 & iFacet) {
    gName = names(dd)[3]
    names(dd)[3] = "g"
    s = ggplot(dd, aes(x= as.numeric(x), y= as.numeric(y))) + geom_point(shape=1, size=1, position= thisPosition) + facet_wrap(~g, scales = facetScales)
  } 
  else if(ncol(dd) == 3) {
    gName = names(dd)[3]
    names(dd)[3] = "g"
    s = ggplot(dd, aes(x= as.numeric(x), y= as.numeric(y), colour= fct_lump(g, maxGroups))) + geom_point(shape=1, size=1, position= thisPosition)
  } 
  
  
  ## General theme and lables
  s = s + labs(x=x.lab, y=y.lab, title=tit, colour=gName) + theme_BSC()
  
  if(iSmooth) s = s + geom_smooth(method= lm, se= FALSE, colour= "black")
  if(iXY)   s = s + geom_abline(slope = 1, intercept = 0, lty = 2)
  if(iLog)  s = s + scale_y_log10() + scale_x_log10()
  if(trans) s = s + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent")
  if(echo) s
}


gg_jitter <- function(dd, filename="", tit="", x.lab ="", y.lab ="", g.lab="", maxLevels=8, maxGroups=4, iPareto=F, iRotateX=F, 
                      maxLabelL = 20, manualColors=F, write=F, echo=T, trans=T) {
  ### Creates a jitter plot of two categorical variables, grouped by a third
  # TODO: Incorporate pareto on x and y
  
  ## Inputs:
  # dd[1] (req): factor/char
  # dd[2] (req): factor/char
  # dd[3] (req): logical/factor/char
  
  stopifnot(ncol(dd) == 3)
  names(dd) <- c("x", "y", "group")
  
  ## Max levels for x and y, and group
  dd = dd %>%
    mutate(
      group = fct_lump(factor(group), n = maxGroups)
      , x = fct_lump(factor(x), n = maxLevels)
      , y = fct_lump(factor(y), n = maxLevels)
    )
  
  ## Abbreviate names
  dd = dd %>% mutate(
    group = abbreviate(group, maxLabelL)
    , x = abbreviate(x, maxLabelL)
    , y = abbreviate(y, maxLabelL)
    )
  
  if(!iPareto) j = ggplot(dd, aes(x= x, y= y, color= group)) + geom_jitter(alpha=0.8)
  if(iPareto)  j = ggplot(dd, aes(x= fct_infreq(x), y= fct_infreq(y), color= group)) + geom_jitter(alpha=0.8)
  
  ## General theme and lables
  j = j + labs(x=x.lab, y=y.lab, title=tit, color=g.lab) + theme_BSC() 
  
  if(iRotateX) j = j + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  if(manualColors) j = j + scale_colour_manual(values = coloursBSC)
  if(trans) j = j + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent") 
  if(echo) j
}


# gg_time <- function(dd, xVar, yVar, idVar="", groupVar="", facetVar="", iAvg=F, iFreeScales=F, iErrorBars=F, filename="", write=F, echo=T, trans=T) {
gg_time2 <- function(dd, xVar, yVar, idVar="", groupVar="", facetVar="", iAgg=F, AggType="sum", iFreeScales=F, 
                     iErrorBars=F, iShowLeg=T, textSize = 12, filename="", write=F, echo=T, trans=T) {
  ### Timeplot showing change in yVar over Time (xVar)
  ### Optionally grouped by 'groupVar' and/or faceted by 'facetVar'
  
  ## Inputs:
  # dd: data frame with column names matching xVar, yVar, groupVar, facetVar
  # xVar: factor or date
  # yVar: numeric
  # groupVar: factor
  # facetVar: factor
  # iAgg: flag to indicate whether to aggregate data or not
  # AggType: Aggregation function to be used to aggregate groups, if iAgg is TRUE, otherwise ignored if passed
  # iFreeScales: flag for whether to allow scales to be free on faceting, vs consistent
  # iErrorBars: flag for whether to add CIs to avgs
  
  # stopifnot(all(c(xVar, yVar, idVar, groupVar, facetVar) %in% c("", names(dd))))
  stopifnot(AggType %in% c("sum", "mean", "median", "min", "max"))
  # dd = dd %>% filter(!is.na(!!sym(yVar)))
  dd = dd %>% filter_at(all_of(yVar), all_vars(!is.na(.)))
  
  ## Convert to long if multiple Y variables
  if(length(yVar) > 1){
    dd = gather(dd, "grp", "y", yVar)
    if(facetVar %in% names(dd)) dd = select(dd, xVar, facetVar, grp, y) else dd = select(dd, xVar, grp, y)
    
    yVar = "y"
    groupVar = "grp"
  } 
  
  confInterval = 0.95
  pd = position_dodge(0.1)
  
  if(nrow(dd) < 1) return(NULL)
  
  ## Is aggregation desired?
  if(iAgg){
    if(groupVar != "" && facetVar != "" && groupVar == facetVar) dd = dd %>% group_by(!!sym(facetVar), !!sym(xVar))
    else if(groupVar != "" && facetVar != "") dd = dd %>% group_by(!!sym(groupVar), !!sym(facetVar), !!sym(xVar))
    else if(groupVar != "")                   dd = dd %>% group_by(!!sym(groupVar), !!sym(xVar))
    else if(facetVar != "")                   dd = dd %>% group_by(!!sym(facetVar), !!sym(xVar))
    else                                      dd = dd %>% group_by(!!sym(xVar))
    
    ## Only run this if error bars requested and mean aggregation
    if(iErrorBars && AggType == "mean") {
      dd = dd %>% summarize(
        Y = mean(!!sym(yVar), na.rm=T)
        ,SD = sd(!!sym(yVar), na.rm=T)
        , N = n()
      ) %>%
        mutate(
          SE = SD / sqrt(N)
          , CI = SE * qt(confInterval/2 + .5, N) # Using N instead of N-1 to avoid errors when N = 1
        )
    } else {
      ## What kind of aggregation?
      FUN <- match.fun(AggType) 
      dd = dd %>% summarize(Y = FUN(!!sym(yVar), na.rm=T))
    } # END if
    
    yVar = "Y"
  } # END if
  
  ## Make base plot
  if(groupVar != "" && iAgg) p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= groupVar, color= groupVar)) # Grouping variable, aggregated
  else if(groupVar != "")    p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= idVar, color= groupVar)) # Grouping variable, individuals
  else if(iAgg)              p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= 1)) # No grouping, aggregated
  else                       p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= idVar, color= idVar)) # No grouping, individuals
  
  # Optional error bars
  if(iAgg && iErrorBars && AggType == "mean") p = p + geom_errorbar(aes(ymin = !!ensym(yVar) - CI, ymax = !!ensym(yVar) + CI), color="black", lty=2, width=0.1, position=pd)
  
  # Layers to add regardless
  p = p + geom_line(position=pd) + geom_point(size=3, shape=21, fill="white", position=pd) # Add point last to have nice fill
  
  # Optional faceting
  if(facetVar != "" && iFreeScales) p = p + facet_wrap(as.formula(paste("~", facetVar)), scales = "free_y")
  else if(facetVar != "")           p = p + facet_wrap(as.formula(paste("~", facetVar)))

  ## General theme
  p = p + theme_BSC(size_base = textSize)
  
  if(!iShowLeg) p = p + theme(legend.position = "none")
  if(trans) p = p + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent")
  if(echo) p
} 


gg_time <- function(dd, xVar, yVar, idVar="", groupVar="", facetVar="", iAvg=F, iFreeScales=F, iErrorBars=F, filename="", write=F, echo=T, trans=T) {
  ### Timeplot showing change in yVar over Time (xVar)
  ### Optionally grouped by 'groupVar' and/or faceted by 'facetVar'
  stopifnot(all(c(xVar, yVar, idVar, groupVar, facetVar) %in% c("", names(dd))))
  dd = dd %>% filter(!is.na(!!sym(yVar)))
  
  ## Inputs:
  # dd: data frame with column names matching xVar, yVar, groupVar, facetVar
  # xVar: factor or date
  # yVar: numeric
  # groupVar: factor
  # facetVar: factor
  # iAvg: Flag to indicate whether to aggregate groups or not (with mean)
  # iFreeScales: flag for whether to allow scales to be free on faceting, vs consistent
  # iErrorBars: flag for whether to add CIs to avgs
  confInterval = 0.95
  pd = position_dodge(0.1)
  
  if(nrow(dd) < 1) return(NULL)
  
  if(iAvg){
    if(groupVar != "" & facetVar != "") dd = dd %>% group_by(!!sym(facetVar), !!sym(groupVar), !!sym(xVar))
    else if(groupVar != "") dd = dd %>% group_by(!!sym(groupVar), !!sym(xVar))
    else if(facetVar != "") dd = dd %>% group_by(!!sym(facetVar), !!sym(xVar))
    else dd = dd %>% group_by(!!sym(xVar))
    
    dd = dd %>% summarize(
      AVG = mean(!!sym(yVar), na.rm=T)
      ,SD = sd(!!sym(yVar), na.rm=T)
      , N = n()
    ) %>%
      mutate(
        SE = SD / sqrt(N)
        , CI = SE * qt(confInterval/2 + .5, N) # Using N instead of N-1 to avoid errors when N = 1
      )
    yVar = "AVG"
  }
  
  ## Make base plot
  if(groupVar != "" & iAvg) p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= groupVar, color= groupVar)) # Grouping variable, averages
  else if(groupVar != "")   p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= idVar, color= groupVar)) # Grouping variable, individuals
  else if(iAvg)             p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= 1)) # No grouping, averages
  else                      p = dd %>% ggplot(aes_string(x=xVar, y=yVar, group= idVar, color= idVar)) # No grouping, individuals
  
  # Optional error bars
  if(iAvg & iErrorBars) p = p + geom_errorbar(aes(ymin = !!ensym(yVar) - CI, ymax = !!ensym(yVar) + CI), color="black", width=0.1, position=pd)
  
  # Layers to add regardless
  p = p + geom_line(position=pd) + geom_point(size=3, shape=21, fill="white", position=pd) # Add point last to have nice fill
  
  # Optional faceting
  if(facetVar != "" & iFreeScales) p = p + facet_wrap(as.formula(paste("~", facetVar)), scales = "free")
  else if(facetVar != "")          p = p + facet_wrap(as.formula(paste("~", facetVar)))
  
  ## General theme
  # p = p + theme_BSC()
  
  if(trans) p = p + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent")
  if(echo) p
} 


gg_line <- function(dd, points2add=NULL, tit="") {
  # DF with 3 cols: Date, Value, Group
  stopifnot(ncol(dd) == 3)
  
  yLab = names(dd)[2]
  names(dd) = c("Date", "var", "group")
  if(!is.null(points2add)) names(points2add) = c("Date", "var", "group")
  
  colsNeeded = length(unique(c(dd$group, points2add$group)))
  
  myPlot = dd %>%
    ggplot(aes(x= Date, y= var, colour= group)) +
    geom_line() +
    scale_colour_manual(values= colFunc(colsNeeded)) +
    # scale_colour_manual(values =coloursBSC) +
    theme_BSC() + theme_BSC_trans() +
    labs(y = yLab) + 
    ggtitle(tit)
  
  if(!is.null(points2add)){
    myPlot = myPlot +
      geom_point(data= points2add, aes(x= Date, y= var, fill= group), size= 3) +
      scale_fill_manual(values= colFunc(colsNeeded))
  }
  
  myPlot
}



gg_event <- function(dd, baseGrpName = "MAIN", addOrigin=TRUE, filename="", tit="", x.lab="", y.lab="", iLog=F, write=F, echo=T, trans=T) {
  ## dd: three columns:
  # 1) time vector
  # 2) y values
  # 3) (optional) Grouping
  ### Under development, needs to be generalized
  stopifnot(ncol(dd) %in% c(2:3))
  if(ncol(dd) == 2) dd$group = "MAIN"
  names(dd) = c("x", "y", "group")
  
  # Add a point on the origin for each group
  if(addOrigin){
    dd = dd %>% 
      group_by(group) %>%
      do(add_row(., x=0, y=0, group="NEW", .before = 0)) %>%
      mutate(y = cumsum(y)) %>%
      ungroup() %>%
      mutate(group = ifelse(group == "NEW", lead(group, 1), group)) # Put the correct group name on this new point
  } 
  
  b = dd %>%
    ggplot(aes(x = x, y = y, color = group)) +
    geom_point(size = 3, show.legend = FALSE) +
    geom_line(show.legend = FALSE) + 
    gghighlight(group == baseGrpName, use_group_by = FALSE)
  
  ## General theme and lables
  b = b + expand_limits(x = 0, y = 0) +
    labs(x=x.lab, y=y.lab, title=tit) + theme_BSC() #+ theme(legend.position = "none")
  
  if(iLog) b = b + scale_x_log10()
  if(trans) b = b + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent") 
  if(echo) b
}



gg_event_old <- function(dd, baseGrpName = "MAIN", addOrigin=TRUE, filename="", tit="", x.lab="", y.lab="", iLog=F, write=F, echo=T, trans=T) {
  ## dd: three columns:
  # 1) time vector
  # 2) event boolean
  # 3) (optional) Grouping
  ### Under development, needs to be generalized
  stopifnot(ncol(dd) %in% c(2:3))
  if(ncol(dd) == 2) dd$group = "MAIN"
  names(dd) = c("time", "event", "group")
  
  # Add in counts
  dd = dd %>% group_by(group) %>% mutate(count = 1:n() + cumsum(event))
  
  # Add a point on the origin for each group
  if(addOrigin){
    dd = dd %>% 
      do(add_row(., time=0, count=0, group="NEW", event=FALSE, .before = 0)) %>%
      ungroup() %>% 
      mutate(group = ifelse(group == "NEW", lead(group, 1), group))
  } 
  
  b = dd %>%
    ggplot(aes(x = time, y = count, color = group)) +
    geom_point(size = 3, show.legend = FALSE) +
    geom_line(show.legend = FALSE) + 
    gghighlight(group == baseGrpName, use_group_by = FALSE)
  
  ## General theme and lables
  b = b +
    expand_limits(x = 0, y = 0) +
    labs(x=x.lab, y=y.lab, title=tit) + theme_BSC() #+ theme(legend.position = "none")
  
  if(iLog) b = b + scale_x_log10()
  if(trans) b = b + theme_BSC_trans()
  if(write) ggsave(paste0(filename, ".png"), width= plotWidth, height= plotHeight, units= "mm", dpi= dpiRes, bg = "transparent") 
  if(echo) b
}

