## Libraries
library(ggplot2)
library(stringr)
library(ggthemes)
library(circlize)
library(formattable)
library(magick)
library(htmltools)
library(htmlwidgets)
library(webshot)


## https://jokergoo.github.io/circlize_book/book/ about circlize package
## to install phantomjs, used only in flowerlegend function use webshot::install_phantomjs()



## plotting elements and theme updates for flower plots ----
cols <- list(
  light_grey1 = "grey95",
  light_grey2 = "grey90",
  med_grey1 = "grey80",
  med_grey3 = "grey52",
  dark_grey3 = "grey22",
  accent_bright = "maroon"
)
palettes <- list(
  ## may need to update this based on goals in your OHI+ assessment goals.csv
  ## can also select other colorpalette
  ## helpful color selection tools:
  ## https://www.hexcolortool.com/
  ## http://applied-r.com/wp-content/uploads/2019/01/rcolors_byname.png
  goals_pal = tibble::tibble(
    goal = c(
      "MAR","FIS","FP","CW","CON","EUT","TRA",
      "SP","LSP","ICO","LE","ECO","LIV",
      "AO","TR","CS","NP", "BD"
    ),
    color = c(
      "#549dad","#4ead9d","#53b3ac","#89b181","#60a777","#7ead6d","#9aad7e",
      "#97a4ba","#9fb3d4","#7c96b4","#9a97ba","#7d7ca3","#9990b4",
      "#e2de9a","#b6859a","#d0a6a1","#ccb4be","#88b1a6"
    )
  )
)

thm <- list(cols = cols, palettes = palettes)


## NOTE: this will update ggplot default elements so will also affect any other plots created after
## before flowerplotting can save defaults
## old <- theme_get()
## then afterwards can restore these with
## theme_update(old)
theme_update(
  text = element_text(family = "Helvetica", color = cols$dark_grey3, size = 9),
  plot.title = element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = "bold"),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background = element_rect(fill = "transparent", color = NA),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.position = "right",
  axis.line = element_blank(),
  axis.text.y = element_blank()
)



## Flowerplot Functions ----

## from PlotFlower.R from ohicore package
## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier
## find original script in R folder of ohicore github repo (as of Mar 27, 2019)


#' create flowerplot
#'
#' This function combines multple helper functions to create the bhi flower plots
#' requires a dataframe of OHI scores filtered to the region of interest
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll
#' @param rgns vector of region ids for which to create flowerplots; 
#' if not specified, plots for all regions in the dataframe will be generated and saved 
#' but only the final one will be returned
#' @param plot_year year by which to filter region score input dataframe;
#' defaults to current year or maximum year in score data input
#' @param dir_save filepath to the folder where flowerplots should be saved
#' @param plot_config dataframe with order_hierarchy, goal, parent, name_flower, weight  
#' e.g. from OHI conf/goals.csv file
#' @param labels one of "none", "regular", "curved"
#' @param color_by either "goal" or "score"
#' @param color_pal a color palette that will map to score values;
#' a discrete palette if color_by 'goal' and continuous if color_by 'score'
#' @param thm theme object, a list, with additional plotting elements/specifications
#'
#' @return result is the last flower plot generated; if curved labels or legend table are included,
#' the resulting class is magick-image, otherwise the result is a ggplot object

flowerdata <- function(rgn_scores, rgns = NA, plot_year = NA){
  
  ## WRANGLING & CHECKS FOR FLOWERPLOT

  ## filtering/wrangling of rgn_scores for years and dimension
  if(!"year" %in% names(rgn_scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      message("no year column in rgn_scores; assuming data is for current year\n")
    } else {
      message(paste("no year column in rgn_scores; assuming data is for given plot_year", plot_year,"\n"))
    }
    rgn_scores$year <- plot_year
  } else {
    if(is.na(plot_year) | !plot_year %in% unique(rgn_scores$year)){
      plot_year <- max(rgn_scores$year)
      message(paste("plotting using most recent year in the input data:", plot_year,"\n"))
    }
  }
  rgn_scores <- rgn_scores %>%
    dplyr::filter(year == plot_year, dimension == "score")
  
  unique_rgn <- unique(rgn_scores$region_id)
  if(length(unique_rgn) != 1 & is.na(rgns)){
    message("note: rgn_scores input contains data for more than one region, will plot all unless given rgns")
  } else {
    unique_rgn <- rgns
  }
  
  return(list(
    unique_rgn = unique_rgn,
    rgn_scores = rgn_scores,
    plot_year = plot_year
  ))
}

flowerconfig <- function(plot_config, labels = "none", color_by = "goal", color_pal = NA, thm){
  
  ## PLOTTING CONFIGURATION, GENERAL
  
  ## sub/supra goals and positioning ----
  ## pos, pos_end, and pos_supra indicate positions, how wide different petals should be based on weightings
  goals_supra <- na.omit(unique(plot_config$parent))
  
  supra_lookup <- plot_config %>%
    dplyr::filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name_flower)
  
  plot_config <- plot_config %>%
    dplyr::left_join(supra_lookup, by = "parent") %>%
    dplyr::filter(!(goal %in% goals_supra)) %>%
    dplyr::select(order_hierarchy, goal, parent, name_flower, weight, name_supra) %>% 
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::mutate(name_supra = gsub("\\n", "\n", name_supra, fixed = TRUE)) %>%
    dplyr::arrange(order_hierarchy)
  if(labels %in% c("curved", "arc")){
    plot_config <- plot_config %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        f = stringr::str_split(name_flower, "\n"),
        f1 = ifelse(order_hierarchy <= max(plot_config$order_hierarchy)/1.5, f[1],  f[2]),
        f2 = ifelse(order_hierarchy <= max(plot_config$order_hierarchy)/1.5, f[2], f[1])) %>%
      dplyr::select(-f)
  }
  
  color_by <- str_to_lower(color_by)
  if(color_by == "goal"){
    if(length(plot_config$goal) > length(color_pal)){
      color_df <- thm$palettes$goals_pal
      message("no palette given or too few colors for plotting by goal; using a predefined color palette\n")
    } else {
      color_pal <- color_pal[1:nrow(plot_config)*length(color_pal)/nrow(plot_config)]
      color_df <- tibble::tibble(goal = plot_config$goal, color = color_pal)
    }
  }
  
  return(list(
    plot_config = plot_config,
    color_df = color_df,
    color_pal = color_pal
  ))
}

rgnconfig <- function(region_id, rgn_scores, plot_config, color_by = "goal", color_df = NULL){
  
  ## PLOTTING CONFIGURATION, REGION SPECIFIC
  
  r <- region_id
  
  ## if you want subgoal petals to have different weights based on region
  ## can add some code here with a wgts dataframe
  # if(!is.null(wgts)){
  #   plot_config$weight[plot_config$goal == "SUBGOAL1"] <- wgts$weightcolumn[wgts$rgn_id_col == r]
  #   plot_config$weight[plot_config$goal == "SUBGOAL2"] <- 1 - wgts$weightcolumn[wgts$rgn_id_col == r]
  # }
  
  ## join plot_config w scores to create plot_df
  plot_df <- rgn_scores %>%
    dplyr::filter(region_id == r) %>%
    dplyr::inner_join(plot_config, by = "goal") %>%
    dplyr::arrange(order_hierarchy) %>%
    dplyr::mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    dplyr::mutate(pos_end = sum(weight)) %>%
    dplyr::group_by(name_supra) %>%
    dplyr::mutate(pos_supra = ifelse(!is.na(name_supra), mean(pos), NA)) %>% # to deal with unequal weights
    dplyr::ungroup() %>%
    dplyr::filter(weight != 0) %>%
    dplyr::select(-dimension, -region_id, -year, -parent) %>%
    dplyr::mutate(plot_NA = ifelse(is.na(score)|is.nan(score), 100, NA)) # for displaying NAs
  
  if(color_by == "goal"){
    plot_df <- plot_df %>%
      ## for matching colors with correct petals, this arrangment is important...
      dplyr::arrange(goal) %>%
      dplyr::left_join(color_df, by = "goal")
  }
  
  return(plot_df)
}

flowerformatting <- function(region_id, rgn_plot_obj, plot_df, flower_rgn_scores, labels, dir_save, rgn_name = NA){
  
  ## general flowerplot elements ----
  
  r <- region_id
  goal_labels <- dplyr::select(plot_df, goal, name_flower)
  name_and_title <- ifelse(is.na(rgn_name), sprintf("Region%s", r), rgn_name)
  blank_circle_rad <- -42
  
  ## labels, polar coordinates, adjust axes
  p <- rgn_plot_obj +
    labs(x = NULL, y = NULL) +
    ## from linear bar chart to polar
    coord_polar(start = pi * 0.5) + 
    scale_x_continuous(
      labels = NULL,
      breaks = plot_df$pos
    ) +
    scale_y_continuous(limits = c(blank_circle_rad, ifelse(dplyr::first(goal_labels == TRUE)|is.data.frame(goal_labels), 150, 100)))
  ## average value to place in center of plot
  score_index <- flower_rgn_scores %>%
    dplyr::filter(region_id == r, goal == "Index") %>%
    dplyr::select(region_id, score) %>%
    dplyr::mutate(score = round(score))
  
  p <- p +
    ## include central value
    geom_text(
      data = score_index, 
      inherit.aes = FALSE, aes(label = score_index$score),
      x = 0, y = blank_circle_rad,
      hjust = 0.5, vjust = 0.5,
      size = 9,
      color = thm$cols$dark_grey3
    )
  
  
  ## LABELS ----
  ## labeling with sub/supra goal names
  
  ## standard labels
  if(labels %in% c("regular", "standard", "normal", "level")){
    p <- p +
      labs(title = name_and_title$plot_title) +
      geom_text(
        aes(label = name_supra, x = pos_supra, y = 150),
        size = 3.4,
        hjust = 0.4, vjust = 0.8,
        color = thm$cols$med_grey1
      ) +
      geom_text(
        aes(label = name_flower, x = pos, y = 120),
        size = 3,
        hjust = 0.5, vjust = 0.5,
        color = thm$cols$dark_grey3
      )
  }
  
  ## curved labels ----
  if(labels %in% c("curved", "arc", "circle")){
    temp_plot <- file.path(
      dir_save,
      sprintf("flowerplot%sbase_%s.png", r, str_to_lower(str_replace_all(name_and_title, " ", "_")))
    )}
  
  ggplot2::ggsave(
    filename = temp_plot,
    plot = p + theme(plot.background = element_rect(fill = "transparent", colour = NA)),
    device = "png",
    height = 6, width = 8, units = "in", dpi = 300
  )
  
  temp_labels <- file.path(
    dir_save,
    paste0("flower_curvetxt_", str_to_lower(str_replace_all(name_and_title, " ", "_")), ".png")
  )
  ## create curved labels
  circ_df <- plot_df %>%
    # dplyr::mutate(f1 = ifelse(weight <= 0.01, "", f1)) %>%
    # dplyr::mutate(f2 = ifelse(weight <= 0.01, "", f2)) %>%
    dplyr::select("goal", "f1", "f2", "name_supra", "weight", "order_hierarchy") %>%
    dplyr::mutate(weight = 0.15 + weight) %>% # spacing of labels around the circle based on weights...
    dplyr::mutate(x = sum(weight)-(cumsum(weight)-weight), x_end = sum(weight)-(cumsum(weight))) %>%
    tidyr::gather("start_end", "range", -goal, -name_supra, -weight, -order_hierarchy, -f1, -f2) %>%
    dplyr::select(-start_end, -weight, -goal) %>%
    dplyr::arrange(order_hierarchy)
  
  ## start creating plot and save with grDevices function
  # jpeg(temp_labels, width = 2450, height = 2450, quality = 220) # jpeg format is lossy, png seems better...
  png(temp_labels, width = 2490, height = 2490, bg = "transparent")
  message("creating curved labels for plot:\n")
  
  ## curved labels created with 'circlize' package ----
  ## setup/initialize new circlize plot
  circos.clear()
  circos.par(
    "track.height" = 0.1,
    cell.padding = c(0.02, 0, 0.02, 0),
    "clock.wise" = FALSE,
    start.degree = 5
  )
  circos.initialize(
    factors = circ_df$order_hierarchy,
    x = circ_df$range
  )
  
  ## make tracks
  circos.track(
    factors = circ_df$order_hierarchy,
    y = circ_df$range,
    panel.fun = function(x, y){
      circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, col = "white")},
    bg.col = NA,
    bg.border = FALSE
  )
  circos.track(
    factors = circ_df$order_hierarchy,
    y = circ_df$range,
    panel.fun = function(x, y){
      circos.text(
        CELL_META$xcenter,
        CELL_META$ycenter,
        circ_df$f1[circ_df$order_hierarchy == as.numeric(CELL_META$sector.index)][1],
        cex = 5,
        col = thm$cols$med_grey3,
        adj = c(0.4, 1),
        facing = "bending.inside",
        niceFacing = TRUE
      )
    },
    bg.col = NA,
    bg.border = FALSE
  )
  circos.track(
    factors = circ_df$order_hierarchy,
    y = circ_df$range,
    panel.fun = function(x, y){
      circos.text(
        CELL_META$xcenter,
        CELL_META$ycenter,
        circ_df$f2[circ_df$order_hierarchy == as.numeric(CELL_META$sector.index)][1],
        cex = 5,
        col = thm$cols$med_grey3,
        adj = c(0.5, 0),
        facing = "bending.inside",
        niceFacing = TRUE
      )
    },
    bg.col = NA,
    bg.border = FALSE
  )
  
  ## add supra goal labeling
  highlight.sector(
    unique(circ_df$order_hierarchy[circ_df$name_supra == "Food Provision" & !is.na(circ_df$name_supra)]),
    track.index = 1,
    text = "Food Provision",
    cex = 6.5,
    text.col = thm$cols$light_grey2,
    col = NA,
    facing = "bending.outside",
    niceFacing = TRUE
  )
  highlight.sector(
    unique(circ_df$order_hierarchy[circ_df$name_supra == "Coastal Livelihoods & Economies" & !is.na(circ_df$name_supra)]),
    track.index = 1,
    text = "Coastal Livelihoods & Economies",
    cex = 6.5,
    text.col = thm$cols$light_grey2,
    col = NA,
    facing = "bending.outside",
    niceFacing = TRUE
  )
  highlight.sector(
    unique(circ_df$order_hierarchy[circ_df$name_supra == "Sense of Place" & !is.na(circ_df$name_supra)]),
    track.index = 1,
    text = "Sense of Place",
    cex = 6.5,
    text.col = thm$cols$light_grey2,
    col = NA,
    facing = "bending.outside",
    niceFacing = TRUE
  )
  highlight.sector(
    unique(circ_df$order_hierarchy[circ_df$name_supra == "Clean Waters" & !is.na(circ_df$name_supra)]),
    track.index = 1,
    text = "Clean Waters",
    cex = 6.5,
    text.col = thm$cols$light_grey2,
    col = NA,
    facing = "bending.outside",
    niceFacing = TRUE
  )
  dev.off() 
  ## can check the tracks and sectors with circos info
  # circos.info(plot = TRUE)
  
  ## end saving labels image with grDevices function
  
  
  ## combine plot and labels into one graphic ----
  img_text <- magick::image_read(temp_labels)
  img_plot <- magick::image_read(temp_plot)
  plot_labeled <- image_composite(
    img_plot,
    image_scale(img_text, 1810),
    offset = "+305-15"
  ) %>% image_crop(geometry_area(1750, 1620, 325, 120))
  
  ## rescale and save the final plot
  magick::image_write(
    ## can adjust here to make smaller, sacrificing image quality...
    image_scale(plot_labeled, 600), 
    path = file.path(
      dir_save,
      sprintf("flowerplot%s_%s.png", r, str_to_lower(str_replace_all(name_and_title, " ", "_")))
    ),
    format = "png"
  )
  
  return(plot_labeled)
}

flowerplot <- function(rgn_scores, rgns = NA, plot_year = NA, dir_save,
                       plot_config, labels = "none", color_by = "goal", color_pal = NA){
  
  
  ## flowerplot data
  rflowerdata <- flowerdata(rgn_scores, rgns, plot_year)
  
  ## flowerplot configuration
  rflowerconfig <- flowerconfig(plot_config, labels, color_by, color_pal, thm)
  
  ## start looping over regions
  for(r in rflowerdata$unique_rgn){
    ## flowerplot region-specific configuration
    ## note some of these arguments come from flowerdata or flowerconfig functions
    plot_df <- rgnconfig(
      r,
      rflowerdata$rgn_scores,
      rflowerconfig$plot_config,
      color_by,
      rflowerconfig$color_df
    )
    
    ## CREATING THE FLOWERPLOTS ----
    ## without a gradient along the petals
    if(color_by == "goal"){
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = goal)) +
        geom_bar(
          aes(y = 100),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = "white"
        ) +
        geom_bar(
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = plot_df$color, na.value = "black")
      
    } else {
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = score)) +
        geom_bar(
          aes(y = 100),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = "white"
        ) +
        geom_bar(
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          show.legend = FALSE
        ) +
        scale_fill_gradientn(colors = color_pal, na.value = "black", limits = c(0, 100))
    }
    ## overlay light grey background for NAs
    if(any(!is.na(plot_df$plot_NA))){ 
      plot_obj <- plot_obj +
        geom_bar(
          aes(y = plot_NA),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = thm$cols$light_grey1
        )
    }
    plot_obj <- plot_obj +
      ## bolded baseline at zero
      geom_errorbar(
        aes(x = pos, ymin = 0, ymax = 0),
        size = 0.5,
        show.legend = NA,
        color = thm$cols$dark_grey3
      ) +
      ## some kind of tipping-point line? currently zero...
      geom_errorbar(
        aes(x = pos, ymin = 0, ymax = 0), 
        size = 0.25,
        show.legend = NA,
        color = thm$cols$accent_bright
      ) +
      ## outer ring indicating room for even more progress?
      geom_errorbar(
        aes(x = pos, ymin = 109, ymax = 109), 
        size = 5,
        show.legend = NA,
        color = thm$cols$light_grey1
      )
    
    ## format plot
    ## for details see flowerformatting function above
    plotformatted <- flowerformatting(r, plot_obj, plot_df, rflowerdata$rgn_scores, labels, dir_save)
    
    ## making legend to go along with this plot, save in same save location
    legendtab <- flowerlegendtab(r, plot_df, dir_save)
  }
  
  
  ## returns only the last plot...
  return(plotformatted) 
}

flowerwgradient <- function(rgn_scores, rgns = NA, plot_year = NA, dir_save,
                            plot_config, labels = "none", color_by = "goal", color_pal = NA){
  
  
  ## flowerplot data
  rflowerdata <- flowerdata(rgn_scores, rgns, plot_year)
  
  ## flowerplot configuration
  rflowerconfig <- flowerconfig(plot_config, labels, color_by, color_pal, thm)
  
  ## plotting by region
  ## NOTE: will only plot the first region, because otherwise it is very slow!
  ## if you want all the plots, you can run this code for one region at a time...
  r <- rflowerdata$unique_rgn[1]
  message("since plotting with a gradient is so intensive, plotting only for first region!!!\n")
  
  plot_df <- rgnconfig(
    r,
    rflowerdata$rgn_scores,
    rflowerconfig$plot_config,
    color_by,
    rflowerconfig$color_df
  )
  
  ## CREATING THE FLOWERPLOTS ----
  ## if plotting with a gradient, expand plot_df with y column indicating
  ## plot_df and  plot_df0 will be same if no gradient on petals
  plot_df0 <- plot_df
  gradient <- TRUE
  if(isTRUE(gradient)){
    plot_df <- plot_df %>%
      dplyr::mutate(x = pos - (weight/2), x_end = pos + (weight/2)) %>%
      dplyr::mutate(y_end = ifelse(is.na(score), 0, score)) %>%
      dplyr::rowwise() %>%
      ## NOTE
      ## this sequence, together w alpha + size params in geom_segement, create gradient...
      ## you can edit this function to affect line spacing and therefore gradient steepness/intensity...
      dplyr::mutate( 
        y = list(Filter(function(x) x < y_end, 11^(seq(0, 1, 0.001))*10-10))) %>%
      dplyr::mutate(y = if(length(unlist(y)) > 0){list(y)} else {list(0)}) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(y) %>%
      dplyr::mutate(
        name_flower = ifelse(y != 0, NA, name_flower),
        name_supra = ifelse(y != 0, NA, name_supra)) %>%
      dplyr::select(-y_end)
  }
  
  ## CREATING THE FLOWERPLOTS ----
  ## with a gradient
  plot_obj <- ggplot(plot_df, aes(x = x, xend = x_end, y = y, yend = y)) +
    geom_rect(
      inherit.aes = FALSE,
      aes(xmin = x, xmax = x_end, ymin = 0, ymax = 100),
      size = 0.15,
      color = thm$cols$light_grey2,
      fill = "white"
    )
  if(color_by == "goal"){
    plot_obj <- plot_obj +
      geom_segment(
        aes(color = goal),
        size = 0.15, alpha = 0.15,
        show.legend = FALSE,
        arrow = arrow(length = unit(0.01, "cm"))
      ) +
      scale_color_manual(
        values = unique(plot_df$color),
        na.value = "black"
      )
  } else {
    plot_obj <- plot_obj +
      geom_segment(
        aes(color = y),
        size = 0.2, alpha = 0.3,
        show.legend = FALSE,
        arrow = arrow(length = unit(0.02, "cm"))
      ) +
      scale_color_gradient2(
        low = color_pal[1],
        mid = color_pal[length(color_pal)/2],
        high = color_pal[length(color_pal)], midpoint = 50
      )
  }
  ## overlay light grey background for NAs
  if(any(!is.na(plot_df$plot_NA))){
    plot_obj <- plot_obj +
      geom_rect(
        data = dplyr::filter(plot_df, !is.na(plot_NA)),
        inherit.aes = FALSE,
        aes(xmin = x, xmax = x_end, ymin = 0, ymax = plot_NA),
        fill = thm$cols$light_grey1
      )
  }
  plot_obj <- plot_obj +
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 0, yend = 0),
      size = 0.5,
      color = thm$cols$dark_grey3
    ) +
    ## could replace zero with some other critical value, tipping-point type threshold...
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 0, yend = 0),
      size = 0.1,
      color = thm$cols$accent_bright
    ) +
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 109, yend = 109),
      size = 5,
      color = thm$cols$light_grey1
    )
  
  ## format plot
  ## for details see flowerformatting function above
  plotformatted <- flowerformatting(r, plot_obj, plot_df0, rflowerdata$rgn_scores, labels, dir_save)
  
  
  ## making legend to go along with this plot, save in same save location
  legendtab <- flowerlegendtab(r, plot_df0, dir_save)
  

  return(plotformatted)
}

flowerlegendtab <- function(region_id, plot_df, dir_save){
 
  ## creating a legend table to accompany flowerplot
  message("creating legend table for flowerplot:\n")

  temp_legend <- file.path(
    dir_save,
    sprintf("flowerlegend_by_%s_region%s.jpg", color_by, r)
  )
  
  ## formattable dataframe ----
  legend_df <- plot_df %>%
    dplyr::mutate(name = str_replace_all(name_flower, "\n", " ")) %>% 
    dplyr::mutate(name = str_replace_all(name, "- ", "")) %>% 
    dplyr::select(goal, name, name_supra, score, color)
  
  
  if(color_by == "goal"){
    legend_cols <- legend_df$color
  } else {
    legend_cols <- dplyr::mutate(
      legend_df, color = color_pal[length(color_pal)*score/100]
    )$color
  }
  
  legend_cols[is.na(legend_cols)] <- "#DDDDDD"
  
  legend_df <- legend_df %>%
    dplyr::mutate(
      Key = "*******",
      Goal = dplyr::case_when(
        is.na(name_supra) ~ sprintf("%s ( %s )", name, goal),
        !is.na(name_supra) ~ sprintf("%s: %s ( %s )", name_supra, name, goal)
       )
    ) %>%
    dplyr::select(Goal, Key, score)
  
  names(legend_df) <- c("Goal", "Key", "Score")
  
  
  ## formattable formatted table ----
  tab <- formattable(
    legend_df,
    align = c("l","l","r"),
    
    list(
      `Goal` = formatter("span", style = ~ style(color = "grey")),
      `Score` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "white", "grey"))),
      `Key` = formatter("span", style = x ~ style("background-color" = legend_cols, color = ifelse(is.na(x), "#DDDDDD", legend_cols)))
    ))
  
  ## NOTE: for the following to work phantomjs must be installed!
  ## this can be done with webshot::install_phantomjs()
  path <- htmltools::html_print(
    as.htmlwidget(tab, width = "48%", height = NULL),
    background = "white",
    viewer = NULL
  )
  
  ## webshot and save widget
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot::webshot(
    url,
    file = temp_legend,
    selector = ".formattable_widget",
    delay = 0.2
  )
  img_legend <- magick::image_read(temp_legend) %>%
    magick::image_border("white", "20x92") %>%
    magick::image_scale(600)
  
  return(img_legend)
}
