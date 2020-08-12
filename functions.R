mapshot <- function(x,
                    url = NULL,
                    file = NULL,
                    remove_url = TRUE,
                    remove_controls = c("zoomControl",
                                        "layersControl",
                                        "homeButton",
                                        "scaleBar"),
                    ...) {
  
  ## if both 'url' and 'file' are missing, throw an error
  avl_url <- !is.null(url)
  avl_file <- !is.null(file)
  
  if (!avl_url & !avl_file)
    stop("Please provide a valid 'url' or 'file' argument (or both).")
  
  ## if a 'mapview' object is supplied, extract map
  if (inherits(x, "mapview")) {
    x <- mapview2leaflet(x)
  }
  
  if (avl_file & !avl_url) {
    for (i in remove_controls) {
      x = removeMapJunk(x, i)
    }
  }
  
  ## if url is missing, create temporary .html file
  if (!avl_url)
    url <- gsub(tools::file_ext(file), "html", file)
  # url <- gsub("\\.png|\\.pdf|\\.jpeg|\\.jpg", ".html", file)
  
  if (dir.exists(file.path(tempdir(), "popup_graphs"))) {
    file.copy(from = file.path(tempdir(), "popup_graphs"),
              to = paste0(dirname(url), "/"), recursive = TRUE)
    file.copy(from = file.path(dirname(url),  "popup_graphs"),
              to = paste0(dirname(dirname(url)), "/"), recursive = TRUE)
    unlink(file.path(dirname(url),  "popup_graphs"), recursive = TRUE)
  }
  
  args <- list(url = url, file = file, ...)
  sw_ls <- args
  sw_ls[names(sw_ls) == "file"] <- NULL
  names(sw_ls)[which(names(sw_ls) == "url")] <- "file"
  sw_args <- match.arg(names(sw_ls),
                       names(as.list(args(htmlwidgets::saveWidget))),
                       several.ok = TRUE)
  
  do.call(htmlwidgets::saveWidget, append(list(x), sw_ls[sw_args]))
  
  ## save to file
  
  if (avl_file) {
    url_tmp = gsub(".html", "_tmp.html", url)
    sw_ls[which(names(sw_ls) == "file")] = url_tmp
    args$url = url_tmp
    # names(sw_ls)[which(names(sw_ls) == "url")] <- "file"
    x_tmp = x
    for (i in remove_controls) {
      x_tmp = removeMapJunk(x_tmp, i)
    }
    do.call(htmlwidgets::saveWidget, append(list(x_tmp), sw_ls[sw_args]))
    ws_args <- match.arg(names(args),
                         names(as.list(args(webshot::webshot))),
                         several.ok = TRUE)
    do.call(webshot::webshot, args[ws_args])
  }
  
  ## if url was missing, remove temporary .html file
  if (!avl_url & remove_url) {
    url_files = paste0(tools::file_path_sans_ext(url), "_files")
    unlink(c(url, url_files), recursive = TRUE)
    url_tmp_files = paste0(tools::file_path_sans_ext(url_tmp), "_files")
    unlink(c(url_tmp, url_tmp_files), recursive = TRUE)
  }
  
  return(invisible())
}



removeMapJunk = function(map, junk) {
  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  switch(
    junk,
    "zoomControl" = removeZoomControl(map),
    "layersControl" = leaflet::removeLayersControl(map),
    "homeButton" = removeHomeButtons(map),
    "scaleBar" = removeScalebar(map),
    NULL = map
  )
}

removeZoomControl = function(map) {
  map$x$options = append(map$x$options, list("zoomControl" = FALSE))
  return(map)
}

removeHomeButtons = function(map) {
  hb_ind = getCallEntryFromMap(map, "addHomeButton")
  map$x$calls[hb_ind] = NULL
  return(map)
}

removeScalebar = function(map) {
  sb_ind = getCallEntryFromMap(map, "addScaleBar")
  map$x$calls[sb_ind] = NULL
  return(map)
}


