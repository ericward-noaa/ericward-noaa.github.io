# Shared helpers for turning myCV.bib into formatted reference strings.
# Used by ward_cv_markdown.Rmd (the PDF CV) and build_publications.R (the
# website's Publications page), so both always format entries the same way.

library(RefManageR)

# Read the .bib file and sort entries by year, then month (oldest first).
read_sorted_bib = function(file = "myCV.bib") {
  BibOptions(restore.defaults = TRUE)
  biblio = ReadBib(file = file)

  BibOptions(sorting = "ydnt", match.date = 'year.month')
  biblio = sort(biblio, decreasing = TRUE)

  # overwrite default sorting, to sort by year and month
  df = data.frame(years = rep(NA, length(biblio)),
  months = rep(NA, length(biblio)))
  for(i in 1:nrow(df)) {
    df$years[i] = biblio[[i]]$year
    if(!is.null(biblio[[i]]$month)) {
      df$months[i] = biblio[[i]]$month
    }
  }
  df$months = match(df$months, c("jan","feb","mar","apr","may","jun",
    "jul","aug","sep","oct","nov","dec"))
  df$order = 1:nrow(df)
  for(y in unique(df$years)) {
    indx = which(df$years==y)
    df$order[indx] = df$order[indx][sort(df$months[indx], index.return=TRUE)$ix]
  }
  biblio = biblio[[df$order]]
  biblio
}

parse_authors = function(s) {
  # first author
  s_init = strsplit(as.character(s[[1]]), " ")[[1]]
  s_init_last = s_init[length(s_init)]
  s_init_str = paste0(s_init_last, ", ")
  for(i in 1:(length(s_init)-1)) {
    s_init_str = paste0(s_init_str, substr(s_init[i],1,1), ".")
  }
  if(s_init_last == "Ward" & substr(s_init[1],1,1) == "E") {
    s_init_str = paste0("**",s_init_str,"**")
  } 
  s_str = s_init_str
  
  if(length(s) > 1) {
    # multiple authors
    for(j in 2:length(s)) {
      # parse name first
      s_init = strsplit(as.character(s[[j]]), " ")[[1]]
      s_init_last = s_init[length(s_init)]
      s_init_str = ""
      for(i in 1:(length(s_init)-1)) {
        s_init_str = paste0(s_init_str, substr(s_init[i],1,1), ".")
      }
      s_init_str = paste0(s_init_str, " ", s_init_last)
      if(s_init_last == "Ward" & substr(s_init[[1]],1,1) == "E") {
        s_init_str = paste0("**",s_init_str,"**")
      } 
      s_str = ifelse(j != length(s), paste0(s_str, ", ", s_init_str),
      paste0(s_str, ", and ", s_init_str))
    }
  }
  return(s_str)
}

parse_title = function(s) {
  # remove bibtex formats
  s = gsub("\\textendash{}", "-", s, fixed=TRUE)
  words = strsplit(s, " ")[[1]]
  str = words[1]
  for(i in 2:length(words)) {
    words[i] = tolower(words[i])
    str = paste(str, words[i])
  }
  # find locations of words' first characters
  init_pos = as.numeric(gregexpr(" ", str, fixed=TRUE)[[1]]) + 1
  # also include words right in a bracket
  init_pos = c(init_pos, as.numeric(gregexpr("{{", str, fixed=TRUE)[[1]]) + 2)
  # find locations of brackets. any words in this range are in caps
  bracket_pos = cbind(as.numeric(gregexpr("{{", str, fixed=TRUE)[[1]]),
    as.numeric(gregexpr("}}", str, fixed=TRUE)[[1]]))
  for(i in 1:nrow(bracket_pos)) {
    for(j in 1:length(init_pos)) {
      if(init_pos[j] > as.numeric(bracket_pos[i,1]) & init_pos[j] < as.numeric(bracket_pos[i,2])) {
        beg = substr(str, 1, init_pos[j]-1)
        mid = substr(str, init_pos[j], init_pos[j])
        end = substr(str, init_pos[j]+1, nchar(str))
        str = paste0(beg,toupper(mid),end)
      }
    }
  }
  
  # remove curly brackets from string
  str = gsub("\\{", "", str)
  str = gsub("\\}", "", str)
  return(str)
}


# Format entry n of a sorted bib as a markdown reference (no number prefix).
format_ref = function(biblio, n) {
  author_str = parse_authors(biblio[[n]]$author)
  title_str = parse_title(biblio[[n]]$title)

  doi_str = biblio[[n]]$doi
  # some .bib entries store the full URL (https://doi.org/...), others the bare DOI
  if(!is.null(doi_str)) doi_str = sub("^https?://(dx\\.)?doi\\.org/", "", doi_str)
  doi_str = ifelse(is.null(doi_str), "", paste0("[link](https://doi.org/", doi_str, ")"))
  author_per = paste0(author_str, ". ")
  if(author_str == "**Ward, E.J.**") author_per = "**Ward, E.J. **"
  paste0(author_per,
    biblio[[n]]$year, ". ", title_str, ". ",
    "*", biblio[[n]]$journal, "*, ", biblio[[n]]$volume, ":",
    biblio[[n]]$pages, ". ", doi_str)
}
