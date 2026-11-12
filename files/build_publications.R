# Regenerates _pages/publications.md (the website's Publications page) from
# myCV.bib, using the same formatting as the CV.
#
# Run from the files/ folder:   Rscript build_publications.R
# (GitHub Actions also runs this automatically on every deploy.)

source("bib_helpers.R")
biblio = read_sorted_bib("myCV.bib")

header = c(
  "---",
  'title: "Publications (peer-reviewed)"',
  "permalink: /publications/",
  "author_profile: true",
  "---",
  "",
  "<!-- This page is generated from files/myCV.bib by files/build_publications.R.",
  "     Edit the .bib file rather than this page; changes here will be overwritten. -->",
  ""
)

# Newest first; the list is displayed in reverse numbering (newest = highest number)
refs = vapply(length(biblio):1, function(n) paste0("0. ", format_ref(biblio, n)), "")
# the CV's LaTeX needs "**Ward, E.J. **" for single-author papers; the web page doesn't
refs = gsub("**Ward, E.J. **", "**Ward, E.J.** ", refs, fixed = TRUE)

# the {: reversed} tag must sit directly under the last item (no blank line)
writeLines(c(header, paste(refs, collapse = "\n\n"), '{: reversed="reversed"}'),
           "../_pages/publications.md", useBytes = TRUE)
cat("Wrote", length(refs), "publications to _pages/publications.md\n")
