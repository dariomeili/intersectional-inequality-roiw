clean_latex_table <- function(latex_table) {
  # Replace longtable with tabular
  latex_table <- gsub("\\\\begin\\{longtable\\}", "\\\\begin\\{tabular\\}", latex_table)
  latex_table <- gsub("\\\\end\\{longtable\\}", "\\\\end\\{tabular\\}", latex_table)
  
  # Remove \begingroup and \endgroup commands
  latex_table <- gsub("\\\\begingroup", "", latex_table)
  latex_table <- gsub("\\\\endgroup", "", latex_table)
  
  # Remove font size adjustments
  latex_table <- gsub("\\\\fontsize\\{[^\\}]+\\}\\{[^\\}]+\\}\\\\selectfont", "", latex_table)
  
  # Remove \setlength command related to longtable
  latex_table <- gsub("\\\\setlength\\{\\\\LTpost\\}\\{[^\\}]+\\}", "", latex_table)
  
  # Match the entire minipage section (even if it spans multiple lines)
  latex_table <- gsub("\\\\begin\\{minipage\\}\\{[^\\}]+\\}([\\s\\S]*?)\\\\end\\{minipage\\}", "", latex_table, perl = TRUE)
  
  # Return the cleaned LaTeX table
  return(latex_table)
}
