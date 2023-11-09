# Load necessary libraries
library(xml2)

# Define the base URL of your website
base_url <- "https://mmiots9.github.io/R-for-biologist/"

# List all R Markdown files in the book directory
rmd_files <- list.files(".", pattern = "\\.Rmd$", full.names = TRUE)

# Create a new XML document
sitemap <- xml2::xml_new_document()

# Add the root element to the XML document
root_node <- xml2::xml_add_child(sitemap, "urlset", 
                                 xmlns = "http://www.sitemaps.org/schemas/sitemap/0.9")

# Iterate over each R Markdown file and add it to the sitemap
for (rmd_file in rmd_files) {
  url <- paste0(base_url, tools::file_path_sans_ext(basename(rmd_file)), "/")
  
  # Create a new 'url' element for each R Markdown file
  url_node <- xml2::xml_add_child(root_node, "url")
  
  # Add 'loc' and 'changefreq' child elements to the 'url' element
  xml2::xml_add_child(url_node, "loc", url)
  xml2::xml_add_child(url_node, "changefreq", "daily")
}

# Save the sitemap to a file
xml2::write_xml(sitemap, "sitemap.xml")
xml2::write_xml(sitemap, "docs/sitemap.xml")
