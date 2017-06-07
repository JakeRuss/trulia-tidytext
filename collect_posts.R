
library(httr)
library(rvest)
library(stringr)
library(lubridate)
library(magrittr)
library(dplyr)

# trulia's housing economists
econs <- c("ralph", "cherylyoung", "fchacon", "david")

# number of pages for each writer
pages <- c(12, 2, 2, 2)

# build a data_frame with their page links
page_urls <- data_frame(author = rep(econs, pages)) %>%
  group_by(author) %>%
  mutate(page_num = row_number(),
         page_url = paste0("https://www.trulia.com/blog/author/", author, "/page/", page_num, "/")) %>%
  ungroup()

# Initailize an empty frame to collect the links
post_urls <- data_frame()

# Loop to get all the links
for (i in 1:nrow(page_urls)) {

  doc <- page_urls$page_url[i] %>% read_html()
  
  posts_titles <- doc %>%
    html_nodes("h3") %>%
    html_text() %>%
    str_trim()
  
  posts_links <- doc %>%
    html_nodes(xpath = "//a[@class='block-up']") %>%
    html_attr("href") %>%
    str_c("https://www.trulia.com", .)
  
  posts_themes <- doc %>%
    html_nodes(".cat-title") %>%
    html_text() %>%
    str_trim()
  
  # Collect the pieces into a temporary tibble
  temporary <- data_frame(author     = page_urls$author[i],
                          blog_title = posts_titles,
                          blog_theme = posts_themes,
                          blog_url   = posts_links)

  # Store the results in the post_urls collector
  post_urls <- bind_rows(post_urls, temporary)
  
  # Be polite to Trulia's server; between url calls, 
  # have the scraper rest for a few second.
  Sys.sleep(time = 1)
  
} # Close for loop

# Intialize an empty data frame to collect blog text
raw_post_text <- data_frame()

for (i in 1:nrow(post_urls)) {

  doc <- post_urls$blog_url[i] %>% read_html()

  post_text <- doc %>%
    html_nodes(".dek, .entry") %>%
    html_text() %>%
    str_trim()
  
  post_date <- doc %>%
    html_nodes(".date") %>%
    html_text() %>%
    as.Date(format = "%b %d, %Y")
  
  # Collect the pieces into a temporary tbl df
  temporary <- data_frame(blog_url  = post_urls$blog_url[i],
                          blog_date = post_date,
                          blog_text = post_text) 

  # Store the results in the post_urls collector
  raw_post_text <- bind_rows(raw_post_text, temporary)
  
  # Be polite to Trulia's server; between url calls, 
  # have the scraper rest for a few second.
  Sys.sleep(time = 2)
  
  print(post_urls$blog_url[i])
  
} # Close for loop

# I should probably clean up the text extraction process because in some cases 
# I'm getting multiple rows per blog post. But, (1) I want to be polite to 
# Trulia's server, and (2) I want to start analyzing the text quickly. Which 
# means I will just collapse what I have.

raw_post_text2 <- raw_post_text %>%
  group_by(blog_url, blog_date) %>%
  summarise(blog_text = str_c(blog_text, collapse = "; "))
# Sweet! that gets me my 142 posts

# Merge blog text with post meta data
combined <- post_urls %>%
  left_join(raw_post_text2, by = "blog_url")

# Output data to a CSV
readr::write_csv(combined, "trulia_blog_text.csv")
