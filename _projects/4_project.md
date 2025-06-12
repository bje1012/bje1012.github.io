---
layout: page
title: Text Mining with R
description: Text mining using R selenium
img: assets/img/textmining.png
importance: 1
category: Others
giscus_comments: false
---

This page is just a document I worked on for fun in my free time. It explains text mining using R and Selenium and analyzes reviews of Titane, a film by director Julia Ducournau, which is one of my favorites. The analysis includes sentiment analysis and an examination of frequently used emotionally expressive words.

First, load the required packages:

{% raw %}
```{r}
library(multilinguer)
library(rJava)
rJava::.jinit()
pacman::p_load(gt, rvest, tidyverse, tuber, httpuv, dplyr, tidytext, stringr)
library(RSelenium)
library(tidyverse)
library(quanteda)
library(kableExtra)
```
{% endraw %}

Then, run Geckodriver to start Selenium.

You should download the `geckodriver.exe` file, Selenium server, and ChromeDriver (if you're using Chrome to launch the browser), and save all of them in the same directory where you run this code.

Enter the code below in the terminal to run Geckodriver (In the terminal, make sure your current location is the same folder where all the files are saved): 
`java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445`

{% raw %}
```{r}
remDr <- remoteDriver(
  remoteServerAddr = 'localhost',  # Address of the Selenium server (localhost means it's running on your machine)
  port = 4445,                     # Port where the Selenium server is listening
  browserName = "chrome"          # The browser to use for automation
)

# Start a browser session
remDr$open()
```
{% endraw %}

The next step is to set the URL of the page I want to access. In this case, it’s the review page for Titane on Rotten Tomatoes.

{% raw %}
```{r}
# The URL address that I want to access
url <- "https://www.rottentomatoes.com/m/titane/reviews"  # URL of the movie review page

# Navigate to the specified URL using Selenium
remDr$navigate(url)  # Direct the browser to open the given URL
```
{% endraw %}


Since the page requires clicking the "Load More" button to view additional reviews, I need to set up a loop to handle this automatically. Once the loop is written, I no longer have to click the button manually to scrape the reviews.

Since the page is overflowing with reviews, I capped the number of clicks at 10 — just enough to get the data I need without sending my computer into a meltdown or accidentally scraping until the end of time. 

{% raw %}
```{r}
# Set a limit on how many times to click the "Load More" button
max_clicks <- 10     # Maximum number of times to try clicking the "Load More" button
clicks <- 0          # Initialize the click counter

# Start a repeat loop to load more reviews by clicking the "Load More" button
repeat {
  # Exit the loop if the maximum number of clicks has been reached
  if (clicks >= max_clicks) {
    message("Reached maximum number of clicks. Exiting loop.")  # Print a message to the console
    break  # Stop the loop
  }

  # Try to locate the "Load More" button using a CSS selector
  load_more_btn <- tryCatch({
    remDr$findElement(using = "css selector", "rt-button[data-LoadMoreManager='btnLoadMore:click']")
  }, error = function(e) NULL)  # If the button is not found, return NULL

  # If no "Load More" button is found, assume all reviews are loaded
  if (is.null(load_more_btn)) {
    message("All comments loaded. Exiting loop.")  # Print a message
    break  # Stop the loop
  }

  # Try clicking the button and wait briefly
  tryCatch({
    load_more_btn$clickElement()  # Click the "Load More" button
    Sys.sleep(2)                  # Wait 2 seconds for new content to load
    clicks <- clicks + 1          # Increment the click counter
  }, error = function(e) {
    message("Failed to click the button. Exiting loop.")  # If an error occurs, print message
    break  # Stop the loop
  })
}

# Get the HTML source of the fully loaded page
page_source <- remDr$getPageSource()[[1]]  # Retrieve the HTML source code
html_content <- read_html(page_source)     # Parse the HTML content

# Extract reviewer names from the HTML
reviewer_names <- html_content %>%
  html_nodes(".reviewer-name-and-publication .display-name") %>%  # Select reviewer name elements
  html_text(trim = TRUE)  # Extract and trim the text

# Extract review texts from the HTML
review_texts <- html_content %>%
  html_nodes(".review-text") %>%  # Select review body elements
  html_text(trim = TRUE)  # Extract and trim the text

# Combine reviewer names and reviews into a data frame
reviews_data <- data.frame(
  Reviewer = reviewer_names,  # Column for reviewer names
  Review = review_texts,      # Column for review texts
  stringsAsFactors = FALSE    # Keep character strings as-is (not factors)
)
```
{% endraw %}

Okay! Here's a table showing the first 5 rows of the reviews. Looks like the loop did its job nicely.

{% raw %}
```{r}
# The first 5 rows of the collected review data
Table <- head(reviews_data, 5)

# Show the first 5 rows as a nicely formatted table
Table %>%
  kable() %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover"))
```
{% endraw %}

<div class="row">
    <div class="col-sm mt-3 mt-md-0">
        {% include figure.liquid loading="eager" path="assets/img/cap1.png" title="example image" class="img-fluid rounded z-depth-1" style="max-width: 80%; height: auto;" %}
    </div>
</div>

The next step is sentiment analysis. I used the LSD2015 dictionary to get the job done.

{% raw %}
```{r}
# Load review texts into a quanteda corpus object from the 'Review' column
reviews_corpus <- corpus(reviews_data,
                         text_field = "Review")

# Perform dictionary-based sentiment analysis using the LSD2015 dictionary:
# 1. Tokenize the corpus into words
# 2. Look up tokens in the sentiment dictionary
# 3. Create a document-feature matrix (dfm)
# 4. Convert the dfm into a standard data.frame for easier manipulation
reviews_sentiment <- tokens(reviews_corpus) |>
  tokens_lookup(data_dictionary_LSD2015) |>
  dfm() |>
  convert(to = "data.frame")

# Compute a log-based sentiment score:
# (positive + neg_negative) / (negative + neg_positive)
# where 'neg_negative' are words with negative valence used in a positive context,
# and 'neg_positive' are words with positive valence used in a negative context.
# 0.5 is added to numerator and denominator for smoothing (Laplace correction).
reviews_sentiment <- reviews_sentiment |>
  mutate(sentiment =
           log((positive + neg_negative + 0.5) /
                 (negative + neg_positive + 0.5)))
```
{% endraw %}

{% raw %}
```{r}
ggplot(reviews_sentiment, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white", boundary = 0) +
  geom_vline(aes(xintercept = mean(sentiment)), color = "red", linetype = "dashed") +
  labs(
    title = "",
    x = "\nSentiment Score (log ratio)",
    y = "Number of Reviews\n"
  ) +
  theme_minimal()
```
{% endraw %}

<div class="row">
    <div class="col-sm mt-3 mt-md-0">
        {% include figure.liquid loading="eager" path="assets/img/cap2.png" title="example image" class="img-fluid rounded z-depth-1" style="max-width: 80%; height: auto;" %}
    </div>
</div>

The graph shows that most reviews are clustered around neutral, with a slight lean toward negativity overall. 

To better understand this, I also tokenized the reviews and created a plot to show word frequencies.

{% raw %}
```{r}
# Tokenize the reviews into individual words (1 word per row)
ut1 <- reviews_data %>%
  unnest_tokens(input = Review, output = word, token = "words")

# Extract word lists from the LSD2015 dictionary
lsd_words <- unlist(data_dictionary_LSD2015) |> unique()

# Only include the words from the dictionary
cf <- ut1 %>%
  count(word, sort=T) %>%
  filter(word %in% lsd_words)

# Select the top 20 most frequent content words
top <- head(cf, 20)

# Plot a horizontal bar chart showing word frequencies
ggplot(top, aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity") +      # Draw bars
  coord_flip() +                     # Flip axes for readability
  labs(title = "",     # Add title and axis labels
       x = "Word\n",
       y = "\nFrequency") +
  theme_minimal()                    # Use a clean minimal theme
```
{% endraw %}

<div class="row">
    <div class="col-sm mt-3 mt-md-0">
        {% include figure.liquid loading="eager" path="assets/img/cap3.png" title="example image" class="img-fluid rounded z-depth-1" style="max-width: 80%; height: auto;" %}
    </div>
</div>

It seems the summary leans slightly negative because the film includes shocking scenes and conveys strong emotions like pain.

After finish the analysis, don't forget to close the browser!

{% raw %}
```{r}
remDr$close()
```
{% endraw %}

This little project was just a fun way for me to try out some text mining and sentiment analysis using R and Selenium. By scraping and analyzing reviews of Titane, I got a glimpse into how people responded emotionally to the film. It was interesting to see how certain words stood out and how patterns emerged from the data. Hope you found this walk-through interesting or helpful in some way!


