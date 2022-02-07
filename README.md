# covid-by-county
R code to visualize and analyze Covid cases by US county. Data comes from NYTimes Covid dataset https://github.com/nytimes/covid-19-data.

**Example:**
`Rscript covid.R --set=USA --lag=21 --output_dir=.`
Generates plots for the counties in the "USA" set, with a lag of 21 days (so that new deaths lines up with new cases), and writes the image to the current directory.
