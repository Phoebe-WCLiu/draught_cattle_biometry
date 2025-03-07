FROM rocker/rstudio:4.3.3

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# Install R packages
RUN R -e "install.packages(c('ggplot2', 'ggstar', 'dplyr', 'ggthemes','tibble','factoextra','FactoMineR'))"

# Set directory
WORKDIR /home/rstudio/Biometry

# Copy the entire Biometry folder
COPY . /home/rstudio/Biometry


EXPOSE 8787

# Run RStudio
CMD ["/init"]

# Instructions :
# 1. Build the Docker image using the following command in the terminal:
#                docker build -t biometry-project .

# 2. To run the container with a password, use this command        in the terminal:
#                docker run -p 8787:8787 -v $(pwd):/home/rstudio/Biometry -e PASSWORD=rstudio biometry-project

# 3. Go to http://localhost:8787 in the browser
#                Use the username 'rstudio' and the password 'rstudio' to log in to RStudio.
