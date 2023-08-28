# Spotify Charts Network
This is the repository for analysing the networks of artists who appear in Spotify charts. Please read the following to gain a first overview of the GitHub repo.
## Scripts
### Preparation
[Data preparation in R](https://github.com/simonweigold/spotify-charts-network/blob/main/scripts/preparation.R)  
[API access for genre information](https://github.com/simonweigold/spotify-charts-network/blob/main/scripts/genres)  
[API access for popularity information](https://github.com/simonweigold/spotify-charts-network/blob/main/scripts/popularity.py)  
### Analysis

### Helper code

## Todos
- [x] set up project infrastructure (GitHub repository + local machine)
- [x] Data preparation
  - [x] Data collection (Kaggle)
  - [x] Data preprocessing
  - [x] Transform network object
- [x] Analysis
  - [x] Exploratory data analysis
  - [x] Hypothesis 1
    - [x] Calculate centrality measures
    - [x] Perform regression (dependent variable = streams; independent variables = centrality measures)
  - [x] Hypothesis 2
    - [x] Create genre subgraphs
    - [x] Calculate centrality measures
    - [x] Perform ANOVA on avg number of streams between different genres + whole network
- [x] Write report
