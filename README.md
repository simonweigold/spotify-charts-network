# spotify-charts-network
this is the repository for analysing the networks of artists who appear in Spotify charts

## Todos
- [x] set up project infrastructure (GitHub repository + local machine)
- [x] Data preparation
  - [x] Data collection (Kaggle)
  - [x] Data preprocessing
  - [x] Transform network object
- [ ] Analysis
  - [ ] Exploratory data analysis
  - [ ] Hypothesis 1
    - [ ] Calculate centrality measures
    - [ ] Perform regression (dependent variable = streams; independent variables = centrality measures)
  - [ ] Hypothesis 2
    - [ ] Create genre subgraphs
    - [ ] Calculate centrality measures
    - [ ] Perform ANOVA on avg number of streams between different genres + whole network
    - [ ] Perform ANOVA on avg centrality measures between different genres + whole network
    - [ ] Perform regression (dependent variable = streams; independent variables = centrality measures of genre subgraphs); compare to results from H1 regression
- [ ] Write report
