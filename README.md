# Abalone-dataset

I have extensively leveraged R's diverse set of packages, including gmodels, caret, psych, and class, to conduct a comprehensive analysis on a dataset comprising 4177 rows. The dataset focuses on predicting the age of abalones through the incorporation of eight distinct physical measurements. 

Through this analytical exploration, I aimed to derive meaningful insights and facilitate a deeper understanding of the underlying patterns within the data.

During the preprocessing phase of our dataset, a discerning observation led me to conclude that the "Sex" feature does not contribute substantial value to our analysis (shown below figure). Consequently, I have opted to drop this column before proceeding with any further data processing steps. This strategic decision aims to streamline the dataset and focus our efforts on the most relevant and impactful features for predicting the age of abalones.

<img width="431" alt="image" src="https://github.com/rashitiwary/Abalone-dataset/assets/117954453/8b85b470-a607-4b5e-9ced-86cb233b527f">

Post the pre-processing of the dataset, we apply K-means and KNN to compare our results through accuracy as our metric. 
