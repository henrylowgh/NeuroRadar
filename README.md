# NeuroRadar
An automated tool that enables dynamic classification and analysis of neurotransmitter receptor trafficking events, including those associated with AMPA receptors (AMPAR), in a live imaging context

<a href="https://zenodo.org/badge/latestdoi/632710370"><img src="https://zenodo.org/badge/632710370.svg" alt="DOI"></a>

### MIT License 

![img (composite)](https://user-images.githubusercontent.com/131828718/235338077-254c0684-7d7b-4d78-a8f0-6a2859c08926.png)

# To Download:
```
git clone https://github.com/henryghlow/NeuroRadar
```
 
# To Use:

This tool that can be implemented in RStudio allows for the automated analysis of puncta changes in neuronal live imaging. By inputting ROI intensity data collected in ImageJ, one can use this tool to analyze and batch process puncta data. Using linear regression techniques, the tool enables classification of underlying neurotransmitter receptor trafficking patterns and attempts to classify according to several archetypal event types: 
 
### Classes
Type 1: Very short-duration transient puncta intensity increase with large spike (< 10 seconds)

Type 2: Medium-duration puncta intensity transient increase (returns to baseline within 50-100 seconds)

Type 3: Sudden sustained puncta intensity increase (within 30 seconds)

Type 4: Gradual sustained puncta intensity increase

Type 5: Gradual puncta intensity increase followed by sudden decrease (within 30 seconds)

Type 6: Gradual sustained puncta intensity decrease

Other: No significant change / Inconclusive pattern
