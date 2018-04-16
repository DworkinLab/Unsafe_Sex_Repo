## Summary of mating data collected

Collected by Abhijna Parigi at Michigan State University (MSU)

### Brief Methods:
  
  Wild *Drosophila melanogaster* were collected in Southwest Michigan and split into 12 populations, each designated a treatment. Four populations were controls with no predators present, four populations were continuously exposed to ~30 zebra jumping spiders and four populations were continuously exposed to ~30 1st instar Chinese praying mantids.
  
  Virgin males and virgin females were collected and placed into 4 age categories (age bins by days old: 1) 1-3, 2) 4-7, 3) 8-11 and 4) 12-15). After being placed in a vial, the mating pair was recorded for the first instance of courtship, copulation start and copulation end times. Ten pairs for each treatment-population-age bin was completed, for approximately 360 total pairs analyzed.
  
### Prediction: 


### Data Layout:
```
Vial --> Individual replicates
Treatment --> Evolved Treatment (Spider, Mantid or control)
Rep --> Replicate populaton (1-4)
Rel_Court_lat --> Relative Courtship Latency
Rel_Cop_lat --> Relative Copulaton Latency
Rel_Cop_dur --> Relative Copulation Duration
Copulation --> 0==no copulation, 1== copulation
Date --> Date
AgeBin --> Age Bin (1-4, corresponding to age range)
Treatment.Rep --> Treatment and Replicate together
Treatment2 --> Treatment Short form
```