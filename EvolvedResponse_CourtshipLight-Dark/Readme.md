# Summary of courtship data for evolved courtship in light vs. dark

### Brief methods used: 

  Using populations of *Drosophila melanogaster* that have undergone one of three treatments: exposure to zebra jumping spiders, exposure to Chinese praying mantids, or controls (no predatory exposure).
  
  Using one male (aged ~24 hours) with two immature females (less
than 20 hour old).

  Recordings under red light for courtship times for night time mating displays, and under normal ceiling laboratory lights, the daytime courtship times was recorded
  
  In 15 minutes sessions, the proportion of time spent courting for each male was calculated for 187 mating pairs (~60 per treatment) over 5 days of experiments.

### Prediction: 
  
  A higher ratio of night-time to daytime courtship in the predation selection lines compared to the control populations.
  
### Layout of Data:

```
Day Phase Vial Observer C_dur     P_court Time Session Trt Population
 1  Dark    1        R     3 0.003333333    8       1   C          1
 1  Dark    2        A    28 0.031111111    8       1   P          1
 1  Dark    3        A    88 0.097777778    8       1   S          1
 1  Dark    4        A     0 0.000000000    8       1   C          2
 1  Dark    5        R     9 0.010000000    8       1   P          2
 1  Dark    6        R   113 0.125555556    8       1   S          2
```
  Day == Day of trial
  
  Phase == light or Dark; corresponds to Time (start time) of 8 am (dark, red light) or 11 am (light, room lighting)
  
  Vial == Different recoreded vial of the day
  
  Observer == two observers A and R
  
  C_dur == Courtship duration in the session time (15 minutes or 900 seconds)
  
  P_court == proportion of time courting in session time (C_dur/900)
  
  Time == Start time
  
  Session == Session of recording: block within the hour of recording (3 sessions of 15 minutes?) with 2 replicates of each population is recorded over 3 sessions
  
  Trt == Predation selection treatment
  
  Population == Replicate population
  
  