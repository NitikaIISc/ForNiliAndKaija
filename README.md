# ForNiliAndKaija
Nitika's codes for extracting movement data from Movebank and step-by-step creating interaction networks in coflying, coroosting and cofeeding situations 
along with calculating movement measures such as roost fidelity i.e. which vulture sleeps in which cliff and with whom, the maximum displacement on each day and 
average flight time.
The extraction of movement data for network analysis involves multiple steps:
>identifying the time period and extracting only a subset of movebank data based on the season under study.
>checking for errors in the large dataset as well as removing data that is not needed or faulty
>"geo-fencing" to in and around Israel
>only including individuals that have been recorded for a third of the study duration ~70 days
>identifying relocations of different individuals that occered within 10 minutes of each other and within a diatnce threshold (that varies based on the social situation i.e.
co-flying, co-roosting and co-feeding)
>create interaction networks as a template for ALL possible interactions that could've happened (i.e. when the vultures were located) as well as those that were 'observed' resulting 
in a simple ratio index (SRI)
> make sure the networks are non-directed
>export KMLs or save CSVs
> use this to create beautiful network visualizations by color-coding/ size-coding/ shape-coding for various attributes of the individuals.
