# Identifying key traits in Hawaiian fish to predict risk of extinction 
### EDS 222 Statistics for Environmental Data Science Final Project

## Summary
This is the original code repository for the following blog post and my final project for a statistics course at the Bren School of Envrionmental Science and Management taught by Tamma Carleton (completed in early December 2022). I investigate Hawaiian fish ecological traits – such as size, endemism, and reef-association – to find their probability of being threatened as ranked by the IUCN Red List. 

[Check out the blog!](https://elkewind.github.io/posts/2022-12-02-hawaiian-fish-analysis/)

## Background
Global human activity threatens many species with extinction. According to the International Union and Conservation of Nature (IUCN), “More than 41,000 species are threatened with extinction. That is still 28% of all assessed species.” [1]. Increased extinction and loss of biodiversity can have severe ecological, economic, and cultural impacts. Cardinale et al.’s deep dive into biodiversity and ecosystem services research conclude that biodiversity loss reduces ecological communities’ efficiency, stability, and productivity. Decreased productivity from ecosystem services can have a negative impact on ecosystem economics [2]. Additionally, cultures worldwide have strong ties to local flora and fauna, much of which now face extinction risk. Improving understanding of extinction risk is ecologically, economically, and culturally important.

Wildlife scientists have been working to understand what ecological traits of vertebrates predict threat level, and what common risk factors drive those threat level rates. Munstermann et al. investigate what terrestrial vertebrate functional groups are most at risk of extinction threat and find that cave dwelling amphibian, arboreal quadrupedal mammals, aerial and scavenging birds, and pedal squamates are at high risk [3]. This knowledge can help inform policies and practices with the goal to decrease threats of extinction of wildlife. However, less comprehensive research has been done to conduct similar analyses on marine species.

In recent years, the waters surrounding the Hawaiian Islands have been exposed to ecological changes due to mass coral bleaching events, El Niño events, and pollution. Rapidly changing marine ecosystems may pose a threat to Hawaiian fish. Fish hold significant cultural value in Hawaiʻi, and many local people rely on seafood as a major source of protein. However, approximately 72% of fish in Hawaiʻi present in FishBase have been evaluated by the IUCN and have sufficient data to be assessed. Here I run a small-scale analysis to investigate Hawaiian fish ecological traits – such as endemism, size, and reef-association – to predict a binary status on the IUCN red list and predict which unevaluated fish species in Hawaiʻi may be threatened.

## Data
For my analyses I use the IUCN Red List data accessed via the [IUCN Red List](https://www.iucnredlist.org/) API[1] and package rredlist[4]. Fish ecological data were accessed from [FishBase.](www.fishbase.org)[5] via package rfishbase[6].

![Collage of three underwater fish photos](./fish_collage.jpg)

## All References 
[1] “IUCN,” IUCN Red List of Threatened Species. Version 2022-1, 2022. https://www.iucnredlist.org/ (accessed Dec. 02, 2022).

[2] B. J. Cardinale et al., “Biodiversity loss and its impact on humanity,” Nature, vol. 486, no. 7401, Art. no. 7401, Jun. 2012, doi: 10.1038/nature11148. 

[3] M. J. Munstermann et al., “A global ecological signal of extinction risk in terrestrial vertebrates,” Conserv. Biol., vol. 36, no. 3, p. e13852, 2022, doi: 10.1111/cobi.13852.

[4] “IUCN,” IUCN Red List of Threatened Species. Version 2022-1, 2015. www.iucnredlist.org

[5] R. Froese and D. Pauly, “FishBase,” 2022. www.fishbase.org

[6] C. Boettiger, D. Temple Lang, and P. Wainwright, “rfishbase: exploring, manipulating and visualizing FishBase data from R.,” J. Fish Biol., 2012, doi: https://doi.org/10.1111/j.1095-8649.2012.03464.x.

[7] W. J. Ripple, C. Wolf, T. M. Newsome, M. Hoffmann, A. J. Wirsing, and D. J. McCauley, “Extinction risk is most acute for the world’s largest and smallest vertebrates,” Proc. Natl. Acad. Sci. U. S. A., vol. 114, no. 40, pp. 10678–10683, Oct. 2017, doi: 10.1073/pnas.1702078114.

[8] K. D. Bahr, P. L. Jokiel, and K. S. Rodgers, “The 2014 coral bleaching and freshwater flood events in Kāneʻohe Bay, Hawaiʻi,” PeerJ, vol. 3, p. e1136, Aug. 2015, doi: 10.7717/peerj.1136.
