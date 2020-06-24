# Doctoral thesis - Plant-pollinator communities: environmental gradients, trade-offs, and controllability

**Fernando Cagua** - *School of Biological Sciences, University of Canterbury*

Submited: December 2019
Defended: June 2020

#### Summary

In this thesis, I focus on the network of mutualistic interactions between plants and pollinators in an ecological community. 
These networks, which form the base of pollination systems, play a globally significant role in the maintenance of biodiversity and crop production. 
Regrettably, just like other species interactions, the relationship between plants and pollinators, is currently being disrupted by global change at a worldwide scale. 

The number of partners species have is a defining feature of the roles they play in its community (Cirtwill et al. 2018). At the species level it determines whether a species is a special- ist or a generalist. At the community level, the distribution of the number of partners species have in the community is the main ingredient defining network structure.

In Chapter 1, I investigate how the environment may affect the number of partners a species has. In Capter 2, I ask the question of whether the number of partners is overall good or bad for pollination or whether there are trade-offs. Finally, in Chapter 3 I explore how the number of partners is related to the controllability of an ecological community. 

I answer these questions using a combination of limited empirical data and simulated big data. I analyse these data sets using a combination of machine learning,  bayesian statistics, and network tools. 

## Features

Code, data, and models described in this thesis can be found in it's respective repo.

* *Chapter 1* - **Effect of environment in specialisation**: Environmental stress affects specialists and generalists in different ways (source code: [efcaguab/species-distributions-and-networks](https://github.com/efcaguab/species-distributions-and-networks); published as a pre-print in BioRxiv: https://doi.org/10.1101/866772
* *Chapter 2* - **Trade-offs of sharing pollinators**: There is a trade-off between maximising pollination quantity and pollination purity (source code: [efcaguab/pollen-competition](https://github.com/efcaguab/pollen-competition); published as a pre-print in BioRxiv: https://doi.org/10.1101/865279 
* *Chapter 3* **Structural controllability of networks**: A rigorous application of structural controllability to ecological networks (source code: [stoufferlab/driver-species](https://github.com/stoufferlab/driver-species); published in the Journal of Ecology: https://doi.org/10.1111/1365-2745.13147)
* *Apendix A* - **Cophylogeny of pollination networks**: An exploration of cophylogeny at multiple scales of pollination networks (source code is private); published in Ecology: https://doi.org/10.1002/ecy.1955


## Repo Description

This repository contains the text for my thesis and other milestone documents of my PhD. 
The all can be found under releases.
It includes:

* A (successful) [funding proposal](https://github.com/efcaguab/phd-thesis/releases/download/v0.1-nzirds_proposal/NZIDRS-application-proposal.pdf) to New Zealand Education.
* The [research proposal](https://github.com/efcaguab/phd-thesis/releases/download/v0.2-uc_proposal/proposal.pdf).
* The PhD Confirmation [report](https://github.com/efcaguab/phd-thesis/releases/download/v0.3-confirmation/doctoral-confirmation-report.pdf) and [presentation](https://github.com/efcaguab/phd-thesis/releases/download/v0.3-confirmation/doctoral-confirmation-presentation.pptx).
* The [latest version of my thesis](https://github.com/efcaguab/phd-thesis/releases/download/v1.0.2-thesis/thesis.pdf).

## Reproducibility

I used LaTeX and RMarkdown to typeset all documents and figures. 
You can find a `Makefile` in each subdirectory and a [`Dockerfile`](https://github.com/efcaguab/phd-thesis/blob/master/thesis/Dockerfile) encapsulating all software requirements can be used to generate the thesis document. 

## Contact

If you have any comments or questions about the thesis, email me at fernando@cagua.co

