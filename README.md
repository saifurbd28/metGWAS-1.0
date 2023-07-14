# metGWAS-1.0
The supplementary material for "metGWAS 1.0: An R workflow for network-driven over-representation analysis between independent metabolomic and meta-genome wide association studies "

# ABSTRACT 

## Background
Many diseases may result from disrupted metabolic regulation. Metabolite-GWAS studies assess the association of polymorphic variants with metabolite levels in body fluids. While these studies are successful, they have a high cost and technical expertise burden due to combining the analytical biochemistry of metabolomics with the computational genetics of GWAS. Currently, there are 100s of standalone metabolomics and GWAS studies related to similar diseases or phenotypes. A method that could statically evaluate these independent studies to find novel metabolites-genes association is of high interest. Although such an analysis is limited to genes with known metabolite interactions due to the unpaired nature of the data sets, any discovered associations may represent biomarkers and druggable targets for treatment and prevention. 

## Methods
We developed a bioinformatics tool, metGWAS 1.0, that generates and statistically compares metabolic and genomic gene sets using a hypergeometric test. Metabolic gene sets are generated by mapping disease-associated metabolites to interacting proteins (genes) via online databases. Genomic gene sets are identified from a network representation of the GWAS Catalog comprising 100s of studies. 

## Results
The metGWAS 1.0 tool was evaluated using standalone metabolomics datasets extracted from two metabolomics-GWAS case studies. In case-study 1, a cardiovascular disease association study, we identified nine genes (APOA5, PLA2G5, PLA2G2D, PLA2G2E, PLA2G2F, LRAT, PLA2G2A, PLB1, and PLA2G7) that interact with metabolites in the KEGG glycerophospholipid metabolism pathway and contain polymorphic variants associated with cardiovascular disease (P < 0.005). The gene APOA5 was matched from the original metabolomics-GWAS study. In the case study 2,  a urine metabolome study of kidney metabolism in healthy subjects, we found marginal significance (P = 0.10 and P = 0.13) for glycine, serine, and threonine metabolism and alanine, aspartate, and glutamate metabolism pathways to GWAS data relating to kidney disease. 

## Conclusion
The metGWAS 1.0 platform provides insight into developing methods that bridge standalone metabolomics and disease and phenotype GWAS data. We show the potential to reproduce findings of paired metabolomics-GWAS data and provide novel associations of gene variation and metabolite expression.

# Data structure and implementation
It contains two supplementary materials. The supplementary material-1 consists of several R-scripts (i.e., metGWAS 1.0_19 April, qvalue and bioconductor install, and RSelenium Driver SetUp) with an object named folder and the user manual, split into two documents: a tutorial and a technical, for running the workflow. The provided two necessary R-scripts have been given here (i.e., qvalue and bioconductor install.R, and RSelenium Driver SetUp.R) for setting up the environment. metGWAS 1.0_19 April.R contains R-codes for running the workflow. The objects folder should contain the following files:

1. discoverableGenes_akaAllGenesInHMDB_2021_10_14.RData (64KB)
2. GWASnetwork_discoverableGenesOnly_2021_10_14.RData (64,516KB)

Since file-2 is too large to upload here, we have provided a downloading link from where this RData file can be downloaded. The link information is given in the "GWASnetwork_discoverableGenesOnly_2021_10_14.RData.docx". For the convenience of the users, we have also given the link here:

https://nam12.safelinks.protection.outlook.com/?url=https%3A%2F%2Fdataverse.harvard.edu%2Fdataset.xhtml%3FpersistentId%3Ddoi%3A10.7910%2FDVN%2FB32FRP&data=05%7C01%7Ckhanmsr%40pitt.edu%7Cc22d973dc2464e13bdd608db83e60a5f%7C9ef9f489e0a04eeb87cc3a526112fd0d%7C1%7C0%7C638248796268228403%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=Kr13ycc24z5BVkTQzUc7BcFSw9z1adMV1jB8gU0FVPE%3D&reserved=0


