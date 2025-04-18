# tFUS-mVEPBCI-Analysis

v1.0.0 DOI: [10.5281/zenodo.10968398](https://doi.org/10.5281/zenodo.10969062)

These codes were used for the analysis of EEG BCI data as part of the study: Kosnoff J, Yu K, Liu C, He B. Transcranial focused ultrasound to V5 enhances human visual motion brain-computer interface by modulating feature-based attention. Nat Commun 2024;15:4382. https://doi.org/10.1038/s41467-024-48576-8.

In many cases, MNE Python based codes were copied, with some alteration, from MNE Python tutorials. I have done my best to mark all of the direct copied and pasted lines of code with their source. Please note that, for these marked lines, the license is BSD-3-Clause, and original copyright belongs to the MNE-Python contributors.

This work was supported by NIH grants R01NS124564 (PI: B.H.), R01AT009263 (PI: B.H.), U18EB029354 (PI: B.H.), T32EB029365 (J.K.), RF1NS131069 (PI: B.H.), R01NS096761 (PI: B.H.), and NS127849 (PI: B.H.), as well as National Science Foundation Graduate Research Fellowship Program grant DGE2140739 (J.K.). Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Institutes of Health or the National Science Foundation.

Please direct correspondence about the paper to: Dr. Bin He, Carnegie Mellon University, Department of Biomedical Engineering, Pittsburgh, PA 15213. E-mail: bhe1@andrew.cmu.edu

Some comments in the code may refer to "tFUS" and "US-Control" conditions. These are in reference to our earliest naming of experimental conditions. The names were changed during the Peer Review process, and in most cases updated in the code. If you do come across "tFUS" and "US-Control," "tFUS" refers to "tFUS-GC" and "US-Control" to "tFUS-GP"

## Contents: 

### R Analysis scripts:

These files contain the R codes to run the linear mixed effect models on the data. They have been updated to run directly on the Source Data file that is provided with the paper. Only the path to those source files, and the specific sheet to analyze, need to be changed by a different user. 

### Python Notebooks:

These notebooks are the rest of the analysis files. They have been named in a (mostly) intuitive way. The Behavioral Analysis should run directly on the BCI Outcomes file included in the this paper's FigShare repository. For the other files, you will need to create your own file_paths directory organizer. Once this is done, the EEG Preprocessor and Sensor Analysis notebooks should run. Due to subject confidentiality, we are not releasing the MRI files of associated with this study, so the N200 power analysis and Correlation Analysis files will not be able run as intended. However, they could still be run with assuming standard EEG montage and using MNE Python's sample head model (but please note doing so may produce different results than the ones reported in the paper).
