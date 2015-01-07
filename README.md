multiclassLR: multinomial regression in R  
============
This package contains an implementation of multinomial logistic regression and is optimized with the optim function (this is quite slow, so alternative methods are currently being developed). It can be used for any data set with both categorical and continuous variables, but is specifically designed to be used on the output of MESH ([Wen et al, 2012]). 

MESH takes as input for a single snp that has been subjected to some treatment, an estimate of allelic imblance, it's error, an estimate of allelic imblance of its control, and the corresponding error, and out outputs posterior probabilities of four configurations. The configurations are treatment only Allele specific expression (ASE), control only ASE, ASE in both treatment/control, & ASE in neither treatment/control. 

<!-- links -->
[Wen et al, 2012]:http://arxiv.org/abs/1111.1210