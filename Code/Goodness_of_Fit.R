# Goodness of fit :) 


# Example: 

#We have differents trees in a forest and we are expecting to have the same proportion 
#Ho= Equal distribution of "correct choices"
#Ha= Not equal distribution 

#We are taking a sample of n= 100 of the whole population 

observed = c(20,20,25,35)
theo = c(1/4,1/4,1/4,1/4) #This value has to be in proportion

chisq.test (x=observed,
            p=theo)



