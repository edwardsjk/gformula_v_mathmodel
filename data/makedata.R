set.seed(12123)
z = rbinom(100, 1,0.5)
x = rbinom(100, 1, 1/(1+exp(-(-0.5 + z))))
y = rbinom(100, 1, 1/(1+exp(-(-0.5 - 1 + z + 2*x))))

write.csv(data.frame(z=z,x=x,y=y), "~/Documents/programming_examples/R/_mine/shiny/gformula_slider/data/testdata.csv", row.names=FALSE)

set.seed(12123)
z = rnorm(100, 0, 1)
x = rnorm(100, 3*z, 1)
y = rbinom(100, 1, 1/(1+exp(-(-0.5 - 1 + z + 2*x))))
write.csv(data.frame(z=z,x=x,y=y), "~/Documents/programming_examples/R/_mine/shiny/gformula_slider/data/testdata2.csv", row.names=FALSE)