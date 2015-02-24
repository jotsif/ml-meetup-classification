
###
### Generate random training set to illustrate bias variance trade off
###

library(ggplot2)
library(rpart)
library(parallel)
library(reshape2)

n = 10000
cp = 0.00000001

errorvectors = lapply(1:100, function(r) {
    x1 = rnorm(n, 0, 1)
    x2 = rnorm(n, 0, 1)
    x3 = rnorm(n, 0, 5)
    y =  as.integer(x1 * x2 - 3 * x3 * x2 * x1 + runif(n, -10, 10) > 0)
    dataset = data.frame(x1 = x1, x2 = x2, x3 = x3, y = y)
    train = runif(n) > 0.5
    tree = rpart(formula(y ~ .), data = subset(dataset, train), control = rpart.control(cp = cp), )
    error = lapply(seq(0.01, cp, length.out = 100), function(i) {
        pruned.tree = prune.rpart(tree, cp = i)
        preds = predict(pruned.tree, dataset)
#        print(summary((preds == y)[!train]))
        cat('\r', r, i)
        mean(with(dataset, ((preds - y)[!train])**2))
    })
    return (unlist(error))
})#, mc.cores = 8)

p = ggplot(data = melt(cbind(x = seq(0.01, cp, length.out = 100),data.frame(do.call(cbind, errorvectors))), id.vars = 'x')) + geom_line(aes(x = x, y = value, col = variable, alpha = 0.2))
p = p + geom_line(aes(x = seq(0.01, cp, length.out = 100), y = rowMeans(data.matrix(do.call(cbind, errorvectors)))))

ggsave('test.pdf', plot = p)
