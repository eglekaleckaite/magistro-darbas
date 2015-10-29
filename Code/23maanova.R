library("maanova")
data("kidney")
gridcheck(kidney.raw)
graphics.off()
riplot(kidney.raw)
graphics.off()
arrayview(kidney.raw)
kidney <- transform.madata(kidney.raw, method="rlowess")
fit.fix <- fitmaanova(kidney, formula = ~Dye+Array+Sample)
resiplot(kidney, fit.fix)
test.fix <- matest(kidney, fit.fix, term="Sample", n.perm=500)
test.fix <- adjPval(test.fix, method = 'jsFDR')
result = summarytable(test.fix)
summarytable(test.fix, method=c("Pvalperm"), test=c("F1", "Fs"),
             whichTest=c("Fs.Pvalperm"), threshold=.1, outfile='shortsummarytable.csv')
idx.fix <- volcano(test.fix,method=c("unadj", "fdrperm"),
                   highlight.flag=FALSE)
cluster.kmean <- macluster(fit.fix, ,term="Sample",
                           idx.gene=idx.fix$idx.all,what="gene", method="kmean",
                           kmean.ngroups=5, n.perm=100)
con.kmean <- consensus(cluster.kmean, 0.7)
con.kmean$groupname
cluster.hc <- macluster(fit.fix, term="Sample",
                        idx.gene=idx.fix$idx.all,what="sample",
                        method="hc", n.perm=100)
con.hc <- consensus(cluster.hc)


library(affy)

beforeRma <- ReadAffy()
data(abf1)
fit.full.mix <- fitmaanova(abf1, formula = ~Strain+Sample,
                           random = ~Sample)
resiplot(abf1, fit.full.mix)
ftest.all = matest(abf1, fit.full.mix, test.method=c(1,1),
                   shuffle.method="sample", term="Strain", n.perm= 100)
C = matrix(c(1,-1,0,1,0,-1), ncol=3, byrow=T)
C.all = PairContrast(3)
ftest.pair = matest(abf1, fit.full.mix, Contrast = C,
                    term="Strain", n.perm=100)
ftest.all = adjPval(ftest.all, method = 'jsFDR')
summarytable(ftest.all)



