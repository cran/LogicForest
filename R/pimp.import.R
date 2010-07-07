pimp.import <-
function(fit, data, testdata, BSpred, pred, Xs)#assumes have test data
{
  n<-nrow(testdata) 
  tree<-fit$model$trees[[1]]
  y<-testdata[,(BSpred+1)]
  x.names<-colnames(data[,1:BSpred])
  orig.pred<-predict.logreg(fit, newbin=testdata[,1:BSpred]) 
  orig.miss<-sum(abs(orig.pred-y))/n 
  pimpinfo<-prime.imp(tree=tree, data=data, Xs=Xs)
  vec.Xvars<-pimpinfo$vec.pimpvars
  nxvars<-length(vec.Xvars)
  single.vimp<-c()
  single.vimp.nms<-c()
  Xids<-c()
  for (i in 1:nxvars)
    {
    id<-vec.Xvars[i]
    Xid<-Xs[id]
    permute.ind<-sample(1:n, n, replace=FALSE)
    permute.col<-testdata[permute.ind, id] 
    pre.id<-if(id>1) as.matrix(testdata[,1:(id-1)]) 
    post.id<-if(id<pred) as.matrix(testdata[,(id+1):BSpred])
    permute.testdata<-cbind(pre.id, permute.col, post.id)
    perm.pred<-predict.logreg(fit, newbin=as.matrix(permute.testdata[,1:BSpred])) 
    perm.misclass<-sum(abs(perm.pred-y))/n  
    vimp<-perm.misclass-orig.miss
    single.vimp<-append(single.vimp, vimp)
    single.vimp.nms<-append(single.vimp.nms, x.names[id])
    Xids<-append(Xids, Xid)
    }
  names(single.vimp)<-single.vimp.nms
  pimp.info<-pimp.mat(pimps.out=pimpinfo, testdata=testdata)
  pimpmat<-pimp.info[[2]] 
  pimpnames<-pimp.info[[1]]
  tmp.mat<-pimpinfo$tmp.mat
  zero.ids<-c()
  for(i in 1:ncol(tmp.mat))
    {
    ids<-if(all(tmp.mat[,i]==0)) {ids<-i}
    zero.ids<-append(zero.ids, ids)
    }
  if (length(zero.ids) > 0) {tmp.mat<-tmp.mat[,-zero.ids]}
  if (is.matrix(tmp.mat)) {npimps<-nrow(tmp.mat)}
  if (is.vector(tmp.mat)) {npimps<-1}
  pimp.vimp<-c()
  for (j in 1:npimps) 
    {
    perm.ind<-sample(1:n, n, replace=FALSE)
    perm.col<-pimpmat[perm.ind, j] 
    pre.j<-if(j>1) pimpmat[,1:(j-1)] 
    post.j<-if(j<npimps) pimpmat[,(j+1):npimps]
    permute.pimpdata<-cbind(pre.j, perm.col, post.j)
    pimp.pred<-c()
    for (k in 1:n)
      {
      pred<-ifelse(any(permute.pimpdata[k,]==1), 1, 0)
      pimp.pred<-append(pimp.pred, pred)
      }
    permpimp.miss<-sum(abs(pimp.pred-y))/n
    pvimp<-permpimp.miss-orig.miss 
    pimp.vimp<-append(pimp.vimp, pvimp)
    }
  names(pimp.vimp)<-paste(pimpnames)
  out<-list(single.vimp=single.vimp, pimp.vimp=pimp.vimp, vec.Xvars=vec.Xvars, Xids=Xids)
}

