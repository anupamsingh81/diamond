var n =100000
var n0 = 54
var x0 = 18
var n1 = 18
var x1 =10
var n2 = 36
var x2 = 26




var P = calls(n=n, jStat.beta.sample, alpha= x0+1, beta=n0-(x0+1))
var M = calls(n=n, jStat.beta.sample, alpha= x1+1, beta=n1-(x1+1))
var N = calls(n=n, jStat.beta.sample, alpha= x2+1, beta=n2-(x2+1))

var Post = []

function calc(P0,P1,P2){ return P0*P1/(P0*P1+ (1-P0)*P2 ) }


for(var i=0;i<n;i++){
Post.push(calc(P0=P[i],P1=M[i],P2=N[i]))
}
var rez = jStat.quantiles(Post,[0.025,0.5,0.975])
