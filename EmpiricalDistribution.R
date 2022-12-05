EmpDistribution = function(x){
  k = length(x)
  p = 1:k
  x = sort(x)
  sd = 1:k
  sup95 = 1:k
  inf95 = 1:k
  for(i in 1:k){
    p[i] = sum(as.numeric(x <= x[i])/k)
    sd[i] = sqrt((p[i] * (1-p[i]))/k)
    sup95[i] = min(p[i] + 1.96 * sd[i], 1)
    inf95[i] = max(p[i] - 1.96 * sd[i], 0)
  }
  
  return(list(Probability = p, Data = x, SDesvian = sd, ICSup95 = sup95, ICInf95 = inf95))
}

x1 = rnorm(250, 150, 500)

x = EmpDistribution(x1)

x = data.frame(x)

ggplot(x, aes(Data, Probability)) +
  geom_step() +
  geom_step(aes(Data, ICInf95), color = "red") +
  geom_step(aes(Data, ICSup95), color = "red")


t = 1:(6*30)
l = 0
n = 1:(6*30)
for(j in c(100, 1000, 2500, 5000, 7500, 10000)){
  data = rnorm(j, 0, 1)
  for(i in 1:30){
    start_timer = Sys.time()
    EmpDistribution(data)
    end_timer = Sys.time()
    l = l + 1
    t[l] = end_timer - start_timer
    n[l] = j
  }
}

df = data.frame(t, n)

ggplot(df, aes(n, t)) +
  geom_point()
