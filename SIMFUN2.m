function Totalcost_Final=SIMFUN2(O,p,c)
nsim=5000;
Totalcostarray = zeros (1,nsim);
for n = 1:nsim
    totalcost=0;
    startinv = zeros (1,24);
    dem= zeros (1,24);
    netdem= zeros (1,24);
    invpositionbeg= zeros (1,24);
    order= zeros (1,24);
    invheld= zeros (1,24);
    backorders= zeros (1,24);
    invcost= zeros (1,24);
    backordercost= zeros (1,24);
    for i = 1:24
        if i==1
          startinv(i) = p.horstartinv;
          dem(i) = p.Demmean(i)+ normrnd(p.errormean,p.errorstdev);
          netdem(i) = dem(i);
          invpositionbeg(i) = startinv(i)-netdem(i);
          order(i)= O(i+1)-invpositionbeg(i);
          invheld(i) = max(invpositionbeg(i),0);
          backorders(i) = max(-invpositionbeg(i),0);
          invcost(i)= (c.i*invheld(i))+(c.i*max((invheld(i)-c.climit),0));
          backordercost(i) = c.b*backorders(i);
          totalcost = totalcost+invcost(i)+backordercost(i);
        elseif i==24
          startinv(i) = invheld(i-1)+ order(i-1);
          dem(i) = p.Demmean(i)+ normrnd(p.errormean,p.errorstdev);
          netdem(i) = dem(i)+ backorders (i-1);
          invpositionbeg(i) =  startinv(i)-netdem(i);
          %order(i)= O(i+1)-invpositionbeg(i);
          invheld(i) = max(invpositionbeg(i),0);
          backorders(i) = max(-invpositionbeg(i),0);
          invcost(i)= (c.i*invheld(i))+(c.i*max((invheld(i)-c.climit),0));
          backordercost(i) = c.b*backorders(i);
          totalcost = totalcost+invcost(i)+backordercost(i);  
      else
          startinv(i) = invheld(i-1)+ order(i-1);
          dem(i) = p.Demmean(i)+ normrnd(p.errormean,p.errorstdev);
          netdem(i) = dem(i)+ backorders (i-1);
          invpositionbeg(i) =  startinv(i)-netdem(i);
          order(i)= O(i+1)-invpositionbeg(i);
          invheld(i) = max(invpositionbeg(i),0);
          backorders(i) = max(-invpositionbeg(i),0);
          invcost(i)= (c.i*invheld(i))+(c.i*max((invheld(i)-c.climit),0));
          backordercost(i) = c.b*backorders(i);
          totalcost = totalcost+invcost(i)+backordercost(i);
        end
    end
    Totalcostarray(n) = totalcost;
end
Totalcost_Final = mean(Totalcostarray);
%variance = var(Totalcostarray);
%z = norminv(1 - 0.025);
%CI= [Totalcost_Final - z*sqrt(variance/nsim) ,Totalcost_Final + z*sqrt(variance/nsim)]
end