clc;clear;

%% COSTS

% Backorder cost
c.b= 3;
% Holding cost
c.i = 1;
c.climit= 90;

%% other inputs
p.horstartinv = 73;
% Deemand Distribution
forecastmean = [97.17678027
98.03143327
110.389103
95.58407278
98.14415763
109.2789882
101.2469888
85.75616091
110.3280022
105.6473042
106.6270272
114.2883564
97.5451419
97.74342712
110.4018176
95.75948379
97.96844686
109.3241998
101.3214145
85.65572738
110.3718331
105.6733951
106.5731747
114.3212175];
errordistmean =0.17;
errorstdev = 2.69;


basestock =[99.0
99.8
112.2
97.4
100.0
111.1
103.1
87.6
112.1
107.5
108.4
116.1
99.4
99.6
112.2
97.6
99.8
111.1
103.1
87.5
112.2
107.5
108.4
116.1];


p.Demmean = forecastmean';
p.errormean=errordistmean;
p.errorstdev = errorstdev;
order = forecastmean';

%% Calling the fun
SIMFUN(order,p,c)
order1=[order(2:end) 130]

Pop =[order; order1]
%% Setting up the gA
LB = zeros (size(order));   
UB = ones(size(order))*500;
% GA options
options = optimoptions('ga','PlotFcn',{@gaplotbestf,@gaplotstopping},.....
    'Display','iter','MaxGenerations',200,'MaxStallGenerations',4,.....
    'EliteCount',10,'PopulationSize',100,'CrossoverFraction',0.6);
%options = optimoptions('ga',options);
options = optimoptions('ga',options,'InitialPopulationMatrix',Pop);
%
% Calling GA optimization routine   
[Rgaout,FVAL1,EXITFLAG1,Output,POPULATION1,SCORES1]....
            = ga(@(order)SIMFUN2(order,p,c),24,[],[],[],[],LB,UB,[],options)
