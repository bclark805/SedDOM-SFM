% Simple script to plot the test sed dom code vs observed values from R64

%need to load in the R64 workspace for all the vars;

mydirectory=input('What is the full path and directory of the SedDDOM-SFM model?','s');

addpath(genpath(mydirectory));

load('fluxzilla.mat');

seddom_flag = input('Is this run with Sediment DOM?   Enter 0 for no and 1 for yes --->   ');
% get in the output from the sediment DOM module for plotting
mystat = input('Is this for RGPT, R-64, PNPT or another station ? Enter here ---> ','s');


if(seddom_flag)
    OLW_DOM = importdata('outputs/watercolumn_DOM_stations.dat');
    SED_DOM = importdata('outputs/sediment_DOM_stations.dat');
    SED_JDOM = importdata('outputs/fluxes_DOM_stations.dat');
    
    OLW_DOC = OLW_DOM.data(:,1:6);
    OLW_DON = OLW_DOM.data(:,7:12);

    DOC = SED_DOM.data(:,1:6);
    DON = SED_DOM.data(:,7:12);

    JDOC = SED_JDOM.data(:,1:6);
    JDON = SED_JDOM.data(:,7:12);
end

Nutrients = importdata('sedtest_2c.csv');
JNH4 = Nutrients.data(:,17);
SOD = Nutrients.data(:,16);
JNO3 =  Nutrients.data(:,18);
JHS = Nutrients.data(:,22);
SEDPC = Nutrients.data(:,40:42);
SEDPN = Nutrients.data(:,43:45);
% 
% if(strcmp(mystat,'RGPT'))
%     mystatid = 216;
% elseif(strcmp(mystat,'R64'))
%     mystatid = 220;
% elseif(strcmp(mystat,'PNPT')
% 
% end

% find the index
for i = 1: length(fluxzilla)
    mystat1(i) = strcmp(fluxzilla(i).station,mystat);
end

mystatid=find(mystat1) % get the index


MODTIME = Nutrients.data(:,2);

% find the corresponding times in each array, observed and modeled for
% statistical comparison

newdate=datenum(fluxzilla(mystatid).date);
for i = 1: length(MODTIME)
    for j = 1 : length(newdate)
        if(MODTIME(i) == newdate(j)-newdate(1));
            matched_times(j)=i;
        end
    end
end

%make a directory to save the files
mkdir('outputs/figs');

%====== DON=====
if(seddom_flag)
figure;

subplot(3,1,1)
plot(MODTIME,sum(DON,2),'Linewidth',3)
hold on
plot(MODTIME,DON(:,1)+DON(:,4),'Linewidth',3)  % Labile
plot(MODTIME,DON(:,2)+DON(:,5),'Linewidth',3)  % Semi-labile
plot(MODTIME,DON(:,3)+DON(:,6),'Linewidth',3)

ylabel('DON conc (g N m^-^3)','Fontsize',16)
hold on
% plot(Burdidge_DOM_concs(:,1),Burdidge_DOM_concs(:,3),'kd','markersize',10)
legend('TDON','DON1' ,'DON2' ,'DON3','Observed','Fontsize',16)


subplot(3,1,2)
plot(MODTIME,sum(JDON,2),'Linewidth',3)
hold on
plot(MODTIME,JDON(:,1)+JDON(:,4),'Linewidth',3)  % Labile
plot(MODTIME,JDON(:,2)+JDON(:,5),'Linewidth',3)  % Semi-labile
plot(MODTIME,JDON(:,3)+JDON(:,6),'Linewidth',3)

% plot(Burdidge_DOM_fluxes(:,1),Burdidge_DOM_fluxes(:,4),'kd','markersize',10);
ylabel('DON flux (g N m^-^2 d^-^1','Fontsize',16)

subplot(3,1,3)
plot(MODTIME,zeros(length(OLW_DON),1),'Linewidth',3,'color','w')
hold on
plot(MODTIME,OLW_DON(:,1)+OLW_DON(:,4),'Linewidth',3)

plot(MODTIME,OLW_DON(:,2)+OLW_DON(:,5),'Linewidth',3)
plot(MODTIME,OLW_DON(:,3)+OLW_DON(:,6),'Linewidth',3)
ylabel('Water Column DON (mg N l^-^1)');xlabel('Days from 05/06/1985','Fontsize',16)


saveas(gcf,['outputs/figs/DON_fluxconc'],'png');
% ======= DOC ===========
figure
subplot(3,1,1)
plot(MODTIME,sum(DOC,2),'Linewidth',3)
hold on
plot(MODTIME,DOC(:,1)+DOC(:,4),'Linewidth',3)  % Labile
plot(MODTIME,DOC(:,2)+DOC(:,5),'Linewidth',3)  % Semi-labile
plot(MODTIME,DOC(:,3)+DOC(:,6),'Linewidth',3)

hold on
% plot(Burdidge_DOM_concs(:,1),Burdidge_DOM_concs(:,2),'kd','markersize',10);
ylabel('DOC  (g C m^-^3')
legend('TDOC','DOC1' ,'DOC2' ,'DOC3','Observed','Fontsize',16)
subplot(3,1,2)
plot(MODTIME,sum(JDOC,2),'Linewidth',3)
hold on

plot(MODTIME,JDOC(:,1)+JDOC(:,4),'Linewidth',3)  % Labile
plot(MODTIME,JDOC(:,2)+JDOC(:,5),'Linewidth',3)  % Semi-labile
plot(MODTIME,JDOC(:,3)+JDOC(:,6),'Linewidth',3)

% plot(Burdidge_DOM_fluxes(:,1),Burdidge_DOM_fluxes(:,2),'kd','markersize',10);
ylabel('DOC flux (g C m^-^2 d^-^1')

subplot(3,1,3)
plot(MODTIME,zeros(length(OLW_DON),1),'Linewidth',3,'color','w')
hold on
plot(MODTIME,OLW_DOC(:,1)+OLW_DOC(:,4),'Linewidth',3)

plot(MODTIME,OLW_DOC(:,2)+OLW_DOC(:,5),'Linewidth',3)
plot(MODTIME,OLW_DOC(:,3)+OLW_DOC(:,6),'Linewidth',3)
ylabel('Water Column DOC (mg l^-^1)');xlabel('Days from 05/06/1985','Fontsize',16)
end

saveas(gcf,['outputs/figs/DOC_fluxconc'],'png');
%%
% ======== Nutrients =========


goodtimes = matched_times(5:end); 
model_time = MODTIME(matched_times(5):matched_times(end));
%SOD

X_sod = fluxzilla(mystatid).doflux(5:length(matched_times))/32*1000*-1; % get the points in our time frame to plot
Y_sod = SOD(goodtimes)/32*1000; % find the corresponding points int he model

fluxzilla_date=fluxzilla(mystatid).datetime(5:length(matched_times))-datenum([1985 05 06 00 00 00]);

figure
subplot(3,1,1)
hold on
[ax,h1,h2]=plotyy(model_time,SOD(matched_times(5):matched_times(end))/32*1000,model_time,Nutrients.data(matched_times(5):matched_times(end),7)./32*1000)
hold on
plot(fluxzilla_date,X_sod,'kd','markersize',8,'linewidth',2);

% hold on
% 
% plot(fluxzilla_date,X_sod,'k--d','markersize',8,'linewidth',2);
% % plot(model_time,Nutrients(7).data(matched_times(5):matched_times(end))/32*1000,'linewidth',3);
% plot(model_time,Nutrients.data(matched_times(5):matched_times(end),7)./32*1000,'linewidth',3);  % observed oxygen conc
ax(1).XLim=[236 4254];
ax(2).XLim=[236 4254];
ax(2).YLim=[0 470];
ax(2).YTick=[0:94:470];
ax(1).YTick=[0:15:90];
ax(1).YLim=[0 90];

legend('Modeled','Observed','O2');
ylabel('SOD (mmol O_2 m^-^2 d^-^1)');

[r2] =  rsquare(Y_sod,X_sod);

% model efficiency   

part1 = sum((X_sod-mean(X_sod)).^2);
part2 = sum((Y_sod-X_sod).^2);

MEF =(part1-part2)./part1;

% r
bottom = sqrt(sum((X_sod-mean(X_sod)).^2).*sum((Y_sod-mean(Y_sod)).^2))

top=sum((X_sod-mean(X_sod)).*(Y_sod-mean(Y_sod)))

r=top./bottom;

%rmse
rmse = sqrt((sum((Y_sod-X_sod).^2))./length(X_sod));

%      
%      R1 = exp(((sum(ln(Y./X)^2)/length(Y)).^0.5))
text(1000,max(Y_sod),['r = ',num2str(r), ', MEF = ', num2str(MEF),  ', RMSE = ',num2str(rmse)],'fontsize',12)

    %NH4
    
 X_nh4 =  fluxzilla(mystatid).nh4flux(5:length(matched_times))./1000*24;
 Y_nh4 = JNH4(goodtimes)/14*1000;
 
%  figure;
subplot(3,1,2)
plot(model_time,JNH4(matched_times(5):matched_times(end))/14*1000,'linewidth',3)

hold on
% plot(model_time,Nutrients.data(matched_times(5):matched_times(end),9),'linewidth',3);  % temperature and oxygen

plot(fluxzilla_date,X_nh4,'kd','markersize',8,'linewidth',2)
ylabel('NH_4 flux (mmol N m^-^2 d^-^1)');

axis([236 4254 -1  24])


% [fitted1,s] = polyfit(X,Y,1);
%     values1=[];
%     r2 = [];
%     rmse = [];
%     values1 = polyval(fitted1,X);
%     plot(X,fitted1(1)*X+fitted1(2),'linewidth',2,'color','k')
      [r2]=  rsquare(X_nh4,Y_nh4);
     %model efficiency   
part1 = sum((X_nh4-mean(X_nh4)).^2);
part2 = sum((Y_nh4-X_nh4).^2);
MEF =(part1-part2)./part1;
bottom = sqrt(sum((X_nh4-mean(X_nh4)).^2).*sum((Y_nh4-mean(Y_nh4)).^2))

top=sum((X_nh4-mean(X_nh4)).*(Y_nh4-mean(Y_nh4)))


r=top./bottom;
rmse= sqrt((sum((Y_nh4-X_nh4).^2))./length(X_nh4));

%      
%      R1 = exp(((sum(ln(Y./X)^2)/length(Y)).^0.5))
    text(1000,max(Y_nh4),['r = ',num2str(r), ', MEF = ', num2str(MEF), ', RMSE = ',num2str(rmse)],'fontsize',12)
       
 %NO3   
 X_no3 = fluxzilla(mystatid).no23flux(5:length(matched_times))./1000*24;
 Y_no3 =JNO3(goodtimes)/14*1000;
subplot(3,1,3)
plot(model_time,JNO3(matched_times(5):matched_times(end))/14*1000,'linewidth',3);
hold on
plot(fluxzilla_date,X_no3,'kd','markersize',8,'linewidth',2)
% plot(model_time,Nutrients.data(matched_times(5):matched_times(end),11),'linewidth',3);  % temperature and oxygen

ylabel('NO_3 flux (mmol N m^-^2 d^-^1)'); xlabel(['Days from ',fluxzilla(mystatid).date(5)])
axis([236 4254 -10  3])

% [fitted1,s] = polyfit(X,Y,1);
%     values1=[];
%     r2 = [];
%     rmse = [];
%     values1 = polyval(fitted1,X);
%     plot(X,fitted1(1)*X+fitted1(2),'linewidth',2,'color','k')
      [r2] =  rsquare(X_no3,Y_no3);
     
     %model efficiency   
 part1 = sum((X_no3-mean(X_no3)).^2);
part2 = sum((Y_no3-X_no3).^2);
MEF =(part1-part2)./part1;

bottom = sqrt(sum((X_no3-mean(X_no3)).^2).*sum((Y_no3-mean(Y_no3)).^2))

top=sum((X_no3-mean(X_no3)).*(Y_no3-mean(Y_no3)))

r=top./bottom;

rmse = sqrt((sum((Y_no3-X_no3).^2))./length(X_no3));
%      
%      R1 = exp(((sum(ln(Y./X)^2)/length(Y)).^0.5))
    text(1000,max(Y_no3),['r = ',num2str(r), ', MEF = ', num2str(MEF), ', RMSE = ',num2str(rmse)],'fontsize',12)
    
saveas(gcf,['outputs/figs/fluxzilla_compare'],'png');

%Particualte C and N content
figure

%    density = 600;  % kg m^-3
   m2 = 360  % kg m^-3 sediment solid density in model
   
   subplot(2,1,1) 
    plot(MODTIME,sum(SEDPC(:,1:3),2)./2.677/1000/m2*100,'linewidth',3)  % gO2 m^-3 --> kg C l^-1 / kg  sediments l^-1  --> kg C / kg sediments / 100 --> % TOC
    hold on
    % TOC =  10g/kg * kg m^-3 = g C  m^-3
    plot(fluxzilla(mystatid).datetime-datenum([1985 05 06 00 00 00]),fluxzilla(mystatid).sedpc,'k--o','markersize',10,'linewidth',2)
    ylabel('Sediment TOC conc (%)');xlabel(['Days from ',fluxzilla(mystatid).date(5)])
    legend('Modeled','Observed');
    
  subplot(2,1,2)     
    plot(MODTIME,sum(SEDPN(:,1:3),2)./1000/m2*100,'linewidth',3)  % gO2 m^-3 --> kg C l^-1 / kg  sediments l^-1  --> kg C / kg sediments / 100 --> % TOC
    hold on
    % TOC =  10g/kg * kg m^-3 = g C  m^-3
    plot(fluxzilla(mystatid).datetime-datenum([1985 05 06 00 00 00]),fluxzilla(mystatid).sedpn,'k--o','markersize',10,'linewidth',2)
    ylabel('Sediment TON conc (%)');xlabel(['Days from ',fluxzilla(mystatid).date(5)])
    
    saveas(gcf,['outputs/figs/SEDPOM_compare'],'png');

%%
%plot yearly average for all compounds
if(seddom_flag)
%get rid of the first partial year
  % 239 days after may 6 (day 0 ) is Jan 1
  end_year1 = find(MODTIME ==240);
my_idxs_yearly = end_year1:365*4:length(MODTIME);
 yearly_Cfluxes = [];
 yearly_Cconcs=[];
 yearly_Nfluxes = [];
 yearly_Nconcs=[];
   yearly_OLW_N=[];
     yearly_OLW_C=[];close
 %fill a 3D array witht the yaerly fluxes
 
 yearly_JPOC = [];
  yearly_JPON = [];
  
 JPOC = Nutrients.data(:,3)./2.667;
 JPON = Nutrients.data(:,4);

for i = 2 : length(my_idxs_yearly);
    
    yearly_Cfluxes = cat(3,yearly_Cfluxes,JDOC(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_Nfluxes = cat(3,yearly_Nfluxes,JDON(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_OLW_C = cat(3,yearly_OLW_C,OLW_DOC(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    
    yearly_Cconcs = cat(3,yearly_Cconcs,DOC(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_Nconcs = cat(3,yearly_Nconcs,DON(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_OLW_N = cat(3,yearly_OLW_N,OLW_DON(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
     yearly_JPOC = cat(2,yearly_JPOC,JPOC(my_idxs_yearly(i-1):my_idxs_yearly(i)));
     yearly_JPON = cat(2,yearly_JPON,JPON(my_idxs_yearly(i-1):my_idxs_yearly(i)));
end



%plot the results

mean_yearly_Cfluxes = mean(yearly_Cfluxes,3);
mean_yearly_Nfluxes = mean(yearly_Nfluxes,3);
mean_yearly_Cconcs = mean(yearly_Cconcs,3);
mean_yearly_Nconcs = mean(yearly_Nconcs,3);
mean_yearly_OLW_N = mean(yearly_OLW_N,3);
mean_yearly_OLW_C = mean(yearly_OLW_C,3);;



%JDOC
plot_time = 0:0.25:365;
figure;
subplot(2,2,1)
plot(plot_time,sum(mean_yearly_Cfluxes,2)*1000,'linewidth',3);  % totalDOC
hold on

plot(plot_time,mean_yearly_Cfluxes(:,1)+mean_yearly_Cfluxes(:,4)*1000,'linewidth',3); % DOC1
[max_val,max_idx]=max(mean_yearly_Cfluxes(:,1)+mean_yearly_Cfluxes(:,4));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Cfluxes(:,2)+mean_yearly_Cfluxes(:,5)*1000,'linewidth',3);%DOC2
[max_val,max_idx]=max(mean_yearly_Cfluxes(:,2)+mean_yearly_Cfluxes(:,5));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Cfluxes(:,3)+mean_yearly_Cfluxes(:,6)*1000,'linewidth',3);%DOC3
ylabel('DOC flux (mg C m^-^2 d^-^1');xlabel('Days from Jan 1')
legend('TDOC','DOC1' ,'DOC2' ,'DOC3','Observed','Fontsize',16)

subplot(2,2,2)

%DOC concs
plot(plot_time,sum(mean_yearly_Cconcs,2),'linewidth',3);  % totalDOC
hold on
plot(plot_time,mean_yearly_Cconcs(:,1)+mean_yearly_Cconcs(:,4),'linewidth',3); % DOC1
[max_val,max_idx]=max(mean_yearly_Cconcs(:,1)+mean_yearly_Cconcs(:,4));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Cconcs(:,2)+mean_yearly_Cconcs(:,5),'linewidth',3);%DOC2
[max_val,max_idx]=max(mean_yearly_Cconcs(:,2)+mean_yearly_Cconcs(:,5));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Cconcs(:,3)+mean_yearly_Cconcs(:,6),'linewidth',3);%DOC3
ylabel('DOC  (g C m^-^3)');xlabel('Days from Jan 1')
legend('TDOC','DOC1' ,'DOC2' ,'DOC3','Observed','Fontsize',16)



%JDON
subplot(2,2,3)
plot(plot_time,sum(mean_yearly_Nfluxes,2),'linewidth',3);  % totalDOC
hold on

%plot the max value days as well to see where they peak
plot(plot_time,mean_yearly_Nfluxes(:,1)+mean_yearly_Nfluxes(:,4),'linewidth',3); % DON1
[max_val,max_idx]=max(mean_yearly_Nfluxes(:,1)+mean_yearly_Nfluxes(:,4));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Nfluxes(:,2)+mean_yearly_Nfluxes(:,5),'linewidth',3);%DON2
[max_val,max_idx]=max(mean_yearly_Nfluxes(:,2)+mean_yearly_Nfluxes(:,5));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);
plot(plot_time,mean_yearly_Nfluxes(:,3)+mean_yearly_Nfluxes(:,6),'linewidth',3);%DON3

ylabel('DON flux (g N m^-^2 d^-^1');xlabel('Days from Jan 1')
legend('TDON','DON1' ,'DON2' ,'DON3','Observed','Fontsize',16)

% put some text at the min and max value

subplot(2,2,4)
%DON concs
plot(plot_time,sum(mean_yearly_Nconcs,2),'linewidth',3);  % totalDON
hold on

plot(plot_time,mean_yearly_Nconcs(:,1)+mean_yearly_Nconcs(:,4),'linewidth',3); % DON1
[max_val,max_idx]=max(mean_yearly_Nconcs(:,1)+mean_yearly_Nconcs(:,4));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);


plot(plot_time,mean_yearly_Nconcs(:,2)+mean_yearly_Nconcs(:,5),'linewidth',3);%DON2
[max_val,max_idx]=max(mean_yearly_Nconcs(:,2)+mean_yearly_Nconcs(:,5));
text(plot_time(max_idx),max_val,num2str(plot_time(max_idx)),'fontsize',16);

plot(plot_time,mean_yearly_Nconcs(:,3)+mean_yearly_Nconcs(:,6),'linewidth',3);%DON3
ylabel('DON  (g N m^-^3)');xlabel('Days from Jan 1')
legend('TDOC','DOC1' ,'DOC2' ,'DOC3','Observed','Fontsize',16)

saveas(gcf,['outputs/figs/avg_DOM_flux_conc'],'png');

% % Create axes
% axes('Parent',figure,...
%     'XTickLabel',{'1/1','3/1','5/1','7/1','9/1','11/1','12/31'},...
%     'XTick',[0 60 121 182 244 305 365],...
%     'FontSize',20);


%Over Lying water DOC and DOC
figure;
subplot(2,1,1)
plot(plot_time,(sum(mean_yearly_OLW_C,2)./12.)./(sum(mean_yearly_OLW_N,2)./14),'linewidth',3);
ylabel('Bottom Water C:N ');xlabel('Days from Jan 1')
subplot(2,1,2)
plot(plot_time,sum(mean_yearly_OLW_N,2),'linewidth',3)
ylabel('DON  (g N m^-^3)');xlabel('Days from Jan 1')

saveas(gcf,['outputs/figs/OLW_dom_conc'],'png');

figure;
%  subplot(2,1,1)
plot(plot_time,(sum(mean_yearly_Cfluxes,2)./12)./(sum(mean_yearly_Nfluxes,2)./14),'linewidth',3)
hold on
plot(plot_time,(sum(mean_yearly_Cconcs,2)./12)./(sum(mean_yearly_Nconcs,2)./14),'linewidth',3)
plot(plot_time,(sum(mean_yearly_OLW_C,2)./12.)./(sum(mean_yearly_OLW_N,2)./14),'linewidth',3);
redfield(1:1461)=106/16;
plot(plot_time,redfield,':','color','k','linewidth',2);% redfield ratio line, for reference
%
%plot_time,sum(mean_yearly_Cfluxes,2)./mean(yearly_JPOC,2));%,'linewidth',3);
 ylabel('C:N (mol C mol N^-^1)')
legend('Flux','Concentration','OLW Conc.')

saveas(gcf,['outputs/figs/CtoN_fluxes_concs'],'png');

figure;
% subplot(2,1,1)
plot(plot_time,sum(mean_yearly_Cfluxes,2)./(mean(yearly_JPOC,2)),'linewidth',3);
hold on
plot(plot_time,sum(mean_yearly_Nfluxes,2)./(mean(yearly_JPON,2)),'linewidth',3,'color','k');

legend('Carbon','Nitrogen')
xlabel('Days from Jan 1'); ylabel('JDOM/JPOM')


saveas(gcf,['outputs/figs/avg_net_flux'],'png');

% figure;
% % subplot(2,1,2)
% [ax,h1,h2]=plotyy(MODTIME,JPOC,MODTIME,sum(JDOC,2)./JPOC);
% hold on
% plot(MODTIME,sum(JDON,2)./JPON,'linewidth',3,'color','k');
% 
% xlabel('Days from Jan 1'); ylabel('JDOM/JPOM')
% 
% h1.LineWidth = 3;
% h2.LineWidth = 3;
% ax(1).FontSize=16
% ax(2).FontSize=16
% 
% 
% saveas(gcf,['outputs/figs/long_Net_flux'],'png');
% 

% now look at the different sources and sinks of OM remineralization

effective_rateDOC1 = 0.5*1.1.^((Nutrients.data(:,9)-20));%./2);  % temperature dependent remin rate of DOM
effective_rateDOC2 = 0.045*1.15.^((Nutrients.data(:,9)-20));%./2); 

TM1 = effective_rateDOC1.*(DOC(:,1)+DOC(:,4));  % DOC remin rate (g C m^-3 d^-1)
TM2 = effective_rateDOC2.*(DOC(:,2)+DOC(:,5));

effective_rate1 = 0.01*1.1.^((Nutrients.data(:,9)-20));%./2);  % effective rate of POC hydrolysis
effective_rate2 = 0.0018*1.15.^((Nutrients.data(:,9)-20));%./2);

HF2 = effective_rate2.*SEDPC(:,2)./2.667; % hydrolysis/fermenation rate (g C m^-3 d^-1)
HF1 = effective_rate1.*SEDPC(:,1)./2.667;

%plot the rates
% figure;
% semilogy(MODTIME,effective_rateDOC1)
% hold on
% semilogy(MODTIME,effective_rateDOC2)
% semilogy(MODTIME,effective_rate1);
% semilogy(MODTIME,effective_rate2);
% legend('kDOC1','kDOC2','kPOC1','kPOC2');
% ylabel('Rate (d^-^1');

% figure; % plot the Metabolic index (from Weston and Joye 2005);

MI1=TM1./HF1;
MI2=TM2./HF2;

% 
% plot(MODTIME,MI1);
% hold on
% plot(MODTIME,MI2);
% 
% big_TM2 = TM2+(JDOC(:,2)+JDOC(:,5))./0.1;%(DOC(:,2)+DOC(:,5)).*0.05;
% bigMI2 = big_TM2./HF2;
% 
% plot(MODTIME,bigMI2);
% ylabel('Metabolic Index (TM/HF)');
% legend('OC1','OC2','OC2 with DOM sink');
% 
% saveas(gcf,['outputs/figs/metabolic_index'],'png');

yearly_MI1=[];
yearly_MI2=[];
yearly_HF1=[];
yearly_HF2=[];
yearly_TM1 = [];
yearly_TM2 = [];
yearly_SEDPC = [];

yearly_Temp=[];
Temp = Nutrients.data(:,9) ;
SEDPC = sum(SEDPC,2);

for i = 2 : length(my_idxs_yearly);
    
    yearly_MI1 = cat(2,yearly_MI1,MI1(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_MI2 = cat(2,yearly_MI2,MI2(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_HF1 = cat(2,yearly_HF1,HF1(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_HF2 = cat(2,yearly_HF2,HF2(my_idxs_yearly(i-1):my_idxs_yearly(i),:));    
    yearly_TM1 = cat(2,yearly_TM1,TM1(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_TM2 = cat(2,yearly_TM2,TM2(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_Temp = cat(2,yearly_Temp,Temp(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
    yearly_SEDPC = cat(2,yearly_SEDPC,SEDPC(my_idxs_yearly(i-1):my_idxs_yearly(i),:));
end

avg_SEDPC = mean(yearly_SEDPC,2);
avg_MI1 = mean(yearly_MI1,2);
avg_MI2 = mean(yearly_MI2,2);

avg_HF1 = mean(yearly_HF1,2);
avg_HF2 = mean(yearly_HF2,2);

avg_TM1 = mean(yearly_TM1,2);
avg_TM2 = mean(yearly_TM2,2);
avg_Temp =mean(yearly_Temp,2);


end
 
