plot(0:25,a,'linewidth',2,'color','b');

% hold on
% a=[ 14
%     26
%     35
%     49
%     66
%     77
%     90
%    106
%    126
%    150
%    177
%    202
%    215
%    228
%    242
%    263
%    279
%    288
%    298
%    291
%    293
%    295
%    290
%    274
%    271
%    263
%    261
%    244
%    238
%    223
%    217
% ];
% plot(0:30,a,'linewidth',2,'color','r')

hold on

% plot(T_CI_vector,CI_IH_standing_bounds_matrix(1,:),'--','color','r');
% hold on 
% plot(T_CI_vector,CI_IH_standing_bounds_matrix(2,:),'+','color','r');
% hold on;
plot(T_CI_vector,(CI_IH_total_bounds_matrix(1,:)+CI_IH_total_bounds_matrix(2,:))/2,'--','linewidth',2,'color','r');
hold on 
plot(T_CI_vector,(CI_active_bounds_matrix(1,:)+CI_active_bounds_matrix(2,:))/2,'--','linewidth',2,'color','k');

% legend('recorded accumulated confirmed','recorded standing hospitalized', 'fitted standing hospitalized CI lower bound',...
%     'fitted standing hospitalized CI upper bound','fitted accumulated confirmed CI lower bound',...
%     'fitted accumulated confirmed CI upper bound',  ...
%     'fitted active CI lower bound', 'fitted active CI upper bound', ...
%     'Location','Northwest');


[legend_1,legend_2,legend_3]=xlsread('legends_2.xlsx');
legend_temp=legend_3(:,1);
title(legend_temp{21},'FontSize',25)
legend(legend_temp{1},legend_temp{23},legend_temp{24}, 'Location','Northwest','FontSize',15,'LineWidth',1.5)
% title('Italy','FontSize',25)
% legend('recorded accumulated confirmed','fitted accumulated confirmed CI lower bound',...
%     'fitted accumulated confirmed CI upper bound','fitted active virus carrier CI lower bound',  ...
%     'fitted active virus carrier CI upper bound','Location','Northwest','FontSize',15,'LineWidth',1.5);
    set(gca, 'XTick', 0:2:T_total)
    set(gca, 'XLim', [0 27])
    set(gca, 'YLim', [0 20000])
     [date_1,date_2,date_3]=xlsread('date_label.xlsx');
    date_temp=date_3(:,1);
 length_date_label=length(date_temp);
 for j=1:length_date_label
    if isnan(date_temp{j})==0
        date_label_temp=date_temp{j};
      date_label_temp(1:5)=[];
        date_temp(j)={date_label_temp};
    end
 end
    set(gca,'XTickLabel',date_temp)
% Date_vector={'Jan 23' 'Jan 25' 'Jan 27' 'Jan 29' 'Jan 31'...
%         'Feb 2' 'Feb 4' 'Feb 6' 'Feb 8' 'Feb 10'  ...
%         'Feb 12' 'Feb 14' 'Feb 16' 'Feb 18' 'Feb 20'};
%     set(gca,'XTickLabel',Date_vector)

% figure
% [N_end,EDGES_end]=histcounts(CI_end_time_vector,'BinWidth',0.5);
% density_em_end_vector=N_end/sample_total/0.5;
% l_temp=length(EDGES_end);
% time_hist_vector=(EDGES_end(1:l_temp-1)+EDGES_end(2:l_temp))/2;
% bar(time_hist_vector,density_em_end_vector);
% 
% hold on;
% [T_end_mu, T_end_sigma]=normfit(CI_end_time_vector,p_ci);
% Y=normpdf(0:0.1:T_total,T_end_mu, T_end_sigma);
% plot(0:0.1:T_total,Y,'--','color','m','linewidth',2);
% 
% M=max(Y);
% plot([T_end_mu+2*T_end_sigma T_end_mu+2*T_end_sigma], [0 M],'--','color','r','linewidth',2)
% hold on
% plot([T_end_mu T_end_mu], [0 M],'--','color','g','linewidth',2)
% hold on
% plot([T_end_mu-2*T_end_sigma T_end_mu-2*T_end_sigma], [0 M],'--','color','k','linewidth',2)
% title('Beijing','FontSize',25)
% % title(legend_temp{18},'FontSize',20)
% legend('recaled histogram of ending time', 'fitted ending time distribution',...
%     ['end time CI upper bound=' num2str(T_end_mu+2*T_end_sigma) ', p=' num2str(p_ci)], ...
%     ['expected end time=' num2str(T_end_mu)], ...
%     ['end time CI lower bound=' num2str(T_end_mu-2*T_end_sigma) ', p=' num2str(p_ci)],'Location','Northwest','FontSize',15,'LineWidth',1.5);
% % legend(legend_temp{9}, legend_temp{10},...
% %     [legend_temp{11} num2str(T_end_mu+2*T_end_sigma) ', p=' num2str(p_ci)], ...
% %     [legend_temp{12} num2str(T_end_mu)], ...
% %     [legend_temp{13} num2str(T_end_mu-2*T_end_sigma) ', p=' num2str(p_ci)],'Location','Northeast','FontSize',15,'LineWidth',1.5);
% set(gca, 'XTick', 0:2:T_total)
% set(gca, 'XLim', [0 30])
% set(gca,'XTickLabel',date_temp)

figure
CI_Rc_bounds_matrix=zeros(2,Rc_num);
CI_bounds_proportion_of_AE=zeros(2,Rc_num);
CI_bounds_proportion_of_A=zeros(2,Rc_num);

for i=1:Rc_num
    Rc_data_temp=CI_Rc_matrix(:,i);
    [Rc_mu, Rc_sigma, Rc_mu_ci, Rc_sigma_ci]=normfit(Rc_data_temp,p_ci);    
    CI_Rc_bounds_matrix(:,i)=[Rc_mu-2*Rc_sigma Rc_mu+2*Rc_sigma]';
    
    proportion_temp=CI_proportion_of_AE(:,i);
    [prop_mu, prop_sigma, prop_ci, prop_sigma_ci]=normfit(proportion_temp,p_ci);
    CI_bounds_proportion_of_AE(:,i)=[prop_mu-2*prop_sigma prop_mu+2*prop_sigma];
    
    proportion_temp=CI_proportion_of_A(:,i);
    [prop_mu, prop_sigma, prop_ci, prop_sigma_ci]=normfit(proportion_temp,p_ci);
    CI_bounds_proportion_of_A(:,i)=[prop_mu-2*prop_sigma prop_mu+2*prop_sigma];
end

valid_data_length=find(isnan(CI_Rc_bounds_matrix(1,:))==0);
valid_data_length=valid_data_length(end);

plot(T_Rc_vector(1:valid_data_length),mean(CI_Rc_bounds_matrix(:,1:valid_data_length)),'c+:','MarkerSize',6, 'linewidth',2);

title(legend_temp{21},'FontSize',25)
legend(legend_temp{14},'FontSize',15,'LineWidth',1.5)
% title('Italy','FontSize',25)
% legend('Expectation of the estiamted Controlled Reproduction Number R_c over time ','FontSize',15,'LineWidth',1.5)
set(gca, 'YTick', 0:1:3)
set(gca, 'YLim', [0 3])
set(gca, 'XTick', 0:2:40)
set(gca,'XTickLabel',date_temp)

active_two_week_ago=mean(active_number(19:46));
active_two_week_later=mean(active_number(47:74));


figure
plot(T_Rc_vector(1:Rc_num),nanmean(CI_proportion_of_AE(:,1:Rc_num)),'r+:','MarkerSize',6, 'linewidth',2);
hold on
plot(T_Rc_vector(1:Rc_num),nanmean(CI_proportion_of_A(:,1:Rc_num)),'c+:','MarkerSize',6, 'linewidth',2);
title(legend_temp{21},'FontSize',25)
legend(legend_temp{15},legend_temp{16},'FontSize',15,'LineWidth',1.5)
% title('Italy','FontSize',25)
% legend('Expectation of the proportion of patients that are infected by A and E',...
% 'Expectation of the proportion of patients that are infected by A','FontSize',15,'LineWidth',1.5,'Location','Northwest')
set(gca, 'YTick', 0:0.1:1)
set(gca, 'YLim', [0 1])
set(gca, 'XTick', 0:2:40)
set(gca, 'XLim', [0 40])
set(gca,'XTickLabel',date_temp)


