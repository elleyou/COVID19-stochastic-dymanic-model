sample_total=1000;
T_total=100;
CI_step=0.5;
T_CI_vector=CI_step:CI_step:T_total-CI_step*0.1;
CI_num=length(T_CI_vector);
p_ci=0.05;

CI_RH_matrix=zeros(sample_total,CI_num);
CI_IH_total_matrix=zeros(sample_total,CI_num);
CI_Active_matrix=zeros(sample_total,CI_num);

Threshold_end=10; %we call the epidemic reaches an end if the total active person is less than this threshold! 
CI_end_time_vector=zeros(sample_total,1);

Rc_step=2; %time interval of calculating Rc
T_Rc_vector=Rc_step:Rc_step:T_total-CI_step*0.1;
Rc_num=length(T_Rc_vector);
CI_Rc_matrix=zeros(sample_total,Rc_num);
CI_proportion_of_AE=zeros(sample_total,Rc_num);
CI_proportion_of_A=zeros(sample_total,Rc_num);
for sample_id=1:sample_total
    t_end_temp=0;
    find_id_end=0;

    N=21540000; %total population
    
    lambda_N=0.416;%infection rate of IN
    theta=0.467; %factor for lambda_E=lambda_A=lambda_N*theta;
    lambda_E=lambda_N*theta; %infection rate of the E, same as A
    lambda_A=lambda_N*theta; %infection rate of the A, same as E

    rho=0.675; %probabiity an exposed person will grow symptom (go to A or E)
    q=0; %probability an exposed person is tracible
%     q=0;

    r_q=1/(10.9-4.3); %rate that E_1, A_1 and IN get quarantined 
    r_s=1/5.3;%rate that E_1, E_2 and E_q grow symptom
    gamma_A=0.1;% the rate A no longer carry virus
    r_H=1/(5.6);% the rate IN to go hospital

    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Please be advised that these parameters are NOT estiamted or fit from
    %real world data!!
    p_l=1-0.0088; %probability a newly hospitialize person is light/severe. NOT estimated!
    delta_IN=0; %death rate of IN. This assumed to be 0 in the parameter estimate
    gamma_IN=0; %recover rate of IN. This assumed to be 0 in the parameter estimate
    delta_IH=0.43073; % death rate of IHS NOT estimated!
    r_b=0.35260; %rate an IHS change to IHL NOT estimated!
    
    c=0.4433;
    %c_1=1.1;
    gamma_IH=0.0367*c;%recovery rate of IHL NOT estimated!
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    IN0 = 48;
%     d = 2.42;
    E0 = fix(72);
    E1_t=fix(E0*rho*(1-q)); %incubation, non-tracible
    E2_t=fix(E0*rho*q); %incubation, tracible
    A1_t=fix(E0*(1-rho)*(1-q)); %asymptomatic non-tracible
    A2_t=fix(E0*(1-rho)*q); %asymptomatic tracible
    IN1_t=fix(IN0*(1-q)); %symptom non-tracible
    IN2_t=fix(IN0*q); %symptom tracible
    
    Eq_t=0; %incubation quarantined
    Aq_t=0; %asymptomatic quarantined
    IHL_t=14; %hospitalized light
    IHS_t=0; %hospitalized severe
    R_t=0; %recovered
    D_t=0; %dead
    
    RH_t=0;
    IH_total=14;
    Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
    
    S_t=N-Active_t-IH_total;
    
    Rc_in_count=0;
    Rc_out_count=0;
    t_rc=0;
    Rc_record_vector=zeros(1,0);
    
    count_IN=0;
    count_A=0;
    count_E=0;
    accumulate_count_A=0;
    accumulate_count_E=0;
    accumulate_count_IN=0;
    proportion_of_AE=zeros(1,0);
    proportion_of_A=zeros(1,0);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    t_temp=0;
    threshold_record=0.1;
    t_record=0;
    t_ci=0;
    
    E_record_vector=E1_t+E2_t+Eq_t;
    A_record_vector=A1_t+A2_t+Aq_t;
    IN_record_vector=IN1_t+IN2_t;
    IH_record_vector=IHL_t+IHS_t;
    R_record_vector=R_t;
    D_record_vector=D_t;
    T_record_vector=0;
    
    RH_record_vector=RH_t;
    IH_total_record_vector=IH_total;
    Active_record_vector=Active_t;
    
    RH_ci_vector=zeros(1,0);
    IH_total_ci_vector=zeros(1,0);
    Active_ci_vector=zeros(1,0);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    T_control=7; %when control measrues are implemented
    a=0.0985; %strength of control: all lambda's reduced to lambda*a
    b=3; %increased efficiency: mean time of quarantine and hospitalization
    %decreased by b days!
    T_heal_impr=15;
    
    
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    fprintf(['simulating copy: ' num2str(sample_id) '\n']);
    while t_temp<T_control
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_record>threshold_record
            E_record_vector=[E_record_vector E1_t+E2_t+Eq_t];
            A_record_vector=[A_record_vector A1_t+A2_t+Aq_t];
            IN_record_vector=[IN_record_vector IN1_t+IN2_t];
            IH_record_vector=[IH_record_vector IHL_t+IHS_t];
            R_record_vector=[R_record_vector R_t];
            D_record_vector=[D_record_vector D_t];
            RH_record_vector=[RH_record_vector RH_t];
            IH_total_record_vector=[IH_total_record_vector IH_total];
            
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_record_vector=[Active_record_vector Active_t];
            
            T_record_vector=[T_record_vector t_temp];
            %         plot(T_record_vector, E_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, A_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IN_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IH_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, R_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, D_record_vector,'linewidth',2);
            %         legend('total exposed population','total asymptomatic population',...
            %             'total symptomatic but not hospitalized population','total hospitalized population','total recovered population','total dead population','Location','northwest');
            %         strings='simulation MAY NOT precisely reflect reality, due to modelling and that parameters may not always be precise and up-to-date';
            %         annotation('textbox',[0.4,0.8,0.3,0.1],'LineStyle','-','LineWidth',2,'String',strings);
            %         drawnow;
            %         hold off
            t_record=0;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        infection_rate_total=lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t);
        quarantine_rate_total=r_q*(E2_t+A2_t+IN2_t);
        symptom_rate_total=r_s*(E1_t+E2_t+Eq_t);
        hospitalization_rate_total=r_H*(IN1_t+IN2_t);
        turn_rate_total=IHS_t*r_b;
        recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
        death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
        rates_vector=[infection_rate_total quarantine_rate_total symptom_rate_total hospitalization_rate_total turn_rate_total recovery_rate_total death_rate_total];
        rate_total=sum(rates_vector);
        delta_t=log(1/rand)/rate_total;
        
        t_ci=t_ci+delta_t;
        if t_ci>CI_step 
            RH_ci_vector=[RH_ci_vector RH_t];
            IH_total_ci_vector=[IH_total_ci_vector IH_total];
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_ci_vector=[Active_ci_vector Active_t];
            t_ci=t_ci-CI_step;
        end
        
        
        t_rc=t_rc+delta_t;
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_rc>Rc_step
            if Rc_out_count>0
                Rc_temp=Rc_in_count/Rc_out_count;
            else
                Rc_temp=NaN;
            end
            Rc_record_vector=[Rc_record_vector Rc_temp];
            Rc_in_count=0;
            Rc_out_count=0;
            
            if count_A+count_E+count_IN==0
                proportion_of_AE=[proportion_of_AE   NaN];
                proportion_of_A=[proportion_of_A   NaN];
            end
            if count_A+count_E+count_IN>0
                proportion_of_AE=[proportion_of_AE   (count_A+count_E)/(count_A+count_E+count_IN)];
                proportion_of_A=[proportion_of_A   count_A/(count_A+count_E+count_IN)];
            end
 
            count_A=0;
            count_E=0;
            count_IN=0;
            t_rc=t_rc-Rc_step;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        transition_sampler=rand*rate_total;
        if transition_sampler<sum(rates_vector(1:1))% transmission
            Rc_in_count=Rc_in_count+1;
            transmission_id=rand*N;
            if transmission_id<S_t %transmission sucessful
                S_t=S_t-1;
                symptom_id=rand;
                trace_id=rand;
                
                ran_loc=rand*(lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t));
                if ran_loc<lambda_A*(A1_t+A2_t)  %%infected by A 
                    count_A=count_A+1;
                    accumulate_count_A=accumulate_count_A+1;
                end
                if (ran_loc>lambda_A*(A1_t+A2_t) && ran_loc <lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t))  %%infected by E 
                    count_E=count_E+1;
                    accumulate_count_E=accumulate_count_E+1;
                end
                if ran_loc>lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)  %%%infected by IN 
                    count_IN=count_IN+1;
                    accumulate_count_IN=accumulate_count_IN+1;
                end

                
                if symptom_id<rho && trace_id>q %symptomatic and non-tracible
                    E1_t=E1_t+1;
                elseif symptom_id<rho && trace_id<=q %symptomatic and tracible
                    E2_t=E2_t+1;
                elseif symptom_id>=rho && trace_id>q %asymptomatic and non-tracible
                    A1_t=A1_t+1;
                else %asymptomatic and tracible
                    A2_t=A2_t+1;
                end
            end
        elseif  transition_sampler<sum(rates_vector(1:2))% quarantine
            Rc_out_count=Rc_out_count+1;
            quarantine_sampler=rand*(E2_t+A2_t+IN2_t); %determine which type is quarantined
            if quarantine_sampler<E2_t % an E2 get quarantined
                E2_t=E2_t-1;
                Eq_t=Eq_t+1;
            elseif quarantine_sampler<E2_t+A2_t % an A2 get quarantined
                A2_t=A2_t-1;
                Aq_t=Aq_t+1;
            else % an IN2 get hospitalized immediately
                IN2_t=IN2_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:3))% symptom
            symptom_sampler=rand*(E1_t+E2_t+Eq_t); %determine which type is quarantined
            if symptom_sampler<E1_t %an E1_t grows symptom to IN1_t
                E1_t=E1_t-1;
                IN1_t=IN1_t+1;
            elseif symptom_sampler<E1_t+E2_t %an E2_t grows symptom to IN2_t
                E2_t=E2_t-1;
                IN2_t=IN2_t+1;
            else % an Eq_t directly get hospitalized
                Eq_t=Eq_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:4))% hospitalization
            Rc_out_count=Rc_out_count+1;
            symptom_sampler=rand*(IN1_t+IN2_t);
            if symptom_sampler<IN1_t %an IN_1 get hospitalized
                IN1_t=IN1_t-1;
            else
                IN2_t=IN2_t-1; %an IN_2 get hospitalized
            end
            severeness_id=rand;
            if severeness_id<p_l %light
                IHL_t=IHL_t+1;
            else %severe
                IHS_t=IHS_t+1;
            end
            IH_total=IH_total+1;
        elseif transition_sampler<sum(rates_vector(1:5)) %severe turn to light
            IHS_t=IHS_t-1;
            IHL_t=IHL_t+1;
        elseif transition_sampler<sum(rates_vector(1:6)) %recovery
            %recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
            recover_sampler=rand*recovery_rate_total;
            if recover_sampler<gamma_A*A1_t %an A1 recovers
                Rc_out_count=Rc_out_count+1;
                A1_t=A1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t)%an A2 recovers
                Rc_out_count=Rc_out_count+1;
                A2_t=A2_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)%an Aq recovers
                Aq_t=Aq_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*IN1_t%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                R_t=R_t+1;
            else %an IHL recovers
                IHL_t=IHL_t-1;
                R_t=R_t+1;
                RH_t=RH_t+1;
            end
        else %death
            %death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
            death_sampler=rand*death_rate_total;
            if death_sampler<delta_IN*IN1_t %an IN1 dies
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                D_t=D_t+1;
            elseif death_sampler<delta_IN*(IN1_t+IN2_t) %an IN2 dies
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                D_t=D_t+1;
            else %an IHS dies
                IHS_t=IHS_t-1;
                D_t=D_t+1;
            end
        end
        t_temp=t_temp+delta_t;
        t_record=t_record+delta_t;
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %at this point the control measure is implemented!
    lambda_N=lambda_N*a;%infection rate of IN
    lambda_E=lambda_N*theta; %infection rate of the E, same as A
    lambda_A=lambda_N*theta; %infection rate of the A, same as E
    r_q=1/3.6; %mean quarantine time reduce by b days
    r_H=1/5.6; %mean hospitalization time reduce by b days
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    while t_temp<T_heal_impr
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_record>threshold_record
            E_record_vector=[E_record_vector E1_t+E2_t+Eq_t];
            A_record_vector=[A_record_vector A1_t+A2_t+Aq_t];
            IN_record_vector=[IN_record_vector IN1_t+IN2_t];
            IH_record_vector=[IH_record_vector IHL_t+IHS_t];
            R_record_vector=[R_record_vector R_t];
            D_record_vector=[D_record_vector D_t];
            RH_record_vector=[RH_record_vector RH_t];
            IH_total_record_vector=[IH_total_record_vector IH_total];
            
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_record_vector=[Active_record_vector Active_t];
            
            T_record_vector=[T_record_vector t_temp];
            %         plot(T_record_vector, E_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, A_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IN_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IH_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, R_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, D_record_vector,'linewidth',2);
            %         legend('total exposed population','total asymptomatic population',...
            %             'total symptomatic but not hospitalized population','total hospitalized population','total recovered population','total dead population','Location','northwest');
            %         strings='simulation MAY NOT precisely reflect reality, due to modelling and that parameters may not always be precise and up-to-date';
            %         annotation('textbox',[0.4,0.8,0.3,0.1],'LineStyle','-','LineWidth',2,'String',strings);
            %         drawnow;
            %         hold off
            t_record=0;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        infection_rate_total=lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t);
        quarantine_rate_total=r_q*(E2_t+A2_t+IN2_t);
        symptom_rate_total=r_s*(E1_t+E2_t+Eq_t);
        hospitalization_rate_total=r_H*(IN1_t+IN2_t);
        turn_rate_total=IHS_t*r_b;
        recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
        death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
        rates_vector=[infection_rate_total quarantine_rate_total symptom_rate_total hospitalization_rate_total turn_rate_total recovery_rate_total death_rate_total];
        rate_total=sum(rates_vector);
        delta_t=log(1/rand)/rate_total;
        
        t_ci=t_ci+delta_t;
        if t_ci>CI_step 
            RH_ci_vector=[RH_ci_vector RH_t];
            IH_total_ci_vector=[IH_total_ci_vector IH_total];
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_ci_vector=[Active_ci_vector Active_t];
            t_ci=t_ci-CI_step;
        end
        
        
        t_rc=t_rc+delta_t;
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_rc>Rc_step
            if Rc_out_count>0
                Rc_temp=Rc_in_count/Rc_out_count;
            else
                Rc_temp=NaN;
            end
            Rc_record_vector=[Rc_record_vector Rc_temp];
            Rc_in_count=0;
            Rc_out_count=0;
            
            if count_A+count_E+count_IN==0
                proportion_of_AE=[proportion_of_AE   NaN];
                proportion_of_A=[proportion_of_A   NaN];
            end
            if count_A+count_E+count_IN>0
                proportion_of_AE=[proportion_of_AE   (count_A+count_E)/(count_A+count_E+count_IN)];
                proportion_of_A=[proportion_of_A   count_A/(count_A+count_E+count_IN)];
            end
 
            count_A=0;
            count_E=0;
            count_IN=0;
            
            t_rc=t_rc-Rc_step;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        transition_sampler=rand*rate_total;
        if transition_sampler<sum(rates_vector(1:1))% transmission
            Rc_in_count=Rc_in_count+1;
            transmission_id=rand*N;
            if transmission_id<S_t %transmission sucessful
                S_t=S_t-1;
                symptom_id=rand;
                trace_id=rand;
                
                % lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t)
              
                ran_loc=rand*(lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t));
                if ran_loc<lambda_A*(A1_t+A2_t)  %%infected by A 
                    count_A=count_A+1;
                    accumulate_count_A=accumulate_count_A+1;
                end
                if (ran_loc>lambda_A*(A1_t+A2_t) && ran_loc < lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t))  %%infected by E 
                    count_E=count_E+1;
                    accumulate_count_E=accumulate_count_E+1;
                end
                if ran_loc>lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t) %%%infected by IN 
                    count_IN=count_IN+1;
                    accumulate_count_IN=accumulate_count_IN+1;
                end
                
                if symptom_id<rho && trace_id>q %symptomatic and non-tracible
                    E1_t=E1_t+1;
                elseif symptom_id<rho && trace_id<=q %symptomatic and tracible
                    E2_t=E2_t+1;
                elseif symptom_id>=rho && trace_id>q %asymptomatic and non-tracible
                    A1_t=A1_t+1;
                else %asymptomatic and tracible
                    A2_t=A2_t+1;
                end
            end
        elseif  transition_sampler<sum(rates_vector(1:2))% quarantine
            Rc_out_count=Rc_out_count+1;
            quarantine_sampler=rand*(E2_t+A2_t+IN2_t); %determine which type is quarantined
            if quarantine_sampler<E2_t % an E2 get quarantined
                E2_t=E2_t-1;
                Eq_t=Eq_t+1;
            elseif quarantine_sampler<E2_t+A2_t % an A2 get quarantined
                A2_t=A2_t-1;
                Aq_t=Aq_t+1;
            else % an IN2 get hospitalized immediately
                IN2_t=IN2_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:3))% symptom
            symptom_sampler=rand*(E1_t+E2_t+Eq_t); %determine which type is quarantined
            if symptom_sampler<E1_t %an E1_t grows symptom to IN1_t
                E1_t=E1_t-1;
                IN1_t=IN1_t+1;
            elseif symptom_sampler<E1_t+E2_t %an E2_t grows symptom to IN2_t
                E2_t=E2_t-1;
                IN2_t=IN2_t+1;
            else % an Eq_t directly get hospitalized
                Eq_t=Eq_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:4))% hospitalization
            Rc_out_count=Rc_out_count+1;
            symptom_sampler=rand*(IN1_t+IN2_t);
            if symptom_sampler<IN1_t %an IN_1 get hospitalized
                IN1_t=IN1_t-1;
            else
                IN2_t=IN2_t-1; %an IN_2 get hospitalized
            end
            severeness_id=rand;
            if severeness_id<p_l %light
                IHL_t=IHL_t+1;
            else %severe
                IHS_t=IHS_t+1;
            end
            IH_total=IH_total+1;
        elseif transition_sampler<sum(rates_vector(1:5)) %severe turn to light
            IHS_t=IHS_t-1;
            IHL_t=IHL_t+1;
        elseif transition_sampler<sum(rates_vector(1:6)) %recovery
            %recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
            recover_sampler=rand*recovery_rate_total;
            if recover_sampler<gamma_A*A1_t %an A1 recovers
                Rc_out_count=Rc_out_count+1;
                A1_t=A1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t)%an A2 recovers
                Rc_out_count=Rc_out_count+1;
                A2_t=A2_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)%an Aq recovers
                Aq_t=Aq_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*IN1_t%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                R_t=R_t+1;
            else %an IHL recovers
                IHL_t=IHL_t-1;
                R_t=R_t+1;
                RH_t=RH_t+1;
            end
        else %death
            %death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
            death_sampler=rand*death_rate_total;
            if death_sampler<delta_IN*IN1_t %an IN1 dies
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                D_t=D_t+1;
            elseif death_sampler<delta_IN*(IN1_t+IN2_t) %an IN2 dies
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                D_t=D_t+1;
            else %an IHS dies
                IHS_t=IHS_t-1;
                D_t=D_t+1;
            end
        end
        t_temp=t_temp+delta_t;
        t_record=t_record+delta_t;
    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    gamma_IH=0.0367;
    while t_temp<T_total
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_record>threshold_record
            E_record_vector=[E_record_vector E1_t+E2_t+Eq_t];
            A_record_vector=[A_record_vector A1_t+A2_t+Aq_t];
            IN_record_vector=[IN_record_vector IN1_t+IN2_t];
            IH_record_vector=[IH_record_vector IHL_t+IHS_t];
            R_record_vector=[R_record_vector R_t];
            D_record_vector=[D_record_vector D_t];
            RH_record_vector=[RH_record_vector RH_t];
            IH_total_record_vector=[IH_total_record_vector IH_total];
            
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_record_vector=[Active_record_vector Active_t];
            
            T_record_vector=[T_record_vector t_temp];
            %         plot(T_record_vector, E_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, A_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IN_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, IH_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, R_record_vector,'linewidth',2);
            %         hold on
            %         plot(T_record_vector, D_record_vector,'linewidth',2);
            %         legend('total exposed population','total asymptomatic population',...
            %             'total symptomatic but not hospitalized population','total hospitalized population','total recovered population','total dead population','Location','northwest');
            %         strings='simulation MAY NOT precisely reflect reality, due to modelling and that parameters may not always be precise and up-to-date';
            %         annotation('textbox',[0.4,0.8,0.3,0.1],'LineStyle','-','LineWidth',2,'String',strings);
            %         drawnow;
            %         hold off
            t_record=0;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        infection_rate_total=lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t);
        quarantine_rate_total=r_q*(E2_t+A2_t+IN2_t);
        symptom_rate_total=r_s*(E1_t+E2_t+Eq_t);
        hospitalization_rate_total=r_H*(IN1_t+IN2_t);
        turn_rate_total=IHS_t*r_b;
        recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
        death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
        rates_vector=[infection_rate_total quarantine_rate_total symptom_rate_total hospitalization_rate_total turn_rate_total recovery_rate_total death_rate_total];
        rate_total=sum(rates_vector);
        delta_t=log(1/rand)/rate_total;
        
        t_ci=t_ci+delta_t;
        if t_ci>CI_step 
            RH_ci_vector=[RH_ci_vector RH_t];
            IH_total_ci_vector=[IH_total_ci_vector IH_total];
            Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
            Active_ci_vector=[Active_ci_vector Active_t];
            t_ci=t_ci-CI_step;
        end
        
        
        t_rc=t_rc+delta_t;
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if t_rc>Rc_step
            if Rc_out_count>0
                Rc_temp=Rc_in_count/Rc_out_count;
            else
                Rc_temp=NaN;
            end
            Rc_record_vector=[Rc_record_vector Rc_temp];
            Rc_in_count=0;
            Rc_out_count=0;
            
            if count_A+count_E+count_IN==0
                proportion_of_AE=[proportion_of_AE   NaN];
                proportion_of_A=[proportion_of_A   NaN];
            end
            if count_A+count_E+count_IN>0
                proportion_of_AE=[proportion_of_AE   (count_A+count_E)/(count_A+count_E+count_IN)];
                proportion_of_A=[proportion_of_A   count_A/(count_A+count_E+count_IN)];
            end
 
            count_A=0;
            count_E=0;
            count_IN=0;
            
            t_rc=t_rc-Rc_step;
        end
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        transition_sampler=rand*rate_total;
        if transition_sampler<sum(rates_vector(1:1))% transmission
            Rc_in_count=Rc_in_count+1;
            transmission_id=rand*N;
            if transmission_id<S_t %transmission sucessful
                S_t=S_t-1;
                symptom_id=rand;
                trace_id=rand;
                
                %lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)
                
                ran_loc=rand*(lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)+lambda_N*(IN1_t+IN2_t));
                if ran_loc<lambda_A*(A1_t+A2_t)  %%infected by A 
                    count_A=count_A+1;
                    accumulate_count_A=accumulate_count_A+1;
                end
                if (ran_loc>lambda_A*(A1_t+A2_t) && ran_loc < lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t))  %%infected by E 
                    count_E=count_E+1;
                    accumulate_count_E=accumulate_count_E+1;
                end
                if ran_loc>lambda_E*(E1_t+E2_t)+lambda_A*(A1_t+A2_t)  %%%infected by IN 
                    count_IN=count_IN+1;
                    accumulate_count_IN=accumulate_count_IN+1;
                end
                
                if symptom_id<rho && trace_id>q %symptomatic and non-tracible
                    E1_t=E1_t+1;
                elseif symptom_id<rho && trace_id<=q %symptomatic and tracible
                    E2_t=E2_t+1;
                elseif symptom_id>=rho && trace_id>q %asymptomatic and non-tracible
                    A1_t=A1_t+1;
                else %asymptomatic and tracible
                    A2_t=A2_t+1;
                end
            end
        elseif  transition_sampler<sum(rates_vector(1:2))% quarantine
            Rc_out_count=Rc_out_count+1;
            quarantine_sampler=rand*(E2_t+A2_t+IN2_t); %determine which type is quarantined
            if quarantine_sampler<E2_t % an E2 get quarantined
                E2_t=E2_t-1;
                Eq_t=Eq_t+1;
            elseif quarantine_sampler<E2_t+A2_t % an A2 get quarantined
                A2_t=A2_t-1;
                Aq_t=Aq_t+1;
            else % an IN2 get hospitalized immediately
                IN2_t=IN2_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:3))% symptom
            symptom_sampler=rand*(E1_t+E2_t+Eq_t); %determine which type is quarantined
            if symptom_sampler<E1_t %an E1_t grows symptom to IN1_t
                E1_t=E1_t-1;
                IN1_t=IN1_t+1;
            elseif symptom_sampler<E1_t+E2_t %an E2_t grows symptom to IN2_t
                E2_t=E2_t-1;
                IN2_t=IN2_t+1;
            else % an Eq_t directly get hospitalized
                Eq_t=Eq_t-1;
                severeness_id=rand;
                if severeness_id<p_l %light
                    IHL_t=IHL_t+1;
                else %severe
                    IHS_t=IHS_t+1;
                end
                IH_total=IH_total+1;
            end
        elseif transition_sampler<sum(rates_vector(1:4))% hospitalization
            Rc_out_count=Rc_out_count+1;
            symptom_sampler=rand*(IN1_t+IN2_t);
            if symptom_sampler<IN1_t %an IN_1 get hospitalized
                IN1_t=IN1_t-1;
            else
                IN2_t=IN2_t-1; %an IN_2 get hospitalized
            end
            severeness_id=rand;
            if severeness_id<p_l %light
                IHL_t=IHL_t+1;
            else %severe
                IHS_t=IHS_t+1;
            end
            IH_total=IH_total+1;
        elseif transition_sampler<sum(rates_vector(1:5)) %severe turn to light
            IHS_t=IHS_t-1;
            IHL_t=IHL_t+1;
        elseif transition_sampler<sum(rates_vector(1:6)) %recovery
            %recovery_rate_total=gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)+gamma_IH*IHL_t;
            recover_sampler=rand*recovery_rate_total;
            if recover_sampler<gamma_A*A1_t %an A1 recovers
                Rc_out_count=Rc_out_count+1;
                A1_t=A1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t)%an A2 recovers
                Rc_out_count=Rc_out_count+1;
                A2_t=A2_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)%an Aq recovers
                Aq_t=Aq_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*IN1_t%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                R_t=R_t+1;
            elseif recover_sampler<gamma_A*(A1_t+A2_t+Aq_t)+gamma_IN*(IN1_t+IN2_t)%an IN1 recovers
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                R_t=R_t+1;
            else %an IHL recovers
                IHL_t=IHL_t-1;
                R_t=R_t+1;
                RH_t=RH_t+1;
            end
        else %death
            %death_rate_total=delta_IN*(IN1_t+IN2_t)+delta_IH*IHS_t;
            death_sampler=rand*death_rate_total;
            if death_sampler<delta_IN*IN1_t %an IN1 dies
                Rc_out_count=Rc_out_count+1;
                IN1_t=IN1_t-1;
                D_t=D_t+1;
            elseif death_sampler<delta_IN*(IN1_t+IN2_t) %an IN2 dies
                Rc_out_count=Rc_out_count+1;
                IN2_t=IN2_t-1;
                D_t=D_t+1;
            else %an IHS dies
                IHS_t=IHS_t-1;
                D_t=D_t+1;
            end
        end
        t_temp=t_temp+delta_t;
        t_record=t_record+delta_t;
        Active_t=E1_t+E2_t+A1_t+A2_t+IN1_t+IN2_t;
        if Active_t<Threshold_end && find_id_end==0% the first time Threshold_end has been reached
            t_end_temp=t_temp;
            find_id_end=1;
        end
    end
    
    if t_end_temp<T_total %the epidemic ends before T_total
        CI_end_time_vector(sample_id)=t_end_temp;
    else
        fprintf('T_total is too small! Re-set!!\n');
        break;
    end
    %     a=[ 51   76   96  144  184  236  305  382  509  592  669  777  838  895  965  879  994 1007  995  977  955  927  906  878  845  794];
    %     hold on
    %     plot(0:25,a,'*')
    %     hold on
    %     plot(T_record_vector,E_record_vector+A_record_vector+IN_record_vector+IH_record_vector);
    %     legend('total exposed population','total asymptomatic population',...
    %         'total symptomatic but not hospitalized population',...
    %         'total hospitalized population','total recovered population', ...
    %         'total dead population','Recorded hopsitalized population',...
    %         'Total infected population','Location','northwest')
    %     figure
%     Untitled3;
%     set(gca, 'XTick', 0:2:T_total)
%     set(gca, 'YLim', [0 2000])
%     
%     [date_1,date_2,date_3]=xlsread('date_label.xlsx');
%     date_temp=date_3(:,1);
%     set(gca,'XTickLabel',date_temp)
%     
% %     Date_vector={'Jan 23' 'Jan 25' 'Jan 27' 'Jan 29' 'Jan 31'...
% %         'Feb 2' 'Feb 4' 'Feb 6' 'Feb 8' 'Feb 10'  ...
% %         'Feb 12' 'Feb 14' 'Feb 16' 'Feb 18' 'Feb 20'};
% %     set(gca,'XTickLabel',Date_vector)
%     
%     saveas(gca,['Sun_0220_1508_' num2str(sample_id) '.jpg'])
%     close
    %%
    %this part deal with the case that the last ONE jump is longer than the CI
    %record step length, then this means the state is the same as at
    %T_total!!
    if length(RH_ci_vector)<CI_num
        RH_ci_vector=[RH_ci_vector RH_t*ones(1,CI_num-length(RH_ci_vector))];
    end
    
    if length(IH_total_ci_vector)<CI_num
        IH_total_ci_vector=[IH_total_ci_vector IH_total*ones(1,CI_num-length(IH_total_ci_vector))];
    end
    
    if length(Active_ci_vector)<CI_num
        Active_ci_vector=[Active_ci_vector Active_t*ones(1,CI_num-length(Active_ci_vector))];
    end
    
    if length(Rc_record_vector)<Rc_num
        if Rc_out_count>0
            Rc_temp=Rc_in_count/Rc_out_count;
        else
            Rc_temp=NaN;
        end
        Rc_record_vector=[Rc_record_vector Rc_temp*ones(1,Rc_num-length(Rc_record_vector))];
    end
    
    if length(proportion_of_AE)<Rc_num
        if count_A+count_E+count_IN==0
                proportion_of_AE=[proportion_of_AE   NaN*ones(1,Rc_num-length(proportion_of_AE))];
        end
        if count_A+count_E+count_IN>0
                proportion_of_AE=[proportion_of_AE   (count_A+count_E)/(count_A+count_E+count_IN)*ones(1,Rc_num-length(proportion_of_AE))];
        end 
    end
    
    if length(proportion_of_A)<Rc_num
       if count_A+count_E+count_IN==0
                proportion_of_A=[proportion_of_A   NaN*ones(1,Rc_num-length(proportion_of_A))];
        end
        if count_A+count_E+count_IN>0
                proportion_of_A=[proportion_of_A   (count_A+count_E)/(count_A+count_E+count_IN)*ones(1,Rc_num-length(proportion_of_A))];
        end 
    end
    %%
    CI_RH_matrix(sample_id,:)=RH_ci_vector(1:CI_num);
    CI_IH_total_matrix(sample_id,:)=IH_total_ci_vector(1:CI_num);
    CI_Active_matrix(sample_id,:)=Active_ci_vector(1:CI_num);
    CI_Rc_matrix(sample_id,:)=Rc_record_vector(1:Rc_num);
    CI_proportion_of_AE(sample_id,:)=proportion_of_AE(1:Rc_num);
    CI_proportion_of_A(sample_id,:)=proportion_of_A(1:Rc_num);
end
%save('CI_data.mat','CI_IH_total_matrix','CI_IH_total_matrix');

CI_IH_standing_matrix=CI_IH_total_matrix-CI_RH_matrix;

CI_IH_total_bounds_matrix=zeros(2,CI_num);
CI_IH_standing_bounds_matrix=zeros(2,CI_num);
CI_active_bounds_matrix=zeros(2,CI_num);

for i=1:CI_num
    standing_data_temp=CI_IH_standing_matrix(:,i);
    [standing_mu, standing_sigma, standing_mu_ci, standing_sigma_ci]=normfit(standing_data_temp,p_ci);
    
    total_data_temp=CI_IH_total_matrix(:,i);
    [total_mu, total_sigma, total_mu_ci, total_sigma_ci]=normfit(total_data_temp,p_ci);
    
    active_data_temp=CI_Active_matrix(:,i);
    [active_mu, active_sigma, active_mu_ci, active_sigma_ci]=normfit(active_data_temp,p_ci);    
    
    CI_IH_total_bounds_matrix(:,i)=[total_mu-2*total_sigma total_mu+2*total_sigma]';
    CI_IH_standing_bounds_matrix(:,i)=[standing_mu-2*standing_sigma standing_mu+2*standing_sigma]';
    CI_active_bounds_matrix(:,i)=[active_mu-2*active_sigma active_mu+2*active_sigma]';
end

a=[399
396
395
393
387
381
380
375
372
366
352
342
337
327
315
297
274
253
228
212
183
156
132
111
91
80
68
51
36
26
14
];
plot(0:30,a(31:-1:1),'linewidth',2,'color','b');

hold on
a=[ 14
    26
    35
    49
    66
    77
    90
   106
   126
   150
   177
   202
   215
   228
   242
   263
   279
   288
   298
   291
   293
   295
   290
   274
   271
   263
   261
   244
   238
   223
   217
];
plot(0:30,a,'linewidth',2,'color','r')

hold on

plot(T_CI_vector,CI_IH_standing_bounds_matrix(1,:),'--','color','r');
hold on 
plot(T_CI_vector,CI_IH_standing_bounds_matrix(2,:),'+','color','r');
hold on;
plot(T_CI_vector,CI_IH_total_bounds_matrix(1,:),'--','color','b');
hold on 
plot(T_CI_vector,CI_IH_total_bounds_matrix(2,:),'+','color','b');
hold on;
plot(T_CI_vector,CI_active_bounds_matrix(1,:),'--','color','k');
hold on 
plot(T_CI_vector,CI_active_bounds_matrix(2,:),'+','color','k');

% legend('recorded accumulated confirmed','recorded standing hospitalized', 'fitted standing hospitalized CI lower bound',...
%     'fitted standing hospitalized CI upper bound','fitted accumulated confirmed CI lower bound',...
%     'fitted accumulated confirmed CI upper bound',  ...
%     'fitted active CI lower bound', 'fitted active CI upper bound', ...
%     'Location','Northwest');


[legend_1,legend_2,legend_3]=xlsread('legends_2.xlsx');
legend_temp=legend_3(:,1);
title('Beijing','FontSize',25)
% title(legend_temp{18},'FontSize',20)
% legend(legend_temp{1},legend_temp{2},legend_temp{3},legend_temp{4},legend_temp{5},legend_temp{6},legend_temp{7},legend_temp{8}, 'Location','Northwest','FontSize',15,'LineWidth',1.5)
legend('recorded accumulated confirmed','recorded standing hospitalized', 'fitted standing hospitalized CI lower bound',...
    'fitted standing hospitalized CI upper bound','fitted accumulated confirmed CI lower bound',...
    'fitted accumulated confirmed CI upper bound','fitted active virus carrier CI lower bound',  ...
    'fitted active virus carrier CI upper bound','Location','Northwest','FontSize',13,'LineWidth',1.5);
    set(gca, 'XTick', 0:2:T_total)
    set(gca, 'XLim', [0 50])
    set(gca, 'YLim', [0 800])
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


figure
[N_end,EDGES_end]=histcounts(CI_end_time_vector,'BinWidth',0.5);
density_em_end_vector=N_end/sample_total/0.5;
l_temp=length(EDGES_end);
time_hist_vector=(EDGES_end(1:l_temp-1)+EDGES_end(2:l_temp))/2;
bar(time_hist_vector,density_em_end_vector);

hold on;
[T_end_mu, T_end_sigma]=normfit(CI_end_time_vector,p_ci);
Y=normpdf(0:0.1:T_total,T_end_mu, T_end_sigma);
plot(0:0.1:T_total,Y,'--','color','m','linewidth',2);

M=max(Y);
plot([T_end_mu+2*T_end_sigma T_end_mu+2*T_end_sigma], [0 M],'--','color','r','linewidth',2)
hold on
plot([T_end_mu T_end_mu], [0 M],'--','color','g','linewidth',2)
hold on
plot([T_end_mu-2*T_end_sigma T_end_mu-2*T_end_sigma], [0 M],'--','color','k','linewidth',2)
set(gca, 'XTick', 0:2:T_total)
set(gca, 'XLim', [0 100])
set(gca,'XTickLabel',date_temp)
f = get(gca,'XTickLabel');
set(gca,'XTickLabel',f,'fontsize',7);
title('Beijing','FontSize',25)
legend('recaled histogram of ending time', 'fitted ending time distribution',...
    ['end time CI upper bound=' num2str(T_end_mu+2*T_end_sigma) ', p=' num2str(p_ci)], ...
    ['expected end time=' num2str(T_end_mu)], ...
    ['end time CI lower bound=' num2str(T_end_mu-2*T_end_sigma) ', p=' num2str(p_ci)],'Location','Northeast','FontSize',15,'LineWidth',1.5);



%set(gca, 'XTick', 0:2:T_total)
%set(gca, 'XLim', [0 80])
%set(gca,'XTickLabel',date_temp)

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

valid_data_length=find(isnan(CI_Rc_bounds_matrix(1,:))==1);
valid_data_length=valid_data_length(1);

plot(T_Rc_vector(1:valid_data_length),mean(CI_Rc_bounds_matrix(:,1:valid_data_length)),'c+:','MarkerSize',6, 'linewidth',2);
title('Beijing','FontSize',25)
% title(legend_temp{18},'FontSize',20)
% legend(legend_temp{14},'FontSize',11,'LineWidth',1.5)
legend('Expectation of the estiamted Controlled Reproduction Number R_c over time ','FontSize',15,'LineWidth',1.5)
set(gca, 'YTick', 0:0.2:3.5)
set(gca, 'YLim', [0 3.5])
set(gca, 'XTick', 0:2:40)
set(gca,'XTickLabel',date_temp)


figure
plot(T_Rc_vector(1:Rc_num),nanmean(CI_proportion_of_AE(:,1:Rc_num)),'r+:','MarkerSize',6, 'linewidth',2);
hold on
plot(T_Rc_vector(1:Rc_num),nanmean(CI_proportion_of_A(:,1:Rc_num)),'c+:','MarkerSize',6, 'linewidth',2);
title('Beijing','FontSize',25)
% title(legend_temp{18},'FontSize',20)
% legend(legend_temp{14},'FontSize',11,'LineWidth',1.5)
legend('Expectation of the proportion of patients that are infected by A and E',...
'Expectation of the proportion of patients that are infected by A','FontSize',15,'LineWidth',1.5,'Location','Northwest')
set(gca, 'YTick', 0:0.1:1)
set(gca, 'YLim', [0 1])
set(gca, 'XTick', 0:2:40)
set(gca, 'XLim', [0 40])
set(gca,'XTickLabel',date_temp)