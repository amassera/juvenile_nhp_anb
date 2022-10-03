function fit_behavior(sim_name, task, timepoint, model_v)

if timepoint==2
    out_dir='../../../output/year2';
    data_fname='coded_data/year2.csv';
elseif timepoint==3
    out_dir='../../../output/year3';
    if strcmp(task,'classic')
        data_fname='coded_data/year3.csv';
    elseif strcmp(task,'random')
        out_dir='../../../output/year3/random';
        data_fname='coded_data/year3_random.csv';
    end
end

ntrial_thresh=12;

% Read data
data=readtable(fullfile('../../../',data_fname),'Delimiter',',');

% Decode ITI
if iscell(data.ITI)
    iti_str=data.ITI;
    iti=[];
    for i=1:length(iti_str)
        if length(iti_str{i})>0
            iti(i)=str2num(iti_str{i});
        else
            iti(i)=NaN;
        end
    end
    data.ITI=iti';
end

iti_thresh=nanmedian(data.ITI)+2.5*nanstd(data.ITI);


if iscell(data.Vocalization_ITI)
    voc_str=data.Vocalization_ITI;
    voc=[];
    for i=1:length(voc_str)
        if length(voc_str{i})>0
            disp(voc_str{i})
            voc(i)=str2num(voc_str{i});
        else
            voc(i)=NaN;
        end
    end
    data.Vocalization_ITI=voc';
end

if iscell(data.Affiliative_ITI)
    aff_str=data.Affiliative_ITI;
    aff=[];
    for i=1:length(aff_str)
        if length(aff_str{i})>0
            disp(aff_str{i})
            aff(i)=str2num(aff_str{i});
        else
            aff(i)=NaN;
        end
    end
    data.Affiliative_ITI=aff';
end

data.Locomotion(isnan(data.Locomotion))=0;
data.Pace(isnan(data.Pace))=0;
data.Anxiety(isnan(data.Anxiety))=0;
data.Vocalization(isnan(data.Vocalization))=0;
data.Threat(isnan(data.Threat))=0;
data.Fear(isnan(data.Fear))=0;
data.Affiliative(isnan(data.Affiliative))=0;
data.Locomotion_ITI(isnan(data.Locomotion_ITI))=0;
data.Pace_ITI(isnan(data.Pace_ITI))=0;
data.Anxiety_ITI(isnan(data.Anxiety_ITI))=0;
data.Vocalization_ITI(isnan(data.Vocalization_ITI))=0;
data.Threat_ITI(isnan(data.Threat_ITI))=0;
data.Fear_ITI(isnan(data.Fear_ITI))=0;
data.Affiliative_ITI(isnan(data.Affiliative_ITI))=0;
data.Delay(data.Delay==0)=1;

% Group that each subject is in
subj_ids={};
groups={};

% List of all subjects
all_subj_ids=unique(data.ID(:));
sessions=unique(data.Session);

param_limits=[];
param_limits.w_1=[0 1];
param_limits.w_2=[-1 1];
param_limits.w_3=[0 1];
param_limits.lambda_1=[0 10];
param_limits.lambda_2=[0 10];
param_limits.alpha=[0 1];
param_limits.beta=[0 10];

% Parameters to fit
% Lower AIC with all parameters
if model_v==1
    param_names={'w_1','lambda_1','beta'};
elseif model_v==2 || model_v==3
    param_names={'w_2','lambda_2','beta'};
elseif model_v==4 || model_v==5
    param_names={'w_1','lambda_1','w_2','lambda_2','beta'};
elseif model_v==6
    param_names={'w_3','alpha','beta'};
elseif model_v==7
    param_names={'w_1','lambda_1','w_2','alpha','beta'};
elseif model_v==8 || model_v==9
    param_names={'w_2','lambda_2','w_3','alpha','beta'};
elseif model_v==10 || model_v==11
    param_names={'w_1','lambda_1','w_2','lambda_2','w_3','alpha','beta'};
end


% Parameter estimates for each subject
subj_param_ests=[];
% Proportion of correctly predicted responses for each subject
subj_prop_correct=[];

% Log likelihood for each subject
subj_ll=[];

for subj_idx=1:length(all_subj_ids)
    subj_id=all_subj_ids{subj_idx}

    % Add group to list of subject groups
    group=unique(data.Group(strcmp(data.ID,all_subj_ids(subj_idx))));
    
    subj_data=read_subj_data(data, subj_id, sessions, iti_thresh);
      
    n_trials=length(subj_data.food_side);
    
    if n_trials>ntrial_thresh
        subj_ids{end+1}=subj_id;
        groups{end+1}=group{1};   

        [ll,param_ests]=fit(subj_data, param_names, param_limits, model_v);            
        % Get log likelihood for this fit
        disp(sprintf('Log likelihood=%.4f', ll));
        
        [weighted_inputs, resp_probs]=predict(param_ests, subj_data, model_v);

        figure();
        subplot(4,1,1);
        hold all;
        plot([1:length(subj_data.response)],subj_data.food_side);
        plot([1:length(subj_data.response)],subj_data.response);
        ylim([-1.1 1.1]);
        title(sprintf('%s - %s', subj_id,group{1}));
        legend('food','response');
        subplot(4,1,2);
        hold all;
        plot([1:length(subj_data.response)],subj_data.prev_response);
        plot([1:length(subj_data.response)],subj_data.response);
        ylim([-1.1 1.1]);
        legend('prev response','response');
        subplot(4,1,3);
        ax=plotyy([1:length(subj_data.response)],weighted_inputs,[1:length(subj_data.response)],subj_data.response);
        ylabel(ax(1),'weighted inputs');
        ylabel(ax(2),'response');
        subplot(4,1,4);
        ax=plotyy([1:length(subj_data.response)],resp_probs,[1:length(subj_data.response)],subj_data.response);
        ylabel(ax(1),'resp probs');
        ylabel(ax(2),'response');
        
        % For each try, proportion of correctly predicted response
        prop_correct_vec=zeros(1,1000);
        for i=1:1000
            % Choice for each trial
            fit_choices=zeros(size(subj_data.response));
            % Sample according to response probability
            for t=1:length(subj_data.response)    
                fit_choices(t)=randsample([-1 1],1,true,[1-resp_probs(t) resp_probs(t)]);       
            end
            % Compute proportion of correctly predicted responses for this try
            prop_correct_vec(i)=mean(fit_choices==subj_data.response); 
        end
        
        subj_param_ests(end+1,:,:)=param_ests;
        subj_prop_correct(end+1,:)=mean(prop_correct_vec);
        subj_ll(end+1,:)=ll;
    end    
end

sim_results=[];
sim_results.model_v=model_v;
sim_results.param_names=param_names;
sim_results.subj_ids=subj_ids;
sim_results.sessions=sessions;
sim_results.groups=groups;
sim_results.subj_param_ests=subj_param_ests;
sim_results.subj_prop_correct=subj_prop_correct;
sim_results.subj_ll=subj_ll;


subj_param_ests=squeeze(subj_param_ests);
sim_results.subj_param_ests=squeeze(subj_param_ests);
sim_results.subj_prop_correct=subj_prop_correct;
sim_results.subj_ll=squeeze(subj_ll);

% Save results
save(fullfile(out_dir,sprintf('sim_results-%s_%d.mat', sim_name, model_v)),'sim_results');

fid=fopen(fullfile(out_dir,sprintf('fitted_coefficients-%s_%d.csv',sim_name, model_v)),'w');
fprintf(fid, 'subject,group');
for i=1:length(param_names)
    fprintf(fid, ',%s', param_names{i});
end
fprintf(fid,',prop_predicted\n');
for i=1:size(subj_param_ests,1)
    fprintf(fid,'%s,%s',subj_ids{i},groups{i});
    for j=1:size(subj_param_ests,2)
        fprintf(fid,',%.4f',subj_param_ests(i,j));
    end
    fprintf(fid,',%.3f\n',subj_prop_correct(i));
end
fclose(fid);


end
