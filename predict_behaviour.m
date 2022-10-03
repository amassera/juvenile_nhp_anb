function predict_behaviour(sim_name, task, timepoint, model_v)

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
data.RespProb=NaN*zeros(length(data.ID),1);

% Group that each subject is in
subj_ids={};

% List of all subjects
all_subj_ids=unique(data.ID(:));
sessions=unique(data.Session);

if model_v>0
    load(fullfile(out_dir,sprintf('sim_results-%s_%d.mat', sim_name, model_v)));
    param_ests=sim_results.subj_param_ests;
else
    param_ests={};
    for m=1:11
        load(fullfile(out_dir,sprintf('sim_results-%s_%d.mat', sim_name, m)));
        param_ests{m}=sim_results.subj_param_ests;
    end
end

for subj_idx=1:length(all_subj_ids)
    subj_id=all_subj_ids{subj_idx}

    if find(strcmp(sim_results.subj_ids,subj_id))
        
        % Side that the food is on in every trial
        subj_data.food_side=[];
        % Delay for every trial
        subj_data.delay=[];
        % Previous response for every trial
        subj_data.prev_response=[];
        % Response for every trial 
        subj_data.response=[];
        % Inter-trial-interval for every trial
        subj_data.iti=[];
        % Previous trial inter-trial-interval for every trial
        subj_data.prev_iti=[];
        % Previous trial delay for every trial
        subj_data.prev_delay=[];
        % Prev previous response for every trial
        subj_data.prev_prev_response=[];

        % For each session
        for session_idx=1:length(sessions)
            session=sessions(session_idx);

            % Find rows corresponding to this subject and this session
            session_rows=find((strcmp(data.ID,subj_id)) & (data.Session==session));

            % Recode food side as Left=-1 and Right=1
            subj_data.food_side=double(strcmp(data.FoodSide(session_rows),'R'));
            subj_data.food_side(subj_data.food_side==0)=-1;
            subj_data.food_side(strcmp(data.FoodSide(session_rows),''))=0;

            % Get delay
            subj_data.delay=data.Delay(session_rows);

            subj_data.iti=data.ITI(session_rows);

            % Recode response as Left=-1 and Right=1
            subj_data.response=subj_data.food_side;
            incorrect_responses=data.CorrectIncorrect(session_rows)==0;
            subj_data.response(incorrect_responses)=subj_data.response(incorrect_responses)*-1;

            % Get previous response (first is NaN)
            subj_data.prev_response=NaN(size(subj_data.response));
            subj_data.prev_response(2:end)=subj_data.response(1:end-1);

            % Get previous ITI (first is NaN)
            subj_data.prev_iti=NaN(size(subj_data.response));
            subj_data.prev_iti(2:end)=subj_data.iti(1:end-1);

            % Get previous delay (first is NaN)
            subj_data.prev_delay=NaN(size(subj_data.response));
            subj_data.prev_delay(2:end)=subj_data.delay(1:end-1);

            % Get previous previous response (first is NaN)
            subj_data.prev_prev_response=NaN(size(subj_data.response));
            subj_data.prev_prev_response(2:end)=subj_data.prev_response(1:end-1);

            if model_v>0
                subj_param_ests=param_ests(find(strcmp(sim_results.subj_ids,subj_id)),:);        
                [weighted_inputs, resp_probs]=predict(subj_param_ests, subj_data, model_v);
                l_responses=find(subj_data.response==-1);
                resp_probs(l_responses)=1-resp_probs(l_responses);
                data.RespProb(session_rows)=resp_probs;
            else
                all_resp_probs=[];
                for m=1:11
                    m_param_ests=param_ests{m};
                    subj_param_ests=m_param_ests(find(strcmp(sim_results.subj_ids,subj_id)),:);        
                    [weighted_inputs, resp_probs]=predict(subj_param_ests, subj_data, m);
                    l_responses=find(subj_data.response==-1);
                    resp_probs(l_responses)=1-resp_probs(l_responses);
                    all_resp_probs(m,:)=resp_probs;
                end
                data.RespProb(session_rows)=max(all_resp_probs,[],1);
            end
        end
    end
end
data.RespProb(data.ITI>=iti_thresh)=NaN;
out_fname='';
if model_v>0
    out_fname=fullfile(out_dir,sprintf('model_predictions-%s_%d.csv', sim_name, model_v));
else
    out_fname=fullfile(out_dir,sprintf('model_predictions-%s_best.csv', sim_name));
end
writetable(data,out_fname);