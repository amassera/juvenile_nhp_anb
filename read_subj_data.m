function subj_data=read_subj_data(data, subj_id, sessions, iti_thresh)

% Side that the food is on in every trial
subj_data.food_side=[];
subj_data.prev_food_side=[];
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
    food_side=double(strcmp(data.FoodSide(session_rows),'R'));
    food_side(food_side==0)=-1;
    food_side(strcmp(data.FoodSide(session_rows),''))=0;

    % Get delay
    delay=data.Delay(session_rows);

    iti=data.ITI(session_rows);

    % Recode response as Left=-1 and Right=1
    response=food_side;
    incorrect_responses=data.CorrectIncorrect(session_rows)==0;
    response(incorrect_responses)=response(incorrect_responses)*-1;

    prev_food_side=NaN(size(food_side));
    prev_food_side(2:end)=food_side(1:end-1);
    
    % Get previous response (first is NaN)
    prev_response=NaN(size(response));
    prev_response(2:end)=response(1:end-1);

    % Get previous ITI (first is NaN)
    prev_iti=NaN(size(response));
    prev_iti(2:end)=iti(1:end-1);

    % Get previous delay (first is NaN)
    prev_delay=NaN(size(response));
    prev_delay(2:end)=delay(1:end-1);

    % Get previous previous response (first is NaN)
    prev_prev_response=NaN(size(response));
    prev_prev_response(2:end)=prev_response(1:end-1);
    
    trial_action=data.Locomotion(session_rows);

    % Keep trials where food was presented, not the first trial
    % (because there is no previous response), and delay is not NaN
    % (aborted trial)
    keep_trials=(food_side~=0) & (~isnan(prev_response)) & (~isnan(delay)) & (~isnan(prev_prev_response) & (iti<iti_thresh)) & (~trial_action);
    n_removed=length(food_side)-length(find(keep_trials));
    disp(sprintf('Removed %d/%d trials', n_removed, length(food_side)));
    food_side=food_side(keep_trials);
    prev_food_side=prev_food_side(keep_trials);
    delay=delay(keep_trials);
    iti=iti(keep_trials);
    prev_response=prev_response(keep_trials);
    response=response(keep_trials);
    prev_iti=prev_iti(keep_trials);
    prev_delay=prev_delay(keep_trials);
    prev_prev_response=prev_prev_response(keep_trials);

    % Add session data to all data for this subject
    subj_data.food_side(end+1:end+length(food_side))=food_side;
    subj_data.prev_food_side(end+1:end+length(food_side))=prev_food_side;
    subj_data.delay(end+1:end+length(delay))=delay;
    subj_data.iti(end+1:end+length(iti))=iti;
    subj_data.prev_response(end+1:end+length(prev_response))=prev_response;
    subj_data.response(end+1:end+length(response))=response;
    subj_data.prev_iti(end+1:end+length(prev_iti))=prev_iti;
    subj_data.prev_delay(end+1:end+length(prev_delay))=prev_delay;
    subj_data.prev_prev_response(end+1:end+length(prev_prev_response))=prev_prev_response;
end
