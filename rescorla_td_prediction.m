function [x_pre] = rescorla_td_prediction(subj_data,learningrate)
% Same as RL_rescorlaTD_miriam.m in pilot 2 with the slight change that now
% not all outcomes are shown and thus the td algorithm only updates the prediction 
% for the one that is shown and leaves the two others the same

% Initialise (step 0 of recursive iteration)
x_pre(:,1)  = [0.5 0.5];  % Initial estimate (mean): no big effect if changed

reward=subj_data.food_side==subj_data.response;

% Update - for all rewards in all steps because all thre outcomes shown
for t=1:length(reward)-1
    
    % received reward and prediction error
    x      = reward(t);
    resp_idx=0;
    if subj_data.response(t)==-1
        resp_idx=1;
    elseif subj_data.response(t)==1
        resp_idx=2;
    end
    if resp_idx>0
        pe     = x - x_pre(resp_idx,t);               
        % leave prediction the same for unchosen and update it for chosen stim
        x_pre(resp_idx,t+1)= x_pre(resp_idx,t) + learningrate * pe;        
    end
end
disp('');