function [weighted_inputs, resp_probs]=predict(subj_params, subj_data, model_v)

weighted_inputs=zeros(1,length(subj_data.food_side));

if model_v==1
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    beta = subj_params(3);
    weighted_inputs=w1*subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
elseif model_v==2
    w2 = subj_params(1);
    lambda2 = subj_params(2);
    beta = subj_params(3);
    weighted_inputs=w2*subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
elseif model_v==3
    w2 = subj_params(1);
    lambda2 = subj_params(2);
    beta = subj_params(3);
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    prev_prev_resp_input=subj_data.prev_prev_response.*exp(-lambda2.*(subj_data.prev_iti-1+subj_data.prev_delay-1+subj_data.iti-1+subj_data.delay-1));
    weighted_inputs=w2*(prev_resp_input+prev_prev_resp_input);
elseif model_v==4
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    w2 = subj_params(3);
    lambda2 = subj_params(4);
    beta = subj_params(5);
    wm_input=subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    weighted_inputs=w1*wm_input+w2*prev_resp_input;
elseif model_v==5
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    w2 = subj_params(3);
    lambda2 = subj_params(4);
    beta = subj_params(5);
    wm_input=subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    prev_prev_resp_input=subj_data.prev_prev_response.*exp(-lambda2.*(subj_data.prev_iti-1+subj_data.prev_delay-1+subj_data.iti-1+subj_data.delay-1));
    weighted_inputs=w1*wm_input+w2*(prev_resp_input+prev_prev_resp_input);
elseif model_v==6
    w3 = subj_params(1);
    alpha=subj_params(2);
    beta=subj_params(3);
    model_probs = rescorla_td_prediction(subj_data,alpha);
    weighted_inputs=w3*(model_probs(2,:)-model_probs(1,:));    
elseif model_v==7
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    w3 = subj_params(3);
    alpha=subj_params(4);
    beta = subj_params(5);
    wm_input=subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
    model_probs = rescorla_td_prediction(subj_data,alpha);
    val_input=(model_probs(2,:)-model_probs(1,:));
    weighted_inputs=w1*wm_input+w3*val_input;
elseif model_v==8
    w2 = subj_params(1);
    lambda2 = subj_params(2);
    w3 = subj_params(3);
    alpha=subj_params(4);
    beta = subj_params(5);
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    model_probs = rescorla_td_prediction(subj_data,alpha);
    val_input=(model_probs(2,:)-model_probs(1,:));
    weighted_inputs=w2*prev_resp_input+w3*val_input;
elseif model_v==9
    w2 = subj_params(1);
    lambda2 = subj_params(2);
    w3 = subj_params(3);
    alpha=subj_params(4);
    beta = subj_params(5);
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    prev_prev_resp_input=subj_data.prev_prev_response.*exp(-lambda2.*(subj_data.prev_iti-1+subj_data.prev_delay-1+subj_data.iti-1+subj_data.delay-1));
    model_probs = rescorla_td_prediction(subj_data,alpha);
    val_input=(model_probs(2,:)-model_probs(1,:));    
    weighted_inputs=w2*prev_resp_input+w3*(prev_prev_resp_input+val_input);
elseif model_v==10
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    w2 = subj_params(3);
    lambda2 = subj_params(4);
    w3 = subj_params(5);
    alpha=subj_params(6);
    beta = subj_params(7);
    wm_input=subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    model_probs = rescorla_td_prediction(subj_data,alpha);
    val_input=(model_probs(2,:)-model_probs(1,:));    
    weighted_inputs=w1*wm_input+w2*prev_resp_input+w3*val_input;
elseif model_v==11
    w1 = subj_params(1);
    lambda1 = subj_params(2);
    w2 = subj_params(3);
    lambda2 = subj_params(4);
    w3 = subj_params(5);
    alpha=subj_params(6);
    beta = subj_params(7);
    wm_input=subj_data.food_side.*exp(-lambda1.*(subj_data.delay-1));
    prev_resp_input=subj_data.prev_response.*exp(-lambda2.*(subj_data.iti-1+subj_data.delay-1));
    prev_prev_resp_input=subj_data.prev_prev_response.*exp(-lambda2.*(subj_data.prev_iti-1+subj_data.prev_delay-1+subj_data.iti-1+subj_data.delay-1));
    model_probs = rescorla_td_prediction(subj_data,alpha);
    val_input=(model_probs(2,:)-model_probs(1,:));    
    weighted_inputs=w1*wm_input+w2*(prev_resp_input+prev_prev_resp_input)+w3*val_input;
end

% Compute response probability for each trial
resp_probs=1./(1+exp(-beta*weighted_inputs));