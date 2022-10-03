function energy = energy_decision(params, subj_data, param_names, param_limits, model_v)

% don't allow negative estimates
if(~params_within_limits(param_names, params, param_limits))
    energy=10000000; return;
end

% set parameters
beta = params(end);

[weighted_inputs, resp_probs]=predict(params, subj_data, model_v);

% get modelled probs using Rescorla-Wagner model, then vals, then energy
weighted_inputs(find(subj_data.response<0))=-weighted_inputs(find(subj_data.response<0)); % swap if LH chosen
lh=1./(1+exp(-beta*weighted_inputs));
%lh(find(lh==0))=1e-6;
energy=-sum(log(lh)); % maximise log likelihood (softmax function)
