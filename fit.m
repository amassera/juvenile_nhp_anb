function [ll,param_ests]=fit(subj_data, param_names, param_limits, model_v)

init_vals={};
for p_idx=1:length(param_names)
    param_name=param_names{p_idx};
    lims=param_limits.(param_name);
    init_vals{p_idx}=linspace(lims(1), lims(2), 10);
end
C = cell(1,numel(init_vals));
[C{:}] = ndgrid(init_vals{:});
C = cellfun(@(X) reshape(X,[],1),C,'UniformOutput',false);
C = horzcat(C{:});

% Parameter estimates for each fit
all_param_estimates=zeros(length(param_names),size(C,1));
% Energy for each fit
estimate_energy=[];
parfor fit_idx=1:size(C,1)
    % Start with random initial parameters
    params = C(fit_idx,:);
    options = optimset('MaxFunEvals', 50000*length(param_names),'MaxIter',50000*length(param_names));
    % Fit 
    [pe,ee,exitflag]=fminsearch(@energy_decision,params,options,...
        subj_data, param_names, param_limits, model_v);

    % Only save if no parameters are at limits
    all_param_estimates(:,fit_idx)=pe;
    if params_within_limits(param_names, pe, param_limits)                    
        estimate_energy(fit_idx)=ee;
    else
        estimate_energy(fit_idx)=10000000;
    end
end
% Find fit that minimizes energy
minidx=find(estimate_energy==min(estimate_energy),1);
% Get log likelihood for this fit
ll=-estimate_energy(minidx);
param_ests=all_param_estimates(:,minidx);