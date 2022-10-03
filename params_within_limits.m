function in_limits=params_within_limits(param_names, params, limits)

in_limits=true;
for i=1:length(param_names)
    param_name=param_names{i};
    lims=limits.(param_name);
    if params(i)<=lims(1) || params(i)>=lims(2)
        in_limits=false;
        break
    end
end
