function run_model_comparison()

sim_name='model_comparison';
n_models=11;

for model_v=1:n_models
    fit_behavior(sim_name, 'classic', 2, model_v);
    close all;
end


for model_v=1:n_models
    fit_behavior(sim_name, 'classic', 3, model_v);
    close all;
end


for model_v=1:n_models
    fit_behavior(sim_name, 'random', 3, model_v);
    close all;
end
