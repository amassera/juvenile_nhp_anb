function run_models()

sim_name='run';

model_v=5;

fit_behavior(sim_name, 'classic', 2, model_v);

fit_behavior(sim_name, 'classic', 3, model_v);

fit_behavior(sim_name, 'random', 3, model_v);

