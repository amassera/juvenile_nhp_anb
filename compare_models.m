function compare_models()

sim_name='model_comparison';
n_models=11;

years={'year2','year3','year3'};
tasks={'classic','classic','random'};

figure();
    
for y=1:length(years)
    year=years{y};
    task=tasks{y};

    out_dir=fullfile('../../../output/iti_removed',year);
    if strcmp(task,'random');
        out_dir=fullfile(out_dir,task);
    end
    
    aic=zeros(2,n_models);
    subplot(1,3,y);
    
    for model_v=1:n_models
        load(fullfile(out_dir,sprintf('sim_results-%s_%d.mat', sim_name, model_v)));
        n_params=length(sim_results.param_names);
        
        group_idx=find(strcmp(sim_results.groups,'mother'));
        subj_aics=2*(n_params)-2*sim_results.subj_ll(group_idx);                
        aic(1,model_v)=aic(1,model_v)+sum(subj_aics);        
        
        group_idx=find(strcmp(sim_results.groups,'nursery'));
        subj_aics=2*(n_params)-2*sim_results.subj_ll(group_idx);                
        aic(2,model_v)=aic(2,model_v)+sum(subj_aics);        
    end
    
    rel_aic=aic;
    rel_aic(1,:)=aic(1,:)-max(aic(1,:));
    rel_aic(2,:)=aic(2,:)-max(aic(2,:));
    
    hold all;
    w = 0.4;
    g=0.05;
    bar((1:n_models)-w/2-g/2,rel_aic(1,:),w,'FaceColor',[31,120,180]/255.0);
    bar((1:n_models)+w/2+g/2,rel_aic(2,:),w,'FaceColor',[227,26,28]/255.0);
    [r_mother,m_mother]=min(rel_aic(1,:));
    plot([m_mother m_mother]-w/2-g/2,[r_mother r_mother],'ko');
    [r_nursery,m_nursery]=min(rel_aic(2,:));
    plot([m_nursery m_nursery]+w/2+g/2,[r_nursery r_nursery],'ko');
    xlabel('Model','Fontsize',12,'Fontname','Arial');
    ylabel('\Delta AIC','Fontsize',12,'Fontname','Arial');
    xlim([.5 n_models+.5]);
    legend('mother','nursery');
    title(sprintf('%s-%s', year, task));
end

