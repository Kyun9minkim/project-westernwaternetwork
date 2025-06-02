%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qhistorical = readmatrix('./modifiedgenerator/Qmonthly.csv');

histCA=Qhistorical(:,1);
histCO=Qhistorical(:,2);
histRG=Qhistorical(:,3);

%% read data-generated

qCA=readmatrix("./modifiedgenerator/synthetic/qCA-100x10-monthly.csv");
qCA = qCA';
qCO=readmatrix("./modifiedgenerator/synthetic/qCO-100x10-monthly.csv");
qCO = qCO';
qRG=readmatrix("./modifiedgenerator/synthetic/qRG-100x10-monthly.csv");
qRG = qRG';

%% bootstrap

B = 100; % 부트스트랩 샘플 개수
years = 10; % 20년치 데이터

boot_histCA = yearly_bootstrap(histCA, B, years);
boot_histCO = yearly_bootstrap(histCO, B, years);
boot_histRG = yearly_bootstrap(histRG, B, years);

%% transpose bootstraped data

boot_histCA = boot_histCA';
boot_histCO = boot_histCO';
boot_histRG = boot_histRG';

%% box plot preparation

histCA_monthly = reshape(boot_histCA, 12, 10, 100);
histCO_monthly = reshape(boot_histCO, 12, 10, 100);
histRG_monthly = reshape(boot_histRG, 12, 10, 100);

qCA_monthly = reshape(qCA, 12, 10, 100);
qCO_monthly = reshape(qCO, 12, 10, 100);
qRG_monthly = reshape(qRG, 12, 10, 100);

%% 초기 설정
regions = {'CA', 'CO', 'RG'};
monthly_labels = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
labels = {'Hist', 'Gen'};

mean_diff = struct();
std_diff = struct();
mean_avg = struct();
std_avg = struct();

%% Set Figure size

set(0, 'DefaultFigurePosition', [100, 100, 1200, 400]); % 모든 figure 크기 설정

%% Mean Difference Percentage 및 Standard Deviation 계산 (수정됨)%% Mean Difference Percentage 및 Standard Deviation 계산 (최종 수정)
for i = 1:length(regions)
    region = regions{i};
    
    gen_data = eval(['q' region '_monthly']);
    hist_data = eval(['hist' region '_monthly']);
    
    mean_diff.(region) = zeros(12, 1);
    std_diff.(region) = zeros(12, 1);
    
    for month = 1:12
        gen_monthly = squeeze(gen_data(month, :, :)); % [10 × 100]
        hist_monthly = squeeze(hist_data(month, :, :)); % [10 × 100]
        
        % 10년 × 100개 전체 데이터의 평균 계산
        mean_gen = mean(gen_monthly(:)); % 스칼라 값
        mean_hist = mean(hist_monthly(:)); % 스칼라 값
        mean_diff.(region)(month) = ((mean_gen - mean_hist) / mean_hist) * 100;

        % 10년 × 100개 전체 데이터의 표준편차 계산
        std_gen = std(gen_monthly(:)); % 스칼라 값
        std_hist = std(hist_monthly(:)); % 스칼라 값
        std_diff.(region)(month) = ((std_gen - std_hist) / std_hist) * 100;
    end
end


%% Figure 1: Mean Difference Percentage 시각화

% 1-1. 막대그래프 (Mean Difference Percentage)
figure;
for i = 1:length(regions)
    region = regions{i};
    subplot(3,1,i);
    bar(mean_diff.(region)); % 각 월별 평균 차이
    set(gca, 'XTickLabel', monthly_labels);
    title(region);
    ylabel('Percentage (%)');
    ylim([-100 100])
    grid on;
end
sgtitle('Monthly Mean Difference Percentage');

%% Figure 2: Standard Deviation Difference 시각화

% 2-1. 막대그래프 (Standard Deviation Difference)
figure;figure(1);
for i = 1:length(regions)
    region = regions{i};
    subplot(3,1,i);
    bar(std_diff.(region)); % 각 월별 표준편차 차이
    set(gca, 'XTickLabel', monthly_labels);
    title(region);
    ylabel('Percentage (%)');
    ylim([-100 100])
    grid on;
end
sgtitle('Monthly Std Difference Percentage');
 
%% CA Aqueduct


figure(1);
for month = 1:12
    subplot(1, 12, month);

    histCA_monthly_index = squeeze(histCA_monthly(month, :, :));
    qCA_monthly_index = squeeze(qCA_monthly(month, :, :));
    histCA_monthly_index = reshape(histCA_monthly_index, 1000, 1);
    qCA_monthly_index = reshape(qCA_monthly_index, 1000, 1);
    boxplot([histCA_monthly_index, qCA_monthly_index]);
    set(gca, 'XTickLabel', labels);
end
sgtitle('CA Aqueduct')
%% 

figure(2)
p_median_CA = zeros(12,1);

for month = 1:12
    subplot(1, 12, month);

    histCA_monthly_index = squeeze(histCA_monthly(month, :, :));
    qCA_monthly_index = squeeze(qCA_monthly(month, :, :));

     %calculate mean for boxplot
    histCA_monthly_mean = mean(histCA_monthly_index);
    qCA_monthly_mean = mean(qCA_monthly_index);

    %boxplot
    boxplot([histCA_monthly_mean(:), qCA_monthly_mean(:)]);
    set(gca, 'XTickLabel', labels);
    %vectorization
    histCA_data = histCA_monthly_index(:);
    qCA_data = qCA_monthly_index(:);

    % Rank Sum Test (Mann-Whitney U test)
    [p_median_CA(month, 1), ~] = ranksum(histCA_data, qCA_data);



end
sgtitle('CA Aqueduct mean')
%% 


figure(3)
p_var_CA = zeros(12,1);

for month = 1:12
    subplot(1, 12, month);

    histCA_monthly_index = squeeze(histCA_monthly(month, :, :));
    qCA_monthly_index = squeeze(qCA_monthly(month, :, :));

    %calculate std for boxplot
    histCA_monthly_std = std(histCA_monthly_index);
    qCA_monthly_std = std(qCA_monthly_index);

    %boxplot 
    boxplot([histCA_monthly_std(:), qCA_monthly_std(:)]);
    set(gca, 'XTickLabel', labels);
    %vectorization
    p_var_CA_data = [histCA_monthly_index(:); qCA_monthly_index(:)];

    %make as group
    group_CA = [ones(numel(histCA_monthly_index),1); 2*ones(numel(qCA_monthly_index),1)];

    %Levene’s test
    p_var_CA(month) = vartestn(p_var_CA_data, group_CA, 'TestType', 'LeveneAbsolute', 'Display', 'off');

end
sgtitle('CA Aqueduct std')
%% 


figure(4)
subplot(2,1,1)
bar(p_median_CA)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('CA Aqueduct rank-sum p')
subplot(2,1,2)
bar(p_var_CA)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('CA Aqueduct levene p')

%% Colorado

figure(5);

for month = 1:12
    
    subplot(1, 12, month);

    histCO_monthly_index = squeeze(histCO_monthly(month, :, :));
    qCO_monthly_index = squeeze(qCO_monthly(month, :, :));

    histCO_monthly_combined = reshape(histCO_monthly_index, 1000, 1);
    qCO_monthly_combined = reshape(qCO_monthly_index, 1000, 1);

    boxplot([histCO_monthly_combined, qCO_monthly_combined]); 
    set(gca, 'XTickLabel', labels);

end
sgtitle('Colorado')
%% 



figure(6)
p_median_CO = zeros(12,1);
for month = 1:12
    subplot(1, 12, month);

    histCO_monthly_index = squeeze(histCO_monthly(month, :, :));
    qCO_monthly_index = squeeze(qCO_monthly(month, :, :));

    %calculate mean for boxplot
    histCO_monthly_mean = mean(histCO_monthly_index);
    qCO_monthly_mean = mean(qCO_monthly_index);

    %boxplot
    boxplot([histCO_monthly_mean(:), qCO_monthly_mean(:)]);
    set(gca, 'XTickLabel', labels);

    %vectorization
    histCO_data = histCO_monthly_index(:);
    qCO_data = qCO_monthly_index(:);

    % Rank Sum Test (Mann-Whitney U test)
    [p_median_CO(month, 1), ~] = ranksum(histCO_data, qCO_data);
  
end
sgtitle('Colorado mean')

%% 

figure(7)
p_var_CO = zeros(12,1);

for month = 1:12
    subplot(1, 12, month);

    histCO_monthly_index = squeeze(histCO_monthly(month, :, :));
    qCO_monthly_index = squeeze(qCO_monthly(month, :, :));

    %calculate std for boxplot
    histCO_monthly_std = std(histCO_monthly_index);
    qCO_monthly_std = std(qCO_monthly_index);

    %boxplot 
    boxplot([histCO_monthly_std(:), qCO_monthly_std(:)]);
    set(gca, 'XTickLabel', labels);

    %vectorization
    p_var_CO_data = [histCO_monthly_index(:); qCO_monthly_index(:)];

    %make as group
    group_CO = [ones(numel(histCO_monthly_index),1); 2*ones(numel(qCO_monthly_index),1)];

    %Levene’s test
    p_var_CO(month) = vartestn(p_var_CO_data, group_CO, 'TestType', 'LeveneAbsolute', 'Display', 'off');

end
sgtitle('Colorado std')

%% 

figure(8)
subplot(2,1,1)
bar(p_median_CO)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('Colorado rank-sum p')
subplot(2,1,2)
bar(p_var_CO)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('Colorado levene p')

%% Rio Grande

figure(9);
for month = 1:12
    subplot(1, 12, month);

    histRG_monthly_index = squeeze(histRG_monthly(month, :, :));
    qRG_monthly_index = squeeze(qRG_monthly(month, :, :));
    histRG_monthly_index = reshape(histRG_monthly_index, 1000, 1);
    qRG_monthly_index = reshape(qRG_monthly_index, 1000, 1);
    boxplot([histRG_monthly_index, qRG_monthly_index]);
    set(gca, 'XTickLabel', labels);
end
sgtitle('Rio Grande')

%% 

figure(10)
p_median_RG = zeros(12,1);
for month = 1:12
    subplot(1, 12, month);

    histRG_monthly_index = squeeze(histRG_monthly(month, :, :));
    qRG_monthly_index = squeeze(qRG_monthly(month, :, :));

    %calculate mean for boxplot
    histRG_monthly_mean = mean(histRG_monthly_index);
    qRG_monthly_mean = mean(qRG_monthly_index);

    %boxplot
    boxplot([histRG_monthly_mean(:), qRG_monthly_mean(:)]);
    set(gca, 'XTickLabel', labels);
    % vectorization
    histRG_data = histRG_monthly_index(:);
    qRG_data = qRG_monthly_index(:);

    % Rank Sum Test (Mann-Whitney U test)
    [p_median_RG(month, 1), ~] = ranksum(histRG_data, qRG_data);
  

end
sgtitle('Rio Grande mean')

%% 

figure(11)
p_var_RG = zeros(12,1);
for month = 1:12
    subplot(1, 12, month);

    histRG_monthly_index = squeeze(histRG_monthly(month, :, :));
    qRG_monthly_index = squeeze(qRG_monthly(month, :, :));

    %calculate mean and std
    histRG_monthly_std = std(histRG_monthly_index);
    qRG_monthly_std = std(qRG_monthly_index);

    %p_var_RG(month,1) = vartest2(histRG_monthly_std(:),qRG_monthly_std(:));
 
    %boxplot mean and std
    boxplot([histRG_monthly_std(:), qRG_monthly_std(:)]);
    set(gca, 'XTickLabel', labels);
    
    p_var_RG_data = [histRG_monthly_index(:); qRG_monthly_index(:)];
    group_RG = [ones(numel(histRG_monthly_index),1); 2*ones(numel(qRG_monthly_index),1)];

    p_var_RG(month) = vartestn(p_var_RG_data, group_RG, 'TestType', 'LeveneAbsolute', 'Display', 'off');



end
sgtitle('Rio Grande std')

%% 

figure(12)
subplot(2,1,1)
bar(p_median_RG)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('Rio Grande rank-sum p')
subplot(2,1,2)
bar(p_var_RG)
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
ylim([0 1])
hold off;
title('Rio Grande levene p')

