%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qhistorical = readmatrix('./modifiedgenerator/Qmonthly_CA+LA.xlsx');
Qconsumptive = readmatrix('./consumptive.xlsx');

histCO=Qhistorical(:,1);
histRG=Qhistorical(:,2);
histCALA=Qhistorical(:,3);

%% read data-generated

qCO=readmatrix("synthetic/qColorado-1000x20-monthly.csv");
qCO = qCO';
qRG=readmatrix("synthetic/qRiograde-1000x20-monthly.csv");
qRG = qRG';
qCALA=readmatrix("synthetic/qCALA-1000x20-monthly.csv");
qCALA = qCALA';

%% bootstrap

B = 1000; % 부트스트랩 샘플 개수
years = 20; % 20년치 데이터

boot_histCO = yearly_bootstrap(histCO, B, years);
boot_histRG = yearly_bootstrap(histRG, B, years);
boot_histCALA = yearly_bootstrap(histCALA, B, years);

%% transpose bootstraped data

boot_histCO = boot_histCO';
boot_histRG = boot_histRG';
boot_histCALA = boot_histCALA';

%% box plot preparation

histCO_monthly = reshape(boot_histCO, 12, 20, 1000);
histRG_monthly = reshape(boot_histRG, 12, 20, 1000);
histCALA_monthly = reshape(boot_histCALA, 12, 20, 1000);

qCO_monthly = reshape(qCO, 12, 20, 1000);
qRG_monthly = reshape(qRG, 12, 20, 1000);
qCALA_monthly = reshape(qCALA, 12, 20, 1000);

%% 

num_years = 20; % 20년 데이터 존재

% 1️⃣ 히스토리컬 데이터의 전체 시계열 만들기 (240개월 데이터)
histCO_series = reshape(histCO_monthly, [12*num_years, 1000]); % (240 x 1000)
histRG_series = reshape(histRG_monthly, [12*num_years, 1000]); % (240 x 1000)
histCALA_series = reshape(histCALA_monthly, [12*num_years, 1000]); % (240 x 1000)

qCO_series = reshape(qCO_monthly, [12*num_years, 1000]); % (240 x 1000)
qRG_series = reshape(qRG_monthly, [12*num_years, 1000]); % (240 x 1000)
qCALA_series = reshape(qCALA_monthly, [12*num_years, 1000]); % (240 x 1000)

% 2️⃣ Pairwise Cross-Correlation 저장 공간
pairs = {'CO-RG', 'CO-CALA', 'RG-CALA'};
hist_corr_all = zeros(1000, 3); % 히스토리컬 데이터의 cross-correlation 저장
sim_corr_all = zeros(1000, 3);  % 시뮬레이션 데이터의 cross-correlation 저장

% 3️⃣ 100개 샘플 각각에 대해 cross-correlation 계산
for i = 1:1000
    % 히스토리컬 데이터의 cross-correlation
    hist_corr_all(i, 1) = corr(histCO_series(:, i), histRG_series(:, i)); % CO-RG
    hist_corr_all(i, 2) = corr(histCO_series(:, i), histCALA_series(:, i)); % CO-CA
    hist_corr_all(i, 3) = corr(histRG_series(:, i), histCALA_series(:, i)); % CO-LA

    % 시뮬레이션 데이터의 cross-correlation
    sim_corr_all(i, 1) = corr(qCO_series(:, i), qRG_series(:, i)); % CO-RG
    sim_corr_all(i, 2) = corr(qCO_series(:, i), qCALA_series(:, i)); % CO-CA
    sim_corr_all(i, 3) = corr(qRG_series(:, i), qCALA_series(:, i)); % CO-LA
end

% 4️⃣ Wilcoxon Rank Sum Test & Levene’s Test
p_wilcoxon = zeros(3,1); % Wilcoxon Rank Sum Test p-values
p_levene = zeros(3,1);   % Levene’s Test p-values

for j = 1:3
    % Wilcoxon Rank Sum Test (중간값 차이 검정)
    p_wilcoxon(j) = ranksum(hist_corr_all(:, j), sim_corr_all(:, j));

    % Levene’s Test (분산 차이 검정)
    p_data = [hist_corr_all(:, j); sim_corr_all(:, j)];
    group = [ones(1000,1); 2*ones(1000,1)]; % 1=히스토리컬, 2=시뮬레이션
    p_levene(j) = vartestn(p_data, group, 'TestType', 'LeveneAbsolute', 'Display', 'off');
end

% 5️⃣ 데이터 통합 (히스토리컬 + 시뮬레이션을 하나의 박스플롯으로)
group_labels = [repmat({'CO-RG Historical'}, 1000, 1); repmat({'CO-RG Generated'}, 1000, 1); 
                repmat({'CO-CALA Historical'}, 1000, 1); repmat({'CO-CALA Generated'}, 1000, 1); 
                repmat({'RG-LACA Historical'}, 1000, 1); repmat({'RG-LACA Generated'}, 1000, 1)];

all_data = [hist_corr_all(:,1); sim_corr_all(:,1); 
            hist_corr_all(:,2); sim_corr_all(:,2); 
            hist_corr_all(:,3); sim_corr_all(:,3)];

% 6️⃣ Boxplot 그리기 (히스토리컬 + 시뮬레이션 한 그래프에 포함)
figure(1);
boxplot(all_data, group_labels);

% 그래프 설정
xlabel('Pairwise Comparison');
ylabel('Cross-Correlation');
title('Monthly Real Space Pairwise Correlation (1000sim)');
ylim([-1 1])
yline(0, 'k-', 'LineWidth', 2);
grid on;

% 7️⃣ Wilcoxon Rank Sum Test & Levene’s Test 결과를 Bar Graph로 표시
figure(2);
bar([p_wilcoxon, p_levene]); 
hold on;
yline(0.05, 'r--', 'LineWidth', 2);
hold off;

% Bar Graph 설정
set(gca, 'XTickLabel', pairs);
legend({'Wilcoxon p-value', 'Levene p-value'}, 'Location', 'northeast');
ylabel('p-value');
title('Wilcoxon Rank Sum Test & Levene’s Test');
ylim([0 1]);
grid on;
