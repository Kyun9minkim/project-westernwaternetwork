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

%% 위치별 데이터 설정

locations = {'CA', 'CO', 'RG'};

% 히스토리컬 데이터 (각각 120×100 행렬)
hist_data = {boot_histCA, boot_histCO, boot_histRG};

% 생성 데이터 (각각 예를 들어 20×100 행렬)
gen_data = {qCA, qCO, qRG};

%% 서브플롯으로 각 위치의 Flow Duration Curve 플로팅
figure;

for i = 1:3
    % 히스토리컬 데이터 내림차순 정렬 (각 시뮬레이션 열마다)
    sorted_hist = sort(hist_data{i}, 1, 'descend');
    % 각 순위(행)별 최소, 최대값 계산
    min_hist = min(sorted_hist, [], 2);
    max_hist = max(sorted_hist, [], 2);
    % Exceedance probability 계산 (예: 1/240 ~ 240/240)
    n_hist = size(sorted_hist, 1);  
    exceedance_hist = (1:n_hist)' / n_hist;
    
    % 생성 데이터 내림차순 정렬
    sorted_gen = sort(gen_data{i}, 1, 'descend');
    min_gen = min(sorted_gen, [], 2);
    max_gen = max(sorted_gen, [], 2);
    % 생성 데이터의 Exceedance probability (예: 1/20 ~ 20/20)
    n_gen = size(sorted_gen, 1);  
    exceedance_gen = (1:n_gen)' / n_gen;
    
    % 서브플롯 생성
    subplot(3, 1, i);
    hold on;
    
    % 히스토리컬 데이터 셰도잉 영역 (파란색)
    x_patch_hist = [exceedance_hist; flipud(exceedance_hist)];
    y_patch_hist = [min_hist; flipud(max_hist)];
    fill(x_patch_hist, y_patch_hist, 'b', 'FaceAlpha', 0.3, 'EdgeColor', 'none');
    
    % 생성 데이터 셰도잉 영역 (빨간색)
    x_patch_gen = [exceedance_gen; flipud(exceedance_gen)];
    y_patch_gen = [min_gen; flipud(max_gen)];
    fill(x_patch_gen, y_patch_gen, 'r', 'FaceAlpha', 0.3, 'EdgeColor', 'none');
    
    xlabel('Exceedance Probability');
    ylabel('Flow');
    title(['Flow Duration Curve ', locations{i}]);
    legend('Historical Data', 'Generated Data', 'Location', 'best');
    
    hold off;
end
