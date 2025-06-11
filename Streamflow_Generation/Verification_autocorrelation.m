%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qhistorical = readmatrix('./modifiedgenerator/Qmonthly.csv');

histCO=Qhistorical(:,1);
histRG=Qhistorical(:,2);
histCA=Qhistorical(:,3);

%% read data-generated

qCA=readmatrix("./modifiedgenerator/synthetic/qCA-100x20-monthly.csv");
qCA = qCA';
qCO=readmatrix("./modifiedgenerator/synthetic/qCO-100x20-monthly.csv");
qCO = qCO';
qRG=readmatrix("./modifiedgenerator/synthetic/qRG-100x20-monthly.csv");
qRG = qRG';

%% bootstrap

B = 100; % 부트스트랩 샘플 개수
years = 20; % 20년치 데이터

boot_histCA = yearly_bootstrap(histCA, B, years);
boot_histCO = yearly_bootstrap(histCO, B, years);
boot_histRG = yearly_bootstrap(histRG, B, years);

%% transpose bootstraped data

boot_histCA = boot_histCA';
boot_histCO = boot_histCO';
boot_histRG = boot_histRG';

%% box plot preparation

histCA_monthly = reshape(boot_histCA, 12, 20, 100);
histCO_monthly = reshape(boot_histCO, 12, 20, 100);
histRG_monthly = reshape(boot_histRG, 12, 20, 100);

qCA_monthly = reshape(qCA, 12, 20, 100);
qCO_monthly = reshape(qCO, 12, 20, 100);
qRG_monthly = reshape(qRG, 12, 20, 100);

%% 
set(0, 'DefaultFigurePosition', [100, 100, 1600, 400]); % 모든 figure 크기 설정

figure(1);

%% California

subplot(3,1,1);
max_lag = 12; % 최대 시차 (Lag 0~12)
num_years = 20; % 20년 데이터 존재

% 1️⃣ 히스토리컬 데이터의 전체 시계열 만들기 (240개월 데이터)
hist_full_series = reshape(histCA_monthly, [12*num_years, 100]); % (240 x 100)
qCA_full_series = reshape(qCA_monthly, [12*num_years, 100]); % (240 x 100)

% ACF 저장 공간
acf_hist_all = zeros(100, max_lag+1);
acf_qCA_all = zeros(100, max_lag+1);

% 2️⃣ 100개 샘플 각각에 대해 전체 10년(240개월) 시계열로 ACF 계산
for i = 1:100
    hist_sample = hist_full_series(:, i); % i번째 히스토리컬 샘플 (240x1)
    qCA_sample = qCA_full_series(:, i); % i번째 시뮬레이션 샘플 (240x1)
    
    % 자기상관 계산 (Lag 0~12)
    [acf_hist, lags] = xcorr(hist_sample, max_lag, 'coeff');
    [acf_qCA, ~] = xcorr(qCA_sample, max_lag, 'coeff');
    
    acf_hist_all(i, :) = acf_hist(lags >= 0 & lags <= max_lag); % 히스토리컬 ACF 저장
    acf_qCA_all(i, :) = acf_qCA(lags >= 0 & lags <= max_lag); % 시뮬레이션 ACF 저장
end

% 3️⃣ ACF 중앙값 및 95% 신뢰구간 계산
acf_median_final = median(acf_hist_all, 1); % 중앙값 ACF
acf_ci_lower = prctile(acf_hist_all, 2.5, 1); % 95% 신뢰구간 하한
acf_ci_upper = prctile(acf_hist_all, 97.5, 1); % 95% 신뢰구간 상한

% 4️⃣ 그래프 그리기
hold on;

% 시뮬레이션 데이터 개별 ACF 추가 (파란색, 얇은 선)
for i = 1:100
    stairs(0:max_lag, acf_qCA_all(i, :), 'b', 'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% 히스토리컬 ACF (굵은 파란색)
stairs(0:max_lag, acf_median_final, 'k', 'LineWidth', 2, 'DisplayName', 'Historical Median ACF');

% 95% 신뢰구간 (검은 점선)
stairs(0:max_lag, acf_ci_lower, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Lower');
stairs(0:max_lag, acf_ci_upper, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Upper');

% 그래프 설정
ylabel('Autocorrelation');
title('California Autocorrelation');
xlim([0 max_lag]); % x축을 0부터 12까지 설정
ylim([-1 1]); % y축 범위 설정
grid on;
legend;
hold off;

%% Colorado


subplot(3,1,2);
max_lag = 12; % 최대 시차 (Lag 0~12)
num_years = 20; % 10년 데이터 존재

% 1️⃣ 히스토리컬 데이터의 전체 시계열 만들기 (240개월 데이터)
hist_full_series = reshape(histCO_monthly, [12*num_years, 100]); % (240 x 100)
qCO_full_series = reshape(qCO_monthly, [12*num_years, 100]); % (240 x 100)

% ACF 저장 공간
acf_hist_all = zeros(100, max_lag+1);
acf_qCO_all = zeros(100, max_lag+1);

% 2️⃣ 100개 샘플 각각에 대해 전체 10년(240개월) 시계열로 ACF 계산
for i = 1:100
    hist_sample = hist_full_series(:, i); % i번째 히스토리컬 샘플 (240x1)
    qCO_sample = qCO_full_series(:, i); % i번째 시뮬레이션 샘플 (240x1)
    
    % 자기상관 계산 (Lag 0~12)
    [acf_hist, lags] = xcorr(hist_sample, max_lag, 'coeff');
    [acf_qCO, ~] = xcorr(qCO_sample, max_lag, 'coeff');
    
    acf_hist_all(i, :) = acf_hist(lags >= 0 & lags <= max_lag); % 히스토리컬 ACF 저장
    acf_qCO_all(i, :) = acf_qCO(lags >= 0 & lags <= max_lag); % 시뮬레이션 ACF 저장
end

% 3️⃣ ACF 중앙값 및 95% 신뢰구간 계산
acf_median_final = median(acf_hist_all, 1); % 중앙값 ACF
acf_ci_lower = prctile(acf_hist_all, 2.5, 1); % 95% 신뢰구간 하한
acf_ci_upper = prctile(acf_hist_all, 97.5, 1); % 95% 신뢰구간 상한

% 4️⃣ 그래프 그리기
hold on;

% 시뮬레이션 데이터 개별 ACF 추가 (파란색, 얇은 선)
for i = 1:100
    stairs(0:max_lag, acf_qCO_all(i, :), 'b', 'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% 히스토리컬 ACF (굵은 파란색)
stairs(0:max_lag, acf_median_final, 'k', 'LineWidth', 2, 'DisplayName', 'Historical Median ACF');

% 95% 신뢰구간 (검은 점선)
stairs(0:max_lag, acf_ci_lower, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Lower');
stairs(0:max_lag, acf_ci_upper, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Upper');

% 그래프 설정
ylabel('Autocorrelation');
title('Colorado Autocorrelation');
xlim([0 max_lag]); % x축을 0부터 12까지 설정
ylim([-1 1]); % y축 범위 설정
grid on;
legend;
hold off;

%% Rio Grande

subplot(3,1,3);
max_lag = 12; % 최대 시차 (Lag 0~12)
num_years = 20; % 20년 데이터 존재

% 1️⃣ 히스토리컬 데이터의 전체 시계열 만들기 (240개월 데이터)
hist_full_series = reshape(histRG_monthly, [12*num_years, 100]); % (240 x 100)
qRG_full_series = reshape(qRG_monthly, [12*num_years, 100]); % (240 x 100)

% ACF 저장 공간
acf_hist_all = zeros(100, max_lag+1);
acf_qRG_all = zeros(100, max_lag+1);

% 2️⃣ 100개 샘플 각각에 대해 전체 10년(240개월) 시계열로 ACF 계산
for i = 1:100
    hist_sample = hist_full_series(:, i); % i번째 히스토리컬 샘플 (240x1)
    qRG_sample = qRG_full_series(:, i); % i번째 시뮬레이션 샘플 (240x1)
    
    % 자기상관 계산 (Lag 0~12)
    [acf_hist, lags] = xcorr(hist_sample, max_lag, 'coeff');
    [acf_qRG, ~] = xcorr(qRG_sample, max_lag, 'coeff');
    
    acf_hist_all(i, :) = acf_hist(lags >= 0 & lags <= max_lag); % 히스토리컬 ACF 저장
    acf_qRG_all(i, :) = acf_qRG(lags >= 0 & lags <= max_lag); % 시뮬레이션 ACF 저장
end

% 3️⃣ ACF 중앙값 및 95% 신뢰구간 계산
acf_median_final = median(acf_hist_all, 1); % 중앙값 ACF
acf_ci_lower = prctile(acf_hist_all, 2.5, 1); % 95% 신뢰구간 하한
acf_ci_upper = prctile(acf_hist_all, 97.5, 1); % 95% 신뢰구간 상한

% 4️⃣ 그래프 그리기
hold on;

% 시뮬레이션 데이터 개별 ACF 추가 (파란색, 얇은 선)
for i = 1:100
    stairs(0:max_lag, acf_qRG_all(i, :), 'b', 'LineWidth', 0.5, 'HandleVisibility', 'off');
end

% 히스토리컬 ACF (굵은 파란색)
stairs(0:max_lag, acf_median_final, 'k', 'LineWidth', 2, 'DisplayName', 'Historical Median ACF');

% 95% 신뢰구간 (검은 점선)
stairs(0:max_lag, acf_ci_lower, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Lower');
stairs(0:max_lag, acf_ci_upper, 'k--', 'LineWidth', 1.5, 'DisplayName', '95% CI Upper');

% 그래프 설정
xlabel('Lag (months)');
ylabel('Autocorrelation');
title('Rio Grande Autocorrelation');
xlim([0 max_lag]); % x축을 0부터 12까지 설정
ylim([-1 1]); % y축 범위 설정
grid on;
legend;
hold off;

