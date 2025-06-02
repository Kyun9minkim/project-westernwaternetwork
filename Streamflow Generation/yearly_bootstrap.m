function boot_data = yearly_bootstrap(data, B, years)
    % data: 부트스트랩할 시계열 데이터 (240 × 1)
    % B: 샘플 개수 (100)
    % years: 연 단위 데이터 개수 (20년)
    % boot_data: 부트스트랩된 데이터 (B × 240)

    months_per_year = length(data) / years;  % 1년당 월 개수 (12개월)
    boot_data = zeros(B, length(data));  % 결과 저장할 행렬

    rng(123); % 랜덤 시드 설정
    for b = 1:B
        idx = randi([1, years], 1, years);  % 연 단위 샘플링 (20개 선택)
        boot_sample = [];
        for i = 1:years
            start_idx = (idx(i)-1) * months_per_year + 1;
            boot_sample = [boot_sample; data(start_idx:start_idx+months_per_year-1)];
        end
        boot_data(b, :) = boot_sample(1:length(data));  % 240개월로 맞춤
    end
end
