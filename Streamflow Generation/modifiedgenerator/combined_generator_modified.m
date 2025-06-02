function D = combined_generator_modified(hist_data, nR, nY)

    Nsites = size(hist_data, 2);

    % Kirsch et al. (2013) 방법을 사용한 월별 데이터 생성
    QQg = monthly_main_modified(hist_data, nR, nY);  % 월별 데이터 생성

    % 저장할 변수 초기화 (월 단위 데이터)
    D = zeros(nR, 12 * nY, Nsites);

    for r = 1:nR
        for i = 1:nY*12
            % 월별 값 저장
            D(r, i, :) = QQg(r, i, :);  
        end
    end

end
