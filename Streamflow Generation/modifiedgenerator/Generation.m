%% prepare workspace
clear all
close all
clc
path_read = './';

% load multi-site observations of daily streamflow
Qmonthly = readmatrix('Qmonthly.csv');

% column 1 is California, 2 is Colorado, 3 is Rio Grande

%% preprocess data 

% make normally distributed evaporation log-normal like flows
% (monthly_gen.m takes the log of Qmonthly to make all columns normally
% distributed)

sites = {'qCA', 'qCO', 'qRG'};
Nyears = size(Qmonthly,1)/12;
Nsites = size(Qmonthly,2);

%% 

% Initialize cell array
Qcells = cell(1, Nsites);

% Split and reshape data
for i = 1:Nsites
    Qcells{i} = reshape(Qmonthly(:,i), 12, Nyears)'; % Transpose to 10×12
end

Qmonthly=Qcells;

%% Kirsch + Nowak generation

% 100 realizations of 10 years

num_realizations = [100];
num_years = [10];
dimensions = {'-100x10'};

%% 

% directory to write output to
mkdir('synthetic');
for k=1:length(num_realizations)
    Qd_cg = combined_generator_modified(Qmonthly, num_realizations(k), num_years(k) );

    % write simulated data to file
    for i=1:Nsites
        q_ = [];
        for j=1:num_realizations(k)
            qi = nan(12*num_years(k),1);
            qi(1:size(Qd_cg,2)) = Qd_cg(j,:,i)';
            q_ = [q_ reshape(qi,12,num_years(k))];
        end
        Qd2(:,i) = reshape(q_(:),[],1);
        saveQ = reshape(Qd2(:,i), 12*num_years(k), num_realizations(k))';
        filename = sprintf('synthetic/%s%s-monthly.csv', sites{i}, dimensions{k});
        writematrix(saveQ, filename);

    end

    filename2 = sprintf('synthetic/%s.csv', dimensions{k});
    writematrix(Qd2, filename2);

    clear Qd2;
end
