%% prepare workspace
clear all
close all
clc
path_read = './';

% load multi-site observations of daily streamflow
Qmonthly = readmatrix('Qmonthly.xlsx');
% column 1 is colorado, 2 is riogrande, 3 is California, 4 is LA, 5 is
% CA+LA%% preprocess data 

% make normally distributed evaporation log-normal like flows
% (monthly_gen.m takes the log of Qmonthly to make all columns normally
% distributed)
sites = {'qColorado', 'qRiograde', 'qCA', 'qLA'};
Nyears = size(Qmonthly,1)/12;
Nsites = size(Qmonthly,2);

%% 

% Initialize cell array
Qcells = cell(1, Nsites);

% Split and reshape data
for i = 1:Nsites
    Qcells{i} = reshape(Qmonthly(:,i), 12, Nyears)'; % Transpose to 20×12
end

Qmonthly=Qcells;

%% Kirsch + Nowak generation

% 1000 realizations of 20 years
% and then 100 realizations of 100 year
num_realizations = [10, 1000];
num_years = [20, 20];
dimensions = {'-10x20','-10x100'};

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
