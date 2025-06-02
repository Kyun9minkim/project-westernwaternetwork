%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qhistorical = readmatrix('./modifiedgenerator/Qmonthly_CA+LA.xlsx');
Qconsumptive = readmatrix('./consumptive.xlsx');

Qhistorical(:,1);
histCO=Qhistorical(:,1)-Qconsumptive;
histRG=Qhistorical(:,2);
histCALA=Qhistorical(:,3);


%% read data-generated

qCO=readmatrix("synthetic/qColorado-100x20-monthly.csv");
qCO = qCO';
qCO = qCO-Qconsumptive;
qCO(qCO < 0.01) = 0.01;
qRG=readmatrix("synthetic/qRiograde-100x20-monthly.csv");
qRG = qRG';
qCALA=readmatrix("synthetic/qCALA-100x20-monthly.csv");
qCALA = qCALA';

writematrix(qCO, "synthetic/qColorado-100x20-monthly_Actual.csv");