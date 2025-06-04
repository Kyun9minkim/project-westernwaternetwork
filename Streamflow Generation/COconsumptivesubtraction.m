%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qhistorical = readmatrix('./modifiedgenerator/Qmonthly.csv');
Qconsumptive = readmatrix('./consumptive.csv');

Qhistorical(:,1);
histCA=Qhistorical(:,1)-Qconsumptive;
histCO=Qhistorical(:,2);
histRG=Qhistorical(:,3);


%% read data-generated

qCA=readmatrix("./modifiedgenerator/synthetic/qCA-100x10-monthly.csv");
qCA = qCA';
qCO=readmatrix("./modifiedgenerator/synthetic/qCO-100x10-monthly.csv");
qCO = qCO';
qCO = qCO-Qconsumptive;
qCO(qCO < 0.01) = 0.01;
qRG=readmatrix("./modifiedgenerator/synthetic/qRG-100x10-monthly.csv");
qRG = qRG';

writematrix(qCO, "./modifiedgenerator/synthetic/qCO-100x10-monthly-Actual.csv");