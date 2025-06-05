%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qconsumptive = readmatrix('./consumptive.csv');

%% read data-generated


qCO=readmatrix("./modifiedgenerator/synthetic/qCO-100x10-monthly.csv");
qCO = qCO';
%% 
qCO = qCO-Qconsumptive;
qCO(qCO < 0.01) = 0.01;

writematrix(qCO, "./modifiedgenerator/synthetic/qCO-100x10-monthly-Actual.csv");