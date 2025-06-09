%verifying generation results

close all;
clc;
clear all;

%% read data-original historical

Qconsumptive = readmatrix('./consumptive.csv');

%% read data-generated

qCO=readmatrix("./modifiedgenerator/synthetic/qCO-100x20-monthly.csv");
qCO = qCO';

%% save data-subtract consumptive

qCO_Actual = qCO-Qconsumptive;
%% replace negative value with 0.01

qCO_Actual(qCO_Actual < 0.01) = 0.01;
%% transpose entire data

qCO_Actual = qCO_Actual';

%% save data

writematrix(qCO_Actual, "./modifiedgenerator/synthetic/qCO-100x20-monthly-Actual.csv");