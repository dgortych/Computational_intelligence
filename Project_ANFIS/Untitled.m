%% Wczytanie danych
close all;clear;clc;
data = readtable("data.csv");
data = table2array(data);
data(:,1) = [];
cv = cvpartition(size(data,1),'KFold',4);
idx1 = cv.test(1);
idx2 = cv.test(2);
idx3 = cv.test(3);
idx4 = cv.test(4);

%%
% Separate to training and test data
dataTrain1 = data(~idx1,:);
dataTest1  = data(idx1,:);
dataTrain2 = data(~idx2,:);
dataTest2  = data(idx2,:);
dataTrain3 = data(~idx3,:);
dataTest3  = data(idx3,:);
dataTrain4 = data(~idx4,:);
dataTest4 = data(idx4,:);
%%