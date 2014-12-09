	clear all;
	clc
# set the working directory
	printf 'Loading working directory...'
	cd 'C:\Users\Public\Documents\RDataAnalysis\03_Rendimento\glpk'
	printf 'done.\n'
# read the data from AACA016 Repport	
	printf 'Reading CSV file...'
	inpt = csvread('data\aaca016.csv');
	printf 'done.\n'
	
	alloylist = inpt(:,1);
	A = inpt(:,2:end);
	clear inpt;
	

