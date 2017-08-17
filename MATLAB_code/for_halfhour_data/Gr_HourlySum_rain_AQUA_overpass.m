%This function reads ground monitoring data, calculates daily average for
%a specific time range according to AQUA and TERRA overpass time
%and organizes as a final table for each var with first two columns (x,y-
%Israel cooredinates),hourly avg,NaN expresses days with no data at all.
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_052016';
clear
clc

I=12
    Infilename={'PM25','PM10','RH','Temp','WD','SR','WS','Rain','NO2','NO','O3','SO2'};
    file= ['LoadDataVar_' Infilename{I} '_2000#2015.mat']; 
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_052016';

    load (file);
    
    % Calculate hourly average for each variable. Resulting table: each row is a
    % station
    K=4;HourlySum=cell(size(DATA,1)+6,1);temp=0;D=zeros(size(DATA,1),1);
   Y=25; %column number for 11:30 am 1-1-2000; see in TA
    for J=Y+2:48:length(DATA);  
        temp=DATA(:,J:J+5); % dataset including 11:30 till 14:00 only
        D(:,K)=nansum(temp,2); K=K+1; % calculate the mean without NA 
        temp=[];
    end
    
    %Find indices of date and make a matrix of 5 variables:
    %'year,Month,day,hour,Holiday Flag'
    W=1; Date=zeros(5,5);
    for II=Y:48:length(TA)
        Date(1:5,W)=TA([1:4 6],II);
        W=W+1;
    end
    
    %insert date , coordinates and station ifd to the finaltable.
    HourlySum(7:end,1:2)=num2cell(DATA(:,1:2)); %Add coordinates
    HourlySum(7:end,3)=StNames(:,2); %Add StationID
    HourlySum(7:end,4:size(D,2))=num2cell(D(:,4:size(D,2)));%Add var data
    HourlySum(1:5,4:size(HourlySum,2))=num2cell(Date(1:5,1:size(Date,2)));
    HourlySum(1:5,3)={'Year';'Month';'Day';'Hour';'Holiday'};
    HourlySum(6,1:3)={'X';'Y';'StationID'};

    Outfilename=['HourlySum' '_' Infilename{I} '.mat']
    % cell2csv(Outfilename,HourlySum);
    s=cell2table(HourlySum);
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\work\Meteorological_Data\Hourly_data\AQUA_Hourly_data_May16'
    writetable(s,['HourlySum' '_' Infilename{I}]);
    save (Outfilename,'HourlySum');
    
   







%end