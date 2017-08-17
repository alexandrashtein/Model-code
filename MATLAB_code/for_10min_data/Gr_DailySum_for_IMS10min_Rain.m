%This function reads ground monitoring data, calculates daily average
%and organizes as a final table for each var with first two columns (x,y-
%Israel cooredinates),daily avg,NaN expresses days with no data at all.
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';
clear
clc


for I=4:12
    Infilename={'PM25','PM10','RH','Temp','WD','SR','WS','Rain','NO2','NO','O3','SO2'};
    file= ['LoadDataVar_' Infilename{I} '_2005#2015.mat']; 
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';

    load (file);
    
    % Calculate daily average for each variable. Resulting table: each row is a
    % station
    K=4;DailySum=cell(size(DATA,1)+6,1);temp=0;D=zeros(size(DATA,1),1);
    Y=1; %column number for 1-1-2005; see in TA
    for J=Y+2:144:length(DATA); % 6 time points in 1 hour x 24 hours = 144
        temp=DATA(:,J:J+143); % dataset including 00:10 till 00:00 
        D(:,K)=nansum(temp,2); K=K+1;
        temp=[];
    end
    
    %Find indices of date and make a matrix of 5 variables:
    %'year,Month,day,day of week,Holiday Flag'
    W=1; Date=zeros(5,5);
    for II=Y:144:length(TA)
        Date(1:5,W)=TA([1:3 6:7],II); 
        W=W+1;
    end
    
    %insert date, coordinates and station ifd to the finaltable.
    DailySum(7:end,1:2)=num2cell(DATA(:,1:2)); %Add coordinates
    DailySum(7:end,3)=StNames(:,2); %Add StationID
    DailySum(7:end,4:size(D,2))=num2cell(D(:,4:size(D,2)));%Add var data
    DailySum(1:5,4:size(DailySum,2))=num2cell(Date(1:5,1:size(Date,2)));
    DailySum(1:5,3)={'Year';'Month';'Day';'DOW';'Holiday'};
    DailySum(6,1:3)={'X';'Y';'StationID'};

    Outfilename=['DailySum' '_' Infilename{I} '.mat']
    %cell2csv(Outfilename,DailySum);
    s=cell2table(DailySum);
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\work\Meteorological_Data\Daily_Data\Daily_Data_Yuval\IMS_stn_July16'
    writetable(s,['DailySum' '_' Infilename{I}]);
    save (Outfilename,'DailySum');
    
    
end








%end