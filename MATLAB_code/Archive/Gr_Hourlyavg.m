%This function reads ground monitoring data, calculates daily average for
%a specific time range according to AQUA and TERRA overpass time
%and organizes as a final table for each var with first two columns (x,y-
%Israel cooredinates),hourly avg,NaN expresses days with no data at all.
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';
clear
clc


for I=1:12
    Infilename={'PM25','PM10','RH','Temp','WD','SR','WS','Rain','NO2','NO','O3','SO2'};
    file= ['LoadDataVar_' Infilename{I} '_2005#2015.mat']; 
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';

    load (file);
    
    % Calculate hourly average for each variable. Resulting table: each row is a
    % station
    K=4;HourlyAVG=cell(size(DATA,1)+6,1);temp=0;D=zeros(size(DATA,1),1);
   Y=60; %terra overpass- 9:30-11:30 -column number for 09:00 am 1-1-2005; see in TA (9 hours x 6 time points = 54 +6 to start from 09:00)
    for J=Y+2:144:length(DATA);  
        temp=DATA(:,J:J+15); % dataset including 09:00 till 11:30 only
        D(:,K)=nanmean(temp,2); K=K+1; % calculate the mean without NA 
        temp=[];
    end
    
    %Find indices of date and make a matrix of 5 variables:
    %'year,Month,day,Hour,Holiday Flag'
    W=1; Date=zeros(5,5);
    for II=Y:144:length(TA)
        Date(1:5,W)=TA([1:4 7],II);
        W=W+1;
    end
    
    %insert date , coordinates and station ifd to the finaltable.
    HourlyAVG(7:end,1:2)=num2cell(DATA(:,1:2)); %Add coordinates
    HourlyAVG(7:end,3)=StNames(:,2); %Add StationID
    HourlyAVG(7:end,4:size(D,2))=num2cell(D(:,4:size(D,2)));%Add var data
    HourlyAVG(1:5,4:size(HourlyAVG,2))=num2cell(Date(1:5,1:size(Date,2)));
    HourlyAVG(1:5,3)={'Year';'Month';'Day';'Hour';'Holiday'};
    HourlyAVG(6,1:3)={'X';'Y';'StationID'};

    Outfilename=['HourlyAVG' '_' Infilename{I} '.mat']
    % cell2csv(Outfilename,HourlyAVG);
    s=cell2table(HourlyAVG);
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016\Hourly_data'
    writetable(s,['HourlyAVG' '_' Infilename{I}]);
    save (Outfilename,'HourlyAVG');
    
    
end








%end