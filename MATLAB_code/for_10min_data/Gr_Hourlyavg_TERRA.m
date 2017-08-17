%This function reads ground monitoring data, calculates daily average for
%a specific time range according to AQUA and TERRA overpass time
%and organizes as a final table for each var with first two columns (x,y-
%Israel cooredinates),hourly avg,NaN expresses days with no data at all.
% cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';

% Last update: 30.03.2017
clear
clc


for I=3:6
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_082016';
    Infilename={'PM25','PM10','RH','Temp','WS','Rain','NO2','NO','O3','SO2'};
    file= ['LoadDataVar_' Infilename{I} '_2005#2015.mat']; 
    % cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016';
    load (file);
    
    % Calculate hourly average for each variable. Resulting table: each row is a
    % station
    K=4;HourlyAVG=cell(size(DATA,1)+6,1);temp=0;D=zeros(size(DATA,1),1);
    %terra overpass- 9:30-11:30
    % Y=54; % column number for 09:00 am 1-1-2005; see in TA (9 hours x 6 time points = 54 to start from 09:00)
    Y=45; % column number for 07:30 am 1-1-2005; see in TA (7.5 hours x 6 time points = 45 to start from 07:30)
    
    for J=Y+2:144:length(DATA);  
        %temp=DATA(:,J:J+15); % dataset including 09:00 till 11:30 only
        temp=DATA(:,J:J+35); % dataset including 07:30 till 13:30 only
        D(:,K)=nanmean(temp,2); K=K+1; % calculate the mean without NA 
        temp=[];
    end
    
    %Find indices of date and make a matrix of 5 variables:
    %'year,Month,day,Hour,Holiday Flag'
    W=1; Date=zeros(5,5);
    for II=Y:144:length(TA)
        Date(1:5,W)=TA([1:5],II);
        W=W+1;
    end
    
    %insert date , coordinates and station ifd to the finaltable.
    HourlyAVG(7:end,1:2)=num2cell(DATA(:,1:2)); %Add coordinates
    HourlyAVG(7:end,3)=StNames(:,2); %Add StationID
    HourlyAVG(7:end,4:size(D,2))=num2cell(D(:,4:size(D,2)));%Add var data
    HourlyAVG(1:5,4:size(HourlyAVG,2))=num2cell(Date(1:5,1:size(Date,2)));
    HourlyAVG(1:5,3)={'Year';'Month';'Day';'Hour';'Minutes'};
    HourlyAVG(6,1:3)={'X';'Y';'StationID'};
    
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\work\Meteorological_Data\Hourly_data\AQUA_Hourly_data_Aug16\TERRA'
    % Outfilename=['HourlyAVG_09301130' '_' Infilename{I} '.mat']
    Outfilename=['HourlyAVG_07301330' '_' Infilename{I} '.mat']
    % cell2csv(Outfilename,HourlyAVG);
    s=cell2table(HourlyAVG);
    % writetable(s,['HourlyAVG_09301130' '_' Infilename{I}]);
    writetable(s,['HourlyAVG_07301330' '_' Infilename{I}]);
    save (Outfilename,'HourlyAVG');
    
    
end

%% create manually table - change name for each variable
% Create table of ground monitoring data with columns:
% Day,month,year,station id,x,y (Israel grid),var name
clear
clc
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\work\Meteorological_Data\Hourly_data\AQUA_Hourly_data_Aug16\TERRA'
%Create cell variable ('DATA') with a column for each variable (defined in
%'colnames')
for I=3:6
    Infilename={'PM25','PM10','RH','Temp','WS','Rain','NO2','NO','O3','SO2'};
    %file= ['HourlyAVG_09301130_' Infilename{I} '.mat'];
    file= ['HourlyAVG_07301330_' Infilename{I} '.mat'];
    load (file)
    
    DATA=cell(10,9);
    %load PMData.mat;
    temp=HourlyAVG;
    k=2; colnames={temp{1:5,3}, temp{6, 1:2},'stn', Infilename{I}};  id=temp(7,3);
    DATA(1,1:length(colnames))=colnames;
    
    for W=4:size(temp,2)
        date=temp(1:5,W);
        for D=7:size(temp,1)
            DATA(k,1:5)=transpose(date);
            DATA(k,6:8)=temp(D,1:3);
            DATA(k,9)=temp(D,W);
            k=k+1;
        end
    end
    
    %save ([Infilename{I} '_D'],'DATA');
    R=size(DATA,1);
    s=cell2table(DATA(2:R,:));
    H=DATA(1,:);
    s.Properties.VariableNames=H;
    % writetable(s,[Infilename{I} '_H_09301130.csv']);
    writetable(s,[Infilename{I} '_H_07301330.csv']);
end










%end