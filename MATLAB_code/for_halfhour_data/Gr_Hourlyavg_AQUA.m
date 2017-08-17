%This function reads ground monitoring data, calculates daily average for
%a specific time range according to AQUA and TERRA overpass time
%and organizes as a final table for each var with first two columns (x,y-
%Israel cooredinates),hourly avg,NaN expresses days with no data at all.
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_052016';
clear
clc

for I=5:12
    Infilename={'PM25','PM10','RH','Temp','WD','SR','WS','Rain','NO2','NO','O3','SO2'};
    file= ['LoadDataVar_' Infilename{I} '_2000#2015.mat']; 
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_052016';

    load (file);
    
    % Calculate hourly average for each variable. Resulting table: each row is a
    % station
    K=4;HourlyMean=cell(size(DATA,1)+6,1);temp=0;D=zeros(size(DATA,1),1);
    %Y=25; %column number for 11:30 am 1-1-2000; see in TA
    Y=21; %column number for 10:00 am 1-1-2000; see in TA
    for J=Y+2:48:length(DATA);  
        %temp=DATA(:,J:J+5); % dataset including 11:30 till 14:00 only
        temp=DATA(:,J:J+12); % dataset including 10:00 till 16:00 only
        D(:,K)=nanmean(temp,2); K=K+1; % calculate the mean without NA 
        temp=[];
    end
    
    %Find indices of date and make a matrix of 5 variables:
    %'year,Month,day,hour,Holiday Flag'
    W=1; Date=zeros(5,5);
    for II=Y:48:length(TA)
        Date(1:5,W)=TA([1:5],II);
        W=W+1;
    end
    
    %insert date , coordinates and station ifd to the finaltable.
    HourlyMean(7:end,1:2)=num2cell(DATA(:,1:2)); %Add coordinates
    HourlyMean(7:end,3)=StNames(:,2); %Add StationID
    HourlyMean(7:end,4:size(D,2))=num2cell(D(:,4:size(D,2)));%Add var data
    HourlyMean(1:5,4:size(HourlyMean,2))=num2cell(Date(1:5,1:size(Date,2)));
    HourlyMean(1:5,3)={'Year';'Month';'Day';'Hour';'DOW'};
    HourlyMean(6,1:3)={'X';'Y';'StationID'};
    
    cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\work\Meteorological_Data\Hourly_data\AQUA_Hourly_data_May16'
    Outfilename=['HourlyMean1016' '_' Infilename{I} '.mat']
    % cell2csv(Outfilename,HourlyMean);
    s=cell2table(HourlyMean);
    writetable(s,['HourlyMean1016' '_' Infilename{I}]);
    save (Outfilename,'HourlyMean');
    
    %% create manually table - change name for each variable
    % Create table of ground monitoring data with columns:
    % Day,month,year,station id,x,y (Israel grid),var name
     %Create cell variable ('DATA') with a column for each variable (defined in
     %'colnames')
    file= ['HourlyMean1016_' Infilename{I} '.mat'];
    load (file)
    
    DATA=cell(10,9);
    %load PMData.mat;
    temp=HourlyMean;
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
    writetable(s,[Infilename{I} '_H_1016.csv']);
    
%end
end
