% %% Create table of ground monitoring data with columns: Day,month,year,station id,x,y (IWDael grid),pm25,pm10
% updated 11-08-2014

clear
clc
cd 'N:\Projects\P028.IL.Israel.MAIAC.PM.V2\raw\Meteorological_Data\Yuvals_Stations\Data_IMS_072016\Hourly_data';
%Create cell variable ('DATA') with a column for each variable (defined in
%'colnames')
%
% DATA25=cell(10,10);
% load PMData.mat;
%
% k=2; colnames={PM25{1:5,3}, PM25{6,[3 1 2]}, 'PM25'};  id=PM25(7,3);
% DATA25(1,1:length(colnames))=colnames;
%
% for W=4:size(PM25,2)
%     date=PM25(1:5,W);
%     for D=7:size(PM25,1)
%         DATA25(k,1:5)=transpose(date);
%         DATA25(k,6:8)=PM25(D,1:3);
%         DATA25(k,9)=PM25(D,W);
%         k=k+1;
%     end
% end
%
% save ('PMData25','DATA25');
% cell2csv('PMData25.csv',DATA25)
%% create manually table - change name for each variable
% Create table of ground monitoring data with columns:
% Day,month,year,station id,x,y (Israel grid),var name
clear
clc
cd 'N:/Projects/P028.IL.Israel.MAIAC.PM.V2/work/Meteorological_Data/Hourly_data/TERRA_Hourly_data_May16';
%Create cell variable ('DATA') with a column for each variable (defined in
%'colnames')
I=8
    Infilename={'PM25','PM10','RH','Temp','WD','SR','WS','Rain','NO2','NO','O3','SO2'};
    file= ['HourlySum_' Infilename{I} '.mat'];
    load (file)
    
    DATA=cell(10,9);
    %load PMData.mat;
    temp=HourlySum;
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
    
    save ([Infilename{I} '_D'],'DATA');
    R=size(DATA,1);
    s=cell2table(DATA(2:R,:));
    H=DATA(1,:);
    s.Properties.VariableNames=H;
    writetable(s,[Infilename{I} '_H_sum.csv']);
