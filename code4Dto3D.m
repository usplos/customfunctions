Funpattern = 'FunImg*'; % Set the pattern of the folders where Functional Images are
Subpattern = 'Sub*'; % Set the pattern of the folders for each subjects
%%
Funfolders = dir(Funpattern);
Subfolders = dir([Funfolders(1).folder '\' Funfolders(1).name '\' Subpattern]);

for ff=1:length(Funfolders)
    for ss=1:length(Subfolders)
        wd = [Funfolders(ff).folder '\' Funfolders(ff).name '\' Subfolders(ff).name];
        eval(['cd ' wd]);
        file4D = dir('*.nii');
        % [y0,Head] = y_Read(file4D.name);
        [y0,voxelsize,Filelist,Head] = y_ReadAll(file4D.name);
        Size = size(y0);
        for ii=1:Size(4)
            y1 = y0(:,:,:,ii);
            if ii > 100
                id = num2str(ii);
            else if ii > 9
                    id = ['0' num2str(ii)];
                else
                    id = ['00' num2str(ii)];
                end
            end
            y_Write(y1,Head,['Fun' num2str(ff) '_' Subfolders(ss).name '_' id '.nii']);
        end
        delete(file4D.name);
    end
end