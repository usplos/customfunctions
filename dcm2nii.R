library(tidyverse);library(rio);rm(list = ls())

dcm_wd = 'E:/project3_nback/备份dicom'
FunSession = 4:7
T1Session = 8
Disk='/e'
Out = getwd()#'E:/dcm2nii'
dcm2niix = '/e/dcm2nii/dcm2niix.exe'
SubID = 49:49

SubDir = dir(path = dcm_wd,pattern = '*')
Subs = SubDir %>% substr(x = ., start = nchar(.)-1, stop = nchar(.)) %>% as.integer()
SubDir = SubDir[Subs %in% SubID]
Subs = Subs[Subs %in% SubID]

Fun_dcm_wd = paste(dcm_wd, SubDir,'scans',
                   rep(FunSession,each = length(SubDir)),sep = '/')

Fun_dcm_wd = paste0(Disk,Fun_dcm_wd %>% substr(x = ., start = 3, stop = nchar(.)))

T1_dcm_wd = paste(dcm_wd, SubDir,'scans',
                  rep(T1Session,each = length(SubDir)),sep = '/')
T1_dcm_wd = paste0(Disk, T1_dcm_wd %>% substr(x = ., start = 3, stop = nchar(.)))

for(ss in 1:length(FunSession)){
  if(ss == 1){
    dir.create(paste0(Out,'/','FunImg'))
  }else{
    dir.create(paste0(Out,'/','S',ss,'_FunImg'))
  }
}
dir.create(paste0(Out,'/','T1Img'))

FunDir = dir(Out,'FunImg')
for (ff in 1:length(FunDir)) {
  #ff=1
  Subdir = paste('Sub',ifelse(Subs < 9,'0',''),Subs,sep = '')
  mapply(dir.create, paste0(Out,'/',FunDir[[ff]],'/',Subdir))
}
T1Dir = dir(Out,'T1Img')
Subdir = paste('Sub',ifelse(Subs < 9,'0',''),Subs,sep = '')
mapply(dir.create, paste0(Out,'/',T1Dir,'/',Subdir))


Outdcm2nii = paste0(Out,'/',rep(FunDir, each = length(Subs)),'/',
                    paste('Sub',ifelse(Subs < 9,'0',''),Subs,sep = ''))
Outdcm2nii = paste0('/e',Outdcm2nii %>% substr(x = ., start = 3, nchar(.)))

Outdcm2niiT1 = paste0(Out,'/',rep(T1Dir, each = length(Subs)),'/',
                    paste('Sub',ifelse(Subs < 9,'0',''),Subs,sep = ''))
Outdcm2niiT1 = paste0('/e',Outdcm2niiT1 %>% substr(x = ., start = 3, nchar(.)))


Funcode = paste0(dcm2niix, ' -o ','"',
       Outdcm2nii,'"',
       ' -z 3 ', Fun_dcm_wd) %>% as_tibble()
T1code = paste0(dcm2niix, ' -o ','"',
                Outdcm2niiT1,'"',
                ' -z 3 ', T1_dcm_wd) %>% as_tibble()

dcmcode = bind_rows(Funcode,T1code)# %>% export('dcmcode.txt',quote=F)

Sub = strsplit(dcmcode[[1]],'Sub') %>% unlist() %>% .[c(F,T)] %>% substr(x = ., start = 1,stop = 2)
dcmcode[['Sub']] = Sub

export(dcmcode,'dcmcode.xlsx',quote=F)

export(dcmcode[1], 'dcm2nii.txt',quote=F, col.names=F)
file.rename('dcm2nii.txt','dcm2nii.sh')

cat('\014','\n','Please run < sh dcm2nii.sh > in Git')
