%%
% load 

chinastat = load('chinastat.mat');  

chinastat_chinadata = chinastat.chinadata ; 
chinastat.chinadata = [] ; 

chinastat_XYZhat = chinastat.XYZhat ; 
chinastat.XYZhat = []; 

chinastat_XYZres = chinastat.XYZres ; 
chinastat.XYZres = []; 

chinastat_D3XYZ = chinastat.D3XYZ ; 
chinastat.D3XYZ = []; 

chinastat_DXYZ = chinastat.DXYZ ; 
chinastat.DXYZ = []; 

chinastat_DXYZsmo = chinastat.DXYZsmo ; 
chinastat.DXYZsmo = []; 

chinastat_DXYZreg = chinastat.DXYZreg ; 
chinastat.DXYZreg = []; 

chinastat_XYZreg = chinastat.XYZreg ; 
chinastat.XYZreg = []; 

chinastat_D3XYZreg = chinastat.D3XYZreg ; 
chinastat.D3XYZreg = []; 

chinastat_D2XYZreg = chinastat.D2XYZreg ; 
chinastat.D2XYZreg = []; 

%%
% save 
save('chinastat_chinadata.mat', 'chinastat_chinadata') ; 

save('chinastat_XYZhat.mat', 'chinastat_XYZhat') ; 

save('chinastat_XYZres.mat', 'chinastat_XYZres') ; 

save('chinastat_D3XYZ.mat', 'chinastat_D3XYZ') ; 

save('chinastat_DXYZ.mat', 'chinastat_DXYZ') ; 

save('chinastat_DXYZsmo.mat', 'chinastat_DXYZsmo') ; 

save('chinastat_DXYZreg.mat', 'chinastat_DXYZreg') ; 

save('chinastat_XYZreg.mat', 'chinastat_XYZreg') ; 

save('chinastat_D3XYZreg.mat', 'chinastat_D3XYZreg') ; 

save('chinastat_D2XYZreg.mat', 'chinastat_D2XYZreg') ; 

save('chinastat_smaller.mat', 'chinastat') ; 




