
addpath ('c:\matlab7\fdaM')
addpath ('c:\matlab7\fdaM\examples\handwrit')

%  Last modified 15 December 2004

%  ------------------------------------------------------------
%            Analyses of 50 productions of the 
%           Chinese characters for "Statistics"
%  ------------------------------------------------------------

%  The data are in file Chinastat.txt in the form of 
%  of lines containing three values, corresponding to X, Y and Z
%  coordinates.  There is a line for each of 2401 time values 
%  equally spaced between 0 and 6 seconds, and these are nested within
%  50 replications, for a total of 120050 lines.   
%  The coordinate values are in metres, and X is the horizontal 
%  direction on the writing surface, Y the vertical direction on 
%  the surface, and Z is perpendicular to the writing surface.

%   load previous analysis results

load chinastat

%  Input the data

load chinastat.txt

XYZ = reshape(chinastat, [2401,50,3])./1000;

%  set up the time values

n   = 2401;
sec = linspace(0,6,n)';
records = 1:50;

%  set up functions to be smoothed

X = XYZ(:,:,1);
Y = XYZ(:,:,2);
Z = XYZ(:,:,3);

%  plot each of the coordinate directions

subplot(3,1,1)
plot(sec, X, '-')
ylabel('\fontsize{16} X (meters)')
subplot(3,1,2)
plot(sec, Y, '-')
ylabel('\fontsize{16} Y (meters)')
subplot(3,1,3)
plot(sec, Z, '-')
xlabel('\fontsize{16} Seconds')
ylabel('\fontsize{16} Z (meters)')

%  set up basis for smoothing
%  knots at every 10 seconds ensures enough resolution
%  given what is known about muscle dynamics

nbasis     = 606;
norder     =   7;  %  order 7 for a smooth 3rd deriv.
chinabasis = create_bspline_basis([0,6],nbasis,norder);
lambda     = 1e-14;   %  gives std err of 0.5 mm
XYZsmthfdPar = fdPar(chinabasis, 5, lambda);

%  smooth all three coordinates

[XYZfd, df, gcv] = smooth_basis(sec, XYZ, XYZsmthfdPar);

%  display standard error

XYZhat = eval_fd(sec, XYZfd);
XYZres = XYZ - XYZhat;
XYZvar = sum(sum(sum(XYZres.^2)))./(150*(n - df));
XYZstderr = sqrt(XYZvar)

%     0.00045  Signal-to-noise ratio of about 400 to 1

%  display third X-derivatives

D3XYZ = eval_fd(sec, XYZfd, 3);

subplot(3,1,1)
plot(sec, D3XYZ(:,:,1), '-')
ylabel('\fontsize{16} D^3 X')
axis([0,6,-500,500])
subplot(3,1,2)
plot(sec, D3XYZ(:,:,2), '-')
ylabel('\fontsize{16} D^3 Y')
axis([0,6,-500,500])
subplot(3,1,3)
plot(sec, D3XYZ(:,:,3), '-')
ylabel('\fontsize{16} D^3 Z')
axis([0,6,-500,500])

%  Compute tangential accelerations

D2X  = eval_fd(sec, XYZfd(:,1), 2);
D2Y  = eval_fd(sec, XYZfd(:,2), 2);
D2XY = sqrt(D2X.^2 + D2Y.^2);

%  plot tangential accelerations

subplot(1,1,1)
plot(sec, D2XY, '-')
axis([0,6,0,30])
xlabel('\fontsize{19} Seconds')
ylabel('\fontsize{19} Tangential Acceleration')

%  set up array of first derivative values

DXYZ = eval_fd(sec, XYZfd, 1);

%  plot first derivatives

subplot(3,1,1)
plot(sec, DXYZ(:,:,1), '-')
ylabel('\fontsize{16} D X')
axis([0,6,-1,1])
subplot(3,1,2)
plot(sec, DXYZ(:,:,2), '-')
ylabel('\fontsize{16} D Y')
axis([0,6,-1,1])
subplot(3,1,3)
plot(sec, DXYZ(:,:,3), '-')
ylabel('\fontsize{16} D Z')
axis([0,6,-1,1])

%  set up functional data object for first derivatives

nDbasis     = 605;
norder      =   6;
chinaDbasis = create_bspline_basis([0,6],nDbasis,norder);
lambda      = 1e-14;   %  gives std err of 0.5 mm
DXYZsmthfdPar = fdPar(chinaDbasis, 4, lambda);

%  smooth the derivatives

DXYZfd = smooth_basis(sec, DXYZ, DXYZsmthfdPar);

DXYZsmo = eval_fd(sec, DXYZfd);

DXYZmean = squeeze(mean(DXYZsmo, 2));

%  ------------------------------------------------------
%                   Registration phases
%  Registration takes place in two phases:
%  In the first, landmark registration is used.  The
%  landmarks are the peaks in the Z-coordinate during
%  the three major strokes that separate the characters,
%  during which the pen is raised.
%  The second phase consists of continuously registering
%  the landmark-registered curves.  This goes faster and
%  is more stable because the bulk of the registration
%  is already done with the faster landmark registration
%  process.
%  First derivatives and coordinates are registered 
%  separately.  This is because the first derivatives
%  are the base variables for estimating the differential
%  equations that model the pen velocities along X, Y and
%  Z coordinates.  Coordinate values are mainly used only
%  for displaying results.
%  ------------------------------------------------------


%  plot mean Z function and locate times of the
%  three major crossings, corresponding to raised strokes
%  separating characters

plot(sec, DXYZmean(:,3), '-', [0,6], [0,0], 'r:')
ginput(3)

%  Register first derivatives using landmark registration
%  The landmarks are the three peaks in Z separating
%  the characters.


Zpeaktimes = [0.795,2.537,4.016];

%  locate peak times for each record

Zcross = zeros(50,3,2);
subplot(1,1,1)
for i=records
    lhdl = plot(sec, DXYZ(:,i,3), '-');
    set(lhdl, 'LineWidth', 2)
    hold on
    lhdl = plot(sec, DXYZmean(:,3), '--', ...
                [0,6], [0,0], 'r:', ...
                [0.795,0.795], [-0.25,0.25], 'r--', ...
                [2.537,2.537], [-0.25,0.25], 'r--', ...
                [4.016,4.016], [-0.25,0.25], 'r--');
    set(lhdl, 'LineWidth', 1)
    axis([0,6,-.25,.25])
    title(['Record ',num2str(i)])
    hold off
    Zcross(i,:,:) = ginput(3);
end

Zcross = Zcross(:,:,1);  %  ignore y coordinates

plot(Zcross(:,1), Zcross(:,2), 'o')
plot(Zcross(:,1), Zcross(:,3), 'o')
plot(Zcross(:,2), Zcross(:,3), 'o')

mean(Zcross)
%    0.7772    2.5642    3.9946

sqrt(var(Zcross))
%    0.0452    0.0772    0.0808

%  set up basis for landmark registraton

nwbasis = 7;
nworder = 4;
wbreaks = [0, Zpeaktimes, 6];
wbasis  = create_bspline_basis([0,6], nwbasis, nworder, wbreaks);
WfdPar  = fdPar(wbasis);

%  register the first derivatives 

[DXYZregfd, warpfd, Wfd] = ...
    landmarkreg(DXYZfd, Zcross, Zpeaktimes, WfdPar, 1);

%  plot the registered and unregistered first derivatives

DXYZreg = eval_fd(sec, DXYZregfd);

for j=1:3
    subplot(2,1,1)
    plot(sec, squeeze(DXYZreg(:,:,j)), '-')
    subplot(2,1,2)
    plot(sec, squeeze(DXYZ(:,:,j)), '-')
    pause
end

%  register the coordinates 

[XYZregfd, warpfd, Wfd] = ...
    landmarkreg(XYZfd, Zcross, Zpeaktimes, WfdPar, 1);

%  plot the registered and unregistered coordinates

XYZreg = eval_fd(sec, XYZregfd);

for j=1:3
    subplot(2,1,1)
    plot(sec, squeeze(XYZreg(:,:,j)), '-')
    subplot(2,1,2)
    plot(sec, squeeze(XYZ(:,:,j)), '-')
    pause
end

%  compute the mean landmark-registered derivatives

DXYZmean = zeros(2401,1,3);
for j=1:3
    DXYZmean(:,1,j) = mean(squeeze(DXYZreg(:,:,j)),2);
end

DXYZmeanfd = smooth_basis(sec, DXYZmean, DXYZsmthfdPar);

%  now refine the registration using continuous registration

%  set up basis for continuous registration

wnbasis = 23;
wbasis  = create_bspline_basis([0,6],wnbasis);
Wfd0    = fd(zeros(wnbasis,50),wbasis);
Wfd0Par = fdPar(Wfd0);

%  set arrays and parameters for continuous registration

periodic = 0;
crit     = 2;
conv     = 1e-4;
iterlim  = 40;
dbglev   = 1;

%  carry out the continuous registration

[DXYZregfd, Wfd] = ...
      registerfd(DXYZmeanfd, DXYZregfd, Wfd0Par, ...
                   periodic, crit, conv, iterlim, dbglev);

%  plot the registered and unregistered curves

DXYZreg = eval_fd(sec, DXYZregfd);
for j=1:3
    subplot(2,1,1)
    plot(sec, squeeze(DXYZreg(:,:,j)), '-')
    subplot(2,1,2)
    plot(sec, squeeze(DXYZ(:,:,j)), '-')
    pause
end

%  carry out the continuous registration of the coordinates

XYZmeanfd = mean(XYZregfd);

coefw   = getcoef(Wfd);
Wfd0    = fd(coefw,wbasis);
Wfd0Par = fdPar(Wfd0);

iterlim = 1;
XYZregfd = registerfd(XYZmeanfd, XYZregfd, Wfd0Par, ...
                   periodic, crit, conv, iterlim, dbglev);

%  plot the registered and unregistered curves

XYZreg = eval_fd(sec, XYZregfd);
for j=1:3
    subplot(2,1,1)
    plot(sec, squeeze(XYZreg(:,:,j)), '-')
    subplot(2,1,2)
    plot(sec, squeeze(XYZ(:,:,j)), '-')
    pause
end

%  plot selected curves

XYZmean = eval_fd(sec, XYZmeanfd);
i = 22;
for j=1:3
    subplot(2,1,1)
    plot(sec, squeeze(XYZreg(:,i,j)), 'b-', ...
         sec, XYZmean(:,j), 'g--', ...
         [0,6], [0,0], 'r:')
    title(['Record ',num2str(i)])
    subplot(2,1,2)
    plot(sec, squeeze(DXYZreg(:,i,j)), 'b-', ...
         sec, DXYZmean(:,j), 'g--', ...
         [0,6], [0,0], 'r:')
    pause
end

%  compute registered tangential accelerations

D2XYZreg = eval_fd(sec, DXYZregfd, 1);
D2XYZregfd = smooth_basis(sec, D2XYZreg, chinabasis);
D2XYreg  = sqrt(D2XYZreg(:,:,1).^2 + ...
                D2XYZreg(:,:,2).^2);

D2XYZmeanfd = mean(D2XYZregfd);

D2XYZmean = eval_fd(sec, D2XYZmeanfd);

%  plot tangential accelerations

subplot(2,1,1)
plot(sec, D2XY, '-')
axis([0,6,0,25])
ylabel('\fontsize{12} Tan. Accel.')

subplot(2,1,2)
plot(sec, D2XYreg, '-')
axis([0,6,0,25])
xlabel('\fontsize{12} Seconds')
ylabel('\fontsize{12} Reg. Tan. Accel.')

subplot(1,1,1)
plot(sec, D2XYreg, '-')
axis([0,6,0,25])
xlabel('\fontsize{19} Seconds')
ylabel('\fontsize{19} Tangential Acceleration')

%  phase-plane plots

for i=records
    subplot(1,2,1)
    lhdl = plot(DXYZreg(:,i,1), D2XYZreg(:,i,1), '-', ...
         [0,0], [-20,20], 'r:', ...
         [-1,1], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    title(['Record ',num2str(i)])
    axis([-1,1,-20,20])
    subplot(1,2,2)
    lhdl = plot(DXYZreg(:,i,2), D2XYZreg(:,i,2), '-', ...
         [0,0], [-20,20], 'r:', ...
         [-1,1], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    axis([-1,1,-20,20])
    pause
end

%  plot second derivative

%  two panel plot

for i=records
    subplot(2,1,1)
    lhdl = plot(sec, D2XYZreg(:,i,1), '-', ...
                [0,6], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    hold on
    for j=1:24
        xvl = j*0.24;
        plot([xvl,xvl],[-20,20],'g--')
    end
    hold off
    title(['Record ',num2str(i)])
    axis([0,6,-20,20])
    subplot(2,1,2)
    lhdl = plot(sec, D2XYZreg(:,i,2), '-', ...
                [0,6], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    hold on
    for j=1:24
        xvl = j*0.24;
        plot([xvl,xvl],[-20,20],'g--')
    end
    hold off
    axis([0,6,-20,20])
    pause
end

% single panel plot

for i=records
    subplot(1,1,1)
    lhdl = plot(sec, D2XYZreg(:,i,1), 'b-', ...
                sec, D2XYZreg(:,i,2), 'g-', ...
                [0,6], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    hold on
    for j=1:24
        xvl = j*0.24;
        plot([xvl,xvl],[-20,20],'r--')
    end
    for j=1:3
        lhdl = plot([landmarktimes(j),landmarktimes(j)], ...
                    [-20,20], 'm-');
        set(lhdl, 'LineWidth', 2)
    end
    hold off
    title(['Record ',num2str(i)])
    axis([0,6,-20,20])
    pause
end

%  plot third derivatives

D3XYZreg   = eval_fd(sec, DXYZregfd, 2);
D3XYZregfd = smooth_basis(sec, D3XYZreg, chinabasis);

D3XYZmeanreg = squeeze(mean(D3XYZreg, 2));

for i=records
    subplot(2,1,1)
    lhdl = plot(sec, D3XYZreg(:,i,1), '-', ...
                [0,6], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    title(['Record ',num2str(i)])
    axis([0,6,-500,500])
    subplot(2,1,2)
    lhdl = plot(sec, D3XYZreg(:,i,2), '-', ...
                [0,6], [0,0], 'r:');
    set(lhdl, 'LineWidth', 2)
    axis([0,6,-500,500])
    pause
end

%  plot characters

subplot(1,1,1)
for i=records
    landmarks = zeros(3,2);
    for j=1:3
        landmarks(j,1) = eval_fd(landmarktimes(j), Xfd(i));
        landmarks(j,2) = eval_fd(landmarktimes(j), Yfd(i));
    end
    lhdl = plot(X(:,i), Y(:,i), '-');
    hold on
    for j=1:3
        plot(landmarks(j,1),landmarks(j,2),'o')
    end
    hold off
    pause
end

%  plot mean character

%  plot XY coordinates below a threshold

thresh = 0.0023;
inddn  = find(XYZmean(:,3) <= thresh);
indup  = find(XYZmean(:,3) >  thresh);
indtm  = 1:50:2401;

figure(1)
subplot(1,1,1)
for igap=50
    indtm  = 1:igap:2401;
    plot(XYZmean(inddn,1), XYZmean(inddn,2), 'b.')
    title(['\fontsize{19} Circles every ',num2str(1000*igap/400), ...
           ' milliseconds'])
    hold on
    lhdl = plot(XYZmean(:,1), XYZmean(:,2), 'r:');
    set(lhdl, 'LineWidth', 1)
    plot(XYZmean(indtm,1), XYZmean(indtm,2), 'ro')
    hold off
    pause
end

%  plot Z coordinate

figure(2)
subplot(1,1,1)
lhdl   = plot(sec(inddn), XYZmean(inddn,3), 'b.', ...
              sec(indup), XYZmean(indup,3), 'go', ...
              [0,6], [thresh, thresh], 'g--');
set(lhdl, 'LineWidth', 1)

%   ---------------------------------------------------------
%                   Principal Differential Analysis
%   ---------------------------------------------------------

%  set up a basis for constant coefficients

conbasis = create_constant_basis([0,6]);
zerofd   = fd(0, conbasis);
onesfd   = fd(ones(1,50),conbasis);

%  set up cell array for output functions

xfdcell{1,1} = XYZfd(:,1);
xfdcell{2,1} = XYZfd(:,2);

%  set up 2 by 2 by 3 cell array for derivative weight functions

lambda = 0;

bwtcell{1,1,1} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{1,1,2} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient
bwtcell{1,1,3} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient

bwtcell{1,2,1} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{1,2,2} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{1,2,3} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant

bwtcell{2,1,1} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{2,1,2} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{2,1,3} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant

bwtcell{2,2,1} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{2,2,2} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient
bwtcell{2,2,3} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient

%  set up cell array for constant forcing functions

ufdcell{1,1} = onesfd;
ufdcell{2,1} = onesfd;

%  set up cell array for forcing function weight functions

awtcell{1,1} = fdPar(fd(0,conbasis), 0, 0, 1);
awtcell{2,1} = fdPar(fd(0,conbasis), 0, 0, 0);

%  carry out the PDA

[bfdcell, resfdcell, afdcell] = ...
    pdacell(xfdcell, bwtcell, awtcell, ufdcell, 3, n);

subplot(2,1,1)
plot(afdcell{1,1})
subplot(2,1,2)
plot(afdcell{2,1})

subplot(2,1,1)
plot(getfd(bfdcell{1,1,2}))
subplot(2,1,2)
plot(getfd(bfdcell{1,1,3}))

subplot(2,1,1)
plot(getfd(bfdcell{2,2,2}))
subplot(2,1,2)
plot(getfd(bfdcell{2,2,3}))

subplot(2,1,1)
plot(resfdcell{1})
subplot(2,1,2)
plot(resfdcell{2})

%  ------------
%  X-coordinate
%  ------------

%  set up cell array for output functions

clear xfdcell
xfdcell{1} = XYZfd(:,1);

%  set up 2 by 2 by 3 cell array for derivative weight functions

lambda = 1e-8;

clear bwtcell
bwtcell{1,1} = fdPar(zerofd,     0, lambda, 0);  % unestimated zero constant
bwtcell{1,2} = fdPar(chinabasis, 2, lambda, 1);  % estimated coefficient
bwtcell{1,3} = fdPar(chinabasis, 2, lambda, 1);  % estimated coefficient

%  set up cell array for X forcing function

slopebasis = create_monomial_basis([0,6],2);
slopefd    = smooth_basis(sec, (sec-3)*ones(1,50), slopebasis);
clear ufdcell
ufdcell{1} = slopefd;

%  set up cell array for forcing function weight functions

clear awtcell
awtcell{1} = fdPar(fd(0,conbasis), 0, 0, 1);

%  carry out the PDA

[bfdcell, afdcell, resfdcell] = ...
    pdacell(xfdcell, bwtcell, awtcell, ufdcell, 3, n);

getcoef(getfd(afdcell{1}))

subplot(2,1,1)
bvec1 = eval_fd(sec,getfd(bfdcell{1,2})); 
plot(sec, bvec1, '-')
subplot(2,1,2)
bvec2 = eval_fd(sec,getfd(bfdcell{1,3})); 
plot(sec, bvec2, '-')

subplot(1,1,1)
resmat = eval_fd(sec, resfdcell{1});
plot(sec, resmat, '-', sec, D3XYZmeanreg(:,1), '--')

subplot(1,1,1)
resmat = eval_fd(sec, resfdcell{1});
plot(sec, mean(resmat, 2), '-', sec, D3XYZmeanreg(:,1), '--')

%  solve equation for functions spanning the null space of
%  the homogeneous operator

odeoptions = [];
ystart = eye(3);

dy = derivcell(0, ystart(:,1), bfdcell)

yp1 = zeros(n, 3);
yp1(:,1) = 1;

[tp2, yp2] = ode45(@derivcell, sec, ystart(:,2), odeoptions, bfdcell);
[tp3, yp3] = ode45(@derivcell, sec, ystart(:,3), odeoptions, bfdcell);

%  ------------
%  Y-coordinate
%  ------------

%  set up cell array for output functions

clear yfdcell
yfdcell{1} = XYZregfd(:,2);

%  set up 2 by 2 by 3 cell array for derivative weight functions

lambda = 1e-10;

clear bwtcell
bwtcell{1,1} = fdPar(zerofd,     0, lambda, 0);  
% unestimated zero constant
bwtcell{1,2} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient
bwtcell{1,3} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient

clear ufdcell
ufdcell = {};

%  set up cell array for forcing function weight functions

clear awtcell
awtcell = {};

%  carry out the PDA

[bfdcell, afdcell, resfdcell] = ...
    pdacell(yfdcell, bwtcell, awtcell, ufdcell, 3, n);

figure(1)
subplot(2,1,1)
bvec1 = eval_fd(sec,getfd(bfdcell{1,2})); 
plot(sec, bvec1, '-', ...
     [0,6], [0,0], 'r:')
axis([0,6,-100,1200])
ylabel('\fontsize{16} \beta_1')
subplot(2,1,2)
bvec2 = eval_fd(sec,getfd(bfdcell{1,3})); 
plot(sec, bvec2, '-', ...
     [0,6], [0,0], 'r:')
ylabel('\fontsize{16} \beta_2')
axis([0,6,-50,50])

figure(2)
subplot(1,1,1)
resmat = eval_fd(sec, resfdcell{1});
plot(sec, resmat, '-', sec, D3XYZmeanreg(:,1), '--')
axis([0,6,-500,500])

subplot(1,1,1)
resmat = eval_fd(sec, resfdcell{1});
plot(sec, mean(resmat, 2), '-', ...
     sec, D3XYZmeanreg(:,1), '--', ...
     [0,6], [0,0], 'r:')
axis([0,6,-500,500])

%  solve equation for functions spanning the null space of
%  the homogeneous operator

odeset(odeoptions, 'RelTol', 1e-7);
ystart = eye(3);

yp1 = zeros(n, 3);
yp1(:,1) = 1;

[tp2, yp2] = ode45(@derivcell, sec, ystart(:,2), odeoptions, bfdcell);
[tp3, yp3] = ode45(@derivcell, sec, ystart(:,3), odeoptions, bfdcell);

%  plot solutions

yarray = zeros(n,3,3);
yarray(:,:,1) = yp1;
yarray(:,:,2) = yp2;
yarray(:,:,3) = yp3;

figure(3)
for j=1:3
    subplot(3,1,1)
    plot(sec, yarray(:,1,j), '-', [0,6], [0,0], 'r:')
    ylabel('\fontsize{16} y')
    title(['\fontsize{16} Solution ',num2str(j)])
    subplot(3,1,2)
    plot(sec, yarray(:,2,j), '-', [0,6], [0,0], 'r:')
    ylabel('\fontsize{16} Dy')
    subplot(3,1,3)
    plot(sec, yarray(:,3,j), '-', [0,6], [0,0], 'r:')
    ylabel('\fontsize{16} D^2y')
    pause
end

%  reconstruct each curve and its derivatives

%  Y - curves

figure(5)
subplot(1,1,1)
Zmat = squeeze(yarray(:,1,:));
for i=records
    coef  = Zmat\XYZreg(:,i,2);
    yhat = Zmat*coef;
    plot(sec, XYZreg(:,i,2), '-', sec, yhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    pause
end

%  Y - first derivatives

subplot(1,1,1)
Zmat = squeeze(yarray(:,2,:));
for i=records
    coef  = Zmat\DXYZreg(:,i,2)
    Dyhat = Zmat*coef;
    plot(sec, DXYZreg(:,i,2), '-', sec, Dyhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    pause
end

%  Y - second derivatives

subplot(1,1,1)
Zmat = squeeze(yarray(:,3,:));
for i=records
    coef  = Zmat\D2XYZreg(:,i,2)
    D2yhat = Zmat*coef;
    plot(sec, D2XYZreg(:,i,2), '-', sec, D2yhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    pause
end

%  -----------------------------------------------------------
%             First derivative of Y coordinates
%  -----------------------------------------------------------

%  set up cell array for output functions

clear yfdcell
yfdcell{1} = DXYZregfd(:,2);

%  set up 2 by 2 by 3 cell array for derivative weight functions

lambda = 0;

clear bwtcell
bwtcell{1,1} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient
bwtcell{1,2} = fdPar(chinabasis, 2, lambda, 1);  
% estimated coefficient

clear ufdcell
ufdcell = {};

%  set up cell array for forcing function weight functions

clear awtcell
awtcell = {};

%  carry out the PDA

[bfdcell, afdcell, resfdcell] = ...
    pdacell(yfdcell, bwtcell, awtcell, ufdcell, 2, n);

% xfdcell{1} = DXYZregfd(:,2);
% xfdcell{2} = D2XYZregfd(:,2);
% 
% clear betacell
% betacell{1} = fdPar(chinabasis, 2, lambda, 1);  
% betacell{2} = fdPar(chinabasis, 2, lambda, 1);  
% 
% [betaestcell, yhatfdobj] = fRegress(-D3XYZregfd(:,2), xfdcell, betacell);
% 
% bfdcell{1,1} = betaestcell{1};
% bfdcell{1,2} = betaestcell{2};

figure(1)
subplot(2,1,1)
bvec1 = eval_fd(sec,getfd(bfdcell{1,1})); 
plot(sec, bvec1, '-', ...
     [0,6], [0,0], 'r:')
axis([0,6,-200,1500])
ylabel('\fontsize{16} \beta_1')
subplot(2,1,2)
bvec2 = eval_fd(sec,getfd(bfdcell{1,2})); 
plot(sec, bvec2, '-', ...
     [0,6], [0,0], 'r:')
ylabel('\fontsize{16} \beta_2')
axis([0,6,-50,50])

figure(2)
subplot(1,1,1)
resmat = eval_fd(sec, resfdcell{1});
plot(sec, mean(resmat, 2), '-', ...
     sec, D3XYZmeanreg(:,1), '--', ...
     [0,6], [0,0], 'r:')
axis([0,6,-500,500])

%  solve equation for functions spanning the null space of
%  the homogeneous operator

odeset(odeoptions, 'RelTol', 1e-7);
ystart = eye(2);
ystart(1,1) = DXYZmean(1,1,2);
ystart(2,2) = D2XYZmean(1,1,2);

[tp1, yp1] = ode45(@derivcell, sec, ystart(:,1), odeoptions, bfdcell);
[tp2, yp2] = ode45(@derivcell, sec, ystart(:,2), odeoptions, bfdcell);

% coef = [getcoef(getfd(bfdcell{1})), getcoef(getfd(bfdcell{2}))];
% wfd = fd(coef, chinabasis);
% 
% global wfd
% 
% [tp1, yp1] = ode45(@derivs, sec, ystart(:,1), odeoptions);
% [tp2, yp2] = ode45(@derivs, sec, ystart(:,2), odeoptions);

%  plot solutions

yarray = zeros(n,2,2);
yarray(:,:,1) = yp1;
yarray(:,:,2) = yp2;

figure(3)
for j=1:2
    subplot(2,1,1)
    plot(sec, yarray(:,1,j), '-', [0,6], [0,0], 'r:')
    ylabel('\fontsize{16} y')
    title(['\fontsize{16} Solution ',num2str(j)])
    subplot(2,1,2)
    plot(sec, yarray(:,2,j), '-', [0,6], [0,0], 'r:')
    ylabel('\fontsize{16} Dy')
    pause
end

%  reconstruct each curve and its derivatives

%  Y - curves

figure(5)
subplot(1,1,1)
Zmat = squeeze(yarray(:,1,:));
for i=records
    coef  = Zmat\DXYZreg(:,i,2);
    Dyhat = Zmat*coef;
    plot(sec, DXYZreg(:,i,2), '-', sec, Dyhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    axis([0,6,-1,1])
    pause
end

%  Y - first derivatives

subplot(1,1,1)
Zmat = squeeze(yarray(:,2,:));
for i=records
    coef  = Zmat\DXYZreg(:,i,2)
    Dyhat = Zmat*coef;
    plot(sec, DXYZreg(:,i,2), '-', sec, Dyhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    pause
end

%  Y - second derivatives

subplot(1,1,1)
Zmat = squeeze(yarray(:,3,:));
for i=records
    coef  = Zmat\D2XYZreg(:,i,2)
    D2yhat = Zmat*coef;
    plot(sec, D2XYZreg(:,i,2), '-', sec, D2yhat, '--', ...
         [0,6], [0,0], 'r:');
    title(['\fontsize{16} Record ',num2str(i)])
    pause
end

    
