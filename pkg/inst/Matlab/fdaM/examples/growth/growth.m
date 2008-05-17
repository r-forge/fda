%  Windows:

addpath ('c:\Program Files\Matlab\fdaM')
addpath ('c:\Program Files\Matlab\fdaM\examples\growth')

%  Last modified 31 July 2006

%  -----------------------------------------------------------------------
%                    Berkeley Growth Data
%  -----------------------------------------------------------------------

%  ------------------------  input the data  -----------------------

ncasem = 39;
ncasef = 54;
nage   = 31;

fid = fopen('hgtm.dat','rt');
hgtmmat = reshape(fscanf(fid,'%f'),[nage,ncasem]);

fid = fopen('hgtf.dat','rt');
hgtfmat = reshape(fscanf(fid,'%f'),[nage,ncasef]);

age = [ 1:0.25:2, 3:8, 8.5:0.5:18 ]';
rng = [1,18];

%  --------------  Smooth the data nonmonotonically  --------------
%  This smooth uses the usual smoothing methods to smooth the data,
%  but is not guaranteed to produce a monotone fit.  This may not
%  matter much for the estimate of the height function, but it can
%  have much more serious consequences for the velocity and
%  accelerations.  See the monotone smoothing method below for a
%  better solution, but one with a much heavier calculation overhead.

%  -----------  Create fd objects   ----------------------------

%  A B-spline basis with knots at age values and order 6 is used

knots    = age;
norder   = 6;
nbasis   = length(knots) + norder - 2;
hgtbasis = create_bspline_basis(rng, nbasis, norder, knots);

%  --- Smooth these objects, penalizing the 4th derivative  --
%  This gives a smoother estimate of the acceleration functions

%  set up roughness penalty smoothing function smooth_basis

Lfd      = int2Lfd(4);
lambda   = 1e-1;
hgtfdPar = fdPar(hgtbasis, Lfd, lambda);

%  smooth the data

hgtmfd = smooth_basis(age, hgtmmat, hgtfdPar);
hgtffd = smooth_basis(age, hgtfmat, hgtfdPar);

%  plot the data and the smooth

plotfit_fd(hgtmmat, age, hgtmfd)
plotfit_fd(hgtfmat, age, hgtffd)

%  ---------------  plot the first 10 female data  ---------------

%  Height

agefine = linspace(1,18,101)';
hgtfmatfine = eval_fd(agefine, hgtffd(1:10));

phdl = plot(agefine, hgtfmatfine, '-');
set(phdl, 'LineWidth', 2)
hold on
plot(age, hgtfmat(:,1:10), 'o')
hold off
xlabel('\fontsize{19} Age')
ylabel('\fontsize{19} Height (cm)')
axis([1,18,60,200])

%  Velocity

velfmatfine = eval_fd(agefine, hgtffd(1:10), 1);

phdl = plot(agefine, velfmatfine, '-');
set(phdl, 'LineWidth', 2)
xlabel('\fontsize{19} Age')
ylabel('\fontsize{19} Height Velocity (cm/yr)')
axis([1,18,0,20])

%  Acceleration

accfmatfine = eval_fd(agefine, hgtffd(1:10), 2);

phdl = plot(agefine, accfmatfine, '-', ...
            [1,18], [0,0], 'r:');
set(phdl, 'LineWidth', 2)
xlabel('\fontsize{19} Age')
ylabel('\fontsize{19} Height Acceleration (cm/yr/yr)')
axis([1,18,-4,2])


%  plot velocities with knots at each age

plot((1:10),1)
xlabel('\fontsize{16} Age')
ylabel('\fontsize{16} Velocity (cm/yr)')

%  plot velocities with 12 basis functions

hgtbasis = create_bspline_basis(rng, 12, norder);
hgtffd   = data2fd(hgtfmat, age, hgtbasis);

plot((1:10),1)
xlabel('\fontsize{16} Age')
ylabel('\fontsize{16} Velocity (cm/yr)')

%  plot accelerations with 12 basis functions

plot((1:10),2)
xlabel('\fontsize{16} Age')
ylabel('\fontsize{16} Acceleration (cm/yr^2)')

%  plot acceleration curves for the first 10 girls
%  estimated both by 12 basis
%  functions and by spline smoothing.

hgtbasis1 = create_bspline_basis(rng, 12, norder);
hgtffd1   = data2fd(hgtfmat, age, hgtbasis1);

subplot(1,2,1)
hgtfmat1 = eval_fd(agefine, hgtffd1(1:10), 2);
plot(agefine, hgtfmat1, 'k-')
xlabel('\fontsize{12} Age')
ylabel('\fontsize{12} Acceleration (cm/yr^2)')
axis([1,18,-40,10])
axis('square')

hgtbasis2 = create_bspline_basis(rng, 35, 6, age);
hgtfdPar2 = fdPar(hgtbasis2, 4, lambda);
hgtffd2   = smooth_basis(age, hgtfmat, hgtfdPar2);

subplot(1,2,2)
hgtfmat2 = eval_fd(agefine, hgtffd2(1:10), 2);
plot(agefine, hgtfmat2, 'k-')
xlabel('\fontsize{12} Age')
ylabel('\fontsize{12} Acceleration (cm/yr^2)')
axis([1,18,-12,2])
axis('square')

print -dps2 'c:/MyFiles/fdabook1/figs.dir/twoaccelplots.ps'

subplot(1,1,1)
hgtfmat2 = eval_fd(agefine, hgtffd2(1:10), 2);
phdl = plot(agefine, hgtfmat2, 'k-', [1,18], [0,0], 'k:');
set(phdl, 'LineWidth', 1)
lhdl = line(agefine, mean(hgtfmat2,2));
set(lhdl, 'LineStyle', '--')
xlabel('\fontsize{19} Age')
ylabel('\fontsize{19} Acceleration (cm/yr^2)')
axis([1,18,-4,2])

%  ----------------------------------------------------------
%  Estimate standard error of measurement for velocity and 
%    acceleration, re-smooth using the reciprocal of variance
%    of estimate as a weight, and display results
%  ----------------------------------------------------------

%  set up function smooth_pos

norderse = 3;
nbasisse = nage + norderse - 2;
stderrbasis = create_bspline_basis([1,18], nbasisse, norderse, age);
Wfd0   = fd(zeros(nbasisse,1),stderrbasis);  %  initial value for Wfd

%  Males

hgtmfit      = eval_fd(age, hgtmfd);
hgtmres      = hgtmmat - hgtmfit;   %  residuals
hgtmresmnsqr = mean(hgtmres.^2,2);  %  mean squared residuals

%  positively smooth the mean squared residuals

Lfdobj = 1;             %  smooth toward a constant
lambda = 1e-3;          %  smoothing parameter
hgtfdPar = fdPar(Wfd0, Lfdobj, lambda);

Wfd = smooth_pos(age, hgtmresmnsqr, hgtfdPar);

%  compute the variance and standard error of measurements

hgtmvar = eval_pos(age, Wfd);
hgtmstd = sqrt(hgtmvar);

subplot(1,1,1)
plot(age, sqrt(hgtmresmnsqr), 'o', age, hgtmstd, 'b-')

%  update weight vector for smoothing data

wtvec = 1./hgtmvar;
wtvec = wtvec./mean(wtvec);

%  set up new smooth of the data using this weight vector

Lfdobj   = int2Lfd(4);
lambda   = 1e-2;
hgtfdpar = fdPar(hgtbasis, Lfdobj, lambda);

%  smooth the data again

[hgtmfd, df, gcv, coef, SSE, penmat, y2cMap] = ...
    smooth_basis(age, hgtmmat, hgtfdpar, wtvec);

%  display the results

growthdisplay(age, hgtmmat, hgtmfd, hgtmstd, y2cMap, 'male')

%  Females

hgtffit      = eval_fd(age, hgtffd);
hgtfres      = hgtfmat - hgtffit;   %  residuals
hgtfresmnsqr = mean(hgtfres.^2,2);  %  mean squared residuals

%  positively smooth the mean squared residuals

Wfd = smooth_pos(age, hgtfresmnsqr, hgtfdPar);

%  compute the variance and standard error of measurements

hgtfvar = eval_pos(age, Wfd);
hgtfstd = sqrt(hgtfvar);

subplot(1,1,1)
plot(age, hgtfresmnsqr, 'o', age, hgtfvar, 'b-')
plot(age, sqrt(hgtfresmnsqr), 'o', age, hgtfstd, 'b-')

plot(age, hgtfresmnsqr, 'ko', age, hgtfvar, 'k-')
xlabel('\fontsize{16} Age')
ylabel('\fontsize{16} Variance of Measurement')

print -dps2 'c:/MyFiles/fdabook1/figs.dir/growthvariance.ps'

%  update weight vector for smoothing data

wtvec = 1./hgtfvar;
wtvec = wtvec./mean(wtvec);

%  set up new smooth of the data using this weight vector

Lfdobj   = int2Lfd(4);
lambda   = 1e-1;
hgtfdPar = fdPar(hgtbasis, Lfdobj, lambda);

%  smooth the data again

[hgtffd, df, gcv, coef, SSE, penmat, y2cMap] = ...
    smooth_basis(age, hgtfmat, hgtfdPar, wtvec);

accffd = deriv(hgtffd,2);
accmat = eval_fd(agefine, accffd);
accmn  = mean(accmat(:,1:10),2);

plot(agefine, accmat(:,1:10), '-', [1,18], [0,0], 'r:')
lhdl = line(agefine, accmn);
set(lhdl, 'LineWidth', 2, 'LineStyle', '--', 'color', 'b')
xlabel('\fontsize{19} Age')
ylabel('\fontsize{19} Height Acceleration(cm/year/year)')
axis([1,18,-4,2])


%  display the results

growthdisplay(age, hgtfmat, hgtffd, hgtfstd, y2cMap, 'female')

%  ----------------------------------------------------------
%          Compute monotone smooths of the data  
%  ----------------------------------------------------------

%  These analyses use a function written entirely in S-PLUS called
%  smooth.monotone that fits the data with a function of the form
%                   f(x) = b_0 + b_1 D^{-1} exp W(x)
%     where  W  is a function defined over the same range as X,
%                 W + ln b_1 = log Df and w = D W = D^2f/Df.
%  The constant term b_0 in turn can be a linear combinations of covariates:
%                         b_0 = zmat * c.
%  The fitting criterion is penalized mean squared error:
%    PENSSE(lambda) = \sum [y_i - f(x_i)]^2 +
%                     \lambda * \int [L W(x)]^2 dx
%  where L is a linear differential operator defined in argument LFD.
%  The function W(x) is expanded by the basis in functional data object
%  Because the fit must be calculated iteratively, and because S-PLUS
%  is so slow with loopy calculations, these fits are VERY slow.  But
%  they are best quality fits that I and my colleagues, notably
%  R. D. Bock, have been able to achieve to date.
%  The Matlab version of this function is much faster.

%  ------  First set up a basis for monotone smooth   --------
%  We use b-spline basis functions of order 6
%  Knots are positioned at the ages of observation.

norder = 6;
nbasis = nage + norder - 2;
wbasis = create_bspline_basis(rng, nbasis, norder, knots);

%  starting values for coefficient

cvec0 = zeros(nbasis,1);
Wfd0  = fd(cvec0, wbasis);

Lfd      = int2Lfd(3);  %  penalize curvature of velocity
lambda   = 10^(-1.5);   %  smoothing parameter
hgtfdPar = fdPar(Wfd0, Lfd, lambda);

% -----------------  Male data  --------------------

cvecm = zeros(nbasis, ncasem);
betam = zeros(2,      ncasem);
RMSEm = zeros(1,      ncasem);

index = 1:ncasem;

for icase=index
  hgt = hgtmmat(:,icase);
  [Wfd, beta, Fstr, iternum, iterhist] = ...
             smooth_monotone(age, hgt, hgtfdPar);
  cvecm(:,icase) = getcoef(Wfd);
  betam(:,icase) = beta;
  hgthat = beta(1) + beta(2).*monfn(age, Wfd);
  RMSEm(icase) = sqrt(mean((hgt - hgthat).^2));
  fprintf('\n%5.f %10.4f\n', [icase, RMSEm(icase)])
end

% -----------------  Female data  --------------------

cvecf = zeros(nbasis, ncasef);
betaf = zeros(2,      ncasef);
RMSEf = zeros(1,      ncasef);
resf  = zeros(nage,   ncasef);

index = 1:ncasef;

for icase=index
  hgt = hgtfmat(:,icase);
  [Wfd, beta] = smooth_monotone(age, hgt, hgtfdPar);
  cvecf(:,icase) = getcoef(Wfd);
  betaf(:,icase) = beta;
  hgthat = beta(1) + beta(2).*monfn(age, Wfd);
  resf(:,icase) = hgt - hgthat;
  RMSEf(icase) = sqrt(mean((hgt - hgthat).^2));
  fprintf('\n%5.f %10.4f\n', [icase, RMSEf(icase)])
end

%  histograms of residuals

for icase=1:ncasef
    hist(resf(:,icase))
    pause
end

resfvec = reshape(resf, ncasef*nage,1);
hist(resfvec)

resftrim = resf;
for icase=1:ncasef
    index = resf(:,icase) < -1;
    resftrim(index,icase) = -1;  
    index = resf(:,icase) >  1;
    resftrim(index,icase) =  1;  
end

nWbasis = 13;
Wbasis  = create_bspline_basis([-1, 1], nWbasis);
Wfd0    = fd(zeros(nWbasis,1), Wbasis);

Lfdobj = int2Lfd(2);
lambda = 1e-3;
WfdPar = fdPar(Wfd0, Lfdobj, lambda);

[Wfdobj, C] =  density_fd(resfvec, WfdPar);

resfine  = linspace(-1,1,51);
densfine = eval_pos(resfine, Wfdobj)./C;

plot(resfine, densfine)

denssave = zeros(51,nage);
for iage=1:nage
    [Wfdi, C] = density_fd(resftrim(iage,:)', WfdPar, 1e-4, 20, 0);
    denssave(:,iage) = eval_pos(resfine, Wfdi)./C;
end
 
contour(age, resfine, denssave)

save growthdensity

%  plot data and smooth, residuals, velocity, and acceleration

%  Males:

index = 1:ncasem;
for i = index
  Wfd  = fd(cvecm(:,i),wbasis);
  beta = betam(:,i);
  hgtmhat   = beta(1) + beta(2).*monfn(age, Wfd);
  Dhgtmhat  = beta(2).*eval_mon(age, Wfd, 1);
  D2hgtmhat = beta(2).*eval_mon(age, Wfd, 2);
  subplot(2,2,1)
  plot(age, hgtmmat(:,i), 'go', age, hgtmhat, '-')
  axis([1, 18, 60, 200]);
  xlabel('Years');  title(['Height for male ',num2str(i)])
  resi = hgtmmat(:,i) - hgtmhat;
  subplot(2,2,2)
  plot(age, resi, '-o',     [1,18], [0,0], 'r--')
  axis([1,18,-1,1]);
  xlabel('Years');  title('Residuals')
  subplot(2,2,3)
  plot(age, Dhgtmhat, '-',  [1,18], [0,0], 'r--')
  axis([1,18,0,15]);
  xlabel('Years');  title('Velocity')
  subplot(2,2,4)
  plot(age, D2hgtmhat, '-')
  axis([1,18,-6,6]);
  xlabel('Years') ;  title('Acceleration')
  pause;
end

% Females:

index = 1:ncasef;
for i = index
  Wfd  = fd(cvecf(:,i),wbasis);
  beta = betaf(:,i);
  hgtfhat   = beta(1) + beta(2).*monfn(age, Wfd);
  Dhgtfhat  = beta(2).*eval_mon(age, Wfd, 1);
  D2hgtfhat = beta(2).*eval_mon(age, Wfd, 2);
  subplot(2,2,1)
  plot(age, hgtfmat(:,i), 'go', age, hgtfhat, '-')
  axis([1, 18, 60, 200]);
  xlabel('Years');  title(['Height for female ',num2str(i)])
  resi = hgtfmat(:,i) - hgtfhat;
  subplot(2,2,2)
  plot(age, resi, '-o',     [1,18], [0,0], 'r--')
  axis([1,18,-1,1]);
  xlabel('Years');  title('Residuals')
  subplot(2,2,3)
  plot(age, Dhgtfhat, '-',  [1,18], [0,0], 'r--')
  axis([1,18,0,15]);
  xlabel('Years');  title('Velocity')
  subplot(2,2,4)
  plot(age, D2hgtfhat, '-')
  axis([1,18,-6,6]);
  xlabel('Years') ;  title('Acceleration')
  pause;
end

%  ---------------------------------------------------------------------
%            Register the velocity curves for the girls
%  ---------------------------------------------------------------------

nbasisw = 15;
norder  = 5;
basisw  = create_bspline_basis([1,18], nbasisw, norder);

index = 1:ncasef;

agefine = linspace(1, 18, 101)';
agemat  = agefine * ones(1,length(index));

y0fd = deriv(mean(hgtffd), 1);
yfd  = deriv(hgtffd(index), 1);

y0vec = eval_fd(y0fd, agefine);
yvec  = eval_fd(yfd,  agefine);

coef0 = zeros(nbasisw,length(index));
Wfd0  = fd(coef0, basisw);

Lfdobj = int2Lfd(2);
lambda = 1;
WfdPar = fdPar(Wfd0, Lfdobj, lambda);

[yregfd, Wfd] = registerfd(y0fd, yfd, WfdPar);

yregmat = eval_fd(yregfd,agefine);

warpmat = monfn(agefine, Wfd);
warpmat = 1 + 17.*warpmat./(ones(101,1)*warpmat(101,:));

for i = 1:length(index)
   subplot(1,2,1)
   plot(agefine, yvec(:,i), '-', agefine, y0vec, '--', agefine, yregmat(:,i), '-');
   axis('square')
   title(['Case ',num2str(i)])
   subplot(1,2,2)
   plot(agefine, warpmat(:,i), '-', agefine, agefine, '--')
   axis('square')
   pause
end

%  ---------------------------------------------------------------------
%        Monotone smooth of short term height measurements
%  ---------------------------------------------------------------------

%  ---------------- input the data  ----------------------------------

clear;
fid  = fopen('onechild.dat','rt');
temp = fscanf(fid,'%f');
n    = 83;
data = reshape(temp, [n, 2]);
day  = data(:,1);
hgt  = data(:,2);
rng  = [day(1), day(n)];
wgt  = ones(n,1);
zmat = wgt;

%  set up the basis

nbasis   = 43;
norder   = 4;
hgtbasis = create_bspline_basis(rng, nbasis, norder);

%  set parameters for the monotone smooth

Lfd      = int2Lfd(2);
lambda   = 1;
hgtfdPar = fdPar(hgtbasis, Lfd, lambda);

%  carry out the monotone smooth

[Wfd, beta, Fstr, iternum, iterhist] = ...
    smooth_monotone(day, hgt, hgtfdPar);

%  plot the function W = log Dh

subplot(1,1,1)
plot(Wfd);

%  plot the data plus smooth

dayfine  = linspace(day(1),day(n),151)';
yhat     = beta(1) + beta(2).*eval_mon(day, Wfd);
yhatfine = beta(1) + beta(2).*eval_mon(dayfine, Wfd);
phdl = plot(day, hgt, 'o', dayfine, yhatfine, 'b-');
set(phdl, 'LineWidth', 2)
xlabel('\fontsize{19} Day')
ylabel('\fontsize{19} Height (cm)')
axis([0,312,123,131])

%  plot growth velocity

Dhgt = beta(2).*eval_mon(dayfine, Wfd, 1);
phdl = plot(dayfine, Dhgt);
set(phdl, 'LineWidth', 2)
xlabel('\fontsize{19} Days')
ylabel('\fontsize{19} Velocity (cm/day)')
axis([0,312,0,.06])

%  plot growth acceleration

D2hgt = beta(2).*eval_mon(dayfine, Wfd, 2);
phdl = plot(dayfine, D2hgt, [0,312], [0,0], 'r:');
set(phdl, 'LineWidth', 2)
xlabel('\fontsize{19} Days')
ylabel('\fontsize{19} Velocity (cm/day/day)')
axis([0,312,-.003,.004])
