function [tfine,eigvals,limvals] = eigen_pda(bwtlist,awtlist,ufdlist,plotresult,npts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eigen_pda
%
% A graphical stability analysis of the result of a principal differential
% analysis. Computes the instantaneous Eigenvalues and stable points of a
% time-varying linear differential equation. 
%
% Arguments:
%
%   - bwtlist, awtlist: lists defining the alpha and beta functions as
%   returned by pda_cell. 
%
%   - ufdlist: list of forcing functions as given to pda_cell.
%
%   - plotresult: should the stability analysis be plotted? Defaults to
%   true. 
%
%   - npts: number of time points at which to evaluate the stability
%   analysis. Defaults to 501. 
%
% Returns:
%
%   If plotres is non-zero a graph is produced giving the real and
%   imaginary eigenvalues of the system at each of npts time points.
%   Positive real values indicate a system undergoing exponential growth,
%   negative real values indicate decay to a fixed point. Non-zero
%   imaginary values indicate a system that is oscillating. 
%
%   If ufdcell is not empty a third pane is created giving the stable
%   points of the system at each time. 
%
%   In addition, it returns
%
%   - tfine: time points at which the stability analysis is plotted
%
%   - eigvals: a matrix of eigenvalues; rows correspond to tfine
%
%   - limvals: a matrix of stable points of the system, rows correspond to
%   tfine. 

% Last modified May 15, 2009

if nargin < 3, npts = 501; end
if nargin < 2, plotresult = 1; end

rangval = getbasisrange(getbasis(pdaList.resfdcell{1}));

m = length(pdaList.resfdcell);
tfine = linspace(rangval(1),rangval(2),npts);

if m == 1
    d = length(bwtlist);
    xlabstr = getnames(bwtlist{1});
    xlabstr = xlabstr(1);

    betamat = zeros([npts,d,d]);

    for i = 1:d
        betamat(:,1,d-i+1) = -eval_fd(tfine,getfd(bwtlist{i}));
        if i < d, betamat(:,i+1,i) = 1; end
    end

    if ~isempty(awtlist)
        umat = matrix(0,npts,d);
        for i = 1:length(awtlist)
            umat(:,1) = umat(:,1) + eval_fd(tfine,getfd(awtlist{i})).*eval_fd(tfine,ufdlist{i});
        end
    end
else
    d = length(bwtlist{1,1});
    xlabstr = getnames(bwtlist{1,1,1});
    xlabstr = xlabstr(1);

    betamat = zeros(npts,m*d,m*d);

    for k = 1:d
        for j = 1:m
            for i = 1:m
                if ~isempty(bwtlist{i,j,k})
                    betamat(:,j,m*(d-k)+i) = -eval_fd(tfine,getfd(bwtlist{i,j,k}));
                end
            end
            if k < d
                betamat(:,m*k+j,m*(k-1)+j) = 1;
            end
        end
    end

    if ~isempty(awtlist)
        umat = zeros(npts,m*d);
        for k = 1:d
            for i = 1:length(awtlist{k})
                if ~isempty(awtlist{k,i})
                    umat(:,k) = umat(:,k) + eval_fd(tfine,getfd(awtlist{k,i}))*eval.fd(tfine,getfd(ufdlist{k,i}));
                end
            end
        end
    end
end

eigvals = zeros(npts,m*d);
limvals = zeros(npts,m*d);

for i = 1:npts
    eigvals(i,:) = diag(eig(betamat(i,:,:)));
    if ~isempty(awtlist)
        limvals(i,:) = squeeze(betamat(i,:,:))\umat(i,:);
    end
end

if plotresult
    if ~isempty(awtlist), subplot(3,1,1);
    else subplot(2,1,1); end
    plot(tfine,Real(eigvals))
    xlabel(xlabstr)
    ylabel('Real')
    title('Eigenvalues')
    hold on
    plot(rangeval,[0 0])
    hold off

    if ~isempty(awtlist), subplot(3,2,2);
    else subplot(2,1,2); end
    plot(tfine,imag(eigvals))
    xlabel(xlabstr)
    ylabel('Imaginary')
    hold on
    plot(rangeval,[0 0])
    hold off

    if ~is.null(awtlist)
        subplot(3,1,3)
        plot(tfine,limvals(:,1:d))
        xlabel(xlabstr)
        ylabel('Fixed Point')
        title('Instantaneous Limits')
    end
end
