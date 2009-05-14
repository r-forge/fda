function [pval,qval,Fobs,Fnull,Fvals,Fnullvals,pvals_pts,qvals_pts,fregresscell,argvals] = ...
    Fperm_fd(yfdPar, xfdlist, betalist,wt,nperm,argvals,q,plotres)

   if nargin<8, plotres = 1; end
   if nargin < 7, q = 0.05; end
   if nargin < 5, nperm = 200; end
    
  Fnull = zeros(nperm,1);
  Fnullvals = [];

  q = 1-q;

%%  begin = proc.time()
  fregresscell = getfd(fRegress(yfdPar, xfdlist, betalist));
%%  elapsed.time = max(proc.time()-begin,na.rm=TRUE)

%%  if( elapsed.time > 30/nperm ){
%%    print(paste('Estimated Computing time =',
%%                round(nperm*elapsed.time),'seconds.'))
%%  }

    yhat = fregresscell.yhatfdobj;

%%  if isa_fdPar(fregresscell.yfdPar), yfdPar = getfd(fregresscell.fdPar); 
%%  else yfdPar = fregresscell.yfdPar

  [Fvals,argvals] = Fstat_fd(yfdPar,yhat,argvals);

  Fobs = max(Fvals);

  if isnumeric(yfdPar), n = length(yfdPar);
  else
      tempc = getcoefs(yfdPar);
      n = size(tempc,2); 
  end

  for i = 1:nperm

    tyfdPar = yfdPar(randperm(n));

    fregresscell = fRegress(tyfdPar, xfdlist, betalist);
    yhat = fregresscell.yhatfdobj;
    
    Fnullvals = [Fnullvals,Fstat_fd(yfdPar,yhat,argvals)];

    Fnull(i) = max(Fnullvals(:,i));
  end


    pval = mean( Fobs < Fnull );
    qval = quantile(Fnull,q);

    pvals_pts = mean(repmat(Fvals,1,nperm)<Fnullvals,2);
    qvals_pts = quantile(Fnullvals,q,2);

    if plotres 
        if isa_fd(yfdPar),
            fdnames = getnames(yfdPar);
            
            plot(argvals,Fvals,'r','linewidth',2);
			xlabel(fdnames(3));
            ylabel('F-statistic');
            title('Permutation F-Test');
            hold on
            plot(argvals,qvals_pts,'b-.','linewidth',2);
            lines(rangeobs,qval*ones(1,2),'b-','linewidth',2);
            lengend('Observed Statistic',stracat('pointwise ',1-q,' critical value'),starcat('maximum ',1-q,' critical value'));
        else
            N = hist(Fnull);
            xlabel('F-value')
			title('Permutation F-Test')
            hold on
            plot(Fobs*ones(1,2),[0 N],'r-')
            plot(qval*ones(1,2),'b-')
            legend('Observed Statistic',strcat('Permutation ',1-q,' critical value'))

        end
    end
