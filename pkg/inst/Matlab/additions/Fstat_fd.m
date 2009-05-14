function [F,argvals] = Fstat_fd(y,yhat,argvals)

    if  isnumeric(yhat), yhat = reshape(yhatnumel(yhat),1); end

    if (isnumeric(y) & ~isnumeric(yhat)) | (isa_fd(y) & ~isa_fd(yhat)) 
        error('y and yhat must both be either scalars or functional data objects.');
    end


    if isa_fd(y)
        rangeobs = getbasisrange(getbasis(y));
        rangehat = getbasisrange(getbasis(yhat));

        if any(rangeobs ~= rangehat)
            error('y and yhat do not have the same range');
        end


        if nargin < 3
            argvals = linspace(rangeobs(1),rangeobs(2),101);
        end

        yvec = eval_fd(argvals,y);
        yhatvec = eval_fd(argvals,yhat);

        F = var(yhatvec,[],2)./mean( (yvec-yhatvec).^2,2);
    else
        yvec = y;
        yhatvec = yhat;

        F = var(yhatvec)./mean( (yvec-yhatvec).^2 );
    end