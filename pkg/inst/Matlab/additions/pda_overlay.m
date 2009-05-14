function [] = pda_overlay(bfdcell,nfine,ncoarse)

if nargin < 3, ncoarse = 11; end
if nargin < 2, nfine = 501; end

fdrange = getbasisrange(getbasis(getfd(bfdcell{1})));

tfine = linspace(fdrange(1),fdrange(2),nfine);
beta0vals = eval_fd(tfine,getfd(bfdcell{1}));
beta1vals = eval_fd(tfine,getfd(bfdcell{2}));

beta0range = [min(beta0vals),max(beta0vals)];
beta1range = [min(beta1vals),max(beta1vals)];

plot(beta1vals,beta0vals,'b','linewidth',2)
xlabel('beta 1')
ylabel('beta 0')
hold on
plot([0 0],beta0range,'r')
plot(beta1range,[0 0],'r')

bv = linspace(beta1range(1),beta1range(2),nfine);
which = (bv/2).^2 < beta0range(2);
plot(bv(which),(bv(which)/2).^2,'r')

tcoarse = linspace(fdrange(1),fdrange(2),ncoarse);
beta0valsc = eval_fd(tcoarse,getfd(bfdcell{1}));
beta1valsc = eval_fd(tcoarse,getfd(bfdcell{2}));

text(beta1valsc,beta0valsc,num2str(tcoarse'))
hold off
