zeroAndOne = rand(1000,1);
SLE = norminv(zeroAndOne,142,0.994);
figure(1)
figure1 = histogram(SLE,'Normalization','probability')
xlabel('Expected cost lost / incident (USD)') 
ylabel('Probability')

zeroAndOne = rand(1000,1);
ARO = norminv(zeroAndOne,480123,48012.3);
figure(2)
figure2 = histogram(ARO,'Normalization','probability')
xlabel('Estimated frequency of attacks / year') 
ylabel('Probability')

ALE = ARO.*SLE
figure(3)
figure3 = histogram(ALE,'Normalization','probability')
xlabel('Expected loss to society (USD)') 
ylabel('Probability')

ROSI = (ALE(:,1)*1-108800000)/108800000
figure(4)
figure4 = histogram(ROSI,'Normalization','probability')
xlabel('Expected return of investment (ROSI)') 
ylabel('Probability')
