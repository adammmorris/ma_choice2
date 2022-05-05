function [] = graphCV(cv, which_models)
cv1 = cv(:,which_models(1));
cv2 = cv(:,which_models(2));
d = cv1 - cv2;
m = mean(d);
se = std(d) / sqrt(size(cv,1));

modelNames = {'WAD', 'WP', 'EW', 'TAL', 'LEX'};

figure
errorbar(m,se,'ok','MarkerFaceColor','k','MarkerSize',12,'LineWidth',4);
set(gca,'YLim',[min(d) max(d)+1],'XLim',[0.5 1.5],'XTick',1,'XTickLabel',{[modelNames{which_models(1)} ' vs ' modelNames{which_models(2)}]},'FontSize',25);
ylabel('Relative log predictive prob.','FontSize',25);
hold on; plot([0.5 1.5],[0 0],'--r','LineWidth',3); % red line shows chance performance
title('Cross-validation','FontSize',25);

figure
scatter(cv1, cv2);
hold on;
plot([min(cv1), max(cv1)],[min(cv1) max(cv1)],'--r','LineWidth',3);
xlabel([modelNames{which_models(1)} ': Log predictive prob.'],'FontSize',18);
ylabel([modelNames{which_models(2)} ': Log predictive prob.'],'FontSize',18);
title('Cross-validation (all subj)','FontSize',25);
hold off
end