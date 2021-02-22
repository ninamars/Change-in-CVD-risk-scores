# Change in risk score as a predictor of future cardiovascular disease: analyses of resurvey data from the British Whitehall II prospective cohort study

These are the instructions for the Online tool presented in publication:
Change in risk score as a predictor of future cardiovascular disease: analyses of resurvey data from the British Whitehall II prospective cohort study, Lindbohm et al. 2021.


## Using the calculator


### Link to calculator

https://0123095223939283891000129wf3.shinyapps.io/CVD_change_calculator_2021_02_19/

Built with [Shiny](https://shiny.rstudio.com/)


### The main steps when using the tool are:

1.	Choose “analysis of risk history” or “analysis of targeted change” tab
2.	Insert risk factor values
3.	Choose interventions chosen (only for analysis of targeted change)
4.	Press calculate
5.	Interpret the results (Note, changes in the risk factors from interventions should last at least 5 years)


### Examples

The online tool is an interactive calculator to estimate cardiovascular disease-free life-years gained or lost based on preceding, current, and future risk factor measurements for SCORE or ASCVD risk scores at each age between 40 and 75. The preceding measurements (analysis of risk history) show how changes in risk factors between two health checks affect the risk score when compared to ageing only. The future measurements (analysis of targeted change) facilitate planning of targeted risk factor levels by quantifying the potential effects of future lifestyle change and medical interventions on risk factor levels.

As an example of analysis of risk history consider a male smoker aged 55, with systolic blood pressure at 140mmHg, total cholesterol at 6mmol/l and HDL-C at 1mmol/l at his current measurement. When combined to data from earlier measurement when the participant was age 50, with systolic blood pressure at 130mmHg, total cholesterol at 5mmol/l and HDL-C at 1mmol/l, we can observe that his 10-year risk for cardiovascular diseases measured with SCORE has progressed from 1.8% to 4.3% over the 5-year period. The progression is faster than would have been observed with only changing age (risk with age only changing is 3%), and a loss of 1.3 diseases-free years is estimated. Risk of 4.3% would place the individual to moderate risk category where current ESC guidelines recommend considering medical intervention. Additional information on faster rate of risk progression could be considered as risk enhancing factor and support initiation of preventive medication.

The second example illustrate how the calculator can be used to facilitate planning of targeted risk factor levels by quantifying the potential effects of future lifestyle change and medical interventions on risk factor levels. Consider a 50 years old non-smoker male, with systolic blood pressure at 145mmHg, total cholesterol at 6mmol/l and HDL-C at 1mmol/l. Measured with ASCVD score the participant has 6.9% 10-year risk of cardiovascular diseases, which indicates consideration of preventive medication. However, the participant prefers lifestyle change to medication and decides to start DASH-diet, three sessions of aerobic endurance per week, and weight management to lose 5kg during 1 year. This would translate to:
| Intervention | Change in systolic blood pressure (mmHg) by baseline blood pressure | Change in total cholesterol (mmol/l) by baseline cholesterol concentration |
| ------------ | ------------------------------------------------------------------- |--------------------------------------------------------------------------- |
| Start DASH diet| -11.4 | -0.2 |
| Aerobic endurance training (3 sessions per week) | -8.3 | -0.1 |
| A 5-kilogram reduction in weight | -5.5 | -0.5 |
| Total | -25.2 | -0.8 |

These interventions, if successful, would prolong diseases free life by 0.7 years and lead to a 4.5% risk in the follow-up visit next year, or a 6.3% risk in the next five years after taking into account ageing. As such, the targeted lifestyle change would be sufficient to postpone preventive medication. 


### 10-year risk of cardiovascular diseases

We chose to use SCORE and ASCVD risk score because they are widely used risk scores in primary prevention. They provide 10-year risk for cardiovascular diseases and their characteristics are described here: 

|  | ASCVD | SCORE |
| -------------- |-------------- | -------------- |
| Participants	| 4 pooled prospective studies	| 12 prospective studies
| Time period	| 1968-1993	| 1972-1991|
| Population	| Black and white Americans |	Europeans from 11 countries
| Sample size	| 205178 (57% men) |	24626 (48% men) |
| Baseline age range	| 20-79	| 40-65|
| Variables included	| Age, sex, race (white or other/African American), total cholesterol, HDL-C, SBP, antihypertensive treatment, DM, smoking + interaction terms when appropriate|	Sex, age, total cholesterol, SBP, smoking status. |
| Outcomes	| Non-fatal myocardial infarction or coronary heart disease death or fatal or nonfatal stroke	|  Fatal following diseases: hypertension, hypertensive heart disease, hypertensive chronic kidney disease, hypertensive heart disease, acute venous embolism or thrombosis of deep vessels of lower extremity, myocardial infarction, angina pectoris, ischemic heart disease, conduction disorders, cardiac dysrhythmias, heart failure, intracranial haemorrhage, ischemic stroke, transient cerebral ischemia, atherosclerosis, aortic aneurysm and dissection, peripheral vascular disease, death within 24 h of symptom onset  (71% of all events were coronary heart diseases) |



### Cardiovascular disease-free life-years

Cardiovascular disease-free life-years for both SCORE and ASCVD were estimated as years free of stroke (ICD-10 codes under I60, I61, I63, and I64; ICD-9 codes 430, 431, 434, and 436), myocardial infarction (ICD-10 codes under I21; ICD-9 codes under 410), heart failure (ICD10-codes under I50), definite angina (ICD-10 codes under I20; ICD-9 codes under 410, both verified from medical records), peripheral artery disease (ICD-10 codes I70·2, I73·3, I73·9, I74·3-5, E10·5, E11·5, E12·5, E13·5, E14·5; ICD-9 codes 250·7, 440·2, 440·4, 443·8-9, 444·2, 444·81), coronary artery bypass graft, or percutaneous coronary intervention. We chose to use a broad combination of incident non-fatal and fatal cardiovascular diseases  as our primary outcome for cardiovascular disease-free life-years because knowledge of preventing any of these diseases may be the preferred optio.



## References 


#### The original SCORE and ASCVD publications are:
* Conroy RM, Pyorala K, Fitzgerald AP, et al. Estimation of ten-year risk of fatal cardiovascular disease in Europe: the SCORE project. Eur Heart J 2003; 24(11): 987-1003. doi:S0195668X03001143 [pii].
* Goff DC,Jr, Lloyd-Jones DM, Bennett G, et al. 2013 ACC/AHA guideline on the assessment of cardiovascular risk: a report of the American College of Cardiology/American Heart Association Task Force on Practice Guidelines. Circulation 2014; 129(25 Suppl 2): S49-73. doi:10.1161/01.cir.0000437741.48606.98 [doi].


#### The intervention effects were acquired from:
* Eckel RH, Jakicic JM, Ard JD, et al. 2013 AHA/ACC guideline on lifestyle management to reduce cardiovascular risk: a report of the American College of Cardiology/American Heart Association Task Force on Practice Guidelines. J Am Coll Cardiol 2014; 63(25 Pt B): 2960-84. doi:10.1016/j.jacc.2013.11.003 [doi].
* Arnett DK, Blumenthal RS, Albert MA, et al. 2019 ACC/AHA Guideline on the Primary Prevention of Cardiovascular Disease: A Report of the American College of Cardiology/American Heart Association Task Force on Clinical Practice Guidelines. Circulation 2019; 140(11): e596-646. doi:10.1161/CIR.0000000000000678 [doi].
* Mach F, Baigent C, Catapano AL, et al. 2019 ESC/EAS Guidelines for the management of dyslipidaemias: lipid modification to reduce cardiovascular risk. Eur Heart J 2020; 41(1): 111-88. doi:10.1093/eurheartj/ehz455 [doi].
* He FJ, Li J, Macgregor GA. Effect of longer term modest salt reduction on blood pressure: Cochrane systematic review and meta-analysis of randomised trials. BMJ 2013; 346: f1325. doi:10.1136/bmj.f1325 [doi].
* Appel LJ, Moore TJ, Obarzanek E, et al. A clinical trial of the effects of dietary patterns on blood pressure. DASH Collaborative Research Group. N Engl J Med 1997; 336(16): 1117-24. doi:10.1056/NEJM199704173361601 [doi].
* Aburto NJ, Hanson S, Gutierrez H, Hooper L, Elliott P, Cappuccio FP. Effect of increased potassium intake on cardiovascular risk factors and disease: systematic review and meta-analyses. BMJ 2013; 346: f1378. doi:10.1136/bmj.f1378 [doi].
* Roerecke M, Kaczorowski J, Tobe SW, Gmel G, Hasan OSM, Rehm J. The effect of a reduction in alcohol consumption on blood pressure: a systematic review and meta-analysis. Lancet Public Health 2017; 2(2): e108-20. doi:S2468-2667(17)30003-8 [pii].
* Domenech M, Roman P, Lapetra J, et al. Mediterranean diet reduces 24-hour ambulatory blood pressure, blood glucose, and lipids: one-year randomized, clinical trial. Hypertension 2014; 64(1): 69-76. doi:10.1161/HYPERTENSIONAHA.113.03353 [doi].
* Hollaender PL, Ross AB, Kristensen M. Whole-grain and blood lipid changes in apparently healthy adults: a systematic review and meta-analysis of randomized controlled studies. Am J Clin Nutr 2015; 102(3): 556-72. doi:10.3945/ajcn.115.109165 [doi].
* Berger S, Raman G, Vishwanathan R, Jacques PF, Johnson EJ. Dietary cholesterol and cardiovascular disease: a systematic review and meta-analysis. Am J Clin Nutr 2015; 102(2): 276-94. doi:10.3945/ajcn.114.100305 [doi].
* Gylling H, Plat J, Turley S, et al. Plant sterols and plant stanols in the management of dyslipidaemia and prevention of cardiovascular disease. Atherosclerosis 2014; 232(2): 346-60. doi:10.1016/j.atherosclerosis.2013.11.043 [doi].
* Mozaffarian D, Micha R, Wallace S. Effects on coronary heart disease of increasing polyunsaturated fat in place of saturated fat: a systematic review and meta-analysis of randomized controlled trials. PLoS Med 2010; 7(3): e1000252. doi:10.1371/journal.pmed.1000252 [doi].
* Li Y, Jiang L, Jia Z, Xin W, Yang S, Yang Q, Wang L. A meta-analysis of red yeast rice: an effective and relatively safe alternative approach for dyslipidemia. PLoS One 2014; 9(6): e98611. doi:10.1371/journal.pone.0098611 [doi].
* Patnode CD, Evans CV, Senger CA, Redmond N, Lin JS. Behavioral Counseling to Promote a Healthful Diet and Physical Activity for Cardiovascular Disease Prevention in Adults Without Known Cardiovascular Disease Risk Factors: Updated Evidence Report and Systematic Review for the US Preventive Services Task Force. JAMA 2017; 318(2): 175-93. doi:10.1001/jama.2017.3303 [doi].
* Kelley GA, Kelley KS. Impact of progressive resistance training on lipids and lipoproteins in adults: a meta-analysis of randomized controlled trials. Prev Med 2009; 48(1): 9-19. doi:10.1016/j.ypmed.2008.10.010 [doi].
* Cornelissen VA, Fagard RH, Coeckelberghs E, Vanhees L. Impact of resistance training on blood pressure and other cardiovascular risk factors: a meta-analysis of randomized, controlled trials. Hypertension 2011; 58(5): 950-8. doi:10.1161/HYPERTENSIONAHA.111.177071 [doi].
* Cornelissen VA, Fagard RH. Effects of endurance training on blood pressure, blood pressure-regulating mechanisms, and cardiovascular risk factors. Hypertension 2005; 46(4): 667-75. doi:01.HYP.0000184225.05629.51 [pii].
* Cornelissen VA, Smart NA. Exercise training for blood pressure: a systematic review and meta-analysis. J Am Heart Assoc 2013; 2(1): e004473. doi:10.1161/JAHA.112.004473 [doi].
* Zomer E, Gurusamy K, Leach R, et al. Interventions that cause weight loss and the impact on cardiovascular risk factors: a systematic review and meta-analysis. Obes Rev 2016; 17(10): 1001-11. doi:10.1111/obr.12433 [doi].
* Williams B, Mancia G, Spiering W, et al. 2018 ESC/ESH Guidelines for the management of arterial hypertension. Eur Heart J 2018; 39(33): 3021-104. doi:10.1093/eurheartj/ehy339 [doi].


## Authors

**Nina Mars**

**Joni Lindbohm**


