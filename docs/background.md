---
title: Background
nav_order: 2
---

# Background
{: .no_toc }

Menopause is a part of the aging process that most women go through. It 
can be broken down into three stages:
- **Perimenopause** (menopausal transition): Reproductive hormones change 
irregularly, menstrual periods become more irregular, and menopausal symptoms 
start to develop; typically 3-7 years before menopause.
- **Menopause**: No menstrual period for 12 months.
- **Postmenopause**: After 12 months of no menstruation.

During this transition, many women may experience menopausal symptoms caused by 
a drop in hormone levels, specifically estrogen and progesterone. These include 
hot flashes, night sweats, vaginal dryness, mood changes, sleeping problems, 
memory problems, and more. 

To alleviate these symptoms, doctors can prescribe menopausal hormone therapies (MHTs)—otherwise known as hormone replacement therapies (HRTs)—to replace the drop in hormones, but a 2002 women’s health study (widely known 
as the Women's Health Initiative (WHI) study) found HRTs to 
be associated with an increased risk of breast cancer, cardiovascular disease, 
and stroke, making HRTs a bit controversial. In November 2025, the FDA (under Marty Makary) announced that they were beginning the process of changing the black box warnings on HRTs, specifically "removing risk statements about cardiovascular diseases, breast cancer, and probably dementia" after an assessment of relevant literature. It has since been approved (February 2026, see [press release](http://www.fda.gov/news-events/press-announcements/fda-approves-labeling-changes-menopausal-hormone-therapy-products)). 

Prior to the WHI study results, HRTs were also used to prevent osteoporosis due 
to the positive effect of estrogen on bone mineral density (cite paper). Other 
observational studies have found a ...

Given the limitations of the data available on menopausal women, we cannot really 
investigate the effects of HRTs on certain health outcomes that the WHI study 
examined such as breast cancer, cardiovascular disease, and neurodegenerative diseases. Furthermore, we don't have access to the WHI data. So, in this project we aim to quantify the effect of HRTs on BMD in menopausal women using publicly available data, knowing that current literature and science supports a positive effect. 

Lastly, the WHI studied outcomes that were mainly time-to-event, and thus conducted survival analysis and reported hazard ratios. Our project is not a survival analysis project, and we report ATE-like estimates. 

# A Brief Digression on the 2002 WHI Study
Timeline of relevant events, statements, and papers (many papers have reviewed the 2002 WHI study):
- July 9, 2002: the National Heart, Lung and Blood Institute (NHLBI) of the NIH stopped early a WHI trial citing 
- April 21, 2025: DHHS announce that annual funding of the WHI Regional Center contracts will be terminated in September 2025
- May 6, 2025: 
- July 17, 2025: FDA expert panel on menopause and HRT for women
- September 15, 2025: WHI responds to FDA expert panel
- November 10, 2025: FDA requests labeling changes to MHTs
- November 12, 2025: WHI responds to FDA removal of black box warning on MHTs
- February 12, 2026: FDA approves labeling changes to MHTs

## Some Notes From the [FDA Expert Panel on Menopause and HRT for Women (July 17, 2025)](https://www.youtube.com/live/_2ZRlOivC5M?si=C41F8v8nHOyhYO8a)

Note: menopausal hormone therapy (MHT) is the preferred term in the medical field, as there are some differences between the goals of MHT and HRT. 

### Dr. Heather Hirsch
There are different types of MHTs:
- Systemic combination therapy (estrogen and progestogen)
- Systemic estrogen-alone therapy
- Systemic progestogen-alone therapy for women with a uterus using systemic estrogen
- Topical vaginal estrogen therapy
	- Placed in the vagina to treat genitourinary syndrome of menopause (GSM) which includes painful intercourse, vaginal dryness, and recurrent UTIs.
	- Does not travel systemically (does not go through the entire body) and no data showing increased risk in cardiovascular diseases, breast cancer, or stroke, yet there is a black box warning; categorically safe for all women. 

Menopausal hormone therapy (MHT): combination of an estrogen $$\pm$$ progestin or progesterone (for women with an intact uterus), can also include testosterone.

MHT is broad and poorly defined. Specifics matter:
- Route: oral or transdermal?
- Formulation: chemical structure?
- Time: at what age is it taken and wrt what stage of menopause?
- Dose: how much is taken?

HRT is a medical necessity for women who have menopause before the ages of 40-45. 

Boxed warning (formerly, black box warning) is the highest safety-related warning that can be placed on a medication by the FDA; means the medication can incur life-threatening or serious adverse effects. The fear that the black box warning from the WHI study incurred has harmed the lives of many women (50 million women aged 40-60) who are suffering from menopause symptoms. 

Clinicians need more training and education on menopause. 

### Dr. Barbara Levy
- Not all hormones are equivalent
	- WHI studied conjugated equine estrogen (CEE) and medroxyprogesterone acetate (MPA, commonly Prempro)
	- 17 beta estradiol is the estrogen naturally produced in the body; it binds to receptors differently than conjugated equine estrogen.
	- Micronized progesterone is the progesterone naturally produced in the body; it binds to receptors differently than medroxyprogesterone acetate.
- Need to stratify studies of MHTs by age, dose, formulation, and route of administration.
	- WHI study investigators were interested in: "Should we start all women at all ages on HT to prevent cardiovascular disease?", and purposefully enrolled older women to power the study (wasn't about menopausal symptoms); WHI study participants average age was 63. 
- Initiation of MHT vs continuation beyond age 60
- Preservation of function: MHTs are not primarily used to treat cardiovascular disease, Alzheimer's disease, osteoporosis, etc. but MHTs can prevent them, so study these

### Dr. JoAnn Manson
- See papers that show benefits of HT in women aged 50-59 in the WHI HT trials: [Manson JE and Kaunitz AM (2016)](https://www.nejm.org/doi/full/10.1056/NEJMp1514242), Manson JE, et al. (2013) [https://jamanetwork.com/journals/jama/fullarticle/1745676]. 

### Dr. JoAnn Pinkerton 
- Boxed warning on *vaginal estrogen dosed to treat GSM* is harming women; it reflects estrogen class labeling extrapolated from *systemic* HT in women (average age 63) and overstates risk.

### Dr. James Simon
- The FDA has failed in constructing a label that is understandable, straightforward, and carefully & correctly balances risks & benefits of local/low-dose vaginal estrogen.
	- It is not consistent with the product and the data presented on the product, and with the scientific literature.
		- After the 2002 WHI study, all estrogen products contain class warnings. These include vaginal estrogen products, which they labeled as causing/associating with increased risks of cardiovascular diseases, breast cancer, and dementia. For this to be true, the estrogen placed in the vagina must travel through the blood, to the brain, heart, etc. However, there is no evidence that estrogen placed in the vagina can be measured in the blood, see a large observational study (400,000 women in the U.S., 200,000 in Denmark) that showed no impact of vaginal estrogen to the endometrium.
	- It is not consistent and fair with other products used for the same treatment: Imvexxy vs Intrarosa. 

### Dr. Philip Sarrel
- Effects of untreated symptoms in the workplace.
- Death attributable to estrogen avoidance. 
- Estradiol has a major role in biological existence, e.g. the octopus!

### Dr. Roberta Diaz Brinton
- Estrogen action in the brain.
- 2/3 of Americans with Alzheimer's are women:
	- Alzheimer's can start earlier in women, corresponds to menopausal transition
	- During menopause, brain loses ability to use glucose (glucose metabolism), rise in beta-amyloid and loss of white matter.
	- Hot flash is generated by brain heat (chain reaction from mitochondria transitioning from efficient to inefficient heat generators).
	- MHT reduces risk of age-associated neurodegenerative diseases (Alzheimer's, Parkinson's, all cause dementia, multiple sclerosis, ALS)
	- Brain changes: decline in glucose metabolism activates a starvation response that leads to utilization of auxiliary fuels (in fact, its own white matter).
	- Initiating MHT before or at menopause is associated with a reduced risk of developing Alzheimer's; however once the brain undergoes the decline in glucose metabolism, there is increased inflammation and increased risk for Alzheimer's
	- Gap: precision menopausal hormone therapy; use precision medicine! 

### Dr. Vonda Wright
- Women are 4x more likely to develop osteoporosis and occurs 10 years earlier than men. Hip fractures and its complications are dangerous and possibly fatal. 
- Estrogen plays a key role in the rebuilding of bone. 
	- FDA has approved the use of estrogen for the prevention of post-menopausal osteoporosis; only an estimated 4% use it.
	- Estrogen is also important for prevention of osteoporosis during perimenopause. It is estimated that estrogen must be used 10 years to change the outcome of fracture. 

### Dr. Kelly Casperson
- FDA has no approved testosterone doses for women even though it is naturally occurring in the body and can help with recovery after a hip fracture (50% less likely to need a cane or walker 6 months after). We can also see differences in brain activity.  
	- Men and women both experience a drop in testosterone (hypogonadism).
	- Rejected two attempts for a female testosterone dose, citing insufficient safety data. 

### Dr. Mary Jane Minkin
- Education
	- Not enough practitioners who know about menopause. 
	- After WHI study (July 9, 2002), women threw away their estrogen products, and women stopped learning about menopause. Residency programs also stopped menopause education.
- Cancer
	- Need to address menopausal issues for cancer survivors.
	- Previvors: diagnosing women with genetic abnormalities that affect women's ovarian function (e.g. by taking out, providing therapies); women are scared to remove their ovaries because they are scared of going through menopause, even though keeping their ovaries in longer increases their risk of getting cancer.

### Dr. Rachel Rubin
- GSM & vaginal estrogen; local vaginal hormones are safe, effective, and essential.
- UTI is expensive, dangerous (fatal), long-term antibiotic use is dangerous.

### Dr. Howard Hodis
- An explanation of why the WHI study concluded that CE+MPA increased risk of breast cancer (it was due to an anomaly of decreased incidence of breast cancer in the placebo arm of the CEE+MPA trial for women with prior HT use; this falsely gave the impression that CEE+MPA for women with prior HT use was associated with breast cancer).
- Today, the conclusion from the WHI study is:
	- In women who started HT prior to age 60, all major outcomes except for DVT in the E+P did not reach the rare threshold of 1 event per 10,000 women (meaning they were statistically non significant compared to placebo). Some important outcomes include breast cancer, all cause mortality, all cancer deaths, other mortality, Alzheimer's & dementia mortality. All of these outcomes are reduced in both the CEE alone trial and the CEE+MPA trial. 
	- CEE significantly reduced breast cancer in women who were more than 80% adherent and significantly reduced breast cancer mortality after 20 year follow-up.
	- The author of the CEE+MPA 2002 results that caused the panic, stated "breast cancer almost reached nominal statistical significance" (p.327) WHICH MEANS IT WAS NOT SIGNIFICANT!
	- Since WHI was not a single-outcome trial, adjustment was conducted for breast cancer which was a priori specified as a monitoring and secondary outcome, multiple statistical testing, sequential monitoring, and confounding bias. Each showed non-significant effect of CEE+MPA on breast cancer. 
	- Cochrane RCT meta-analysis: HT reduces coronary heart disease & all-cause mortality in women starting HT when less than 60 years old or 10 years since menopause. 

## WHI Response to FDA Expert Panel (September 19, 2025)
See: https://www.whi.org/doc/banner/WHI_response_to_FDA_Expert_Panel_on_MHT_use_09.19.2025.pdf. 
