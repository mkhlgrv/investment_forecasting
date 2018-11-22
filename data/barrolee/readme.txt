

readme.txt
September 29, 1994








                     Data Set for a Panel of 138 Countries



                               Robert J. Barro

                                    and

                                Jong-Wha Lee






                            Revised January, 1994






                            Overview of the Data


     The data set contains variables for the panel estimation. Data are
presented either quinquennially for the years 1960-1985, i.e., 1960, 1965,
1970, 1975, 1980, and 1985, or for averages of five years' sub-periods over
1960-1985. The variables are named, when available, after those in
BARRO-WOLF. The term XX behind a variable denotes a specific year (for
example, P60 is the primary school enrollment ratio in 1960), while the term
X denotes average of a sub-period among the five year sub-periods: 1960-64,
1965-69,  1970-74, 1975-79,and 1980-84 for the averaged variables (e.g. INV3
denotes average investment rate during 1970-74). Some variables are also
available for the subperiod over 1985-1989 and for the year of 1990. Growth
rates are calculated for 1960-65 and so on. The data listed below are
available as ASCII files (see Appendix 2).

     The documentation of the data is divided into eight broad categories :

     (1) National Accounts of Income
     (2) Education
     (3) Population/ Fertility
     (4) Government Expenditures
     (5) PPP Deflators
     (6) political Variables
     (7) Trade Policy and Others
     Appendix  1. Country list.
               2. How to retrieve data.

     For the data sources, references are abbreviated as follows:

BANKS            :  Banks, A.S., "Cross-National Time Series Data Archive",
                    Center for Social Analysis, State University of New York
                    at Binghampton, September 1979, updated.
BARRO and LEE    :  Barro R. and J. Lee, "International Comparisons of
                    Educational Attainment," NBER Working Paper, 1993.
BARRO and WOLF   :  BARRO R. and H. Wolf, "Data Appendix for Economic
                    Growth in a Cross Section of Countries," 1989.
GASTIL           :  Freedom in the World, various years
GFS              :  IMF, Government Financial Statistics, various issues
HDR              :  UNDP, Human Development Report, 1990
IFS              :  IMF, International Financial Statistics, various issues
LEE              :  Lee, Jong-Wha, "International Trade, Distortions and
                    Long-run Economic Growth," IMF Staff Papers, June 1993.
OECD             :  OECD, OECD National Accounts, various issues
SH v.4.0         :  Summers, R and A. Heston, "A New Set of International
                    Comparisons of Real Product and Price Levels: Estimates
                    for 130 Countries, 1950-1985," Review of Income and
                    Wealth 34, No.1 (March 1988): 1-25, (Dataset on
                    Floppies)
SH v.5.0         :  Summers.R and A. Heston, "The Penn World Table (Mark 5):
                    An Expanded Set of International Comparisons,
                    1950-1988", Quarterly Journal of Economics, May 1991,
                    (Dataset on Floppies)
SH v.5.5         :  Summers.R and A. Heston, "The Penn World Table (Mark
                    5.5)" (Dataset on Floppies)
SIPRI            :  SIPRI Yearbooks, various issues.
UN               :  United Nations, Global Estimates and Projections of
                    Population by Sex and Age, the 1990 Assessment, 1991.
                    (Dataset on Floppies)
UNCTAD           :  United Nations Conference on Trade and Development,
                    Handbook of International Trade and Development
                    Statistics, Supplement, various issues.
UNESCO           :  UNESCO, Statistical Yearbooks , various issues.
WB               :  World Bank, World Tables, various editions.
WDR              :  World Bank, World Development Report Data Set, 1991.
WOOD             :  Wood, A., "Global Trends in Real Exchange Rates, 1960 to
                    1984," World Bank, 1988.



                          Variables Grouped by Category

____________________________________________________________________________
Category/
Variable         :          Definition and Source
____________________________________________________________________________


Identifying Variables

SHCODE           :  Numerical country code in SH v.5.0.
ASIAE            :  Dummy for East Asian countries
BACODE           :  BARRO-WOLF country code.
BIGSMPL          :  Dummy for Big Sample country code (98 countries).
                    Source : BARRO-WOLF.
LAAM             :  Dummy for Latin-American Countries.
OECD             :  Dummy for OECD members.
SAFRICA          :  Dummy for Sub-Saharan African countries.
SMPL97           :  Dummy for 97 countries (see Appendix 1). (This sample  is
                    used for the two decades panel estimation as in Barro
                    Robert J. and Xavier Sala-i-Martin, Economic Growth,
                    Chapter 12, forthcoming, 1994.)


(1) National Accounts

GDPSH4xx         :  Real GDP per capita (1980 international prices) from SH
                    v.4.0. (xx = 1950, 55, 60, 65, 70, 75, 80 and 85)

(GDP4xxL         :  Log value of GDPSH4xx.)

GRSH4x           :  Growth rate of GDPSH4xx.

INVSH4x          :  Ratio of real domestic investment (private plus public) to
                    real GDP. Source : SH v.4.0.

GDPSH5xx         :  Real GDP per capita (1985 international prices) from SH
                    v.5.5. (xx = 1950, 55, 60, 65, 70, 75, 80, 85, 89 and 90)

(GDP5xxL         :  Log value of GDPSH5xx.)

GRSH5x           :  Growth rate of GDPSH5xx.
                    (x = 1, 2, 3, 4, 5, 6 (1985-90) and 6a (1985-89))

INVSH5x          :  Ratio of real domestic investment (private plus public) to
                    real GDP. Source : SH v.5.5.
                    (x = 1, 2, 3, 4, 5, and 6)

GDPWBxx 1        :  Real GDP per capita ( 1980 constant prices; local currency)
                    from WB. (xx = 1960, 65, 70, 75, 80, 85, and 90)

GRWBx            :  Growth rate of GDPWBxx. (x = 1, 2, 3, 4, 5, and 6)

INVWBx           :  Ratio of nominal domestic investment (private plus public)
                    to nominal GDP (using current local currency). Source : WB


(2) Education


HUMANxx          :  Average schooling years in the total population over age 25
                    (xx=1960, 65, 70, 75, 80 and 85)
                    Source : BARRO and LEE

HUMANMxx         :  Average schooling years in the male population over age 25
                    Source : BARRO and LEE

HUMANFxx         :  Average schooling years in the female population over age
                    25.  Source : BARRO and LEE

PYRxx            :  Average years of primary schooling in the total population
                    over age 25.  Source : BARRO and LEE

PYRMxx           :  Average years of primary schooling in the male population
                    over age 25.  Source : BARRO and LEE

PYRFxx           :  Average years of primary schooling in the female population
                    over age 25.  Source : BARRO and LEE

SYRxx            :  Average years of secondary schooling in the total
                    population over age 25.  Source : BARRO and LEE

SYRMxx           :  Average years of secondary schooling in the male population
                    over age 25.  Source : BARRO and LEE

SYRFxx           :  Average years of secondary schooling in the female
                    population over age 25.  Source : BARRO and LEE

HYRxx            :  Average years of higher schooling in the total population
                    over age 25.  Source : BARRO and LEE

HYRMxx           :  Average years of higher schooling in the male population
                    over age 25.  Source : BARRO and LEE

HYRFxx           :  Average years of higher schooling in the female population
                    over age 25.  Source : BARRO and LEE

NOxx             :  Percentage of "no schooling" in the total population
                    Source: BARRO and LEE

NOMxx            :  Percentage of "no schooling" in the male population
                    Source: BARRO and LEE

NOFxx            :  Percentage of "no schooling" in the female population
                    Source: BARRO and LEE

PRIxx            :  Percentage of "primary school attained" in the total pop.
                    Source : BARRO and LEE

PRIMxx           :  Percentage of "primary school attained" in the male pop.
                    Source : BARRO and LEE

PRIFxx           :  Percentage of "primary school attained" in the female pop.
                    Source : BARRO and LEE

PRICxx           :  Percentage of "primary school complete" in the total pop.
                    Source : BARRO and LEE

PRICMxx          :  Percentage of "primary school complete" in the male pop.
                    Source : BARRO and LEE

PRICFxx          :  Percentage of "primary school complete" in the female pop.
                    Source : BARRO and LEE

SECxx            :  Percentage of "secondary school attained" in the total pop.
                    Source : BARRO and LEE

SECMxx           :  Percentage of "secondary school attained" in the male pop.
                    Source : BARRO and LEE

SECFxx           :  Percentage of "secondary school attained" in female pop.
                    Source : BARRO and LEE

SECCxx           :  Percentage of "secondary school complete" in the total pop.
                    Source : BARRO and LEE

SECCMxx          :  Percentage of "secondary school complete" in the male pop.
                    Source : BARRO and LEE

SECCFxx          :  Percentage of "secondary school complete" in female pop.
                    Source : BARRO and LEE

HIGHxx           :  Percentage of "higher school attained" in the total pop.
                    Source : BARRO and LEE

HIGHMxx          :  Percentage of "higher school attained" in the male pop.
                    Source : BARRO and LEE

HIGHFxx          :  Percentage of "higher school attained" in female pop.
                    Source : BARRO and LEE

HIGHCxx          :  Percentage of "higher school complete" in the total pop.
                    Source : BARRO and LEE

HIGHCMxx         :  Percentage of "higher school complete" in the male pop.
                    Source : BARRO and LEE

HIGHCFxx         :  Percentage of "higher school complete" in female pop.
                    Source : BARRO and LEE

DURP             :  Duration in years of the primary education in 1965
                    Source : BARRO and LEE

DURS             :  Duration in years of the secondary education in 1965
                    Source : BARRO and LEE

DURS1            :  Duration in years of the first cycle of the secondary
                    education (or a half of DURS if data is unavailable)
                    Source : BARRO and LEE

Pxx              :  Total gross enrollment ratio for primary education.
                    Source : UNESCO

PMxx             :  Male gross enrollment ratio for primary education.
                    Source : UNESCO

PFxx             :  Female gross enrollment ratio for primary education.
                    Source : UNESCO

Sxx              :  Total gross enrollment ratio for secondary education.
                    Source : UNESCO

SMxx             :  Male gross enrollment ratio for secondary education.
                    Source : UNESCO

SFxx             :  Female gross enrollment ratio for secondary education.
                    Source : UNESCO

Hxx              :  Total gross enrollment ratio for higher education.
                    Source : UNESCO

HMxx             :  Male gross enrollment ratio for higher education.
                    Source : UNESCO

HFxx             :  Female gross enrollment ratio for higher education.
                    Source : UNESCO

TEAPRIxx         :  Pupil/Teacher Ratio in primary school
                    (xx = 1950, 55, 60, 65, 70, 75, and 80) Source: UNESCO

TEASECxx         :  Pupil/Teacher Ratio in secondary school
                    (xx = 1950, 55, 60, 65, 70, 75, and 80) Source: UNESCO


(3) Population/Fertility 2


POPxx            :  Total Population.  Source : SH v.5.0
                    (xx = 1960, 65, 70, 75, 80 and 85)

WORKERxx         :  Ratio of total Workers to population. Source : SH v.5.0
                    (xx = 1960, 65, 70, 75, 80 and 85)

POP15xx          :  Population Proportion under 15. Source : UN
                    (xx = 1960, 65, 70, 75, 80 and 85)

POP65xx          :  Population Proportion over 65. Source : UN
                    (xx = 1960, 65, 70, 75, 80 and 85)

FERTxx, FERTx    :  Total fertility rate (children per woman)
                    Source : WB

MORTxx, MORTx    :  Infant Mortality Rate (ages 0-1)
                    Source : WB

LIFEE0xx, LIFEE0x:  Life expectancy at age 0
                    Source : WDR, WB and other national sources

GPOPx            :  Growth rate of population
                    Source : SH v.5.0



(4) Government Expenditure 3


GOVSH4x          :  Ratio of real government "consumption" expenditure to real
                    GDP. (Period average) from SH v.4.0.

GOVSH5x          :  Ratio of real government "consumption" expenditure to real
                    GDP. (Period average) from SH v.5.5.
                    (x  = 1, 2, 3, 4, 5, and 6)

GOVWBx           :  Ratio of nominal government "consumption" expenditure to
                    nominal GDP (using current local currency). Source : WB

GEXPx            :  Ratio of nominal government current expenditure (inccluding
                    interest payments and transfers) to nominal GDP.
                    (x= 3, 4, and 5).  Source : WB

GDEx 4           :  Ratio of nominal government expenditure on defense to
                    nominal GDP.  Source : GFS, SIPRI

GEERECx 5        :  Ratio of recurring nominal government expenditure on
                    education to nominal GDP.  Source : UNESCO

GEETOTx 6        :  Ratio of total nominal government expenditure on education
                    to nominal GDP.  Source : UNESCO

INVPUBx 7        :  Ratio of nominal public domestic investment (fixed capital
                    formation) to nominal GDP.  Source : WDR, OECD

GGCFDx           :  Ratio of real public domestic investment (using HS
                    deflator for investment) to real GDP (deflated).
                    Source : SH v.5.0, IFS, GFS

GVXDXE4x 8       :  Ratio of real government "consumption" expenditure net of
                    spending on defense and on education to real GDP, sourced
                    from SH v.4.0. (GVXDXE4=GOVSH4-GDE-GEEREC)

GVXDXE5x 8       :  Ratio of real government "consumption" expenditure net of
                    spending on defense and on education to real GDP, sourced
                    from SH v.5.5. (GVXDXE=GOVSH5-GDE-GEEREC)


(5) PPP Deflators


PYSH5xx          :  Price level of GDP (PPP GDP/ Xrate relative to U.S.)
                    (U.S=1.0).  Source : SH v.5.5

PCSH5%xx         :  Price level of consumption (PPP C/ Xrate relative to U.S.)
                    (U.S=1.0).  Source : SH v.5.5

PISH5xx          :  Price level of investment (PPP I/ Xrate relative to U.S.)
                    (U.S=1.0). Source : SH v.5.5

PGSH5xx          :  Price level of govern't consumption (PPP  G/  Xrate
                    relative to U.S.). (U.S=1.0).  Source : SH v.5.5


(6) Political Variables 9


ASSASSPx         :  Number of assassinations per million population per year
                    Source : BANKS

COUPx            :  Number of coups per year
                    Source : BANKS

REVOLx           :  Number of revolutions per year
                    Source : BANKS

REVOL            :  Number of revolutions per year, averaged over the period,
                    1960-84.

REVCOUP          :  Number of revolutions and coups per year, averaged over the
                    period, 1960-84.

PINSTABx         :  Measure of political instability.
                    (0.5*ASSASSPx + 0.5*REVOLx)

POLRIGHTx        :  Index of political rights (from 1 to 7; 1=most freedom),
                    Source : GASTIL
                    (x = 3, 4, 5, 6, B; 3=1972-74; B=1972-89.)

CIVLIBx          :  Index of civil liberties, (from 1 to 7; 1=most freedom),
                    1972-85.  Source : GASTIL
                    (x = 3, 4, 5, 6, B; 3=1972-74; B=1972-89.)

WARDUM           :  Dummy for countries that participated in at least one
                    external war over the period, 1960-85.  Source : BANKS


WARTIME          :  The fraction of time over 1960-85 involved in external war
                    Source : BANKS


(7) Trade Policy and Others


EXx              :  Ratio of export to GDP (in current international prices)
                    Source : SH v.5.0

IMx              :  Ratio of import to GDP (in current international prices)
                    Source : SH v.5.0

OWTI             :  Own-import weighted tariff rates on intermediate inputs and
                    capital goods. Source : LEE (Constructed from UNCTAD data)

OWQI             :  Own-import weighted non-tariff frequency on intermediate
                    inputs and capital goods.
                    Source : Constructed from UNCTAD data.

AREA             :  Size of land, million squares Km. Source : LEE

DIST             :  Average distance to capitals of world 20 major exporters,
                    weighted by values of biliateral imports, 1000 Km.
                    Source : LEE

FREEOP           :  Measure of "Free trade openness". Source : LEE
                    FREEOP=.528 - .026 log(AREA) -.095 log(DIST)

FREETAR          :  Measure of tariff restriction. Source : LEE
                    FREETAR=FREEOP*log(1+OWTI)

BMPx 10          :  Black market premium.  Source : WOOD and WDR
                    BMP = (Black market Xrate/ Official Xrate) -1
                    (Xrate = Local currency per dollar)
                    (x = 1, 2, 3, 4, 5, and 6)

BMPxL            :  Log (1+BMP)

XRxx             :  Exchange rate (domestic currency per U.S. dollar)
                    Source : SH v.5.0

TOTx 11          :  Terms of trade shock (growth rate of export prices minus
                    growth rate of import prices) Source: UNTACD, WB

LLYx             :  Ratio of liquid liabilities to GDP
                    Source : IFS
                    (x = 1, 2, 3, 4, 5, and 6)


                                      Notes

1. Data on real GDP in terms of constant local currency come from World Bank
except the following countries: Tunisia, Burma, Iran (from IFS), Iraq (from the
Unied Nations), and Taiwan (from Taiwan Statistical Data Book).

2. For the following countries, LIFE0 ,and MORT came from
 Iran : UN Demographic Yearbook for 1960,65.  World Tables.
 Iraq : UN Demographic Yearbook.
 Taiwan : Taiwan Statistical Yearbook. Average of male and female lifeexp

3. Nominal GDP data come from World Bank except three countries, such as Iran
(IFS), Iraq (IFS), and Taiwan (Taiwan Statistical Data Book). Data on Taiwan's
education expenditure come from Taiwan Statistical Yearbook.

4. GDE data come from SIPRI for most countries. The SIPRI data on the military
expenditure includes all paramilitary expenditure but does not include military
imports funded by foreign aids. For the following nine countries, GFS data are
used:  Gambia, Guinea-Bissau, Lesotho, Somalia, Swaziland, Barbados, Suriname,
Iceland, Malta.  Also data has been supplemented as follows:
  Bangladesh (for 1960, and 65): Statistical Digest of East Pakistan
  Guyana (for 1960)    : World Tables
  Nepal (for 1960)    : World Tables
  Papua N. Guinea: World Tables (for 1960, and 65) and Estimates of Revenue
     (for 1970)
  Rwanda (for 1960) : Bulletin de Statisque, 1984
  Singapore (for 1960)  : World Tables

5. The following countries (and years) do not have data on GEEREC, but they
have data on GEETOT:  Ivory coast (1980), Lesotho (1975), Mali (1960), Suriname
(1970), Bahrain (1960, 65, and 70), Oman (1960, and 65), U. Arab Emirate (1960,
and 65), Yemen (1960, and 65), Yugoslavia (1960), and Fiji (1965).

6. For the following countries, GEETOT comes from:
  Bangladesh (for 1960) : Statistical Digest of East Pakistan
  Benin (for 1960)     : Human Development Report, 1991
  Cyprus :  World Tables (for 1960, and 1965) and Republic of Cyprus,
     Statistical Abstract
  Indonesia (for 1960, and 1965): Human Development Report, 1991
  Jordan (for 1965):  World Tables
  Papua N. Guinea (80 ): Estimates of Revenue and Expenditure

7. INVPUB data for the most OECD countries come from OECD.  Data for Developing
countries including Portugal, New Zealand and Yugoslavia come from WDR. The
public investment of the developing countries (and Switzerland) is defined as
capital expenditure of the consolidated general government plus that of public
corporate entities.  All inventories are assumed to be held in the private
sector.  Some OECD data do not include investment by public enterprises. This
data are available for most countries since 1970.


8. Since total government consumption and defence/education expenditures are
differently measured, net government consumption ratios are negative for some
countries. For these cases, we assumed 0.01.

9. BANKS do not report data on political variables for Hong Kong. Thus, we
assume that there have been none of political crises during the period from
1960 to 1985 in Hong Kong.

10. Data on black market premium for the following countries come from WDR:
Gabon, Gambia, Guinea, Mali, Mauritius, Sierra Leone, Somalia, Swaziland,
Barbados, Trinidad & Tobago, Guyana, Jordan, Yemen-N. Arab, Cyprus, Hungary,
Malta, Poland, Papua N. Guinea. Western Samoa. And data for Cyprus and Taiwan
come from Pick's Currency Yearbook.

11. Data on terms of trade shock for the following countries for the period 2
are the growth rates of the terms of trade over period 1966-70: Botwana, Burma,
Burundi, China, Kuwait, Lesotho, Oman, Yemen-N. Arab, Papua N. Guinea,
U.Arab.Emirates, Zimbabwe.




                            Appendix 1. Country List

      SHCODE     : Summers-Heston V.5.0 country code
                   (This code is also used in this data set)
      WBCTRY     : World Bank country code
      Sample (1) : BIGSMPL in BARRO and WOLF (N=98)
      Sample (2) : SMPL97. A sample of 97 countries

_________________________________________________________________

SHCODE     COUNTRY  NAME           WBCTRY       (1)       (2)
_________________________________________________________________

  1        Algeria                  DZA          +         +
  2        Angola                   AGO          -         -
  3        Benin                    BEN          -         +
  4        Botswana                 BWA          +         +
  5        Burkina Faso             HVO          -         -
  6        Burundi                  BDI          +         -
  7        Cameroon                 CMR          +         +
  8        Cape verde               CPV          -         -
  9        Central African Rep.     CAF          +         +
 10        Chad                     TCD          -         -
 11        Comoros                  COM          -         -
 12        Congo                    COG          -         +
 13        Egypt                    EGY          +         +
 14        Ethiopia                 ETH          +         -
 15        Gabon                    GAB          +         -
 16        Gambia                   GMB          -         +
 17        Ghana                    GHA          +         +
 18        Guinea                   GIN          -         -
 19        Guinea-Bissau            GNB          -         -
 20        Cote d'Ivoire            CIV          +         -
 21        Kenya                    KEN          +         +
 22        Lesotho                  LSO          -         +
 23        Liberia                  LBR          +         +
 24        Madagascar               MDG          +         -
 25        Malawi                   MWI          +         +
 26        Mali                     MLI          -         +
 27        Mauritania               MRT          -         -
 28        Mauritius                MUS          +         +
 29        Morocco                  MAR          +         -
 30        Mozambique               MOZ          -         -
 31        Niger                    NER          -         +
 32        Nigeria                  NGA          +         -
 33        Rwanda                   RWA          +         +
 34        Senegal                  SEN          +         +
 35        Seychelles               SYC          -         -
 36        Sierra Leone             SLE          +         +
 37        Somalia                  SOM          -         -
 38        South africa             ZAF          +         +
 39        Sudan                    SDN          +         +
 40        Swaziland                SWZ          +         +
 41        Tanzania                 TZA          +         +
 42        Togo                     TGO          +         +
 43        Tunisia                  TUN          +         +
 44        Uganda                   UGA          +         +
 45        Zaire                    ZAR          +         +
 46        Zambia                   ZMB          +         +
 47        Zimbabwe                 ZWE          +         +
 48        Bahamas, The             BHS          -         -
 49        Barbados                 BRB          +         +
 50        Canada                   CAN          +         +
 51        Costa Rica               CRI          +         +
 52        Dominica                 DMA          -         -
 53        Dominican Rep.           DOM          +         +
 54        El Salvador              SLV          +         +
 55        Grenada                  GRD          -         -
 56        Guatemala                GTM          +         +
 57        Haiti                    HTI          +         +
 58        Honduras                 HND          +         +
 59        Jamaica                  JAM          +         +
 60        Mexico                   MEX          +         +
 61        Nicaragua                NIC          +         +
 62        Panama                   PAN          +         +
 63        St.Lucia                 LCA          -         -
 64        St.Vincent & Grens.      VCT          -         -
 65        Trinidad & Tobago        TTO          +         +
 66        United States            USA          +         +
 67        Argentina                ARG          +         +
 68        Bolivia                  BOL          +         +
 69        Brazil                   BRA          +         +
 70        Chile                    CHL          +         +
 71        Colombia                 COL          +         +
 72        Ecuador                  ECU          +         +
 73        Guyana                   GUY          +         +
 74        Paraguay                 PRY          +         +
 75        Peru                     PER          +         +
 76        Suriname                 SUR          -         -
 77        Uruguay                  URY          +         +
 78        Venezuela                VEN          +         +
 79        Afghanistan              AFG          -         -
 80        Bahrain                  BHR          -         -
 81        Bangladesh               BGD          +         +
 82        Myanmar (Burma)          BUR          +         -
 83        China                    CHN          -         -
 84        Hong Kong                HKG          +         +
 85        India                    IND          +         +
 86        Indonesia                IDN          +         +
 87        Iran, I.R. of            IRN          -         +
 88        Iraq                     IRQ          +         +
 89        Israel                   ISR          +         +
 90        Japan                    JPN          +         +
 91        Jordan                   JOR          +         +
 92        Korea                    KOR          +         +
 93        Kuwait                   KWT          -         -
 94        Malaysia                 MYS          +         +
 95        Nepal                    NPL          +         +
 96        Oman                     OMN          -         -
 97        Pakistan                 PAK          +         +
 98        Philippines              PHL          +         +
 99        Saudi Arabia             SAU          -         -
100        Singapore                SGP          +         +
101        Sri Lanka                LKA          +         +
102        Syria                    SYR          -         +
103        Taiwan                   OAN          +         +
104        Thailand                 THA          +         +
105        United Arab Emirates     ARE          -         -
106        Yemen, N.Arab            YEM          -         +
107        Austria                  AUT          +         +
108        Belgium                  BEL          +         +
109        Cyprus                   CYP          +         +
110        Denmark                  DNK          +         +
111        Finland                  FIN          +         +
112        France                   FRA          +         +
113        Germany, West            DEU          +         +
114        Greece                   GRC          +         +
115        Hungary                  HUN          -         -
116        Iceland                  ISL          +         -
117        Ireland                  IRL          +         +
118        Italy                    ITA          +         +
119        Luxembourg               LUX          +         -
120        Malta                    MLT          +         +
121        Netherlands              NLD          +         +
122        Norway                   NOR          +         +
123        Poland                   POL          -         -
124        Portugal                 PRT          +         +
125        Spain                    ESP          +         +
126        Sweden                   SWE          +         +
127        Switzerland              CHE          +         +
128        Turkey                   TUR          +         +
129        United Kingdom           GBR          +         +
130        Yugoslavia               YUG          -         +
131        Australia                AUS          +         +
132        Fiji                     FJI          +         -
133        New Zealand              NZL          +         +
134        Papua New Guinea         PNG          +         +
135        Solomon Islands          SLB          -         -
136        Tonga                    TON          -         -
137        Vanuatu                  VUT          -         -
138        Western Samoa            WSM          -         -
_________________________________________________________________


                       Appendix 2.  How to retrieve data.


     The 30 files are found on the enclosed diskette.  Each of the files
contains data as described below.  The files are plain text (ASCII) files. Data
on each variable are listed by the order of SHCODE (see Appendix 1).  The
missing observations are denoted by 'NA'.


ASCII File                   Data

codes.prn        :  asiae bacode bigsmpl laam oecd safrica smpl97

gdpsh4.prn       : gdpsh450 gdpsh455 gdpsh460 gdpsh465 gdpsh470 gdpsh475
                 gdpsh480 gdpsh485 grsh41 grsh42 grsh43 grsh44 grsh45 invsh41
                 invsh42 invsh43 invsh44 invsh45
gdpsh5.prn       : gdpsh550 gdpsh555 gdpsh560 gdpsh565 gdpsh570 gdpsh575
                 gdpsh580 gdpsh585 gdpsh589 gdpsh590 grsh51 grsh52 grsh53
                 grsh54 grsh55 grsh56 grsh56a invsh51 invsh52 invsh53 invsh54
                 invsh55 invsh56
gdpwb.prn        : gdpwb60 gdpwb65 gdpwb70 gdpwb75 gdpwb80 gdpwb85 gdpwb90
                 grwb1 grwb2 grwb3 grwb4 grwb5 grwb6 invwb1 invwb2 invwb3
                 invwb4 invwb5


no.prn           : no60 no65 no70 no75 no80  no85 nom60 nom65 nom70 nom75 nom80
                 nom85 nof60 nof65 nof70 nof75 nof80 nof85
pri.prn          : pri60 pri65 pri70 pri75 pri80  pri85 prim60 prim65 prim70
                 prim75 prim80 prim85 prif60 prif65 prif70 prif75 prif80 prif85
pric.prn         : pric60 pric65 pric70 pric75 pric80  pric85 pricm60 pricm65
                 pricm70 pricm75 pricm80 pricm85 pricf60 pricf65 pricf70
                 pricf75 pricf80 pricf85
sec.prn          : sec60 sec65 sec70 sec75 sec80  sec85 secm60 secm65 secm70
                 secm75 secm80 secm85 secf60 secf65 secf70 secf75 secf80 secf85
secc.prn         : secc60 secc65 secc70 secc75 secc80  secc85 seccm60 seccm65
                 seccm70 seccm75 seccm80 seccm85 seccf60 seccf65 seccf70
                 seccf75 seccf80 seccf85
high.prn         : high60 high65 high70 high75 high80  high85 highm60 highm65
                 highm70 highm75 highm80 highm85 highf60 highf65 highf70
                 highf75 highf80 highf85
highc.prn        : highc60 highc65 highc70 highc75 highc80  highc85 highcm60
                 highcm65 highcm70 highcm75 highcm80 highcm85 highcf60 highcf65
                 highcf70 highcf75 highcf80 highcf85
pyr.prn          : pyr60 pyr65 pyr70 pyr75 pyr80  pyr85 pyrm60 pyrm65 pyrm70
                 pyrm75 pyrm80 pyrm85 pyrf60 pyrf65 pyrf70 pyrf75 pyrf80 pyrf85
syr.prn          : syr60 syr65 syr70 syr75 syr80  syr85 syrm60 syrm65 syrm70
                 syrm75 syrm80 syrm85 syrf60 syrf65 syrf70 syrf75 syrf80 syrf85
hyr.prn          : hyr60 hyr65 hyr70 hyr75 hyr80  hyr85 hyrm60 hyrm65 hyrm70
                 hyrm75 hyrm80 hyrm85 hyrf60 hyrf65 hyrf70 hyrf75 hyrf80 hyrf85
human.prn        : human60 human65 human70 human75 human80  human85 humanm60
                 humanm65 humanm70 humanm75 humanm80 humanm85 humanf60 humanf65
                 humanf70 humanf75 humanf80 humanf85
enrolp.prn       : p60 p65 p70 p75 p80  p85 pm60 pm65 pm70 pm75 pm80 pm85 pf60
                 pf65 pf70 pf75 pf80 pf85
enrols.prn       : s60 s65 s70 s75 s80  s85 sm60 sm65 sm70 sm75 sm80 sm85 sf60
                 sf65 sf70 sf75 sf80 sf85
enrolh.prn       : h60 h65 h70 h75 h80  h85 hm60 hm65 hm70 hm75 hm80 hm85 hf60
                 hf65 hf70 hf75 hf80 hf85
teacher.prn      : teapri50 teapri55 teapri60 teapri65 teapri70 teapri75
                 teapri80 teasec50 teasec55 teasec60 teasec65  teasec70
                 teasec75 teasec80 durp durs durs1

pop.prn          : pop60 pop65 pop70 pop75 pop80 pop85 worker60 worker65
                 worker70 worker75 worker80 worker85 pop1560 pop1565 pop1570
                 pop1575 pop1580 pop1585 pop6560 pop6565 pop6570 pop6575
                 pop6580 pop6585
fert.prn         : fert60 fert65 fert70 fert75 fert80 fert85 mort60 mort65
                 mort70 mort75 mort80 mort85 lifee060 lifee065 lifee070
                 lifee075 lifee080 lifee085 gpop1 gpop2 gpop3 gpop4 gpop5

fert1.prn        : fert1 fert2 fert3 fert4 fert5 mort1 mort2 mort3 mort4 mort5
                 lifee01 lifee02 lifee03 lifee04 lifee05

gvxdxe.prn       : govsh41 govsh42 govsh43 govsh44 govsh45 govsh51 govsh52
                 govsh53 govsh54 govsh55 govsh56 gvxdxe41 gvxdxe42 gvxdxe43
                 gvxdxe44 gvxdxe45 gvxdxe51 gvxdxe52 gvxdxe53 gvxdxe54 gvxdxe55
geegde.prn       : geetot1 geetot2 geetot3 geetot4 geetot5  geerec1 geerec2
                 geerec3 geerec4 geerec5 gde1 gde2 gde3 gde4 gde5
invpub.prn       : invpub1 invpub2 invpub3 invpub4 invpub5 ggcfd1 ggcfd2 ggcfd3
                 ggcfd4 ggcfd5
govwb.prn        : govwb1 govwb2 govwb3 govwb4 govwb5 gexp3 gexp4 gexp5

ppp.prn          : pysh560 pysh565 pysh570 pysh575 pysh580 pysh585 pcsh560
                 pcsh565 pcsh570 pcsh575 pcsh580 pcsh585 pish560 pish565
                 pish570 pish575 pish580 pish585 pgsh560 pgsh565 pgsh570
                 pgsh575 pgsh580 pgsh585

politics.prn     : assassp1 assassp2 assassp3 assassp4 assassp5 coup1 coup2
                 coup3 coup4 coup5  revol1 revol2 revol3 revol4 revol5 revol
                 revcoup

pinstab.prn      : pinstab1 pinstab2 pinstab3 pinstab4 wardum wartime pinstab5
                 prights3 prights4 prights5 prights6 prightsb civlib3 civlib4
                 civlib5 civlib6 civlibb

trade.prn        : ex1 ex2 ex3 ex4 ex5 im1 im2 im3 im4 im5 xr60 xr65 xr70 xr75
                 xr80 xr85 tot1 tot2 tot3 tot4 tot5

bmp.prn          : bmp1 bmp2 bmp3 bmp4 bmp5 bmp6 bmp1l bmp2ml bmp3l bmp4l bmp5l
                 bmp6l owti owqi area dist freeop freetar lly1 lly2 lly3 lly4
                 lly5 lly6

