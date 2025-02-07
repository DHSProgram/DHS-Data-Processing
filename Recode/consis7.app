PROC GLOBAL

{ Country = COUNTRY, Year. }

{ Version 3.0.1 of 10/22/07, update 11/25/08 - Based on Standard Recode DHS-V 1.1.1 - }
{ Every message should be classified as:
    1) Structure check -> adjust recode or add country specific skipping pattern to notes secion in recode.doc
    2) Note in Doc -> add to notes section in recode.doc
    3) Inconsistency in DOC -> add to inconsistency secion in recode.doc
    4) Test to be commented out because variable(s) are not applicable
}

  set explicit;

  array kount(99);
  array hwqline(99);   { line numbers }
  array hwqtype(99);   { type of entry }

  numeric cday, xday,
          year, month, day,
          leapy;
  numeric iday,   {* code cdc of interview date *}
          bday,   {* code cdc of birth date     *}
          flagv;  {* Flag error on vaccination  *}
  numeric cdcb, cdcd, cdcp, cdcm, cdcva, cdcva2;
  numeric dday, pday;
  numeric ycmc;
  numeric x, x1, x2, x3, cnt, cntw, cntm, v, usedv, nbans;
  numeric soc1, soc4;
  numeric xsex;
  numeric alt, adjalt, adjsmk, adjanem, anemlev, preg;
  numeric e, ok;
  numeric i, j, k, xi;
  numeric imax;
  numeric childm, lastb;
  numeric found;
  numeric kmethod, umethod, method, methcode, obiuse;
  numeric interval;
  numeric lastsex, durabst;
  numeric pregnant, amenor, user, whenwant, fail, infert, infert2, sex30;
  numeric defacto,    { De facto sample = 1, de jure = 0 }
          mHV020,     { Ever married men/husband = 1     }
          anthro,     { Height & Weight history exists }
          anthroHH,   { Height & Weight history collected in HH }
          maxmemb,    { Maximum household members }
          HWmin,      { Height & Weight minimum age }
          HWmax,      { Height & Weight maximum age }
          Hmax,       { Health section based on 5 or 3 years }
          Fmax,       { Maximum for feeding questions }
          Vmax,       { Maximum for vaccination questions }
          lastch,     { Questions for last child }
          minab,      { Minimum age at birth }
          minam;      { Minimum age at marriage }
  numeric myear;      { Cut off year for method use }
  alpha(19) vcalmeth; { method in calendar }
  alpha(19) vcaldisc; { discontinuation in calendar }
  numeric vcallen,
          len,
          calused,    { Calendar used }
          col2used,   { Discontinuation = column 2 in calendar used }
          col3used,   { Marriage = column 3 in calendar used }
          col4used,   { Region = column 4 in calendar used }
          col5used,   { Source of contraception = column 5 in calendar used }
          maxcols,
          calcmc,
          caluse,
          used,
          usem, { use of method }
          oldres,
          curres  { residence }
          preval,
          useval;

  numeric aidsmod;    { AIDS module used }
  numeric marstatm,   { Marital status in MMR section (1) }
          cutoff;     { Cut off age for the MMR section   }
  numeric births35, births24, births5, b;
  numeric twins;
  numeric treated;
  numeric preschool;
  numeric DaysMonth = 30.4375;
  numeric minaim,maxaim;
  numeric fdingcutoff; { !!! age limit for feeding section DSH7 is 24 months !!!}

  function cdc(vday,vmonth,vyear,cmin)
  {* Code of the day since 01/01/1980 *}

    if special(vyear) | vyear < 1980 | vyear > 9000 then
      cday = cmin
    else
      { handle leap years properly }
      cday = int((vyear-1980)*365.25+0.75);       {* B2: 1960...2007 *}
      leapy = (vyear%4=0);
      if special(vmonth) | vmonth > 12 then
        if cday < cmin & cday+365+leapy > cmin then
          cday = cmin
        endif
      else
        box vmonth => xday;
               0,1 => 0;      {31}
                 2 => 31;     {28}
                 3 => 59;     {31}
                 4 => 90;     {30}
                 5 => 120;    {31}
                 6 => 151;    {30}
                 7 => 181;    {31}
                 8 => 212;    {31}
                 9 => 243;    {30}
                10 => 273;    {31}
                11 => 304;    {30}
                12 => 334;    {31}
        endbox;
        if leapy & vmonth > 2 then
          xday = xday + 1
        endif;
        cday = cday+xday;
        if special(vday) | vday > 31 then
          if cday < cmin & cday+31 > cmin then
            cday = cmin
          endif
        else
          cday = cday+vday
        endif
      endif
    endif;
    cdc = cday;
  end

  function errvac(idx)
    e = errmsg(4390,flagv);
    e = errmsg(4391,H2d(idx),H2m(idx),H2y(idx));     { BCG       }
    e = errmsg(4392,H0d (idx),H0m (idx),H0y (idx),   { POLIO 0   }
                    H4d (idx),H4m (idx),H4y (idx),   { POLIO 1   }
                    H6d (idx),H6m (idx),H6y (idx),   { POLIO 2   }
                    H8d (idx),H8m (idx),H8y (idx));  { POLIO 3   }
    e = errmsg(4393,H3d (idx),H3m (idx),H3y (idx),   { DPT 1     }
                    H5d (idx),H5m (idx),H5y (idx),   { DPT 2     }
                    H7d (idx),H7m (idx),H7y (idx));  { DPT 3     }
    e = errmsg(4394,H9d (idx),H9m (idx),H9y (idx));  { MEASLES   }
    e = errmsg(4395,H33d(idx),H33m(idx),H33y(idx));  { VITAMIN A }
    e = errmsg(4396,H40d(idx),H40m(idx),H40y(idx));  { VITAMIN A }
  end

  function cdc2date(zday);
  {* Extract the day, month and year from the day code *}
    year = int((zday-1)/365.25)+1980;
    zday = zday - int((year-1980)*365.25+0.75);
    leapy = (year%4=0);
    if !leapy & zday > 59 then
      zday = zday + 1
    endif;
    box zday => month;
       <=31  =>  1;    {31}
       <=60  =>  2;    {29!}
       <=91  =>  3;    {31}
       <=121 =>  4;    {30}
       <=152 =>  5;    {31}
       <=182 =>  6;    {30}
       <=213 =>  7;    {31}
       <=244 =>  8;    {31}
       <=274 =>  9;    {30}
       <=305 => 10;    {31}
       <=335 => 11;    {30}
             => 12;    {31}
    endbox;
    box month => day;
         1    => zday    ; {31}
         2    => zday-31 ; {29!}
         3    => zday-60 ; {31}
         4    => zday-91 ; {30}
         5    => zday-121; {31}
         6    => zday-152; {30}
         7    => zday-182; {31}
         8    => zday-213; {31}
         9    => zday-244; {30}
        10    => zday-274; {31}
        11    => zday-305; {30}
        12    => zday-335; {31}
    endbox;
  end;

  function rowcmc(yrow);
  {* CMC of calendar row *}
    if yrow < V018 | yrow > vcallen then
    {* After the interview or before the beginning of the calendar *}
      ycmc = 0
    else
      ycmc = V017+vcallen-yrow
    endif;
    rowcmc = ycmc;
  end;

  function fed(foodvar);
  {* Valid values from 1 to 7 *}
    fed = (!special(foodvar) & foodvar > 0 & foodvar < 8);
  end;

  function AddVals( Var )

    if     Var = 1       then x  = x  + 1
    elseif Var = missing then x1 = x1 + 1
    elseif Var = notappl then x2 = x2 + 1
    endif;
    k = k + 1;

    AddVals = 1;

  end;

  function validresp(hwval);
  {* used to check anthropometric results for missing /refused/not present etc *}
    validresp = !(hwval in missing, 9994:9996);
  end;


PROC RECODE7_FF
preproc

  defacto  = 1;           { De facto sample = 1, de jure = 0 }         {!!}
  mHV020   = 0;           { Ever married men/husband = 1     }         {!!}

  anthro   = 1;           { Height & Weight check in individual quest.}{!!}
  anthroHH = 1;           { Height & Weight collected in HH }          {!!}

  maxmemb  = 90;          { Maximum household members }                {!!}

  HWmin    = 0;           { Height & Weight minimum age }              {!!}
  HWmax    = 59;  {35}    { Height & Weight maximum age }              {!!}

  Hmax     = 59;  {35}    { Health section based on 5 or 3 years }     {!!}
  Fmax     = 35;          { Maximum for feeding questions }            {!!}
  Vmax     = 35;          { Maximum for vaccination questions }        {!!}
  lastch   = 1;           { Questions for last child only }            {!!}

  minab    = 120;         { Minimum age at birth }                     {!!}
  minam    = 120;         { Minimum age at marriage }                  {!!}

  minaim   = 15;        { !!! Minimum age at interview - men           }
  maxaim   = 54;        { !!! Maximum age at interview - men           }

  calused  = 1;           { Calendar used: 1-yes, 0-no }               {!!}
  col2used = 1;           { Discontination in calendar: 1-yes, 0-no }  {!!}
  col3used = 0;           { Marriage column in calendar: 1-yes, 0-no } {!!}
  col4used = 0;           { Country specific column used: 1-yes, 0-no }{!!}
  col5used = 0;           { Source of contraception column used: 1-0 } {!!}

  vcalmeth = "123456789WNALCFàáâ?";                                    {!!}
  { if bcoreq then vcalmeth = " " endif; }
  vcaldisc = "1236574908FADXRBTZ?";

  myear    = 2010;        { Cut off year for method use: A310 }        {!!}
  vcallen  = 80;

  aidsmod  = 0;            { AIDS module used: 1 = used, 0 = not used }{!!}

  marstatm = 0;            { Marital status in MMR section (1) }       {!!}
  cutoff   = 12;           { Cut off age for the MMR section   }       {!!}

  {* kount: Statistics for repeated errors that are not corrected. *}
  i = 1;
  while i <= 99 do
    kount(i) = 0;
    i = i + 1
  enddo;

  
postproc

  {* Printout statistics on particular errors *}
  i = 1;
  while i <= 99 do
    if kount(i) then
      e = errmsg(20000+i,kount(i))
    endif;
    i = i + 1
  enddo;

{ ---------------------------------------------------------------------------- }
PROC HOUSEHOLD

  i = 1;
  while i <= maxmemb do
    hwqline (i) = 0;      {* Line number of member/child of member   *}
    hwqtype (i) = 0;      {* 2 = Respondent, 3 = Child of respondent *}
    i = i + 1
  enddo;

PROC RECH0_EDT
preproc

  kount(95) = kount(95) + 1;
  if HV015 = 1 then
    kount(97) = kount(97) + 1;
  endif;

  {* Household's basic data *} {* Set unused variables to <> notappl !!}
  if !length(strip(HV000)) |
     special(HV001) | special(HV002) | special(HV003) |
     special(HV004) | special(HV005) | special(HV006) | special(HV007) |
     special(HV008) | special(HV009) | special(HV010) | special(HV011) |
     special(HV012) | special(HV013) | special(HV014) | special(HV015) |
     special(HV016) | special(HV017) | HV018 = notappl| HV019 = notappl|
     special(HV020) | special(HV021) | special(HV022) | special(HV023) |
     special(HV024) | special(HV025) | HV026 = notappl| special(HV027) |
     special(HV028) | special(HV030) | special(HV031) | special(HV032) |
     special(HV035) | HV040 = missing| special(HV041) |
     special(HV042) | HV044 = missing
   then
     e = errmsg(12000,"RECH0");
     errmsg("HV000 set: HV000=%s",HV000);
     errmsg("not special: HV001=%d,HV002=%d,HV003=%d,HV004=%d,HV005=%d,HV006=%d",HV001,HV002,HV003,HV004,HV005,HV006);
     errmsg("not special: HV007=%d,HV008=%d,HV009=%d,HV010=%d,HV011=%d,HV012=%d,HV013=%d",HV007,HV008,HV009,HV010,HV011,HV012,HV013);
     errmsg("not special: HV014=%d,HV015=%d,HV016=%d,HV017=%d,HV020=%d,HV021=%d,HV022=%d",HV014,HV015,HV016,HV017,HV020,HV021,HV022);
     errmsg("not special: HV023=%d,HV024=%d,HV025=%d,HV027=%d,HV028=%d",HV023,HV024,HV025,HV027,HV028);
     errmsg("not special: HV030=%d,HV031=%d,HV032=%d,HV035=%d,HV041=%d,HV042=%d",HV030,HV031,HV032,HV035,HV041,HV042);
     errmsg("not notappl: HV018=%d,HV019=%d,HV026=%d",HV018,HV019,HV026);
     errmsg("not missing: HV040=%d,HV044=%d",HV040,HV044);
  endif;

  {* HV008 = HV006 * 12 + HV007 *}
  if HV008 <> cmcode(HV006,HV007) then
    e = errmsg(10100,HV006,HV007,HV008)
  endif;

  {* Eligible men and women and de jure children under 5 more than HH members *}
  if HV010+HV011+HV014 > HV009 then
    e = errmsg(10101,HV009,HV010,HV011,HV014)
  endif;

  if HV012 > HV009 |      {* De jure members more than HH members *}
     HV013 > HV009 |      {* De facto members more than HH members *}
     HV012+HV013 < HV009  {* De jure and De facto members less than HH members *}
    then
    e = errmsg(10102,HV009,HV012,HV013)
  endif;

  if defacto then
    {* Eligible members more than de facto members *}
    if HV010+HV011 > HV013 then
      e = errmsg(10103,3,HV013,HV010,HV011)
    endif;
  else {* De jure sample *}
    {* Eligible members more than de jure members *}
    if HV010+HV011 > HV012 then
      e = errmsg(10103,2,HV012,HV010,HV011)
    endif;
  endif;

  {* Sample weight set for incomplete interviews *}
  if HV005 = 0 <=> HV015 = 1 then
    e = errmsg(10104,HV005,HV015)
  endif;

  {* Ultimate area unit not the cluster number *}
  if HV004 <> HV001 then
    kount(54) = kount(54) + 1;
  endif;
  {* Primary sampling unit not the cluster number *}
  if HV021 <> HV001 then
    kount(55) = kount(55) + 1;
  endif;
  {* Sample stratum number equation of the cluster number *}
  if HV022 = int((HV001+1)/2) then
    kount(56) = kount(56) + 1;
  endif;
  {* Sample domain is national *}
  if HV023 = 0 then
    kount(57) = kount(57) + 1;
  endif;
  {* Check Husband vs male survey (Yes/No variable) *}
  if HV027 = 1 then
    kount(58) = kount(58) + 1;
  elseif HV027 = 2 then
    kount(59) = kount(59) + 1;
  endif;

  {* Setting of the sample weight for a male/husband subsample *}
  if HV028 = 0 <=> (HV027 & HV015 = 1) then
    e = errmsg(10105,HV027,HV028,HV015)
  endif;

  {* Check altitude for hemoglobin selection *}
  if HV042 = 1 <=> special(HV040) then
    if HV042 = 1 then     {!!! Take out in case you want the altitude set for selected cases only }
      e = errmsg(11030,HV042,HV040)
    endif;                {!!! Take out in case you want the altitude set for selected cases only }
  endif;

  { ---------------------------------------------------------------------------- }
  {* Basic data vs HH schedule *}

  {* Total number of HH members vs occurrences of the HH schedule *}
  cnt = soccurs(RECH1);
  if HV009 <> cnt then
    e = errmsg(10106,HV009,cnt)
  endif;

  {* Number of eligible members *}
  {* Women *}
  cnt = count(RECH1_EDT where HV117 = 1);
  if HV010 <> cnt then
    e = errmsg(10107,HV010,cnt);
  endif;
  {* Men *}
  cnt = count(RECH1_EDT where HV118 = 1);
  if HV011 <>  cnt then
    e = errmsg(10108,HV011,cnt);
  endif;
  {* De jure members *}
  cnt = count(RECH1_EDT where HV102 = 1);
  if HV012 <> cnt then
    e = errmsg(10109,HV012,cnt);
  endif;
  {* De facto members *}
  cnt = count(RECH1_EDT where HV103 = 1);
  if HV013 <> cnt then
    e = errmsg(10110,HV013,cnt);
  endif;
  {* De jure children under 6 *}
  cnt = count(RECH1_EDT where HV102 = 1 & HV105 in 0:5);
  if HV014 <> cnt then
    e = errmsg(10111,HV014,cnt);
  endif;
  {* Eligible children for height and weight: All children under 5 *}
  cnt = count(RECH6_EDT where HV008-HC32 < 60);
  if HV035 <> cnt then
    e = errmsg(10112,HV035,cnt);
  endif;
  {* Adults measured: All women 15-49 & All men 15-49.  Take out those who were not measured !! *}
  cntw = count(RECH1_EDT where HV104 = 2 & HV105 in 15:49);
  cntm = count(RECH1_EDT where HV104 = 1 & HV105 in minaim:maxaim);
  cnt  = cntw + cntm;
  if HV041 <> cnt then
    e = errmsg(10113,HV041,cnt);
  endif;

{ ---------------------------------------------------------------------------- }
PROC RECH1_EDT
preproc

  for i in RECH1_EDT do

    {*!! Preschool = 0; *}
    {*!! Either set preschool = 1; or change preschool into its CS variable = value of preschool; *}
    {*!! within the messages that includes preschool.                                             *}
    if HV101 = notappl | HV102 = notappl | HV103 = notappl |
       HV104 = notappl | HV105 = notappl | HV106 = notappl |
       HV108 = notappl | HV109 = notappl | HV110 = notappl |
       HV117 = missing | HV118 = missing |
       HV120 = missing |
       HV122 = notappl |
       {HV124 = notappl |OUT of consis HV124 can be NA when HV121 = 0 BERT!!}
       HV125 = notappl | HV126 = notappl |
       HV128 = notappl
     then
       e = errmsg(12101,i,"RECH1");
       errmsg("not notappl: HV101=%d,HV102=%d,HV103=%d,HV104=%d,HV105=%d,HV106=%d,HV108=%d",HV101,HV102,HV103,HV104,HV105,HV106,HV108);
       errmsg("not notappl: HV109=%d,HV110=%d,HV122=%d,HV124=%d,HV125=%d,HV126=%d,HV128=%d",HV109,HV110,HV122,HV124,HV125,HV126,HV128);
       errmsg("not missing: HV117=%d,HV118=%d,HV120=%d",HV117,HV118,HV120);
    endif;

  enddo;

  {* Household schedule *}

  {* Head of HH is not the first member of the HH *}
  if HV101(1) <> 1 then                         {?? Should it be a count only??*}
    kount(60) = kount(60) + 1;
  endif;

  soc1 = sOccurs(RECH1);
  soc4 = sOccurs(RECH4);

  {* Occurrences of the HH schedule and the CS HH schedule should equal *}
  if soc4 & (soc1 <> soc4) then  {* CS HH schedule variables exist *}
    e = errmsg(11027,soc1,soc4)
  endif;

  for i in RECH1_EDT do

    {* HH schedule's indexes are incorrect *}
    if i <> HVIDX then
      e = errmsg(11000,i,HVIDX)
    endif;

    {* Indexes of the CS HH schedule and the HH schedule should match *}
    if soc4 & IDXH4(i) <> HVIDX then
      e = errmsg(11001,i,HVIDX,IDXH4(i))
    endif;

    {* Not de jure nor de facto member *}
    if HV102 <> 1 & HV103 <> 1 then
      e = errmsg(11003,i,HV102,HV103)
    endif;

    {* Co-spouse not woman *}
    if HV101 = 9 & HV104 <> 2 then
      e = errmsg(11004,i,HV101,HV104)
    endif;

    {* Children under 5 with education.  !!! Check the questionnaire first *}
    if HV105 in 0:4 & HV106 <> 0 then
      e = errmsg(11007,i,HV106,HV105)
    endif;

  //preschool = (SH17A(i) = 0); // todo check with GR how to get preschool from standard data !!!
    {* No grade known but level of education or vice versa *}
    if (HV106 in 0, 8, missing & not preschool) <=> HV107 <> notappl then
      e = errmsg(11005,i,HV106,HV107,preschool)
    endif;

    {* Complete/Incomplete education into grade *}
    x = HV109;
    box x => x;
      1,2 => 1;   {* Primary complete/Incomplete into primary *}
      3,4 => 2;   {* Secondary complete/Incomplete into secondary *}
        5 => 3;
          => x;
    endbox;
    if x <> HV106 then
      e = errmsg(11006,i,HV109,HV106)
    endif;

    {* Over schooling age member still at school *}
    if !(HV105 in 0:25) & HV110 <> 0 then
      e = errmsg(11008,i,HV105,HV106,HV110)
    endif;

    {* Member with no education still at school *}
    if (HV106 = 0 & HV107 = notappl) & HV110 <> 0 then    {* !!! Check for preschool *}
      e = errmsg(11016,i,HV105,HV106,HV110)
    endif;

    {* Parenthood and age of HH member *}
    {* No information for children under 18 *}
    if (HV105(i) in 0:17 &
       (HV111 = notappl | HV112 = notappl |
        HV113 = notappl | HV114 = notappl)) then
      e = errmsg(11009,i,HV105,HV111,HV112,HV113,HV114)
    endif;
    {* Information for adults 18 and older *}
    if (!(HV105 in 0:17) &
       (HV111 <> notappl | HV112 <> notappl |
        HV113 <> notappl | HV114 <> notappl)) then
      e = errmsg(11009,i,HV105,HV111,HV112,HV113,HV114)
    endif;

    {* Mother not alive but line number *}
    if (HV111 = 0 & HV112 <> 0) then
      e = errmsg(11010,i,HV111,HV112,HV113,HV114)
    endif;

    {* Father not alive but line number *}
    if (HV113 = 0 & HV114 <> 0) then
      e = errmsg(11010,i,HV111,HV112,HV113,HV114)
    endif;

    {* Parents' line number *}
    {* Mother *}
    if !special(HV112) & HV112 > HV009 then
      e = errmsg(11028,i,HV112,HV114,HV009)
    endif;
    {* Father *}
    if !special(HV114) & HV114 > HV009 then
      e = errmsg(11028,i,HV112,HV114,HV009)
    endif;

    {* Parents' gender *}
    {* Mother *}
    if !special(HV112) & HV112 <> 0 then
      x = HV104(HV112);
      if x <> 2 then
        e = errmsg(11011,i,2,HV112,"mother",x)
      endif;
    endif;
    {* Father *}
    if !special(HV114) & HV114 <> 0 then
      x = HV104(HV114);
      if x <> 1 then
        e = errmsg(11011,i,4,HV114,"father",x)
      endif;
    endif;

    {* HH marital status whenever asked *}
    if HV115 <> notappl then
      x = HV115;
      box x => x;
          0 => 0;
        1,2 => 1;       {* Married/Living together into in union *}
        3-5 => 2;       {* Widowed/Divorced/Not living together into formerly *}
            => x;
      endbox;
      {* Grouping of marital status into not married/married/formerly married *}
      if x <> HV116 then
        e = errmsg(11012,i,HV115,HV116)
      endif;
    endif;

    {* Members' eligibility *}
    {* Female *}
    if !HV020 then
      {* All women sample: De facto women 15-49 *}
      if !HV117 <=> (HV103 = 1 & HV104 = 2 & HV105 in 15:49) then
        e = errmsg(11013,i,HV117,HV103,HV104,HV105,HV116,HV020)
      endif;
    else
      {* Ever married sample: De jure women ever married, 15-49 *}
      if !HV117 <=> ( HV102 = 1 & HV104 = 2 & HV116 in 1:2 & HV105 in 15:49) then
        e = errmsg(11013,i,HV117,HV103,HV104,HV105,HV116,HV020)
      endif;
    endif;

    {* Male/Husband's survey *}

    if HV027 = 0 & HV118 then  {* Eligible men/husband for not selected HH *}
      e = errmsg(11031,i,HV118,HV027,mHV020);
    endif;

    if !mHV020 then
      {* All husband/men sample *}
      if HV027 = 1 then
        {* All men sample: De facto men 15-69 *}
        if !HV118 <=> (HV103 = 1 & HV104 = 1 & HV105 in minaim:maxaim) then
          e = errmsg(11014,i,HV118,3,HV103,HV104,HV105,HV116,HV020)
        endif;
      elseif HV027 = 2 then
        {* All husband sample: De facto men married, 15-69 *}
        if !HV118 <=> (HV103 = 1 & HV104 = 1 & HV116 = 1 & HV105 in minaim:maxaim) then
          e = errmsg(11014,i,HV118,3,HV103,HV104,HV105,HV116,HV020)
        endif;
      endif;

    else
    {* Ever married men/husband sample *}
      if HV027 = 1 then
        {* Ever married men sample: De jure men ever married, 15-59 *}
          if !HV118 <=> ( HV102 = 1 & HV104 = 1 & HV116 in 1:2 & HV105 in minaim:maxaim) then
          e = errmsg(11014,i,HV118,2,HV102,HV104,HV105,HV116,HV020)
        endif;
      elseif HV027 = 2 then
        {* De jure men married, 15-69 *}
        if !HV118 <=> ( HV102 = 1 & HV104 = 1 & HV116 = 1 & HV105 in minaim:maxaim) then
          e = errmsg(11014,i,HV118,2,HV102,HV104,HV105,HV116,HV020)
        endif;
      endif;
    endif;

    {* Eligibility of children under 5 for H/W and Hemoglobin testing *}
    if HV040 <> 0 & (!HV120 <=> (HV105 in 0:4 {& HV103 = 1})) then
      e = errmsg(11015,i,HV120,HV103,HV105)
    endif;

    {* Schooling *}
    {* Member attended school this year *}
    if HV121 <> 0 & HV106 = 0 & not preschool & HV107 = notappl then                    {!!}
      e = errmsg(11020,i,HV121,HV106,preschool,HV107)
    endif;

  //  preschool = (SH19A(i) = 0);
    {* Educational level this school year *}
    if (HV122 = 0 & not preschool) <=> HV121 <> 0 then                                                  {!!}
      e = errmsg(11021,i,HV122,HV121,preschool)
    endif;
    {* Grade of education this school year *}
    if (HV122 in 0, 8, missing) <=> HV123 <> notappl then
      e = errmsg(11022,i,HV122,HV123)
    endif;
    {* Member attended school last year *}
    if HV125 <> 0 & HV106 = 0 & not preschool then                                                             {!!}
      e = errmsg(11023,i,HV125,HV106)
    endif;

  {* Level of education previous year *}
    if (HV126 = 0 & not preschool) <=> HV125 <> 0 then
  e = errmsg(11024,i,HV126,HV125)
    endif;
  {* Grade of education previous year *}
  if (HV126 in 0, 8, missing {& not preschool}) <=> HV127 <> notappl then               {!!}
  e = errmsg(11025,i,HV126,HV127)
    endif;


    {*ART Check school attendance status - adjusted syntax - structure check *}
    x = default;
    if !(HV105 in 5:24) then
      x = notappl
        elseif HV106 in 0, missing then
        x = HV106
    else
      if HV121 = 0 & HV125 = 0 then
        x = 5
          elseif HV121 in 8,missing then
          x = HV121
          elseif HV121 = 0 & HV125 = 1 then            { dropout }
    x = 4
      elseif HV125 in 8,missing then
      x = HV125
      elseif HV125 = 0 then                        { entered school }
    x = 1
      elseif HV128 in 97,98,missing | HV124 in 97,98,missing then
      x = missing;
    elseif HV128 >= HV124 then
      x = 3                                      { repeating }
      else
        x = 2                                      { advanced }
    endif;
    endif;
    if x <> HV129 then
      e = errmsg(11026,i,HV105,HV106,HV129,x,HV121,HV124,HV125,HV128);
    endif;


    { Birth certificate asked for 0-4 }
    if HV140 = notappl <=> HV105 in 0:4 then
      e = errmsg(11053, i, HV140, HV105)
    endif;

  enddo;

{ ---------------------------------------------------------------------------- }
PROC RECH2_EDT
preproc
                                       {* Set unused variables to <> notappl !!}
  if special(HV217)  | special(HV218)  | HV219 = notappl | HV220 = notappl |
     HV226 = notappl | HV227 = notappl |
     special(HV270)  | special(HV271) |
     HML1A = missing
   then
     e = errmsg(12000,"RECH2");
     errmsg("not special: HV217=%d,HV218=%d,HV270=%d,HV271=%d",HV217,HV218,HV270,HV271);
     errmsg("not notappl: HV219=%d,HV220=%d,HV226=%d,HV227=%d",HV219,HV220,HV226,HV227);
     errmsg("not missing: HML1A=%d",HML1A);
  endif;

  { Source is bottled water }
  if (HV202 = notappl  & HV201  = 71) |
     (HV202 <> notappl & HV201 <> 71) then                                                 {!!! Adjust to used categories of water }
    e = errmsg(12001, HV202, HV201)
  endif;

  { Water source location }
  if !((HV201 in 11:13 | (HV201 = 71 & HV202 in 11:13)) <=> HV235 = notappl) then
    e = errmsg(12002, HV201, HV202, HV235)
  endif;

  { Distance to water }
  if !(HV235 = 3 & ( HV201 in 11: 13 | HV202 in 11: 13 )) <=> HV204 = notappl then                      {!!! Adjust to used categories of water }
    e = errmsg(12003, HV201, HV204)
  endif;
  { Water source outside the dwelling }
  if HV235 in 3, missing <=> HV204 = 996 then
    e = errmsg(12004, HV204, HV235)
  endif;


  { Both questions skipped }
  if HV204 = 996 <=> HV236 <> notappl then
    e = errmsg(12006, HV204, HV236)
  endif;
  { Toilet facility }
  if HV205 = 31 <=> HV225 <> notappl then
    e = errmsg(12008, HV205, HV225)
  endif;
  { Households using the toilet facility }
  if (HV205 = 31 | HV225 <> 1) <=> HV238 <> notappl then
    e = errmsg(12009, HV205, HV225, HV238)
  endif;
  { Type of stove }
  if HV226 in 1:4, 95 <=> HV239 <> notappl then                           {!!! Adjust to used categories of stove }
    e = errmsg(12010, HV226, HV239)
  endif;
  { Type of stove }
  if HV239 <> notappl & (HV239 in 1,2 <=> HV240 = notappl) then
    e = errmsg(12011, HV239, HV240)
  endif;
  { Location of cooking }
  if HV226 = 95 <=> HV241 <> notappl then                                                                                                 {!!! Adjust to used categories of water }
    e = errmsg(12012, HV226, HV241)
  endif;
  { Kitchen }
  if HV241 <> notappl & (HV241 = 1 <=> HV242 = notappl) then
    e = errmsg(12013, HV241, HV242)
  endif;
  { Agricultural land }
  if HV244 = 1 <=> HV245 = notappl then
    e = errmsg(12014, HV244, HV245)
  endif;
  { Animals owned }
  if HV246 = 1 & HV246A = 0 & HV246B = 0 & HV246C = 0 & HV246D = 0 & HV246E = 0 & HV246F = 0
               & HV246G = 0               {!!! Comment out if unused }
               & HV246H = 0               {!!! Comment out if unused }
               & HV246I = 0               {!!! Comment out if unused }
               & HV246J = 0               {!!! Comment out if unused }
               & HV246K = 0 then  {!!! Comment out if unused }
    e = errmsg(12015, HV246, HV246A, HV246B, HV246C, HV246D, HV246E, HV246F, HV246G, HV246H, HV246I, HV246J, HV246K)
  endif;

  { Occurrences of Malaria module }
  if HML1A <> soccurs( RECHML ) then
    e = errmsg(12017, HML1A, soccurs( RECHML ))
  endif;

  {* Relationship structure *}
  xsex = default;
  x    = default;
  i = 1;
  while i <= soccurs(RECH1) do
    if (special(HV105(i)) | HV105(i) > 14) & HV102(i) = 1 then
       {* De jure member 15 and over including the DKs age *}
       if HV101(i) = 11 | HV101(i) = 12 | HV101(i) = 98 | HV101(i) = missing then
          x = 5;     { Unrelated member }
       else
          box x => x;
        default => 1;
              1 => 2;
            2,3 => 4;
              4 => 4;
              5 => 5;
          endbox;
          if x = 1 then
             {* One adult *}
             xsex = HV104(i);
          elseif x = 2 & HV104(i) = xsex then
             {* Two adults same sex *}
             x = 3
          endif
       endif
    endif;
    i = i + 1
  enddo;
  {* Check the HH structure *}
  if x <> HV217 then
    if x = default then
      {* No de jure adults 15 and over including missing ages *}
      kount(12) = kount(12)+1;
    else
      {* HH structure incorrect *}
      e = errmsg(12025,HV217,x)
    endif
  endif;

  {* Head of HH information *}
  {* Head of HH not in first line *}
  {? why check when there is variable indicating line number of head of household
  if HV218 <> 1 then
    e = errmsg(12023,HV218)
  endif;
  ?}

  if HV219 <> HV104(HV218) |       {* Sex of head of HH not sex of line number *}
     HV220 <> HV105(HV218)         {* Age of head of HH not age of line number *}
   then
    e = errmsg(12024,HV218,HV219,HV220,HV104(HV218),HV105(HV218))
  endif;

  {* Children under 5 slept under a bednet last night *}
  x = count(RECH1_EDT where HV103 = 1 & HV105 < 5);
  if HV228 = notappl <=> x then
    e = errmsg(12026,HV228,x)
  endif;
  if HV228 <> notappl & (HV228 = 3 <=> HV227 = 1) then
    e = errmsg(12027,HV228,HV227)
  endif;

{ ---------------------------------------------------------------------------- }
PROC RECH5_EDT
preproc

  if anthroHH then

    for i in RECH5_EDT do
                                         {* Set unused variables to <> notappl !!}
      if special(HA0)       | special(HA1)   |
         HA13 = notappl |
         special(HA32)  | HA33 = missing |
         { HA51 = missing | CONSIS6 -> I believe that HA51 can be missing BERT!}
         HA60 = missing |
         HA65 = missing | HA66 = notappl then
        e = errmsg(12101,i,"RECH5");
        errmsg("Not special: HA0=%d,HA1=%d,HA32=%d",HA0,HA1,HA32);
        errmsg("Not notappl: HA13=%d,HA33=%d,HA35=%d,HA66=%d",HA13,HA33,HA35,HA66);
        errmsg("Not missing: HA33=%d,HA51=%d,HA60=%d,HA65=%d",HA33,HA51,HA60,HA65);
      endif;

      if HA0 > HV009 then
        e = errmsg(15200,i,HA0,HV009)
      else
        j = HA0;
        if !(HV105(j) in 15:49) then
              e = errmsg(15203,i,j,j,HV105(j))
        else
              {* Age of the woman in the H/W differs from her     age in the HH *}
              if HA1 <> HV105(j) then
                kount(15) = kount(15)+1;
            if HA1 < HV105(j)-1 | HA1 > HV105(j)+1 then
                  {* Age of the woman in the H/W differs from her age in the HH by more than 1 year. *}
                  e =     errmsg(15204,i,HA1,j,HV105(j))
                endif;
              endif;
        endif;
      endif;
      if HA2 = notappl | HA3 = notappl then
        e = errmsg(15201,i,HA2,HA3)
      endif;
      {* Age of women in years is different from her age taken from the date of interview and the date of birth in CMC *}
      if HA1 <> int((HV008-HA32)/12) then
        e = errmsg(15207,i,HA1,HV008,HA32,int((HV008-HA32)/12))
      endif;
      {* Result of the respondent do not match the Weight/Height *}
      {* Data Entry allows missings for either weight or height:??*}
      { OLD if HA13     = 0 <=> (HA2 = missing | HA3 = missing) then }
      if (HA13 = 0 & !(HA2 in 0:4000 & HA3 in 0:2200)) |
         (HA13 = 3 & (HA2 <> 9994 & HA3 <> 9994)) |
         (HA13 = 4 & (HA2 <> 9995 & HA3 <> 9995)) |
         (HA13 = 6 & !(HA2 in 9994:9996 & HA3 in 9994:9996)) |
         (HA13 = missing & (HA2 = missing | HA3 = missing)) then
        e = errmsg(15202,i,HA13,HA2,HA3)
      endif;

      {* Anemia testing Module *}

      {* No anemia testing but information present *}
      if !HV042 &
          (HA50 <> notappl | HA51 <> notappl | HA52 <> notappl |
                   HA53 <> notappl | HA54 <> notappl | HA55 <> notappl |
                   HA56 <> notappl | HA57 <> notappl | HA58 <> notappl )  then
        e = errmsg(15223,i,HA50,HA51,HA52,HA53,HA54,HA55,HA56,HA57,HA58,HV042)
      endif;

      {* Anemia testing *}
      if HV042 then
                                                          {??}
        if (HA50 = notappl | HA52 = notappl{| HA55  = notappl}) then
              e = errmsg(15224,i,HA50,HA52,HA55,HV042)
        endif;
        {* Caretaker for woman *}
        if HA50 = 1 & HA60 = 1 <=> HA51 = notappl then
          e = errmsg(15225,i,HA50,HA51)
        endif;
        {* Line of caretaker is outside the range *}
        if HA51 <> notappl & HA51 > HV009 then
              e = errmsg(15230,i,HA51,HV009)
        endif;
        {* Caretaker is the woman *}
        if HA51 <> notappl & HA51 = HA0 then
              e = errmsg(15231,i,HA51,HA0)
        endif;

        {* Consent granted but no information on anemia *}
        if (HA52 = 1 &
           (HA53 = notappl | HA54 = notappl | HA56 = notappl | HA57 = notappl | HA55 = notappl)) then
               e = errmsg(15226,i,HA52,HA53,HA54,HA55,HA56,HA57);
        endif;

        {* Consent refused but information on anemia *}                                                       {??}
        if (HA52 <> 1 &
           { (HA53 <> missing | HA54 <> missing | HA56 <> missing | HA57 <> missing)) then    BERT!!! }
             (HA53 in 0:900   | HA56 <> missing | HA57 <> missing)) then
              e = errmsg(15232,i,HA50,HA52,HA53,HA54,HA55,HA56,HA57);
        endif;

        { Consent not granted but result different from refused }
        { if HA52 <> 1 <=> HA55 <> 4 then  OLD BERT!!! }
        if (HA52 in 2,3 <=>  HA55 <> 4) then
              e = errmsg(15233,i,HA52,HA55);
        endif;

        if HA52 = 1 then                              { Hemo consent granted }
          {* Result of woman's hemoglobulin inconsistent with level informations *}
          if HA55 = 0 & HA53 in 994,995,996,missing |
             HA55 = 3 & HA53 <> 994 |
             HA55 = 4 & HA53 <> 995 |
             HA55 = 6 & HA53 <> 996 |
             HA55 = missing & HA52 <> missing then
             errmsg(15228,i,HA53,HA55);
          endif;
          {* No hemo measurement then adjusted hemo levels should be missing *}
          if (HA55  = 0 & ({HA53  = missing |} HA56  = missing | HA57  = missing)) |
             (HA55 <> 0 & ({HA53 <> missing |} HA56 <> missing | HA57 <> missing)) then
                e = errmsg(15227,i,HA53,HA55,HA56,HA57,HV040);
              endif;
        endif;
        if HA55 = 0 then
          { Altitude adjustment }
          alt  = (HV040 / 1000) * 3.3;
          adjalt   = -0.032 * alt + 0.022 * alt * alt;
          if adjalt < 0 | HV040 < 1000 then adjalt = 0 endif;
          { smoking adjustment }
          box HA35  => adjsmk;
            notappl => 0;      { no smoking }
              0-9   => 0;      { no smoking or < 10 cigarettes }
             10-19  => 0.3;
             20-39  => 0.5;
             40-80  => 0.7;    { 80 means 80+ cigarttes }
                    => 0.3;    { missing, DK or smokes pipe, cigars }
          endbox;
          { Hemoglobin level adjusted by altitude and smoking (g/dl - 1 decimal) }
          adjanem  = int( ( HA53(i) / 10 - adjalt - adjsmk ) * 10 + 0.5 );
          preg = HA54;
          box adjanem : preg => anemlev;
              missing :          => missing;
              notappl :          => notappl;
                 <70  :          => 1;
                <100  :          => 2;
                <110  :       1  => 3;
                <120  :<>     1  => 3;
                          :      => 4;
          endbox;
          if adjanem <> HA56 | anemlev <> HA57 then
            e = errmsg(15229,i,HA53,HA54,HA56,HA57,HV040);
            errmsg("adjanem = %d <> HA56 = %d or anemlev = %d <> HA57 = %d", adjanem, HA56, anemlev, HA57);
          endif;
        endif;

      endif;
      {* End Anemia testing Module *}

      { HIV testing }
      { Consent not granted }

      if !( HA61 <> 1 <=> pos(HA62, "99992999949999599996?    ") ) then
        e = errmsg(15241, HA61, HA62)
      endif;
      if !( HA61 = 1 <=> HA63 in 1,4:6 ) then
        e = errmsg(15242, HA61, HA63)
      endif;
      if !( HA61 = 1 & HA63 = 1 & HA65 = 1 & HV103(HA0) = 1 <=> HA69 <> 0 ) then
        e = errmsg(15243, HA61, HA62, HA63, HA65, HV103(HA0), HA69)
      endif;
      { Education }
      if HA66 <> HV106(HA0) then
        kount(71) = kount(71) + 1;
      endif;
      if HA67 <> HV107(HA0) then
        kount(72) = kount(72) + 1;
      endif;
      { End HIV testing }

    enddo;
  endif;

{ ---------------------------------------------------------------------------- }
PROC RECH6_EDT
preproc

  if anthroHH then
    for i in RECH6_EDT do
                                              {* Set unused variables to <> notappl !!}
      if special(HC0)  | special(HC1) |
        HC13 = notappl |
        HC16 = notappl | HC17 = missing | HC18 = missing | HC19 = missing |
        HC27 = notappl |
        special(HC32)  | HC33 = missing |
        { HC51-HC58 generally NA for children 0-5 months }
        HC51 = missing |
        HC57 = notappl | HC58 = notappl |
        HC60 = missing | HC61 = notappl |
        HC64 = notappl
      then
        e = errmsg(12101,i,"RECH6");
        errmsg("Not special: HC0=%d,HC1=%d,HC32=%d",HC0,HC1,HC32);
        errmsg("Not notappl: HC13=%d,HC16=%d,HC27=%d,HC57=%d,HC58=%d,HC61=%d,HC64=%d",HC13,HC16,HC27,HC57,HC58,HC61,HC64);
        errmsg("Not missing: HC17=%d,HC18=%d,HC19=%d,HC33=%d,HC51=%d,HC60=%d",HC17,HC18,HC19,HC33,HC51,HC60);
      endif;

      if HC0 > HV009 then
       {* Index to the HH schedule of the child is out of range *}
       e = errmsg(16200,i,HC0,HV009)
      else
        j = HC0;
        if HC1 in 0:59 then
          if HV105(j) > 5 then
            {* Child not eligibile included in the H/W section *}
            e = errmsg(16203,i,j,j,HV105(j))
          else
            {* Age of child in months in the H/W section differs
               from the age of the child in the HH schedule. *}
            x = HV105(j)*12;
            if HC1 < x | HC1(i) > x+11 then
              kount(16) = kount(16)+1;
              e = errmsg(16204,i,HC1,j,HV105(j));
            endif;
          endif;
        endif;
      endif;
      {* Age of child in months is different from his/her age taken from the date of interview and the date of birth in CMC *}
      if HC1 <> int((HV008A-HC32A)/DaysMonth) then
        e = errmsg(16207,i,HC1,HV008A,HC32A,int((HV008A-HC32A)/DaysMonth))
      endif;

      {*ART Weight/Height/Lying or standing is not given - structure check *}
      if (HC2 = notappl  & HC3 <> notappl) |
         (HC2 <> notappl & HC3 =  notappl) |
         { (!HC2 in missing,notappl & !HC3 in missing,notappl & HC15 = notappl) then OLD CONSIS6 BERT!!! }
         (!HC3 in 9994,9995,9996,missing,notappl & HC15 = notappl) then
        e = errmsg(16201,i,HC2,HC3,HC15)
      endif;

      {* Result of the child do not match the Height/Weight measurements *}{??}
      {* to keep instead of splitting the test                                  ??
      if (HC13  = 0 & (HC2  = missing | HC3  = missing | HC15  = missing)) |
         (HC13 <> 0 & (HC2 <> missing | HC3 <> missing | HC15 <> missing)) then
        e = errmsg(16202,i,HC13,HC2,HC3,HC15)
      endif;
      *}

      if (HC13  = 0 & (HC2 in 9994,9995,9996,missing | HC3 in 9994,9995,9996,missing)) |
         (HC13 <> 0 & (!HC2 in 9994,9995,9996,missing & !HC3 in 9994,9995,9996,missing)) then
        e = errmsg(16202,i,HC13,HC2,HC3)
      endif;

      {* Height of the child do not match the Lying or standing information *}
      if HC3 in 9994,9995,9996,missing <=> HC15 <> 0 then
        e = errmsg(16230,i,HC3,HC15)
      endif;

      {* Check H/W measurements against the Percentiles/S.D./% ref. medians *}
      if ((validresp(HC2) & validresp(HC3) & HC33  = 1) &
          (HC4  = missing | HC5  = missing | HC6  = missing |
           HC7  = missing | HC8  = missing | HC9  = missing))
         |
         ((HC2  = missing | HC3  = missing | HC33 <> 1) &
          (HC4 <> missing | HC5 <> missing | HC6 <> missing |
          HC7 <> missing | HC8 <> missing | HC9 <> missing))
         |
         ((validresp(HC2) & validresp(HC3) ) &
          (HC10  = missing | HC11  = missing | HC12  = missing))
         |
         ((HC2   = missing & HC3  <> missing) &
          (HC10 <> missing | HC11 <> missing | HC12 <> missing))
        then
        e = errmsg(16205,i,HC2,HC3,HC33,HC4,HC5,HC6,
                           HC7,HC8,HC9,HC10,HC11,HC12)
      endif;
      { Warning may generate many messages }
      if (HC1 < 24 & HC15 = 2) | (HC1 >= 24 & HC15 = 1) then
      {*e = errmsg(16206,i,HC1,HC15);{: No instruction given }          ??}
        kount(6) = kount(6) + 1
      endif;

      {* Anemia *}
      {* Anemia selection against caretaker, consent and result of measurement *}
      { ART this should exclude children 0-5 months! CONSIS6 BERT!!! }
    if HC1 > 5 then    { 6+ months }
      if ( HV042 & (HC51  = notappl & HC52  = notappl & HC55  = missing)) |
         (!HV042 & (HC51 <> notappl | HC52 <> notappl | HC55 <> notappl)) then
        e = errmsg(16224,i,HC51,HC52,HC55,HV042)
      endif;
    else               { 0 - 5 months }
      if ( HV042 & (HC51 <> notappl & HC52 <> notappl & HC55 <> missing)) |
         (!HV042 & (HC51 <> notappl | HC52 <> notappl | HC55 <> notappl)) then
        e = errmsg(16224,i,HC51,HC52,HC55,HV042)
      endif;
    endif;
      {* Age under 5 months *}
      if (HC1 <= 5) <=> HC51 <> notappl then
        e = errmsg(16232,i,HC1,HC51)
      endif;

      {* Validity of caretaker's line number *}
      {* Greater than the number of HH members *}
      if (HC51 <> notappl) & (HC51 > HV009) then
        e = errmsg(16225,i,HC51,HV009)
      endif;
      {* Child is his/her own catetaker *}
      if (HC51 <> notappl) & (HC51 = HC0) then
        e = errmsg(16231,i,HC51,HC0)
      endif;
      {* Consent granted but anemia test not given *}
      if HC1 in 6:59 then
        if HC52 = 1 <=> HC53 in 994,995,996,missing |
           HC52 = 1 <=> HC56  = missing |
           HC52 = 1 <=> HC57  = missing then
          e = errmsg(16226,i,HC52,HC53,HC55,HC56,HC57);
        endif;
        if HC52 = 1 then {* Consent granted *}
          {* Result of child's hemoglobin inconsistent with levels *}
          if HC55 = 0 <=> HC53 in 994,995,996,missing |
             HC55 = 0 <=> HC56  = missing |
             HC55 = 0 <=> HC57  = missing then
            e = errmsg(16227,i,HC53,HC55,HC56,HC57,HV040);
          endif;
        endif;
      endif;
      if HC55 = 0 then
        alt    = (HV040 / 1000) * 3.3;
        adjalt = (-0.032 * alt) + (0.022 * alt * alt);
        { if adjalt < 0 then adjalt = 0 endif; }
        if adjalt < 0 | HV040 < 1000 then adjalt = 0 endif;

        { adjanem  = ( HC53(i) / 10 - adjalt ) * 10; }
        adjanem  = int( ( HC53(i) / 10 - adjalt ) * 10 + 0.5 );
        box adjanem => anemlev;
            missing => missing;
            notappl => notappl;
               <70  => 1;
              <100  => 2;
              <110  => 3;
                    => 4;
        endbox;
        { adjanem = int(adjanem+0.5); }
        if adjanem <> HC56 | anemlev <> HC57 then
          e = errmsg(16229,i,HC53,HC56, adjanem, HC57, anemlev, HV040);
        endif;
      endif;

    enddo;

  endif;

{ ---------------------------------------------------------------------------- }
PROC RECHMA_EDT
preproc

  if anthroHH then

    for i in RECHMA_EDT do
                                                                                          {* Set unused variables to <> notappl !!}
      if special(HB0)       | special(HB1)   |
         HB13 = notappl |
         special(HB32)  | HB33 = missing |
         HB35 = notappl |
         HB51 = missing |
         HB60 = missing |
         HB65 = missing | HB66 = notappl then
        e = errmsg(12101,i,"RECHMA");
        errmsg('Not special: HB0=%d,HB1=%d,HB32=%d',HB0,HB1,HB32);
        errmsg("Not notappl: HB13=%d,HB33=%d,HB35=%d,HB66=%d",HB13,HB33,HB35,HB66);
        errmsg("Not missing: HB33=%d,HB51=%d,HB60=%d,HB65=%d",HB33,HB51,HB60,HB65);
      endif;

      if HB0 > HV009 then
        e = errmsg(17200,i,HB0,HV009)
      else
        j = HB0;
        if !(HV105(j) in minaim:maxaim) then
              e = errmsg(17203,i,j,j,HV105(j))
        else
              {* Age of the man in the H/W differs from his age in the HH *}
              if HB1 <> HV105(j) then
                kount(17) = kount(17) + 1;
            if HB1 < HV105(j)-1 | HB1 > HV105(j)+1 then
                  {* Age of the man in the H/W differs from his age in the HH by more than 1 year. *}
                  e =     errmsg(17204,i,HB1,j,HV105(j))
                endif;
              endif;
        endif;
      endif;
      if HB2 = notappl | HB3 = notappl then
        e = errmsg(17201,i,HB2,HB3)
      endif;

      {* Age of man in years is different from his age taken from the date of interview and the date of birth in CMC *}
      if HB1 <> int((HV008-HB32)/12) then
        e = errmsg(17207,i,HB1,HV008,HB32,int((HV008-HB32)/12))
      endif;

      {* Result of the respondent do not match the Weight/Height *}
      {* Data Entry allows missings for either weight or height:??*}
      if HB13     = 0 <=> (HB2 in 9994:9996,missing | HB3 in 9994:9996,missing) then
        e = errmsg(17202,i,HB13,HB2,HB3)
      endif;

      {* Anemia testing Module *}

      {* No anemia testing but information present *}
      if !HV042 &
          (HB50 <> notappl | HB51 <> notappl | HB52 <> notappl |
                   HB53 <> notappl |
                   HB55 <> notappl | HB56 <> notappl | HB57 <> notappl |
                   HB58 <> notappl) then
        e = errmsg(17223,i,HB50,HB51,HB52,HB53,HB55,HB56,HB57,HB58,HV042)
      endif;

      {* Anemia testing *}
      if HV042 then

        {* HH eligible for anemia testing => these variables should have values *}
        if (HB50 = notappl | HB52 = notappl | HB55 = notappl) then
              e = errmsg(17224,i,HB50,HB52,HB55,HV042)
        endif;
        {* Caretaker for man: 15-17 and never in union => linenumber of caretaker should be given *}
        if (HB50 = 1 & HB60 = 1) <=> HB51 = notappl then
          e = errmsg(17225,i,HB50,HB51)
        endif;
        {* Line of caretaker is outside the range *}
        if HB51 <> notappl & HB51 > HV009 then
              e = errmsg(17230,i,HB51,HV009)
        endif;
        {* Caretaker is the man *}
        if HB51 <> notappl & HB51 = HB0 then
              e = errmsg(17231,i,HB51,HB0)
        endif;

        {* Consent granted but no information on anemia *}
        if (HB52 = 1 &
           (HB53 = notappl | HB56 = notappl | HB57 = notappl | HB55 = notappl)) then
          e = errmsg(17226,i,HB52,HB53,HB55,HB56,HB57);
        endif;

        {* Consent refused but information on anemia *}                                                       {??}
        if (HB52 <> 1 &
            (HB53 <> missing | HB56 <> missing | HB57 <> missing )) then
        e = errmsg(17232,i,HB50,HB52,HB53,HB55,HB56,HB57);
        endif;

        if HB52 = 1 then
              {* Result of man's hemoglobulin inconsistent with level informations *}
              if (HB55  = 0 & (HB53  = missing | HB56  = missing | HB57  = missing)) |
                 (HB55 <> 0 & (HB53 <> missing | HB56 <> missing | HB57 <> missing)) then
                e = errmsg(17227,i,HB53,HB55,HB56,HB57,HV040);
              endif;
        endif;

        if HB55 = 0 then
          { Altitude adjustment }
          alt = (HV040 / 1000) * 3.3;
              adjalt = (-0.032 * alt) + (0.022 * alt * alt);
          if adjalt < 0 | HV040 < 1000 | HV040 = missing then adjalt = 0 endif;
          { smoking adjustment }
          { BERT: added smoking adjustment }
          box HB35  => adjsmk;
            notappl => 0;
              0-9   => 0;      { no smoking or < 10 cigarettes }
             10-19  => 0.3;
             20-39  => 0.5;
             40-80  => 0.7;    { 80 means 80+ cigarttes }
                    => 0.3;    { missing, DK or smokes pipe, cigars }
          endbox;

          { Hemoglobulin level adjusted by altitude and smoking (g/dl - 1 decimal) }
          adjanem  = int ( ( HB53 / 10 - adjalt - adjsmk ) * 10 + 0.5 );
          box adjanem => anemlev;
              missing => missing;
              notappl => notappl;
                 <70  => 1;
                <100  => 2;
                <120  => 3;
                      => 4;
            endbox;
            adjanem     = int(adjanem+0.5);
            if adjanem <> HB56 | anemlev <> HB57 then
              e = errmsg(17229,i,HB53,HB56,HB57,HV040);
              errmsg("adjanem = %d <> HB56 = %d or anemlev = %d <> HB57 = %d", adjanem, HB56, anemlev, HB57);
            endif;
        endif;

      endif;
      {* End Anemia testing Module *}

      { HIV testing }
      { Consent not granted }
      if HB61 <> 1 <=> HB62 <> "" then
        e = errmsg(17401, HB61, HB62)
      endif;
      if HB61 <> 1 <=> HB63 = 1 then
        e = errmsg(17402, HB61, HB63)
      endif;
      if !( HB61 = 1 & HB63 = 1 & HB65 = 1 & HV103(HB0) = 1 <=> HB69 <> 0 ) then
        e = errmsg(17403, HB61, HB62, HB63, HB65, HV103(HB0), HB69)
      endif;
      { Education }
      if HB66 <> HV106(HB0) then
        kount(75) = kount(75) + 1;
      endif;
      if HB67 <> HV107(HB0) then
        kount(76) = kount(76) + 1;
      endif;
      { End HIV testing }

    enddo;
  endif;

{ ---------------------------------------------------------------------------- }
PROC RECHML_EDT
preproc

  for i in RECHML_EDT do
                                                                { Set unused variables to <> notappl !!}
    if special(HMLIDX) |
       HML6  <> notappl |
       HML21 = notappl
     then
      e = errmsg(12101,i,"RECHML");
      errmsg("Not special: HMLIDX=%d",HMLIDX);
      errmsg("Not notappl: HML6=%d,HML21=%d",HML6,HML21);
    endif;

    { check index }
    { Bert: Not more nets than household members? - check makes little sense
    if HMLIDX > soccurs(RECH1) then
      e = errmsg(18101, HMLIDX, soccurs(RECH1))
    endif;
    }

    { Net treated at home }
        {!!! Check the questionnaire }
    if HML6 = 3 <=> HML9 = notappl then
      e = errmsg(18102, HML6, HML9)
    endif;

    { Permanent nets }
    { Bert: was HML5 = notappl }
    if HML7 in 10:19 <=> HML5 <> notappl then
      e = errmsg(18103, HML7, HML5)
    endif;
    if HML7 in 10:19 <=> HML8 <> 2 then
      e = errmsg(18104, HML7, HML8)
    endif;
    { Bert: was HML9 = notappl }
    if HML7 in 10:19 <=> HML9 <> notappl then
      e = errmsg(18105, HML7, HML9)
    endif;
    { Pretreated nets }
    { Bert: was HML5 = notappl }
    if HML7 in 20:29 <=> HML5 <> notappl then
      e = errmsg(18106, HML7, HML5)
    endif;

    { Number of persons slept under a bednet }
    if HML11 > HV009 then
      e = errmsg(18107, HML1, HV009)
    endif;

    { Someone in the household slept under a bednet }
    if HML21 = 1 <=> (HMLA = notappl & HMLB = notappl & HMLC = notappl & HMLD = notappl) then
      e = errmsg(18108, HML21, HMLA, HMLB, HMLC, HMLD, HMLE)
    endif;
    if (!special(HMLA) & HMLA > HV009) |
       (!special(HMLB) & HMLB > HV009) |
       (!special(HMLC) & HMLC > HV009) |
       (!special(HMLD) & HMLD > HV009) |
       (!special(HMLE) & HMLE > HV009) then
      e = errmsg(18109, HMLA, HMLB, HMLC, HMLD, HMLE, HV009)
    endif;

  enddo;

PROC RECHMH_EDT
preproc

  for i in RECHMH_EDT do
                                                                { Set unused variables to <> notappl !!}
    if special(HMHIDX) |
       HML13 = missing | HML14 = missing | HML15 = missing | HML16 = notappl | HML17 = missing |
       special(HML19)
     then
      e = errmsg(12101,i,"RECHMH");
      errmsg("Not special: HMHIDX=%d,HML19=%d",HMHIDX,HML19);
      errmsg("Not notappl: HML16=%d",HML16);
      errmsg("Not missing: HML13=%d,HML14=%d,HML15=%d,HML17=%d",HML13,HML14,HML15,HML17);
    endif;

    { Check index }
    if HMHIDX > soccurs(RECH1) then
      e = errmsg(18201, HMHIDX, soccurs(RECH1))
    endif;

    { Net number a member slept under }
    { Bert: changed to HML13, HML14 and HML15 from HML14, HML15 and HML16
            and compared to number of nets not number of household members }
    if HML13 > soccurs(RECHML) |
       HML14 > soccurs(RECHML) |
       HML15 > soccurs(RECHML) then
      e = errmsg(18202, HML13, HML14, HML15, soccurs(RECHML))
    endif;

    { Slept under a treated bednet }
    { Bert: changed this
    treated = HML6(HML13) in 2:6 | HML6(HML14) in 2:6 | HML6(HML15) in 2:6;
    }
    { NOUR: HML12 based on HML10 in recode application, don't know whether this is correct,
      if so label should then refer to ITN net }
    treated = (HML13 in 1:7 & HML10(HML13) = 1) |
              (HML14 in 1:7 & HML10(HML14) = 1) |
              (HML15 in 1:7 & HML10(HML15) = 1);
    if HML12 in 1:2 <=> !treated then
      e = errmsg(18203, HML12, treated)
    endif;

    { Age }
    if HML16 <> HV105(HMHIDX) then
      e = errmsg(18204, HML16, HV105(HMHIDX))
    endif;

    { Slept under an ever treated bednet }
    { NOUR: HML12 in 1:2 is really ITN treated and untreated nets in HH, whereas HML19 = ever treated;
            thus if HML12 not in 1:2 "no ITN net in HH" then HML19 not necessarily 0
    if !HML19 <=> HML12 in 1:2 then }
    if !HML19 and HML12 in 1:2 then
      e = errmsg(18205, HML12, HML19)
    endif;

  enddo;

PROC WOMAN
preproc

  childm = 0;      {* Find first child living with mother *}
  k      = 1;
  while k <= soccurs(REC21) do
    if B9(k) = 0 & B19(k) < 36 then
      childm = k;
      k = soccurs(REC21);
    endif;
    k = k + 1;
  enddo;

  {* Check that all children in the reproductive history under Hmax age
     are present in the Maternity section.

     ADDKID is needed in case there is a discrepancy between the number of
            children under Hmax age in REC21 and children in REC41, ...
  *}
  i = 1;
  while i <= sOccurs(REC21) do
    if B19(i) <= Hmax then
      found = 0;
      j = 1;
      while j <= sOccurs(REC41) do
        if i = MIDX(j) then
          found = j;
          j = sOccurs(REC41);
        endif;
        j = j + 1;
      enddo;
      if !found then
      {* Child missing from the maternity section *}
        e = errmsg(4101,i,V008,B19(i))
      endif
    endif;
    i = i + 1;
  enddo;

  {* Check if all children in the reproductive history under Hmax age are present
     in the Health section.

     ADDKID is needed in case there is a discrepancy between the number of
            children under Hmax age in REC21 and children in REC43.
  *}
  i = 1;
  while i <= sOccurs(REC21) do
    if B19(i) <= Vmax then
      found = 0;
      j = 1;
      while j <= sOccurs(REC43) do
        if i = HIDX(j) then
          found = j;
          j = sOccurs(REC43);
        endif;
        j = j + 1;
      enddo;
      if !found then
      {* Child missing from the vaccination section *}
        e = errmsg(4301,i,V008,B19(i),B5(i))
      endif;
    endif;
    i = i + 1;
  enddo;

  if anthro then  {?? needs checking whether anthro should be taken into account }
                  {?? included in the recode whether there is H/W or not.        }
    {* Check if all children in the reproductive history under HWmax age are present
       in the Height and weight section.

     ADDKID is needed in case there is a discrepancy between the number of
            children under HWmax age in REC21 and children in REC44.
    *}

    i = 1;
    while i <= sOccurs(REC21) do
      if B19(i) <= HWmax then
        found = 0;
        j = 1;
        while j <= sOccurs(REC44) do
          if i = HWIDX(j) then
            found = j;
            j = sOccurs(REC44);
          endif;
          j = j + 1;
        enddo;
        if !found then
        {* Child missing from the height and weight section *}
          e = errmsg(4401,i,V008,B19(i),B5(i))
        endif
      endif;
      i = i + 1;
    enddo;

  endif;

  { ---------------------------------------------------------------------------- }
  
postproc

  { Check line numbers }
  if hwqline(V003) then
    if hwqtype(V003) <> 3 then
      e = errmsg(10006,V003,hwqline(V003));
    endif
  endif;
  hwqtype (V003) = 2;
  hwqline (V003) = V003;
  i = 1;
  while i <= V224 do
    if !special(B16(i)) & B16(i) then
      if hwqline(B16(i)) then
        if hwqtype(B16(i)) = 3 then
          e = errmsg(10007,i,V003,hwqline(B16(i)));
        endif;
      else
        hwqtype (B16(i)) = 3;
        hwqline (B16(i)) = V003;
      endif;
    endif;
    i = i + 1
  enddo;

{ ---------------------------------------------------------------------------- }
PROC REC01_EDT
preproc

  kount(96) = kount(96) + 1;
  if V015 = 1 then
    kount(98) = kount(98) + 1;
  endif;
                                                                  { Set unused variables to <> notappl !!}
  if !length(strip(V000)) |
     special(V001) | special(V002) | special(V003) | special(V004) |
     special(V005) | special(V006) | special(V007) | special(V008) |
     V009 = missing| V010 = missing| V011 = missing| V012 = missing|
     V013 = missing| V014 = missing| V015 = notappl| special(V016) |
     V017 = missing| special(V018) | special(V019) | V019A= missing|
     special(V020) | special(V021) | special(V022) | special(V023) |
     special(V024) | special(V025) | V026 = notappl| special(V027) |
     V028 = notappl| V029 = notappl| V030 = notappl| V031 = notappl|
     V032 = notappl| V040 = missing| V042 = missing|
     V044 = missing
    then
    e = errmsg(12000,"REC01");
    errmsg("not special: V001=%d,V002=%d,V003=%d,V004=%d,V005=%d,V006=%d,V007=%d,V008=%d",V001,V002,v003,V004,V005,V006,v007,V008);
    errmsg("not special: V016=%d,V018=%d,V019=%d,V020=%d,V021=%d,V022=%d,V023=%d,V024=%d",V016,V018,V019,V020,V021,V022,V023,V024);
    errmsg("not special: V025=%d,V027=%d",V025,V027);
    errmsg("not missing: V009=%d,V010=%d,V011=%d,V012=%d,V013=%d,V014=%d,V017=%d,V019A=%d",V009,V010,V011,V012,V013,V014,V017,V019A);
    errmsg("not missing: V040=%d,V042=%d,V044=%d",V040,V042,V044);
    errmsg("not notappl: V015=%d,V026=%d,V028=%d,V029=%d,V030=%d,V031=%d,V032=%d",V015,V026,V028,V029,V030,V031,V032);
  endif;

  {* Ever married sample vs de jure/de facto sample *}
  if HV020 & HV102(V003) <> 1 then
    e = errmsg(0050,HV020,HV102(V003));
  endif;
  if !HV020 & HV103(V003) <> 1 then
    e = errmsg(0051,HV020,HV103(V003));
  endif;

  {* Check variable assignment is correct, variables are the same *}
  if HV000 <> V000 |
     HV001 <> V001 |
     HV002 <> V002 |
     HV024 <> V024 |
     HV025 <> V025 |
     HV026 <> V026 |
     HV040 <> V040 |
     HV042 <> V042 then
    e = errmsg(10000,HV000,V000,HV001,V001,HV002,V002,HV024,V024,HV025,V025);
    e = errmsg(10001,HV026,V026,HV040,V040,HV042,V042);
  endif;
  {*!! recode variables, should be the same *}
  if HV004 <> V004 |
     HV020 <> V020 |
     HV021 <> V021 |
     HV022 <> V022 |
     HV023 <> V023 then
    e = errmsg(10002,HV004,V004,HV020,V020,HV021,V021,HV022,V022,HV023,V023)
  endif;

  {*!! Should be set *}
  if (HV044 <> notappl) <=> (V044 = notappl) then
    e = errmsg(10009,HV044,V044)
  endif;

  {* Variables that are supposed to be the same *}
  if HV019 <> V029 |
     HV030 <> V030 |
     HV031 <> V031 |
     HV032 <> V032 then
    e = errmsg(10005,HV019,V029,HV030,V030,HV031,V031,HV032,V032)
  endif;

  if HV008 > V008 then                                                                    { Date of interview }
    e = errmsg(10003,HV008,V008)
  endif;
  if V003 > HV009 | !HV117(V003) then                                             { Member's line number }
    e = errmsg(10004,V003,HV009,V003,HV117(V003))
  endif;

  {* For incomplete interviews weights should be set to 0 -> structure check *}
  if V005 = 0 <=> V015 = 1 then                                                   { Weights }
    e = errmsg(0100,V005,V015)
  endif;

  {* CMC code V008 = V007 * 12 + V006 should be correct -> structure check *}
  if V008 <> cmcode(V006,V007) then                                               { CMC date of interview }
    e = errmsg(0101,V006,V007,V008)
  endif;

  {* Variable that should exist for complete interviews and be NA for incomplete interviews -> structure check *}
  if (V015 =  1 & (V009  = notappl | V010  = notappl | V011  = notappl |
                   V012  = notappl | V013  = notappl | V014  = notappl))
   | (V015 <> 1 & (V009 <> notappl | V010 <> notappl | V011 <> notappl |
                   V012 <> notappl | V013 <> notappl | V014 <> notappl)) then
    e = errmsg(0102,V015,V009,V010,V011,V012,V013,V014)
  endif;

  {* CMC date of birth -> structure check *}
    if (V015 = 1) & V011 <> cmcode(V009,V010) then
      e = errmsg(0103,V009,V010,V011)
    endif;
  {* Age of respondent -> structure check *}
    if V012 <> int((V008-V011)/12) then
      e = errmsg(0104,V008,V011,V012)
    endif;
  {* Age of respondent in 5-year age group -> structure check *}
    if V013 <> int(V012/5)-2 then
      e = errmsg(0105,V012,V013)
    endif;

  {* consistency between interview month and day -> structure check *}
  box V016 : V006            => x;
       >31 : 1,3,5,7,8,10,12 => 1;
       >30 : 4,6,9,11        => 1;
       >29 : 2               => 1;
           :                 => 0;
  endbox;
  if x | (V016 = 29 & V006 = 2 & (V007 % 4)) then
    e = errmsg(0106,V016,V006,V007)
  endif;

  if (V017-1) % 12 | V008-V017 >= vcallen | V008-V017 < 60 then
    e = errmsg(0107,V008,V017)
  endif;

  if calused then
    if (V015  = 1) <=> (!V018 | !V019 | !V019A) then
      e = errmsg(0108,V015,V018,V019,V019A)
    endif;
    if V015 = 1 & V018 <> vcallen - (V008-V017) then
      e = errmsg(0109,V015,V008,V017,V018,V019)
    endif;
    if V015 = 1 & V019 <> vcallen + 1 - V018 then
      e = errmsg(0110,V008,V017,V018,V019)
    endif;
  else
    if (V018 | V019 | V019A) then
      e = errmsg(0112,V018,V019,V019A)
    endif;
  endif;

    if V023 = 0 then
      kount(20) = kount(20) + 1;
    endif;

  if !special(V034) & (V034 = V003 | V034 < 0 | V034 > HV009) then
    {* Line number of husband inconsistent *}
    e=errmsg(0113,V034,V003,HV009);
  elseif !special(V034) & V034 & (HV104(V034) <> 1 | HV105(V034) < 15) then
    {* Husband has an incorrect gender or underage *}
    e = errmsg(0114,V034,HV104(V034),HV105(V034));
  endif;

{ ---------------------------------------------------------------------------- }
PROC REC11_EDT
preproc

  {* Structure check - Set unused variables to <> notappl !! *}
  if special(V101) | special(V102) | V103 = notappl| V104 = notappl |
     V106 = notappl|
     V133 = notappl| V134 = notappl| V135 = notappl | V136 = missing|
     V137 = missing| V138 = missing| V139 = notappl | V140 = notappl|
     V141 = notappl| V149 = notappl| V150 = notappl | special(V151) |
     V152 = notappl|
     AWFACTT = missing | AWFACTU = missing |
     AWFACTR = missing | AWFACTE = missing |
     AWFACTW = missing |
     V155 = notappl|
     V190 = missing| V191 = missing
   then
     e = errmsg(12000,"REC11");
     errmsg("not special: V101=%d,V102=%d,V151=%d",V101,V102,v151);
     errmsg("not missing: V136=%d,V137=%d,V138=%d,V190=%d,V191=%d",V136,V137,V138,V190,V191);
     errmsg("not missing: AWFACTT=%d,AWFACTU=%d,AWFACTR=%d,AWFACTE=%d,AWFACTW=%d",AWFACTT,AWFACTU,AWFACTR,AWFACTE,AWFACTW);
     errmsg("not notappl: V103=%d,V104=%d,V106=%d,V133=%d,V134=%d,V135=%d,V139=%d,V140=%d,V141=%d,",V103,V104,V106,V133,V134,V135,V139,V140,V141);
     errmsg("not notappl: V149=%d,V150=%d,V152=%d,V155=%d",V149,V150,V152,V155);
  endif;

  {*ART Region, Type of place of residence and De-facto place of residence should be identical - structure check }
  if V024 <> V101 | V025 <> V102 | V026 <> V134 then
    e = errmsg(0111,V015,V024,V025,V026,V101,V102,V134)
  endif;

  {*ART Years lived in place of residence V104 higher than Age of respondent V012 - Inconsistency in Doc *}
  if V104 <> missing & V104 < 95 & V104 > V012 then
    e = errmsg(1101,V012,V104)
  endif;

  {*ART Always lived in current place of residence or visitor then previous place of residence should be notappl - structure check *}
  if (V104 = 95 | V104 = 96) <=> V105 <> notappl then
    e = errmsg(1102,V104,V105)
  endif;

  {*ART Level of education and grade are given at the same time - structure check *}
  if (V106 = 0 | V106 = missing) <=> V107 <> notappl then
    e = errmsg(1103,V106,V107)
  endif;

  {*ART Not dejure resident but these variables are not set to 7, 97 or 997 - structure check }
  if HV102(V003) = 1 <=> (V113 = 97 | V115 = 997 | V116 = 97 |
                          V119 =  7 | V120 =   7 | V121 =  7 |
                          V122 =  7 | V123 =   7 | V124 =  7 |
                          V125 =  7 | V127 =  97 | V128 = 97 |
                          V129 = 97 | V139 =  97 | V140 =  7 |
                          V153 =   7 | V160 =  7 |
                          V161 = 97 | v166 = 997) then
    e = errmsg(1123,HV102(V003),V113,V115,V116,V119,V120,V121,V122,V123,V124,V125);
    e = errmsg(1124,HV102(V003),V127,V128,V129,V139,V140,V141,V153,V160,V161,v166);
  endif;

  {*ART Children went to school under age 5 - Note in DOC *}
  if V133 <> missing & !V133 in 97,98 & V133+5 > V012 then
    e = errmsg(1104,V012,V133)
  endif;

  {*ART For usual residents individual variables and HH variables should be consistent - structure check *}
  if V135 = 1 then
    { first group }
    if V113 <> HV201 |
       V115 <> HV204 |
       V116 <> HV205 |
       V119 <> HV206 |
       V120 <> HV207 |
       V121 <> HV208 then
       e = errmsg(1116, V113,HV201,V115,HV204,V116,HV205,V119,HV206,V120,HV207,V121,HV208);
    endif;
    { second group }
    if V122 <> HV209 |
       V123 <> HV210 |
       V124 <> HV211 |
       V125 <> HV212 |
       V127 <> HV213 |
       V128 <> HV214 then
       e = errmsg(1117, V122,HV209,V123,HV210,V124,HV211,V125,HV212,V127,HV213,V128,HV214);
    endif;
    { third group }
    if V129 <> HV215 |
       V153 <> HV221 |
       V160 <> HV225 |
       V161 <> HV226 |
       V166 <> HV234 then
       e = errmsg(1118, V129,HV215,V153,HV221,V160,HV225,V161,HV226,V166,HV234);
    endif;
  endif;

  {*ART Individual variables and HH variables schould be consistent - structure check *}
  if HV009 <> V136 |
     HV014 <> V137 |
     HV010 <> V138 |
     HV101(V003) <> V150 |
     HV219 <> V151 |
     HV220 <> V152 |
     HV270 <> V190 |
     HV271 <> V191 |
     HML12(V003) <> ML101 then
    e = errmsg(1105,HV009,V136,HV014,V137,HV010,V138,V003,HV101(V003),V150,HV219,V151,HV220,V152,HV270,V190,HV271,V191,V003,HML12(V003),ML101);
  endif;

  {*ART Questionnaire complete: # of HH-members, # of children < 5 and # of eligible women should be given - structure check }
  if (V015 = 1 <=> V136 = notappl) |
     (V015 = 1 <=> V137 = notappl) |
     (V015 = 1 <=> V138 = notappl) then
    e = errmsg(1106,V015,V136,V137,V138)
  endif;

  {*ART Residence status as declared by the respondent and her residence as declared in the household - structure check *}
  if V135 <> missing & V135 <> 2 - (HV102(V003) = 1) then
    e = errmsg(1119,V135,HV102(V003))
  endif;

  {*ART # Of children under 5 + # of eligible women more than # of HH-members - structure check *}
  if V137+V138 > V136 then
    e = errmsg(1107,V136,V137,V138)
  endif;

  {*ART # Of eligible women more than # of HH-members - structure check *}
  if V138 > V136 then
    e = errmsg(1108,V138,V136)
  endif;

  { {* V114, V142 not used in DHS 4 *}
  if V142 = 1 <=> V113 <> V114 then
    e = errmsg(1109,V113,V114,V142);
    kount(2) = kount(2) + 1
  endif;
  }

  {*ART Checking the recode of the educational attainment - structure check *}
  box V149 => x;
       1,2 => 1;          {* Primary incomplete and complete *}
       3,4 => 2;          {* Secondary incomplete and complete *}
         5 => 3;          {* Higher *}
           => V149;
  endbox;
  if x <> V106 then
    e = errmsg(1115,V149,V106)
  endif;

  {* Relation to HH-head for respondent should be same in individual and HH data file - structure check *}
  if V150 <> HV101(V003) then
    e = errmsg(1111,V150,HV101(V003))
  endif;

  {*ART Sex of HH-head is not sex of first person in HH-schedule; not required but - Test to be deleted *}
  if V151 <> HV104(1) then
    e = errmsg(1112,V151,HV104(1))
  endif;

  {*ART Age of HH-head is not age of HH-head in HH-schedule - structure check *}
  if V152 <> HV105(HV218) then
    e = errmsg(1113,V152,HV105(1))
  endif;

  {*ART Not all-women sample but all-woman factors not included - structure check *}
  if !V020 & (AWFACTT <> 100 | AWFACTU <> 100 |
              AWFACTR <> 100 | AWFACTE <> 100 | AWFACTW <> 100) then
    e = errmsg(1114,V020,AWFACTT,AWFACTU,AWFACTR,AWFACTE,AWFACTW)
  endif;

  {*ART Education secondary or higher then V155 = 2 (able to read whole sentence) - structure check *}
  { !! DHS7 V155 asked for primary or secondary !!}
  if V155 <> 2 & V106 = 3 then
    e = errmsg(1120,V155,V106);
  endif;
  { !! literacy program checks commented out since this q is no longer asked in DHS7 !!}
  {
  {*ART Literacy program only asked to no education or primary or DK - structure check *}
  { if V156 = notappl <=> (V106 = 0 | V106 = 1 | V106 = 8) then }
  if V106 in 0,1,8,missing <=> V156 = notappl then
    e = errmsg(1121,V156,V106);
  endif;

  {*ART Reads newspaper but no literacy program and no education -> Note in Doc *}
  if !V157 in 0,missing & V106 in 0,missing & V156 = 0 then
    e = errmsg(1122,V106,V157,V156);
  endif;
  }

  {* ART No trips away from home then Away for more than 1 month should be not applicable - structure check *}
  if (V167 = 0) <=> (V168 <> notappl)  then
    e = errmsg(1125,V167,V168);
  endif;

{ ---------------------------------------------------------------------------- }
PROC REC21_EDT
preproc

  lastb = 0;

  for i in REC21_EDT do
                                                                  { Set unused variables to <> notappl !!}
    if special(BIDX) | special(BORD) |
       special(B0)   | special(B1)   | special(B2)   | special(B3) |
       special(B4)   | special(B5)   |
       B7 = missing  | B8 = missing  |
       special(B10)  | B11 = missing | B12 = missing | B13 = missing| B19 = notappl
     then
       errmsg(12101,i,"REC21");
       errmsg("not special: BIDX=%d,BORD=%d,B0=%d,B1=%d,B2=%d,B3=%d,B4=%d,B5=%d,B10=%d",BIDX,BORD,B0,B1,B2,B3,B4,B5,B10);
       errmsg("not missing: B7=%d,B8=%d,B11=%d,B12=%d,B13=%d,B19=%d",B7,B8,B11,B12,B13,B19);
    endif;

    if BIDX <> i then
      errmsg(2101,i,BIDX)
    endif;

    if BORD <> V201-i+1 then
      errmsg(2102,i,BORD)
    endif;

    if i > 1 & B3 <> B3(i-1) & B3 > B3(i-1)-7 then
      errmsg(2103,i-1,B3(i-1),i,B3)
    endif;
    if i > 1 & B3 = B3(i-1) then
      if B0 <> B0(i-1) - 1 then
        errmsg(2104,i-1,B0(i-1),B3(i-1),i,B0,B3)
      endif
    else
      if B0 = 1 then
        errmsg(2105,i,B0,B3)
      endif
    endif;

    if B3 <> (B2-1900)*12+B1 then
      errmsg(2106,i,B1,B2,B3)
    endif;

    if i > 1 & B3 <> B3(i-1) & B3 > B3(i-1) + 7 then
      e = errmsg(2116,i,B3,i-1,B3(i-1))
    endif;

    if B5 then
      {* Living children *}
      if B6 <> notappl | B7 <> notappl | B13 <> notappl then
        errmsg(2107,i,B5,B6,B7,B13)
      endif;
      if special(B8) | B9 = notappl then
        errmsg(2108,i,B5,B8,B9)
      endif;
      if B8 <> int( B19(i)/12 ) then
        errmsg(2109,i,B5,V008,B3,B8)
      endif;
    else
      {* Dead children *}
      if B6 = notappl | special(B7) | special(B13) then
        errmsg(2117,i,B5,B6,B7,B13)
      endif;
      if B7 > V008-B3 then
        errmsg(2110,i,B5,B7,V008,B3)
      endif;
      x = B6;
      box x   => x1;
      301-340 => 11;
              => 0;
      endbox;
      box x   => x;
      100-129 => 0;
      130-131 => 1;
      201-290 => x-200;
      301-340 => (x-300)*12;
      900-999 => missing;
      197-199 => missing;
      297-299 => missing;
      397-399 => missing;
      missing => missing;
              => notappl;
      endbox;
      if B7 = 12 & B6 = 301 then
        {* Age declared as 1 year *}
        kount(11) = kount(11) + 1
      elseif x = notappl | (x <> missing & B13 <> 1 & (B7 < x | B7 > x+x1)) then
        {* Check age at death. Cases with flag = 1 are not printed out *}
        errmsg(2115,i,B6,B7,B13)
      endif;
      if B8 <> notappl | B9 <> notappl then
        errmsg(2118,i,B5,B8,B9)
      endif;
    endif;

    if lastb & int( (B18(lastb)-B18)/DaysMonth ) <> B12 then
      errmsg(2111,i,B18,B12,lastb,B18(lastb))
    elseif !lastb & B12 <> notappl then
      errmsg(2112,i,B18,B12)
    endif;

    if B0 = 0 | B0 = 1 then
      lastb = i
    endif;
    j = i + B0;
    if !B0 then
      j = j + 1
    endif;
    if j <= V224 & int((B18-B18(j)) / DaysMonth) <> B11 then
      errmsg(2113,i,B18,B11(i),j,B18(j))
    elseif !j & B11 <> notappl then
      errmsg(2114,i,B18,B11)
    endif;

    if calused then
          { No birth in the calendar }
      x = V018+V008-B3;
      if x <= vcallen & !pos("B",VCAL(1)[x:1]) then
        errmsg(2121,i,B3,V008,V018,x,VCAL(1)[x:1])
      elseif x > vcallen & i < V235 then  { last child in calendar }
        errmsg(2122,i,B3,V008,V018,x)
      endif;
    endif;

    {* Following checks to detect inconsistencies between birth history and household schedule, but should
       provide insight on the quality of B16 - line number of child in household schedule: when 2125, 2126
       and 2127 all occur for the same child then B16 is most likely wrong! *}
    if !special(B16) & B16 > 0 then    { child line number in the HH }
      {*ART Duplicate line numbers of children in HH-schedule - Inconsistency in doc: should never happen *}
      if count(REC21_EDT where B16 = B16(i) & BIDX <> BIDX(i)) then
        errmsg(2124,i,B16);
      endif;

      {*ART Mother's line number disagrees with line number of mother in HH-schedule - Inconsistency in Doc *}
      if HV111(B16) <> notappl & HV112(B16) <> V003 then
        errmsg(2125,i,B16,HV112(B16),V003);
      endif;

      {*ART Sex of child disagrees with sex in HH-schedule - Inconsistency in doc *}
      if B4 <> HV104(B16) then
        errmsg(2126,i,B16,HV104(B16),B4);
      endif;

      {*ART Age of child disagrees with age in HH-schedule - Inconsistency in doc *}
      if !special(B8) & !special(HV105(B16)) then
        x = B8(i) - HV105(B16);
        if x > 2 | x < (-2) then
          errmsg(2127,i,B16,HV105(B16),B8);
        endif
      endif;

      {*ART Living status of child disagrees with living status in HH-schedule - Inconsistency in doc *}
      x = B9; if x = 0 then x = 1 elseif x > 0 then x = 0 endif;
      if x <> HV102(B16) & HV102(V003) = 1 then
        errmsg(2128,i,B16,HV102(B16),B9);
      endif;
    endif;

  enddo;

{ ---------------------------------------------------------------------------- }
PROC REC22_EDT
preproc
                                                                { Set unused variables to <> notappl !!}
  if special(V201) | special(V202) | special(V203) | special(V204) |
     special(V205) | special(V206) | special(V207) | special(V208) |
     special(V209) | special(V210) | V211 = missing| V212 = missing|
     special(V213) | V214 = missing|
     V216 = missing| V217 = notappl| special(V218) | special(V219) |
     special(V220) | V221 = missing| V222 = missing| V223 = missing|
     special(V224) |
     V227 = missing|
     V232 = missing|
     V235 = missing|
     special(V238) |
     V243 = missing
   then
     e = errmsg(12000,"REC22");
     errmsg("not special: V201=%d,V202=%d,V203=%d,V204=%d,V205=%d,V206=%d,V207=%d,V208=%d",V201,V202,V203,V204,V205,V206,V207,V208);
     errmsg("not special: V209=%d,V210=%d,V213=%d,V218=%d,V219=%d,V220=%d,V224=%d,V238=%d",V209,V210,V213,V218,V219,V220,V224,V238);
     errmsg("not missing: V211=%d,V212=%d,V214=%d,V216=%d",V211,V212,V214,V216);
     errmsg("not missing: V220=%d,V221=%d,V222=%d,V223=%d,V227=%d,V232=%d,V235=%d,V243=%d",V220,V221,V222,V223,V227,V232,V235,V243);
     errmsg("not notappl: V217=%d",V217);
  endif;

  {* Children that have the respondent as mother in the HH should exist in the birth history of the mother ->
     inconsistency in doc.* }

  for i in RECH1_EDT do
    if HV112 = V003 & count(REC21_EDT where B16 = i) <> 1 then
      errmsg(2129,i)
    endif;
  enddo;

  if V224 then
    if B3(V224) < V011+minab then
      errmsg(2119,V224,B3(V224),V011,minab)
    endif;
    if B3(1) > V008 then
      errmsg(2120,V224,B3(1),V008)
    endif;
  endif;

  if V201 <> sOccurs(REC21) then
    errmsg(2201,sOccurs(REC21),V201)
  endif;

  x = count(REC21_EDT where B4 = 1 & B5 & B9 = 0);
  if V202 <> x then
    errmsg(2202,V202,x)
  endif;
  x = count(REC21_EDT where B4 = 2 & B5 & B9 = 0);
  if V203 <> x then
    errmsg(2203,V203,x)
  endif;
  x = count(REC21_EDT where B4 = 1 & B5 & B9 <> 0);
  if V204 <> x then
    errmsg(2204,V204,x)
  endif;
  x = count(REC21_EDT where B4 = 2 & B5 & B9 <> 0);
  if V205 <> x then
    errmsg(2205,V205,x)
  endif;
  x = count(REC21_EDT where B4 = 1 & !B5);
  if V206 <> x then
    errmsg(2206,V206,x)
  endif;
  x = count(REC21_EDT where B4 = 2 & !B5);
  if V207 <> x then
    errmsg(2207,V207,x)
  endif;
  x = count(REC21_EDT where B19 < 60);
  if V208 <> x then
    errmsg(2208,V208,x)
  endif;
  x = count(REC21_EDT where B19 <= 12);
  if V209 <> x then
    errmsg(2209,V209,x)
  endif;
  x = count(REC21_EDT where V008=B3);
  if V210 <> x then
    errmsg(2210,V210,x)
  endif;

  if V201 & V211 <> B3(V224) then
    errmsg(2211,V211,V201,B3(V224))
  elseif V201 <=> V211 = notappl then
    errmsg(2211,V211,V201,notappl)
  endif;
  if V201 & V212 <> int((B3(V224) - V011)/12) then
    errmsg(2212,V212,V201,B3(V224),V011)
  elseif V201 <=> V212 = notappl then
    errmsg(2212,V212,V201,notappl,V011)
  endif;

  if V213 then
    if V201 & V008-V214 < B3(1) then
      errmsg(2214,V214,V008,B3(1))
    endif;
  endif;

  {* Time since last menstrual period is before last birth but no children -> inconsistency in doc *}
  if (V215 = 995 & !V201) then
    errmsg(2215,V215,V201)
  endif;
  {* Never menstruated but has children *}
  if (V215 = 996 & V201) then
    errmsg(2215,V215,V201)
  endif;
  {* Check menstruation last 6 weeks *}
  box V215 => x;
   100-142 => 1;
   200-206 => 1;
   300-301 => 1;
           => 0;
  endbox;
  if x <> V216 then
    errmsg(2216,V216,V215)
  endif;

  x = int(V215/100);
  if x = missing then
    x = 9
  endif;
  x1 = V215%100;
  if x1 = missing then
    x1 = 99
  endif;
  box     x : x1    => x;
          1 : 0-90  => int(x1/30);
          2 : 0-90  => int(x1/4.3);
          3 : 0-90  => x1;
          4 : 0-90  => x1*12;
            :       => missing;
  endbox;
  if x <> missing & V201 > 0 & B3(1)+x > V008 then
    if V226 = 0 | V226 = 8 then
      errmsg(2217,V215,V226,V227,B3(1),V008)
    elseif V227 = 0 | V227 = notappl then
      kount(7) = kount(7) + 1
    endif
  endif;

  if x = missing & V215 > 900 then
    x = V215
  endif;
  if V227 = 9 then
    x = 995
  elseif V227 <> 0 & V227 <> 8 then
    x = 997
  endif;
  if x <> V226 then
    errmsg(2230,V215,V226,x,V227)
  endif;

  x = count(REC21_EDT where B5 = 1);
  if V218 <> x then
    errmsg(2218,V218,x)
  endif;
  if V219 <> V218+V213 then
    errmsg(2219,V219,V218,V213)
  endif;
  if V220 <> V219 & (V219 < 6 | V220 <> 6) then
    errmsg(2220,V220,V219)
  endif;

  if V224 & V509 <> notappl then
    if (V221 <> B3(V224)-V509 & (B3(V224) < V509 <=> V221 <> 996)) then
      errmsg(2221,V221,V224,B3(V224),V509)
    endif
  elseif V221 <> notappl then
    errmsg(2221,V221,V224,notappl,V509)
  endif;
  if (V224 <=> V222 = notappl) then
    errmsg(2222,V222,V224,notappl,V008)
  elseif (V224 & V222 <> B19(1) ) then
    errmsg(2222,V222,V224,B19(1),V008)
  endif;
  if ( V213 & (V223  = notappl | V225  = notappl)) |
     (!V213 & (V223 <> notappl | V225 <> notappl)) then
    errmsg(2223,V223,V225,V213)
  endif;
  if V224 <> sOccurs(REC21) then
    errmsg(2224,sOccurs(REC21),V224)
  endif;

  if calused <=> V228 = notappl then
    errmsg(2225,V228)
  endif;

  {*ART What does this check? *}
  if (V228  = 1 & (V229  = notappl | V230  = notappl | V231  = notappl | V232  = notappl))
   | (V228 <> 1 & (V229 <> notappl | V230 <> notappl | V231 <> notappl | V232 <> notappl)) then
     errmsg(2226,V228,V229,V230,V231,V232)
  endif;

  {*ART What does this check? *}
  { Ever had terminated pregnancy and cmd pregnancy ended during calendar then V233 and V234 can not be NA  |
    Never had a terminated pregnancy and cmc terminated prgnancy or V233 and V234 not notappl then}
  if ((V228  = 1 & !special(V231) & V231 in V017:9995) & V233  = notappl )
   | ((V228 <> 1 |  special(V231) | V231 <  V017)      & V233 <> notappl ) then
     errmsg(2227,V228,V017,V231,V233,V234)
  endif;

  if V230 = notappl then
    x  = notappl;
    x2 = notappl;
  elseif special(V230) | V230 > 9996 then  { CONSIS 4 }
    x = 9900 + (V230 % 100);               { CONSIS 4 }
    if special(V229) | V229 > 96 then
      x2 = 8
    else
      x2 = 7
    endif
  elseif special(V229) | V229 > 96 then
    x = 9900 + V229;
    x2 = 5;
  else
    x = (V230 - 1900)*12 + V229;            { CONSIS 4 }
    x2 = 1;
  endif;
  if V018 then
    x1 = pos("T",VCAL(1));
    if x1 then
      if x2 <> 1 then
        x  = V017 + vcallen - x1;
        x2 = 2;
      elseif x <> V017 + vcallen - x1 then
        errmsg(2228,V229,V230,V231,x,V232,x2,x1)
      endif
    elseif !special(x) & x < 9996 & x >= V017 then
      errmsg(2228,V229,V230,V231,x,V232,x2,x1)
    endif;
  endif;
  if V231 <> x | V232 <> x2 then
    errmsg(2228,V229,V230,V231,x,V232,x2,x1)
  endif;

  x = count(REC21_EDT where B3 < V017);
  if x then
    x = V224 - x + 1;
  endif;
  if (V235 <> x) then
    errmsg(2229,V235,x)
  endif;

  x = count(REC21_EDT where B19 < 36);
  if V238 <> x then
    errmsg(2231,V238,x)
  endif;

  {* Date pregnancy terminated before beginning of calendar - Note in doc? *}
  if !special(V231) & (V231 > 9990 | V231 < V017) then
    if V239 <> 1 then
      errmsg(2232,V239,V240,V241,V229,V230,V231)
    elseif V230 <> V241 | V229 <> V240 then
      errmsg(2233,V239,V240,V241,V229,V230,V231)
    endif;
  else
    if V239 = 1   & (!special(V241) & V241 < 9990 & (V241 > V230 |
      (V241 = V230 & !special(V240) & V240 < 90   &  V240 > V229))) then
      errmsg(2234,V239,V240,V241,V229,V230)
    endif;
  endif;

  if (V239 =  1 & (V240 =  notappl | V241 =  notappl | V242 =  notappl | V243 =  notappl)) |
     (V239 <> 1 & (V240 <> notappl | V241 <> notappl | V242 <> notappl | V243 <> notappl)) then
    errmsg(2235,V239,V240,V241,V242,V243)
  endif;
  if V241 = notappl then
    x  = notappl;
    x2 = notappl;
  elseif special(V241) | V241 > 9996 then  { CONSIS 4 }
    x = 9900 + (V241 % 100);               { CONSIS 4 }
    if special(V240) | V240 > 96 then
      x2 = 8
    else
      x2 = 7
    endif
  elseif special(V240) | V240 > 96 then
    x = 9900 + V240;
    x2 = 5;
  else
    x = (V241 - 1900)*12 + V240;            { CONSIS 4 }
    x2 = 1;
  endif;
  if V242 <> x | V243 <> x2 then
    errmsg(2236,V240,V241,V242,x,V243,x2)
  endif;

{ ---------------------------------------------------------------------------- }
PROC REC31_EDT
preproc
                                                                { Set unused variables to <> notappl !!}
  if special(V301) then
    errmsg(12000,"REC31");
    errmsg("not special: V301=%d,V302=%d",V301,V302);
  endif;

  kmethod = 0;
  umethod = 0;
  for i in REC31_GROUP000 do
                                                                  { Set unused variables to <> notappl !!}
    if special(V304A) | V304 = notappl then
      errmsg("not special: REC31 occurrence %d V304A = %d", i, V304A);
      errmsg("not notappl: REC31 occurrence %d V304 = %d", i, V304);
    endif;

    box i        => x;
    1-7,11,13-17 => 1;       { 17 = other modern }
    8-9,12       => 2;
    10,18-20     => 3;
    endbox;
    if V304A <> x then
      errmsg(3105,i,V304A)
    endif;
  { !! V305 not longer asked !!
    if ((V304  = 1 {| V304  = 2}) & V305  = notappl)
    |  ((V304 <> 1 {& V304 <> 2}) & V305 <> notappl) then
      errmsg(3104,i,V304,V305)
    endif;
    if i <> 16 then          { Comment out if emergency contraception is used !!}
      if (V305  = 1 <=> V307 = notappl) then
        errmsg(3106,i,V305,V307)
      endif;
    endif;                   { Comment out if emergency contraception is used !!}
  }
    if V304 = 1{| V304 = 2}then
      if i <= 7 | i = 11 | (i >= 13 & i <= 16) then
        kmethod = 3;
      elseif i = 8 | i = 9 | i = 12 then
        if kmethod < 2 then kmethod = 2 endif
      else
        if kmethod = 0 then kmethod = 1 endif
      endif;
      if V305 = 1 then
        if i <= 7 | i = 11 | (i >= 13 & i <= 16) then
          umethod = 3
        elseif i = 8 | i = 9 | i = 12 then
          if umethod < 2 then umethod = 2 endif
        else
          if umethod = 0 then umethod = 1 endif
        endif
      endif;
    endif;

  enddo;
  if kmethod <> V301 then
    errmsg(3101,V301,kmethod)
  endif;
  { !! V302 no longer asked !!
    if umethod <> V302 then
      errmsg(3102,V302,umethod)
        endif;
  }

{ ---------------------------------------------------------------------------- }
PROC REC32_EDT
preproc
                                                                { Set unused variables to <> notappl !!}
  if V311 <> notappl| V312 = notappl| V313 = notappl|
     V315 = missing| V316 = missing| V317 = missing|
     V318 = missing| V319 = missing| V320 = missing|
     V321 = missing| V322 = missing|
     special(V361) |
     special(V364) |
     V384A = notappl | V384B = notappl | V384C = notappl |
     V393  = notappl | V394  = notappl
   then
     errmsg(12000,"REC32");
     errmsg("not special: V361=%d,V364=%d",V361,V364);
     errmsg("not missing: V315=%d,V316=%d,V317=%d,V318=%d,V319=%d,V320=%d,V321=%d,V322=%d",V315,V316,V317,V318,V319,V320,V321,V322);
     errmsg("not notappl: V311=%d,V312=%d,V313=%d,V384A=%d,V384B=%d,V384C=%d,V393=%d,V394=%d",V311,V312,V313,V384A,V384B,V384C,V393,V394);
  endif;

  len = length(strip(vcalmeth));
  if calused & (len <> 19) |
     !calused & (len <>  0) then
     if calused then
       errmsg(3221,len,"")
     else
       errmsg(3221,len,"Not")
     endif;
  endif;

  births35 = V238;
  if Hmax = 59 then births35 = V208 endif;

  { !! V310 not used in DHS 7 !!
    {* Living children at first use more than the children ever born *}
    if (V310 <> missing & V310 > V201) then
      errmsg(3201,V310,V302,V201)
        endif;

    {* Living children at first use and ever use do not match *}
    if (V302 <=> V310 = notappl) then
      errmsg(3201,V310,V302,V201)
        endif;

    box V310 => x;
    notappl => 5;
    missing => missing;
    0-4  => V310;
    => 4;
    endbox;
    if x <> V311 then
      errmsg(3202,V311,V310)
        endif;
  }
  { !! not in DHS7
    {* Current pregnancy, ever use and current use *}
    if (V213 | !V302) & V312 then
      errmsg(3203,V312,V213,V302)
        endif;
  }
  {* Ever use of current method *}
  { !! not in DHS7
    if V312 in 1:20 & V305(V312) <> 1 then
      errmsg(3204,V312,V305(V312))
        endif;
  }
  {* using current method *}
  if V312 in 1:20 & V307(V312) <> 1 then
    errmsg(3200,V312,V307(V312))
  endif;

  {* check consistency between modern, trad, folk methods in V312 and V313 -> structure check *}
  box V312       => x;
         0       => 0;
   1-7,11,13-16,17 => 3;        { 17 = other modern }
        8-9,12     => 2;
        10,18-20   => 1;
      missing    => missing;
                 => default;
  endbox;
  if x <> V313 then
    errmsg(3205,V313,V312);
  endif;

  { V305 not used in DHS7 !!
    {* Ever used female/male sterilization and current use of the method *}
    if (V305(6) = 1 & V312 <> 6) |
      (V305(6) <> 1 & V305(7) = 1 & V312 <> 7) then
        errmsg(3207,V312,V305(6),V305(7))
        endif;
  }

  { Start of use of current method }
  if (V312 <> 0 &
      (V315  = notappl | V316  = notappl | V317  = notappl | V318  = notappl))
   | (V312 =  0 &
      (V315 <> notappl | V316 <> notappl | V317 <> notappl | V318 <> notappl)) then
    errmsg(3208,V312,V315,V316,V317,V318,V319,V320,V321,V322)
  endif;
  if ((V312  = 6 | V312  = 7) &
      (V319  = notappl | V320  = notappl | V321  = notappl | V322  = notappl))
   | ((V312 <> 6 & V312 <> 7) &
      (V319 <> notappl | V320 <> notappl | V321 <> notappl | V322 <> notappl)) then
    errmsg(3208,V312,V315,V316,V317,V318,V319,V320,V321,V322)
  endif;

  {* Date of first use of a method *}
  if V317 <> notappl then

    if V317 > V008 then
    {* First use of a method after the date of interview *}
      errmsg(3209,V312,V317,V008)
      { !! todo msg removed pending discussion as to whether it is necesssary to check this !!
    elseif V224 & V317 < B3(1) then
    {* First use of a method before the birth of the first child *}
      errmsg(3218,V312,V317,V008,B3(1))
      }
    endif;

    if V317 <> (V316-1900)*12 + V315 then
      errmsg(3210,V317,V316,V315)
    endif;

    if V312 = 6 | V312 = 7 then
      x = int((V008-V317)/24)+1;         { years since sterilization in groups of 2 years }
      if x > 6 then
        x = 6
      endif;
      {* computed years since sterilization does not match V319 -> structure check *}
      if x <> V319 then
        errmsg(3211,V319,V317,V008)
      endif;
      x = int((V317-V011)/60)-3;         { age at sterilization in 5-year age groupd }
      if x = 0 then
        x = 1
      endif;
      {* computed age at sterilization does not match V320 -> structure check *}
      if x <> V320 then
        errmsg(3212,V320,V317,V011)
      endif;
      if V509 = notappl | V509 > V317 then
        x = 0
      else
        x = int((V317-V509)/60)+1;
        if x > 6 then x = 6 endif;
      endif;
      if x <> V321 then
        errmsg(3213,V321,V317,V509)
      endif;
      x = count(REC21_EDT where B3 <= V317);
      if x > 5 then
        x = 5
      endif;
      if x <> V322 then
        errmsg(3214,V322,V317,x)
      endif;
    else
      if V319 <> notappl | V320 <> notappl | V321 <> notappl | V322 <> notappl then
        errmsg(3216,V312,V319,V320,V321,V322);
      endif;
    endif;
  endif;

    { Pill use }
    if V312 = 1 <=> V323 = notappl then
      errmsg(3215,V312,V323);
    endif;
    { Condom use }
    if V312 = 5 <=> V323A = notappl then
      errmsg(3225,V312,V323A);
    endif;
  { !! not used in DHS7
    { Cost of method }
    if !(V312 in 1:7,11,14:16) <=> V325A <> notappl then  { Modern methods excluding LAM }
    errmsg(3226,V312,V325A);
    endif;
  }

    {* Current users of modern methods then check first (V3A07) and last (V326, V327) source *}
    { DHS7 no source for LAM }
    if V312 = 8 then                               { V3A07 now asked for users of rhythm }
      if V326 <> notappl & V327 <> notappl & V3A07 = notappl then
        errmsg(3217,V312,V326,V327,V3A07);
      endif;
    elseif V316 > myear then                       { V3A07 now asked for methods started during calendar only }
      if (V312 in 1:7, 11, 14:16 & (V326 = notappl | V327 = notappl | V3A07 = notappl))
        | (V312 in missing, 0, 9:10, 12, 13, 17:20 & (V326 <> notappl | V327 <> notappl | V3A07 <> notappl)) then
        errmsg(3217,V312,V326,V327,V3A07);
        errmsg("V315=%d, V316 = %d, V317 = %d, V318 = %d", V315, V316, V317, V318);
      endif;
    else
      if (V312 in 1:7, 11, 14:16 & (V326 = notappl | V327 = notappl {| V3A07 = notappl} ))
        | (V312 in missing, 0, 9:10, 12,13,17:20 & (V326 <> notappl | V327 <> notappl | V3A07 <> notappl)) then
        errmsg(3217,V312,V326,V327,V3A07);
      endif;
  endif;

  { Duration of current use }
  if ( V312 <> 0 <=> V337 = notappl) then
    errmsg(3219,V312,V337)
  endif;
  if {(V312 = 6 | V312 = 7)} V312 <> 0 & V337 <> V008-V317 &
    ((calused  & (V337 <> 95 | V008-V317 < V019)) |
     (!calused & (V337 <> 96 | V008-V317 < 96  ))) then
    errmsg(3220,V312,V337,V008-V317,V019)
  endif;

  if calused & V337 <> notappl then
    method = V312;
    if V312 = missing then
      V312 = 19
    endif;
    methcode = pos(VCAL(1)[V018:1],vcalmeth);
    if methcode = method then
      i = V018+1;
      while i <= vcallen & method = pos(VCAL(1)[i:1],vcalmeth) do
        i = i + 1;
      enddo;
      if i <= vcallen | V317 = V017 then
        if (pos(VCAL(1)[i:1],"B") & method = 6 & V317 <= B3(1)) |
           (pos(VCAL(1)[i:1],"T") & method = 6 & V317 = V231)  then
          i = i + 1
        endif;
        if V337 <> i - 1 - V018 then
              errmsg(3222,V312,V337,i-1-V018,V018)
        endif
      else
        if V337 <> 95 then
          errmsg(3223,V337,V019)
        endif
      endif
    else
      errmsg(3224,V312,VCAL(1)[V018:1]);
      if (pos(VCAL(1)[V018:1],"B") | pos(VCAL(1)[V018:1],"T")) then
        kount(8) = kount(8) + 1
      endif;
    endif
  endif;

  if col2used then
    { Last method discontinued in last 5 years }
    i    = V018;
    imax = V018 + 59;
    if vcallen < imax then
      imax = vcallen
    endif;
    while i <= imax & pos(" ",VCAL(2)[i:1]) do
      i = i + 1
    enddo;
    if i > imax then
      x  = notappl;
      x1 = notappl;
    else
      x = pos(VCAL(1)[i:1],vcalmeth);
      if x = 19 then          { Missing - code ? }
        x = missing
      endif;
      x1 = pos(VCAL(2)[i:1],vcaldisc);
      if x1 = 19 then         { Missing - code ? }
        x1 = missing
      elseif x1 = 18 then     { DK - code K }
        x1 = 98
      endif
    endif;
    if x <> V359 | x1 <> V360 then
      e = errmsg(3236,V359,x,V360,x1)
    endif;
  endif;

  obiuse = 0;
  if calused then
    i = V018;
    { Search for method use or birth in calendar }
    while i <= vcallen & pos(VCAL(1)[i:1],"0PT") do
      i = i + 1;
    enddo;
    if i <= vcallen then
      obiuse = pos(VCAL(1)[i:1],vcalmeth);      { Used in calendar ? }
    else
      obiuse = 0                                { Used before calendar ? }
    endif;
  endif;
  if V312 <> 0 then
    x = 1
  elseif obiuse then
    x = 2
  elseif V302 > 0 | V302A > 0 then              { CONSIS6 }
    x = 3
  else
    x = 4
  endif;
  if x <> V361 then
    errmsg(3237,V361,V312,obiuse,V302,V302A)
  endif;
  {* Pattern of use and intention *}
  if V361 <> 1 <=> V362 = notappl then
    errmsg(3238,V361,V362)
  endif;
  {* Intend to use and preferred future method *}
  if (V362 = 1 | V362 = 2 | V362 = 3) <=> V363 = notappl then
    errmsg(3239,V312,V362,V363)
  endif;
  if (V602 = 4) & (V362 <> notappl | V363  <> notappl |
                   V376 <> notappl | V376A <> notappl )then
    errmsg(3243,V602,V362,V363,V376,V376A)
  endif;
  box V312       : V362 => x;
    1-7,11,13-17 :      => 1;               { 17 = other modern }
    8-10,12,18-20:      => 2;
         missing :      => 2;
                 :   6  => 5;
                     : 1-3  => 3;
                     :      => 4;
  endbox;
  if x <> V364 then
    errmsg(3240,V364,V312,V362)
  endif;
  x = notappl;
  if births35 then
    x = M10(1)
  endif;
  if (births35 <=> V367 = notappl) | (V367 <> x) then
    errmsg(3241,V208,V238,V367,x)
  endif;

  { Pill use }
  if V312 = 1 <=> V372 =  notappl then
    errmsg(3249,V312,V372)
  endif;

  { Condom use }
  if V312 = 5 <=> V372A = notappl then
    errmsg(3250,V312,V372A)
  endif;

  {* Main reason  not using a method. *}
  { Check which one apply }                                            {!!}
  { Old logic }
  if V312 = 0 <=> V375A = notappl then
    errmsg(3255,V375A,V312,V213,V605)
  endif;
  { new logic }
  if V375A = notappl <=> (V605 in 2, 5 & V312 = 0 & V213  = 0) then
    errmsg(3255,V375A,V312,V213,V605)
  endif;

  {* Unsure about use and not intending to use should have an answer to
     main reason not intend to use a method.
  *}
  if V362 = 5 <=> V376 = notappl then
    errmsg(3246,V362,V376)
  endif;

  if V376 = 11 <=> V376A = notappl then
    errmsg(3256,V376A,V376)
  endif;

  if V313 = 3 then
    x  = V326;
    x1 = V327;
  else
    x  = V379;          { Cannot check this part }
    x1 = V380;
  endif;
  if V379 <> x | V380 <> x1 then
    errmsg(3248,V313,V326,V327,V379,x,V380,x1)
  endif;

  if V395 = notappl <=> V394 = 1 then
    errmsg(3257,V395,V394)
  endif;
                                                                                          { Comment out unused variables !!}
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V3A00A );
    e = AddVals( V3A00B );
    e = AddVals( V3A00C );
    e = AddVals( V3A00D );
    e = AddVals( V3A00E );
    e = AddVals( V3A00F );
    e = AddVals( V3A00G );
    e = AddVals( V3A00H );
    e = AddVals( V3A00I );
    e = AddVals( V3A00J );
    e = AddVals( V3A00K );
    e = AddVals( V3A00L );
    e = AddVals( V3A00M );
    e = AddVals( V3A00N );
    e = AddVals( V3A00O );
    e = AddVals( V3A00P );
    e = AddVals( V3A00Q );
    e = AddVals( V3A00R );
    e = AddVals( V3A00S );
    e = AddVals( V3A00T );
    e = AddVals( V3A00U );
    e = AddVals( V3A00V );
    e = AddVals( V3A00W );
    e = AddVals( V3A00X );
                                            {* Add country specific variables !!}
    if ( x & (x1 | V3A00Y <> 0 | V3A00Z <> 1)) |                                  { Some yes => V3A00Y=no, V3A00Z=yes, no missings }
       (!x & !x1 & x2 <> k & V3A00Y <> 1 & V3A00Z <> 0) |                 { No yes, no missings => V3A00Y = yes, V3A00Z = no }
       (x1 & (x1+x2 <> k | V3A00Y <> missing | V3A00Z <> missing)) |  { Some missing => all missing, V3A00Y = missing, V3A00Z = missing }
       (x2 = k & (V3A00Y <> notappl | V3A00Z <> notappl)) then        { All notappl => V3A00Y = notappl, V3A00Z = notappl }
      errmsg(3269,V3A00Y,V3A00Z,x,x1,x2)
    endif;

  if V3A01 = notappl <=> (V312 in 6,7) then
    errmsg(3261,V3A01,V312)
  endif;
  {* V3A02 applicable for methods 1,2,3,6 and 11 when use started during calendar *}
  if ( V3A02 = notappl <=> (V312 in 6, 1, 2, 3, 11) ) & !special(V316) & V316 >= myear then
    errmsg(3262,V3A02,V312,V316)
  endif;
  if V3A03 = notappl <=> (V3A02 <> 1 & V3A02 <> notappl) then
    errmsg(3263,V3A03,V3A02)
  endif;
  if ((V3A02 <> 1) & (V3A04 =     notappl <=> V3A03 = 1)) |
     ((V3A03  = 1) & (V3A04 = notappl <=> V3A02 <> notappl)) then
    errmsg(3264,V3A04,V3A03,V3A02)
  endif;
  {* V3A05 applicable for methods 1,2,3,4,6,11,14 and 15 when use started during calendar *}
  if ( V3A05 = notappl <=> (V312 in 6, 1, 2, 3, 11, 14, 4, 15{, 13}) ) & !special(V316)   & V316 >= myear then
    errmsg(3265,V3A05,V312,V316)
  endif;
  if !V312 in 0,5,7,8,9,13,10,17 then
    if V3A06 = notappl <=> (V3A05 <> 1 & V3A05 <> notappl) then
      errmsg(3266,V3A06,V3A05,V312)
    endif;
  endif;
                                                                                          { Comment out unused variables !!}
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V3A08A );
    e = AddVals( V3A08B );
    e = AddVals( V3A08C );
    e = AddVals( V3A08D );
    e = AddVals( V3A08E );
    e = AddVals( V3A08F );
    e = AddVals( V3A08G );
    e = AddVals( V3A08H );
    e = AddVals( V3A08I );
    e = AddVals( V3A08J );
    e = AddVals( V3A08K );
    e = AddVals( V3A08L );
    e = AddVals( V3A08M );
    e = AddVals( V3A08N );
    e = AddVals( V3A08O );
    e = AddVals( V3A08P );
    e = AddVals( V3A08Q );
    e = AddVals( V3A08R );
    e = AddVals( V3A08S );
    e = AddVals( V3A08T );
    e = AddVals( V3A08U );
    e = AddVals( V3A08V );
    e = AddVals( V3A08W );
    e = AddVals( V3A08X );
                                                          {* Add CS variables !!}
    if ( x & (x1 | V3A08Z <> 0)) |                                        { Some yes => V3A08Z=no, no missings    }
       (!x & !x1 & x2 <> k & V3A08Z <> 1) |                       { No    yes, no missings => V3A08Z = yes }
       (x1 & (x1+x2 <> k | V3A08Z <> missing)) |          { Some missing => all missing, V3A08Z = missing }
       (x2 = k & V3A08Z <> notappl) then                          { All notappl => V3A08Z = notappl }
      e = errmsg(3267,V3A08Z,x,x1,x2)
    endif;

  {* If V3A08A is not used replace it with the first applicable variable.     !!}
  if V3A08A = notappl <=> ((V605 = 2 | V605 = 5) & V312 = 0 & V213 = 0) then
    e = errmsg(3268,V3A08A,V605,V312,V213)
  endif;

{ ---------------------------------------------------------------------------- }
PROC REC41_EDT
preproc

  lastch = V468;    { Questions A408-A429, A434A, A436-A447 and A455-A459
                                      are for last child only or for all children }
  for i in REC41_EDT do
                                                                  { Set unused variables to <> notappl !!}
    if special(MIDX)  |
       M19A = missing |
       M27  = missing | M28 = missing | M29  = missing
     then
       e = errmsg(12101,i,"REC41");
       errmsg("not special: MIDX=%d",MIDX);
       errmsg("not missing: M19A=%d,M27=%d,M28=%d,M29=%d",M19A,M27,M28,M29);
    endif;

    ok = 1;
    {* occurrence in REC41 should match MIDX - structure check *}
    if i <> MIDX then
      errmsg(4102,i,MIDX); ok=0
    endif;
    {* occurence can not be bigger than number of children in birth history - structure check *}
    if i > sOccurs(REC21) then
      errmsg(4103,i,sOccurs(REC21)); ok=0
    else
      {* age of child in month not to exceed Hmax - structure check *}
      if B19(i) > Hmax then
        errmsg(4104,i,V008,B3(i)); ok=0
      endif
    endif;

    if ok then                      { occurrence checks ok }
      xi = i-1;                     { xi = later birth }
      if i & B0(i) then
        while xi & B0(xi) > 1 do    { later birth = multiple birth }
          xi = xi - 1;              { xi = last birth }
        enddo;
      endif;
      if !xi then                       { no later birth }
        if V213 then
          interval = V008-V214-B3(i);   { interval = duration of pregnancy }
        else
          interval = V008-B3(i)         { interval = open birth interval }
        endif
      else                              { later birth }
        interval = B3(xi)-B3(i)         { interval = open birth interval }
      endif;

      j = i + 1;
      if B0(i) > 1 &
       (
       (lastch <> 1 {* questions asked only for last child in standard, but not in country -> structure check *} &
         (M1  <> M1 (j) |

          M1A <> M1A(j) | M1B <> M1B(j) | M1C <> M1C(j) |
          M1D <> M1D(j) | M1E <> M1E(j) |

          M2A <> M2A(j) | M2B <> M2B(j) | M2C <> M2C(j) |
          M2D <> M2D(j) | M2E <> M2E(j) | M2F <> M2F(j) |
          M2G <> M2G(j) | M2H <> M2H(j) | M2I <> M2I(j) |
          M2J <> M2J(j) | M2K <> M2K(j) | M2L <> M2L(j) |
          M2M <> M2M(j) | M2N <> M2N(j) |

          M4  <> M4(j)  |

          M13 <> M13(j) | M14 <> M14(j) |

          M34 <> M34(j) |

          M42A<> M42A(j)| M42B<> M42B(j)| M42C<> M42C(j)|
          M42D<> M42D(j)| M42E<> M42E(j)|

          M43 <> M43(j) | M44 <> M44(j) | M45 <> M45(j) |
          M46 <> M46(j) | M47 <> M47(j) | M48 <> M48(j) |

          M49A<> M49A(j)| M49B<> M49B(j)| M49C<> M49C(j)|
          M49D<> M49D(j)| M49E<> M49E(j)| M49F<> M49F(j)|
          M49G<> M49G(j)| M49X<> M49X(j)| M49Z<> M49Z(j)|

          M54 <> M54(j) |

          M55A<> M55A(j)| M55B<> M55B(j)| M55C<> M55C(j)|
          M55D<> M55D(j)| M55E<> M55E(j)| M55F<> M55F(j)|
          M55G<> M55G(j)| M55H<> M55H(j)| M55I<> M55I(j)|
          M55J<> M55J(j)| M55K<> M55K(j)| M55L<> M55L(j)|
          M55M<> M55M(j)| M55N<> M55N(j)| M55O<> M55O(j)|
          M55X<> M55X(j)| M55Z<> M55Z(j)|

          M57A<> M57A(j)| M57B<> M57B(j)| M57C<> M57C(j)|
          M57D<> M57D(j)| M57E<> M57E(j)| M57F<> M57F(j)|
          M57G<> M57G(j)| M57H<> M57H(j)| M57I<> M57I(j)|
          M57J<> M57J(j)| M57K<> M57K(j)| M57L<> M57L(j)|
          M57M<> M57M(j)| M57N<> M57N(j)| M57O<> M57O(j)|
          M57P<> M57P(j)| M57Q<> M57Q(j)| M57R<> M57R(j)|
          M57S<> M57S(j)| M57T<> M57T(j)| M57U<> M57U(j)|
          M57V<> M57V(j)| M57X<> M57X(j)|

          M60 <> M60(j) | M61 <> M61(j) | M62 <> M62(j) |

          M65A<> M65A(j)| M65B<> M65B(j)| M65C<> M65C(j)|
          M65D<> M65D(j)| M65E<> M65E(j)| M65F<> M65F(j)|
          M65G<> M65G(j)| M65H<> M65H(j)| M65I<> M65I(j)|
          M65J<> M65J(j)| M65K<> M65K(j)| M65L<> M65L(j)|
          M65X<> M65X(j)|

          M66 <> M66(j) |

          M70 <> M70(j) | M71 <> M71(j) | M72 <> M72(j)|
          M73 <> M73(j) )
        ) {end of questions asked of last child only in standard} |

         {* questions asked for all children in standard - so twins should have same response? *}
         {* Place of delivery *}
         M3A <> M3A(j) | M3B <> M3B(j) | M3C <> M3C(j) |
         M3D <> M3D(j) | M3E <> M3E(j) | M3F <> M3F(j) |
         M3G <> M3G(j) | M3H <> M3H(j) | M3I <> M3I(j) |
         M3J <> M3J(j) | M3K <> M3K(j) | M3L <> M3L(j) |
         M3M <> M3M(j) | M3N <> M3N(j) |

         M6  <> M6 (j) | M7  <> M7 (j) |       {* Duration of amenorrhea *}
         M8  <> M8 (j) | M9  <> M9 (j) |       {* Duration of abstinence *}
         M10 <> M10(j) | M11 <> M11(j) |           { Wanted pregnancy }

         M15 <> M15(j) | M17 <> M17(j) | M18 <> M18(j) |
         M19 <> M19(j) | M19A <> M19A(j) |

         M28 <> M28(j) | M29 <> M29(j) |           { Flags for amenorrhea and abstinence }

         M38 <> M38(j) |

         M61 <> M61(j) | M62 <> M62(j)

       ) then
        e = errmsg(4109,i,B0(i),j)
      endif;

      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes }
      x1 = 0;     { Count of missing }
      x2 = 0;     { Count of notappl }
      e = AddVals( M2A );
      e = AddVals( M2B );
      e = AddVals( M2C );
      e = AddVals( M2D );
      e = AddVals( M2E );
      e = AddVals( M2F );
      e = AddVals( M2G );
      e = AddVals( M2H );
      e = AddVals( M2I );
      e = AddVals( M2J );
      e = AddVals( M2K );
      e = AddVals( M2L );
      e = AddVals( M2M );
                                                      {* Add CS variables !!}
      if ( x & (x1 | M2N <> 0)) |                     { Some yes => M2N=no, no missings }
         (!x & !x1 & x2 <> k & M2N <> 1) |            { No yes, no missings => M2N = yes }
         (x1 & (x1+x2 <> k | M2N <> missing)) |       { Some missing => all missing, M2N = missing }
         (x2 = k & M2N <> notappl) then               { All notappl => M2N = notappl }
        e = errmsg(4116,i,M2N,x,x1,x2)
      endif;

      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes }
      x1 = 0;     { Count of missing }
      x2 = 0;     { Count of notappl }
      e = AddVals( M3A );
      e = AddVals( M3B );
      e = AddVals( M3C );
      e = AddVals( M3D );
      e = AddVals( M3E );
      e = AddVals( M3F );
      e = AddVals( M3G );
      e = AddVals( M3H );
      e = AddVals( M3I );
      e = AddVals( M3J );
      e = AddVals( M3K );
      e = AddVals( M3L );
      e = AddVals( M3M );
                                                      {* Add CS variables !!}
      if ( x & (x1 | M3N <> 0)) |                     { Some yes => M3N=no, no missings }
         (!x & !x1 & x2 <> k & M3N <> 1) |            { No yes, no missings => M3N = yes }
         (x1 & (x1+x2 <> k | M3N <> missing)) |       { Some missing => all missing, M3N = missing }
         (x2 = k & M3N <> notappl) then               { All notappl => M3N = notappl }
        e = errmsg(4117,i,M3N,x,x1,x2)
      endif;

      {*ART Currently breastfeeding non-lastborn child - Note in Doc - see 4203 and 4216 *}
      if M4 = 95 & xi then
        e = errmsg(4111,i,M4,xi)
      endif;
      {* Check BF status (M4 = 96: BF until died. M4=95: still BF)
         against living status (B5) *}
      if (M4 = 96 & B5(i) = 1) |
         (M4 = 95 & B5(i) = 0) then
        e = errmsg(4105,i,M4,B5(i))
      endif;
  { !!! comment out if M27 is N/A in DHS7 !!!
      {* Check months of breastfeeding (M5) against duration of breastfeeding
         and age at death (for dead children). *}
      x   = M4;
      x1  = B7(i);
      x2  = B3(i);
      box x  => x;
          96 => x1;
          95 => V008-x2;
             => x;
      endbox;
      if !special(x1) & x1 < 996 & x < 94 & x > x1 then

        {* Duration of BF greater than age at death and Flag not set to 3 *}
        if M27 <> 3 then
          if M27 = 0 then
            x = x1;
          endif;

        {* Flag should have been set to 3, but other inconsistencies would have
           reset it to a different value (1 or 2). *}
          e = errmsg(4123,i,M4,M5,M27,B7(i))
        endif
      endif;
  }
      if M27 = 1 then                  { BF > interval }
        x = 97
      elseif M27 = 2 & x then          { BF > interval by 1 month }
        x = x - 1
      elseif M27 = 3 then              { BF > age at death }
        x = B7(i)
  {   elseif M27 = 4 then              { Ignore }
        j = V018+V008-B3(i)-1;
        while j >= V018 & !pos(VCAL(1)[j:1],"B") do
          j = j - 1
        enddo;
        x3 = V008 + V018 - j - B3(i) - 1;
        if x3 < x then x = x3 endif;
      }
     endif;
  {
    if x <> M5 then
      e = errmsg(4106,i,M4,M5,x,M27,x2,x1,V008)
        endif;
    if !special(M5) & M5 < 90 then
      if M5 > V008-B3(i) then
        e = errmsg(4110,i,M4,M5,M27,V008,B3(i))
          endif
          endif;
  }

      { Amenorrhea }
      if M6 = 96 then                                  { period not returned }
        x1 = interval;                                 { x1 = open birth interval }
        if xi > 0 then                                 { later birth }
          x1 = x1-9                                    { x1 = 9 months between births }
        endif;
        if x1 < 0 then
          x1 = 0
        endif;
      else                                             { period returned }
        x1 = M6                                        { x1 = M6 }
      endif;
      if M28 = 1 then                                  { amenorrhea flag = 1: > interval }
        x1 = 97                                        { x1 = 97 }
      elseif M28 = 2 & x1 then                         { amenorrhea flag = 2: > interval by 1 mnth }
        x1 = x1 - 1                                    { x1 = x1 - 1 }
      elseif M28 = 4 then                              { amenorrhea flag = 4: during pregnancy in calendar }
        j = V018+V008-B3(i)-1;
        while j >= V018 & !pos(VCAL(1)[j:1],"BPT") do
          j = j - 1
        enddo;
        x3 = V008 + V018 - j - B3(i) - 1;
        if x3 < x1 then x1 = x3 endif;                  { x1 reset based on calendar }
      endif;
      {*ART M7: Calculated months of amenorrhea <> M6: Duration of amenorrhea - structure check *}
      if x1 <> M7 then
        e = errmsg(4107,i,M6,M7,x1,M28)
      endif;

      x1 = interval;                               { x1 = interval }
      if xi > 0 then                               { later birth }
        x1 = x1-7                                  { x1 = birth interval - 7 months }
      endif;
      {*ART M7: Calculated months of amenorrhea longer than maximum expected - structure check *}
      {*ART If it turns out that in all cases flag is set accordingly then flag should be included in test *}
      if !special(M7) & M7 < 90 & M7 > x1 then
        e = errmsg(4112,i,M6,M7,M28,x1)
      endif;

      if !special(V226) & V226 < 994 then
        x = V226
      else
        x = 0
      endif;
      if xi = 0 & V213 then
        x1 = x1 + V214
      endif;
      if i = 1 then
        if {* Duration of amenorrhea and conception > the birth interval *}
           (!special(M7(1)) & M7(1) < 90 & M7(1)+x > x1) |
           {* Period not returned but duration given *}
           (M6(1)  = 96 & !V226 in 994:997) |
           {* Period given but before last birth or never menstruated *}
           (M6(1) <> 96 & V226 in 995:996)
         then
           e = errmsg(4113,1,M6(1),M7(1),M28(1),V215,V226,V227,x1);
           if (!special(M7(1)) & M7(1) < 90 & M7(1)+x > x1) then
             errmsg("Duration of amenorrhea and conception > the birth interval");
           endif;
           if (M6(1)  = 96 & !V226 in 994:997) then
             errmsg("Period not returned but duration given");
           endif;
           {* Period given but before last birth or never menstruated *}
           if (M6(1) <> 96 & V226 in 995:996) then
             errmsg("Period given but before last birth or never menstruated");
           endif;
        endif;
      endif;

      { Abstinence }
      if M8 = 96 then
        x1 = V008-x2
      else
        x1 = M8
      endif;
      if M29 = 1 then
        x1 = 97
      elseif M29 = 2 & x1 then
        x1 = x1 - 1
      elseif M29 = 4 then
        j = V018+V008-B3(i)-1;
        while j >= V018 & !pos(VCAL(1)[j:1],"BPT") do
          j = j - 1
        enddo;
        x3 = V008 + V018 - j - B3(i) - 1;
        if x3 < x1 then x1 = x3 endif;
      endif;
      if (M8 = 96 & V213 = 1) | (M9 <> x1) then
        e = errmsg(4108,i,M8,M9,x1,M29,V213,V008,x2)
      endif;

      x1 = interval;
      if xi > 0 then      {* Interval between birth & conception *}
        x1 = x1-7
      endif;
      {* Months of abstinence > Interval between births and following conception,
                              > Open birth interval (OBI).
      *}
      if !special(M9) & M9 < 90 & M9 > x1 then
        e = errmsg(4114,i,M8,M9,M29,x1)
      endif;

      {* Months of abstinence and date of last intercourse *}
      if !special(V529) & V529 < 994 then
        x = V529;
      else
        x = 0
      endif;
      if xi = 0 & V213 then
        x1 = x1 + V214
      endif;
      if i = 1 then
        if V530 <> 2 & (!special(M9(1)) & M9(1) < 90 & M9(1)+x > x1) then
        {* Duration of abstinence and time since last sex > interval *}
          e = errmsg(4115,1,M8(1),M9(1),x,M29(1),V527,V529,V530,x1)
        {*???? {* Need to catch a flag not set properly *} ????*}
        elseif V529 <> notappl & (
                                  {* Still abstaining but time since last sex given *}
                                  (M8(1) =  96 & V529 <> 996 & V529 <> 997 & V530 = 0) |
                                  {* Duration of abstinence but time since last sex is before last birth *}
                                  (M8(1) <> 96 & V529 =  996 & M29(1) = 0)
                                 ) then
          e = errmsg(4115,1,M8(1),M9(1),x,M29(1),V527,V529,V530,x1)
        endif;
      endif;

      if M11 = notappl <=> M10 = 2 then
        e = errmsg(4118,i,M10,M11)
      endif;

      {* No one for prenatal care but month of 1st visit or number of visits *}
      if (M2N <> notappl | M13 <> notappl | M14 <> notappl) &
        ((M13 <> notappl <=> M2N = 1) |
         (M14 <> 0       <=> M2N = 1)) then
        e = errmsg(4119,i,M13,M14,M2N)
      endif;

      if M17 <> 0 & (M15 < 20 | M15 = 96) then
        e = errmsg(4120,i,M15,M17)
      endif;

      { if (M19A = 0 <=> M19 <> 9996) | (M19A = 9 <=> !(M19 in missing, 9998)) then       { CONSIS6 }}
      if (M19A = 0 <=> M19 <> 9996) | (M19A in 8,9 <=> !(M19 in missing, 9998)) then        { CONSIS6 }
        e = errmsg(4121,i,M19,M19A)
      endif;

      {*ART if mother never breastfed then timing when child was put to breast should be notappl - structure check *}
      if (lastch <> 1 | i = 1) & (M4 = 94 <=> M34 <> notappl) then
        e = errmsg(4124,i,M4,M34)
      endif;

      if (B5(i) = 1 <=> M38 = notappl) |
         (( lastch <> 1 | i = 1 ) & B9(i) = 0 & V008 - B3(i) < fdingcutoff & (B5(i) = 1 <=> M39 = notappl)) then
        e = errmsg(4128,i,B9(i),B5(i),M38,M39)
      endif;

      { need to modify this if asked for all children ***??*** !!}
      if Midx = childm then

      endif;

      if M2N = 1 then                                                             { Comment out unsued variable !! }
        if (
            M42D <> notappl | M42E <> notappl | M57X <> notappl ) then
          e = errmsg(4136,i,M42A,M42B,M42C,M42D,M42E,M43,M2N)
        endif
      else
        if (lastch <> 1 | i = 1) &
           (M42A  = notappl | M42B  = notappl | M42C  = notappl |
            M42D  = notappl | M42E  = notappl | M43   = notappl) then
          e = errmsg(4136,i,M42A,M42B,M42C,M42D,M42E,M43,M2N)
        endif
      endif;

      { CONSIS6 skip in A414 has been taken out in DHS6 and M44 is no longer standard -> take out test
      if M44 = notappl <=> M43 = 1 then
        e = errmsg(4137,i,M44,M43)
      endif;
      }

      if M46 = notappl <=> M45 = 1 then
        e = errmsg(4138,i,M46,M45)
      endif;

      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes }
      x1 = 0;     { Count of missing }
      x2 = 0;     { Count of notappl }
      e = AddVals( M49A );
      e = AddVals( M49B );
      e = AddVals( M49C );
      e = AddVals( M49D );
      e = AddVals( M49E );
      e = AddVals( M49F );
      e = AddVals( M49G );
      e = AddVals( M49X );
                                                     {* Add CS variables !!}
      if ( x & (x1 | M49Z <> 0)) |                   { Some yes => M49Z=no, no missings }
         (!x & !x1 & x2 <> k & M49Z <> 1) |          { No yes, no missings, all vars NA => M49Z = yes }
         (x1 & (x1+x2 <> k | M49Z <> missing)) |     { Some missing => all missing, M49Z = missing }
         (x2 = k & M49Z <> notappl) then             { All notappl => M49Z = notappl }
        e = errmsg(4139,i,M49Z,x,x1,x2)
      endif;
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes }
      x1 = 0;     { Count of missing }
      x2 = 0;     { Count of notappl }
      e = AddVals( M55A );
      e = AddVals( M55B );
      e = AddVals( M55C );
      e = AddVals( M55D );
      e = AddVals( M55E );
      e = AddVals( M55F );
      e = AddVals( M55G );
      e = AddVals( M55H );
      e = AddVals( M55I );
      e = AddVals( M55J );
      e = AddVals( M55K );
      e = AddVals( M55L );
      e = AddVals( M55M );
      e = AddVals( M55N );
      e = AddVals( M55X );
                                                     {* Add CS variables !!}
      if ( x & (x1 | M55Z <> 0)) |                   { Some yes => M55Z=no, no missings }
         (!x & !x1 & x2 <> k & M55Z <> 1) |          { No yes, no missings => M55Z = yes }
         (x1 & (x1+x2 <> k | M55Z <> missing)) |     { Some missing => all missing, M55Z = missing }
         (x2 = k & M55Z <> notappl) then             { All notappl => M55Z = notappl }
        e = errmsg(4140,i,M55Z,x,x1,x2)
      endif;
                                                                          { Comment out unused variables }
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes - not used}
      x1 = 0;     { Count of missing - not used}
      x2 = 0;     { Count of notappl }

      e = AddVals( M13 );

      e = AddVals( M42A );
      e = AddVals( M42B );
      e = AddVals( M42C );
      e = AddVals( M42D );
      e = AddVals( M42E );

      e = AddVals( M43  );

      e = AddVals( M57A );
      e = AddVals( M57B );
      e = AddVals( M57C );
      e = AddVals( M57D );
      e = AddVals( M57E );
      e = AddVals( M57F );
      e = AddVals( M57G );
      e = AddVals( M57H );
      e = AddVals( M57I );
      e = AddVals( M57J );
      e = AddVals( M57K );
      e = AddVals( M57L );
      e = AddVals( M57M );
      e = AddVals( M57N );
      e = AddVals( M57O );
      e = AddVals( M57P );
      e = AddVals( M57Q );
      e = AddVals( M57R );
      e = AddVals( M57S );
      e = AddVals( M57T );
      e = AddVals( M57U );
      e = AddVals( M57V );
      e = AddVals( M57X );
                                              {* Add CS variables !!}
      if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
        e = errmsg(4142,i,M57X,x2,k)
      endif;

      {* No one for prenatal care but information given *}
      if (M2N <> notappl | M57A <> notappl) & (M57A <> notappl <=> M2N = 1) then
        e = errmsg(4122,i,M57A,M2N)
      endif;

      if M60 = notappl <=> M45 <> notappl then
        e = errmsg(4143,i,M60,M45)
      endif;


      if i = 1 then                        { for all last births }
        {*ART Delivered at home or 'other', but duration of stay in facility given - structure check *}
        if M61 <> notappl <=> (M15 in 11:12, 96, missing) then
          e = errmsg(4144,i,M15,M61)
        endif;
        if M62 <> notappl <=> (M15 in 0:19, 96, missing) then
          e = errmsg(4145,i,M15,M62);
        endif;
      elseif !lastch & i > 1 then          { M62 not restricted to last birth then check all }
        {*ART Delivered at home or 'other', but duration of stay in facility given - structure check *}
        if M61 <> notappl <=> (M15 in 11:12, 96, missing) then
          e = errmsg(4144,i,M15,M61)
        endif;
        if M62 <> notappl <=> (M15 in 0:19, 96, missing) then
          e = errmsg(4145,i,M15,M62);
        endif;
      endif;

      { BERT: M63 replaced by M50, M51, M52 from DHS4 -> review BERT
      if (lastch <> 1 | i = 1) & (M62 = 1 <=> M63 = notappl) then
        e = errmsg(4146,i,M62,M63)
      endif;
      }

      if M70 = 1 <=> M71 = notappl then
        e = errmsg(4147,i,M70,M71)
      endif;

      if M70 = 1 <=> M72 = notappl then
        e = errmsg(4148,i,M70,M72)
      endif;

      if M70 = 1 <=> M73 = notappl then
        e = errmsg(4149,i,M70,M73)
      endif;

      if lastch = 1 & i <> 1 then  { check questions that are only for last child in standard }
        if M1   <> notappl | M1A  <> notappl | M1B  <> notappl | M1C  <> notappl |
           M1D  <> notappl | M1E  <> notappl | M2N  <> notappl | M13  <> notappl |
           M14  <> notappl | M42A <> notappl | M42B <> notappl | M42C <> notappl |
           M42D <> notappl | M42E <> notappl | M43  <> notappl | M44  <> notappl |
           M45  <> notappl | M46  <> notappl | M47  <> notappl | M48  <> notappl |
           M49X <> notappl | M54  <> notappl | M55X <> notappl | M57X <> notappl |
           M60  <> notappl | M65X <> notappl |
           M70  <> notappl |
           M71  <> notappl | M72  <> notappl | M73  <> notappl then
          e = errmsg(4160,i,V468,M1,M1A,M1B,M1C,M1D,M1E,M2N,M13,M14);
          e = errmsg(4161,i,V468,M42A,M42B,M42C,M42D,M42E,M43,M44,M45,M46,M47,M48);
          e = errmsg(4162,i,V468,M49X,M54,M55X,M57X,M60,M70,M71,M72,M73)
        endif
      endif;

    endif;

  enddo;
PROC REC42_EDT
preproc
                                                                { Set unused variables to <> notappl !!}
  if special(V404)   | special(V405)   | special(V406)   |
     special(V417)   | special(V418)   | special(V419)   |
     V455  = notappl |
     V463A = notappl |
     V467A = notappl | V467B = notappl | V467C = notappl |
     V467D = notappl | V467E = notappl | V467F = notappl |
     V467G = notappl | V467H = notappl | V467I = notappl |
     V467J = notappl | V467K = notappl | V467L = notappl | V467M = notappl |
     V468  = missing |
     V473B = notappl |
     V477  = notappl |
     V481  = notappl |
     V482B = notappl
   then
     e = errmsg(12000,"REC42");
     errmsg("not special: V404=%d,V405=%d,V406=%d,V417=%d,V418=%d,V419=%d",V404,V405,V406,V417,V418,V419);
     errmsg("not missing: V468=%d",V468);
     errmsg("not notappl: V455=%d,V463A=%d,V467A=%d,V467B=%d,V467C=%d,V467D=%d,V467E=%d,V467F=%d",V455,V463A,V467A,V467B,V467C,V467D,V467E,V467F);
     errmsg("not notappl: V467G=%d,V467H=%d,V467I=%d,V467J=%d,V467K=%d,V467L=%d,V467M=%d",V467G,V467H,V467I,V467J,V467K,V467L,V467M);
     errmsg("not notappl: V473B=%d,V477=%d,V481=%d,V482B=%d",V473B,V477,V481,V482B);
  endif;

  if V417 <=> V401 = notappl then
    if V417 then
      e = errmsg(4201,V401,V417,M15(1))
    else
      e = errmsg(4201,V401,V417,notappl)
    endif;
  elseif V417 & (M15(1) = 11 | M15(1) = 12 | M15(1) = 96 & M15(1) = missing) &
         V401 <> 0 then
      e = errmsg(4201,V401,V417,M15(1))
  endif;

  { Take out and rewrite as suggested by SG below? }
  {* Currently breastfeeding, but not last child - Note in Doc, see 4111 and 4216 *}
  if !V404 <=> (sOccurs(REC41) & M4(1) = 95 & B5(1) = 1) then
    x = notappl;
    if sOccurs(REC41) then
      x = M4(1)
    endif;
    e = errmsg(4203,V404,x)
  endif;
  {*ART currently breastfeeding but not breastfeeding one of her children - structure check *}
  {
  x = count(REC41 where M4 = 95);
  if !V404 <=> x then
    errmsg(4203,V404,x);
  endif;
  }

  if !V405 <=> (sOccurs(REC41) & M6(1) = 96 & !V213) then
    x = notappl;
    if sOccurs(REC41) then
      x = M6(1)
    endif;
    e = errmsg(4204,V405,x)
  endif;
  if !V406 <=> (sOccurs(REC41) & M8(1) = 96) then
    x = notappl;
    if sOccurs(REC41) then
      x = M8(1)
    endif;
    e = errmsg(4205,V406,x)
  endif;

  if (x &                 { comment out unused varaibles }
    (V409   = notappl | V409A  = notappl |
     V410   = notappl | V410A  = notappl | V411   = notappl | V411A  = notappl |
     V412   = notappl | V412A  = notappl | V412B  = notappl | V413   = notappl |
     V413A  = notappl | V413B  = notappl | V413C  = notappl | V413D  = notappl |
     V414A  = notappl | V414B  = notappl | V414C  = notappl | V414D  = notappl |
     V414E  = notappl | V414F  = notappl | V414G  = notappl | V414H  = notappl |
     V414I  = notappl | V414J  = notappl | V414K  = notappl | V414L  = notappl |
     V414M  = notappl | V414N  = notappl | V414O  = notappl | V414P  = notappl |
     V414Q  = notappl | V414R  = notappl | V414S  = notappl | V414T  = notappl |
     V414U  = notappl)) |
     (!x &
    (V409  <> notappl | V409A <> notappl |
     V410  <> notappl | V410A <> notappl | V411  <> notappl | V411A <> notappl |
     V412  <> notappl | V412A <> notappl | V412B <> notappl | V413  <> notappl |
     V413A <> notappl | V413B <> notappl | V413C <> notappl | V413D <> notappl |
     V414A <> notappl | V414B <> notappl | V414C <> notappl | V414D <> notappl |
     V414E <> notappl | V414F <> notappl | V414G <> notappl | V414H <> notappl |
     V414I <> notappl | V414J <> notappl | V414K <> notappl | V414L <> notappl |
     V414M <> notappl | V414N <> notappl | V414O <> notappl | V414P <> notappl |
     V414Q <> notappl | V414R <> notappl | V414S <> notappl | V414T <> notappl |
     V414U <> notappl)) then
    e = errmsg(4230,x,V409, V409A,V410, V410A,V411, V411A,V412, V412A,V412B,
                      V413A,V413B,V413C,V413D,V413, V414A,V414B,V414C,V414D,
                      V414E);
    e = errmsg(4231,x,V414F,V414G,V414H,V414I,V414J,V414K,V414L,V414M,
                      V414N,V414O,V414P,V414Q,V414R,V414S,V414T,V414U)
  endif;

  if V415 = notappl <=> (V417 & B5(1)) then
    x = 0;
    if V417 then
      x = B5(1)
    endif;
    e = errmsg(4207,V415,V417,x)
  endif;

  if count(REC4A_EDT where H13=1 | H13=2) & V416 <> 1 then
    e = errmsg(4208,V416,count(REC43_EDT where H13=1 | H13=2))
  endif;

  if V417 <> sOccurs(REC41) then
    e = errmsg(4209,V417,sOccurs(REC41))
  endif;
  if V418 <> sOccurs(REC43) then
    e = errmsg(4210,V418,sOccurs(REC43))
  endif;
  if V418A <> sOccurs(REC4A) then
    e = errmsg(4210,V418,sOccurs(REC4A))
  endif;
  if V419 <> sOccurs(REC44) then
    e = errmsg(4211,V419,sOccurs(REC44))
  endif;

  {{ Not standard for Core 5.  Uncomment if measurer/assistant given}  {!!}
  if (V419 <=> V420 = notappl) | (V419 <=> V421 = notappl) then
    e = errmsg(4212,V419,V420,V421)
  endif;
  }

  {*ART Never breastfed but child put to breast - structure check *}
  if sOccurs(REC41) & M4(1) <> missing then
    if V426 = notappl <=> M4(1) <> 94 then
      e = errmsg(4218,V426,M4(1))
    endif;
  endif;

  if anthro then
     {* respondents height but not weight given - structure check *}
     if ( V437 = notappl <=> V438 <> notappl) then
      e = errmsg(4220,V419,V437,V438)
    endif;

    {* HH selected for hemo, but result of H/W measurement = 7 *}
    {* This test only makes sense if selection for hemo is same as for H/W, *}
    {* however often H/W for all women and Hemo for subsample as indicated by V042 *}
    if V042 <> 0 <=> V447 = 7 then
      e = errmsg(4221,V447)
    endif;

    if anthroHH then
      i = 1;
      found = 0;
      while !found & i <= sOccurs(RECH5) do
        if V003 = HA0(i) then
          found = i
        endif;
        i = i + 1
      enddo;

      {* Respondent found in HH H/W, but result of H/W measurement = 7 - structure check *}
      if found <=> V447 = 7 then
        e = errmsg(4222,found,V447)
      endif;

      {* Respondent found in HH H/W, but age in HH schedule differs from age in *}
      {* HH H/W (as brought up from ind. quest.) - structure check *}
      if found & V447A <> HA1(found) then
        e = errmsg(4223,V447A,found,HA1(found))
      endif;

      {* Respondent found in HH H/W, but consent in HH not same as consent
         in ind. questionnaire - structure check *}
      if found & V473A <> HA61(found) then
        e = errmsg(4232,V473A,found,HA61(found))
      endif;

      {* Respondent found in HH H/W, but result of HIV measurement differs from *}
      {* result of HIV measurement in ind. quest. - structure check *}
      if found & V473B <> HA63(found) then
        e = errmsg(4233,V473B,found,HA63(found))
      endif;
    endif;

    {* HH selected for Hemo subsample but weight or height skipped - structure check *}
    if V042 <> 0 <=> V437 = notappl |
       V042 <> 0 <=> V438 = notappl then
      e = errmsg(4213,V437,V438)
    endif;

    {* Result of measurment inconsistent with measurments - structure check *}
    { if V042 <> 0 & (V447 = 0 <=> (V437 = missing | V438  = missing)) then   NOT CONSIS 6 }
    if V042 <> 0 & (V447 = 0 <=> (V437 in 9994,9995,9996,missing | V438 in 9994,9995,9996,missing)) then
      e = errmsg(4214,V447,V437,V438)
    endif;

    {*ART Only one measurement was taken: result code V447 should be <> 0 - Note in Doc *}
    if (V437 <> missing & V438 = missing) | (V437 = missing & V438 <> missing) then
      e = errmsg(4215,V447,V437,V438);
      if V447 <> 0 then
        errmsg("V447: result of measurement should be <> 0");
      endif;
    endif;

    {* Anemia testing Module *}

    {* No anemia testing but information present *}
    if !V042 & (V452A <> notappl | V452B <> notappl | V452C <> notappl |
                V453  <> notappl | V454  <> notappl | V455  <> notappl |
                V456  <> notappl | V457  <> notappl | V458  <> notappl ) then
      e = errmsg(4219,V452A,V452B,V452C,V453,V454,V455,V456,V457,V458,V042);
    endif;
    {* Anemia testing *}
    if V042 then
      {* Age, consent or result not given for selected woman *}
      if (V452A = notappl | V452C  = notappl) |
         (V452C <> missing & V452C <> 2 & V455  = notappl)
        then
        {* V452C = 2,missing is allowed for V455 = N/A*}
        e = errmsg(4224,V452A,V452C,V455,V042)
      endif;
      {* Under 18 with no line for caretaker or not valid caretaker line number *}
      if (V452A = 1 <=>  V452B = notappl) then
          if (V452B = notappl <=> HV116(V003) = 0) then
            e = errmsg(4225,V452A,V452B,HV009,V003,HV116(V003))
          endif;
      elseif (V452B <> notappl & (V452B > HV009 | V452B = V003)) then
            e = errmsg(4225,V452A,V452B,HV009,V003,HV116(V003))
      endif;
      {* Result of anemia and info. on other related variables. *}
      if (V452C  = 1 & (V453  = notappl | V454  = notappl | V456  = notappl | V457  = notappl | V455 = notappl)) |
  { NOT CONSIS6  (V452C <> 1 & (V453 <> missing | V454 <> missing | V456 <> missing | V457 <> missing | V455 <> 4 )) then   }
         (V452C <> 1 & (!V453 in 994,995,996,missing | !V455 in 3,4,6,missing | V456 <> missing | V457 <> missing )) then
        e = errmsg(4226,V452C,V453,V454,V455,V456,V457);
      endif;
      if V452C = 1 then   {* Consent granted *}
        {* Result of anemia testing and level or adjusted level. }
        if (V455  = 0 & (V453 = missing | V456  = missing | V457  = missing)) |                   { hemo measured }
           (V455 <> 0 & (!V453 in 994,995,996,missing | V456 <> missing | V457 <> missing)) then  { hemo not measured }
          e = errmsg(4227,V453,V455,V456,V457);
        endif;
        {* Pregnancy reported in the HH and reported by the respondent don't match *}
        if V454 <> V213 then
          e = errmsg(4228,V454,V213)
        endif;
      endif;

      if V455 = 0 then    {* Anemia tested *}
        alt      = (V040 / 1000) * 3.3;
        adjalt   = (-0.032 * alt) + (0.022 * alt * alt);
        if adjalt < 0 | HV040 < 1000 then adjalt = 0 endif;
        box V463Z : V464  => adjsmk; { whole bit of logic (box statement) was missing - gah }
                  :notappl=> 0;      { no smoking }
                  : 0-9   => 0;      { no smoking or < 10 cigarettes }
                  : 10-19 => 0.3;
                  : 20-39 => 0.5;
                  : 40-80 => 0.7;    { 80 means 80+ cigarttes, this needs to be 40-80 as per standard recode dictionary }
         {    0   :       => 0.3;    { missing, DK or smokes pipe, cigars } }
                  :       => 0;
        endbox;
        {* Less than 10 cigarettes then check whether smokes anything else *}
        if !V464 in 10:80 & V463B = 1 {| V463X = 1  ONLY PIPE or similar though not cigar etc. PER FRED NOV 19 2011 } then
          adjsmk = 0.3;
        endif;
        adjanem  = int( ( V453 / 10 - adjalt - adjsmk ) * 10 + 0.5 );
        box adjanem : V454 => anemlev;
               <70  :      => 1;
              <100  :      => 2;
              <110  :   1  => 3;
              <120  :<> 1  => 3;
                    :      => 4;
        endbox;

        {* Computed adjusted anemia and anemia level don't match the data file *}
        if adjanem <> V456 | anemlev <> V457 then
          e = errmsg(4229,V453,V454,V456,adjanem,V457,anemlev,V040);
        endif;
      endif
    endif
  endif;

  if V459 <> HV227 | V460 <> HV228 then
    e=errmsg(4259,V459,V460,HV227,HV228)
  endif;


    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V463A );
    e = AddVals( V463B );
    e = AddVals( V463C );
    e = AddVals( V463D );
    e = AddVals( V463E );
    e = AddVals( V463F );
    e = AddVals( V463G );
    e = AddVals( V463X );
                                                    {* Add CS variables !!}
    if ( x & (x1 | V463Z <> 0)) |                   { Some yes => V463Z=no, no missings }
       (!x & !x1 & x2 <> k & V463Z <> 1) |          { No yes, no missings => V463Z = yes }
    { ART: following check not correct: V463Z based on A1004 and A1006, V463A-X based on A1112 }
    {  (x1 & (x1+x2 <> k | V463Z <> missing)) | }   { Some missing => all missing, V463Z = missing }
    { todo - problem with check below, doesn't take account of A1104 = 2}
       (x2 = k & V463Z <> notappl { & V463AA <> 2 }) then             { All notappl => V463Z = notappl }
      e = errmsg(4263,V463Z,x,x1,x2)
    endif;
    if V463A = 1 <=> V464 = notappl then
      e = errmsg(4264,V464,V463A)
    endif;

  { !! DHS7 restricted to children < 24 months}
  x = count(REC21_EDT where (B9 = 0) & B19 < fdingcutoff);
  if x <=> V465 = notappl then
    e = errmsg(4265,V465,x)
  endif;

  x = count(REC21_EDT where B9 = 0);
  if x <=> V466 = notappl then
    e = errmsg(4266,V466,x)
  endif;


  { !! DHS7 no standard qs on mother's foods, uncomment below if survey contains them !!
  x = count(REC21_EDT where B9 = 0 & V008-B3 < 36);
  if (x &                 { comment out unused varaible }
    (V471A  = notappl | V471B  = notappl | V471C  = notappl | V471D  = notappl |
     V471E  = notappl | V471F  = notappl | V471G  = notappl | V472A  = notappl |
     V472B  = notappl | V472C  = notappl | V472D  = notappl | V472E  = notappl |
     V472F  = notappl | V472G  = notappl | V472H  = notappl | V472I  = notappl |
     V472J  = notappl | V472K  = notappl | V472L  = notappl | V472M  = notappl |
     V472N  = notappl | V472O  = notappl | V472P  = notappl | V472Q  = notappl |
     V472R  = notappl | V472S  = notappl | V472T  = notappl | V472U  = notappl)) |
     (!x &
    (V471A <> notappl | V471B <> notappl | V471C <> notappl | V471D <> notappl |
     V471E <> notappl | V471F <> notappl | V471G <> notappl | V472A <> notappl |
     V472B <> notappl | V472C <> notappl | V472D <> notappl | V472E <> notappl |
     V472F <> notappl | V472G <> notappl | V472H <> notappl | V472I <> notappl |
     V472J <> notappl | V472K <> notappl | V472L <> notappl | V472M <> notappl |
     V472N <> notappl | V472O <> notappl | V472P <> notappl | V472Q <> notappl |
     V472R <> notappl | V472S <> notappl | V472T <> notappl | V472U <> notappl)) then
    e = errmsg(4267,x,V471A,V471B,V471C,V471D,V471E,V471F,V471G);
    e = errmsg(4268,x,V472A,V472B,V472C,V472D,V472E,V472F,V472G,V472H,V472I,V472J,V472K,
                      V472L,V472M,V472N,V472O,V472P,V472Q,V472R,V472S,V472T,V472U);
  endif;
  }                                                                                                        {!! Comment out unused variables }
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V474A );
    e = AddVals( V474B );
    e = AddVals( V474C );
    e = AddVals( V474D );
    e = AddVals( V474E );
    e = AddVals( V474F );
    e = AddVals( V474G );
    e = AddVals( V474H );
    e = AddVals( V474I );
    e = AddVals( V474J );
    e = AddVals( V474X );
                                                    {* Add CS variables !!}
    if ( x & (x1 | V474Z <> 0)) |                   { Some yes => V474Z=no, no missings }
       (!x & !x1 & x2 <> k & V474Z <> 1) |          { No yes, no missings => V474Z = yes }
       (x1 & (x1+x2 <> k | V474Z <> missing)) |     { Some missing => all missing, V474Z = missing }
       (x2 <> 0 | V474Z = notappl) then              { No notappl }
      e = errmsg(4269,V474Z,x,x1,x2)
    endif;

    if V474 = 1 <=> V475 = notappl then
      e = errmsg(4270,V474,V475)
    endif;

    if V474 = 1 <=> V476 = notappl then
      e = errmsg(4271,V474,V476)
    endif;

    if V477 in 0,missing <=> V478 <> notappl then
      e = errmsg(4272,V477,V478)
    endif;

    if !(V477 in 0,missing) & (V478 in 0,missing <=> V479 <> notappl) then
      e = errmsg(4273,V477,V478,V479)
    endif;

    if V479 = notappl <=> V480 <> notappl then
      e = errmsg(4274,V479,V480)
    endif;
                                                                  { comment out unused variables }
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V481A );
    e = AddVals( V481B );
    e = AddVals( V481C );
    e = AddVals( V481D );
    e = AddVals( V481E );
    e = AddVals( V481F );
    e = AddVals( V481G );
    e = AddVals( V481X );
                                                    {* Add CS variables !!}
    if ( x2 <> 0 ) then             { No notappl }
      e = errmsg(4275,x2)
    endif;

    x = 0;
    if V201 & int((B19(1))/12) in 0:17 then
      x = 1
    endif;
    { !! caregivver questions removed in DHS7  !!
    if x <=> V482A = notappl then
      e = errmsg(4276,V482A)
    endif;

    if V482B = 1 <=> V482C = notappl then
      e = errmsg(4277,V482B,V482C)
    endif;
    }

{ ---------------------------------------------------------------------------- }
PROC REC43_EDT
preproc

  for i in REC43_EDT do

                                                                  { Set unused variables to <> notappl !!}
    if special(HIDX) then
      e = errmsg(12101,i,"REC43");
      errmsg("not special: HIDX=%d",HIDX);
    endif;

    ok = 1;
    if i <> HIDX then
      e = errmsg(4302,i,HIDX); ok=0
    endif;
    if i > sOccurs(REC21) then
      e = errmsg(4303,i,sOccurs(REC21)); ok=0
    else
      if B19(i) > Hmax then
        e = errmsg(4304,i,V008,B19(i),B5(i)); ok=0
      endif
    endif;

    if ok then

      {* No vaccination card but vaccinations given from card *}
      if (H1 <> 1 & !(H1A in 1,3,5:7) &
         (H2 = 1 | H3 = 1 | H4 = 1 | H5 = 1 | H6  = 1 |
          H7 = 1 | H8 = 1 | H9 = 1 | H0 = 1 | H33 = 1 | H40 = 1 |
          H2 = 3 | H3 = 3 | H4 = 3 | H5 = 3 | H6  = 3 |
          H7 = 3 | H8 = 3 | H9 = 3 | H0 = 3 | H33 = 3 | H40 = 3))
         {* Vaccination card but no vaccinations given in card *}
       | (H1 = 1 & H1A in 1,3,5:7 &
         (H2 = 0 & H3 = 0 & H4 = 0 & H5 = 0 & H6  = 0 &
          H7 = 0 & H8 = 0 & H9 = 0 & H0 = 0 & H33 = 0 & H40 = 0))
      then
        e = errmsg(4305,i,H1,H1A,H2,H3,H4,H5,H6,H7,H8,H9,H0,H33,H40)
      endif;
      { change }

      {* Checking vaccination received against the date of vaccination *}
      {* BCG *}
      if (H2  = 1 & (H2d  = notappl | H2m  = notappl | H2y  = notappl)) |
         (H2 <> 1 & (H2d <> notappl | H2m <> notappl | H2y <> notappl)) then
        e = errmsg(4306,i,2,H2,H2d,H2m,H2y)
      endif;
      {* DPT 1 *}
      if (H3  = 1 & (H3d  = notappl | H3m  = notappl | H3y  = notappl)) |
         (H3 <> 1 & (H3d <> notappl | H3m <> notappl | H3y <> notappl)) then
        e = errmsg(4306,i,3,H3,H3d,H3m,H3y)
      endif;
      {* POLIO 1 *}
      if (H4  = 1 & (H4d  = notappl | H4m  = notappl | H4y  = notappl)) |
         (H4 <> 1 & (H4d <> notappl | H4m <> notappl | H4y <> notappl)) then
        e = errmsg(4306,i,4,H4,H4d,H4m,H4y)
      endif;
      {* DPT 2 *}
      if (H5  = 1 & (H5d  = notappl | H5m  = notappl | H5y  = notappl)) |
         (H5 <> 1 & (H5d <> notappl | H5m <> notappl | H5y <> notappl)) then
        e = errmsg(4306,i,5,H5,H5d,H5m,H5y)
      endif;
      {* POLIO 2 *}
      if (H6  = 1 & (H6d  = notappl | H6m  = notappl | H6y  = notappl)) |
         (H6 <> 1 & (H6d <> notappl | H6m <> notappl | H6y <> notappl)) then
        e = errmsg(4306,i,6,H6,H6d,H6m,H6y)
      endif;
      {* DPT 3 *}
      if (H7  = 1 & (H7d  = notappl | H7m  = notappl | H7y  = notappl)) |
         (H7 <> 1 & (H7d <> notappl | H7m <> notappl | H7y <> notappl)) then
        e = errmsg(4306,i,7,H7,H7d,H7m,H7y)
      endif;
      {* POLIO 3 *}
      if (H8  = 1 & (H8d  = notappl | H8m  = notappl | H8y  = notappl)) |
         (H8 <> 1 & (H8d <> notappl | H8m <> notappl | H8y <> notappl)) then
        e = errmsg(4306,i,8,H8(i),H8d(i),H8m(i),H8y(i))
      endif;
      {* Measles *}
      if (H9  = 1 & (H9d  = notappl | H9m  = notappl | H9y  = notappl)) |
         (H9 <> 1 & (H9d <> notappl | H9m <> notappl | H9y <> notappl)) then
        e = errmsg(4306,i,9,H9,H9d,H9m,H9y)
      endif;
      {* POLIO 0 *}
      if (H0  = 1 & (H0d  = notappl | H0m  = notappl | H0y  = notappl)) |
         (H0 <> 1 & (H0d <> notappl | H0m <> notappl | H0y <> notappl)) then
        e = errmsg(4306,i,0,H0,H0d,H0m,H0y)
      endif;
      {* Vitamin A *}
      if (H33 = 1 & (H33d = notappl | H33m = notappl | H33y = notappl)) |
         (H33<> 1 & (H33d<> notappl | H33m<> notappl | H33y<> notappl)) then
        e = errmsg(4306,i,33,H33,H33d,H33m,H33y)
      endif;
      {* Vitamin A 2nd dose*}
      if (H40 = 1 & (H40d = notappl | H40m = notappl | H40y = notappl)) |
         (H40<> 1 & (H40d<> notappl | H40m<> notappl | H40y<> notappl)) then
        e = errmsg(4306,i,40,H40,H40d,H40m,H40y)
      endif;

      { Check dates of Vaccinations }

      iday = cdc(V016,V006,V007,0);  {* code cdc of interview date *}
      bday = cdc(1,B1(i),B2(i),0);   {* code cdc of birth date     *}
      flagv = notappl;               {* Flag error on vaccination  *}

      if H2 = 1 then              {* BCG vaccin *}
        cdcb = cdc(H2d,H2m,H2y,bday);
        if cdcb < bday | cdcb > iday then
        {* BCG before day of birth or after the day of interview *}
        {*** Why compute month, day, year from bday? -na ***}
          x = cdc2date(bday);
          e = errmsg(4307,i,2,H2,H2d,H2m,H2y,cdcb,day,month,year,bday,B1(i),B2(i));
          flagv = 2;
        {*** Should BCG day be taken as start date for the following
             vaccinations? -na                                                ??
         ***}
        endif
      endif;

      {* Checking DPT dates *}
      dday = bday;
      if H3(i) = 1 then              {* DPT 1 vaccin *}
        cdcd = cdc(H3d,H3m,H3y,dday);
        if cdcd < bday | cdcd > iday then
        {* DPT 1 before day of birth? or after the day of interview *}
        {*** Should DPT 1 be after BCG/POILO 0?     -na ***}
        {*** Why compute month, day, year from dday=bday? -na ***}
          x = cdc2date(dday);
          e = errmsg(4307,i,3,H3,H3d,H3m,H3y,cdcd,day,month,year,dday,B1(i),B2(i));
          flagv = 3;
        else
          dday = cdcd;
        endif
      endif;
      if H5 = 1 then              {* DPT 2 vaccin *}
        cdcd = cdc(H5d,H5m,H5y,dday);
        if cdcd < dday | cdcd > iday then       {*** dday >= bday ***}
        {* DPT 2 before DPT 1 or after the day of interview *}
          x = cdc2date(dday);
          e = errmsg(4307,i,5,H5,H5d,H5m,H5y,cdcd,day,month,year,dday,B1(i),B2(i));
          flagv = 5;
        elseif cdcd < dday+7 then
        {* DPT 1 & DPT 2 are one week or less apart *}
          kount(10) = kount(10) + 1
        else
          dday = cdcd;
        endif
      endif;
      if H7 = 1 then              {* DPT 3 vaccin *}
        cdcd = cdc(H7d,H7m,H7y,dday);
        if cdcd < dday | cdcd > iday then
        {* DPT 3 before DPT 2 or after the day of interview *}
          x = cdc2date(dday);
          e = errmsg(4307,i,7,H7,H7d,H7m,H7y,cdcd,day,month,year,dday,B1(i),B2(i));
          flagv = 7;
        elseif cdcd < dday+7 then
        {* DPT 3 & DPT 2 are one week or less apart *}
          kount(10) = kount(10) + 1
        else
          dday = cdcd;
        endif
      endif;

      {* Checking Polio dates *}
      pday = bday;
      if H0 = 1 then              {* POLIO 0 vaccin *}
        cdcp = cdc(H0d,H0m,H0y,pday);
        if cdcp < bday | cdcp > iday then                                    {* -na- *}
        {* POLIO 0 before BCG or after the day of interview *}
          x = cdc2date(pday);
          e = errmsg(4307,i,4,H0,H0d,H0m,H0y,cdcp,day,month,year,pday,B1(i),B2(i));
          flagv = 0;
        else
          pday = cdcp;
        endif
      endif;
      if H4 = 1 then              {* POLIO 1 vaccin *}
        cdcp = cdc(H4d(i),H4m(i),H4y(i),pday);
        if cdcp < pday | cdcp > iday then
        {* POLIO 1 before POLIO 0 or after the day of interview *}
          x = cdc2date(pday);
          e = errmsg(4307,i,0,H4,H4d,H4m,H4y,cdcp,day,month,year,pday,B1(i),B2(i));
          flagv = 4;
        else
          pday = cdcp;
        endif
      endif;
      if H6 = 1 then              {* POLIO 2 vaccin *}
        cdcp = cdc(H6d,H6m,H6y,pday);
        if cdcp < pday | cdcp > iday then                                    {* -na- *}
        {* POLIO 2 before POLIO 1 or after the day of interview *}
          x = cdc2date(pday);
          e = errmsg(4307,i,6,H6,H6d,H6m,H6y,cdcp,day,month,year,pday,B1(i),B2(i));
          flagv = 6;
        elseif cdcp < pday+7 then
        {* POLIO 1 & POLIO 2 are one week or less apart *}
          kount(10) = kount(10) + 1
        else
          pday = cdcp;
        endif
      endif;
      if H8 = 1 then              {* POLIO 3 vaccin *}
        cdcp = cdc(H8d,H8m,H8y,pday);
        if cdcp < pday | cdcp > iday then                                    {* -na- *}
        {* POLIO 3 before POLIO 2 or after the day of interview *}
          x = cdc2date(pday);
          e = errmsg(4307,i,8,H8,H8d,H8m,H8y,cdcp,day,month,year,pday,B1(i),B2(i));
          flagv = 8;
        elseif cdcp < pday+7 then
        {* POLIO 3 & POLIO 2 are one week or less apart *}
          kount(10) = kount(10) + 1
        else
          pday = cdcp;
        endif
      endif;
      if H9 = 1 then              {* MEASLES vaccin *}
        cdcm = cdc(H9d,H9m,H9y,bday);
        if cdcm < bday | cdcm > iday then
        {* MEASLES before birth day or after the day of interview *}
          x = cdc2date(bday);
          e = errmsg(4307,i,9,H9,H9d,H9m,H9y,cdcm,day,month,year,bday,B1(i),B2(i));
          flagv = 8;
        endif
      endif;
      if H33 = 1 then              {* VITAMIN A vaccin *}
        cdcva = cdc(H33d,H33m,H33y,bday);
        if cdcva < bday | cdcva > iday then
        {* VITAMIN A before birth day or after the day of interview *}
          x = cdc2date(bday);
          e = errmsg(4307,i,33,H33,H33d,H33m,H33y,cdcva,day,month,year,bday,B1(i),B2(i));
          flagv = 33;
        endif
      endif;
      if H40 = 1 & H33 = 1 then              {* VITAMIN A 2nd dose vaccin *}
        cdcva2 = cdc(H40d,H40m,H40y,bday);
        if cdcva2 < cdcva | cdcva2 > iday then
        {* VITAMIN A before birth day or after the day of interview *}
          x = cdc2date(bday);
          e = errmsg(4307,i,40,H40,H40d,H40m,H40y,cdcva2,day,month,year,bday,B1(i),B2(i));
          flagv = 40;
        endif
      endif;
      {* Vaccination's table printout *}
      if !special(flagv) then
        e = errvac( i );
      endif;

      if H1 <> notappl & (H1 = 1 <=> H10 <> notappl) then
        e = errmsg(4308,i,H1,H10)
      endif;

      if (H35  = 1 & (H36A  = notappl | H36B  = notappl | H36C  = notappl |
            H36D  = notappl | H36E  = notappl | H36F  = notappl)) |
        (H35 <> 1 & (H36A <> notappl | H36B <> notappl | H36C <> notappl |
                     H36D <> notappl | H36E <> notappl | H36F <> notappl)) then
        e = errmsg(4325,i,H35,H36A,H36B,H36C,H36D,H36E,H36F);
      endif;
      if H35 = 1 <=> (H36A <> 1 & H36B <> 1 & H36C <> 1 & H36D <> 1 & H36E <> 1 & H36F <> 1) then
        e = errmsg(4327,i,H35,H36A,H36B,H36C,H36D,H36E,H36F)
      endif;
      { End of Fever }
      if H33 in 1,3 <=> H41A = notappl then
        e = errmsg(4328,i,H33,H41A)
      endif;
    endif; // if ok
  enddo; // REC43

{ ---------------------------------------------------------------------------- }
PROC REC4A_EDT
preproc

  for i in REC4A_EDT do

                                                                  { Set unused variables to <> notappl !!}
    if special(HIDXA) then
      e = errmsg(12101,i,"REC4A");
      errmsg("not special: HIDXA=%d",HIDXA);
    endif;

    ok = 1;
    if i <> HIDXA then
      e = errmsg(4302,i,HIDXA); ok=0
    endif;
    if i > sOccurs(REC21) then
      e = errmsg(4303,i,sOccurs(REC21)); ok=0
    else
      if B19(i) > Hmax then
        e = errmsg(4304,i,V008,B19(i),B5(i)); ok=0
      endif
    endif;

    if ok then
      if (B5(i) <=> H11 = notappl) |
         (B5(i) <=> H22 = notappl) |
         (B5(i) <=> H31 = notappl) then
        e = errmsg(4312,i,B5(i),H11,H22,H31)
      endif;

      if B5(i) then       { Living children }

        { Diarrhea }
        v = 0;                                 { comment out unused variables !!}
        v = v + (H12Y = notappl);
        v = v + (H13  = notappl);
        v = v + (H14  = notappl);
        v = v + (H15  = notappl);
        v = v + (H15A = notappl);
        v = v + (H15B = notappl);
        v = v + (H15C = notappl);
        v = v + (H15D = notappl);
        v = v + (H15E = notappl);
        v = v + (H15F = notappl);
        v = v + (H15G = notappl);
        v = v + (H15H = notappl);
        v = v + (H20  = notappl);
        v = v + (H21A = notappl);
        v = v + (H21  = notappl);
        v = v + (H38  = notappl);
        v = v + (H39  = notappl);
        usedv = 17-0;                  { Substract number of unused variables !!}

        if ((H11  = 1 | H11  = 2) & v) |
           ((H11 <> 1 & H11 <> 2) & (v < usedv)) then
          e = errmsg(4309,i,H11,H12Y,H13,H14,
                            H15,H15A,H15B,H15C,H15D,
                            H15E,H15F,H15G,H15H,
                            H20,H21A,H21,H38,H39)
        elseif (H11 = 1 | H11 = 2) &
           (((H12Y = 0 & H12Z <> missing) |
             (H13  = 1 | H13  = 2) |
             (H14  = 1 | H14  = 2) | H15  = 1 |
              H15A = 1 | H15B = 1  | H15C = 1 | H15D = 1 |
              H15E = 1 | H15F = 1  | H15G = 1 | H15H = 1 | H15I = 1 |
              H15J = 1 | H15K = 1  | H15L = 1 | H15M = 1 |
              H20  = 1
            ) <=> H21 <> 1
           ) then
          e = errmsg(4313,i,H11,H12Y,H13,H14,H15,H15A,H15B,H15C,H15D,
                            H15E,H15F,H15G,H15H,H15I,H15J,H15K,H15L,H15M,H20,H21)
        endif;

        {* Sources of advice and treatment for diarrhea *}
                                                        { Comment out unused variables !!}
        k  = 0;     { Count of applicable questions }
        x  = 0;     { Count of yes }
        x1 = 0;     { Count of missing }
        x2 = 0;     { Count of notappl }
        e = AddVals( H12A );
        e = AddVals( H12B );
        e = AddVals( H12C );
        e = AddVals( H12D );
        e = AddVals( H12E );
        e = AddVals( H12F );
        e = AddVals( H12G );
        e = AddVals( H12H );
        e = AddVals( H12I );
        e = AddVals( H12J );
        e = AddVals( H12K );
        e = AddVals( H12L );
        e = AddVals( H12M );
        e = AddVals( H12N );
        e = AddVals( H12O );
        e = AddVals( H12P );
        e = AddVals( H12Q );
        e = AddVals( H12R );
        e = AddVals( H12S );
        e = AddVals( H12T );
        e = AddVals( H12U );
        e = AddVals( H12V );
        e = AddVals( H12W );
        e = AddVals( H12X );
                                                {* Add country specific responses !!*}
        if ( x & (x1 | H12Y <> 0)) |                   { Some yes => H12Y=no, no missings }
           (!x & !x1 & x2 <> k & H12Y <> 1) |          { No yes, no missings => H12Y = yes }
           (x1 & (x1+x2 <> k | H12Y <> missing)) |     { Some missing => all missing, H12Y = missing }
           (x2 = k & H12Y <> notappl) then             { All notappl => H12Y = notappl }
          e = errmsg(4317,i,H12Y,x,x1,x2);
          if  x & (x1 | H12Y <> 0) then
            errmsg("Some yes => H12Y=no, no missings");
          endif;
          if !x & !x1 & x2 <> k & H12Y <> 1 then
            errmsg("No yes, no missings => H12Y = yes");
          endif;
          if x1 & (x1+x2 <> k | H12Y <> missing) then
            errmsg("Some missing => all missing, H12Y = missing");
          endif;
          if x2 = k & H12Y <> notappl then
            errmsg("All notappl => H12Y = notappl");
          endif;
        endif;

        nbans = x;         {count of yes}
        {*ART Source for diarrhea treatment mentioned => 1st source should be applicable - structure check }
        if !(nbans = 0 and H44A = missing) then
          if nbans >= 1 <=> H44A = notappl then
            e = errmsg(4331,i,nbans,H44A)
          endif;
         endif;
        {*ART No source for diarreha treatment mentioned => start of treatment should be applicable - structure check }
        if !(nbans = 0 and H44B = missing) then
          if nbans = 0 <=> H44B <> notappl then
            e = errmsg(4332,i,nbans,H44B)
          endif;
        endif;
                                                                                                                          { Comment out unused variables }
        k  = 0;     { Count of applicable questions }
        x  = 0;     { Count of yes }
        x1 = 0;     { Count of missing }
        x2 = 0;     { Count of notappl }
        e = AddVals( H15  );
        e = AddVals( H15A );
        e = AddVals( H15B );
        e = AddVals( H15C );
        e = AddVals( H15D );
        e = AddVals( H15E );
        e = AddVals( H15F );
        e = AddVals( H15G );
        e = AddVals( H15H );
        e = AddVals( H15I );
        e = AddVals( H15J );
        e = AddVals( H15K );
        e = AddVals( H15L );
        e = AddVals( H15M );
        e = AddVals( H20  );
                                           {* Add country specific responses !!*}
        if (x & (x1 | H21A <> 0)) |                          { Some yes => H21A=no, no missings }
           (!x & !x1 & x2 <> k & H21A <> 1 & H21A <> 8) |    { No yes, no missings => H21A = yes }
           (x1 & (x1+x2 <> k | H21A <> missing)) |           { Some missing => all missing, H21A = missing }
           (x2 = k & H21A <> notappl) then                   { All notappl => H21A = notappl }
          e = errmsg(4321,i,H21A,H13,H14,x,x1,x2)
        endif;

        {!! H44C not present since DHS V !!
        if H11 in 1:2 <=> H44C = notappl then
          e = errmsg(4334,i,H11,H44C)
        endif;
        }

        { End of Diarrhea }

        { Cough }
        {*ART Cough in last 2 weeks, but H31B: ARI not given - structure check *}
  {!! todo - fix this check
    if (H31 = 1 | H31 = 2) <=> H31B = notappl then
      e = errmsg(4318,i,H31,H31B)
        endif;
  }


        k  = 0;     { Count of applicable questions }
        x  = 0;     { Count of yes }
        x1 = 0;     { Count of missing }
        x2 = 0;     { Count of notappl }
        e = AddVals( H32A );
        e = AddVals( H32B );
        e = AddVals( H32C );
        e = AddVals( H32D );
        e = AddVals( H32E );
        e = AddVals( H32F );
        e = AddVals( H32G );
        e = AddVals( H32H );
        e = AddVals( H32I );
        e = AddVals( H32J );
        e = AddVals( H32K );
        e = AddVals( H32L );
        e = AddVals( H32M );
        e = AddVals( H32N );
        e = AddVals( H32O );
        e = AddVals( H32P );
        e = AddVals( H32Q );
        e = AddVals( H32R );
        e = AddVals( H32S );
        e = AddVals( H32T );
        e = AddVals( H32U );
        e = AddVals( H32V );
        e = AddVals( H32W );
        e = AddVals( H32X );
                                           {* Add country specific responses !!*}
        if ( x & (x1 | H32Y <> 0)) |                   { Some yes => H32Y=no, no missings }
           (!x & !x1 & x2 <> k & H32Y <> 1) |          { No yes, no missings => H32Y = yes }
           (x1 & (x1+x2 <> k | H32Y <> missing)) |     { Some missing => all missing, H32Y = missing }
           (x2 = k & H32Y <> notappl) then             { All notappl => H32Y = notappl }
          e = errmsg(4320,i,H32Y,x,x1,x2)
        endif;

        nbans = x;                   { count of yes }
        {*ART Treatment for fever/ARI, but first treatment not applicable - structure check *}
        if !(nbans = 0 and H46A = notappl) then
          if nbans >= 1 <=> H46A = notappl then
            e = errmsg(4335,i,nbans,H46A)
          endif;
        endif;
        {*ART No treatment for fever/ARI, but days when treatment started given - structure check *}
        if !(nbans = 0 and H46B = missing) then
          if nbans = 0 <=> H46B <> notappl then
            e = errmsg(4336,i,nbans,H46B)
          endif;
        endif;


        { Fever }
        {*ART Fever in last 12 weeks, but H37Y: drugs taken not given - structure check *}
        { !!! DHS7 fever only !!!}
        { !! change H37 -> ML13 if using Malaria Module !!}
        if  (H31B = 1 | H22 = 1)  <=> ML13Y(i) = notappl then
          e = errmsg(4324,i,H31B,H22,ML13Y(i))
        endif;
        { End of Fever/Cough }
                                                                                                                          { Comment out unused variables }
        k  = 0;     { Count of applicable questions }
        x  = 0;     { Count of yes }
        x1 = 0;     { Count of missing }
        x2 = 0;     { Count of notappl }
        e = AddVals( H37A );
        e = AddVals( H37B );
        e = AddVals( H37C );
        e = AddVals( H37D );
        e = AddVals( H37E );
        e = AddVals( H37F );
        e = AddVals( H37G );
        e = AddVals( H37H );
        e = AddVals( H37I );
        e = AddVals( H37J );
        e = AddVals( H37K );
        e = AddVals( H37L );
        e = AddVals( H37M );
        e = AddVals( H37N );
        e = AddVals( H37O );
        e = AddVals( H37P );
        e = AddVals( H37X );
                                                              {* Add CS variables !!}
        if ( x & (x1 | H37Y <> 0 | H37Z <> 0)) |                     { Some yes => H37Y=no or H37Z=no, no missings }
           (!x & !x1 & x2 <> k & H37Y <> 1 & H37Z <> 1) |            { No yes, no missings => H37Y = yes or H37Z=yes }
           (x1 & (x1+x2 <> k | H37Y <> missing | H37Z <> missing)) | { Some missing => all missing, H37Y = missing & H37Z = missing}
           (x2 = k & (H37Y <> notappl | H37Z <> notappl)) then       { All notappl => H37Y = notappl & H37Z = notappl }
          e = errmsg(4326,i,H37Y,H37Z,x,x1,x2);
          if ( x & (x1 | H37Y <> 0 | H37Z <> 0)) then
             errmsg("Some 'yes' => H37Y should be 'no' or H37Z should be 'no' and no 'missings'");
          elseif (!x & !x1 & x2 <> k & H37Y <> 1 & H37Z <> 1) then
             errmsg("No 'yes' and no 'missings' => H37Y should be 'yes' or H37Z should be 'yes'");
          elseif (x1 & (x1+x2 <> k | H37Y <> missing | H37Z <> missing)) then
             errmsg("Some missing => all missing, also H37Y = missing and H37Z = missing");
          elseif (x2 = k & (H37Y <> notappl | H37Z <> notappl)) then
             errmsg("All notappl => H37Y = notappl & H37Z = notappl");
          endif;
        endif;

        if H15E = 1 <=> H45 = notappl then
          e = errmsg(4333,i,H15E,H45)
        endif;

      endif               { Living children }
    endif;
  enddo;

{ ---------------------------------------------------------------------------- }
PROC REC44_EDT

  if anthro then  { Anthropometry }

    for i in REC44_EDT do
                                                                  { Set unused variables to <> notappl !!}
      if special(HWIDX) |
         HW1  = missing |
         HW13 = notappl |
         HW17 = missing | HW18 = missing | HW19 = missing {|
         HW51 = missing  HW51 can be missing }
       then
         e = errmsg(12101,i,"REC44");
         errmsg("not special: HWIDX=%d",HWIDX);
         errmsg("not missing: HW1=%d,HW17=%d,HW18=%d,HW19=%d,HW51=%d",HW1,HW17,HW18,HW19,HW51);
         errmsg("not notappl: HW13=%d",HW13);
      endif;

      ok = 1;
      {* Check that the index for the H/W section is correct *}
      if i <> HWIDX then
        e = errmsg(4402,i,HWIDX); ok=0
      endif;
      if i > sOccurs(REC21) then
        e = errmsg(4403,i,sOccurs(REC21)); ok=0
      else
        if B19(i) < HWmin | B19(i) > HWmax then
          e = errmsg(4404,i,HW1,HV008,B19(i),B5(i),B9(i)); ok=0
        endif
      endif;

      if ok then

        if anthroHH then
          j = 1;
          found = 0;
          while !found & j <= sOccurs(RECH6) do
            if B16(HWIDX) = HC0(j) then
              found = j
            endif;
            j = j + 1
          enddo;
          if found <=> (HW13 = 7 | HW13 = 1) then
            e = errmsg(4412,i,found,HW13)
          endif;
          if found & HW1 <> HC1(found) then
            e = errmsg(4413,i,HW1,found,HC1(found))
          endif;
        endif;

        if B16(i) in 1:90 & V042 = 1 then {* Child in the HH *}
          {* Check H/W variables and living status of the child *}
          if (B5(i) <=> HW1  = notappl) |
             (B5(i) <=> HW2  = notappl) |
             (B5(i) <=> HW3  = notappl) |
             (B5(i) <=> HW13 = 1      ) |
             (B5(i) <=> ( HW15 = notappl & HW13 = 0 )) |
             (B5(i) <=> HW16 = notappl){|
             ((!special(HW2) | !special(HW3)) <=> HW17 = notappl) |
             ((!special(HW2) | !special(HW3)) <=> HW18 = notappl) |
             ((!special(HW2) | !special(HW3)) <=> HW19 = notappl) } then
            e = errmsg(4407,i,B5(i),HW1,HW2,HW3,HW13,HW15,HW16,HW17,HW18,HW19)
          endif;
        endif;

        if B16(i) in 1:90 & B5(i) then {* Living children in the HH *}

          {* H/W age differs from the birth history, because month interview HH < month interview resp. -> note in doc *}
          if (HW1 <> B19(i)) then
            e = errmsg(4414,i,HW1,B19(i),B5(i),B9(i),HV008,V008)
          endif;

          if((validresp(HW2) & validresp(HW3) & B10(i)  = 1) &
             (HW4  = missing | HW5  = missing | HW6  = missing | HW7  = missing | HW8  = missing | HW9  = missing |
              HW70 = missing | HW71 = missing | HW72 = missing | HW73 = missing)
            ) |
            ((!validresp(HW2) & !validresp(HW3) & B10(i)  = 1) &
             (HW4 <> missing | HW5  <> missing | HW6  <> missing | HW7  <> missing | HW8 <> missing | HW9 <> missing |
              HW70<> missing | HW71 <> missing | HW72 <> missing | HW73 <> missing)
            ) |
            ((validresp(HW2) & validresp(HW3)) &
             (HW10  = missing | HW11  = missing | HW12  = missing)
            ) |
            ((!validresp(HW2) & validresp(HW3)) &
             (HW10 <> missing | HW11 <> missing | HW12 <> missing)
            ) then
            e = errmsg(4405,i,HW2,HW3,B10(i),HW4,HW5,HW6,HW7,HW8,HW9,HW10,HW11,HW12,HW70,HW71,HW72,HW73)
          endif;

          { Thus in DHS5: when HW13 <> 0 (measurement not taken) then HW2 and/or HW3 should be missing }
          { In DHS6 the code combinations are more complex. I would have taken 994, 995 and 996 out of HW2 and HW3, since
            they are given in HW13, and keep the old rule -> both measurements there then measurement taken }
          { OLD - but should be valid again
          if (special(HW2) | special(HW3)) <=> HW13 = 0 then
            e = errmsg(4406,i,HW2,HW3,HW13)
          endif;
          }

          if HW2 in notappl,missing,994,995,996 | HW3 in notappl,missing,9994,9995,9996 <=> !HW13 in 1,3,4,6,notappl,missing then
            e = errmsg(4406,i,HW2,HW3,HW13)
          endif;

          { Warning may generate many messages }
          {* Check that children under 24 months are measured lying and standing for the others. *}
          if (HW1 < 24 & HW15 = 2) | (HW1 >= 24 & HW15 = 1) then
          {*e = errmsg(4409,i,HW1,HW15);{: No instruction given }??*}
            kount(5) = kount(5) + 1
          endif;

        endif;                         {* Living children in the HH *}

        if V042 = 1 & HW1 >= 6 then       { Hemoglobin measurement for children 6 months and older }
          if HW52 = 2 <=> HW55 <> 4 then
            e = errmsg(4415,i,HW52,HW55)
          endif;

          { if HW55 = 0 <=> special(HW53) then   NOT CONSIS6 }
          if HW55 in 996,994,995 <=> !HW53 in 3,4,6 then
            e = errmsg(4416,i,HW53,HW55)
          endif;

          if !special(HW56) <=> HW55 <> 0 then
            e = errmsg(4417,i,HW55,HW56)
          endif;
        else
          { For HW55, HW56 and HW57 this leads to an inconsistenty since they are derived from HC55, HC56 and HC57,
            who are set to missing for skipped cases !!!! BERT -> HW55, HW56 and HW57 reset to NA to conform to consis }
          { Guillermo: Currently HW55 does not adhere to >5 and < 60 age ristriction in recode application -> CONSIS6}
          if HW51 <> notappl | HW52 <> notappl | HW53 <> notappl | HW55 <> notappl |
             HW56 <> notappl | HW57 <> notappl | HW58 <> notappl then
            e = errmsg(4418,i,HW51,HW52,HW53,HW55,HW56,HW57,HW58)
          endif;
        endif;

      endif;

    enddo;
  endif;  { Anthropometry }

{ ---------------------------------------------------------------------------- }
PROC REC51_EDT
{*** Done. Check recode of V536 Lastsex <= 3 rather than lastsex <= 4 ????????}

                                                                { Set unused variables to <> notappl !!}
  if V501 = notappl| V502 = notappl|
     V507 = missing| V508 = missing| V509 = missing| V510 = missing|
     V511 = missing| V512 = missing| special(V513) | V525 = notappl|
     V530 = missing|
     V531 = notappl|
     V532 = missing|
     V536 = notappl
   then
     e = errmsg(12000,"REC51");
     errmsg("not special: V513=%d",V513);
     errmsg("not missing: V507=%d,V508=%d,V509=%d,V510=%d,V511=%d,V512=%d,V530=%d,V532=%d",V507,V508,V509,V510,V511,V512,V530,V532);
     errmsg("not notappl: V501=%d,V502=%d,V525=%d,V531=%d,V536=%d",V501,V502,V525,V531,V536);
  endif;

  {* Ever married sample but never married *}
  if V020 & V501 = 0 then
    e = errmsg(5100,V501,V020)
  endif;

  {* Household marital status => marital status ind. quest. - no error in recode then Note in DOC *}
  x = HV115(V003);
  if x <> notappl then
    if (V501 = 0 & x <> 0) |            { never married }
       (V501 in 1,2 & x <> 1) |         { married + living together }
       (V501 = 3 & x <> 3) |            { widowed }
       (V501 in 4,5 & x <> 4) then      { divorced + separated }
      e = errmsg(5121,V501,V003,x)
    endif;
  endif;

  x = HV116(V003);
  if x <> notappl & x <> V502 then
    e = errmsg(5122,V502,V003,x)
  endif;
  {* Recode of respondent marital status *}
  box V501 => x;
         0 => 0;
       1,2 => 1;
           => 2;
  endbox;
  if x <> V502 then
    e = errmsg(5101,V501,V502)
  endif;
  if (V501 <> 0 &
      (V503  = notappl | V507  = notappl | V508  = notappl | V509  = notappl |
       V510  = notappl | V511  = notappl | V512  = notappl | V513  = 0)
     ) |
     (V501  = 0 &
      (V503 <> notappl | V507 <> notappl | V508 <> notappl | V509 <> notappl |
       V510 <> notappl | V511 <> notappl | V512 <> notappl | V513 <> 0)
     ) then
    e = errmsg(5102,V501,V502,V503,V507,V508,V509,V510,V511,V512,V513)
  endif;

  if (V502 = 1 <=> V504 = notappl) |
     (!calused & (V502 = 1 <=> V505 = notappl)) then
    e = errmsg(5103,V502,V504,V505)
  endif;
  if (V506 <> notappl) <=> (V505 = notappl | V505 = 0) then
    e = errmsg(5104,V505,V506)
  endif;
  {* Date of union *}
  if V509 <> notappl & V509 <> cmcode(V507,V508) then
    e = errmsg(5105,V507,V508,V509)
  endif;
  {* Before minimum age at marriage or after the interview -> note in doc *}
  if V509 < V011+minam | V509 > V008 then
    e = errmsg(5106,V509,V008,V011,minam)
  endif;
  {* Age at first marriage *}
  if V511 <> int((V509-V011)/12) then
    e = errmsg(5107,V509,V511,V011)
  endif;
  {* Years since first marriage *}
  if V512 <> int((V008-V509)/12) then
    e = errmsg(5108,V509,V512,V008)
  endif;
  {* Marital duration grouped *}
  x = int(V512/5)+1;
  if V512 = notappl then
    x = 0
  elseif x > 7 then
    x = 7
  endif;
  if x <> V513 then
    e = errmsg(5109,V512,V513)
  endif;
  {* Never had sex *}
  if V525 = 0 then
    if V201 then
      {* But has children *}
      e = errmsg(5116,V525,V201,V501)
    elseif V502 then
      {* But ever been married *}
      kount(14) = kount(14) + 1;
    endif
  endif;
  if (V525  = 0 &
      (V527 <> notappl | V528 <> notappl | V529 <> notappl |
       V530 <> notappl | V531 <> 0       | V532 <> 0
     ))|
     (V525 <> 0 &
      (V527  = notappl | V528  = notappl | V529  = notappl |
       V530  = notappl | V531  = 0       | V532  = notappl
     ))then
    e = errmsg(5117,V525,V527,V528,V529,V530,V531,V532)
  endif;

  {* Age at first sex > age at first birth, > age of respondent. -> Note in doc *}
  if ( V525 in 1:49 ) & ( (V212 <> notappl & V525 > V212) | (V525 > V012) ) then
    e = errmsg(5118,V525,V212,V012,V531,V532);
  endif;

  box V527 => x;
   notappl => notappl;
   199,299 => missing;
   399,499 => missing;
   100-130 => V527-100;
   131-198 => 31;
   200-204 => (V527-200)*7;
   205-499 => 31;
   990-999 => V527-900;
   missing => missing;
  endbox;
  if x <> V528 then
    e = errmsg(5120,V527,V528)
  endif;

  x = int(V527/100);
  x1 = V527%100;
  box x => x;
      1 => int(x1/30);
      2 => int(x1/4.3);
      3 => x1;
      4 => x1*12;
      9 => 900 + x1;
  endbox;
  if V530 = 9 then
    x = 996
  elseif !(V530 in 0, notappl) then
    x = 997
  elseif x1 = 99 then
    x = missing
  endif;
  if x <> V529 then
    e = errmsg(5123,V527,V528,V529,x,V530)
  endif;

  x = V525;
  if x = 96 then
    x = V511
  endif;
  if V532 = 3 then
    x = x - 1
  elseif !(V532 in 0, 6) then
    x = 97
  endif;
  if x <> V531 then
    e = errmsg(5124,V525,V531,x,V532)
  endif;

  if (V535 = notappl <=> V502 <> 1) then
    e = errmsg(5125,V535,V502)
  endif;
  {-na: Check recode of V536 (if lastsex <= 3 rather than lastsex <= 4) ?? *}
  box V525 : V527    : V406 => lastsex;
         0 :         :      => 0;
           : 199,299 :      => missing;
           : 399,499 :      => missing;
           : missing :      => missing;
           :     997 :      => missing;           { incosistent -> missing  CONSIS6 }
           :     995 :      => 1;
           : 100-127 :      => 1;
  {-na     : 128-134 :      => 1;}
           : 200-203 :      => 1;
  {-na     : 204     :      => 1;}
  {-na     : 300     :      => 1;}
           :         :   1  => 2;
           :         :      => 3;
  endbox;
  if V536 <> missing & lastsex <> V536 then
    e = errmsg(5126,V536,lastsex,V525,V527,V406)
  endif;
  x = missing; if nOccurs(REC41_EDT) then x = M9(1) endif;
  box V536 : V529    => durabst;
         2 :         => x;
         3 : <=60    => V529;
         3 : 60-900  => 60;
         3 : 995-998 => V529-900;
         3 : 999     => missing;
         3 : missing => missing;
           :         => notappl;
  endbox;
  if durabst <> V537 then
    e = errmsg(5127,V537,durabst,V536,V529,x)
  endif;
  { !!! variables V538, V539, V541 not used in DHS7 !!
  if V501 in 4,5 <=> V538 = notappl then
    e = errmsg(5128,V501,V538)
  endif;

  if (V501 = 3 | V538 = 1) <=> V539 = notappl then
    e = errmsg(5129,V501,V538,V539)
  endif;
  if (V501 = 3 | V538 = 1) & (V539 <> 1 <=> V540 = notappl) then
    e = errmsg(5130,V501,V538,V539,V540)
  endif;

  if (V525 = 0 & V013 in 1,2) <=> V541 = notappl then
    e = errmsg(5131,V525,V013,V541)
  endif;
  }

{ ---------------------------------------------------------------------------- }
PROC REC61_EDT

                                                 { Set unused variables to <> notappl !!}
  if V613  = notappl | special(V614)   |
     V627  = notappl | V628  = notappl | V629  = notappl |
     V633A = notappl | V633B = notappl | V633C = notappl | V633D = notappl
   then
     e = errmsg(12000,"REC61");
     errmsg("not special: V614=%d",V614);
     errmsg("not notappl: V613=%d,V627=%d,V628=%d,V629=%d,V633A=%d,V633B=%d,V633C=%d,V633D=%d",V613,V627,V628,V629,V633A,V633B,V633C,V633D);
  endif;

  {* Sterilized *}
  if V602 = 4 <=> (V312 <> 6 & V312 <> 7) then
    e = errmsg(6101,V602,V312)
  endif;
  {* Preferred waiting time *}
  if V602 = 1 <=> V603 = notappl then
    e = errmsg(6104,V602,V603)
  endif;
  {* Check recoding of preferred waiting time *}
  box V603 => x;
   notappl => notappl;
   missing => missing;
   199,299 => missing;
   198,298 => 8;
       998 => 8;
       994 => 0;
     >=900 => 7;
   100-183 => int((V603-100)/12);
   184-190 => 6;
   200-206 => V603-200;
   207-290 => 6;
           => missing;
  endbox;
  if V604 <> x then
    e = errmsg(6105,V603,V604)
  endif;
  {* Check recoding of desire for more children *}
  box V602 : V604 => x;
   notappl :      => notappl;
         1 : 0-1  => 1;
         1 : 2-6  => 2;
         1 :      => 3;
         2 :      => 4;
         3 :      => 5;
         4 :      => 6;
         5 :      => 7;
         6 :      => 8;
   missing :      => missing;
  endbox;
  if x <> V605 then
    e = errmsg(6106,V605,V602,V604)
  endif;
  {* Check recode of ideal number of children *}
  box V613 => x;
       0-6 => V613;
      7-90 => 6;
           => 7;
  endbox;
  if x <> V614 then
    e = errmsg(6109,V613,V614)
  endif;
  {* Time to next birth but does not want another, or
     wants another but can't get pregnant. *}
  if (V616 = notappl | V616 = 995) <=> V602 = 1 then
    e = errmsg(6110,V616,V602)
  endif;

  if (V502 = 1 & V312 <> 7 & V312 <> 6) <=> V621 = notappl then
    e = errmsg(6113,V621,V502,V312)
  endif;

  {* Unmet need *}
  { Code for V623, V624 }
  pregnant = V213;
  amenor   = V405;
  user     = (V312 <> 0);
  whenwant = notappl;
  fail     = 0;
  infert   = 0;

  if pregnant then                    { Determines if current pregnancy}
    box V225 => whenwant;             { or last birth was wanted       }
         1-3 => V225;
             => 2;
    endbox;
  elseif sOccurs(REC41) then
    x = M10(1);
    box x => whenwant;
      1-3 => x;
          => 2;
    endbox;
  endif;

  if col2used then
    i = V018;
    if amenor then                        {* Look for last birth in calendar *}
      { i = V018;  IN WRONG PLACE BEFORE CONSIS6 }
      while i <= vcallen & !pos(VCAL(1)[i:1],"B") do
        i = i + 1;
      enddo;
      i = i + 1;
    endif;
    if pregnant | amenor then             { Determines failure of method }
      while i <= vcallen & pos(VCAL(1)[i:1],"P") do  {* Skip months pregnant *}
        i = i + 1;
      enddo;

      if i <= vcallen & pos(VCAL(1)[i:1],vcalmeth){* Using *}
                      & pos(VCAL(2)[i:1],"1")     {* Became preg. while using *}
       then
        fail = 1;
      endif;
    endif;
  endif;

  x  = int(V215/100);
  x1 = V215 % 100;
  if !pregnant & !amenor then         { Determines infertility }
    box x :    x1   => x;
          : missing => 0;               { Assumes less than 6 months }
          :  94-96  => 99;              { Assumes more than 6 months }
          :   >90   => 0;
        1 :         => int(x1/30);
        2 :         => int(x1/4.3);
        3 :         => x1;
        4 :         => x1*12;
          :         => 0;
    endbox;
    if x >= 6 then infert = 1 endif;
  endif;

  infert2 = infert;

  if col3used then
    i = V018;
    x = 60; if 81 - V018 < x then x = 81 - V018 endif;    { short calendar }
    while i < V018+x & pos(VCAL(3)[i:1],"X") &      { cont. married }
                       pos(VCAL(1)[i:1],"BPT0") do  { not using }
      i = i + 1;
    enddo;
    if i >= V018+x then
      { continuously married and not using for five years }
      if !pos("B",VCAL(1)[V018:x]) & !pregnant then
        infert  = 1;
        infert2 = 1;
      endif;
    endif;
  else
    { na variables so logic removed !!}
  endif;

  if V602 = 5 | V376 = 23 then
    infert2 = 1
  endif;

  box pregnant : amenor : infert => x;
         1     :        :        => 1;  { pregnant }
               :   1    :        => 2;  { Amenorrheic }
               :        :   1    => 3;  { Infertile }
               :        :        => 0;  { Fecund }
  endbox;
  box pregnant : amenor : infert2=> x2;
         1     :        :        => 1;  { pregnant }
               :   1    :        => 2;  { Amenorrheic }
               :        :   1    => 3;  { Infertile }
               :        :        => 0;  { Fecund }
  endbox;

  box V502 : V528    => sex30;
         1 :         => 1;      { Currently married or }
           :  <31    => 1;      { Sex in last month or }
           :   95    => 1;      { Sex in last 4 weeks (special code) }
           : notappl => 0;      { Never had sex }
           :         => 2;      { Not currently married, and }
                                { not had sex in last month  }
  endbox;
  box user  : sex30 : V623 : fail : whenwant : V605 => x1;
        1   :       :      :      :          :  5-7 => 4;   { Users - limit }
        1   :       :      :      :          :      => 3;   { Users - space }
            :       :  1,2 :  1   :    3     :      => 6;   { Fail - limit }
            :       :  1,2 :  1   :          :      => 5;   { Fail - space }
            : 1     :  1,2 :      :    2     :      => 1;   { P/A Need - space }
            : 1     :  1,2 :      :    3     :      => 2;   { P/A Need - limit }
            :       :    3 :      :          :      => 9;   { Infecund }
            : 1     :    0 :      :          :  2-4 => 1;   { Need - space }
            : 1     :    0 :      :          :  5   => 2;   { Need - limit }
            : 0     :      :      :          :      => 0;   { Never had sex }
            : 2     :      :      :          :      => 8;   { No sex last month}
            :       :  1,2 :      :    1     :      => 7;   { P/A No need }
            :       :      :      :          :  1   => 7;   { No need }
            :       :      :      :          :      => missing;
  endbox;

  box user  : sex30 : V625 : fail : whenwant : V605 : V631 => x3;
        1   :       :      :      :          :  5-7 :      => 4;   { Users - limit }
        1   :       :      :      :          :      :      => 3;   { Users - space }
            :       :  1,2 :  1   :    3     :  1-3 :      => 5;   { Fail - space }
            :       :  1,2 :  1   :    3     :      :      => 6;   { Fail - limit }
            :       :  1,2 :  1   :          :      :      => 5;   { Fail - space }
            : 1     :  1,2 :      :    2     :      :      => 1;   { P/A Need - space }
            : 1     :  1,2 :      :    3     :  1-3 :      => 1;   { P/A Need - space }
            : 1     :  1,2 :      :    3     :      :      => 2;   { P/A Need - limit }
            :       :    3 :      :          :      :      => 9;   { Infecund }
            : 1     :    0 :      :          :  2   :      => 1;   { Need - space }
            : 1     :    0 :      :          :  3-4 :    3 => 7;   { No need - no problem }
            : 1     :    0 :      :          :  3-4 :      => 1;   { Need - space }
            : 1     :    0 :      :          :  5   :      => 2;   { Need - limit }
            : 0     :      :      :          :      :      => 0;   { Never had sex }
            : 2     :      :      :          :      :      => 8;   { No sex last month }
            :       :  1,2 :      :    1     :      :      => 7;   { P/A No need }
            :       :      :      :          :  1   :      => 7;   { No need }
            :       :      :      :          :      :      => missing;
  endbox;

  if x <> V623 | x1 <> V624 then
    e = errmsg(6114,V623,x,V624,x1)
  endif;
  if x2 <> V625 | x3 <> V626 then
    e = errmsg(6115,V625,x2,V626,x3)
  endif;
  {* Ideal number of children *}
  if special(V613) | V613 >= 90 then
    if V613 <> V627 | V613 <> V628 | V613 <> V629 then
      e = errmsg(6117,V627,V628,V629,V613)
    endif
  else
    if !special(V627) & V627 < 90 & V613 <> V627+V628+V629 then
      e = errmsg(6117,V627,V628,V629,V613)
    endif
  endif;

  {* Decision maker for using contraception for ever married women not using *}
  if V632 = notappl <=> ((V501 = 1 | V501 = 2) & V312 <> 0) then
    e = errmsg(6121,V632,V501,V312)
  endif;
  { !!! not used in DHS7
  {* woman knows husband is using FP *}
  if V634 in 1,2 & V501 in 1, 2 & V312 in 5, 7, 9 then
    e = errmsg(6122,V634,V501,V312)
  endif;
  }

{ ---------------------------------------------------------------------------- }
PROC REC71_EDT
                                                                { Set unused variables to <> notappl !!}
  if V714  = notappl |
     V716  = notappl |
     V731  = notappl |
     V743A = notappl | V743B = notappl | V743C = notappl | V743D = notappl |
     V743E = notappl |
     V744A = notappl | V744B = notappl | V744C = notappl | V744D = notappl |
     V744E = notappl
   then
     e = errmsg(12000,"REC71");
     errmsg("not notappl: V714=%d,V716=%d,V731=%d,V743A=%d,V743B=%d,V743C=%d,V743D=%d",V714,V716,V731,V743A,V743B,V743C,V743D);
     errmsg("not notappl: V743E=%d,V744A=%d,V744B=%d,V744C=%d,V744D=%d,V744E=%d",V743E,V744A,V744B,V744C,V744D,V744E);
  endif;

  {* partner's age *}
  {* Asked for currently married women *}
  if V730 = notappl <=> V502 = 1 then
    e = errmsg(7101,V730,V502)
  endif;
  {* Different from the age in the HH schedule by more than 2 years *}
  if !special(V034) & V034 > 0 & V034 <= HV009 then
    x = HV105(V034);
    if V730 < x - 2 | V730 > x + 2 then
      e = errmsg(7102,V730,V034,x);
    endif
  endif;
  {* Partner's education *}
  if (!V501 in 1,2 <=> V701 <> notappl) {* Partner's education *}
   | (!V501 in 1,2 <=> V704 <> notappl) {* Partner's occupation *}
   | (!V501 in 1,2 <=> V715 <> notappl) {* Partner's education in single years *}
   | (!V501 in 1,2 <=> V729 <> notappl) {* Partner's educational attainment *}
   then
    e = errmsg(7103,V701,V704,V715,V729,V501)
  endif;

  {* Partner's highest year of education and his education level *}
  if (V701 = notappl | V701 = 0 | V701 = 8 | V701 = missing) <=> V702 <> notappl then
    e = errmsg(7104,V701,V702)
  endif;

  {* V705 (partner's occupation) is a recode of V704, thus should be equivalent *}
  { if special(V704) <=> (V705 <> 0) then     NOT CONSIS6 }
  if special(V704) <=> !special(V705) then
    e = errmsg(7105,"V704",V704,"V705",V705)
  endif;

  {* Respondent not working, but probing question on work in the past skipped *}
  if V714 <> 1 & V714A = notappl then
    e = errmsg(7105,"V714",V714,"V714A",V714A)
  endif;

  {* V717 (Respondent's occupation) is a recode of V716, thus should be equivalent *}
  if (!special(V717) <=> special(V716)) |
     (V717 = 0       <=> V716 <> 0    )  then
    e = errmsg(7105,"V716",V716,"V717",V717)
  endif;

  {* Occupation code = not working (V716 and V717 = 0), but did work or works according to V731 }
  if (V716 = 0 <=> (V731 = 1      {* Worked in the past year *}
                  | V731 = 2      {* Currently working *}
                  | V731 = 3      {* have a job but on leave in the last seven days *}
                   )) then
    e = errmsg(7106,V716,V717,V731)
  endif;

  {* Respondent is currently working *}
  if V731 in 2:3 <=> V714 <> 1 then
    e = errmsg(7107,V731,V714)
  endif;

  {* Working status *}
  if ((V731 in 1:3) <=> V719 = notappl) {* Work for family, dicts. *}
   then
    e = errmsg(7108,V719,V721,V731)
  endif;

  if (V732 = notappl <=> (V731 in 1:3))   {* Seasonality of work *}
   | (V741 = notappl <=> (V731 in 1:3))   {* Type of earnings for work *}
   then
    e = errmsg(7109,V731,V732)
  endif;

  if V502 = 1 & (V739 = notappl <=> (V741 in 1, 2)) then
    e = errmsg(7110,V739,V741)
  endif;

  if V502 = 1 & (V746 = notappl <=> (V741 in 1, 2)) then
    e = errmsg(7115,V746,V741)
  endif;

  if V502 = 1 & V741 in 1,2 & (V746 <> 4 <=> V743F = notappl) then
    e = errmsg(7116,V746,V743F)
  endif;

  {* Works in agriculture vs type of land *}

  if V502 in 1 <=> V743A = notappl |
     V502 in 1 <=> V743B = notappl |
     V502 in 1 <=> V743C = notappl |
     V502 in 1 <=> V743D = notappl |
     V502 in 1 <=> V743E = notappl |
     V502 in 1 <=> (V746 <> 4 & V743F = notappl) then
    e = errmsg(7117,V502,V743A,V743B,V743C,V743D,V743E,V743F)
  endif;
{ ---------------------------------------------------------------------------- }
PROC REC75_EDT
preproc

  if V750 = notappl | V751 = notappl | V766A = notappl | V766B = notappl |
      V785  = notappl                                    {* set to <> when not used !!}
   then
     e = errmsg(12000,"REC75");
     errmsg("not notappl: V750=%d,V751=%d,V766A=%d,V766B=%d",V750,V751,V766A,V766B);
  endif;

    box V751 : V785    => x;
           1 :         => 1;
             : 1       => 1;
     missing :         => missing;
             : missing => missing;
             :         => 0;
    endbox;
    if V750 <> x then
      e = errmsg(7503,V750,x,V751,V785)
    endif;

    if V756 = notappl <=> V751 = 1 then
      e = errmsg(7507,V756,V751)
    endif;

    {* Last intercourse used condom *}
    if ((!special(V527) & V527 < 400) <=> V761 = notappl) then
      e = errmsg(7502,V761,V525,V527)
    endif;
    {* Condom use during intercourse with a woman *}
    if ( ((!special(V766B) & V766B <= 90 & V766B >= 1) &   V761  = notappl)|
         ((!special(V766B) & V766B <= 90 & V766B >= 2) &   V761B = notappl)|
         (   aidsmod &
             ((!special(V766B) & V766B <= 90 & V766B >= 3) &   V761C = notappl) )  )
         |
       ( (V766B = notappl &   V761  <> notappl)|
         (V766B = notappl &   V761B <> notappl)|
         (   aidsmod &
             (V766B = notappl &   V761C <> notappl) )  )
       then
      e = errmsg(7509,V766B,V761,V761B,V761C)
    endif;
    {* Relationship with partner *}
    if ( ((!special(V766B) & V766B <= 90 & V766B >= 1) &   V767A = notappl)|
         ((!special(V766B) & V766B <= 90 & V766B >= 2) &   V767B = notappl)|
         ((!special(V766B) & V766B <= 90 & V766B >= 3) &   V767C = notappl)  )
         |
       ( (V766B = notappl &   V767A <> notappl)|
         (V766B = notappl &   V767B <> notappl)|
         (V766B = notappl &   V767C <> notappl)  )
       then
      e = errmsg(7510,V766B,V767A,V767B,V767C)
    endif;

    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V762AA );
    e = AddVals( V762AB );
    e = AddVals( V762AC );
    e = AddVals( V762AD );
    e = AddVals( V762AE );
    e = AddVals( V762AF );
    e = AddVals( V762AG );
    e = AddVals( V762AH );
    e = AddVals( V762AI );
    e = AddVals( V762AJ );
    e = AddVals( V762AK );
    e = AddVals( V762AL );
    e = AddVals( V762AM );
    e = AddVals( V762AN );
    e = AddVals( V762AO );
    e = AddVals( V762AP );
    e = AddVals( V762AQ );
    e = AddVals( V762AR );
    e = AddVals( V762AS );
    e = AddVals( V762AT );
    e = AddVals( V762AU );
    e = AddVals( V762AV );
    e = AddVals( V762AW );
    e = AddVals( V762AX );
                                            {* Add country specific variables !!}
    if ( x & (x1 | V762AZ <> 0)) |                   { Some yes => V762AZ=no, no missings }
       (!x & !x1 & x2 <> k & V762AZ <> 1) |          { No yes, no missings => V762AZ = yes }
       (x1 & (x1+x2 <> k | V762AZ <> missing)) |     { Some missing => all missing, V762AZ = missing }
       (x2 = k & V762AZ <> notappl) then             { All notappl => V762AZ = notappl }
      e = errmsg(7513,V762AZ,x,x1,x2)
    endif;

    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( V762BA );
    e = AddVals( V762BB );
    e = AddVals( V762BC );
    e = AddVals( V762BD );
    e = AddVals( V762BE );
    e = AddVals( V762BF );
    e = AddVals( V762BG );
    e = AddVals( V762BH );
    e = AddVals( V762BI );
    e = AddVals( V762BJ );
    e = AddVals( V762BK );
    e = AddVals( V762BL );
    e = AddVals( V762BM );
    e = AddVals( V762BN );
    e = AddVals( V762BO );
    e = AddVals( V762BP );
    e = AddVals( V762BQ );
    e = AddVals( V762BR );
    e = AddVals( V762BS );
    e = AddVals( V762BT );
    e = AddVals( V762BU );
    e = AddVals( V762BV );
    e = AddVals( V762BW );
    e = AddVals( V762BX );
                                            {* Add country specific variables !!}
    if ( x & (x1 | V762BZ <> 0)) |                   { Some yes => V762BZ=no, no missings }
       (!x & !x1 & x2 <> k & V762BZ <> 1) |          { No yes, no missings => V762BZ = yes }
       (x1 & (x1+x2 <> k | V762BZ <> missing)) |     { Some missing => all missing, V762BZ = missing }
       (x2 = k & V762BZ <> notappl) then             { All notappl => V762BZ = notappl }
      e = errmsg(7514,V762BZ,x,x1,x2)
    endif;

    {* ART Had an STI infection and never had sex - Note in doc *}
    if (                                       {* Comment out unsed variables !!}
        V763A <> 0
      | V763B <> 0
      | V763C <> 0
  {   | V763D <> 0                                       {* Uncomment if used !!}}
  {   | V763E <> 0                                       {* Uncomment if used !!}}
  {   | V763F <> 0                                       {* Uncomment if used !!}}
  {   | V763G <> 0                                       {* Uncomment if used !!}}
      ) & V525 = 0 then
      e = errmsg(7515,V763A,V763B,V763C,V763D,V763E,V763F,V763G,V525)
    endif;

  { todo
    {*ART Knows place for condoms but V769: place to get condom skipped - structure check *}
    if V762AZ <> missing then       { missing from A629 skips A631 but missing from A630 does not }
    if V762AZ <> 1 <=> V769 = notappl then
      e = errmsg(7516,V762AZ,V769)
        endif;
    endif;
  }

  { todo
    {*ART Knows place for female condom but V769A: place to get female condom skipped - structure check *}
    if V762BZ <> missing then       { missing from A632 skips A634 but missing from A633 does not }
    if V762BZ <> 1 <=> V769A = notappl then
      e = errmsg(7517,V762BZ,V769A)
        endif;
    endif;
  }


    {* Had STI but Sought advice or treatment not applicable -> structure check *}
    if V770 = notappl <=> ( V763A = 1          {* Comment out unused variables !!}
                          | V763B = 1
                          | V763C = 1
  {                       | V763D = 1                    {* Uncomment if used !!}}
  {                       | V763E = 1                    {* Uncomment if used !!}}
  {                       | V763F = 1                    {* Uncomment if used !!}}
  {                       | V763G = 1                    {* Uncomment if used !!}}
                           ) then
      e = errmsg(7518,V763A,V763B,V763C,V763D,V763E,V763F,V763G,0,V770)
    endif;
    {* Sought advice for last disease and where sought advice *}
    {* V770 And V770A-X should be present for all resps. with an STI infection -> structure check *}
    { if (V770 = 1 <=> V770A = notappl) then  NOT CONSIS6 }
    if (V770 <> notappl & V770A = notappl) then
      e = errmsg(7519,V770,V770A)
    endif;
                                                                          { Comment out unused variables !!!}
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes - not used}
      x1 = 0;     { Count of missing - not used}
      x2 = 0;     { Count of notappl }

      e = AddVals( V770A );
      e = AddVals( V770B );
      e = AddVals( V770C );
      e = AddVals( V770D );
      e = AddVals( V770E );
      e = AddVals( V770F );
      e = AddVals( V770G );
      e = AddVals( V770H );
      e = AddVals( V770I );
      e = AddVals( V770J );
      e = AddVals( V770K );
      e = AddVals( V770L );
      e = AddVals( V770M );
      e = AddVals( V770N );
      e = AddVals( V770O );
      e = AddVals( V770P );
      e = AddVals( V770Q );
      e = AddVals( V770R );
      e = AddVals( V770S );
      e = AddVals( V770T );
      e = AddVals( V770U );
      e = AddVals( V770V );
      e = AddVals( V770W );
      e = AddVals( V770X );
                                              {* Add CS variables !!}
      if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
        e = errmsg(7520,i,V770X,x2,k)
      endif;

  { !! not used in DHS7
    {* Knows someone who died of AIDS *}
    if V775 = notappl <=> V751 = 1 then
      e = errmsg(7524,V775,V751)
        endif;
  }
    {* Ever heard of AIDS and questions about AIDS *}
    { V777, V778, V779 do not have NatoZero in recode app, thus the base is respondents who know HIV,
      V781 has NatoZero, thus the base is indeed all respondents -> <=> does not hold }
    if (V781 = notappl & V751 = 1) then                               { CONSIS6 : not <=> }
      e = errmsg(7526,V777,V778,V779,V780,V781,V751)
    endif;
    {* Ever been tested for AIDS and Knows place of test *}
    if V751 = 1 & (V783 = notappl <=> V781 <> notappl ) then
      e = errmsg(7527,V751,V781,V783)
    endif;

    { BERT: Does this here really provide a count of the times the AIDS module is selected? }
    kount(70) = kount(70) + 1;

    box  V527      => lastsex;
         100-199   => 1;
         200-251   => 1;
         300-311   => 1;
         995       => 1;
                   => 0;
    endbox;
    if V201 & V527 = 996 & (V008-B3(1) < 12) then
      lastsex = 1
    endif;
    if !lastsex then
      if V761  <> notappl | V761B <> notappl | V761C <> notappl |
         V767A <> notappl | V767B <> notappl | V767C <> notappl |
         V768A <> notappl | V768B <> notappl | V768C <> notappl then
        e = errmsg(7530,V761,V761B,V761C,V766A,V767A,V767B,V767C,V768A,V768B,V768C)
      endif;
    endif;

  { !! todo - check this with GR !!
    if V783 <> notappl <=> V784A = notappl then
      e = errmsg(7531,V751,V784A)
        endif;
  }

                                                                          { Comment out unused variables !!!}
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes - not used}
      x1 = 0;     { Count of missing - not used}
      x2 = 0;     { Count of notappl }

      e = AddVals( V784A );
      e = AddVals( V784B );
      e = AddVals( V784C );
      e = AddVals( V784D );
      e = AddVals( V784E );
      e = AddVals( V784F );
      e = AddVals( V784G );
      e = AddVals( V784H );
      e = AddVals( V784I );
      e = AddVals( V784J );
      e = AddVals( V784K );
      e = AddVals( V784L );
      e = AddVals( V784M );
      e = AddVals( V784N );
      e = AddVals( V784O );
      e = AddVals( V784P );
      e = AddVals( V784Q );
      e = AddVals( V784R );
      e = AddVals( V784S );
      e = AddVals( V784T );
      e = AddVals( V784U );
      e = AddVals( V784V );
      e = AddVals( V784X );
                                              {* Add CS variables !!}
      if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
        e = errmsg(7532,i,V784X,x2,k)
      endif;

{ ---------------------------------------------------------------------------- }
PROC REC80_EDT
preproc

                                        { Set unused variables to <> notappl !!}
  box  V527      => lastsex;                      { lastsex = sex during last year }
       100-199   => 1;
       200-251   => 1;
       300-311   => 1;
       995       => 1;
                 => 0;
  endbox;
  if V201 & V527 = 996 & (V008-B3(1) < 12) then
    lastsex = 1
  endif;
  if !lastsex then
    {*ART no sex in last year then these variables should be skipped - structure check *}
    if V821A <> notappl | V821B <> notappl | V821C <> notappl |
       V832B <> notappl | V832C <> notappl |
       V833A <> notappl | V833B <> notappl | V833C <> notappl |
       V834A <> notappl | V834B <> notappl | V834C <> notappl |
       V835A <> notappl | V835B <> notappl | V835C <> notappl then
      e = errmsg(8230,V821A,V821B,V821C,V832B,V832C,V833A,V833B,V833C,V834A,V834B,V834C,V835A,V835B,V835C)
    endif;
  else
    if V761 = notappl | V835A <> notappl then
      e = errmsg(8231,V761,V835A)
    endif;
    if V832B <> notappl & V835B = notappl then
      e = errmsg(8232,V832B,V835B)
    endif;
    if V832C <> notappl & V835C = notappl then
      e = errmsg(8233,V832C,V835C)
    endif;
  endif;

  {*ART condom used during last sex with this partner => condom used everytime with this partner should be applicable - structure check *}
  if V761 <> notappl & (V761 = 1 <=> V833A = notappl) then
    e = errmsg(8240,V761,V833A)
  endif;
  if V761B <> notappl & (V761B = 1 <=> V833B = notappl) then
    e = errmsg(8241,V761B,V833B)
  endif;
  if V761C <> notappl & (V761C = 1 <=> V833C = notappl) then
    e = errmsg(8242,V761C,V833C)
  endif;

  if V224 then
    births24 = (B19(1) < 24); { !! DHS7 !!}
  endif;

  if V751 = 1 & births24 & (V208 & M2N(1) = 0 <=> V838A = notappl) then
    e = errmsg(8252,M2N(1),V838A)
  endif;

  if V838A = notappl <=> V838B <> notappl |
     V838A = notappl <=> V838C <> notappl |
     V838A = notappl <=> V839  <> notappl |
     V838A = notappl <=> V840  <> notappl |
     V841  = notappl <=> V842  <> notappl |
     V841  = notappl <=> V843  <> notappl then
    e = errmsg(8253,V838A,V838B,V838C,V840,V841,V842,V843)
  endif;

    if V751 = 1 & (V840 = 1 <=> V841 = notappl) then
      e = errmsg(8254,V751,V840,V841)
    endif;

  // !! todo redo this test due to filter for antenatal care
    if V751 = 1  &  V825 = notappl |
       V751 = 1 <=> V828 = notappl |
       V751 = 1 <=> V842 = notappl then
      e = errmsg(8255,V751,V825,V828,V842,V844)
    endif;


    if V751 = 1  then { heard of AIDS }
      if ((!V201 | !V238 ) & V838A <> notappl) |
        births24 &
        ( (M2N(1) = 0  &  V838A = notappl)      |
          (M2N(1) <> 0 &  V838A <> notappl) ) then
          if births24 then
            e = errmsg(8256,V201,V238,B19(1),M2N(1),V838A)
          else
            e = errmsg(8256,V201,V238,B19(1),notappl,V838A)
          endif;
      endif;
    endif;

    if V502 = 1 <=> V850A = notappl |
       V502 = 1 <=> V850B = notappl then
      e = errmsg(8257,V502,V850A,V850B)
    endif;

PROC REC81_EDT
preproc
                                                                { Set unused variables to <> notappl !!}
  if V801 = notappl | V802 = notappl | V803 = notappl | special(V804)  |
     V805 = notappl | V806 = notappl | V811 = notappl | V812 = notappl |
     V813 = notappl | V814 = notappl
   then
     e = errmsg(12000,"REC81");
     errmsg("not special: V804=%d",V804);
     errmsg("not notappl: V801=%d,V802=%d,V803=%d,V805=%d,V806=%d",V801,V802,V803,V805,V806);
     errmsg("not notappl: V811=%d,V812=%d,V813=%d,V814=%d",V811,V812,V813,V814);
  endif;

  if V027 <> V804 | V028 <> V805 | V029 <> V806 then
    e = errmsg(8101,V027,V028,V029,V804,V805,V806)
  endif;

  {* Duration of interview *}
  if V804 = 1 then
    if V801 = missing | V802 = missing then
      x = missing
    else
      x = (int(V802/100)*60 + V802%100) - (int(V801/100)*60 + V801%100);
  {??-na Count of <= 0 should be <= minimum time of interview }
      if x <= 0 | x >= 180 then
        {* Duration of interview unreasonable *}
  {     e = errmsg(8101,V801,V802,V803,V804);                               {!!}}
        if x <= 0 then
          kount(3) = kount(3) + 1
        else
          kount(4) = kount(4) + 1
        endif
      endif;
      if x > 95 then
        x = 95
      elseif x < 0 then
        x = 97
      endif
    endif;
  else
    x = 96;
  endif;

  {* Duration of interview and computed time do not match *}
  if x <> V803 then
    e = errmsg(8102,V801,V802,V803,x,V804)
  endif;

{ ---------------------------------------------------------------------------- }
PROC REC82_EDT
{*** No calendar??                                                      ??????}

  if calused then
    maxcols = V019A;

    j = 1;
    while j <= maxcols do
                                                                  { Set unused variables to <> notappl !!}
      if (!j in 2,4 | col4used) & (special(VCOL(j)) | (length(strip(VCAL(j))) = 0)) then
        e = errmsg(12101,j,"REC82");
      endif;

      {* Entries after month of interview should be blanked out *}
      i = 1;
      while i < V018 do
        if !pos(VCAL(j)[i:1]," ") then
          e = errmsg(9000,j,i,VCAL(j)[i:1])
        endif;
        i = i + 1
      enddo;
      j = j + 1
    enddo;

    { Check column 1  = births, pregnancies and contraceptive use }

    { -------------- }
    i = V018; b = 1; births5 = 0;
    while i & (i <= vcallen) do

      { Check entries in column 1 }
      if !pos(VCAL(1)[i:1],concat("BPT0",vcalmeth)) then
        e = errmsg(9101,i,VCAL(1)[i:1]);
      endif;

      { Check birth in calendar matches births in birth history }
      if pos(VCAL(1)[i:1],"B") then
        births5 = births5 + 1;
        twins = B0(births5);
        if twins then
          births5 = births5 + twins - 1
        endif;

        calcmc = rowcmc(i);
        if calcmc <> B3(b) then
          e = errmsg(9102,i,VCAL(1)[i:1],calcmc,b,B3(b));
        endif;
        while B0(b) > 1 do
          b = b + 1;
        enddo;
        b = b + 1;

        {* Check gestation lengths of births <> 9 months -> note in doc *}
        j = i+1;
        while j <= vcallen & pos(VCAL(1)[j:1],"P") do
          j = j + 1;
        enddo;
        if j > i + 9 | (j <= vcallen & j < i + 9) then
          e = errmsg(9103,i,j-i,VCAL(1)[i:j-i]);
        endif;

      elseif pos(VCAL(1)[i:1],"T") then

        { Check gestation lengths of pregnancy is no more than 9 months -> note in doc }
        j = i+1;
        while j <= vcallen & pos(VCAL(1)[j:1],"P") do
          j = j + 1;
        enddo;
        if j > i + 9 then
          e = errmsg(9104,i,j-i,VCAL(1)[i:j-i]);
        endif;

      endif;
      i = i + 1;
    enddo;

    { Check all births since January 199? are represented in calendar }
    if V235 then
      x = V235 - 1
    else
      x = V201
    endif;
    if V018 & births5 <> x then
      e = errmsg(9105,births5,x)
    endif;

    { Check current contraceptive use }
    if V018 & (V312 <> pos(VCAL(1)[V018:1],vcalmeth)) then
      e = errmsg(9106,V018,VCAL(1)[V018:1],V312)
    endif;

    { Check column 2  = reasons for discontinuation of contraceptive use}
    { -------------- }
    if col2used then

      caluse  = 0;
      user    = default;              { Whether using in month i-1 }
      useval  = 0;
      preval = V312;

      i = V018;
      while i & (i <= vcallen) do

        { Checks entries in column 2 }
        if !pos(VCAL(2)[i:1],vcaldisc) & !pos(VCAL(2)[i:1]," ") then
          e=errmsg(9201,i,VCAL(2)[i:1]);
        endif;

        used = pos(VCAL(1)[i:1],vcalmeth);          { Whether using in month i }

        { Checks for discontinuation of use, but col 2 blank }
        if used & user <> used & user <> default then
          if pos(VCAL(2)[i:1]," ") then
            e=errmsg(9202,i,VCAL(1)[i-1:2],i,VCAL(2)[i:1]);
          endif;
        endif;

        { Checks for col 2 not blank, but no contraceptive method }
        if (i > V018 & pos(VCAL(1)[i:1],"BPT0") | (user = used) ) & !pos(VCAL(2)[i:1]," ") then
          e=errmsg(9203,i,VCAL(1)[i-1:2],i,VCAL(2)[i:1]);
        endif;

        user   = used;

        i = i + 1;
      enddo;
    endif;                    { end of col2used }

    { Check column 3  = episodes of marriage }
    { -------------- }
    if col3used then

      i = V018;
      while i & (i <= vcallen) do

        { Check entries in column 3 }
        if !pos(VCAL(3)[i:1],"X0?") then
          e=errmsg(9301,i,VCAL(3)[i:1]);
        endif;

        i = i + 1;
      enddo;

      if V018 then
        if ((V502 = 0            | V509 >  V017) & pos(VCAL(3)[vcallen:1],"X")) |
           ((V502 = 1 & V503 = 1 & V509 <= V017) & pos(VCAL(3)[vcallen:1],"0")) then
          e = errmsg(9302,vcallen,VCAL(3)[vcallen:1],V501,V503,V509);
        elseif V509 > V017 then
          x = V018+V008-V509;
          x1 = vcallen-x;
          if !pos(VCAL(3)[x:1],"X") | pos(VCAL(3)[x+1:x1],"X") then
            e = errmsg(9303,x,vcallen,VCAL(3)[x:x1+1],V501,V503,V509);
          endif
        endif;
      endif;
    endif;                { end of col3used }

    { Check column 4 = country specific, often residence }
    { -------------- }
    if col4used then

      oldres = pos(VCAL(4)[V018:1],"X01234") - 1;  { residence in month i+1 }
      i = V018;
      while i & (i <= vcallen) do

        { Check entries in column 4 }
        if !pos(VCAL(4)[i:1],"X01234") then
          e=errmsg(9401,i,VCAL(4)[i:1]);
        endif;

        { Check for two consecutive changes of residence
          or for a change without a change code }
        if i < vcallen then
          curres = oldres;                            { residence in month i }
          oldres = pos(VCAL(4)[i+1:1],"X01234") - 1;  { residence in month i+1 }
          if (curres  = 0 & oldres  = 0) |
             (curres <> 0 & oldres <> 0 & oldres <> curres) then
            e=errmsg(9402,i,VCAL(4)[i:2]);
          endif;
        endif;

        i = i + 1;
      enddo;
    endif;          { end of col4used }

    { Check column 5 = source of contraception }
    { -------------- }
    if col5used then
      used = 0;
      i = vcallen;
      while V018 & (i >= V018) do

        { Check entries in column 5 }
        if !pos(VCAL(5)[i:1]," 123456ABCDEFKLMX?") then
          e=errmsg(9501,i,VCAL(5)[i:1]);
        endif;

        { Checks for start of use, but col 5 blank }
        usem = pos(VCAL(1)[i:1],vcalmeth);          { Whether using in month i }
        if usem & usem <> used & VCAL(5)[i:1] = " " then
          e=errmsg(9502,i,VCAL(1)[i:2],i,VCAL(5)[i:1]);
        elseif (usem = used | !usem) & VCAL(5)[i:1] <> " " then
          e=errmsg(9503,i,VCAL(1)[i:2],i,VCAL(5)[i:1]);
        endif;

        used = usem;
        i = i - 1;
      enddo;
    endif;          { end of col5used }

  endif;

{ ---------------------------------------------------------------------------- }
PROC REC83_EDT

  for i in REC83_EDT do

                                                                  { Set unused variables to <> notappl !!}
    if special(MMIDX) | MM1 = notappl | MM4 = missing | MM8 = missing
     then
      e = errmsg(12101,i,"REC83");
      errmsg("not special: MMIDX=%d",MMIDX);
      errmsg("not missing: MM4=%d,MM8=%d",MM4,MM8);
      errmsg("not notappl: MM1=%d",MM1);
    endif;

    if MMIDX <> i then
      e = errmsg(8301,i,MMIDX)
    endif;

    if MM9 = notappl <=>
      (MM1 = 2 & MM2 = 0 & !special(MM7) & MM7 >= MMC5 & MM7 < 98) then
      e = errmsg(8303,i,MM9,MM1,MM2,MM7,MMC5)
    endif;

    if MM14 = notappl <=>
      (MM1 = 2 & MM2 = 0 & !special(MM7) & MM7 >= MMC5 & MM7 < 98) then
      e = errmsg(8306,i,MM14,MM1,MM2,MM7,MMC5)
    endif;

  enddo;

PROC REC84_EDT
                                                                { Set unused variables to <> notappl !!}
  if MMC1 = notappl | MMC5 = missing then
    e = errmsg(12000,"REC84");
    errmsg("not missing: MMC5 = %d",MMC5);
    errmsg("not notappl: MMC1 = %d",MMC1);
  endif;

  if MMC1 <> soccurs(REC83) then
    e = errmsg(8401,MMC1,soccurs(REC83))
  endif;

  {*ART Reported number of preceeding births is not within the range of values - Note in DOC *}
  x = count(REC83_EDT where V011 >  MM4);
  x1= count(REC83_EDT where V011 >= MM4);
  if !MMC2 in x:x1 then
    e = errmsg(8402,MMC2,x,x1,MMC1)
  endif;

  for i in REC84_GROUP do

    if MMC4 <> notappl & MMC4 <> 0 &
       (special(MMC4) | MMC4 < 0 | MMC4 >= HV009) then
      e = errmsg(8405,i,MMC4,HV009)
    elseif MMC4 <> notappl & MMC4 <> 0 then
      if HV117(MMC4) <> 1 then
        e = errmsg(8406,i,MMC4,HV117(MMC4))
      endif
    endif;

  enddo;

  if MMC5 <> cutoff then
    e = errmsg(8407,MMC5)
  endif;

{ ---------------------------------------------------------------------------- }
PROC RECML_EDT
preproc

  for i in RECML_EDT do
                                          { Set unused variables to <> notappl !!}
    if special(IDXML) then
      e = errmsg(12101,i,"RECML");
      errmsg("not special: IDXML=%d",IDXML);
    endif;

    ok = 1;
    if i <> IDXML then
      errmsg(9702,i,IDXML(i)); ok=0
    endif;
    if i > sOccurs(REC21) then
      errmsg(9703,i,sOccurs(REC21)); ok=0
    else
      if V008 - B3(i) > Hmax then
        errmsg(9704,i,V008,B3(i)); ok=0
      endif
    endif;

    if ok then

      xi = i-1;
      if i & B0(i) then
        while xi & B0(xi) > 1 do
          xi = xi - 1
        enddo;
      endif;
      if !xi then
        if V213 then
          interval = V008-V214-B3(i)
        else
          interval = V008-B3(i)
        endif
      else
        interval = B3(xi)-B3(i)
      endif;

      j = i + 1;
      if B0(i) > 1 &
         (lastch <> 1 {questions asked only of last child in standard, but not in country} &
          (ML1 <> ML1 (j) | ML2 <> ML2(j))
         ) {end of questions asked of last child only in standard} then
        e = errmsg(9709,i,B0(i),j)
      endif;

      if lastch = 1 & i <> 1 then  { check questions that are only for last child in standard }
        if ML1 <> notappl | ML2 <> notappl then
          e = errmsg(9720,i,ML1,ML2)
        endif
      endif;

      if (H22(i) = 1 | H31(i) in 1,2) <=> ML11 = notappl then
        e = errmsg(9721,i,H22(i),H31(i),ML11)
      endif;

      {*ART Drugs for fever/ARI - structure check }
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes }
      x1 = 0;     { Count of missing }
      x2 = 0;     { Count of notappl }
      e = AddVals( ML13A );
      e = AddVals( ML13B );
      e = AddVals( ML13C );
      e = AddVals( ML13D );
      e = AddVals( ML13E );
      e = AddVals( ML13F );
      e = AddVals( ML13G );
      e = AddVals( ML13H );
      e = AddVals( ML13I );
      e = AddVals( ML13J );
      e = AddVals( ML13K );
      e = AddVals( ML13L );
      e = AddVals( ML13M );
      e = AddVals( ML13N );
      e = AddVals( ML13O );
      e = AddVals( ML13P );
      e = AddVals( ML13X );
                                            {* Add country specific variables !!}
      if ( x & (x1 | ML13Y <> 0 | ML13Z <> 0)) |                            { Some yes => ML13Y=no, ML13Z=yes, no missings }
         (!x & !x1 & x2 <> k & ML13Y <> 1 & ML13Z <> 1) |                   { No yes, no missings => ML13Y = yes, ML13Z = no }
         (x1 & (x1+x2 <> k | ML13Y        <> missing | ML13Z <> missing)) | { Some missing => all missing, ML13Y = missing, ML13Z = missing }
         (x2 = k & (ML13Y <> notappl | ML13Z <> notappl)) then              { All notappl => ML13Y = notappl, ML13Z =   notappl }
        errmsg(9722,i,ML13Y,ML13Z,x,x1,x2);
        if x & (x1 | ML13Y <> 0 | ML13Z <> 0) then
          errmsg("Some yes => ML13Y=no, ML13Z=yes, no missings");
        endif;
        if !x & !x1 & x2 <> k & ML13Y <> 1 & ML13Z <> 1 then
          errmsg("No yes, no missings, not all vars NA => ML13Y = yes, ML13Z = no");
        endif;
        if x1 & (x1+x2 <> k | ML13Y <> missing | ML13Z <> missing) then
          errmsg("Some missing => all missing, ML13Y = missing, ML13Z = missing");
        endif;
        if x2 = k & (ML13Y <> notappl | ML13Z <> notappl) then
          errmsg("All notappl => ML13Y = notappl, ML13Z =   notappl");
        endif;
        errmsg("ML13A=%d,ML13B=%d,ML13C=%d,ML13D=%d,ML13E=%d,ML13F=%d",ML13A,ML13B,ML13C,ML13D,ML13E,ML13F);
        errmsg("ML13G=%d,ML13H=%d,ML13I=%d,ML13J=%d,ML13K=%d,ML13L=%d",ML13G,ML13H,ML13I,ML13J,ML13K,ML13L);
        errmsg("ML13M=%d,ML13N=%d,ML13O=%d,ML13P=%d,ML13X=%d,ML13Z=%d,ML13Y=%d",ML13M,ML13N,ML13O,ML13P,ML13X,ML13Z,ML13Y);
      endif;

      if ML13A = 1 <=> ML15C = notappl |
         ML13B = 1 <=> ML16C = notappl |
         ML13C = 1 <=> ML17C = notappl |
         ML13D = 1 <=> ML18C = notappl |
         ML13E = 1 <=> ML20C = notappl |
         ML13F = 1 <=> ML21C = notappl |
        {ML13G = 1 <=> ML22C = notappl |}
         ML13H = 1 <=> ML23C = notappl |
         ML13I = 1 <=> ML24C = notappl then
        e = errmsg(9723,i,nbans,ML15C,ML16C,ML17C,ML18C,ML20C,ML21C,ML22C,ML23C,ML24C)
      endif;

      if ML13A = 1 <=> ML15A = notappl |
         ML13A = 1 <=> ML15B = notappl then
        e = errmsg(9724,i,ML13A,ML15A,ML15B)
      endif;

      if ML13B = 1 <=> ML16A = notappl |
         ML13B = 1 <=> ML16B = notappl then
        e = errmsg(9725,i,ML13B,ML16A,ML16B)
      endif;

      if ML13C = 1 <=> ML17A = notappl |
         ML13C = 1 <=> ML17B = notappl then
        e = errmsg(9726,i,ML13C,ML17A,ML17B)
      endif;

      if ML13D = 1 <=> ML18A = notappl |
         ML13D = 1 <=> ML18B = notappl then
        e = errmsg(9727,i,ML13D,ML18A,ML18B)
      endif;

      if ML13E = 1 <=> ML20A = notappl |
         ML13E = 1 <=> ML20B = notappl then
        e = errmsg(9728,i,ML13E,ML20A,ML20B)
      endif;

      if ML13F = 1 <=> ML21A = notappl |
         ML13F = 1 <=> ML21B = notappl then
        e = errmsg(9729,i,ML13F,ML21A,ML21B)
      endif;

      if ML13H = 1 <=> ML23A = notappl |
         ML13H = 1 <=> ML23B = notappl then
        e = errmsg(9731,i,ML13H,ML23A,ML23B)
      endif;

    endif;

  enddo;

PROC REC92_EDT
preproc

  for i in REC92_EDT do
    {* Matching CS variables' index with index of REC21 *}
    if IDX92 <> BIDX(i) then
      e = errmsg(9600,i,IDX92,BIDX(i))
    endif;
  enddo;

PROC REC94_EDT
preproc

  for i in REC94_EDT do
    {* Check indexes of CS section match the indexes of the maternity section *}
    if IDX94 <> MIDX(i) then
      e = errmsg(9650,i,IDX94,MIDX(i))
    endif;
  enddo;

PROC REC95_EDT
preproc

  for i in REC95_EDT do
                                                                { Set unused variables to <> notappl !!}
    if special(IDX95(i)) then
      e = errmsg(12101,i,"REC95");
      errmsg("not special: IDX95=%d",IDX95);
    endif;

    {* Check indexes of CS section match the indexes of the health section *}
    if IDX95 <> HIDX(i) then
      e = errmsg(4300,i,HIDX(i),IDX95)
    endif;

  enddo;


PROC REC96_EDT
preproc

  for i in REC96_EDT do
    {* Check all entries in the H/W Cs section are present *}
    if IDX96 <> HWIDX(i) then
      e = errmsg(4400,i,HWIDX(i),IDX96)
    endif;
  enddo;

