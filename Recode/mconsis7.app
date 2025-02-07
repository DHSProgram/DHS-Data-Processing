PROC GLOBAL

  { Country = COUNTRY, Year. }

  { Version 2.0.0 of 11/16/16, update 04/23/07 - Based on Male Recode DHS-V 1.1.1 }

  set explicit;

  array kount(99);
  array hwqline(99);   { line numbers }
  array hwqtype(99);   { type of entry }

  numeric minab,       { Minimum age at birth }
              minam,       { Minimum age at marriage }
                  aidsmod;     { AIDS module used }
  numeric kmethod, umethod;
  numeric lastsex;
  numeric x, x1, x2;
  numeric i, k;
  numeric e, error;
  numeric livchild;

  function AddVals( Var )
    AddVals = 0;

    if     Var = 1       then x  = x  + 1
    elseif Var = missing then x1 = x1 + 1
    elseif Var = notappl then x2 = x2 + 1
    endif;
    k = k + 1;

    AddVals = 1;

  end;
  { Recodes NA to zero }
  function NAtoZero(ynvar);
    if ynvar = notappl then
      ynvar = 0
    endif;
    NAtoZero = ynvar
  end
{ ---------------------------------------------------------------------------- }
PROC MRECODE7_FF
preproc

  minab    = 120;         { Minimum age at birth }                     {!!}
  minam    = 120;         { Minimum age at marriage }                  {!!}

  aidsmod  = 1;            { AIDS module used }                        {!!}

  {* kount: Statistics for repeated errors that are not corrected. *}
  i = 1;
  while i <= 99 do
    kount(i) = 0;
    i = i + 1
  enddo;
{ ---------------------------------------------------------------------------- }

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
PROC MREC01_EDT
preproc

  kount(96) = kount(96) + 1;
  if MV015 = 1 then
    kount(98) = kount(98) + 1;
  endif;
                                    { Set unused variables to <> notappl !!}
  if !length(strip(MV000)) |
     special(MV001) | special(MV002) | special(MV003) | special(MV004) |
     special(MV005) | special(MV006) | special(MV007) | special(MV008) |
     MV009 = missing| MV010 = missing| MV011 = missing| MV012 = missing|
     MV013 = missing| MV014 = missing| MV015 = notappl| special(MV016) |
     special(MV021) | !special(MV022) | special(MV023) | special(MV024)|
     special(MV025) | MV026 = notappl| special(MV027) | MV028 = notappl|
         MV029 = notappl then
    e = errmsg(12011,"MREC01");
    errmsg("not special: MV001=%d,MV002=%d,MV003=%d,MV004=%d,MV005=%d,MV006=%d,MV007=%d,MV008=%d",MV001,MV002,MV003,MV004,MV005,MV006,MV007,MV008);
    errmsg("not special: MV016=%d,MV021=%d,MV022=%d,MV023=%d,MV024=%d,MV025=%d,MV027=%d",MV016,MV021,MV022,MV023,MV024,MV025,MV027);
    errmsg("not missing: MV009=%d,MV010=%d,MV011=%d,MV012=%d,MV013=%d,MV014=%d",MV009,MV010,MV011,MV012,MV013,MV014);
    errmsg("not notappl: MV015=%d,MV026=%d,MV028=%d,MV029=%d",MV015,MV026,MV028,MV029);
  endif;

  {* Ultimate area unit not the cluster number *}
  if MV004 <> MV001 then
    kount(54) = kount(54) + 1;
  endif;
  {* Primary sampling unit not the cluster number *}
  if MV021 <> MV001 then
    kount(55) = kount(55) + 1;
  endif;
  {* Sample stratum number equation of the cluster number *}
  if MV022 = int((MV001+1)/2) then
    kount(56) = kount(56) + 1;
  endif;
  {* Sample domain is national *}
  if MV023 = 0 then
    kount(57) = kount(57) + 1;
  endif;

  {* Check variable assignment is correct, variables are the same *}

  if MV003 > MV136 then
    e = errmsg(10008,MV003,MV136);
  endif;
  {*??check member defacto??*}

  if MV005 = 0 <=> MV015 = 1 then
    e = errmsg(0100,MV005,MV015)
  endif;
  {* CMC date of interview *}
  if MV008 <> cmcode(MV006,MV007) then
    e = errmsg(0101,MV006,MV007,MV008)
  endif;
  if (MV015 =  1 & (MV009  = notappl | MV010  = notappl | MV011  = notappl |
                    MV012  = notappl | MV013  = notappl | MV014  = notappl |
                    MV801  = notappl | MV802  = notappl | MV803  = notappl))
   | (MV015 <> 1 & (MV009 <> notappl | MV010 <> notappl | MV011 <> notappl |
                    MV801 <> notappl | MV802 <> notappl | MV803 <> notappl |
                    MV012 <> notappl | MV013 <> notappl | MV014 <> notappl)) then
    e = errmsg(0105,MV015,MV009,MV010,MV011,MV012,MV013,MV014)
  endif;
  if MV015 = 1 then
    {* CMC date of birth *}
    if MV011 <> cmcode(MV009,MV010) then
      e = errmsg(0102,MV009,MV010,MV011)
    endif;
    {* Age of man *}
    if MV012 <> int((MV008-MV011)/12) then
      e = errmsg(0103,MV008,MV011,MV012)
    endif;
    {* Age of man in 5-year age group *}
    if MV013 <> int(MV012/5)-2 then
      e = errmsg(0104,MV012,MV013)
    endif;
  endif;
  box MV016 : MV006            => x;
       >31 : 1,3,5,7,8,10,12 => 1;
       >30 : 4,6,9,11        => 1;
       >29 : 2               => 1;
           :                 => 0;
  endbox;
  if x | (MV016 = 29 & MV006 = 2 & (MV007 % 4)) then
    e = errmsg(0106,MV016,MV006,MV007)
  endif;

  if MV015 = 1 & (MV024 <> MV101 | MV025 <> MV102 | MV026 <> MV134) then
    e = errmsg(0111,MV024,MV101,MV025,MV102,MV026,MV134);
  endif;

  for i in MREC01_GROUP000 do

    if !special(MV034) & (MV034 = MV003 | MV034 < 0 | MV034 > MV136) then
      {* Line number of wife/partner (MV034) inconsistent *}
      e=errmsg(0112,i,MV034,MV003,MV136);
  {*?? needs more work ??:
    elseif !special(MV034) & MV034 &
           (HV104(MV034) <> 1 | HV105(MV034) < 15) then
      {* Wife/partner (MV034A) has an incorrect gender or underage *}
      e=errmsg(0113,i,MV034,MV034,HV104(MV034),MV034,HV105(MV034));
  *}
    endif;
  {  !! wife/partner may exist for country
    if MV034 <> notappl <=> MV034A = notappl then
      e=errmsg(0114,i,MV034,MV034A);
    endif;
  }

  enddo;

  x = MV505; if MV505 = notappl then x = 0 endif;
  if x <> noccurs(MREC01_GROUP000) | x <> MV035 then
    e = errmsg(0115,MV505,noccurs(MREC01_GROUP000),MV035);
  endif;

  {* Duration of interview *}
  if MV015 = 1 then

    if MV027 = 1 then
      if MV801 = missing | MV802 = missing then
        x = missing
      else
        x = (int(MV802/100)*60 + MV802%100) - (int(MV801/100)*60 + MV801%100);
        {* ?? Count of <= 0 should be <= minimum time of interview }
        if x <= 0 | x >= 180 then
          {* Duration of interview unreasonable *}
          e = errmsg(8101,MV801,MV802,MV803,MV027);                             {!!}
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
    if x <> MV803 then
      e = errmsg(8102,MV801,MV802,MV803,x,MV027)
    endif;

  endif;

{ ---------------------------------------------------------------------------- }
PROC MREC11_EDT
preproc
                                            {* Set unused variables to <> notappl !!}
  if special(MV101)  | special(MV102)  | MV103 <> notappl | MV104 = notappl |
     MV106 = notappl |
     MV133 = notappl | MV134 <> notappl | MV135 = notappl | special(MV136)  |
     MV149 = notappl | MV150 = notappl | special(MV151)  | MV152 = notappl |
     MV155 = notappl |
     MV158 = notappl | MV159 = notappl |
     MV167 = notappl
    then
    e = errmsg(12011,"MREC11");
     errmsg("not special: MV101=%d,MV102=%d,MV136=%d,MV151=%d",MV101,MV102,MV136,MV151);
     errmsg("not notappl: MV103=%d,MV104=%d,MV106=%d,MV133=%d,MV134=%d,MV135=%d",MV103,MV104,MV106,MV133,MV134,MV135);
     errmsg("not notappl: MV149=%d,MV150=%d,MV152=%d,MV155=%d",MV149,MV150,MV152,MV155);
     errmsg("not notappl: MV158=%d,MV159=%d,MV167=%d",MV158,MV159,MV167);
  endif;

  { Check whether the household exists or not }
  Wcaseid = concat(MCASEID[1:12],"   ");             {!! Check length }
  if loadcase(IDRSMALL,Wcaseid) then

    for i in MREC01_GROUP000 do

      if !special(MV034) & MV034 > 0 then { we have a woman's line number }

        { Check whether the woman exists or not }
        Wcaseid = concat(MCASEID[1:12],edit("ZZ9",MV034)); {!! Check length }
        if !loadcase(IDRSMALL,Wcaseid) then

          if Hhv117(MV034) = 1 then
            { Eligible woman not found in the constructed data file }
            e = errmsg(0010,i,MV034);
          else
            e = errmsg(0011,i,MV034,MV034,Hhv117(MV034));
            kount(37) = kount(37)+1;
          endif;

        elseif Wv015 <> 1 then
        { found a woman with incomplete result }
          e = errmsg(0012,i,MV034,Wv015);
          kount(38) = kount(38) + 1;
       {?? Should the following code be in MICONSIS ??}
        elseif Wv034 <> MV003 then
        { Husband's line number from woman's recode against men's number }
          e = errmsg(0013,i,MV034,Wv034);
        elseif Wv012 <> MV034B then
          { have a match age of wife against wife's age}
          e = errmsg(0014,i,MV034B,Wv012);
        endif;

      endif;

    enddo;

  else
    { Household does not exist in the list of households }
    e = errmsg(0020, Wcaseid);
  endif;

  {* Years lived in place of residence, MV012 = Age of respondent *}
  if MV104 <> missing & MV104 < 95 & MV104 > MV012 then
    e = errmsg(1101,MV012,MV104)
  endif;
  if (MV104 = 95 | MV104 = 96) <=> MV105 <> notappl then
    e = errmsg(1102,MV104,MV105)
  endif;
  {* Children went to school under age 5 *}
  if MV133 <> missing & MV133 <> 97 & MV133+5 > MV012 then
    e = errmsg(1103,MV012,MV133)
  endif;
  {* Level of education and grade are given at the same time *}
  if (MV106 = 0 | MV106 = missing) <=> MV107 <> notappl then
     e = errmsg(1104,MV106,MV107)
  endif;

  {* More eligible men in the HH than the number of members *}
  if MV138 > MV136 then
    e = errmsg(1108,MV138,MV136)
  endif;
  {* Checking the recode of the educational attainment *}
  box MV149 => x;
       1,2 => 1;          {* Primary incomplete and complete *}
       3,4 => 2;          {* Secondary incomplete and complete *}
         5 => 3;          {* Higher *}
           => MV149;
  endbox;
  if x <> MV106 then
    e = errmsg(1115,MV149,MV106)
  endif;

  {* Literacy recoded 2 for more than primary *}
  { !!! check the point of this msg !!!}
  if MV155 <> 2 & (MV106 = 2 | MV106 = 3)
   then
    e = errmsg(1120,MV155,MV106);
  endif;
  { !! MV156 not asked in DHS7 remove comments below if included in survey }
  {* Literacy/Literacy program asked to no education or primary *}
  {
    if MV156 <> notappl <=> (MV106 = 0 | MV106 = 1) then
      e = errmsg(1121,MV156,MV106);
    endif;
  }
  { !! MV156 not asked in DHS7 remove comments below if included in survey }
  {
    {* Reads newspaper but no literacy program and no education *}
    if (MV106 = missing | MV106 = 0) & (MV157 <> 0 & MV156 = 0) then
      e = errmsg(1122,MV106,MV157,MV156);
    endif;
  }
  {* Trips for more than one month *}
  if (MV167 = 0 | MV167 = 98 | MV167 = missing) <=> MV168 <> notappl then
    e = errmsg(1123,MV167,MV168);
  endif;

  { OUT PER JEANNE - 69 cases }
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( MV463A );
    e = AddVals( MV463B );
    e = AddVals( MV463C );
    e = AddVals( MV463D );
    e = AddVals( MV463E );
    e = AddVals( MV463F );
    e = AddVals( MV463G );
    e = AddVals( MV463X );
    {* Add CS variables !!}
    if ( x & (x1 | MV463Z <> 0)) |                   { Some yes => MV463Z=no, no missings }
      (!x & !x1 & x2 <> k & MV463Z <> 1) |          { No yes, no missings => MV463Z = yes }
      (x1 & (x1+x2 <> k | MV463Z <> missing)) |     { Some missing => all missing, MV463Z = missing }
      (x2 = k & MV463Z <> notappl) then             { All notappl => MV463Z = notappl }
      e = errmsg(4263,MV463Z,x,x1,x2)
    endif;

    if MV463A = 1 <=> MV464 = notappl then
      e = errmsg(4264,MV464,MV463A)
    endif;

{ ---------------------------------------------------------------------------- }
PROC MREC22_EDT
preproc
                                            {* Comment out unused variables !!}
  if special(MV201)  | special(MV202) | special(MV203) | special(MV204) |
     special(MV205)  | special(MV206) | special(MV207) |
  {  MV212 = missing |  GR/04/28/04 date of first child is not imputed anymore }
     MV217 = notappl | special(MV218) |
     MV245 = notappl
    then
    e = errmsg(12011,"MREC22");
     errmsg("not special: MV201=%d,MV202=%d,MV203=%d,MV204=%d,",MV201,MV202,MV203,MV204);
     errmsg("not special: MV205=%d,MV206=%d,MV207=%dMV218=%d",MV205,MV206,MV207,MV218);
     errmsg("not notappl: MV217=%d,MV245=%d",MV217,MV245);
  endif;
  {* Number of children *}
  if MV201 <> (MV202+MV203+MV204+MV205+MV206+MV207) then
    e = errmsg(1124,MV201,MV202,MV203,MV204,MV205,MV206,MV207);
  endif;

{* Current pregnancy wanted *}
  if (MV213 = 1) <=> (MV225 = notappl) then
    e = errmsg(1130,MV225,MV213)
  endif;
  {* Living children *}
  if MV218 <> (MV202+MV203+MV204+MV205) then
    e = errmsg(1125,MV218,MV202,MV203,MV204,MV205);
  endif;
  {* Age at first birth *}
  if MV201 = 0 <=> MV212 <> notappl then
    e = errmsg(1126,MV201,MV212);
  endif;
  {* Number of woman fathered children with *}
  if (MV201 = 0 | MV201 = 1) & MV201 <> MV245 then
    e = errmsg(1127,MV201,MV245);
  endif;
  {* Married when child was born *}
  if MV201 = 0 <=> MV246 <> notappl then
    e = errmsg(1128,MV201,MV246);
  endif;

  {* Age at first birth *}
  livchild = NatoZero(MV202) + NatoZero(MV203) + NatoZero(MV204) + NatoZero(MV205);
  if livchild = 0 <=> MV247 <> notappl then
    e = errmsg(1131,MV201,MV247);
  endif;

  if MV247 in 0:2 <=> MV248 = notappl |
     MV247 in 0:2 <=> MV250 = notappl |
     MV247 in 0:2 <=> MV252 = notappl then
    e = errmsg(1132,MV247,MV248,MV250,MV252);
  endif;

  if MV247 in 0:3 & (MV248 = 1 <=> MV249 = notappl) then
    e = errmsg(1133,MV247,MV248,MV249);
  endif;

  if MV247 in 0:2 & (MV250 <> 1 <=> MV251 = notappl) then
    e = errmsg(1134,MV247,MV250,MV251);
  endif;
{ ---------------------------------------------------------------------------- }
PROC MREC31_EDT
preproc
                                            {* Comment out unused variables !!}
  if special(MV301) { | special(MV302) } then
    e = errmsg(12011,"MREC31");
    errmsg("not special: MV301=%d,MV302=%d",MV301,MV302);
  endif;

  i = 1;
  kmethod = 0;
  umethod = 0;
  while i <= 20 do
                                              {* Comment out unused variables !!}
    if special(M304A(i)) | MV304(i) = notappl then
      e = errmsg(12012,i,"MREC31");
    endif;

    box i        => x;
    1-7,11,13-18 => 1;
    8-9,12       => 2;
    10,19-20     => 3;
    endbox;
    if M304A(i) <> x then
      e = errmsg(3105,i,M304A(i))
    endif;
    if i = 7 | i = 5 | i = 8 | i = 9 then
      if ((MV304(i)  = 1 {| V304(i)  = 2}) & MV305(i)  = notappl)
      |  ((MV304(i) <> 1 {& V304(i) <> 2}) & MV305(i) <> notappl) then
  //    e = errmsg(3104,i,MV304(i),MV305(i))
      endif;
    else
      if MV305(i) <> notappl then
         e = errmsg(3107,i,MV305(i))
       endif;
    endif;
    if i <> 16 then          { Comment out if emergency contraception is used !!}
      if (MV305(i)  = 1 <=> MV307(i)  = notappl) then
        e = errmsg(3106,i,MV305(i),MV307(i))
      endif;
    endif;                   { Comment out if emergency contraception is used !!}
    if MV304(i) = 1{| V304(i) = 2}then
      if i <= 7 | i = 11 | (i >= 13 & i <= 16) then
        kmethod = 3;
      elseif i = 8 | i = 9 | i = 12 then
        if kmethod < 2 then kmethod = 2 endif
      else
        if kmethod = 0 then kmethod = 1 endif
      endif;
      if MV305(i) = 1 then
        if i <= 7 | i = 11 | (i >= 13 & i <= 16) then
          umethod = 3
        elseif i = 8 | i = 9 | i = 12 then
          if umethod < 2 then umethod = 2 endif
        else
          if umethod = 0 then umethod = 1 endif
        endif
      endif;
    endif;
    i = i + 1;
  enddo;
  if kmethod <> MV301 then
    e = errmsg(3101,MV301,kmethod)
  endif;
  {
  if umethod <> MV302 then
    e = errmsg(3102,MV302,umethod)
  endif;
  }

{ ---------------------------------------------------------------------------- }
PROC MREC32_EDT
preproc
                                        { Set unused variables to <> notappl !!}
  if MV312  = notappl | MV313  = notappl |
     MV384A = notappl | MV384B = notappl | MV384C = notappl |
     MV395  = notappl | MV396  <> notappl |
     MV3B25A= notappl | MV3B25B= notappl
   then
    e = errmsg(12011,"MREC32");
     errmsg("not notappl: MV312=%d,MV313=%d,MV384A=%d,MV384B=%d,MV384C=%d",MV312,MV313,MV384A,MV384B,MV384C);
     errmsg("not notappl: MV395=%d,MV396=%d,MV3B25A=%d,MV3B25B=%d",MV395,MV396,MV3B25A,MV3B25B);
  endif;
  {* Current pregnancy of partner and currently using a contraceptive method *}
  { IS THIS WORTH A NOTE? }
  if ((MV213 = 1 & MV767A = 1) & MV312) |
     ((MV213 = 1 & MV767B = 1) & MV312) |
     ((MV213 = 1 & MV767C = 1) & MV312) then
    e = errmsg(3203,MV312,MV767A,MV767B,MV767C,MV213)
  endif;

  box  MV312      => x;
          0       => 0;
  1-7,11,13-18    => 3;
       8-9,12     => 2;
       10,19-20   => 1;
       missing    => missing;
                  => default;
  endbox;
  if x <> MV313 then
    e = errmsg(3205,MV313,MV312); error= 1
  endif;

{ ---------------------------------------------------------------------------- }
PROC MREC41_EDT
preproc
                                                                { Set unused MVariables to <> notappl !!}
  if MV463A = notappl |
     MV474  = notappl |
     MV477  = notappl |
     MV481  = notappl |
     MV482B = notappl |
     MV483  = missing
   then
    e = errmsg(12011,"MREC41");
    errmsg("not missing: MV474=%d,MV483=%d",MV474,MV483);
    errmsg("not notappl: MV463A=%d,MV477=%d,MV481=%d,MV482B=%d",MV463A,MV477,MV481,MV482B);
  endif;



    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( MV463A );
    e = AddVals( MV463B );
    e = AddVals( MV463C );
    e = AddVals( MV463D );
    e = AddVals( MV463E );
    e = AddVals( MV463F );
    e = AddVals( MV463G );
    e = AddVals( MV463X );
    {* Add CS MVariables !!}
    if ( x & (x1 | MV463Z <> 0)) |                   { Some yes => MV463Z=no, no missings }
      (!x & !x1 & x2 <> k & MV463Z <> 1) |          { No yes, no missings => MV463Z = yes }
      (x1 & (x1+x2 <> k | MV463Z <> missing)) |     { Some missing => all missing, MV463Z = missing }
      (x2 = k & MV463Z <> notappl) then             { All notappl => MV463Z = notappl }
      e = errmsg(4263,MV463Z,x,x1,x2)
    endif;

    if MV463A = 1 <=> MV464 = notappl then
      e = errmsg(4264,MV464,MV463A)
    endif;

    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( MV474A );
    e = AddVals( MV474B );
    e = AddVals( MV474C );
    e = AddVals( MV474D );
    e = AddVals( MV474E );
    e = AddVals( MV474F );
    e = AddVals( MV474G );
    e = AddVals( MV474H );
    e = AddVals( MV474I );
    e = AddVals( MV474J );
    e = AddVals( MV474X );
    {* Add CS MVariables !!}
    if ( x & (x1 | MV463Z <> 0)) |                   { Some yes => MV474Z=no, no missings }
      (!x & !x1 & x2 <> k & MV474Z <> 1) |          { No yes, no missings => MV474Z = yes }
      (x1 & (x1+x2 <> k | MV474Z <> missing)) |     { Some missing => all missing, MV474Z = missing }
      (x2 <> 0 | MV474Z = notappl) then              { No notappl }
      e = errmsg(4269,MV474Z,x,x1,x2)
    endif;

    if MV474 = 1 <=> MV475 = notappl then
      e = errmsg(4270,MV474,MV475)
    endif;

    if MV474 = 1 <=> MV476 = notappl then
      e = errmsg(4271,MV474,MV476)
    endif;

    if MV477 in 0,missing <=> MV478 <> notappl then
      e = errmsg(4272,MV477,MV478)
    endif;

    if !(MV477 in 0,missing) & (MV478 in 0,missing <=> MV479 <> notappl) then
      e = errmsg(4273,MV477,MV478,MV479)
    endif;

    if MV479 = notappl <=> MV480 <> notappl then
      e = errmsg(4274,MV479,MV480)
    endif;
                                                                  { comment out unused MVariables }
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }

    e = AddVals( MV481A );
    e = AddVals( MV481B );
    e = AddVals( MV481C );
    e = AddVals( MV481D );
    e = AddVals( MV481E );
    e = AddVals( MV481F );
    e = AddVals( MV481G );
    e = AddVals( MV481X );
                                                    {* Add CS MVariables !!}
    if ( x2 <> 0 ) then             { No notappl }
      e = errmsg(4275,x2)
    endif;

    if MV247 in 0:17 <=> MV482A = notappl then
      e = errmsg(4276,MV482A)
    endif;

    if MV482B = 1 <=> MV482C = notappl then
      e = errmsg(4277,MV482B,MV482C)
    endif;

{ ---------------------------------------------------------------------------- }
PROC MREC51_EDT
preproc
                                            {* Comment out unused variables !!}
  if MV501 = notappl | MV502 = notappl |
     MV507 = missing | MV508 = missing | MV509 = missing | MV510 = missing |
     MV511 = missing | MV512 = missing | special(MV513)  | MV525 = notappl |
     MV531 = notappl |
     MV532 = missing |
     MV536 = notappl
   then
    e = errmsg(12011,"MREC51");
     errmsg("not special: MV513=%d",MV513);
     errmsg("not missing: MV507=%d,MV508=%d,MV509=%d,MV510=%d,MV511=%d,MV512=%d,MV532=%d",MV507,MV508,MV509,MV510,MV511,MV512,MV532);
     errmsg("not notappl: MV501=%d,MV502=%d,MV525=%d,MV531=%d,MV536=%d",MV501,MV502,MV525,MV531,MV536);
  endif;

  {* Recode of respondent's marital status *}
  box MV501 => x;
         0 => 0;
       1,2 => 1;
           => 2;
  endbox;
  if x <> MV502 then
    e = errmsg(5101,MV501,MV502)
  endif;
  if (MV501 <> 0 &
      (MV503  = notappl | MV507  = notappl | MV508  = notappl | MV509  = notappl |
       MV510  = notappl | MV511  = notappl | MV512  = notappl | MV513  = 0)
     ) |
     (MV501  = 0 &
      (MV503 <> notappl | MV507 <> notappl | MV508 <> notappl | MV509 <> notappl |
       MV510 <> notappl | MV511 <> notappl | MV512 <> notappl | MV513 <> 0)
     ) then
    e = errmsg(5102,MV501,MV502,MV503,MV507,MV508,MV509,MV510,MV511,MV512,MV513)
  endif;

  if MV502 = 1 <=> MV504 = notappl then
    e = errmsg(5128,MV502,MV504)
  endif;

  if MV502 = 1 <=> MV505 = notappl then
    e = errmsg(5103,MV502,MV505)
  endif;

  {* Date of union *}
  if MV509 <> notappl & MV509 <> cmcode(MV507,MV508) then
    e = errmsg(5105,MV507,MV508,MV509)
  endif;
  {* Before minimum age at marriage or after the interview *}
  if MV509 < MV011+minam | MV509 > MV008 then
    e = errmsg(5106,MV509,MV008,MV011,MV008-MV509,minam,MV509-MV011)
  endif;
  {* Age at first marriage *}
  if MV511 <> int((MV509-MV011)/12) then
    e = errmsg(5107,MV509,MV511,MV011)
  endif;
  {* Years since first marriage *}
  if MV512 <> int((MV008-MV509)/12) then
    e = errmsg(5108,MV509,MV512,MV008)
  endif;
  {* Marital duration grouped *}
  x = int(MV512/5)+1;
  if MV512 = notappl then
    x = 0
  elseif x > 7 then
    x = 7
  endif;
  if x <> MV513 then
    e = errmsg(5109,MV512,MV513)
  endif;
  {* Never had sex *}
  if MV525 = 0 then
    if MV201 then
      {* But has children *}
      e = errmsg(5116,MV525,MV201,MV501)
    elseif MV502 then
      {* But ever been married *}
      kount(14) = kount(14) + 1;
    endif
  endif;
  if (MV525  = 0 &
      (MV527 <> notappl | MV531 <> 0 | MV532 <> 0)
     )|
     (MV525 <> 0 &
      (MV527  = notappl | MV531  = 0 | MV532  = notappl
     ))then
    e = errmsg(5117,MV525,MV527,MV531,MV532)
  endif;
  {* Age at first sex > age at first birth,
                      > age of respondent.   -> NOTE IN DOC *}
  if (!special(MV525) & MV525 > 0 & MV525 < 90) &
     ((MV212 <> notappl & MV525 > MV212) | (MV525 > MV012)) then
    e = errmsg(5118,MV525,MV212,MV012,MV531,MV532);
  endif;

  x = MV525;
  if x = 96 then
    x = MV511
  endif;
  if MV532 = 3 then
    x = x - 1
  elseif MV532 <> 0 & MV532 <> 6 then
    x = 97
  endif;
  if x <> MV531 then
    e = errmsg(5123,MV525,MV531,x,MV532)
  endif;
  {* Have ever been married *}
  if (MV535 = notappl) <=> (MV502 <> 1) then
    e = errmsg(5126,MV535,MV502)
  endif;

  box MV525 : MV527    => lastsex;
          0 :         => 0;
            : 199,299 => missing;
            : 399,499 => missing;
            : missing => missing;
            :     995 => 1;
            : 100-127 => 1;
            : 200-203 => 1;
            :         => 3;
  endbox;
  if lastsex <> MV536 then
    e = errmsg(5127,MV536,lastsex,MV525,MV527)
  endif;

  if MV525 = 0 & MV013 in 1,2 <=> MV541 = notappl then
    e = errmsg(5130,MV525,MV013,MV541)
  endif;
{ ---------------------------------------------------------------------------- }
PROC MREC61_EDT
preproc
                                         { Set unused variables to <> notappl !!}
  if MV613 = notappl | special(MV614)  |
     MV627 = notappl | MV628 = notappl | MV629 = notappl |
     MV633A= notappl | MV633B = notappl | MV633C= notappl | MV633D = notappl |
     MV634A= notappl | MV634B= notappl | MV634C= notappl | MV634D= notappl
   then
    e = errmsg(12011,"MREC61");
     errmsg("not special: MV614=%d",MV614);
     errmsg("not notappl: MV613=%d,MV627=%d,MV628=%d,MV629=%d,MV633A=%d,MV633B=%d,MV633C=%d,MV633D=%d",MV613,MV627,MV628,MV629,MV633A,MV633B,MV633C,MV633D);
     errmsg("not notappl: MV634A=%d,MV634B=%d,MV634C=%d,MV634D=%d",MV634A,MV634B,MV634C,MV634D);
  endif;

  {* Preferred waiting time *}
  if MV602 = 1 <=> MV603 = notappl then
    e = errmsg(6104,MV602,MV603)
  endif;
  {* Check recoding of preferred waiting time *}
  box MV603 => x;
   notappl => notappl;
   missing => missing;
   199,299 => missing;
   198,298 => 8;
       998 => 8;
       994 => 0;
     >=900 => 7;
   100-183 => int((MV603-100)/12);
   184-190 => 6;
   200-206 => MV603-200;
   207-290 => 6;
           => missing;
  endbox;
  if MV604 <> x then
    e = errmsg(6105,MV603,MV604)
  endif;
  {* Check recoding of desire for more children *}
  box MV602 : MV604 => x;
   notappl :      => notappl;
         1 : 0-1  => 1;
         1 : 2-6  => 2;
         1 :      => 3;
         2 :      => 4;
         3 :      => 5;
         4 :      => 6;
         5 :      => 7;
         6 :      => 8;
         7 :      => 7;
         8 :      => missing;    { per Guillermo: 05/06/04 }
   missing :      => missing;
  endbox;
  if x <> MV605 then
    e = errmsg(6106,MV605,MV602,MV604)
  endif;

  {* Check recode of ideal number of children *}
  box MV613 => x;
       0-6 => MV613;
      7-90 => 6;
           => 7;
  endbox;
  if x <> MV614 then
    e = errmsg(6109,MV613,MV614)
  endif;
  {* Time to next birth but does not want another, or
     wants another but can't get pregnant.
  *}
  if (MV616 = notappl | MV616 = 995) <=> MV602 = 1 then
    e = errmsg(6110,MV616,MV602)
  endif;

  {* Ideal number of children *}
  if special(MV613) | MV613 >= 90 then
    if MV613 <> MV627 | MV613 <> MV628 | MV613 <> MV629 then
      e = errmsg(6115,MV627,MV628,MV629,MV613)
    endif
  else
    if !special(MV627) & MV627 < 90 & MV613 <> MV627+MV628+MV629 then
      e = errmsg(6116,MV627,MV628,MV629,MV613)
    endif
  endif;

{* Restriction of "problem if became pregnant" to certain respondents *}
  if MV631  = notappl <=> (MV502 = 1 & MV605 <> 4 & MV213 <> 1) then
    e = errmsg(6119,MV631,MV502,MV605,MV213)
  endif;

{ ---------------------------------------------------------------------------- }
PROC MREC71_EDT
preproc
                                         { Set unused variables to <> notappl !!}
  if MV714  = notappl | MV716  = notappl |
     MV731  = notappl |
     { following variables taken out because they are skipped when not married }
     {MV743A = notappl |}                                  {* = notappl if used !!}
     {MV743B = notappl |} {MV743C <> notappl |} {MV743D <> notappl |}
     MV743E = notappl |                                  {* = notappl if used !!}
     MV743F = notappl | MV743G = notappl |
     MV744A = notappl | MV744B = notappl | MV744C = notappl | MV744D = notappl |
     MV744E = notappl |
     MV747A = notappl | MV747B = notappl
   then
     e = errmsg(12011,"MREC71");
     errmsg("not notappl: MV714=%d,MV716=%d,MV731=%d,MV743A=%d,MV743B=%d,MV743C=%d,MV743D=%d",MV714,MV716,MV731,MV743A,MV743B,MV743C,MV743D);
     errmsg("not notappl: MV743E=%d,MV743F=%d,MV743G=%d,MV744A=%d,MV744B=%d,MV744C=%d",MV743E,MV743F,MV743G,MV744A,MV744B,MV744C);
     errmsg("not notappl: MV743D=%d,MV743E=%d,MV747A=%d,MV747B=%d",MV743D,MV743E,MV747A,MV747B);
  endif;

  {* Respondent's occupation *}
  {* MV717 (Respondent's occupation) is a recode of MV716 *}
  if (!special(MV717) <=> special(MV716)) |
     (MV717 = 0       <=> MV716 <> 0    )
   then
    e = errmsg(7105,MV716,MV717)
  endif;

  if (MV716 = 0 <=> (  MV731 = 1      {* Worked in the past year *}
                     | MV731 = 2      {* Currently working *}
                    )
     ) then
    e = errmsg(7106,MV716,MV717,MV731)
  endif;
  {* Respondent is currently working *}
  if MV731 = 2 <=> MV714 <> 1 then
    e = errmsg(7107,MV731,MV714)
  endif;
  {
    {* Working status *}
    {Not used in the standard??}
    if ((MV731 = 1 | MV731 = 2) <=> MV719 = notappl) {* Work for family, ... *}
    | ((MV731 = 1 | MV731 = 2) <=> MV721 = notappl) {* Place of work *}
    then
      e = errmsg(7108,MV719,MV721,MV731)
      endif;
  }
  {Not used in the standard??}
  if (MV732 = notappl <=> (MV731 = 1 | MV731 = 2))   {* Seasonality of work *}
   then
    e = errmsg(7109,MV731,MV732)
  endif;
   {Not used in the standard}

  {* Type of land for those who work in agriculture *}
  { Replace test on V717 with country-specific test on V716 !!}
  {
    if (MV740 = notappl <=> (MV717 = 4 | MV717 = 5)) then
      e = errmsg(7111,MV716,MV717,MV740)
        endif;
  }
  if (MV741 = notappl <=> (MV731 = 1 | MV731 = 2)) {* Type of earnings for work *}
   then
    e = errmsg(7112,MV741,MV731)
  endif;

{ ---------------------------------------------------------------------------- }
PROC MREC75_EDT
preproc
                                            {* Comment out unused variables !!}
  if MV750  = notappl | MV751  = notappl |
     MV766A = notappl | MV766B = notappl
   | MV763D <> notappl                                 {* Comment out if used !!}
   | MV763E <> notappl                                 {* Comment out if used !!}
   | MV763F <> notappl                                 {* Comment out if used !!}
   | MV763G <> notappl                                 {* Comment out if used !!}
   | MV770E <> notappl                                 {* Comment out if used !!}
   | MV770F <> notappl                                 {* Comment out if used !!}
   | MV770G <> notappl                                 {* Comment out if used !!}
   | MV770H <> notappl                                 {* Comment out if used !!}
   | MV770I <> notappl                                 {* Comment out if used !!}
   then
    e = errmsg(12011,"MREC75");
  endif;

    box MV751 : MV785   => x;
            1 :         => 1;
              : 1       => 1;
      missing :         => missing;
              : missing => missing;
              :         => 0;
    endbox;
    if MV750 <> x then
      e = errmsg(7503,MV750,x,MV751,MV785)
    endif;

    if MV756 = notappl <=> MV751 = 1 then
      e = errmsg(7507,MV756,MV751)
    endif;
                                                                          { Comment out unused variables !!!}
      k  = 0;     { Count of applicable questions }
      x  = 0;     { Count of yes - not used}
      x1 = 0;     { Count of missing - not used}
      x2 = 0;     { Count of notappl }

      e = AddVals( MV756 );
      e = AddVals( MV754BP );
      e = AddVals( MV754CP );
      e = AddVals( MV754DP );
      e = AddVals( MV754JP );
      e = AddVals( MV754WP );
      e = AddVals( MV774A );
      e = AddVals( MV774B );
      e = AddVals( MV774C );
      { e = AddVals( MV785 ); -> !! MV785 can be applicable when the others are NA }
                                              {* Add CS variables !!}
      if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
        e = errmsg(7508,MV785,x2,k)
      endif;

    {* Last intercourse used condom *}
    if ((!special(MV527) & MV527 < 400) <=> MV761 = notappl) then
      e = errmsg(7502,MV761,MV525,MV527)
    endif;
    {* Condom use during intercourse with a woman *}
    if ((!special(MV766B) & MV766B <= 95 & MV766B >= 1) <=> MV761  = notappl) |
       ((!special(MV766B) & MV766B <= 95 & MV766B >= 2) <=> MV761B = notappl) |
       ((!special(MV766B) & MV766B <= 95 & MV766B >= 3) <=> MV761C = notappl) then
      e = errmsg(7509,MV766B,MV761,MV761B,MV761C)
    endif;
    {* Relationship with partner *}
    if ((!special(MV766B) & MV766B <= 95 & MV766B >= 1) <=> MV767A = notappl) |
       ((!special(MV766B) & MV766B <= 95 & MV766B >= 2) <=> MV767B = notappl) |
       ((!special(MV766B) & MV766B <= 95 & MV766B >= 3) <=> MV767C = notappl) then
      e = errmsg(7510,MV766B,MV767A,MV767B,MV767C)
    endif;
    {* Length of time knows partner *}
    if ((MV767A <> notappl & MV767A <> 1) <=> MV768A = notappl) |
       ((MV767B <> notappl & MV767B <> 1) <=> MV768B = notappl) |
       ((MV767C <> notappl & MV767C <> 1) <=> MV768C = notappl) then
        e = errmsg(7512,MV767A,MV768A,MV767B,MV768B,MV767C,MV768C)
    endif;

    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( MV762AA );
    e = AddVals( MV762AB );
    e = AddVals( MV762AC );
    e = AddVals( MV762AD );
    e = AddVals( MV762AE );
    e = AddVals( MV762AF );
    e = AddVals( MV762AG );
    e = AddVals( MV762AH );
    e = AddVals( MV762AI );
    e = AddVals( MV762AJ );
    e = AddVals( MV762AK );
    e = AddVals( MV762AL );
    e = AddVals( MV762AM );
    e = AddVals( MV762AN );
    e = AddVals( MV762AO );
    e = AddVals( MV762AP );
    e = AddVals( MV762AQ );
    e = AddVals( MV762AR );
    e = AddVals( MV762AS );
    e = AddVals( MV762AT );
    e = AddVals( MV762AU );
    e = AddVals( MV762AV );
    e = AddVals( MV762AW );
    e = AddVals( MV762AX );
                                            {* Add country specific variables !!}
    if ( x & (x1 | MV762AZ <> 0)) |                   { Some yes => V762AZ=no, no missings }
       (!x & !x1 & x2 <> k & MV762AZ <> 1) |          { No yes, no missings => V762AZ = yes }
       (x1 & (x1+x2 <> k | MV762AZ <> missing)) |     { Some missing => all missing, V762AZ = missing }
       (x2 = k & MV762AZ <> notappl) then             { All notappl => V762AZ = notappl }
      e = errmsg(7513,MV762AZ,x,x1,x2)
    endif;

    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes }
    x1 = 0;     { Count of missing }
    x2 = 0;     { Count of notappl }
    e = AddVals( MV762BA );
    e = AddVals( MV762BB );
    e = AddVals( MV762BC );
    e = AddVals( MV762BD );
    e = AddVals( MV762BE );
    e = AddVals( MV762BF );
    e = AddVals( MV762BG );
    e = AddVals( MV762BH );
    e = AddVals( MV762BI );
    e = AddVals( MV762BJ );
    e = AddVals( MV762BK );
    e = AddVals( MV762BL );
    e = AddVals( MV762BM );
    e = AddVals( MV762BN );
    e = AddVals( MV762BO );
    e = AddVals( MV762BP );
    e = AddVals( MV762BQ );
    e = AddVals( MV762BR );
    e = AddVals( MV762BS );
    e = AddVals( MV762BT );
    e = AddVals( MV762BU );
    e = AddVals( MV762BV );
    e = AddVals( MV762BW );
    e = AddVals( MV762BX );
                                            {* Add country specific variables !!}
    if ( x & (x1 | MV762BZ <> 0)) |                   { Some yes => V762BZ=no, no missings }
       (!x & !x1 & x2 <> k & MV762BZ <> 1) |          { No yes, no missings => V762BZ = yes }
       (x1 & (x1+x2 <> k | MV762BZ <> missing)) |     { Some missing => all missing, V762BZ = missing }
       (x2 = k & MV762BZ <> notappl) then             { All notappl => V762BZ = notappl }
      e = errmsg(7514,MV762BZ,x,x1,x2)
    endif;

    if (                                       {* Comment out unsed variables !!}
        MV763A <> 0
      | MV763B <> 0
      | MV763C <> 0
  {   | MV763D <> 0                                      {* Uncomment if used !!}}
  {   | MV763E <> 0                                      {* Uncomment if used !!}}
  {   | MV763F <> 0                                      {* Uncomment if used !!}}
  {   | MV763G <> 0                                      {* Uncomment if used !!}}
      ) & MV525 = 0 then
      e = errmsg(7515,MV763A,MV763B,MV763C,MV763D,MV763E,MV763F,MV763G,MV525)
    endif;

    if MV762AZ = 0 <=> MV769 = notappl then           {* Check missing cases ??*}
      e = errmsg(7516,MV762AZ,MV769)
    endif;

    if MV762BZ = 0 <=> MV769A = notappl then          {* Check missing cases ??*}
      e = errmsg(7517,MV762BZ,MV769A)
    endif;
    {* Sought advice for last disease and STDs *}
                                               {* Comment out unsed variables !!}
    if MV770 = notappl <=> ( MV763A = 1
                           | MV763B = 1
                           | MV763C = 1
  {                        | MV763D = 1                  {* Uncomment if used !!}}
  {                        | MV763E = 1                  {* Uncomment if used !!}}
  {                        | MV763F = 1                  {* Uncomment if used !!}}
  {                        | MV763G = 1                  {* Uncomment if used !!}}
                           ) then
      e = errmsg(7518,MV763A,MV763B,MV763C,MV763D,MV763E,MV763F,MV763G,0,MV770)
    endif;
    {* Sought advice for last disease and where sought advice *}
    { NONSENSE CHECK }
    if (MV770 in 0,1,missing <=> MV770A = notappl) then
      e = errmsg(7519,MV770,MV770A)
    endif;

    {* Sought advice for last disease and where sought advice *}
    { Take out per Jeanne }
                                                                          { Comment out unused variables !!!}
                                                                          {* Add country specific vars.   !!}
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes - not used}
    x1 = 0;     { Count of missing - not used}
    x2 = 0;     { Count of notappl }

    e = AddVals( MV770A );
    e = AddVals( MV770B );
    e = AddVals( MV770C );
    e = AddVals( MV770D );
    e = AddVals( MV770E );
    e = AddVals( MV770F );
    e = AddVals( MV770G );
    e = AddVals( MV770H );
    e = AddVals( MV770I );
    e = AddVals( MV770J );
    e = AddVals( MV770K );
    e = AddVals( MV770L );
    e = AddVals( MV770M );
    e = AddVals( MV770N );
    e = AddVals( MV770O );
    e = AddVals( MV770P );
    e = AddVals( MV770Q );
    e = AddVals( MV770R );
    e = AddVals( MV770S );
    e = AddVals( MV770T );
    e = AddVals( MV770U );
    e = AddVals( MV770V );
    e = AddVals( MV770W );
    e = AddVals( MV770X );
                                            {* Add CS variables !!}
    if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
      e = errmsg(7520,i,MV770X,x2,k)
    endif;

    {* Ever been tested for AIDS now for all respondents and Knows place of test for those who know HIV *}
    if MV781 = notappl | MV751 <> 1 <=> MV783 <> notappl then
      e = errmsg(7527,MV751,MV781,MV783)
    endif;

                                                                          { Comment out unused variables !!!}
    k  = 0;     { Count of applicable questions }
    x  = 0;     { Count of yes - not used}
    x1 = 0;     { Count of missing - not used}
    x2 = 0;     { Count of notappl }

    e = AddVals( MV784A );
    e = AddVals( MV784B );
    e = AddVals( MV784C );
    e = AddVals( MV784D );
    e = AddVals( MV784E );
    e = AddVals( MV784F );
    e = AddVals( MV784G );
    e = AddVals( MV784H );
    e = AddVals( MV784I );
    e = AddVals( MV784J );
    e = AddVals( MV784K );
    e = AddVals( MV784L );
    e = AddVals( MV784M );
    e = AddVals( MV784N );
    e = AddVals( MV784O );
    e = AddVals( MV784P );
    e = AddVals( MV784Q );
    e = AddVals( MV784R );
    e = AddVals( MV784S );
    e = AddVals( MV784T );
    e = AddVals( MV784U );
    e = AddVals( MV784V );
    e = AddVals( MV784X );
                                            {* Add CS variables !!}
    if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
      e = errmsg(7532,i,MV784X,x2,k)
    endif;

    if MV793A <> notappl & (MV793A = 1 <=> MV793B = notappl) then
      e = errmsg(7534,MV793A,MV793B)
    endif;

    if ((MV767A = 6 & MV767B = 6 & MV767C = 6) | MV793B = 1) & MV793B = notappl then
      e = errmsg(7535,MV767A,MV767B,MV767C,MV793B);            { changed BERT }
    endif;
{ ---------------------------------------------------------------------------- }
PROC MREC80_EDT
preproc
                                                        { Set unused variables to <> notappl !!}
  if MV851A  = notappl | MV851B  = notappl | MV851C  = notappl | MV851D  = notappl | MV851E  = notappl |
     MV851F  = notappl | MV851G  = notappl | MV851H  = notappl | MV851I  = notappl | MV851J  = notappl |
     MV851K  = notappl | MV851L  = notappl then
     e = errmsg(12011,"REC80");
     errmsg("not notappl: MV851A=%d,MV851B=%d,MV851C=%d,MV851D=%d,MV851E=%d,MV851F=%d",MV851A,MV851B,MV851C,MV851D,MV851E,MV851F);
     errmsg("not notappl: MV851G=%d,MV851H=%d,MV851I=%d,MV851J=%d,MV851K=%d,MV851L=%d",MV851G,MV851H,MV851I,MV851J,MV851K,MV851L);
  endif;
  if (MV525 <> 0 & MV013 in 1,2) <=> MV820 = notappl then
    e = errmsg(8210,MV525,MV013,MV820)
  endif;

                                                                { Comment out unused variables !!!}
  k  = 0;     { Count of applicable questions }
  x  = 0;     { Count of yes - not used}
  x1 = 0;     { Count of missing - not used}
  x2 = 0;     { Count of notappl }

 { e = AddVals( MV781 );     denomoninator now all cases }
 { e = AddVals( MV783 );     denominator different -> need a thorough review of denominators - CONSIS6 }
  e = AddVals( MV785 );
  e = AddVals( MV823 );
  e = AddVals( MV825 );
  e = AddVals( MV837 );
  e = AddVals( MV844 );
  e = AddVals( MV847 );
  e = AddVals( MV848 );
  e = AddVals( MV849 );
                                            {* Add CS variables !!}
  if  (x2 <> 0 & x2 <> k) then            { Some notappl => All notappl }
    e = errmsg(8212,MV849,x2,k)
  endif;

  box  MV527      => lastsex;
       100-199   => 1;
       200-251   => 1;
       300-311   => 1;
       995       => 1;
                 => 0;
  endbox;
  if !lastsex then
    if MV832B <> notappl | MV832C <> notappl |
       MV833A <> notappl | MV833B <> notappl | MV833C <> notappl |
       MV835A <> notappl | MV835B <> notappl | MV835C <> notappl then
      e = errmsg(8230,MV832B,MV832C,MV833A,MV833B,MV833C,MV835A,MV835B,MV835C)
    endif;
  else
    if MV761 = notappl | MV835A = notappl then
      e = errmsg(8231,MV761,MV835A)
    endif;
    if MV832B <> notappl & MV835B = notappl then
      e = errmsg(8232,MV832B,MV835B)
    endif;
    if MV832C <> notappl & MV835C = notappl then
      e = errmsg(8233,MV832C,MV835C)
    endif;
  endif;

  if MV761 <> notappl & (MV761 = 1 <=> MV833A = notappl) then
    e = errmsg(8240,MV761,MV833A)
  endif;
  if MV761B <> notappl & (MV761B = 1 <=> MV833B = notappl) then
    e = errmsg(8241,MV761B,MV833B)
  endif;
  if MV761C <> notappl & (MV761C = 1 <=> MV833C = notappl) then
    e = errmsg(8242,MV761C,MV833C)
  endif;

  if MV525 <> 0 <=> MV836 = notappl then
    e = errmsg(8243,MV525,MV836)
  endif;

  if MV751 = 1 <=> MV825 = notappl |
   MV751 = 1 <=> MV825 = notappl {|
  (MV751 = 1 <=> MV828 = notappl | MV844 = notappl)  take out MV828 can be NA when never tested => MCONSIS
  ( MV751 = 1 <=> MV844 = notappl ) } then
    e = errmsg(8255,MV751,MV825,MV826,MV828,MV844)
  endif;

  if MV751 = 1 then
    if MV781 = 1 <=> MV826 = notappl |
       MV781 = 1 <=> MV827 = notappl |
       MV781 = 1 <=> MV828 = notappl |
       MV781 = 1 <=> MV829 = notappl then
        e = errmsg(8256,MV781,MV826,MV827,MV828,MV829)
    endif;
  endif;
