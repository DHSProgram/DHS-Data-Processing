
  { categories for level of education }
  function LevEduc( attend, edlevel )
    numeric z;
    box   attend : edlevel => z;
        missing  :         => missing;
       notappl,2 :         => 0;  { no education }
                 :     6   => 0;  { informal }
            8    :         => 8;  { don't know }
                 :     1,2 => edlevel;  { don't know }
                 :     4   => 4; { university }
                 :     5   => notappl; { vocational }
                 :         => edlevel;
    endbox;
    LevEduc = z;
  end;

  { grade of education }
  {Function modified by juste to add Qedlvel = Questionnaire edlvel }
  function GraEduc( edlevel, edlvel, edgrade )
    numeric z;
    if edlevel in 0,8,notappl,missing then
      z = notappl
    elseif edlevel = 2 & Qedlvel = 3 & edgrade < 98 then {Secondary 2nd cycle}
      z = edgrade + 4;
        else
          z = edgrade;
    endif;
        { if edgrade = 8 then z = 98 endif;  { in case that 8-DK } }
    GraEduc = z;
  end;

  { education in single years }
  function SingEduc( edlevel, edgrade, zprm, zsec )
    numeric z;
    box edlevel : edgrade => z;
        notappl :         => notappl;
                : missing => missing;
        missing :         => missing;
                :    98   => 98;
              8 :         => 98;
                :    97   => 97;
              0 :         => 0;     { Preschool and no education counted as 0 years }
              1 :         => edgrade;
              2 :         => edgrade+zprm;
              3 :         => edgrade+zprm+zsec;
    endbox;
    SingEduc = z;
  end;

  { education attainment }
  function AttnEduc( edlevel, edgrade )
    numeric z;
    box edlevel : edgrade => z;
        notappl :         => notappl;
        missing :         => missing;
              8 :         => 8;
              0 :         => 0;    { Preschool and no education counted as 0 years }
              1 :    6    => 2;    { 6 = years of primary school !! }
              1 :         => 1;
              2 :    7    => 4;    { 7 = years of secondary school !! }
              2 :         => 3;
              3 :         => 5;
    endbox;
    AttnEduc = z;
  end;

  { Recodes Yes/No variables code 1/2 to 1/0 }
  function YesNo( ynvar );
    if ynvar = 2 then
      ynvar = 0
    endif;
    YesNo = ynvar
  end

  { Recodes NA to zero }
  function NAtoZero( ynvar );
    if ynvar = notappl then
      ynvar = 0
    endif;
    NAtoZero = ynvar
  end
  
  { Juste: Recodes Missing to NA }
  function MissingToNA(xvalue);
    Numeric xvar = xvalue;
    if xvar = missing then
      xvar = NotAppl
    endif;
    MissingToNA = xvar
  end

  { Recodes 3 to zero }
  function Fr3toZro( ynvar );
    if ynvar = 3 then
      ynvar = 0;
    endif;
    Fr3toZro = ynvar;
  end;
  
  { Recodes 4 to zero }
  function Fr4toZro( ynvar );
    if ynvar = 4 then
      ynvar = 0;
    endif;
    Fr4toZro = ynvar;
  end;

  { Recodes 3 to 8 }
  function fr3to8( ynvar );
    if ynvar = 3 then
      ynvar = 8;
    endif;
    fr3to8   = ynvar;
  end;

  { to properly handle missings for the combination of variables with units and two digit numbers }
  function unitnumb( yunits, ynumbers );
    numeric z;
    if ynumbers = missing then
      z = yunits * 100 + 99
    else
      z = yunits * 100 + ynumbers
    endif;
    unitnumb = z;
  end

  { Converts alphanumeric variables to 1/0/8/missing one at a time }
  function alfa2num( vara, alpha(30) varb, alpha(1) varc );
    numeric z;
    { VARA = filter question }
    { VARB = the variable itself }
    { VARC = the character to search in the variable }
    { Example:
                          VARA  <--------- VARB-------->  <------- VARC --------->
        V785A = alfa2num( A1042,  alpha variable itself     The character to search );
    }
    if vara = missing | pos("?", varb) then
      z = missing;
    elseif vara = 8 then
      z = 8;
    elseif vara = 1 then
      z = ( pos(varc, varb) > 0 );
    else
      z = 0;
    endif;
    alfa2num = z;
  end;

  { Converts alphanumeric variables to 1/0/missing one at a time    }
  { This function differs from the previous one in that code 8 (DK) }
  { or alpha code "Z" is converted to zero instead of 8             }
  function alfaDKnum( vara, alpha(30) varb, alpha(1) varc );
    numeric z;
    { VARA = filter question }
    { VARB = the variable itself }
    { VARC = the character to search in the variable }
    { Example:
                          VARA  <--------- VARB-------->  <------- VARC --------->
        V785A = alfaDKnum( A1042,  alpha variable itself     The character to search );
    }
    if vara = missing | pos("?", varb) then
      z = missing;
    elseif vara = 8 | pos("Z",varb) then
      z = 0;
    elseif vara = 1 then
      z = ( pos(varc, varb) > 0 );
    else
      z = 0;
    endif;
    alfaDKnum = z;
  end;

  { calculates the number of days since january 1, 1900 up to the date given in the parameters }
  { the function assumes that the day (zday) within month (zmonth) is consistent               }
  function CDCode( zyear, zmonth, zday )
    numeric z, zz, zdays1 = 0, zdays2 =0, leapday, totdays = 99999;
    if !zyear in 1900:2170 | !zmonth in 1:12 | !zday in 1:31 then
      errmsg( "Invalid date to calculate CDC Year=%04d, Month=%02d, day=%02d", zyear, zmonth, zday );
    else
      { number of days between 1900 and zyear }
      do z = 1900 while z < zyear
        zdays1 = zdays1 + 365 + (z % 4 = 0);
      enddo;
      { number of days up to the month in year zyear }
      leapday = (zyear % 4 = 0);
      do z = 1 while z < zmonth
        box z          => zz;
          1,3,5,7,8,10 => 31;
                     2 => 28+leapday;
              4,6,9,11 => 30;
        endbox;
        zdays2 = zdays2 + zz;
      enddo;
      { total days }
      totdays = zdays1 + zdays2 + zday;
    endif;
    CDCode = totdays;
  end

