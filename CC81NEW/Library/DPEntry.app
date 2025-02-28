  alpha(4) strval;

  { function to check barcode for HIV }
  { barcode follows pattern: letter,digit,letter,digit,letter }
  { return: 0-No errors, 1-error in missing, 2-error in code, 3-error in check digit }
  function BarCodeError( alpha(5) barcode )
     numeric isOK = 0, checkdig = 0, base = 26, k, z;
    { check sequence of characteres }
    if !barcode in "99994":"99996" then
      if pos("?",barcode) & length(strip(barcode)) <> 1 then
        isOK = 1;
      elseif !pos("?",barcode) then
        do k = 1 until k > 5 by 2
          z = pos( barcode[k:1], "ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
          if !z then
            isOK = 1;
            break;
          elseif k < 5 then
            checkdig = checkdig + z-1;
          endif;
          if k < 5 then
            z = pos( barcode[k+1:1], "0123456789" );
            if !z then
              isOK = 2;
              break;
            else
              checkdig = checkdig + z-1;
            endif;
          endif;
        enddo;
        { Now validate the check digit }
        if !isOK then
          while checkdig >= base do
            checkdig = int(checkdig/base)+checkdig%base;
          enddo;
          if barcode[5:1] <> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[checkdig+1:1] then
            isOK = 3;
          endif;
        endif;
      endif;
    endif;
    BarCodeError = isOK;
  end;

  { Check relationship of child's mother/father to the household head  }
  { with the child relationship to the head                            }
  function valrelat( parent, child )
    numeric isOK;
    box  parent  : child => isOK;  { MOTHER/FATHER         - CHILD              }
            1,2  :   3   => 1;     { Head/Spouse           - Son/Daughter       }
              2  :  10   => 1;     { Spouse                - Stepchild          }
            3,4  :   5   => 1;     { Son/Daughter (in-law) - Grandchild         }
              5  :   9   => 1;     { Grandchild            - Other Relative     }
              6  :   1   => 1;     { Parent                - Head               }
              7  :   2   => 1;     { Parent-in-law         - Spouse             }
            6,7  :   8   => 1;     { Parent/Parent-in-Law  - Brother/Sister     }
              7  :   9   => 1;     { Parent-in-law         - Other Relative     }
              8  :   9   => 1;     { Brother/Sister        - Other Relative : Niece/nephew-blood }
           {  ?  :  11   => 1; }   { Niece/Nephew          - Other relative     }
           {  ?  :  13   => 1; }   { Niece/Nephew-marriage - Not related        }
              9  :   9   => 1;     { Other Relative        - Other Relative     }
          10,11  :  11   => 1;     { Adopted/Not Related   - Not Related        }
      missing,98 :98,missing => 1; { Unknown             - Unknown            }
  {{POLIG}       :  11   => 1;     { Co-wife               - Not related        } {POLIG}}
                 :       => 0;     { All others are incorrect                     }
    endbox;
    valrelat = isOK;
  end;

  { valid value in a two digits variable }
  function valid( xvar );
    valid = ( !special(xvar) & xvar <= 96 )
  end;

  { valid value in a four digits digits year }
  function validyr( xvar );
    validyr = ( !special(xvar) & xvar <= 9996 )
  end;

  { convert notappl to zero }
  function NAtoZero( xvar );
    if xvar = notappl then
      xvar = 0
    endif;
    NAtoZero = xvar;
  end;

  { convert notappl to zero }
  function NSmoke( xvar );
    if xvar in 888,missing,notappl then
      xvar = 0
    endif;
    NSmoke = xvar;
  end;

  { check if two values are equal }
  function noteq( xvar, dvar );
    noteq = ( xvar <> NAtoZero(dvar) );
  end;

  { Check special answers for questions with units and numbers }
  { !!! make sure to correctly adjust the ranges of the        }
  {     questions involved, for the function to work properly  }
  function badspecial( zunits, znumber )
    numeric z = 0;
    if zunits  = 9 & znumber <> missing & znumber <  93 |
       zunits <> 9 & znumber <> missing & znumber >= 93 |
       zunits = 0 &  znumber > 0                        |
       zunits  > 1 & znumber = 0 then
      z = 1;
    endif;
    badspecial = z;
  end;

  { Function to check if a date is after the date of interview }
  function afterint( vcheckm, vchecky, IntY, IntM );
    numeric z = 0;
    if validyr(vchecky) & vchecky > IntY |
       vchecky = IntY & valid(vcheckm) & vcheckm > IntM then
      z = 1
    endif;
    afterint = z;
  end;

  { check that number of years of school according to level is correct }
  function LevelYears( xlevel, xyears )
    { Verify the maximum grade for the level }
    numeric isOK = 1, z;
    box xlevel => z;
           0   => 2; { Preschool maximum }
           1   => 6; { Primary maximum }
           2   => 4; { Secondary maximum }
		   3   => 3; { Secondary maximum }
           4   => 4; { Higher maximum }
    endbox;
    if valid(xyears) & xyears > z then
      isOK = 0;
    endif;
    LevelYears = isOK;
  end;

  { returns the first or second digit (decpos) of a decimal variable }
  function GetDecimal( value, decpos )
    numeric wholeval, intval, decval;
    intval   = int( value + 0.00001 ) * 100;   //to properly round the decimal part
    wholeval = int( value * 100 + 0.00001 );
    decval   = wholeval - intval;
    strval   = edit( "99", decval );
    GetDecimal = tonumber( strval[decpos:1] );
  end;

  { Function to check all possible combinations of day, month for full dates }
  function DateOK( xday, xmonth, xyear, uptoday, uptomonth, uptoyear )
    numeric z, leapyear;
    box          xmonth            :        xday        => z;       { !!! }
     1,3,5,7,8,10,12,97,98,missing : 1-31,97,98,missing => 1;
     4,6,9,11                      : 1-30,97,98,missing => 1;
     2                             : 1-28,97,98,missing => 1;
                                   :                    => 0;
    endbox;
    leapyear = (xyear % 4 = 0);
    if z = 0 & leapyear & xday = 29 then z = 1 endif;
    if validyr(xyear) & xyear > uptoyear then
      z = 0
    elseif xyear = uptoyear & xmonth <= 12 & xmonth > uptomonth then
      z = 0
    elseif xyear = uptoyear & xmonth = uptomonth & xday <= 31 & xday > uptoday then
      z = 0
    endif;
    DateOK = Z;
  end;

  { function to convert "methods other" to search in calendar }
  function MethInStr( zmeth, zoth );
    numeric z;
    z = zmeth;
    if zmeth = 95 then
      z = zoth;                { other modern method position in string of methods in calendar }
    elseif zmeth = 96 then
      z = zoth + 1;            { other traditional method position in string of methods in calendar }
    endif;
    MethInStr = z;
  end;

  { calculates the number of days since january 1, 1900 up to the date given in the parameters }
  { the function assumes that the day (zday) within month (zmonth) is consistent               }
  function CDCode( zyear, zmonth, zday )
    numeric z, zz, zdays1 = 0, zdays2 = 0, leapday, totdays = 99999;
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

  { ramdomly impute a day between 1 and the maximum number of days in a month }
  function ImputeDay( yint, mint, dint, zyear, zmonth )
    numeric zz, leapday;
    { number of days up to the month in year zyear }
    leapday = (zyear % 4 = 0);
    box    zmonth     => zz;
      1,3,5,7,8,10,12 => 31;
                    2 => 28+leapday;
             4,6,9,11 => 30;
    endbox;
    if yint = zyear & mint = zmonth then
      zz = dint
    endif;
    ImputeDay = random( 1, zz );
  end

  { checks age and day of birth. check necessary for births in the month of interview and if day, month and year are present }
  function age_day_check( xday, xmonth, xyear, uptoday, uptomonth, uptoyear, agetofit )
    { returns:
      0 if age consistent,
      1 if inconsistent, and
     -1 if can't check age }
    { only need to do the check if:
      1. day is valid
      2. month of birth and month of interview are the same
      3. years are given and valid - otherwise can't check age }
    if valid( xday ) & valid( uptoday ) &
       valid( xmonth ) & xmonth = uptomonth &
       validyr( xyear ) & validyr( uptoyear ) & valid( agetofit ) then
      age_day_check = ( ( (uptoyear - xyear) - (xday > uptoday) ) <> agetofit );
    else
      age_day_check = (-1);
    endif;
  end;
