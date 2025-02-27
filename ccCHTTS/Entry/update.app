{Application 'UPDATE_123' logic file generated by CSPro }
PROC GLOBAL
{ This application 'updates' the status information in the master test sample file          }
{ based on the test results that have been received for that sample to date.  It            }
{ effectively implements the test/confirmatory test protocol for a given survey.  The       }
{ logic will most likely need to be changed from country to country to reflect              }
{ changes in the protocol.                                                                  }
{                                                                                           }
{ The status of each sample is determined by two variables :                                }
{                                                                                           }
{  -LSTAGE : the testing stage that a sample is at                                          }
{  -LSTATUS : whether a test tray containing a given sample has been generated yet          }
{                                                                                           }
{  If LSTATUS is TRUE, the program evaluates the test results according to the protocol.    }
{                                                                                           }
{  If the test results exist (ie the test has been completed, results entered and updated), }
{  the testing stage is set to either the next stage, or a value indicating that testing    }
{  is completed.  LSTATUS is set to zero.                                                   }
{                                                                                           }
{  If the test result(s) are not complete for a given stage, no further action is taken.    }

  set explicit;

  array pnum(10);
  numeric wrow, retest, interv, rtperc, idx, elisa1, elisa2;
  numeric w1, elisa3a, elisa3b, w2, i, j, wcancel, wtest, pctgrey, greyarea;
  alpha (7) plateid;

  { updates sample log with results of measurements }
  function update_log( alpha(1) xres, xmes, alpha(5) xid, alpha(5) xbar, xcol, xrow )
      if length( strip(xbar) ) = 5 & tonumber(xid) in 1:99999 then

        if loadcase( TESTLOG_DICT, xbar ) then
          if tonumber(xid) = LABID then
            { look first for occurrence with same plate id, otherwise use next available occurrence }
            errmsg("occs ltest =%d", totocc(LTEST));
            idx = 0;
            do varying wrow = 1 until wrow > totocc(LTEST)
              if LPLATE(wrow) = PLATE_ID then
                 errmsg( "break, wrow=%d, lplate=%s plate_id = %s", wrow,lplate(wrow), plate_id );
                 idx = wrow;
                 break;
              endif;
            enddo;

            LTDATED(idx) = TDATED;
            LTDATEM(idx) = TDATEM;
            LTDATEY(idx) = TDATEY;
            LPLATE(idx)  = PLATE_ID;
            LCOL(idx)    = xcol;
            LROW(idx)    = xrow;
            LMEAS(idx)   = xmes;
            LCUTOFF(idx) = TCUTOFF;
            LTRES(idx)   = ( pos("+", xres) > 0 );
            LTRETT(idx)  = getlabel(LTRES,(pos("+", xres) > 0));
            LCANCEL(idx) = wcancel; { flag for cancelled tray }
            LRET = 0;
{ **** remove this comment and delete the if after closing the comment if the grey area concept is used
            greyarea = TCUTOFF - TCUTOFF * pctgrey / 100;          { begining of the lower range of grey area up to the cut off }
            if LSTAGE = 1 & LTRES(idx) = 0 & LMEAS(idx) in greyarea:TCUTOFF then
              LRET = 2;           { mark the sample in the grey area in the first level to be retested }
            elseif LTRES(idx) = 0 & TESTTYPE = 1 then
*** }
            if LTRES(idx) = 0 & TESTTYPE = 1 then
              if retest = interv then
                LRET = 1;
                retest = 1;
              else
                LRET = 0;
                retest = retest+1;
              endif;
            endif;
            writecase( TESTLOG_DICT, xbar );
          else
            {+US}
            errmsg( "Lab id code %s not found in database, please check", xid );
            {US+}
            {{SP}
            errmsg( "Muestra %d no se encontro en la base de datos, por favor revise", xid );
            {SP}}
            {{FR}
            errmsg( "Code labo %d non trouv� dans la base de donn�es, veuillez v�rifier", xid );
            {FR}}
          endif;
        else
          {+US}
          errmsg( "Bar code %s not found in database, please check", xbar );
          {US+}
          {{SP}
          errmsg( "Codigo de barra %s no se encontro en la base de datos, por favor revise", xbar );
          {SP}}
          {{FR}
          errmsg( "Code barres %d non trouv� dans la base de donn�es, veuillez v�rifier", xid );
          {FR}}
        endif;

        { !! MODIFY HERE TO FIT COUNTRY SPECIFIC TEST PROTOCOL !! }
        { if positives are to be tested further (ie, Pepti Lav) then LSTAGE needs }
        { to be set to code 5 for positives (LFINRES=1) in levels 2 and 3         }
        elisa1  = notappl; elisa2  = notappl;
        elisa3a = notappl; elisa3b = notappl;

        do varying i = 1 until i > totocc(LTEST)
          if LTYPE(i) = 1 & LCANCEL(i) <> 1 then elisa1  = LTRES(i) endif;   { level 1, vironostika }
          if LTYPE(i) = 2 & LCANCEL(i) <> 1 then elisa2  = LTRES(i) endif;   { level 2, enzygnost   }
          if LTYPE(i) = 3 & LCANCEL(i) <> 1 then elisa3a = LTRES(i) endif;   { level 3, vironostika(repeat) }
          if LTYPE(i) = 4 & LCANCEL(i) <> 1 then elisa3b = LTRES(i) endif;   { level 3, enzygnost (repeat)  }
        enddo;
  {            errmsg("elisa3a=%d, elisa3b=%d, lfinres=%d, lstage=%d", elisa3a,elisa3b,lfinres,lstage);}
        { logic for 'STANDARD' setup with elisa 1 & elisa 2 for confirmatory test }
        if !LCANCEL(idx) then                      { if plate hasn't been canceled }
          if LSTAGE = 1 then                       { for level 1, ie, vironostika }
            if !special(elisa1) then
              box elisa1 : LRET => LFINRES;
                     1   :      => 97;             { level 1 positive }
                         :   1  => 97;             { level 1 negative & selected for level 2 }
                         :   2  => 97;             { level 1 negative but in grey area }
                         :      => 0;              { level 1 negative & not selected for level 2 }
              endbox;
              if LFINRES = 97 then
                LSTAGE = 2;
              else
                LSTAGE = 9;
              endif;
              LSTATUS = 0;
            else
              { do nothing }
            endif;
          elseif LSTAGE = 2 then                   { for level 2, ie, enzygnost }
            if !special(elisa2) then
              box elisa1 : elisa2 => LFINRES;
                     0   :   0    => 0;            { level 1 & 2 negative }
                     1   :   0    => 97;           { level 1 positive & 2 negative }
                     0   :   1    => 97;           { level 1 negative & 2 positive }
                     1   :   1    => 1;            { level 1 & 2 positive }
                         :        => default;
              endbox;
              if LFINRES = 97 then
                LSTAGE = 3;
              elseif LFINRES in 0,1 then
                LSTAGE = 9;                { testing completed }
              endif;
              LSTATUS = 0;
            else
              { do nothing }
            endif;
          elseif LSTAGE = 3 then                   { for level 3, ie, vironostika and enzygnost }
            if !special(elisa3a) & !special(elisa3b) then
              box elisa3a : elisa3b => LFINRES;
                     0    :    0    => 0;          { both repeats negative }
                     1    :    0    => 97;         { repeat level 1 positive & 2 negative }
                     0    :    1    => 97;         { repeat level 1 negative & 2 positive }
                     1    :    1    => 1;          { both repeats positive }
                          :         => default;
              endbox;
              if LFINRES =  97 then
                LSTAGE = 4;               { next stage Western blot assay }
              elseif LFINRES in 0,1 then
                LSTAGE = 9;               { testing completed }
              endif;
              LSTATUS = 0;
              errmsg("elisa3a=%d, elisa3b=%d, lfinres=%d", elisa3a,elisa3b,lfinres);
            else
              { do nothing }
            endif;
          elseif LSTAGE in 4,5 then                { RAPID/WESTERN BLOT assay }
            { DO NOTHING - should not be here anyway, status updated separately by data entry prog for RAPID type tests  }
          endif;                 { end LSTAGE }
        else                   { end tray cancelled by technician }
          LSTATUS = 0;         { !! RESET test status to allow another tray at current level to be generated }
          if LSTAGE = 1 then
            LRET = 0;          { cancel flag indicating selection for 10% retest }
          endif;
          TEMP_ID = xbar;
          TLABID  = LABID;
          writecase( TEMP_DICT, TEMP_ID ); { write out id's for fillplate to use }
          errmsg( "temp_id=%s", temp_id );
        endif; { LCANCEL}
        { set text versions of LSTATUS & LSTAGE }
        LSTATUST = getlabel(LSTATUS,LSTATUS);
        LSTAGET  = getlabel(LSTAGE,LSTAGE);
        writecase( TESTLOG_DICT, LBAR );
      endif;

    {===> END MODIFY !!}
    end;

PROC PNULL_FF
preproc

  plateid = sysparm()[1:7];
  rtperc  = tonumber( sysparm()[8:2] );   { percent retests passed as parameter }
  wcancel = tonumber( sysparm()[10:1] );  { parameter for canceling plate }
  pctgrey = tonumber( sysparm()[11:2] );  { parameter to determine the percentage to establish the grey area for negative samples }
  open(PLATE_DICT);
  open(TESTLOG_DICT);
  open(TEMP_DICT);

  { base seed for random number on plate id}
  seed( tonumber(plateid[4:4]) );
  interv = int(100/rtperc);
  retest = random(1,interv);

  wtest = TESTTYPE;
  if wtest = 4 then wtest = 5 endif; { !! Adjust this line to take account of  'parallel' tests for a given CS protocol !! }

  if loadcase( PLATE_DICT, plateid ) then
    do varying j = 1 until j > 12
      errmsg( "bar=%s",BAR_a(j) ); update_log( RES_A(j), MES_A(j), ID_A(j), BAR_A(j), j, 1 );
      errmsg( "bar=%s",BAR_b(j) ); update_log( RES_B(j), MES_B(j), ID_B(j), BAR_B(j), j, 2 );
      errmsg( "bar=%s",BAR_c(j) ); update_log( RES_C(j), MES_C(j), ID_C(j), BAR_C(j), j, 3 );
      errmsg( "bar=%s",BAR_d(j) ); update_log( RES_D(j), MES_D(j), ID_D(j), BAR_D(j), j, 4 );
      errmsg( "bar=%s",BAR_e(j) ); update_log( RES_E(j), MES_E(j), ID_E(j), BAR_E(j), j, 5 );
      errmsg( "bar=%s",BAR_f(j) ); update_log( RES_F(j), MES_F(j), ID_F(j), BAR_F(j), j, 6 );
      errmsg( "bar=%s",BAR_g(j) ); update_log( RES_G(j), MES_G(j), ID_G(j), BAR_G(j), j, 7 );
      errmsg( "bar=%s",BAR_h(j) ); update_log( RES_H(j), MES_H(j), ID_H(j), BAR_H(j), j, 8 );
    enddo;
    TACCEPT  = 1;
    TCANCEL  = wcancel;
    ACCEPTED = getlabel(TACCEPT, TACCEPT);
    writecase( PLATE_DICT, plateid );
  else
    {+US}
    errmsg( "Plate %s not found, no samples updated", plateid );
    {US+}
    {{SP}
    errmsg( "Bandeja %s no encontrada, no se actualizaran las muestras", plateid );
    {SP}}
    {+US}
    errmsg( "Plaque %s non trouv�e, pas de mise � jour des �chantillons", plateid );
    {US+}
  endif;
  close(PLATE_DICT);
  close(TESTLOG_DICT);
  close(TEMP_DICT);
  stop;
