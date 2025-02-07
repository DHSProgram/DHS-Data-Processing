PROC GLOBAL

  set explicit;

  { Working variables -- check the lengths needed for all fields }
  array pnum (10);               { array to keep track of id numbers of plates at different testing stages}
  array maxsamples (4);          { max samples for each test type/level }
  array alpha (100) elisa_str(6);
  alpha(48)  CSPro;                                    { CSPro executables directory }
  alpha(2)   opid, wopid;                              { operator ID}
  alpha(32)  bkpdr;                                    { backup drive }
  alpha(48)  wrkprj, data, results, outlines, PlateOD; { directories }
  alpha(48)  fname, fnameS, fnameT, reader, hyper;     { filenames }
  alpha(250) menu_title, cmdline, fnamelong;           { menu title, command lines }
  alpha(7)   plateid, newid, test_prefix;              { plate id's }
  alpha(4)   sid;
  alpha(48)  entry, dicts, testlog_dat, testlog_srt, newplate_dat;
  alpha(48)  common_dat, findsamps_dat, pnull_dat, cnull_dat;

  numeric always, mainmenu, submenu, choice, vcancel, chkok, last, i, j, x;
  numeric idx, nsamples, maxqc, ncases, retests, ok, copymode, determ_flag;
  numeric v1, v2, m1, m2, maxplate;
  numeric pctneg, pctpos, pctgrey;

  FILE WriteFile;
  FILE justone;                    { file handler to make sure that there are no concurrent sesions }
  FILE TRAYCONT;                   { file handler to read controls for trays }

  { ---------------------------------------------------------------------------- }
  { Basic file handling functions }

  { function to check the existence of system directories }
  function chkdir( alpha(48) xdir )
    if DirExist( xdir ) <> 1 then
      {+US} errmsg( "005: Directory %s does not exist", xdir ); {US+}
      {{SP} errmsg( "005: Directorio %s no existe", xdir ); {SP}}
      {{FR} errmsg( "005: Répertoire %s n'existe pas", xdir ); {FR}}
      stop(-1);
    endif;
  end;

  { function to prompt for a Yes/No user response }
  function ynprompt( alpha (128) promptstr );
    x = accept( promptstr,
        {+US} "Yes", "No" ); {US+}
        {{SP} "Si", "No" );  {SP}}
        {{FR} "Oui", "Non" ); {FR}}
    ynprompt = ( x = 1 );
  end;

  { !! write pnull.dat to make sure it exists !! }
  function mk_pnull();
    setfile( writefile, pnull_dat, create );
    FileWrite( writefile, " " );
    close( writefile );
  end;

  { writes out standard header for application pff file }
  function pff_head( wtype );
    FileWrite( writefile, "[Run Information]" );
    FileWrite( writefile, "Version=CSPro 2.6" );
    if wtype = 1 then  { batch }
      FileWrite( writefile, "AppType=Batch" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Files]" );
    else { data entry }
      FileWrite( writefile, "AppType=Entry" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[DataEntryInit]" );
      FileWrite( writefile, "OperatorID=Anyone" );
      FileWrite( writefile, "StartMode=ADD" );
      FileWrite( writefile, "FullScreen=YES" );
      FileWrite( writefile, "LOCK=MODIFY,VERIFY" );
      FileWrite( writefile, "NoFileOpen=YES" );
      FileWrite( writefile, "Interactive=Ask" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Files]" );
   endif;
  end;

  { run different CSPro executables }
  function runpff( alpha(32) app, wtype )
    fnameT = concat( strip(entry), "\", strip(app), '.pff' );
    { Using single quotes instad of double quotes as double quotes are needed in the string generated }
    if wtype = 0 then { data entry }
      cmdline = concat( '"', strip(cspro), '\CSEntry.exe" ', fnameT );
    elseif wtype = 1 then  { batch application }
      cmdline = concat( '"', strip(cspro), '\CSBatch.exe" ', fnameT );
    elseif wtype = 2 then  { sort application }
      cmdline = concat( '"', strip(cspro), '\CSSort.exe" ', fnameT );
    elseif wtype = 3 then{ export }
      cmdline = concat( '"', strip(cspro), '\CSExport.exe" ', fnameT );
    endif;
    ExecSystem( cmdline, wait );
  end;

  { Views a text file }
  function textview( alpha(32) textname )
    { Using single quotes instead of double quotes as double quotes are needed in the string generated }
    ExecSystem( concat( '"', strip(cspro), '\TextView.exe" ', textname ), wait );
  end;

  { select operator }
  function getop();
    x = 0;
    open( USERS_DICT );
    clear( USERS_DICT );
    x = selcase( USERS_DICT, "" )
          include( UNAME1, UNAME2 );
    opid = UOPID;
    close( USERS_DICT );
    getop = x;
  end;

  { menu to specify the type of display/print operation to be done over a plate }
  function getplate( viewtype )
  { parameter : viewtype                                                               }
  { Value 1 : display only untested (new) plates                                       }
  {       2 : display only tested plates that have not yet been updated                }
  {       3 : tested & accepted plates                                                 }
  {       4 : cancelled plates                                                         }
  {       5 : all plates                                                               }
  {       6 : display new plates & plates tested but not validated (for correct.app) - }
  {           passed only via parameter not user selectable                            }

    plateid = "";
    open( PLATE_DICT );
    clear( PLATE_DICT );
    if viewtype = 1 then  { new plates }
      x = selcase( PLATE_DICT, "" )
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TDATE = notappl & (TOPCODE = opid | URIGHTS) & !TCANCEL );
    elseif viewtype = 2 then  { tested, not accepted/validated }
      x = selcase( PLATE_DICT, "" )
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TDATE <> notappl & TACCEPT <> 1 & (TOPCODE = opid | URIGHTS) );
    elseif viewtype = 3 then { tested & validated }
      x = selcase( PLATE_DICT, "" )
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TACCEPT = 1 & (TOPCODE = opid | URIGHTS) & !TCANCEL );
    elseif viewtype = 4 then { cancelled }
      x = selcase( PLATE_DICT, "" )
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TCANCEL = 1 & (TOPCODE = opid | URIGHTS) );
    elseif viewtype = 5 then { all }
      x = selcase( PLATE_DICT, "" )
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TOPCODE = opid | URIGHTS );
    elseif viewtype = 6 then  {not accepted/validated }
      x = selcase(PLATE_DICT, "")
          include( TDATE, TEST_TYPE, ACCEPTED, TFLAG, TCANCEL )
            where( TACCEPT <> 1 & (TOPCODE = opid | URIGHTS) );
    endif;
    if x then
      plateid = PLATE_ID;
    else
      { do nothing }
    endif;
    close( PLATE_DICT );
    getplate = length( strip(plateid) );
  end;

  function saveplate( wsamples, alpha(7) pid );
    {+US}
    errmsg( "%d samples available for this level of testing", wsamples );
    saveplate = ( accept( concat( "Create ELISA worksheet for plate identifier ", pid,
                                  " with ", edit("Z9",wsamples), " samples?" ),
                                  "Yes : Create the worksheet",
                                  "No  : Exit without creating worksheet") = 1 );
    {US+}
    {{SP}
    errmsg( "%d muestras disponibles para este nivel de prueba", wsamples );
    saveplate = ( accept( concat( "Generar un hoja de trabajo ELISA para la bandeja ", pid,
                                  " con ", edit("Z9",wsamples), " muestras?" ),
                                  "Si : Generar la hoja de trabajo",
                                  "No : Salir sin generar la hoja de trabajo" ) = 1 );
    {SP}}
    {{FR}
    errmsg( "%d échantillons disponibles pour ce niveau de test", wsamples );
    saveplate = ( accept( concat( "Créer une feuille ELISA pour ce numéro de plaque ", pid,
                                  " avec ", edit("Z9",wsamples), " échantillons ?" ),
                                  "Oui : Créer la feuille",
                                  "Non : Quitter sans créer la feuille") = 1 );
    {FR}}

  end;

  { sorts dbs data file in order of LABID }
  function srt_testlog();
    fname = concat( strip(entry), "\srt_testlog.ssf" );
    setfile ( writefile, fname, create );
    FileWrite( writefile, "[CSSort]" );
    FileWrite( writefile, "Version=CSPro 2.6" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Dictionaries]" );
    FileWrite( writefile, "File=8/16/2004 11:10:47 AM,..\dicts\testlog.dcf" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[SortType]" );
    FileWrite( writefile, "Type=Record" );
    FileWrite( writefile, 'Using=TESTLOG_REC,"testlog record"' );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Keys]" );
    FileWrite( writefile, 'Key=Ascending,LABID,"Sample identification (lab version)"' );
    FileWrite( writefile, 'Key=Ascending,LSTAGE,"Testing stage"' );
    FileWrite( writefile, 'Key=Ascending,LSTATUS,"Stage completed"' );

    fname = concat(strip(entry), "\srt_testlog.ssf.pff" );
    setfile ( writefile, fname, create );
    FileWrite(  writefile, "[Run Information]" );
    FileWrite(  writefile, "Version=CSPro 2.6" );
    FileWrite(  writefile, "AppType=Sort" );
    FileWrite(  writefile, "" );
    FileWrite(  writefile, "[Files]" );
    FileWrite(  writefile, "Application=.\srt_testlog.ssf" );
    FileWrite( writefile, concat( "InputData=", testlog_dat ) );
    FileWrite( writefile, concat( "OutputData=", testlog_srt ) );
    FileWrite(  writefile, "" );
    FileWrite(  writefile, "Listing=.\srt_testlog.ssf.lst" );
    FileWrite(  writefile, "" );
    FileWrite(  writefile, "[Parameters]" );
    FileWrite(  writefile, "ViewListing=OnError" );
    FileWrite(  writefile, "ViewResults=No" );
    FileWrite(  writefile, "" );
    close( writefile );
    runpff( "srt_testlog.ssf", 2 );
  end;

  { finds next set of samples eligible for a testing stage/level and }
  { puts them in a temp file to be used as input for FILLPLATE       }
  { returns number of cases found }
  function findsamps( wstage, wsamples );
    srt_testlog();
    FileDelete( findsamps_dat );    { delete temp file }
    FileDelete( concat( strip(findsamps_dat), ".idx") ); { delete temp idx file }
    fnameS = concat( strip(data), "\TESTLOG.srt" );
    fname  = concat( strip(entry), "\FINDSAMPS.pff" );
    setfile( writefile, fname, create );
    pff_head( 1 );
    FileWrite( writefile, "Application=.\FINDSAMPS.bch" );
    FileWrite( writefile, concat( "InputData=", fnameS ) );
    FileWrite( writefile, "Listing=.\Findsamps.lst" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat( "TEMP_DICT=", findsamps_dat ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, "ViewListing=Never" );
    FileWrite( writefile, "ViewResults=No" );
    FileWrite( writefile, concat( "Parameter=", edit("9", wstage), edit("99", wsamples) ) );
    close( writefile );
    runpff( "findsamps", 1 );

    { count number of samples in temp data file.  This is }
    { returned as the result of the findsamps function    }
    open( TEMP_DICT );
    do x = 0 while loadcase( TEMP_DICT ) & length( strip(TEMP_ID) )
    enddo;
    close( TEMP_DICT );
    findsamps = x;
  end;

  { gnerates PFF for data entry app to scan in barcodes }
  function scn()
    open(COMMON_DICT);
    if loadcase( COMMON_DICT, sid ) & SLASTID then
      last = SLASTID;
    else
      last = 0;
      open( TESTLOG_DICT );
      while loadcase( TESTLOG_DICT ) do
        if length( strip(edit("ZZZZZ",LABID)) ) & LABID > last then last = LABID endif;
      enddo;
      close( TESTLOG_DICT );
    endif;
    close( COMMON_DICT );
    last = last+1;
    fname = concat( strip(entry), "\SCANIDS.pff" );
    setfile( writefile, fname, create );
    pff_head( 0 );
    FileWrite( writefile, concat( "Application=", strip(entry), "\SCANIDS.ent" ) );
    FileWrite( writefile, concat( "InputData=", strip(cnull_dat) ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat( "common_DICT=", common_dat) );
    FileWrite( writefile, concat( "TESTLOG_DICT=", testlog_dat) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, concat( "Parameter=", edit("99999", last), sid ) );
    close(writefile);
    runpff( "scanids", 0 );
  end;

  { generate PFF file and launches application to browse samples database }
  function view( wlock );
    fname = concat( strip(entry), "\browse_dbs.pff" );
    setfile( writefile, fname, create );
    pff_head( 0 );
    FileWrite( writefile, concat( "Application=", strip(entry), "\browse_dbs.ent" ) );
    FileWrite( writefile, concat( "InputData=", cnull_dat ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat( "TESTLOG_DICT=", testlog_dat ) );
    FileWrite( writefile, concat( "PLATE_DICT=", newplate_dat ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile,concat( "Parameter=",edit("9", wlock) ) );
    FileWrite( writefile, "" );
    close( writefile );
    runpff( "browse_dbs", 0 );
  end;

  { creates the PFF to print/view a plate }
  function prnplate( alpha(7) pid, reptype )
  { pid : plate id                                     }
  { reptype : 0 = temp report, saved in ..\entry       }
  {         : 1 = plate outline, saved in ..\outlines  }
  {         : 2 = test results, saved in ..\results    }

    if length( strip(pid) ) then  { check valid pid was returned }
      mk_pnull(); { create pnull.dat to make sure it exists }
      fname = concat( strip(entry), "\printplate.pff");
      setfile( writefile, fname, create );
      pff_head(1);
      FileWrite( writefile, "Application=.\printplate.bch" );
      FileWrite( writefile, "InputData=.\pnull.dat" );
      FileWrite( writefile, "Listing=.\printplate.lst" );
      if reptype = 1 then
        FileWrite( writefile, concat( "WriteData=", strip(outlines), "\", pid, ".wrt" ) );
      elseif reptype = 2 then
        FileWrite( writefile, concat( "WriteData=", strip(results), "\", pid, ".wrt" ) );
      else
        FileWrite( writefile, concat( "WriteData=.\", pid, ".wrt" ) ); { if just viewing listing, put in ..\entry directory }
      endif;
      FileWrite( writefile,"" );
      FileWrite( writefile,"[ExternalFiles]" );
      FileWrite( writefile, concat("PLATE_DICT=", newplate_dat ) );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Parameters]" );
      FileWrite( writefile, "ViewListing=Never" );
      FileWrite( writefile, "ViewResults=Yes" );
      FileWrite( writefile, concat("Parameter=", pid, edit("99",pctgrey) ) );
      close( writefile );
      runpff( "printplate", 1 );
    endif;
  end;

  { creates the PFF for application FILLPLATE }
  function fillplate( alpha (7) pid, wtest );
    fname = concat( strip(entry), "\FILLPLATE.pff" );
    setfile( writefile, fname, create );
    pff_head( 1 );
    FileWrite( writefile, "Application=.\Fillplate.bch" );
    FileWrite( writefile, concat("InputData=", findsamps_dat ) );
    FileWrite( writefile, "Listing=.\Fillplate.lst" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat("TESTLOG_DICT=", testlog_dat ) );
    FileWrite( writefile, concat("PLATE_DICT=", newplate_dat ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, "ViewListing=Never" );
    FileWrite( writefile, "ViewResults=No" );
    FileWrite( writefile, concat( "Parameter=", pid, elisa_str(wtest) ) );
    close( writefile );
    runpff( "fillplate", 1 );
  end;

  { generates a plate or copies it when cancelled }
  function genplate( stage, alpha(7) pid )
  { parameters : stage - testing stage                                                               }
  {            : pid - plate identifier                                                              }
  { Function has two modes -                                                                         }
  { 1. Generate a new plate - pid is blank,                                                          }
  {    - function will generate a new plate identifier                                               }
  {    - will pick up the next set of samples eligible for the specified stage of testing and create }
  {      new plate record                                                                            }
  { 2. Copy an existing plate (used when a plate has been cancelled) - pid is that of copied plate   }
  {    - function will use original pid as root of new pid                                           }
  {    - will create a new plate record with exactly same set of samples in same order, with new id  }

    copymode = ( length( strip(pid) ) > 0 );
    { get last plate number from plate log file }
    do varying idx = 1 until idx > 10
      pnum(idx) = 0
    enddo;
    open( PLATE_DICT );
    while loadcase( PLATE_DICT ) do
      idx  = tonumber( PLATE_ID[4:1] );              { level/stage of testing }
      x =  tonumber( PLATE_ID[5:3] );                { plate number           }
      if x > pnum(idx) then pnum(idx) = x endif;     { gets the greatest plate number for each level }
    enddo;
    pnum(stage) = pnum(stage) + 1;                   { increases plate number for current stage }
    close( PLATE_DICT );

    { !! MODIFY LOGIC HERE TO IMPLEMENT COUNTRY SPECIFIC PROTOCOL            }
    { if administrator, prompt for technician who will be assigned the plate }
    { if not administrator, logged in user will be assigned the plate        }
    if URIGHTS then
      if getop() then
        loadcase( USERS_DICT, wopid );
      endif;
    endif;
    { generate plate identifier }
    if !copymode then
      pid = concat( opid, test_prefix[stage:1], edit("9", stage), edit("999",pnum(stage) ) );
    else
      pid = concat( pid[1:4], edit("999",pnum(stage) ) ); { if making copy, use root of existing pid }
    endif;

    { use FINDSAMPS to get new set of samples for testing  }
    if !copymode then  { if copymode generate plate regardless, using list of id's in FINDSAMPS.DAT }
      nsamples = findsamps( stage, maxsamples(stage) );
    endif;

    { prompt to create new plate record & run fillplate.app }
    if nsamples | copymode then
      if copymode | saveplate( nsamples, pid ) then  { if copying plate we don't prompt user }
        fillplate( pid, stage );
        prnplate( pid, 1 ); { show plate }
        if stage = 3 & !copymode then { 2nd plate for stage 3, hardcoded 4 below is crappy hack to allow for 2nd test type will replace with more elegant code later }
          pid = concat( opid, test_prefix[4:1], edit("9", stage), edit( "999", pnum(stage) ) );
          fillplate( pid, 4 );
          prnplate( pid, 1 ); { show plate }
        endif;
      endif;
    else
      {+US} errmsg( "010: No samples in database available for this level of test" ); {US+}
      {{SP} errmsg( "010: No hay muestras en la base de datos para este nivel de la prueba" ); {SP}}
      {{FR} errmsg( "010: Pas d'échantillons dans la base de données disponibles pour ce niveau de test" ); {FR}}
    endif;

    { !! test whether plate record successfully created and put in return code !!}
    open( PLATE_DICT );
    genplate = loadcase( PLATE_DICT, pid );
    close( PLATE_DICT );
    {===> END MODIFY !!}
  end;

  function update_dbs( alpha(7) pid, tests, cancel );
    mk_pnull(); { create pnull.dat to make sure it exists }
    FileDelete( findsamps_dat );  { delete temp file }

    fname = concat( strip(entry), "\update.pff" );
    setfile( writefile, fname, create );
    pff_head( 1 );
    FileWrite( writefile, "Application=.\update.bch" );
    FileWrite( writefile, concat( "InputData=", pnull_dat ) );
    FileWrite( writefile, "Listing=.\update.lst" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat( "TESTLOG_DICT=", testlog_dat ) );
    FileWrite( writefile, concat( "PLATE_DICT=", newplate_dat ) );
    FileWrite( writefile, concat( "TEMP_DICT=", findsamps_dat ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, "ViewListing=Never" );
    FileWrite( writefile, "ViewResults=No" );
    FileWrite( writefile, concat( "Parameter=", pid, edit("99", tests), edit("9", cancel), edit("99",pctgrey) ) );
    close(writefile);
    runpff( "update", 1 );
  end;

  function valcancel( waction );
    { parms : 1 - validate /cancel }
    {         2 - delete           }
    {                              }
    vcancel = default;
    if waction = 1 then
      if getplate( 2 ) then  { can only validate/cancel tested plates }
        if TFLAG then
          {+US}
          errmsg( "022: Plate %s cannot be validated since the assay was invalid, you must CANCEL this plate and rerun the assay",
                  plateid );
          {US+}
          {{SP}
          errmsg( "022: Bandeja %s no se puede validar porque el ensayo fue invalido, debe cancelar esta bandeja correr de nuevo el ensayo",
                  plateid );
          {SP}}
          {{FR}
          errmsg( "022: Plaque %s ne peut pas être validée étant donné que le test n'était pas valide, Vous devez annuler cette plaque et reprendre le test",
                  plateid );
          {FR}}

          waction = 1;
          vcancel = 1;
        else
          {+US}
          if ynprompt( concat( "Have you fully reviewed and approved the results of plate ", plateid, " ?" ) ) then
            vcancel = ( accept( concat( "Validate/Cancel plate", plateid ),
                                        "Validate plate", "Cancel plate" ) = 2 );
          {US+}
          {{SP}
          if ynprompt( concat( "Ha revisado y aprobado los resultados de la bandeja ", plateid, " ?" ) ) then
            vcancel = ( accept( concat( "Validar/Cancelar la bandeja", plateid ),
                                        "Validar bandeja", "Cancelar bandeja" ) = 2 );
          {SP}}
          {{FR}
          if ynprompt( concat( "Avez-vous entièrement examiné et approuvé les résultats de la plaque ", plateid, " ?" ) ) then
            vcancel = ( accept( concat( "Valider/Annuler plaque", plateid ),
                                        "Valider plaque", "Annuler plaque" ) = 2 );
          {FR}}
          endif;
        endif;
      endif;
    else
      if getplate( 1 ) then { can only DELETE untested plates }
        {+US} vcancel = ynprompt( concat( "Confirm deletion of plate ", plateid, " ?" ) ); {US+}
        {{SP} vcancel = ynprompt( concat( "Confirmar borrado de la bandeja ", plateid, " ?" ) ); {SP}}
        {{FR} vcancel = ynprompt( concat( "Confirmez la suppression de la plaque ", plateid, " ?" ) ); {FR}}
      endif;
    endif;
    if ( waction = 1 &
       {+US}
       ( vcancel = 0 & ynprompt( concat( "Confirm validation of plate ", plateid, " ?" ) ) ) |
       ( vcancel = 1 & ynprompt( concat( "Confirm cancellation of plate ", plateid, " ?" ) ) ) ) |
       {US+}
       {{SP}
       ( vcancel = 0 & ynprompt( concat( "Confirmar validacion de la bandeja ", plateid, " ?" ) ) ) |
       ( vcancel = 1 & ynprompt( concat( "Confirmar cancelacion de la bandeja ", plateid, " ?" ) ) ) ) |
       {SP}}
       {{FR}
       ( vcancel = 0 & ynprompt( concat( "Confirmez validation de la plaque ", plateid, " ?" ) ) ) |
       ( vcancel = 1 & ynprompt( concat( "Confirmez annulation de la plaque ", plateid, " ?" ) ) ) ) |
       {FR}}
       ( waction = 2 & vcancel ) then
      if waction = 1 then
        if vcancel then  { generate new plate for samples if original plate cancelled }
          {+US}
          errmsg( "The system will now generate a new plate for retesting samples from the cancelled plate" );
          {US+}
          {{SP}
          errmsg( "El sistema generara una nueva bandeja para hacer de nuevo la prueba a la bandeja cancelada" );
          {SP}}
          {{FR}
          errmsg( "Le sytème va maintenant générer une nouvelle plaque pour retester les échantillons de la plaque annulée" );
          {FR}}
          update_dbs( plateid, retests, vcancel );
          genplate( tonumber( plateid[4:1]), plateid ); { machine parameter not used }
        else
          update_dbs( plateid, retests, vcancel );
        endif;
      else
        update_dbs( plateid, retests, vcancel );
        {+US} errmsg( "Plate %s deleted", plateid ); {US+}
        {{SP} errmsg( "Bandeja %s borrada", plateid ); {SP}}
        {{FR} errmsg( "Plaque %s supprimée", plateid ); {FR}}
      endif;
      {+US} errmsg( "Samples database has been updated" ); {US+}
      {{SP} errmsg( "La base de datos de las muestras ha sido actualizada" ); {SP}}
      {{FR} errmsg( "La base de données des échantillons est mise à jour" ); {FR}}
    endif;
  end;

  { generate the PFF for the TEST data entry application }
  function test( manual )
    if getplate( 1 ) then  { check valid plateid was returned }
      fname = concat(  strip(entry), "\test.pff" );
      setfile( writefile, fname, create );
      FileWrite( writefile, "[Run Information]" );
      FileWrite( writefile, "Version=CSPro 2.6" );
      FileWrite( writefile, "AppType=Entry" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[DataEntryInit]" );
      FileWrite( writefile, concat( "StartMode=Modify;", plateid ) );
      FileWrite( writefile, "Interactive=Both" );
      FileWrite( writefile, "Lock=Add,Verify,Stats" );
      FileWrite( writefile, "FullScreen=Yes" );
      FileWrite( writefile, "NoFileOpen=Yes" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Files]" );
      FileWrite( writefile, "Application=.\test.ent" );
      FileWrite( writefile, concat( "InputData=", newplate_dat ) );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[ExternalFiles]" );
      FileWrite( writefile, concat( "TESTLOG_DICT=", testlog_dat ) );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Parameters]" );
      FileWrite( writefile, concat( "Parameter=", edit("9", manual), edit("99",pctgrey) ) );
      close( writefile );
      runpff( "test", 0 );
      prnplate( plateid, 2 );
    endif;
  end;

  { enters result for Western Blot(3) or Peptilav(4) }
  function wb_test( wtype )
  { returns value used to control entry of fields LDETERM1/2 }
  { 1 : Add WB (LDETERM1)                                    }
  { 2 : Edit WB (LDTERM1)                                    }
  { 3 : Add Peptilav (LDTERM2)                               }
  { 4 : Edit Peptilav (LDTERM2)                              }

  { set up menu title for appropriate test }
    if wtype = 3 then
      menu_title = "Western Blot";
    else
      menu_title = "Pepti-Lav";
    endif;
    {+US}
    choice =  accept( menu_title,
                      "Add",
                      "Modify",
                      "Main Menu" );
    {US+}
    {{SP}
    choice =  accept( menu_title,
                      "Agregar un nuevo resultado",
                      "Modificar los resultados de pruebas ya relaizadas",
                      "Regresar al menu principal" );
    {SP}}
    {{FR}
    choice =  accept( menu_title,
                      "Ajouter un nouveau résultat",
                      "Modifier les résultats des tests déjà saisis",
                      "Revenir au menu principal" );
    {FR}}
    ok = 0;
    if wtype = 3 then          { WB }
      if choice = 1 then       { add wb case }
        ok = selcase( TESTLOG_DICT, "" )
             include( LABID, LDATE, LSTAGE, LSTAGET, LDETERM1, LDETERM2 )
               where( LSTAGE = 4 & special(LDETERM1) );
        if ok then ok = choice endif;
      elseif choice = 2 then   { edit wb }
        ok = selcase( TESTLOG_DICT, "" )
             include( LABID, LDATE, LSTAGE, LSTAGET, LDETERM1, LDETERM2 )
               where( LDETERM1 in 0:3,7,9 );
        if ok then ok = choice endif;
      endif;
    else                       { Pepti-Lav }
      if choice = 1 then
        ok = selcase( TESTLOG_DICT, "" )
             include( LABID, LDATE, LSTAGE, LSTAGET, LDETERM1, LDETERM2 )
               where( LSTAGE = 5 & LDETERM2 in 9,notappl );
        if ok then ok = choice+2 endif;
      elseif choice = 2 then
        ok = selcase( TESTLOG_DICT, "" )
             include( LABID, LDATE, LSTAGE, LSTAGET, LDETERM1, LDETERM2 )
               where( LDETERM2 in 1:3,7,9 );
        if ok then ok = choice+2 endif;
      endif;
    endif;
    if !ok then
      {+US}
      errmsg( "No samples available for the intended operation" );
      {US+}
    endif;
    wb_test = OK;
  end;

  { generates PFF file for TREPRTS application }
  function reports( viewtype );
    fname = concat( strip(entry), "\TREPORTS.pff" );
    setfile( writefile, fname, create );
    pff_head(1);
    FileWrite( writefile, "Application=.\TREPORTS.bch" );
    FileWrite( writefile, concat( "InputData=", testlog_dat ) );
    FileWrite( writefile, "Listing=.\TREPORTS.lst" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile,concat( "Parameter=", edit("9", viewtype) ) );
    FileWrite( writefile, "ViewListing=Always" );
    FileWrite( writefile, "ViewResults=No" );
    close( writefile );
    runpff( "TREPORTS", 1 );
  end;

  { allows editing of lab id's if mistake was made in placement of DBS }
  function correct();
    if getplate( 6 ) then  { only plates that have not been validated can be corrected }
      fname = concat( strip(entry), "\correct.pff" );
      setfile( writefile, fname, create );
      FileWrite( writefile, "[Run Information]" );
      FileWrite( writefile, "Version=CSPro 2.6" );
      FileWrite( writefile, "AppType=Entry" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[DataEntryInit]" );
      FileWrite( writefile, concat( "StartMode=Modify;", plateid ) );
      FileWrite( writefile, "Interactive=Both" );
      FileWrite( writefile, "Lock=Add,Verify,Stats" );
      FileWrite( writefile, "FullScreen=Yes" );
      FileWrite( writefile, "NoFileOpen=Yes" );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Files]" );
      FileWrite( writefile, "Application=.\correct.ent" );
      FileWrite( writefile, concat( "InputData=", newplate_dat ) );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[ExternalFiles]" );
      FileWrite( writefile, concat( "TESTLOG_DICT=", testlog_dat ) );
      FileWrite( writefile, "" );
      FileWrite( writefile, "[Parameters]" );
      FileWrite( writefile, "" );
      close( writefile );
      runpff( "correct", 0 );
      prnplate( plateid, 2 );
    endif;
  end;

  { creates the PFF file for applicaction ExternQC used to    }
  { generate a listing of the samples slected for external QC }
  function ExternQC();
    fname = concat( strip(entry), "\ExternQC.pff" );
    setfile( writefile, fname, create );
    pff_head( 1 );
    FileWrite( writefile, "Application=.\ExternQC.bch" );
    FileWrite( writefile, concat( "InputData=", testlog_dat ) );
    FileWrite( writefile, concat( "OutputData=", strip(data), "\tmplog.dat" ) );
    FileWrite( writefile, "Listing=.\ExternQC.lst" );
    FileWrite( writefile, "WriteData=.\ExternQC.wrt" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, concat( "Parameter=", edit("999", pctneg), edit("999", pctpos) ) );
    FileWrite( writefile, "ViewListing=Never" );
    FileWrite( writefile, "ViewResults=No" );
    close( writefile );
    runpff( "ExternQC", 1 );
    textview( ".\ExternQC.wrt" );
    {+US}
    if ynprompt( "Do you accept the samples selected for External Quality Control QC" ) then
    {US+}
    {{SP}
    if ynprompt( "Acepta las muestras seleccionadas para control de calidad externo" ) then
    {SP}}
    {{FR}
    if ynprompt( "Acceptez-vous les échantillons sélectionnés pour le contrôle de qualité externe CQ" ) then
    {FR}}
      FileDelete( concat( strip(testlog_dat), "*" ) );
      FileRename( concat( strip(data), "\tmplog.dat" ), strip(testlog_dat) );
      {+US} errmsg( "The system will shut down to reindex the database on re-entering" ); {US+}
      {{SP} errmsg( "El sistema se cancelara para reindexar la base de datos cuando se reingrese" ); {SP}}
      {{FR} errmsg( "Le système s'arrêtera pour réindexer la base de données à la reprise" ); {FR}}
      FileDelete( justone );        { delete file justone to avoid messages when re-entering the system }
      stop(-1);   { quit the system to be sure that a new testlog IDX file is created }
                  { when enetering the system again                                   }
    endif;
  end;

  { A batch application to produce matchdhs.dat file for use in matching with DHS survey    }
  { data to produce response rates.  The data file does NOT include HIV test results, just  }
  { information about the status of a test                                                  }
  function HIVtoDHS();
    fname = concat( strip(entry), "\HIVtoDHS.pff" );
    setfile( writefile, fname, create );
    pff_head( 1 );
    FileWrite( writefile, "Application=.\HIVtoDHS.bch" );
    FileWrite( writefile, concat( "InputData=", testlog_dat ) );
    FileWrite( writefile, "Listing=.\HIVtoDHS.lst" );
    FileWrite( writefile, "WriteData=.\HIVtoDHS.wrt" );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[ExternalFiles]" );
    FileWrite( writefile, concat( "MATCHDHS=", strip(data), "\matchdhs.dat" ) );
    FileWrite( writefile, "" );
    FileWrite( writefile, "[Parameters]" );
    FileWrite( writefile, "ViewListing=Never" );
    FileWrite( writefile, "ViewResults=No" );
    close( writefile );
    runpff( "HIVtoDHS", 1 );
  end;

  { finds the drive letter where the flash memory is located }
  function searchdrive()
    numeric found = 0, lenstr;
    lenstr = length( strip(bkpdr) );
    do i = 1 while i <= 5 & !found
      bkpdr = concat( "DEFGH"[i:1], bkpdr[2:lenstr] );
      if DirExist( strip(bkpdr) ) then           // if using CSPro 4.1 change it for FileExist
        found = 1;
      endif;
    enddo;
    searchdrive = found;
  end;

  { makes a backup of directories containig data }
  function backup()
    if !searchdrive() | !DirExist( strip(bkpdr) ) then
      errmsg( 070, strip(bkpdr) );
      searchdrive();
    endif;
    { if directory found make backup }
    if DirExist( strip(bkpdr) ) then
      FileCopy( concat( strip(data),     "\*.* " ), concat( strip(bkpdr), "\data" ) );     { main data }
      FileCopy( concat( strip(outlines), "\*.* " ), concat( strip(bkpdr), "\outlines" ) ); { trays generated }
      FileCopy( concat( strip(results),  "\*.* " ), concat( strip(bkpdr), "\results" ) );  { trays with results }
      FileCopy( concat( strip(plateOD),  "\*.* " ), concat( strip(bkpdr), "\plateOD" ) );  { trays generated }
      {+US} errmsg( "Backup to drive:%s completed", strip(bkpdr) ); {US+}
      {{SP} errmsg( "Backup a unidad:%s finalizada", strip(bkpdr) ); {SP}}
    else
      errmsg( 071, strip(bkpdr) );
    endif;
  end;

PROC ENTER_FF
preproc

  { !! APPLICATION PARAMETERS !! }
  CSPro    = PathName( CSPRO );     { CSPro version/directory }
  maxplate = 96;         { !! do not change unless using non-standard plate, in which case many other modifications required !!}
  retests  = 10;         { !! percentage of negative samples to be retested (internal QC) }
  pctneg   = 5;          { !! percentage of negative samples for external QC }
  pctpos   = 100;        { !! percentage of positive samples for external QC }
  pctgrey  = 10;         { !! percentage to define the grey area of the cut off for negative ODs }
  wopid    = "";
  sid      = "XX50";     { id for configuration file }

  { Set work drive & project directory name }
  bkpdr    = "F:\ccCHTTS";       { backup drive, flash memory used in DR }
  wrkprj   = "C:\ccCHTTS";

  { Environment variables used for default directory names }
  entry    = concat( strip(wrkprj), "\entry" );      { applications and forms }
  data     = concat( strip(wrkprj), "\data" );       { all data files reuired by system }
  outlines = concat( strip(wrkprj), "\outlines" );   { plates generated }
  results  = concat( strip(wrkprj), "\results" );    { plates with results }
  dicts    = concat( strip(wrkprj), "\dicts" );      { dictionaries }
  PlateOD  = concat( strip(wrkprj), "\PlateOD" );    { work area to store results from spectrometer }

  { Environment variables used for default FILE names }
  testlog_dat   = concat( strip(data), "\", "Testlog.dat" );
  testlog_srt   = concat( strip(data), "\", "Testlog.srt" );
  newplate_dat  = concat( strip(data), "\", "Newplate.dat" );
  common_dat    = concat( strip(data), "\", "Common.dat" );
  findsamps_dat = concat( strip(data), "\", "Findsamps.dat" );
  pnull_dat     = concat( strip(data), "\", "Pnull.dat" );
  cnull_dat     = concat( strip(data), "\", "Cnull.dat" );

  { !! set single letter codes for ELISA tests !! }
  test_prefix = "VEVE~~~";

  { Check that there are not two concurrent sessions of CSPro loaded at the same time }
  SetFile( justone, concat( strip(entry), "\justone.txt" ) );
  if FileExist( justone ) then
    FileRead( justone, cmdline );
    errmsg( "*** %s", strip(cmdline) );
    if accept(
  {+US}       "Reset","Yes","No" {US+}
  {{SP}       "Reiniciar","Yes","No" {SP}}
  {{FR}       "Re-initialiser","Oui","Non" {FR}}
             ) = 1 then
      FileDelete( justone )
    else
      stop(-1);
    endif;
  endif;

  FileWrite( justone,
  {+US} "There is another session running or the computer crashed while running" {US+}
  {{SP} "Hay otra sesión corriendo o el computador falló mientras corría" {SP}}
  {{FR} "Il y a une autre session en cours ou bien que l'ordinateur s'est planté en cours d'exécution" {FR}}
           );
  close( justone );

  { Check that all directories needed exist }
  chkdir( wrkprj );
  chkdir( entry );
  chkdir( dicts );
  chkdir( data );
  chkdir( results );
  chkdir( outlines );

  { reads control layouts from external file ..\data\TRAYCONT.TXT }
  SetFile( TRAYCONT, concat( strip(data), "\TRAYCONT.TXT" ) );
  if !FileExist( TRAYCONT ) then
    {+US} errmsg( "File with tray controls ..\DATA\TRAYSCONT.TXT doesn't exist" ); {US+}
    {{SP} errmsg( "Archivo con controles de bandejas ..\DATA\TRAYCONT.TXT no existe" ); {SP}}
    {{FR} errmsg( "Fichiers des contrôles de la plaque ..\DATA\TRAYSCONT.TXT n'existe pas" ); {FR}}
    stop(-1);
  endif;
  { !! number of lines to be read from tray control file }
  do i = 1 while i <= 16            { should be equal to the maximum number of controls in all trays }
    FileRead( TRAYCONT, cmdline );
    elisa_str(1) = concat( strip(elisa_str(1)), cmdline[ 3:5] );  { controls for elisa 1 }
    elisa_str(2) = concat( strip(elisa_str(2)), cmdline[10:5] );  { controls for elisa 2 }
    elisa_str(3) = concat( strip(elisa_str(3)), cmdline[17:5] );  { controls for repeat elisa 1 }
    elisa_str(4) = concat( strip(elisa_str(4)), cmdline[24:5] );  { controls for repeat elisa 2 }
  enddo;
  { calculate number of samples per tray by subtracting the number of controls }
  { from max number of samples in plate - max possible is usually 96           }
  do i = 1 until i > 4
    maxsamples(i) = maxplate - int( length( strip( elisa_str(i) ) ) / 5 );
  enddo;
  close( TRAYCONT );

  { set attributes for certain variables }
  set attributes ( UPCHECK ) hidden;
  set attributes ( UOPID ) assisted on;
  set attributes ( LDETERM1 ) assisted on;
  set attributes ( LDETERM2 ) assisted on;

PROC ENTER_LEVEL
preproc
  enter USERS_FF;
  { sort samples file and overwrite current version with sorted one  }
  { this is to reduce the size of the file by removing deleted cases }
  { *** GR 05/26/2009 We may consider removing the remaining         }
  {     instructions in this procedure as a result of the addition   }
  {     of variable LFIXLAST to DCF testlog that guarantees that     }
  {     records are kept of fix lenght since scanning                }
  srt_testlog();
  if FileSize( strip(testlog_srt) ) > 0 then
    FileDelete( concat( strip(testlog_dat), "*" ) );
    FileRename( testlog_srt, testlog_dat );
    FileRename( concat( strip(testlog_srt), ".idx" ), concat( strip(testlog_dat), ".idx" ) );
  endif;

PROC ECLUSTER
preproc
  $ = 0;
  noinput;

postproc
  always = 1;
  while always do   { loop forever }
    { Get the selected option }
    { !! MODIFY MENU OPTIONS !! }
    {+US}
    mainmenu = accept( concat( "CSPRO HIV Test Tracking System: User ", strip(UNAME1), " ", strip(UNAME2) ),
              {01}  "S  Scan samples received from field...................S",
              {02}  "W  Create or manage ELISA Worksheets..................A",
              {03}  "E  Read/Enter/Validate Test Results...................E",
              {04}  "R  Status reports.....................................R",
              {05}  "M  Administrator menu.................................M",
              {06}  "Q  Quit program.......................................Q" );
    {US+}
    {{SP}
    mainmenu = accept(concat( "CSPRO HIV Test Tracking System: Usuario ", strip(UNAME1), " ", strip(UNAME2)),
              {01}  "S  Escanear muestras recibidas del campo..............S",
              {02}  "W  Generar o manejar una hoja de trabajo ELISA........A",
              {03}  "E  Leer/Digitar/Validar resultados de una prueba......E",
              {04}  "R  Reportes...........................................R",
              {05}  "M  Menu de administracion.............................M",
              {06}  "Q  Salir del programa.................................Q" );
    {SP}}
    {{FR}
    mainmenu = accept( concat( "Système CSPRO de Suivi du test HIV : Utilisateur ", strip(UNAME1), " ", strip(UNAME2) ),
              {01}  "S  Scanner les échantillons reçus du terrain..........S",
              {02}  "W  Créer ou gérer des feuilles ELISA..................A",
              {03}  "E  Lire/Entrer/Valider les résultats de test..........E",
              {04}  "R  Rapport de situation...............................R",
              {05}  "M  Menu de l'administrateur...........................M",
              {06}  "Q  Quitter le programme...............................Q" );
    {FR}}

    if mainmenu in 0,6 then   { exit the system }
      if FileExist( justone ) then
        FileDelete( justone )
      endif;
      stop(-1);       { quit the process }
    elseif mainmenu = 1 then {SCN}
      scn();
    elseif mainmenu = 2 then  { worksheet menu }
      submenu = 1;
      while submenu in 1:4 do
        {+US}
        submenu = accept( "Worksheets Menu",
                {01}  "1  Generate new worksheet for Bioelisa................1",
                {02}  "2  Generate new worksheet for Enzygnost...............2",
                {03}  "3  Generate worksheets for REPEAT ELISA tests.........3",
                {04}  "P  Print/view individual plate/worksheet data.........P",
                {05}  "D  Delete a worksheet (unprocessed worksheets only)...D",
                {06}  "R  Return to Main Menu................................R" );
        {US+}
        {{SP}
        submenu = accept( "Menu de Hojas de Trabajo",
                {01}  "1  Generar hoja de trabajo para Bioelisa..............1",
                {02}  "2  Generar hoja de trabajo para Enzygnost.............2",
                {03}  "3  Generar hoja de trabajo para repeticion ELISA......3",
                {04}  "P  Imprimir/mirar una bandeja/hoja de trabajo.........P",
                {05}  "D  Borrar hoja de trabajo(solo hojas no procesadas)...D",
                {06}  "R  Regresar al menu principal.........................R" );
        {SP}}
        {{FR}
        submenu = accept( "Menu des feuilles ELISA",
                {01}  "1  Générer une nouvelle grille pour Bioelisa...............1",
                {02}  "2  Générer une nouvelle grille pour Enzygnost..............2",
                {03}  "3  Générer des grilles pour repéter des tests ELISA.......3",
                {04}  "P  Imprimer/afficher une plaque/données d'une grille........P",
                {05}  "D  Supprimer une grille (grilles non traitées seulement)....D",
                {06}  "R  Retour au menu principal..................................R" );
        {FR}}
        if submenu in 0,6 then        { do nothing, go back to previous menu }
        elseif submenu in  1:3 then
          genplate( submenu, "" );    { GEN }
        elseif submenu =  4 then      { PRN }
          {+US}
          choice = accept( "View Menu",
                  {01}  "N  New Plates not yet tested..........................N",
                  {02}  "P  Plates tested, not accepted........................P",
                  {03}  "T  Tested and accepted plates.........................T",
                  {04}  "C  Cancelled plates...................................C",
                  {05}  "A  All ...............................................A",
                  {06}  "M  Return to previous menu............................M" );
          {US+}
          {{SP}
          choice = accept( "Menu para Visualizar/Imprimir Bandejas",
                  {01}  "N  Bandejas nuevas aun no procesadas..................N",
                  {02}  "P  Bandejas procesadas pero aun no aceptadas..........P",
                  {03}  "T  Bandejas procesadas y aceptadas....................T",
                  {04}  "C  Bandejas canceladas................................C",
                  {05}  "A  Todas..............................................A",
                  {06}  "M  Regresar al menu anterior..........................M" );
          {SP}}
          {{FR}
          choice = accept( "Menu d'affichage",
                  {01}  "N  Nouvelles plaques non encore testées...............N",
                  {02}  "P  Plaques testées, non acceptées.....................P",
                  {03}  "T  Plaques testées et acceptées.......................T",
                  {04}  "C  Plaques annulées...................................C",
                  {05}  "A  Toutes.............................................A",
                  {06}  "M  Retour au menu précédent...........................M" );
          {FR}}
          if choice in 0,6 then { do nothing, return to previous menu }
          elseif getplate( choice ) then
            prnplate( plateid, 0 )
          endif;
        elseif submenu =  5 then { PRN }
          valcancel( 2 );
        endif;
      enddo;
    elseif mainmenu = 3 then { Results }
      submenu = 1;
      while submenu in 1:5 do
        {+US}
        submenu = accept( "Results & Validation Menu",
                {01} "R  Read OD values from spectrometer...................R",
                {02} "E  Enter OD values manually...........................E",
                {03} "W  Enter results for Western Blot.....................W",
                {04} "-------------------------------------------------------",
                {05} "V  Validate a worksheet ..............................V",
                {06} "M  Return to Main Menu................................M" );
        {US+}
        {{SP}
        submenu = accept( "Menu de Resultados y Validacion",
                {01} "R  Leer valores de DO del espectrometro...............R",
                {02} "E  Digitar valores de DO manualmente..................E",
                {03} "W  Digitar resultados para Western Blot...............W",
                {04} "P  Digitar resultados para Pepti-Lav..................P",
                {05} "V  Validar hoja de trabajo............................V",
                {06} "M  Regresar al menu principal.........................M" );
        {SP}}
        {{FR}
        submenu = accept( "Menu de Résultats & Validation",
                {01} "R  Lire les valeurs DO du spectromètre................R",
                {02} "E  Entrer les valeurs DO manuellement.................E",
                {03} "W  Entrer les résultats de Western Blot...............W",
                {04} "P  Entrer les résultats de Pepti-Lav..................P",
                {05} "V  Valider une grille.................................V",
                {06} "M  Retour au menu principal...........................M" );
        {FR}}
        if submenu in 0,4,6 then       { do nothing, go back to previous menu }
        elseif submenu =  1 then     { read OD }
          test( 0 );
        elseif submenu =  2 then     { enter OD manually }
          test( 1 );
        elseif submenu in 3,4 then
          determ_flag = wb_test(submenu); { flag to control entry for rapid/wb/determine tests }
          if determ_flag then
            enter TESTLOG_FF;
            writecase( TESTLOG_DICT );
          endif;
          clear( TESTLOG_DICT );
          close( TESTLOG_DICT );
        elseif submenu =  5 then     { validate worksheet }
          valcancel( 1 );
        endif;
      enddo;
    elseif mainmenu = 4 then { reports }
      {+US}
      submenu = accept( "Report Menu",
              {01}  "A  All samples........................................A",
              {02}  "T  Samples awaiting any test (results not final)......T",
              {03}  "1  Samples awaiting Bioelisa..........................1",
              {04}  "2  Samples awaiting Enzygnost....CONFIRMATORY tests...2",
              {05}  "3  Samples awaiting ELISA for repetition--------------3",
              {06}  "4  Samples awaiting Western Blot......................4",
              {07}  "5  Samples for external quality control...............5",
              {08}  "6  Return to main menu................................6" );
      {US+}
      {{SP}
      submenu = accept( "Menu de Reportes",
              {01}  "A  Todas las muestras.........................................A",
              {02}  "T  Muestras esperando cualquier prueba - sin resultado final..T",
              {03}  "1  Muestras esperando Bioelisa................................1",
              {04}  "2  Muestras esperando Enzygnost - prueba confirmatoria........2",
              {05}  "3  Muestras esperando ELISA para repeticion...................3",
              {06}  "4  Muestras esperando Western Blot............................4",
              {07}  "5  Muestras para control de calidad externo...................5",
              {08}  "6  Regresar al menu principal.................................6" );
      {SP}}
      {{FR}
      submenu = accept( "Menu des rapports",
              {01}  "A  Tous les échantillons.........................................A",
              {02}  "T  Echantillons en attente d'un test (resultats non finals).......T",
              {03}  "1  Echantillons en attente de Bioelisa............................1",
              {04}  "2  Echantillons en attente d'Enzygnost....tests de confirmation...2",
              {05}  "3  Echantillons en attente de test ELISA de repétition...........3",
              {06}  "4  Echantillons en attente de Western Blot........................4",
              {07}  "5  Echantillons pour le contrôle de qualité externe.............5",
              {08}  "6  Retour au menu principal.......................................6" );
      {FR}}
      if submenu in 0,8 then       { do nothing, go back to previous menu }
      elseif submenu in 1:7 then
        reports( submenu );
      endif;
    elseif mainmenu = 5 then
      if URIGHTS then
        {+US}
        submenu = accept( "Administrator menu",
                {01}  "M  Manipulate single samples to view/mark/retest......M",
                {02}  "C  Correct sample positions in plate..................C",
                {03}  "V  View individual sample information.................V",
                {04}  "B  Backup data to a flash memory/other drive..........B",
                {05}  "Q  Generate samples for external quality control......Q",
                {06}  "E  Export test status data for Sample Weights.........E",
                {07}  "R  Return to Main Menu................................R" );
        {US+}
        {{SP}
        submenu = accept( "Menu de Administracion",
                {01}  "M  Manipular muestras simples (ver/marcar/resetear)...M",
                {02}  "C  Corregir la posicion de una muestra en la bandeja..C",
                {03}  "V  Mirar informacion especifica de una muestra........V",
                {04}  "B  Backup datos a una memoria flash...................B",
                {05}  "Q  Generar muestras para control de calidad externo...Q",
                {06}  "E  Exportar estado de prueba para calculo de pesos....E",
                {07}  "R  Regresar al menu principal.........................R" );
        {SP}}
        {{FR}
        submenu = accept( "Menu de l'administrateur",
                {01}  "M  Manipulate single samples to view/mark/retest................M",
                {02}  "C  Corriger la position de l'échantillon dans la plaque.........C",
                {03}  "V  Afficher les informations pour un écantillon.................V",
                {04}  "B  Sauvegarder les données sur disquette........................B",
                {05}  "Q  Générer des échantillons pour contrôle de qualité externe....Q",
                {06}  "E  Exporter les données de statut de test pour la pondération...E",
                {07}  "R  Retour au menu principal.....................................R" );
        {FR}}

        if submenu in 0,7 then  { do nothing, go back to previous menu }
        elseif submenu = 1 then
          view( 1 )             { mark/unmark sample as unusable}
        elseif submenu = 2 then
          correct();
        elseif submenu = 3 then
          view( 0 )             { view data }
        elseif submenu = 4 then
          backup();             { back-up data }
        elseif submenu = 5 then
          ExternQC();           { DBS samples for external quality control }
        elseif submenu = 6 then
          HIVtoDHS()            { export data to merge with DHS to calculate weights }
        endif;
      else
        {+US}
        errmsg( "015: You do not have access to these menu options since you are not an Administrator" );
        {US+}
        {{SP}
        errmsg( "015: Usted no tiene acceso a este menu por cuanto usted no es Administrator" );
        {SP}}
        {{FR}
        errmsg( "015: Vous n'avez pas accès à ces options du menu comme vous n'êtes pas l'administrateur" );
        {FR}}
      endif;
    endif;
  {===> END MODIFY }
  enddo;

PROC ID
preproc
  reenter ECLUSTER;

PROC UOPID
preproc
  getop();
  wopid = opid; { stores original user }
  noinput;

PROC UPCHECK
  if $ <> UPASS then
    {+US}
    errmsg( "020: Password incorrect for user %s %s, please re-enter", strip(UNAME1), strip(UNAME2) );
    {US+}
    {{SP}
    errmsg( "020: Contrase¤a de %s %s incorrecta, por favor ingresela de nuevo", strip(UNAME1), strip(UNAME2) );
    {SP}}
    {{FR}
    errmsg( "020: Mot de passe incorrecte pour l'utilisateur %s %s, veuillez re-essayer", strip(UNAME1), strip(UNAME2) );
    {FR}}
    reenter;
  endif;

PROC LEVEL1_FORM
postproc
  { set testing stage and final status variables before exiting }
  if determ_flag in 1,2 then     { wb }
    { !!! you may have to modify the positives in the next "if" if they are further tested by pepti lav }
    if LDETERM1 in 0:7,9 then      { positive, negative, indeterminate, testing finished }
      LSTAGE = 9;                { set stage to finished }
      LFINRES = LDETERM1;        { copy final result }
    endif;
  elseif determ_flag in 3,4 then { pepti lav }
    if LDETERM2 in 1:7,9 then
      LSTAGE = 9;
      LFINRES = LDETERM2;
    endif;
  endif;
  LSTAGET = getlabel(LSTAGE,LSTAGE);

PROC LDETERM1
{ Western Blot }
{ determ_flag values used to control entry in this form: }
{    1 : Add WB (LDETERM1)                               }
{    2 : Edit WB (LDTERM1)                               }
{    3 : Add Peptilav (LDTERM2)                          }
{    4 : Edit Peptilav (LDTERM2)                         }
preproc
  if determ_flag in 3,4 then   { if Pepti Lav, assign 9 and skip to LDETERM 2 }
    if !visualvalue($) then
      $ = 97;
      noinput;
    endif;
    advance to LDETERM2;
  endif;

postproc
  if determ_flag in 1,2 then
    if $ = 97 then
      reenter;
    {+US}
    elseif !ynprompt( concat( "You have entered a result of ", getlabel($,$), ", is this correct ?" ) ) then
    {US+}
    {{SP}
    elseif !ynprompt( concat( "Usted ha ingresado un resultado ", getlabel($,$), ", es correcto ?" ) ) then
    {SP}}
    {{FR}
    elseif !ynprompt( concat( "Vous avez entré un résultat ", getlabel($,$), ", est-ce correct ?" ) ) then
    {FR}}
      reenter;
    endif;
  endif;

PROC LDETERM2
{ Pepti Lav }
preproc
  if determ_flag in 1,2 then
    if !visualvalue($) then   { if Western Bolt, assign 9 and return }
      $ = 9;
    endif;
    noinput;
  endif;

postproc
  if determ_flag in 3,4 then
    if $ = 9 then
      reenter;
    {+US}
    elseif !ynprompt( concat( "You have entered a result of ", getlabel($,$), ", is this correct ?" ) ) then
    {US+}
    {{SP}
    elseif !ynprompt( concat( "Usted ha ingresado un resultado ", getlabel($,$), ", es correcto ?" ) ) then
    {SP}}
    {{FR}
    elseif !ynprompt( concat( "Vous avez entré un résultat ", getlabel($,$), ", est-ce correct ?" ) ) then
    {FR}}
      reenter;
    endif;
  endif;
