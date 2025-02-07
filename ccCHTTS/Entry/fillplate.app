{ Produces plate data file from sample data file containing the  }
{ samples that are to be loaded into a new plate                 }
{ Names the plate with an id input via sysparm                   }
PROC GLOBAL

  set explicit;

  array alpha(5) wplate (8,12);
  alpha(7) plateid;
  numeric ncases, rows, cols, stage, xtype, idx, i;


PROC TEMP_FF
preproc

  { !! MODIFY HERE IF TRAY ID DIFFERENT LENGTH FROM STANDARD !! }
  plateid = sysparm()[1:7];

  {===> END MODIFY !!}

  { !! MODIFY HERE TO FIT COUNTRY SPECIFIC TEST PROTOCOL !! }
  xtype = pos( plateid[3:1], "VE" );
  stage = tonumber( sysparm()[4:1] );
  box stage : xtype => xtype;
          1 : 1     => 1;
          2 : 2     => 2;
          3 : 1     => 3;
          3 : 2     => 4;
            :       => 9;
  endbox;
  errmsg("xtype =%d, stage=%d", xtype, stage);
  {===> END MODIFY !!}
  { get labels from sysparm and put into temp array }
  rows = 1;
  cols = 1;
  do varying i = 8 until i > length( strip(sysparm()) ) by 5
    if length( strip(sysparm()[i:5]) ) then  { only copy non-blank entries }
      wplate(rows,cols) = sysparm()[i:5];
      rows = rows + 1;
    endif;
    if rows > 8 then
      rows = 1;
      cols = cols + 1;
    endif;
  enddo;
  { Check plate with this id does not already exist }
  if loadcase( PLATE_DICT, plateid ) then
    {+US} errmsg( "Plate with id %s already exists, please check", plateid ); {US+}
    {{SP} errmsg( "Bandeja %s ya existe, por favor revise", plateid ); {SP}}
    {{FR} errmsg( "La plaque %s existe déjà, veuillez vérifier", plateid ); {FR}}
  else
    {Initialize plate entries with kit control and qc labels !!}
    {!! Adjust if more than 16 controls and qc labels used !!}
    clear( PLATE_DICT );
    if cols > 1 | cols = 1 & rows >= 1 then ID_A(1) = wplate(1,1); BAR_A(1) = ID_A(1) endif;
    if cols > 1 | cols = 1 & rows >= 2 then ID_B(1) = wplate(2,1); BAR_B(1) = ID_B(1) endif;
    if cols > 1 | cols = 1 & rows >= 3 then ID_C(1) = wplate(3,1); BAR_C(1) = ID_C(1) endif;
    if cols > 1 | cols = 1 & rows >= 4 then ID_D(1) = wplate(4,1); BAR_D(1) = ID_D(1) endif;
    if cols > 1 | cols = 1 & rows >= 5 then ID_E(1) = wplate(5,1); BAR_E(1) = ID_E(1) endif;
    if cols > 1 | cols = 1 & rows >= 6 then ID_F(1) = wplate(6,1); BAR_F(1) = ID_F(1) endif;
    if cols > 1 | cols = 1 & rows >= 7 then ID_G(1) = wplate(7,1); BAR_G(1) = ID_G(1) endif;
    if cols > 1 | cols = 1 & rows >= 8 then ID_H(1) = wplate(8,1); BAR_H(1) = ID_H(1) endif;
    if cols > 2 | cols = 2 & rows >= 1 then ID_A(2) = wplate(1,2); BAR_A(2) = ID_A(2) endif;
    if cols > 2 | cols = 2 & rows >= 2 then ID_B(2) = wplate(2,2); BAR_B(2) = ID_B(2) endif;
    if cols > 2 | cols = 2 & rows >= 3 then ID_C(2) = wplate(3,2); BAR_C(2) = ID_C(2) endif;
    if cols > 2 | cols = 2 & rows >= 4 then ID_D(2) = wplate(4,2); BAR_D(2) = ID_D(2) endif;
    if cols > 2 | cols = 2 & rows >= 5 then ID_E(2) = wplate(5,2); BAR_E(2) = ID_E(2) endif;
    if cols > 2 | cols = 2 & rows >= 6 then ID_F(2) = wplate(6,2); BAR_F(2) = ID_F(2) endif;
    if cols > 2 | cols = 2 & rows >= 7 then ID_G(2) = wplate(7,2); BAR_G(2) = ID_G(2) endif;
    if cols > 2 | cols = 2 & rows >= 8 then ID_H(2) = wplate(8,2); BAR_H(2) = ID_H(2) endif;
  endif;

postproc
  PLATE_ID = plateid;
  TOPCODE = plateid[1:2];
  TESTTYPE = xtype;
  TSTAGE = stage;
  TCDATED = sysdate("DD");
  TCDATEM = sysdate("MM");
  TCDATEY = sysdate("YYYY");
  writecase( PLATE_DICT, PLATE_ID );
  {+US} errmsg("Samples written to plate %s", plateid ); {US+}
  {{SP} errmsg("Muestras sleccionads para bandeja %s", plateid ); {SP}}
  {{FR} errmsg("Echantillons enregsitrés dans la plaque %s", plateid ); {FR}}

  close(PLATE_DICT);

PROC TEMP_QUEST
preproc
  if loadcase( TESTLOG_DICT, TEMP_ID ) then
    LSTATUS = 1;  { test tray created }
    do varying idx = 1 until idx > totocc(LTEST)
      if LPLATE(idx) = PLATE_ID then
        break;
      endif;
    enddo;
    LTYPE(idx) = xtype;
    LTYPET(idx) = getlabel(LTYPE,LTYPE(idx));
    LCDATED(idx) = sysdate("DD");
    LCDATEM(idx) = sysdate("MM");
    LCDATEY(idx) = sysdate("YYYY");
    LPLATE(idx) = plateid;
    LCOL(idx) = cols;
    LROW(idx) = rows;

    if cols <= 12 then
      if rows = 1 then ID_A(cols) = edit("99999", LABID);  BAR_A(cols) = LBAR; endif;
      if rows = 2 then ID_B(cols) = edit("99999", LABID);  BAR_B(cols) = LBAR; endif;
      if rows = 3 then ID_C(cols) = edit("99999", LABID);  BAR_C(cols) = LBAR; endif;
      if rows = 4 then ID_D(cols) = edit("99999", LABID);  BAR_D(cols) = LBAR; endif;
      if rows = 5 then ID_E(cols) = edit("99999", LABID);  BAR_E(cols) = LBAR; endif;
      if rows = 6 then ID_F(cols) = edit("99999", LABID);  BAR_F(cols) = LBAR; endif;
      if rows = 7 then ID_G(cols) = edit("99999", LABID);  BAR_G(cols) = LBAR; endif;
      if rows = 8 then ID_H(cols) = edit("99999", LABID);  BAR_H(cols) = LBAR; endif;

      rows = rows + 1;
      ncases = ncases + 1;
      if rows > 8 then
        rows = 1;
        cols = cols + 1;
      endif;
    endif;
    writecase( TESTLOG_DICT, TEMP_ID );
  else
    {+US} errmsg( "SYSTEM ERROR in Case %s from temp file not found in samples file", TEMP_ID ); {US+}
    {{SP} errmsg( "ERROR DEL SISTEMA en Caso %s en archivo temporal no se encontro en archivo de muestras", TEMP_ID ); {SP}}
    {{FR} errmsg( "ERREUR SYSTEME dans le cas %s du fichier temporaire non trouvé dans le fichier des échantillons", TEMP_ID ); {FR}}
  endif;
