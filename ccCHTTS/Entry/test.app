PROC GLOBAL

  set explicit;

  FILE resfile;                    { file handler with results from spectometer }

  numeric i, j, k, l, xerr, pc1, pc2, pc3, pc4, pc5, pc6, ntot, ncx, nc, tmp, manual;
  numeric linearity, splits, startpos, char, endpos, wcut;
  numeric maxrows, maxcols, maxpos, cols, score, sumpos, arb, isblanked;
  numeric nodrop, odr3, odr4, odr5, wblank1, wblank2, w3a1, w3a2, w3c1, w3c2;
  numeric controls_ok, start, xcol, apc1, time, endline, number;
  numeric x, y, totwell, begcol, begrow;
  numeric wrows, wcols, ncases, IsOk, NoErr, greyarea, pctgrey;
  numeric blkwell = 0;

  array plate_mes(8,12);
  array alpha(8) split_array(100);
  array alpha(5) plate_Bar(8,12);         { array to store all bar codes for aplate }
  array w3b(3);                           { positive control 1 for anilab }

  alpha(2500) note;
  alpha(8)    rowchar;                    { keeps row names of exported trays }
  alpha(500)  textline;                   { Used to read text }
  alpha(250)  xlsFile;
  alpha(250)  txtFile, tempFile, winroot;
  alpha(32)   fname1, fname2;

  { find the first valid sample to establish the column/row where     }
  { the actual samples begin.  Just check the sequence letter, digit, }
  { letter, digit, letter.  Hopefully it is enough                    }
  function findsample( alpha(5) bcode )
    tmp = 1;
    do k = 1 while k <= 3 & tmp by 2
      x = pos( bcode[k:1], "ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
      y = pos( bcode[k+1:1], "0123456789" );
      if !x | !y then
        tmp = 0;
      endif;
    enddo;
    { check last character }
    if tmp & !pos( bcode[5:1], "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ) then
      tmp = 0;
    endif;
    findsample = tmp;
  end;

  function split( alpha(256) alphastr, alpha(1) splitstr )
  {  errmsg( "splitting" );}
    do splits = 1 until splits > 100 split_array(splits) = "" enddo;
  { splits string into pieces, puts into split_array
    split is made using whatever character is put into splitstr }
    splits = 1;
    startpos = 1;
    char = startpos;
  {  errmsg( "splits=%d, char=%d, length=%d", splits, char, length(alphastr) );}
    do splits = 1 until (splits > 100 | char > length(alphastr));
      char = startpos;
      while char <= length(alphastr) & alphastr[char:1] = splitstr do   { find next non split str }
        char = char+1;
      enddo;
      startpos = char;
      while char <= length(alphastr) & alphastr[char:1] <> splitstr do   { find next non split str }
        char = char+1;
      enddo;
      endpos = char-1;
      split_array(splits) = alphastr[startpos:(endpos-startpos+1)];
  {   errmsg( "splits=%d, startpos=%d, endpos=%d, char=%d, split_array=%s",
             splits, startpos, endpos, char, split_array(splits) );}
      startpos = endpos+1;
    enddo;
  end;

  { puts each OD (up to macol) from array plate_mes into data file }
  function putmes( maxcol )
    do varying i = 1 until i > maxcol
      MES_A(i) = plate_mes(1,i) ;
      MES_B(i) = plate_mes(2,i) ;
      MES_C(i) = plate_mes(3,i) ;
      MES_D(i) = plate_mes(4,i) ;
      MES_E(i) = plate_mes(5,i) ;
      MES_F(i) = plate_mes(6,i) ;
      MES_G(i) = plate_mes(7,i) ;
      MES_H(i) = plate_mes(8,i) ;
    enddo;
    putmes = 0;
  end;

  { puts each OD of a plate (upto maxcol) into array plate_mes }
  function getmes( maxcol )
    do varying i = 1 until i > maxcol
      plate_mes(1,i) = MES_A(i);
      plate_mes(2,i) = MES_B(i);
      plate_mes(3,i) = MES_C(i);
      plate_mes(4,i) = MES_D(i);
      plate_mes(5,i) = MES_E(i);
      plate_mes(6,i) = MES_F(i);
      plate_mes(7,i) = MES_G(i);
      plate_mes(8,i) = MES_H(i);
    enddo;
  end;

  { puts each bar code of a plate (upto maxcol) into array plate_bar }
  function getbar( maxcol )
    do varying i = 1 until i > maxcol
      plate_Bar(1,i) = BAR_A(i);
      plate_Bar(2,i) = BAR_B(i);
      plate_Bar(3,i) = BAR_C(i);
      plate_Bar(4,i) = BAR_D(i);
      plate_Bar(5,i) = BAR_E(i);
      plate_Bar(6,i) = BAR_F(i);
      plate_Bar(7,i) = BAR_G(i);
      plate_Bar(8,i) = BAR_H(i);
    enddo;
  end;

  { check if all 5 caracters in bar code for a well are XXXXX }
  function xinwell( yrow, ycol )
    x = 1;
    do l = 1 while l <= 5
      if plate_Bar(yrow,ycol)[l:1] <> "X" then
        x = 0;
      endif;
    enddo;
    xinwell = x;
  end;

  { check if all 5 caracters in bar code for a well are BLANKS }
  function blkinwell( yrow, ycol )
    x = 0;
    do l = 1 while l <= 5
      if plate_Bar(yrow,ycol)[l:1] = " " then
        x = x + 1;
      endif;
    enddo;
    blkinwell = ( x = 5);
  end;

  { puts whether OD negative/positive in a plate (upto maxcol) into array plate_mes }
  function getres( maxcol );
    do varying i=1 until i > maxcol
      plate_mes(1,i) =( MES_A(i) >= wcut );
      plate_mes(2,i) =( MES_B(i) >= wcut );
      plate_mes(3,i) =( MES_C(i) >= wcut );
      plate_mes(4,i) =( MES_D(i) >= wcut );
      plate_mes(5,i) =( MES_E(i) >= wcut );
      plate_mes(6,i) =( MES_F(i) >= wcut );
      plate_mes(7,i) =( MES_G(i) >= wcut );
      plate_mes(8,i) =( MES_H(i) >= wcut );
    enddo;
  end;

  { identifies clustering by calculating # positive cells adjacent to each positive cell }
  function clustering( climit );
    TCLUST = "";
    getres( totocc(MES000) );
    do varying i = 1 until i > maxrows
      do varying j = 1 until j > maxcols
        score = plate_mes(i  ,j  )+
                plate_mes(i-1,j  )+
                plate_mes(i+1,j  )+
                plate_mes(i  ,j+1)+
                plate_mes(i  ,j-1)+
                plate_mes(i-1,j-1)+
                plate_mes(i-1,j+1)+
                plate_mes(i+1,j-1)+
                plate_mes(i+1,j+1);
        score = score / 9 * 100;
        if score > climit then
          TCLUST = concat( strip(TCLUST), rowchar[i:1], edit("99",j), ", " );
        endif;
      enddo;
    enddo;
    if length( strip(TCLUST) ) > 0 then
      errmsg( 47, strip(TCLUST) );
      clustering = 1;
    endif;
  end;

  { calculate the percentage of positive samples, checks whether row or    }
  { column has all positives and whether > 50% of the samples are positive }
  function checkplate( xcut, xstage )
    xerr = 0;
    { identify the column and row where the actual samples begin }
    begcol = 0;
    getbar( 12 );      { populate 2 dimensional plate_bar array with tray bar codes }
    do varying j = 1 while j <= maxcols & !begcol
      do varying i = 1 while i <= maxrows & !begcol
        if findsample( plate_Bar(i,j) ) then
          begcol = j;
          begrow = i;
        endif;
      enddo;
    enddo;
    { count total and positive samples }
    getmes( curocc(MES000) );
    sumpos  = 0;
    totwell = 0;
    do varying j = begcol until j > maxcols
      if j <> begcol then begrow = 1 endif;     { only the first column start at a row <> of 1 }
      do varying i = begrow until i > maxrows
        totwell = totwell + 1;
        if plate_mes(i,j) >= xcut then
          sumpos = sumpos + 1;
        endif;
      enddo;
    enddo;

    if xstage = 1 & clustering( 80 ) then
      xerr = 3;
    endif;

    TPOS = int( sumpos/totwell*100 + 0.5 );      { percentage of positive samples }
    if xstage = 1 & TPOS >  maxpos then
      errmsg( 045, TPOS );
      xerr = 2;
    endif;
    checkplate = xerr;
  end;

  { !! calculate cutting point for VIRONOSTIKA }
  function vironostika();
    getmes( 2 );               { columns because double controls }
    pc1  = plate_mes(4,1);
    pc2  = plate_mes(5,1);
    pc3  = plate_mes(6,1);
    pc4  = plate_mes(7,1);
    pc5  = plate_mes(8,1);
    pc6  = plate_mes(1,2);
    ntot = 0;
    ncx  = 0;
    nc   = 0;

    { calculate mean of negative controls, excluding values > .250 }
    do varying i = 1 until i > 3
      if plate_mes(i,1) >= 0.250 then
         plate_mes(i,1) = default;
         errmsg( 050, i );
      else
         nc = nc + plate_mes(i,1);
         ntot = ntot + 1;
      endif;
    enddo;
    ncx = nc / ntot;

    { drop values < 60% and > 140% of ncx, & recalculate ncx iteratively until no more out of range values  }
    nodrop = 0;
    do until nodrop
      nc     = 0;
      ntot   = 0;
      nodrop = 1;
      do varying i = 1 until i > 3
        if plate_mes(i,1) <> default then
          if plate_mes(i,1) < 0.6 * ncx | plate_mes(i,1) > 1.4 * ncx then
            errmsg( 055, i, 1, ncx );
            plate_mes(i,1) = default;
            nodrop = 0;
          else
            nc   = nc + plate_mes(i,1);
            ntot = ntot + 1;
          endif;
        endif;
        { errmsg( "i=%d,j=%d, nc=%2.4f, ncx=%2.4f, ntot=%d, nodrop=%d", i, j, nc, ncx, ntot, nodrop ); }
      enddo;
    enddo;
    ncx = nc / ntot;
    if ntot < 2 then   { changed from 2 to 4 because double controls }
      errmsg( 057 );
      wcut = 0;
    elseif pc1 - ncx < 0.6 & pc2 - ncx < 0.6 |
           pc3 - ncx < 0.6 & pc4 - ncx < 0.6 |
           pc5 - ncx < 0.4 & pc6 - ncx < 0.4 then
      errmsg( 065, pc1, ncx, pc2, ncx, pc3, ncx, pc4, ncx, pc5, ncx, pc6, ncx );		{ SK }
      wcut = 0;
    else
      wcut = ncx + 0.1;
  {   errmsg( "wcut=%f, ncx=%f", wcut, ncx );}
    endif;
    vironostika = wcut;
  end;

  { !! Calculation and interpretation of GENSCREEN results }
  function genscreen()
    { !! This test has 5 kit's controls                        }
    { !! Well A1, B1, C1  :  negative control                  }
    { !! Well A1,B1,C1    :  cut-off control serum             }
    { !! Well D1,E1,F1,G1 :  positive control in duplicate     }
    
    numeric GSntot = 0, GSncx  = 0, GSnc   = 0;
        
    wcut = 0;
    getmes(1);
    { step 1}
    { calculate the mean absorbance value of negative controls, excluding values >= 0.170 }
    do i = 1 while i <= 3
      if plate_mes(i,1) >= 0.170 then
         errmsg( 131, i );
         plate_mes(i,1) = default;
      else
         GSnc = GSnc + plate_mes(i,1);
         GSntot = GSntot + 1;
      endif;
    enddo;
    GSncx = GSnc / GSntot;  

    { Step 2 and 3: Assay validation and calculating cutoff value }
    { Assay Validation: }
    
    { Two or more absorbance values of the negative control must be valid }        
    if GSntot < 2 then   
      errmsg( 130, plate_mes(1,1), plate_mes(2,1), plate_mes(3,1) );
    { The mean absorbance of cut-off control serum should be < 0.150 }
    elseif GSncx >= 0.150 then
      errmsg( 133, GSncx );
    { The absorbance value of PC-Ab is >0.9. At least one value must be >0.9  }      
    elseif !(plate_mes(4,1) > 0.9 | plate_mes(5,1) > 0.9) then
      errmsg (132,  plate_mes(4,1),  plate_mes(5,1));
    { The absorbance value of PC-Ab is >0.9. At least one value must be >0.9  }  
    elseif !(plate_mes(6,1) > 0.9 | plate_mes(7,1) > 0.9) then
      errmsg (134, plate_mes(6,1),  plate_mes(7,1));
    else
      { Calculating cut-off }
      { Mean of the Negative control (NCx) +0.200 }
      wcut = GSncx + 0.200;
    endif;
    genscreen= wcut;
  end;

  { !! calculate cutting point for ANILAB }
  function anilab()
    wcut = 0;
    getmes(1);
    { set up control variables }
    wblank1 = plate_mes(1,1);
    wblank2 = plate_mes(2,1);
    w3a1    = plate_mes(3,1);
    w3a2    = plate_mes(4,1);
    w3b(1)  = plate_mes(7,1);
    w3b(2)  = plate_mes(8,1);
    w3c1    = plate_mes(5,1);
    w3c2    = plate_mes(6,1);

    { check values are within accepted ranges }
    if wblank1 > 0.10 | wblank2 > 0.10 then  { check blanks }
      errmsg( 081, "Blank controls", wblank1, wblank2, Arb, w3a1, w3a2, 0.15-Arb, w3b(1), w3b(2), 1.99-Arb, w3c1, w3c2, 0.50-Arb );
    endif;
    Arb = ( wblank1 + wblank2 ) / 2; { mean absorbance of reagent blank - needed to adjust accepted ranges below}

    { subtract mean absorbancy of blanks from OD's of kit controls }
    if !isblanked then
      w3a1   = w3a1   - Arb;
      w3a2   = w3a2   - Arb;
      w3c1   = w3c1   - Arb;
      w3c2   = w3c2   - Arb;
      w3b(1) = w3b(1) - Arb;
      w3b(2) = w3b(2) - Arb;
    endif;

    if w3a1 > 0.15 | w3a2 > 0.15  then  { check negative control }
      errmsg( 081, "Negative control", wblank1, wblank2, Arb, w3a1, w3a2, 0.15, w3b(1), w3b(2), 1.99, w3c1, w3c2, 0.50 );
    elseif w3c1 <= 0.50  | w3c2  <= 0.50  then  { positive control 2}
      errmsg( 081, "Positive controls 2", wblank1, wblank2, Arb, w3a1, w3a2, 0.15, w3b(1), w3b(2), 1.99, w3c1, w3c2, 0.50 );
    else { positive control 1, 2 out of 3 controls must pass test }
      controls_ok = 0;
      Apc1 = 0; { mean absorbance }
      do varying i = 1 until i > 2
        if w3b(i)  >= 0.70  & w3b(i)  < 2.00  then
          controls_ok = controls_ok + 1;
          Apc1 = Apc1 + w3b(i); { calculate mean absorbance }
        endif;
      enddo;
      Apc1 = Apc1 / controls_ok;
      if controls_ok < 2 then
        errmsg( 081, "Positive controls 1", wblank1, wblank2, Arb, w3a1, w3a2, 0.15, w3b(1), w3b(2), 1.99, w3c1, w3c2, 0.50 );
      endif;
    endif;
    if isblanked then
      wcut = 0.3 * Apc1; {!! if microplate reader is blanked against reagent blank !!}
    else
     wcut = (0.3 * (Apc1 )) + Arb ;
     errmsg( "Cutoff=%f (adjusted %f), APC1=%f, Mean absorbancy of blanks=%f", wcut, wcut-arb, apc1, arb );
     anilab = wcut;
     TCUTRAW = wcut - Arb; { save unadjusted cutoff for presentation purposes - Anilab only }
    endif;
  end;

  { !! calculate cutting point for MUREX }
  function murex()
    getmes( 2 );               { columns because double controls }
    pc1  = plate_mes(4,1);
    pc2  = plate_mes(5,1);
    pc3  = plate_mes(6,1);
    pc4  = plate_mes(7,1);
    pc5  = plate_mes(8,1);
    pc6  = plate_mes(1,2);
    ntot = 0; ncx = 0;
    nc = 0;

    { calculate mean of negative controls }
    do varying i = 1 until i > 3
       nc = nc+plate_mes(i,1);
       ntot = ntot+1;
  {    errmsg( "i=%d,nc=%2.4f, ncx=%2.4f, ntot=%d, nodrop=%d",i, nc, ncx, ntot, nodrop );}
    enddo;
    ncx = nc/ntot;

    { drop values > 0.15 above ncx, & recalculate ncx iteratively until no more out of range values  }
    nodrop = 0;
    do until nodrop
      nc = 0;
      ntot = 0;
      nodrop = 1;
      do varying i = 1 until i > 3
        if plate_mes(i,1) <> default then
          if plate_mes(i,1) - ncx > 0.15 then
            errmsg( "WARNING : Negative sample %d > 0.15 above NCx %f", i, ncx );
            plate_mes(i,1) = default;
            nodrop = 0;
          else
            nc = nc+plate_mes(i,1);
            ntot = ntot+1;
          endif;
        endif;
  {        errmsg( "i=%d,nc=%2.4f, ncx=%2.4f, ntot=%d, nodrop=%d", i, nc, ncx, ntot, nodrop );}
      enddo;
      ncx = nc/ntot;
    enddo;
    if ntot < 2 then
      errmsg( "ASSAY INVALID . Only 1 negative control <= 0.15 of the negative mean" );
      wcut = 0;
    elseif pc1 - ncx <= 0.8 & pc2 - ncx <= 0.8 |
           pc3 - ncx <= 0.8 & pc4 - ncx <= 0.8 |
           pc5 - ncx <= 0.8 & pc6 - ncx <= 0.8 |
           ncx >= 0.15 then
      errmsg( "ASSAY INVALID . PC1(both) - NCx <= 0.8 or PC2 - NCx <= 0.8 or PC3 - NCx <= 0.8 or NCx >= 0.15 : PC1=%f-%f PC2=%f-%f, PC3=%f-%f NCx=%f", pc1, pc2, pc3, pc4, pc5, pc6, ncx );
      wcut = 0;
    else
      wcut = ncx + 0.150;
{     errmsg( "Cutoff point calculated as %f, NCX=%f", wcut, ncx ); }
    endif;
    murex = wcut;
  end;

  { !! calculate cutting point for ENZYGNOST }
  function enzygnost()
    numeric nc1, nc2, pcx, pc;
    getmes( 1 );              { normally is 1, in DR was 2 because controls were in two columns }
    nc1  = plate_mes(1,1);
    nc2  = plate_mes(2,1);
    pcx  = 0;
    pc   = 0;
    ntot = 0;

    { calculate mean of negative controls, exluding values not in range 0.010 - 0.150 }
    do varying i = 3 until i > 5         { !! normally is 4 in DR only 2 }
      if !plate_mes(i,1) in 0.500:2.600 then
        plate_mes(i,1) = default;
        errmsg( 051, i, 1 );
      else
        pc = pc + plate_mes(i,1);
        ntot = ntot + 1;
      endif;
    enddo;
    pcx = pc / ntot;

    if ntot < 2 then           { at least one invalid positive control }
      errmsg( 060 );
      wcut = 0;
    elseif !nc1 in (-0.010):0.200 | !nc2 in (-0.010):0.200 then  { both negative controls out of range }
      errmsg( 066, nc1, nc2 );
      wcut = 0;
    else                          { valid assay, calculate cutoff point }
      wcut = pcx * 0.23;
    endif;
    enzygnost = wcut;
  end;

  { !! calculate cutting point for bioelisa }
  function bioelisa()
    numeric p1, p2, p3;
    getmes( 2 );
    ncx  = 0;
    nc   = 0;
    ntot = 0;

    blkwell = plate_mes(8,2);                     { !!! make sure that the BLANK well is in that position }
    if !blkwell in -0.010:0.100 then		         { valid BLANK well OD }
      errmsg( 073 );
      wcut = 0;
    else
      if blkwell < 0 then blkwell = 0 endif;
      { calculate mean of negative controls, exluding values > 0.120 }
      do i = 1 while i <= 3
        if plate_mes(i,1) - blkwell > 0.120 then
          plate_mes(i,1) = default;
          errmsg( 052, i, 1 );
        else
          nc = nc + ( plate_mes(i,1) - blkwell );
          ntot = ntot + 1;
        endif;
      enddo;
      ncx = nc / ntot;

      { average of positive controls }
      p1 = ( plate_mes(4,1) + plate_mes(5,1) ) / 2;	{HIV-1 positive }
      p2 = ( plate_mes(6,1) + plate_mes(7,1) ) / 2;	{HIV-2 positive }
      p3 = ( plate_mes(8,1) + plate_mes(1,2) ) / 2;	{HIV-3 p24      }
      if ntot < 2 then           { at least tow valid negative controls }
        errmsg( 071 );
        wcut = 0;
      elseif p1 - blkwell < 0.9 | p2 - blkwell < 0.9 | p3 - blkwell < 0.9 then
        errmsg( 072, blkwell, p1, p2, p3 );
        wcut = 0;
      else                                      { valid assay, calculate cutoff point }
        wcut = ncx + 0.170;
      endif;
    endif;
    bioelisa  = wcut;
  end;

  { !! MODIFY to suit country protocol !! }
  function cutoff( ttype )
    blkwell = 0;
    if ttype in 1,3 then
      tmp = bioelisa();
    else
      tmp = enzygnost();
    endif;
    cutoff = tmp;
  end;

  { !! for manual data entry only - checks result code entered against result calculated  !! }
  {    can only be used when spectrometer prints results as well as od readings !!           }
  function checkres( xmes, alpha(1) xres )
    if manual & xmes <> 0.0 & (!((pos("+", xres)) <=> (xmes >= wcut))) then
      if xmes >= wcut then
        errmsg( 85, xres );
      else
        errmsg( 90, xres );
      endif;
    endif;
  end;

  { function to be used when copying an pasting into the a CSPro note                        }
  { !! use for test plate sets - need to modify to fit output for spectrometer in country !! }
  function readres_note( alpha(32) fname )
    note = editnote();
    { read in OD values }
    start = pos("A", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_A(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("B", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_B(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("C", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_C(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("D", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_D(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("E", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_E(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("F", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_F(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("G", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_G(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    start = pos("H", note) + 7; xcol = 1;
    do  i = start until xcol > 12 by 6 MES_H(xcol) = tonumber(note[i:5]); xcol = xcol+1 enddo;
    if !length( strip(note) ) then
      tmp = 0;
      errmsg( "Results not in correct format, please try again" );
    else
      tmp = 1;
    endif;
    readres_note = tmp;
  end;

  { function to read a text file from ascent software }
  function readres_ascent( alpha(32) fname )
    fname = concat( "..\plateod\", strip(TCONFIRM), ".txt" );
    SetFile( resfile, fname );
    getbar( 12 );      { populate 2 dimensional plate_bar array with tray bar codes }

    tmp = 1;    { variable tmp is set to true }
    if FileExist( fname ) <> 1 then
      {+US}
      errmsg( "File=(%s) doesn't exist.  Maybe it was created with a wrong name", strip(fname) );
      {US+}
      {{SP}
      errmsg( "Archivo=(%s) no existe.  Quizas fue creado con nombre incorrecto", strip(fname) );
      {SP}}
      {{FR}
      errmsg( "Fichier=(%s) n'existe pas. Peut-être il a été créé avec un nom incorrect", strip(fname) );
      {FR}}
      tmp = 0;
    else
      { discard first three lines: blank line, sheet line, and heading columns line }
      do i = 1 while i <= 3
        if FileRead( resfile, textline ) < 1 then
          tmp = 0;
          break
        endif;
      enddo;
      { use textline as a string var to read the file into }
      do i = 1 while i <= 8
        if FileRead( resfile, textline ) < 1 then
          tmp = 0;
          break
        else
{         errmsg( "Line %d=%s", i, strip(textline) ); }
          {  change commas "," by periods "." }
          do k = 1 while k <= length( strip(textline) )
            if textline[k:1] = "," then textline[k:1] = "." endif;
          enddo;
          {  get rif off letters and blanks at the begining of the record }
          do k = 1 while !pos( textline[k:1], "-.0123456789*" )
          enddo;
          { analizes up to 12 numbers by record }
          endline = 0;
          do j = 1 while j <= 12 & !endline
            start = k;
            if textline[k:1] = "*"  then
              {+US}
              errmsg( "An astersik(*) was found in Row=%d Column=%d, value will be converted to 3.5", i, j );
              {US+}
              {{SP}
              errmsg( "Se encontro un astersico(*) en fila=%d Columna=%d, se le asignara un valor de 3.5", i, j );
              {SP}}
              {{FR}
              errmsg( "Un astersique (*) a été trouvé en ligne=%d Colonne=%d, valeur sera convertie en 3.5", i, j );
              {FR}}
              number = 3.5;
              while pos( textline[k:1], "*" ) do
                k = k + 1;
              enddo;
            else
              while pos( textline[k:1], "-.0123456789" ) do
                k = k + 1;
              enddo;
              number = tonumber( textline[start:k-start] );
            endif;
            { if only XXXXX's or BLANKS in bar code in a well assign 0 zero as the OD value }
            if xinwell(i,j) | blkinwell(i,j) then
              number = 0;
            endif;
            if     i = 1 then MES_A(j) = number
            elseif i = 2 then MES_B(j) = number
            elseif i = 3 then MES_C(j) = number
            elseif i = 4 then MES_D(j) = number
            elseif i = 5 then MES_E(j) = number
            elseif i = 6 then MES_F(j) = number
            elseif i = 7 then MES_G(j) = number
            elseif i = 8 then MES_H(j) = number
            endif;
            while !pos( textline[k:1], "-.0123456789*" ) & !endline do
              if textline[k:1] = rowchar[i:1] then
                endline = 1;
                break;
              endif;
              k = k + 1;
            enddo;
          enddo;
          if j < 12 then
            if     i = 1 & length( strip(ID_A(j)) ) then errmsg( 76, ID_A(j), "A", j )
            elseif i = 2 & length( strip(ID_B(j)) ) then errmsg( 76, ID_B(j), "B", j )
            elseif i = 3 & length( strip(ID_C(j)) ) then errmsg( 76, ID_C(j), "C", j )
            elseif i = 4 & length( strip(ID_D(j)) ) then errmsg( 76, ID_D(j), "D", j )
            elseif i = 5 & length( strip(ID_E(j)) ) then errmsg( 76, ID_E(j), "E", j )
            elseif i = 6 & length( strip(ID_F(j)) ) then errmsg( 76, ID_F(j), "F", j )
            elseif i = 7 & length( strip(ID_G(j)) ) then errmsg( 76, ID_G(j), "G", j )
            elseif i = 8 & length( strip(ID_H(j)) ) then errmsg( 76, ID_H(j), "H", j )
            endif;
          endif;
        endif;
      enddo;
    endif;
    readres_ascent = tmp;
  end;

  { functions comread(), convres() and readres_serial() are used when getting ODs straight   }
  { from the microplate reader via the BioTek machine's serial port                          }
  { !! use for test plate sets - need to modify to fit output for spectrometer in country !! }
  function comread();
    fname1 = "..\comread\DATA.TXT";
    fname2 = concat( "..\PlateOD\", strip(TCONFIRM), ".txt" );
    { runs comread application to capture data from reader }
    { delete previously existing data.txt }
    IsOk = 1;
    FileDelete( fname1 );
    { run comread }
    if execsystem( "..\comread\comread.exe", nofocus, minimized ) then
      accept( "Run assay on plate with microplate reader.  When test is COMPLETED, select 'Read'", "Read test results from spectrometer", "Cancel" );
      { kill comread process }
      execsystem( "c:\windows\system32\wscript.exe ..\comread\killproc.vbs", nofocus ); { kills comread }
      if FileExist( fname1 ) then
        if FileCopy( fname1, fname2 )  then {  save original output file as nameofplate.txt }
          FileDelete( fname1 ); { delete old file }
        else
          errmsg( 106 );
	         IsOk = 0;
        endif;
      else
        errmsg( 106 );
        IsOk = 0;
      endif;
    else
      isok = 0;
    endif;
    comread = IsOk;
  end;

  { convert the string of characters "res" into a number that corresponds to row x and column y in the plate }
  function convres( alpha(4) res, x1, y1 )
    if pos( "****", res ) | tonumber(res) > linearity * 1000 then
      res = edit("9999", linearity*1000);
    elseif !length(strip(res)) then
      res = "0000";
    endif;
    if !tonumber(res) in 1:4000 then
      errmsg( 030, rowchar[x1:1], y1, res );
    endif;
    convres = tonumber(res) / 1000;
    {errmsg("j=%d,inserting %s in row %d col %d", j,res, x1,y1);}
  end;

  { function to read a text file sent by BioTek through the machine's serial port }
  function readres_serial()
    NoErr = 0;
    if comread() then
      NoErr = 1;
      setfile( resfile, fname2 );             { fname2 set in comread() }
      if FileExist( resfile ) then
        wrows = 1;
        while FileRead( resfile, textline ) do
          wcols = 0;
          ncases = 0;
          do varying j = 1 until (j > 100 | ncases > 96);
            if pos( ",", textline[j:1] ) then
              wcols = wcols + 1;
              if wrows <= 8 then
                if wrows = 1 & length( strip(BAR_A(wcols)) ) then MES_A(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 2 & length( strip(BAR_B(wcols)) ) then MES_B(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 3 & length( strip(BAR_C(wcols)) ) then MES_C(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 4 & length( strip(BAR_D(wcols)) ) then MES_D(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 5 & length( strip(BAR_E(wcols)) ) then MES_E(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 6 & length( strip(BAR_F(wcols)) ) then MES_F(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 7 & length( strip(BAR_G(wcols)) ) then MES_G(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                if wrows = 8 & length( strip(BAR_H(wcols)) ) then MES_H(wcols) = convres( textline[j+2:4], wrows, wcols ); endif;
                ncases = ncases + 1;
              endif;
            endif;
          enddo;
          wrows = wrows + 1; { next line from file }
        enddo;
      endif;
    endif;
    readres_serial = NoErr;
   end;

PROC PLATE_FF
preproc
  maxcols = 12;
  maxrows = 8;
  rowchar = "ABCDEFGH";
  linearity = 3.0;              { !! MODIFY based on known linearity ie max readable value of microplate reader !!}
  manual    = tonumber( sysparm()[1:1] );      { determines whether values entered manually or imported by cut and paste }
  pctgrey   = tonumber( sysparm()[2:2] );      { percentage to determine the grey area for negative samples }
  isblanked = 0; {!!  1 if anilab reader blanked, 0 if it is not blanked !!}

PROC PLATE_QUEST
preproc
  TCCX   = 0;
  TNCX   = 0;

  { set maximum % positives for tray to trigger warning }
  { !! MODIFY according to expected HIV prevalence rate in country !!}
  if TESTTYPE <  2 then
    maxpos = 20;
  else
    maxpos = 90;
  endif;

  set attributes(ACTION) assisted on;

  if !manual then
    set attributes( MES_A, MES_B, MES_C, MES_D, MES_E, MES_F, MES_G, MES_H ) protect;
    set attributes( RES_A, RES_B, RES_C, RES_D, RES_E, RES_F, RES_G, RES_H ) protect;
  endif;
  set attributes( ID_A, ID_B, ID_C, ID_D, ID_E, ID_F, ID_G, ID_H ) protect;
  set attributes( BAR_A, BAR_B, BAR_C, BAR_D, BAR_E, BAR_F, BAR_G, BAR_H ) protect;

postproc
  stop(-1);

PROC TCONFIRM
onfocus
  do varying cols = 1  until cols > 12
    MES_A(cols) = 0;
    MES_B(cols) = 0;
    MES_C(cols) = 0;
    MES_D(cols) = 0;
    MES_E(cols) = 0;
    MES_F(cols) = 0;
    MES_G(cols) = 0;
    MES_H(cols) = 0;
  enddo;

postproc
  if $ <> PLATE_ID then
    errmsg( 100, $, PLATE_ID );
    reenter;
  endif;
  if !manual then
    { !!! choose the method to be used to get OD from the microplate reader }
    if !readres_serial() then           { from a text file coming from the BioTek's serial port }
      stop(-1);                         { ????? check this out, seems a better option than reentering }
      errmsg( 106 );
      reenter;
    endif;
    {  ****
    if !readres_note( $ ) then             { when using copy and paste into a CSPro note }
      errmsg( 105 );
      reenter;
    endif;
    if !readres_ascent( $ ) then           { from a text file when using the Ascent software }
      errmsg( 105 );
      reenter;
    endif;
    **** }
  endif;

PROC MES_A
preproc
  if manual & !length(strip(ID_A(curocc()))) then {quit roster if no more ids }
    endgroup;
  else
    if TESTTYPE in 2,4 & manual & curocc() = 2 then
      wcut = cutoff( TESTTYPE );
      {+US}
      errmsg( "Cutoff point is %2.4f", wcut );
      {US+}
      {{SP}
      errmsg( "Punto de corte es %2.4f", wcut );
      {SP}}
      {{FR}
      errmsg( "Valeur seuil est %2.4f", wcut );
      {FR}}

    endif;
  endif;

PROC RES_A
  if manual then
    if curocc() > 1 then
      checkres( MES_A, $ );
    endif;
  endif;

PROC MES_B
preproc
  if manual & !length(strip(ID_B(curocc()))) then { quit roster if no more ids }
    endgroup;
  endif;

PROC RES_B
  if manual then
    if curocc() > 1 then
      checkres( MES_B, $ );
    endif;
  endif;

PROC MES_C
preproc
  if manual & !length(strip(ID_C(curocc()))) then { quit roster if no more ids }
    endgroup;
  endif;

PROC RES_C
  if manual then
    if curocc() > 1 then
      checkres( MES_C, $ );
    endif;
  endif;

PROC MES_D
preproc
  if manual & !length(strip(ID_D(curocc()))) then { quit roster if no more ids }
    endgroup;
  endif;

PROC RES_D
  if manual then
    if curocc() > 1 then
      checkres( MES_D, $ );
    endif;
  endif;

PROC MES_E
preproc
  if manual & !length(strip(ID_E(curocc()))) then { quit roster if no more ids }
    endgroup;
  endif;

postproc
  {!! MODIFY : calculate cutoff at end of control fields to check manual entry, need to change
               if assay has more or less than 5 kit control samples }
  if TESTTYPE in 1,3 & manual & curocc() = 1 then
    wcut = cutoff( TESTTYPE );
    {+US}
    errmsg( "Cutoff point is %2.4f", wcut );
    {US+}
    {{SP}
    errmsg( "Punto de corte es %2.4f", wcut );
    {SP}}
    {{FR}
    errmsg( "Valeur seuil est %2.4f", wcut );
    {FR}}

  endif;

PROC RES_E
  if manual then
    if curocc() > 1 then
      checkres( MES_E, $ );
    endif;
  endif;

PROC MES_F
preproc
  if manual & !length(strip(ID_F(curocc()))) then {quit roster if no more ids }
    endgroup;
  endif;

PROC RES_F
  if manual then
    checkres( MES_F, $ );
  endif;

PROC MES_G
preproc
  if manual & !length(strip(ID_G(curocc()))) then {quit roster if no more ids }
    endgroup;
  endif;

PROC RES_G
  if manual then
    checkres( MES_G, $ );
  endif;

PROC MES_H
preproc
  if manual & !length(strip(ID_H(curocc()))) then {quit roster if no more ids }
    endgroup;
  endif;

PROC RES_H
  if manual then
    checkres( MES_H, $ );
  endif;

PROC TCUTOFF
preproc
  $ = 0;

postproc
  { !! MODIFY to suit country protocol !! }
  $ = cutoff( TESTTYPE );
  if !$ then TFLAG = 1; else TFLAG = 0;endif;

  {!!===> end MODIFY }
  if !special(TCUTOFF) & TCUTOFF then
    { blkwell is different from zero only for BIOELISA, therefore it can be substracted from all other assays }
    do varying i = 1 until i > curocc(MES000)
      if MES_A(i) - blkwell >= TCUTOFF then RES_A(i) = "+" else RES_A(i) = " " endif;
      if MES_B(i) - blkwell >= TCUTOFF then RES_B(i) = "+" else RES_B(i) = " " endif;
      if MES_C(i) - blkwell >= TCUTOFF then RES_C(i) = "+" else RES_C(i) = " " endif;
      if MES_D(i) - blkwell >= TCUTOFF then RES_D(i) = "+" else RES_D(i) = " " endif;
      if MES_E(i) - blkwell >= TCUTOFF then RES_E(i) = "+" else RES_E(i) = " " endif;
      if MES_F(i) - blkwell >= TCUTOFF then RES_F(i) = "+" else RES_F(i) = " " endif;
      if MES_G(i) - blkwell >= TCUTOFF then RES_G(i) = "+" else RES_G(i) = " " endif;
      if MES_H(i) - blkwell >= TCUTOFF then RES_H(i) = "+" else RES_H(i) = " " endif;
    enddo;
{ *** remove this comment if the Grey area concept is used
    if TESTTYPE = 1 then   { check grey area just for the first level of the protocol }
	     greyarea = TCUTOFF - TCUTOFF * pctgrey/100;          { to test cases below pctgrey % of cut off point }
      do varying i = 1 until i > curocc(MES000)
        if ID_A(i) in "0":"9" & MES_A(i) in greyarea:TCUTOFF then RES_A(i) = "G" endif;
        if ID_B(i) in "0":"9" & MES_B(i) in greyarea:TCUTOFF then RES_B(i) = "G" endif;
        if ID_C(i) in "0":"9" & MES_C(i) in greyarea:TCUTOFF then RES_C(i) = "G" endif;
        if ID_D(i) in "0":"9" & MES_D(i) in greyarea:TCUTOFF then RES_D(i) = "G" endif;
        if ID_E(i) in "0":"9" & MES_E(i) in greyarea:TCUTOFF then RES_E(i) = "G" endif;
        if ID_F(i) in "0":"9" & MES_F(i) in greyarea:TCUTOFF then RES_F(i) = "G" endif;
        if ID_G(i) in "0":"9" & MES_G(i) in greyarea:TCUTOFF then RES_G(i) = "G" endif;
        if ID_H(i) in "0":"9" & MES_H(i) in greyarea:TCUTOFF then RES_H(i) = "G" endif;
      enddo;
    endif;
*** }
  endif;

PROC ACTION
preproc
  if !TFLAG {& !manual } then
    TWARN = checkplate( tcutoff, tstage );
    if TWARN = 1 then TFLAG = 1 endif;{ set flag if DBS controls invalid }
  endif;

postproc
  if $ = 2 then
    {+US}
    tmp = accept( "These results will not be saved, quit and enter later?", "Yes", "No" );
    {US+}
    {{SP}
    tmp = accept( "Estos resultados no se salvaran, salir del sistema y entrar mas tarde?", "Si", "No" );
    {SP}}
    {{FR}
    tmp = accept( "Ces résultats ne seront pas sauvegardés, quitter et entrer plus tard ?", "Oui", "Non" );
    {FR}}
    if tmp = 1 then
      stop(1);
    else
      reenter;
    endif;
  endif;

PROC TDATE
preproc
  $ = sysdate( "DDMMYYYY" );
  noinput;

PROC TTIMEH
preproc
  time = systime();
  $ = int( time / 10000 );
  noinput;

PROC TTIMEM
preproc
  time = systime();
  $ = int( time / 100 ) % 100;

postproc
  time = systime();
  $ = int( time / 100 ) % 100;
