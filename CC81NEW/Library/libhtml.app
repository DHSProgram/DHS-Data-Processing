
  FILE HTMFILE;
  string htm_separator = "|"; // separator character for adding table rows
  array alpha (90) htm_cells_array (50);  // to store cells

  { Global Flag for setting order cells are filled in tables  }
  { 0 = Left to right }
  { 1 = Right to left use eg with Arabic }
  { NOTE that this is not needed if using RTL syntax in the CSS file }
  { reverse order of columns via CSS }
  numeric htm_r2l = 0; 

  function alpha(15) tag(alpha(10)tagstr,htm_slash);
    // helper function for creating tags
    tag= concat("<", "/"[htm_slash:1],strip(tagstr), ">");
  end;

  function htm_raw(alpha(500) htm_string);
    FileWrite(HTMFILE, strip(htm_string));
  end;
  function htm_newline();
    htm_raw("<br>"); // add blank line
  end;

  function htm_init_styles(alpha(80) htmfname, string cssfilename);
    // sets up html file  and headers
    setfile(HTMFILE, strip(htmfname),create);
    open(HTMFILE); 
    FileWrite(HTMFILE,'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 //EN" "http://www.w3.org/TR/html4/strict.dtd">');
    FileWrite(HTMFILE,"<html>");
    FileWrite(HTMFILE,'  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
    FileWrite(HTMFILE,maketext('  <link href="c:\ke81\library\%s.css" rel="stylesheet" type="text/css">', cssfilename));
    FileWrite(HTMFILE,"  <body>");
  end;

  function htm_init(alpha(80) htmfname);
    // sets up html file  and headers
    setfile(HTMFILE, strip(htmfname),create);
    open(HTMFILE); 
    FileWrite(HTMFILE,'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 //EN" "http://www.w3.org/TR/html4/strict.dtd">');
    FileWrite(HTMFILE,"<html>");
    FileWrite(HTMFILE,'  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">');
    FileWrite(HTMFILE,'  <link href="..\library\styles.css" rel="stylesheet" type="text/css">');
    FileWrite(HTMFILE,"  <body>");
  end;
  function htm_hdr(alpha(500) htm_hdrstr, headertype);
    // writes html header
    FileWrite(HTMFILE,concat(tag(maketext("h%1d", headertype),0), strip(htm_hdrstr), tag(maketext("h%1d", headertype),1)));
  end;

  function htm_par(alpha(500) htm_string);
    // writes html paragraph 
    FileWrite(HTMFILE,concat(tag("p",0), strip(htm_string), tag("p",1)));
  end;

  function htm_tabinit(alpha(25) style);
    // inits table with optional style 
    FileWrite(HTMFILE, concat("<table ", "style=", '"', strip(style), '"', ">"));
  end;
  function htm_tabadd(alpha(500) htm_string, isheader);
    // adds table rows using comma separated string, optional header attribute
    numeric htm_i,htm_beg, htm_len,htm_cells;
    htm_i= 0;
    htm_beg = 1;
    htm_len = -1;
    htm_cells = 0;
    // initialize array for cells
    do htm_i = 0 until htm_i > 50
      htm_cells_array(htm_i) = "";
    enddo;
    FileWrite(HTMFILE, tag("tr",0));
    do htm_i = 0 until htm_i > length(strip(htm_string))
      inc(htm_len);
      if htm_string[htm_i:1] = htm_separator & htm_string[htm_i-1:1] <> "\" then 
        if isheader then
//        FileWrite(HTMFILE, concat(tag("th",0),htm_string[htm_beg:htm_len-1], tag("th",1)));
          htm_cells_array(htm_cells) = concat(tag("th",0),htm_string[htm_beg:htm_len-1], tag("th",1));
        else
//        FileWrite(HTMFILE, concat(tag("td",0),htm_string[htm_beg:htm_len-1], tag("td",1)));
          htm_cells_array(htm_cells) = concat(tag("td",0),htm_string[htm_beg:htm_len-1], tag("td",1));
        endif;
        htm_beg = htm_i+1;
        htm_len=0;
        inc(htm_cells);
      endif;
    enddo;
    if htm_r2l then // table columns filled right to left 
      do htm_i = htm_cells until htm_i < 0 by (-1)
        FileWrite(HTMFILE,strip( htm_cells_array(htm_i) ));
      enddo;
    else 
      do htm_i = 0 until htm_i > htm_cells
        FileWrite(HTMFILE,strip( htm_cells_array(htm_i) ));
      enddo;
    endif;
    FileWrite(HTMFILE, tag("tr",1));
  end;
  // below 2 functions added just to simplify conversion process from write functions 
  ///they just call htm_tabadd with hdr or no hdr parameters
  function htm_tabhdr(alpha(500) htm_string);
    htm_tabadd(htm_string,1);
  end;
  function htm_tabrow(alpha(500) htm_string);
    htm_tabadd(htm_string,0);
  end;

  function htm_tabclose();
    // closes table
    FileWrite(HTMFILE, tag("table",1));
  end;

  function  htm_close();
    // puts doc closing tags, closes file and writes it out
    FileWrite(HTMFILE,"  </body>");
    FileWrite(HTMFILE,"</html>");
    close(HTMFILE);
  end;

  function htm_check_display(alert);
    string chrome_location = concat( pathname(ProgramFiles32), "Google\Chrome\Application\chrome.exe");
    if !FileExist(chrome_location) then
      if alert then
        errmsg("WARNING : Chrome browser not installed. System will attempt to use the default browser to display reports");
      endif;
      htm_check_display = 0;
    else
      htm_check_display = 1;
    endif;
  end;


  function htm_disp( string htmfname);
    string commandstr;
    string doublequotes = '"'; // "
    // displays html file  in browser
      if getos() = 20 then // Android
        commandstr =concat("view:", strip(htmfname));
        execsystem(strip(commandstr));
      else // Windows
      {!! default is to use Chrome browser running in app mode !!}
        if htm_check_display(0) then
          commandstr =concat( pathname(ProgramFiles32), "Google\Chrome\Application\chrome.exe --app=file:///", strip(htmfname) );
          ExecSystem( commandstr, maximized);
        else
      {!! use the installed default browser if Chrome not installed !!}
          commandstr = concat( "cmd /c start /wait Explorer ", strip(htmfname));
          ExecSystem( commandstr, minimized);
        endif;
     endif;
  end;

  function htm_pb();
  // adds page break
    htm_raw('<div class="pagebreak"> </div>');
  end;

