/* ------------------------------------------------------------------------- */
/* NewView - a new OS/2 Help Viewer                                          */
/*                                                                           */
/* Copyright 2003-2006 Aaron Lawrence (aaronl at consultant dot com)         */
/* Copyright 2006-2017 Ronald Brill (rbri at rbri.org)                       */
/*                                                                           */
/* This software is released under the Gnu Public License                    */
/*                                                                           */
/*                                                                           */
/* 1.) Create all needed output directories                                  */
/* 2.) Build all the resource files for our customized sibyl                 */
/* 3.) Compile                                                               */
/* ------------------------------------------------------------------------- */
Signal On NoValue
Signal On Error
Call RxFuncAdd 'SysLoadFuncs', 'REXXUTIL', 'SysLoadFuncs'
Call SysLoadFuncs

Call SetLocal
nv_rc = SetUpPaths()
If nv_rc <> 0 Then Exit rc

nv_build_dir = Value('NV_BUILD',,'OS2ENVIRONMENT')
nv_rc = SysMkDir( nv_build_dir )


/* Project-specific section follows */

nv_build_dir_sibyl = nv_build_dir||'\sibyl'
nv_rc = SysMkDir(nv_build_dir_sibyl)

nv_build_dir_sibyl_lib = nv_build_dir_sibyl||'\lib'
nv_rc = SysMkDir(nv_build_dir_sibyl_lib)

'cd spcc'
'rcomp20 english.rc ' nv_build_dir_sibyl_lib
'rcomp20 buttons.rc ' nv_build_dir_sibyl_lib
'rcomp20 cursors.rc ' nv_build_dir_sibyl_lib
'rcomp20 dbctrls.rc ' nv_build_dir_sibyl_lib
'rcomp20 mmedia.rc ' nv_build_dir_sibyl_lib
'rcomp20 tabctrls.rc ' nv_build_dir_sibyl_lib
'cd ..'

'cd addon'
'rcomp20 CHECKLB.RC ' nv_build_dir_sibyl_lib
'rcomp20 COOLBAR.RC ' nv_build_dir_sibyl_lib
'rcomp20 DIROUTLN.RC ' nv_build_dir_sibyl_lib
'rcomp20 EDITLIST.RC ' nv_build_dir_sibyl_lib
'rcomp20 GLYPHBTN.RC ' nv_build_dir_sibyl_lib
'rcomp20 LED.RC ' nv_build_dir_sibyl_lib
'rcomp20 SEVEN.RC ' nv_build_dir_sibyl_lib
'rcomp20 SPIN.RC ' nv_build_dir_sibyl_lib
'cd ..'

/*
  Libdir is set to appexp so that Appexp.scu is found. Strange
    -t1  PM target
    -L   local symbols debug
    -D   Debug info
    -O0  Disable optimisation
    -$i+ Enable I/O exceptions
    -$W6 Disable warning "Parameter is declared but never used"
*/
'spc20.exe makeall.pas' nv_build_dir_sibyl_lib 'appexp rtl;spcc;addon;appexp; -B -t1 -L -D -O0 -$i+ -$W6-'

Call EndLocal
Exit 0


/* -------------------------------------------------------------------------- *
 * SetUpPaths                                                                 *
 * -------------------------------------------------------------------------- */
SetUpPaths: PROCEDURE EXPOSE g.
    g.!env_inc = Stream('..\env.inc', 'C', 'QUERY EXISTS')
    g.!watcom = GetVar('WATCOM')
    If g.!watcom == '' Then Do
        Say '%WATCOM% environment variable is not set.'
        Say 'Please set %WATCOM% to the location of your OpenWatcom installation.'
        Return 1
    End
    g.!nv_dev = GetVar('NV_DEV')
    If g.!nv_dev == '' Then Do
        Say '%NV_DEV% environment variable is not set.'
        Say 'Please set %NV_DEV% to the top-level path of the development tree.'
        Return 1
    End
    '@SET NV_BUILD='g.!nv_dev'\build'
    '@SET PATH=%WATCOM%\BINP;%WATCOM%\BINW;%NV_BUILD%\newview;%NV_BUILD%\newview_stub;%PATH%'
    '@SET INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2;%PMPRINTF_PATH%;'
    '@SET HELP=%WATCOM%\BINP\HELP;%HELP%'
    '@SET BOOKSHELF=%WATCOM%\BINP\HELP;%BOOKSHELF%'
    '@SET BEGINLIBPATH=%WATCOM%\BINP\DLL;%BEGINLIBPATH%;'
Return 0


/* -------------------------------------------------------------------------- *
 * GetVar                                                                     *
 * -------------------------------------------------------------------------- */
GetVar: Procedure Expose g.
    Parse Upper Arg env_var
    val = Value( env_var,, 'OS2ENVIRONMENT')
    If val == '' Then Do
        if g.!env_inc <> '' Then Do
            match.0 = 0
            mstring = 'SET' env_var'='
            Call SysFileSearch mstring, g.!env_inc, 'match.'
            If match.0 > 0 Then Do i = 1 to match.0
                match.i = Translate( Strip( match.i ))
                If Left( match.i, Length( mstring )) == mstring Then Do
                    Parse Var match.i '='val
                    Leave
                End
            End
        End
    End
Return val


/* -------------------------------------------------------------------------- *
 * Condition Handlers                                                         *
 * -------------------------------------------------------------------------- */
NoValue:
    Say
    Call Lineout 'STDERR:', Right( sigl, 6 ) '+++' STRIP( SOURCELINE( sigl ))
    Call Lineout 'STDERR:', Right( sigl, 6 ) '+++ Non-initialized variable.'
    Say
Exit sigl

Error:
    Say
    Call Lineout 'STDERR:', '+++ Error:' Condition('D')
    Say
Exit sigl


