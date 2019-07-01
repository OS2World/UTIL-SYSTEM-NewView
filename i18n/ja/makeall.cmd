/* ------------------------------------------------------------------------- */
/* NewView - a new OS/2 Help Viewer                                          */
/*                                                                           */
/* Copyright 2003-2006 Aaron Lawrence (aaronl at consultant dot com)         */
/* Copyright 2006-2017 Ronald Brill (rbri at rbri.org)                       */
/*                                                                           */
/* This software is released under the Gnu Public License                    */
/*                                                                           */
/*                                                                           */
/* Compiles the help files and copys the lanugage files to the build         */
/* directory.                                                                */
/* ------------------------------------------------------------------------- */
Parse Arg nv_build_dir_i18n

nv_build_dir = Value('NV_BUILD',, 'OS2ENVIRONMENT')
If nv_build_dir == '' Then Do
    Say '%NV_BUILD% not defined in environment!'
    Return 1
End
If nv_build_dir_i18n == '' Then
    nv_build_dir_i18n = nv_build_dir
nv_rc = SysMkDir( nv_build_dir_i18n )

/* copy the lang file */
'copy newview_ja.lng' nv_build_dir_i18n

/* compile the ipf file */
/* 'ipfc NewView_ja.ipf' nv_build_dir_i18n||'\NewView_ja.hlp -D:081 -C:932 -L:JPN' */
'copy newview_ja.hlp' nv_build_dir_i18n

Return rc

