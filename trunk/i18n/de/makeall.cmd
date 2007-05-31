/* Rexx */

/* ------------------------------------------------------------------------- */
/* NewView - a new OS/2 Help Viewer                                          */
/*                                                                           */
/* Copyright 2003-2006 Aaron Lawrence (aaronl at consultant dot com)         */
/* Copyright 2006 Ronald Brill (rbri at rbri.org)                             */
/*                                                                           */
/* This software is released under the Gnu Public License                    */
/*                                                                           */
/*                                                                           */
/* Compiles the help files and copys the lanugage files to the build         */
/* directory.                                                                */
/* ------------------------------------------------------------------------- */



nv_i18n_dir = directory()

nv_build_dir = value('NV_BUILD', , 'OS2ENVIRONMENT')
nv_rc = SysMkDir(nv_build_dir)

/* copy changes.txt */
/* TODO copy Changes.txt nv_build_dir */


/* compile the ipf file */
ipfc NewView_de.ipf nv_build_dir||'\NewView_de.hlp'

/* copy the lang file */
copy 'newview_de.lng' nv_build_dir

