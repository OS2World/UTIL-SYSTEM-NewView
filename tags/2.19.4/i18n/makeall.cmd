/* Rexx */

/* ------------------------------------------------------------------------- */
/* NewView - a new OS/2 Help Viewer                                          */
/*                                                                           */
/* Copyright 2003-2006 Aaron Lawrence (aaronl at consultant dot com)         */
/* Copyright 2006-2007 Ronald Brill (rbri at rbri.org)                       */
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
copy Changes.txt nv_build_dir


/* compile the ipf file */
ipfc NewView.ipf nv_build_dir||'\NewView.hlp -D:001 -C:850 -L:ENU'

call SysFileTree nv_i18n_dir||'\', subdirs, 'DO'
do i=1 to subdirs.0
    if  0 = lastpos('images', subdirs.i) then
    do
        if  0 = lastpos('.svn', subdirs.i) then
        do
            cd subdirs.i
            /* call makeall for every dir */
            makeall.cmd
            cd ..
        end
    end
end

