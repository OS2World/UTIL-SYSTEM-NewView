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
copy Changes.txt nv_build_dir


/* compile the ipf file */
ipfc NewView.ipf nv_build_dir||'\NewView.hlp'

call SysFileTree nv_i18n_dir||'\', subdirs, 'DO'
do i=1 to subdirs.0
    if  0 = lastpos('images', subdirs.i) then
    do
        if  0 = lastpos('.svn', subdirs.i) then
        do
            nv_locale = substr(subdirs.i, lastpos('\',subdirs.i) + 1)

            /* compile the ipf file inside the dir */
            call SysFileTree subdirs.i || '\NewView_' || nv_locale || '.ipf', ipffiles, 'FO'
            do j=1 to ipffiles.0
                ipfc ipffiles.j nv_build_dir || '\NewView_' || nv_locale || '.hlp'
            end

            /* copy the lang file */
            copy subdirs.i || '\newview_' || nv_locale || '.lng' nv_build_dir
        end
    end
end

