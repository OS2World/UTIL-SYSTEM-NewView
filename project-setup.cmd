/* ------------------------------------------------------------------------- *
 * project-setup.cmd                                                         *
 * ------------------------------------------------------------------------- */
Signal On NoValue
Signal On Error
Call RxFuncAdd 'SysLoadFuncs', 'REXXUTIL', 'SysLoadFuncs'
Call SysLoadFuncs

g.!env_inc = Stream('env.inc', 'C', 'QUERY EXISTS')
g.!nv_dev = GetVar('NV_DEV')
If g.!nv_dev == '' Then Do
    Say '%NV_DEV% environment variable is not defined.'
    Say 'Please set %NV_DEV% to the top-level path of the development tree.'
    Return 1
End

nv_rc = SysFileTree('*.spr.def', 'templates.', 'FSO')
IF nv_rc <> 0 THEN Exit nv_rc

Do i = 1 To templates.0
    CALL CreateProjectFile templates.i
End

Exit 0


/* -------------------------------------------------------------------------- *
 * CreateProjectFile                                                          *
 * -------------------------------------------------------------------------- */
CreateProjectFile: Procedure Expose g.
    Parse Arg tpl_file
    lp = LastPos('.', tpl_file )
    If lp == 0 Then Return 2

    Say
    Parse Var tpl_file proj_file =(lp) .
    If Stream( proj_file, 'C', 'QUERY EXISTS') <> '' Then Do
        Say proj_file 'already exists.'
        Call CharOut, 'Replace this file (y/N)? > '
        Parse Upper Pull 1 answer 2 .
        If answer == 'Y' Then Do
            '@copy' proj_file proj_file'.bak >nul'
            '@erase' proj_file
        End
        Else Return 0
    End

    Say 'Creating' proj_file
    Call LineIn tpl_file, 1, 0
    Do While Lines( tpl_file )
        next_line = Linein( tpl_file )
        Call LineOut proj_file, VarSubst( '@NV_DEV@', G.!nv_dev, next_line )
    End
    Call Lineout proj_file
    Call Stream tpl_file, 'C', 'CLOSE'

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
                    Call Value env_var, val, 'OS2ENVIRONMENT'
                    Leave
                End
            End
        End
    End
Return val


/* -------------------------------------------------------------------------- *
 * VarSubst                                                                   *
 * In the specified string, substitute any instances of the given sequence    *
 * (variable) with the replacement text, and return the modified string.      *                                                              *
 * -------------------------------------------------------------------------- */
VarSubst: Procedure
    Parse Arg variable, replacement, string

    Do Until varpos == 0
        varpos = Pos( variable, string )
        IF varpos > 0 Then Do
            Parse Var string _before (variable) _after
            string = _before || replacement || _after
        End
    End
Return string


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


