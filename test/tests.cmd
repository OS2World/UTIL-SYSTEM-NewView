/** REXX **/

say "========================================================================="
say "= NewView tests                                                         ="
say "========================================================================="
say
say "testing the command line"
say
pull



say "*************************************************************************"
say "* newview                                                               *"
say "*                                                                       *"
say "* Expected                                                              *"
say "* NewView start and shows the NewView help.                             *"
say "* - left navigation is shown                                            *"
say "* - contents tab is selected                                            *"
say "* - all contents are collapsed                                          *"
say "* - 'Introduction' is selected and visible at the right                 *"
say "*************************************************************************"
"start newview"
pull


say "*************************************************************************"
say "* newview cmdref                                                        *"
say "*                                                                       *"
say "* Expected                                                              *"
say "* NewView start and shows the 'Command Reference' help                  *"
say "* - left navigation is shown                                            *"
say "* - contents tab is selected                                            *"
say "* - all contents are collapsed                                          *"
say "* - 'Introduction' is selected and visible at the right                 *"
say "*************************************************************************"
"start newview cmdref"
pull


say "*************************************************************************"
say "* newview cmdref copy                                                   *"
say "*                                                                       *"
say "* Expected                                                              *"
say "* NewView start and shows the 'Command Reference' help                  *"
say "* - left navigation is shown                                            *"
say "* - contents tab is selected                                            *"
say "* - only 'OS/2 Commands by name' is expanded                            *"
say "*   and 'COPY' is selected                                              *"
say "*************************************************************************"
"start newview cmdref copy"
pull


say "*************************************************************************"
say "* newview /s cmdref copy                                                *"
say "*                                                                       *"
say "* Expected                                                              *"
say "* NewView start and shows the 'Command Reference' help                  *"
say "* - left navigation is shown                                            *"
say "* - search tab is selected (copy is visible in the search entry field)  *"
say "* - 'COPY' is selected and visible at the right                         *"
say "* - on contents tab only 'OS/2 Commands by name' is expanded            *"
say "*   and 'COPY' is selected                                              *"
say "*************************************************************************"
"start newview /s:copy cmdref"
pull


say "*************************************************************************"
say "* newview /g cmdref copy                                                *"
say "*                                                                       *"
say "* Expected                                                              *"
say "* NewView start and shows the 'Command Reference' help                  *"
say "* - left navigation is shown                                            *"
say "* - contents tab is selected                                            *"
say "* - all contents are collapsed                                          *"
say "* - 'Introduction' is selected and visible at the right                 *"
say "* - The 'Search all fieles' dialog is opend and searches for copy       *"
say "*************************************************************************"
"start newview /g:copy cmdref"
pull

