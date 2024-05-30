$  save$verify = F$VERIFY('dce$install_verify')
$ Copyright = "Copyright (c) Compaq Computer Corporation. 1995,2000. " -
	       + "All Rights Reserved."
$!
$! ###########################################################################
$!			   DCE$INSTALL.COM
$! ###########################################################################
$!
$!                         DCE Kit Installation
$!
$!                      COPYRIGHT (C) 1998,2000 BY
$!                      COMPAQ COMPUTER CORPORATION
$!                          ALL RIGHTS RESERVED.
$!
$! THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND COPIED
$! ONLY IN ACCORDANCE WITH THE TERMS OF SUCH LICENSE AND WITH THE INCLUSION
$! OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR ANY OTHER COPIES
$! THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAILABLE TO ANY OTHER
$! PERSON.  NO TITLE TO AND OWNERSHIP OF THE SOFTWARE IS HEREBY TRANSFERRED.
$!
$! THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
$! SHOULD NOT BE CONSTRUED AS A COMMITMENT BY COMPAQ COMPUTER CORPORATION.
$!
$! COMPAQ ASSUMES NO RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS
$! SOFTWARE ON EQUIPMENT THAT IS NOT SUPPLIED BY COMPAQ.
$!
$! ###########################################################################
$!
$! NAME:
$!     DCE$INSTALL.COM
$!
$! FACILITY:
$!     DCE Kit Installation procedure for OpenVMS 
$!
$! ABSTRACT:
$!     This procedure will perform the installation of DCE
$!
$! ###########################################################################
$!
$! This command procedure handles the installation of DCE V3.0
$! in 3 steps:
$! 	1) Performs pre-installation tasks:
$!		creates DCE$SERVER account, login file, etc.
$!	2) Installs the PCSI kit by invoking PCSI
$!	3) Performs post-installation:
$!		adds net$declareobject ID to DCE$SERVER account, cleanup, etc.
$!
$  default_dir = F$ENVIRONMENT("DEFAULT")
$  working_dir = ""
$  SET PROC/PRIV=BYPASS
$  IF $SEVERITY .NE. 1
$  THEN
$     CALL print_dce_message E NOSETPRIV "Unable to set process privs to BYPASS...cannot continue."
$     GOTO exit_with_error
$  ENDIF
$  ON ERROR THEN GOTO exit_with_error
$  ON CONTROL_Y THEN GOTO exit_with_ctly
$  SET ON
$  TRUE = 1
$  FALSE = 0
$  platform = f$getsyi("ARCH_NAME")
$!
$! Define various directory and file names
$!
$ Temp			= F$Trnlnm("Sys$Scratch")
$ Pid			= F$GetJPI( "","Pid" )
$ Temp_Output_File	= "''Temp'Dce_Out''Pid'.Tmp"
$ Temp_Data_File	= "''Temp'Dce_Dat''Pid'.Tmp"
$!
$  required_clisymtbl = 250
$  if platform .eqs. "VAX"
$  then
$     required_free_gblsects = 40
$     required_free_gblpages = 3750
$  else
$     required_free_gblsects = 35
$     required_free_gblpages = 7350
$  endif
$!
$  say = "write sys$output"
$  ask = "read sys$command/prompt="
$  authorize = "MCR AUTHORIZE"
$!Check p1 for "HELP" -- the only supported product install option
$  if p1 .nes. ""
$  then
$     if  f$edit(p1,"UPCASE") .nes. "HELP"
$     then
$        CALL print_dce_message E NOINSTALL -
	 "Command line parameter error.  Only suppported value is HELP."
$        goto exit_with_error
$     endif
$     product_help = "/HELP"
$  else
$     product_help = ""
$  endif
$!
$  say ""
$  say ""
$  say "        Performing DCE pre-installation tasks...please wait."
$  say ""
$!********** Create Working Directory and Get Product Info **************
$! Save the current directory, create a working directory and set
$! default there.  Return to default directory when exiting this routine.
$!
$  default_dir = F$ENVIRONMENT("DEFAULT")
$  working_dir = "SYS$COMMON:[SYSUPD.DCE$INSTALL]"
$  working_dir_name = "SYS$COMMON:[SYSUPD]DCE$INSTALL.DIR"
$  adk_installed = FALSE
$  IF F$SEARCH("''working_dir_name'") .EQS. ""
$  THEN
      CREATE/DIR 'working_dir'
$     IF .NOT. $STATUS THEN GOTO exit_with_error
$  ENDIF
$  SET DEF 'working_dir' 
$!
$! PCSI allows installation of kits on devices other than the default
$! system disk,  so we have to find out if/where the kit is installed.
$! Get this from the Product database.  If a DCE kit has not previously
$! been installed by PCSI, then default to sys$sysdevice:[vms$common].
$! 
$!
$  dce_root_dir = ""
$  product_dir = ""
$  DCE_installed = FALSE		! Has PCSI previuosly installed DCE?
$  product_object = "[DCELOCAL]"	! get location of this subdirectory
$  GOSUB get_product_info
$  IF product_dir .EQS. ""
$  THEN dce_root_dir = "SYS$SYSDEVICE:[VMS$COMMON"	! defaults to this
$  ELSE dce_root_dir = product_dir
$  ENDIF
$
$!******************* Create the DCE$SERVER account *************
$! Create the DCE$SERVER account.
$!
$  GOSUB check_system_files
$!
$  DCE$ACCT = "DCE$SERVER"
$  IF F$IDENTIFIER("''DCE$ACCT'","NAME_TO_NUMBER") .ne. 0
$     THEN
$        TYPE SYS$INPUT

    This installation procedure has detected an existing DCE$SERVER
    account. Correct operation of DCE on this system requires that the
    DCE$SERVER account have TMPMBX, NETMBX, DETACH and SYSPRV privileges.

    The installation procedure will modify the DCE$SERVER account to ensure 
    that the prerequisite privileges are present.

$        GOTO tighten_account
$  ENDIF
$!
$!  Account Not already there, must create it. First ask the user for the UIC
$!  Tell the user that we are creating the DCE$SERVER account (if not already 
$!  there)
$!
$!  As part of the  DCE Developer's Kit installation, we will create
$!  an account named DCE$SERVER from which the DCE daemons will be run.
$!  This account cannot be logged into.  It is used solely for execution
$!  of the DCE daemons.  It will be created with TMPMBX, NETMBX, DETACH
$!  and SYSPRV privileges.
$!
$  TYPE SYS$INPUT

    In order to ensure that the DCE daemon processes run in the proper
    environment, this installation procedure will create an account for
    them.  This account, DCE$SERVER, is created with TMPMBX, NETMBX,
    DETACH and SYSPRV privileges.  It is not possible to log into this
    account.

    You must specify a unique UIC for this account in order to ensure
    proper security of the network.  The password for this account will
    be generated.  You do not need to know the password, since the account
    is disabled.  If this scenario violates your security policies, you
    may change it after the installation has finished via the VMS
    AUTHORIZE utility.
$!
$  DCE$GROUP = %O363  ! this is fixed
$  DCE$MEMBR = %O363  ! find the first available Member number from here.
$
$  get_a_default_uic:
$  dce$uic_int = (DCE$MEMBR + (%X10000 * DCE$GROUP))
$  IF F$IDENTIFIER (dce$uic_int,"NUMBER_TO_NAME") .EQS. "" THEN GOTO get_a_dce_uic
$  ! otherwise someone already has that UIC, so try the next.
$  DCE$MEMBR = DCE$MEMBR + 1
$  GOTO get_a_default_uic
$
$  get_a_dce_uic:	!Get a unique UIC
$  dce$def_uic = F$FAO("!%U",dce$uic_int)
$  dce$uic = dce$def_uic
$  say ""
$  ask -
        "Enter the UIC of the new DCE$SERVER account: ''dce$def_uic': " -
	temp_input 
$  IF temp_input .NES. "" THEN dce$uic = temp_input
$  dce$uic = dce$uic - "[" - "]"
$  dce$uic = "[" + dce$uic + "]"
$  IF (F$EXTRACT(0,1,dce$uic) .nes. "[") .OR. -
        (F$EXTRACT(F$LENGTH(dce$uic)-1,1,dce$uic) .nes. "]") -
     THEN GOTO bad_dce_uic
$  dce$check_uic = F$EXTRACT(1,F$LENGTH(dce$uic)-2,dce$uic)
$  IF (F$EXTRACT(0,1,dce$check_uic) .eqs. ",") .OR. -
        (F$EXTRACT(F$LENGTH(dce$check_uic)-1,1,dce$check_uic) .eqs. ",") -
     THEN GOTO bad_dce_uic
$ dce$check_uic_loop:
$  dce$check_last = dce$check_uic
$  dce$check_uic = dce$check_uic - "0" - "1" - "2" - "3" - "4" - "5" - "6" - "7"
$  IF dce$check_uic .eqs. "," THEN GOTO get_a_dce_uic_done
$  IF dce$check_uic .nes. dce$check_last THEN GOTO dce$check_uic_loop
$!
$ bad_dce_uic:
$  CALL print_dce_message W INVUICFMT "Invalid UIC format, please enter OCTAL [###,###]"
$  GOTO get_a_dce_uic
$
$ get_a_dce_uic_done:
$! Now verify that what they gave us is not already in use
$!
$  dce$dgrp = f$element(0,",",dce$uic) - "["
$  dce$dmem = dce$uic - "[" - "''dce$dgrp'" - "," - "]"
$  dce$uint = (%O'dce$dmem + (%X10000 * %O'dce$dgrp))
$  IF F$IDENTIFIER (dce$uint,"NUMBER_TO_NAME") .NES. ""
$     THEN
$         CALL print_dce_message W UICINUSE "That UIC is already in use, please choose another one"
$         GOTO get_a_dce_uic
$  ENDIF
$!
$! If the user enters [200,200], then in some circumstances this may be assigned
$! to DCE$SERVER possibly resulting in two accounts DCE$SERVER and DEFAULT with
$! the same UIC of [200,200]. Check for this case and give the user the option to
$! override or enter a new UIC. Note: this does not happen when DEFAULT is owned
$! by SYSTEM MANAGER; this gets trapped by the above code. (DM:10/28/93)
$! The conversion of [200,200] to Octal is %O00040000200.
$
$  IF F$IDENTIFIER ("DEFAULT","NAME_TO_NUMBER") .EQ. 0 .AND. dce$uint .EQ. %O00040000200
$     THEN
$         say ""
$         ask -
  	  "The UIC [200,200] is already in use, do you want to use it anyway for DCE$SERVER" -
	  dce$uic_okay 
$         IF .NOT. dce$uic_okay THEN GOTO get_a_dce_uic
$  ENDIF
$!
$  IF F$IDENTIFIER (dce$uint,"NUMBER_TO_NAME") .NES. ""
$     THEN
$         CALL print_dce_message W UICINUSE "That UIC is already in use, please choose another one"
$         GOTO get_a_dce_uic
$  ENDIF
$!
$! Is this a system UIC?
$  IF (F$GETSYI("MAXSYSGROUP") .GE. dce$dgrp)
$     THEN
$         CALL print_dce_message I SYSTEMUIC "The UIC entered is a system UIC"
$         say ""
$         ask - 
		"Do you wish to use a system UIC for the DCE$SERVER account? [N]: " -
		dce$uic_okay 
$         IF .NOT. dce$uic_okay THEN GOTO get_a_dce_uic
$  ENDIF
$!
$! Assign "quoted string" Authorize account parameters to global symbols. 
$! VMSINSTAL on AXP requires these else aborts.
$!
$  dce$cputime	= """0 00:00:00.01"
$  dce$cputime	= "00:00:00.01"
$!
$! Now actually create the account
$!
$  dce$device = "/dev=" + F$EXTRACT(0,F$LOCATE(":",dce_root_dir),dce_root_dir)
$  dce$dir    = "/dir=" -
  + F$EXTRACT(F$LOCATE("[",dce_root_dir),F$LENGTH(dce_root_dir),dce_root_dir) -
  + ".DCE$SERVER]
$  dce$flags  = "/flag=(capt,defc,disct,disim,disma,disne,disus,lockp,restr)"
$  dce$qual1  = "/own=DCE$SERVER" + -
		"/defp=(noall,tmpm,netm)" + -
                "/priv=(noall,tmpm,netm,deta,sysp)/pwdex/pwdl=30-/pwdm=16" 
$!
$  dce$qual2  = "/nobat/noint/nonet/cli=DCL/clita=DCLTABLES" + -
		"/uic=''dce$uic'/LGI=SYS$SYSTEM:DCE$LOGIN.COM" + -
                "/cp=''dce$cputime'/maxj=1/maxdet=1" + -
		"/wsdef=1024/wsquo=2048/wsextent=2048"
$!
$  DEFINE/USER sysuaf 'sysuaf'
$  DEFINE/USER rightslist 'rightslist'
$  authorize ADD 'DCE$ACCT' 'dce$device''dce$dir''dce$qual2'
$  DEFINE/USER sysuaf 'sysuaf'
$  DEFINE/USER rightslist 'rightslist'
$  authorize MOD 'DCE$ACCT' 'dce$flags''dce$qual1'
$!
$  search_file = dce_root_dir + "]DCE$SERVER.DIR"
$  IF F$SEARCH("''search_file'") .EQS. "" THEN -
		CREATE/DIR/owner='dce$uic' 'dce_root_dir'.DCE$SERVER]
$!
$tighten_account:
$  dce$privs      =  "/priv=(noall,tmpm,netm,deta,sysp)/defp=(noall,tmpm,netm)"
$  DEFINE/USER sysuaf 'sysuaf'
$  DEFINE/USER rightslist 'rightslist'
$  authorize MOD 'DCE$ACCT' 'dce$privs'
$!
$dce$acct_created:
$!
$!  Create the LOGIN.COM file in the directory.  It simply logs the user back 
$!  out. This is only for security reasons since this account is not to be 
$!  logged into.
$!
$  IF F$SEARCH("SYS$COMMON:[SYSEXE]DCE$LOGIN.COM") .EQS. ""
$  THEN
$     OPEN/WRITE loginfile 'working_dir'DCE$LOGIN.COM
$     WRITE loginfile "$!! This LOGIN command procedure is required by the "
$     WRITE loginfile "$!! DCE$SERVER account. Since the DCE$SERVER account is a" 
$     WRITE loginfile "$!! captive account that is not to be logged into, this "
$     WRITE loginfile "$!! file is created as a last layer of protection to "
$     WRITE loginfile "$!! prevent login if the LOGINOUT checks should be evaded."
$     WRITE loginfile "$!! *** THIS FILE IS REQUIRED BY COMPAQ SQA ***"
$     WRITE loginfile "$ logout = """logout""""
$     WRITE loginfile "$ logout"
$     WRITE loginfile "$ exit %x10000001"
$     CLOSE loginfile
$     COPY/PROT=(S:RWED,O,G,W) 'working_dir'DCE$LOGIN.COM SYS$COMMON:[SYSEXE]
$  ENDIF
$!
$!**************** Invoke PCSI to install the DCE kit ******************
$  this_dir = f$parse(f$environment("PROCEDURE"),,,"DEVICE",) + -
        f$parse(f$environment("PROCEDURE"),,,"DIRECTORY",)
$!
$  PRODUCT INSTALL 'product_help' DCE/SOURCE='this_dir'
$!
$  a = $status
$  IF .NOT. (a .EQ. 1)
$  THEN
$terminate_on_prod_install:
$     say ""
$     CALL print_dce_message W PRODINST -
		"An error occured during the product installation."
$     ask "Do you want to terminate? [Y] " terminate
$     IF terminate .eqs. "" then terminate = "YES"
$     terminate = F$EDIT(terminate,"UPCASE")
$     input_ok = F$LOCATE(terminate,"YES") .EQ. 0  .OR. -
     		F$LOCATE(terminate,"NO") .EQ. 0
$     IF .NOT. input_ok THEN call print_message -
     		"Please enter YES or NO."
$     IF .NOT. input_ok THEN GOTO terminate_on_prod_install
$     IF terminate
$     THEN
$        CALL print_dce_message E NOINSTALL -
		"The PCSI Product installation was not successful."
$        GOTO exit_with_error
$     ELSE
$       say ""
$       say "*** Continuing with post-installation."
$       say ""
$     ENDIF
$  ELSE
$     say ""
$     say "*** DCE Product installation successful...beginning post-installation."
$     say ""
$  ENDIF
$!*************  Perform the post-installation tasks *******************
$!
$! Get a copy of DXD$XDS_SHR.EXE into [syslib] if X500 has not installed one
$!
$  IF f$search("sys$common:[syslib]DXD$XDS_SHR.EXE") .EQS. "" THEN -
	copy sys$common:[dcelocal.etc]DXD$XDS_SHR.EXE sys$common:[syslib]
$!
$  do_ident = F$IDENTIFIER("NET$DECLAREOBJECT","NAME_TO_NUMBER")
$!
$  IF (do_ident .EQ. 0)
$     THEN  		!Identifier doesn't yet exist
$        CALL print_dce_message I ADDID "This installation adds an IDENTIFIER named NET$DECLAREOBJECT."
$        MCR 'dce_root_dir'.SYSEXE]DCE$ADD_ID.EXE
$  	 IF .NOT. $STATUS
$	    THEN
$   	       TYPE SYS$INPUT

    The dynamic rights identifier NET$DECLAREOBJECT could not be created 
    on this system. Without this identifier, each account in which DCE 
    server applications are run must have the SYSNAM privilege in order to 
    listen for RPC requests over DECnet.

    This installation will continue ...
$
$  	 ENDIF 	!if .not. $status
$     ELSE   !do_ident .ne. 0
$        IF (do_ident .NE. %x91F50005)
$           THEN
$     	       TYPE SYS$INPUT

    The rights identifier NET$DECLAREOBJECT already exists on this system.
    However, its current value is not %x91F50005. This product expects this
    identifier to have this value.  You should modify this identifier's
    value to be %x91F50005 to ensure the correct operation of DCE server
    applications. If you do not do this, each account in which DCE server
    applications are run must have the SYSNAM privilege in order to listen
    for RPC requests over DECnet.

$
$      	       ask "Do you wish to continue? [Y]: " continue
$      	       IF (continue .NES. "") .AND. (.NOT. continue) THEN -
			GOTO exit_with_error
$        ENDIF	! if (do_ident .ne. %X91f50005)
$  ENDIF	! if (do_ident .eq. 0)
$!
$   TYPE SYS$INPUT

    The rights identifier NET$DECLAREOBJECT will now be granted to the
    DCE$SERVER account. You may IGNORE the message:

"%UAF-E-GRANTERR, unable to grant identifier NET$DECLAREOBJECT to DCE$SERVER
-SYSTEM-F-DUPIDENT, duplicate identifier"

    if it should occur.

$   ask "Press return to Continue" junk
$  SET NOON
$  DEFINE/USER sysuaf 'sysuaf'
$  DEFINE/USER netproxy 'netproxy'
$  authorize GRANT/IDENTIFIER net$declareobject 'dce$acct'
$  SET ON
$!
$  SET FILE/OWNER=DCE$SERVER 'dce_root_dir']dcelocal.dir
$  SET FILE/OWNER=DCE$SERVER 'dce_root_dir'.dcelocal...]*.dir
$!
$! LSE environment setup for ADK installations -
$! If the LSEUPDATE_ENV.TPU is present in the [DCE$LIBRARY] directory, then
$! the ADK installation option was selected, and the LSE IDL support files
$! have been installed on the system.  Ask if they want the LSE environment
$! file updated to support the IDL.  
$!
$  environment_file = "''dce_root_dir'" + ".SYSLIB]LSEUPDATE_ENV.TPU"
$  IF F$SEARCH("''environment_file'") .NES ""
$  THEN
$     adk_installed = TRUE
$     say ""
$     ask -
	"Load the Language-Sensitive Editor (LSE) templates for IDL? [Y]: " -
		load_lse
$     IF (load_lse .NES. "") .AND. (.NOT. load_lse) THEN -
	GOTO end_lse
$     IF (F$SEARCH("''dce_root_dir'.SYSLIB]LSE$SYSTEM_ENVIRONMENT.ENV") .EQS. "") -
            .OR. (F$SEARCH("''dce_root_dir'.SYSLIB]LSESHR.EXE") .EQS. "")
$     THEN 
$       TYPE SYS$INPUT:

     Support for the use of IDL with the Language-Sensitive Editor (LSE) 
     cannot be installed because a valid LSE is not installed.  Either the LSE
     shareable image or the LSE system environment, or both, is missing.  To 
     install this support, you must first install the LSEDIT product, then 
     reinstall DCE for OpenVMS.

$     ELSE
$	! Next check for an appropriate license for LSE.  Since there are
$       ! many possible licenses that will work, the simplest method for this
$	! check is to see what happens when trying to run LSE.  Invoke LSE
$       ! as a spawned subprocess, directing output to a log file.  Search the
$	! log file to determine if LSE did not run due to a license problem.
$	OPEN/WRITE outfile 'working_dir'lse_license_check.com
$ 	WRITE outfile "$ define sys$output x.x"
$ 	WRITE outfile "$ define sys$error x.x"
$	WRITE outfile "$ lse"
$	WRITE outfile "$ exit"
$ 	WRITE outfile "$ deass sys$output"
$ 	WRITE outfile "$ deass sys$error"
$       CLOSE outfile
$	DEFINE SYS$OUTPUT nl:
$	DEFINE SYS$ERROR nl:
$       SET NOON
$       SPAWN/INPUT='working_dir'lse_license_check.com
$	SET ON
$	SEARCH 'working_dir'x.x LICENSE-F-NOLICENSE
$       severity = $SEVERITY
$	! Following logicals are referenced by LSEUPDATE_ENV.TPU when
$	! updating LSE environment
$	DEFINE VMI$KWD SYS$COMMON:[SYSLIB]
$	DEFINE VMI$ROOT SYS$COMMON
$       DEASS SYS$OUTPUT
$       DEASS SYS$ERROR
$	IF severity .EQ. 1
$	THEN
$	   CALL print_dce_message E NOLSEDIT -
	"Cannot install LSE templates for the IDL compiler, no LSE license is active"
$	   GOTO exit_with_error
$       ENDIF
$       CALL print_dce_message I TEMPLATES -
              "Installing LSE templates for the IDL compiler"
$       lsedit/nodisplay/nosystem_environment/nocurrent_file/nosection -
              /command='environment_file' -
              /noinitialization -
              /noenvironment
$       PURGE/KEEP=2 SYS$COMMON:[SYSLIB]LSE$SYSTEM_ENVIRONMENT.ENV
$     ENDIF
$  ENDIF
$  end_lse:
$!
$ IF adk_installed
$ THEN
$     TYPE SYS$INPUT

    NOTE: Please add the following to your system's SYS$MANAGER:SYLOGIN.COM.
          These files define foreign commands for using DCE on OpenVMS.

          $ @SYS$MANAGER:DCE$DEFINE_REQUIRED_COMMANDS.COM
          $ @SYS$COMMON:[DCE$LIBRARY]DCE$DEFINE_OPTIONAL_COMMANDS.COM

$ ELSE
$     TYPE SYS$INPUT

    NOTE: Please add the following to your system's SYS$MANAGER:SYLOGIN.COM.
          These files define foreign commands for using DCE on OpenVMS.

          $ @SYS$MANAGER:DCE$DEFINE_REQUIRED_COMMANDS.COM

$ ENDIF
$! First find out if Compaq's TCP/IP product is running, and whether it's UCX
$! or TCPIP for use in the following display.
$!
$ TCPIP_Version = 0	! Init to impossible value -- will be changed if
$			! able to determine actual version of TCPIP Product
$ define sys$output 'Temp_Data_File
$ define sys$error 'Temp_Data_File
$ UCX Show Version
$ Temp = $Status
$ deass sys$output
$ deass sys$error
$ If .Not. Temp Then Goto CheckTCPIPVersion_Exit ! Not Compaq TCP/IP product
$ define sys$output nl:
$ define sys$error nl:
$ Sear 'Temp_Data_File openvms,version/mat=and/out = 'Temp_Output_File
$ deass sys$output
$ deass sys$error
$ Open/Read/Error=CheckTCPIPVersion_OpenErr Temp_Output_Lnm 'Temp_Output_File
$ Read/End=CheckTCPIPVersion_FileErr/Error=CheckTCPIPVersion_FileErr Temp_Output_Lnm Temp1
$ Temp1 = F$Edit(Temp1,"compress,trim,upcase")
$ Temp2 = F$Locate("VERSION",Temp1)
$ If Temp2 .Eq. F$Length(Temp1) Then Goto CheckTCPIPVersion_Err
$ ! Version Vx.y
$ Temp1 = F$Extract(Temp2,100,Temp1)
$ ! Version major (x from x.y)
$ TCPIP_Version = F$Extract(F$Locate(".",Temp1)-1,1,Temp1)
$ GoTo CheckTCPIPVersion_Exit
$!
$CheckTCPIPVersion_OpenErr:
$ Temp1 = "Could not open DCL TCPIP check version output file"
$CheckTCPIPVersion_FileErr:
$ Temp1 = "Error while reading DCL TCPIP check version output file"
$CheckTCPIPVersion_Err:
$ Echo     "***     Unable to determine TCP/IP Product Version"
$ Echo     "***  Will not attempt to check UCX parameter settings"
$CheckTCPIPVersion_Exit:
$ Close Temp_Output_Lnm
$ If F$Search("'Temp_Output_File'") .Nes. "" Then Delete 'Temp_Output_File';0
$ If F$Search("'Temp_Data_File'") .Nes. "" Then Delete 'Temp_Data_File';0
$ digital_tcpip_prod = "TCPIP"
$ If TCPIP_Version .Gt. 0 .And. TCPIP_Version .Lt. 5 -
	Then digital_tcpip_prod = "UCX"
$!
$! Tell the user about adding DCE$STARTUP.COM TO SYS$STARTUP:SYSTARTUP_*.COM 
$! and the need to install one TCP/IP product to have a life ...
$!
$  dce$tcpip = ""
$  TYPE SYS$INPUT

      Please add the following command to SYS$STARTUP:SYSTARTUP_*.COM on your
      system. This ensures that DCE$STARTUP.COM is executed at system boot. The 
      parameters supplied to DCE$STARTUP.COM depend on the specific TCP/IP 
      product you intend to use. You will now be asked to select the name of 
      this TCP/IP product, and the installation will supply you with the 
      correct command for SYS$STARTUP:SYSTARTUP_*.COM.

	TCP/IP product					Keyword

$ say -
"	Compaq's TCP/IP Services for OpenVMS		''digital_tcpip_prod'"
$  TYPE SYS$INPUT
	Multinet from TGV				MULTINET
	Pathway from Wollongong				PATHWAY
	TCPware from Process Software			TCPWARE
	No TCP/IP Available at this time		NONE
	
$  select_tcpip:
$  ask "Enter one of the keywords from the table above [''digital_tcpip_prod']: " dce$tcpip
$  dce$tcpip = F$EDIT(dce$tcpip,"UPCASE")
$!
$  IF dce$tcpip .EQS. "" .OR. dce$tcpip .EQS. "UCX" .OR. dce$tcpip .EQS. "TCPIP"
$  THEN
$     CALL print_message "Enter $ @SYS$STARTUP:DCE$STARTUP in your SYS$STARTUP:SYSTARTUP_*.COM"
$     GOTO select_tcpip_found
$  ENDIF
$  IF dce$tcpip .EQS. "MULTINET" 
$  THEN
$     CALL print_message "Enter $ @SYS$STARTUP:DCE$STARTUP START MULTINET in your SYS$STARTUP:SYSTARTUP_*.COM"
$     GOTO select_tcpip_found
$  ENDIF
$  IF dce$tcpip .EQS. "PATHWAY" 
$  THEN
$     CALL print_message "Enter $ @SYS$STARTUP:DCE$STARTUP START PATHWAY in your SYS$STARTUP:SYSTARTUP_*.COM"
$     GOTO select_tcpip_found
$  ENDIF
$  IF dce$tcpip .EQS. "TCPWARE" 
$  THEN
$     CALL print_message "Enter $ @SYS$STARTUP:DCE$STARTUP START TCPWARE in your SYS$STARTUP:SYSTARTUP_*.COM"
$     GOTO select_tcpip_found
$  ENDIF
$  IF dce$tcpip .EQS. "NONE" 
$  THEN
$     CALL print_message "Enter $ @SYS$STARTUP:DCE$STARTUP START NONE in your SYS$STARTUP:SYSTARTUP_*.COM"
$     GOTO select_tcpip_found
$  ENDIF
$  CALL print_message "Your response was not from the above list, please enter keyword again" 
$  GOTO select_tcpip
$  select_tcpip_found:
$!
$! Check the values system parameters and notify if less than
$! recommended minimum.
$!
$  if f$getsyi("CLISYMTBL") .lt. required_clisymtbl then -
	CALL print_dce_message W INSTALL "Please increase the sysgen parameter CLISYMTBL to ''required_clisymtbl'"
$  if f$getsyi("FREE_GBLPAGES") .lt. required_free_gblpages
$  then
$     increase_to = (required_free_gblpages - f$getsyi("FREE_GBLPAGES")) +  f$getsyi("GBLPAGES")
$     CALL print_dce_message W INSTALL "Please increase the sysgen parameter GBLPAGES to ''increase_to'"
$  endif
$  if f$getsyi("FREE_GBLSECTS") .lt. required_free_gblsects
$  then
$     increase_to = (required_free_gblsects - f$getsyi("FREE_GBLSECTS")) +  f$getsyi("GBLSECTIONS")
$     CALL print_dce_message W INSTALL "Please increase the sysgen parameter GBLSECTIONS to ''increase_to'"
$  endif
$!
$!
$  exit_with_success:
$  CALL print_dce_message S INSTALL "Installation of OpenVMS DCE V3.0 completed"
$  GOSUB cleanup
$  EXIT (1)
$!
$  exit_with_error:
$  CALL print_dce_message E ABORTINST "Aborting installation of OpenVMS DCE V3.0"
$  GOSUB cleanup
$  EXIT
$!
$  exit_with_ctly:
$  CALL print_dce_message E ABORTINST "User aborted installation of OpenVMS DCE V3.0...Exiting"
$  GOSUB cleanup
$  EXIT (1553)
$!
$! Subroutines
$!
$print_message: subroutine
$    WRITE SYS$OUTPUT ""
$    WRITE SYS$OUTPUT "''p1'"
$endsubroutine
$!
$print_dce_message: subroutine
$    WRITE SYS$OUTPUT ""
$    WRITE SYS$OUTPUT "%DCE-''p1'-''p2', ''p3'"
$endsubroutine
$!
$get_product_info:
$    !
$    ! PCSI allows installation of kit to a device other than sys$common,
$    ! so we have to find out where the kit is installed.  Get this from the
$    ! Product database
$    !
$    ! Implicit parameters:
$    !
$    !  product_object = (input) check the PCSI database for this object
$    !  product_dir = (output) if product_object exists, it's root directory
$    !  DCE_installed = (output) TRUE if PCSI has record of DCE product
$    !
$    filename = working_dir + "show_object.lis"
$    DEFINE SYS$ERROR nl:
$    DEFINE SYS$OUTPUT 'filename'
$    PRODUCT SHOW OBJECT/FULL 'product_object'
$    DEASS SYS$OUTPUT
$    DEASS SYS$ERROR
$    OPEN/READ/ERROR=open_err infile 'filename'
$    ! Throw out the first line
$    READ/END=not_found/ERROR=read_err infile record
$ read_dest:
$    READ/END=not_found/ERROR=read_err infile record
$    trim_line = F$EDIT(record,"COMPRESS,TRIM")
$    IF F$LOCATE("[DCELOCAL]",trim_line) -
		.NES. F$LENGTH(trim_line) THEN GOTO found_object
$    GOTO read_dest
$ found_object:
$    i = 1
$ get_device:
$    device = F$ELEMENT('i'," ",trim_line)
$    IF device .EQS. " " THEN GOTO not_found
$    if f$locate(":",device) .nes. f$length(device) then -
        	goto found_device
$    i = i + 1
$    goto get_device
$ found_device:
$    product_dir = device - ".]"
$ not_found:
$    CLOSE infile
$    GOTO end_get_product_dir
$ open_err:
$    Call print_dce_message E NOTOPEN "Unable to open file ''filename'"
$    GOTO exit_with_error
$ read_err:
$    Call print_dce_message E NOTOPEN "Unable to read file ''filename'"
$    CLOSE infile
$    GOTO exit_with_error
$end_get_product_dir:
$! Now check the PCSI database for a record of the DCE product and return
$! the result as TRUE or FALSE in DCE_installed
$    filename = working_dir + "show_product.lis"
$    DEFINE SYS$ERROR nl:
$    DEFINE/USER SYS$OUTPUT 'filename'
$    PRODUCT SHOW PRODUCT/FULL DCE
$    DEFINE/USER SYS$OUTPUT nl:
$    SEAR 'filename' VMS,DCE/MAT=AND
$    IF $severity .EQ. 1
$    THEN DCE_installed = TRUE
$    ELSE DCE_installed = FALSE
$    ENDIF
$    DEASSIGN SYS$ERROR
$RETURN ! get_product_info
$!
$check_system_files:
$  sysuaf = F$TRNLNM("SYSUAF")
$  IF sysuaf .EQS. "" THEN -
	sysuaf = F$PARSE("SYSUAF","SYS$SYSTEM:.DAT",,,"SYNTAX_ONLY")
$  IF F$SEARCH("''sysuaf'") .EQS. ""
$  THEN
$     CALL print_dce_message F -
	    "NOSYSUAF "SYS$SYSTEM:SYSUAF.DAT does not exist on this system"
$     GOTO exit_with_error
$  ENDIF
$!
$  rightslist = F$TRNLNM("RIGHTSLIST")
$  IF rightslist .EQS. "" THEN -
        rightslist = F$PARSE("RIGHTSLIST","SYS$SYSTEM:.DAT",,,"SYNTAX_ONLY")
$  IF F$SEARCH("''rightslist'") .EQS. ""
$  THEN
$     CALL print_dce_message F -
	    NORIGHTSLIST "SYS$SYSTEM:RIGHTSLIST.DAT does not exist on this system"
$     GOTO exit_with_error
$  ENDIF
$!
$  netproxy = F$TRNLNM("NETPROXY")
$  IF netproxy .EQS. "" THEN -
	netproxy = F$PARSE("NETPROXY","SYS$SYSTEM:.DAT",,,"SYNTAX_ONLY")
$RETURN ! check_system_files
$!
$!******************* get_image_name *********************
$! This procedure extracts the image name string from
$! an executable image, and returns it in image_name.
$!
$! Implicit Parameters:
$!
$!   1) image_to_check = (input) Full file spec of image to be examined.
$!   2)	image_name = (output) Symbol in which to return image name string:
$!	    Possible return values are:
$!		the actual image name
$!		"not found"
$!		"error"
$!
$!      offsets into the header are found by looking in LIB.REQ for
$!      the following fields:
$!              Alpha:                          VAX:
$!              EIHD$L_IMGIDOFF                 IHD$W_IMGIDOFF
$!              EIHI$T_IMGID                    IHI$T_IMGID
$!              EIHD$L_MAJORID                  IHD$W_MAJORID
$!              EIHD$L_MINORID                  IHD$W_MINORID
$!
$get_image_name:
$!
$ p1 = f$edit(image_to_check,"UPCASE")
$ wrk_image_name = ""
$ file = f$search(p1)
$ if file .eqs. "" then goto gin_no_file
$ open/read/error=gin_no_read i_file 'file'
$ read/end=gin_bad_file/error=gin_no_read i_file record
$ close/nolog i_file
$!
$! For VAX, the 1st 2 bytes are size of header and will show a size
$! of 48 or larger. For Alpha, this is part of the version field.
$!
$       if f$cvui(0,16,f$ext(0,2,record)) .lt. 48
$       then                                    ! We have an ALPHA image
$               ptr = f$cvui(0,32,f$ext(24,4,record))   ! Get image name offset
$!                                                      ! (EIHD$L_IMGIDOFF)
$               if ptr .eq. 0 then goto gin_no_id       ! ID area defined?
$!
$! Setup id offset appropriate for version of VMS file.
$!
$               offset = 16                             ! Set default offset
$!                                                      ! (EIHI$T_IMGID)
$               tmp = f$cvui(0,32,f$ext(0,8,record))    ! Get major/minor id
$!                                                      ! (EIHD$L_MAJORID/EIHD$L_MINORID)
$               if tmp .eq. 2 then offset = 16          ! V1 file
$               ptr = ptr + offset                      ! Bump pointer to id area
$               fis = f$cvui(0,8,f$ext(ptr,1,record))   ! Get length of name
$               fi = "''f$ext(ptr+1,fis,record)'"       ! Get image name 
$               wrk_image_name = "''f$edit(fi,"compress,trim")'"
$       else                                    ! We have a VAX image
$		ptr = f$cvui(0,16,f$ext(6,2,record))	! Get image name offset
$!                                                      ! (IHD$W_IMGIDOFF)
$		if ptr .eq. 0 then goto gin_no_id	! ID area defined?
$!
$! Setup id offset appropriate for version of VMS file.
$!
$		offset = 0				! Set default offset				
$!                                                      ! (IHI$T_IMGID)
$		tmp = "''f$ext(12,4,record)'"		! Get major/minor id
$!                                                      ! (IHD$W_MAJORID/IHD$W_MINORID)
$		if tmp .eqs. "0205" then offset = 0	! V4/V5 file
$		ptr = ptr + offset			! Bump pointer to id area
$		fis = f$cvui(0,8,f$ext(ptr,1,record))	! Get length of name
$		fi = "''f$ext(ptr+1,fis,record)'"	! Get image name
$		wrk_image_name = "''f$edit(fi,"compress,trim")'"
$	endif
$!
$    image_name == wrk_image_name
$ return
$!
$ gin_no_id:
$       image_name == "error"
$       RETURN
$ gin_no_file:
$	image_name == "not found"
$       RETURN
$ gin_no_read:
$	close/nolog i_file
$       image_name == "error"
$       RETURN
$ gin_bad_file:
$	close/nolog i_file	
$	image_name == "error"
$       RETURN ! get_image_name
$!
$!******************* cleanup *********************
$cleanup:
$    if default_dir .NES. "" then -
         SET DEFAULT 'default_dir'
$    IF (working_dir .NES. "") .AND.  F$SEARCH("''working_dir'*.*") .NES. ""
$    THEN
$	DELETE 'working_dir'*.*;*
$	DELETE 'working_dir_name';*
$    ENDIF
$    junk = F$VERIFY(save$verify)
$    say ""
$RETURN ! cleanup
$!******************* Begin History ***************
$! 23-Feb-98 MMF Removed subroutines get_image_version and
$!	 	 compare_image_version.  Added handling
$! 		 of product install help switch.  Removed
$!		 lines handling of appended lines for DCE version.
$!
$! 13-Nov-97	 MMF Creation
$!
$!******************* End History ***************
