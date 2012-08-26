" Vim syntax file

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn keyword dblStatement $scr_att $scr_clr $scr_mov $scr_pos ^a ^addr ^arg ^arga ^argdim ^argn
syn keyword dblStatement ^argnum ^argtype ^b ^d ^datatype ^defined ^descr ^eof ^f ^first ^i ^last
syn keyword dblStatement ^m ^null ^o ^passed ^passthru ^ref ^size ^typeof ^val ^varargarray
syn keyword dblStatement ^variant ^x ^xtrnl abstract accept addhandler align alloc ansi any app
syn keyword dblStatement append as assembly begin begincase bktsiz blink blk blksiz block bol
syn keyword dblStatement bold boolean bos bufnum bufsiz by byref byte byval call case catch char
syn keyword dblStatement character chn chr class clear close cls clsmap common cond const contig
syn keyword dblStatement copies copy data datetime decimal decr del delegate delete deq descr
syn keyword dblStatement detach dictionary dimension dir direction directory display disposable
syn keyword dblStatement do double dup else end endcase endclass endcommon enddel enddelegate
syn keyword dblStatement endenum endexternal endfunc endglobal endgroup endinter endinterface
syn keyword dblStatement endliteral endmethod endname endnamespace endparams endprop endproperty
syn keyword dblStatement endprototype endrecord endstruct endstructure endsub endtry endusing
syn keyword dblStatement enum eof eol eos err error event exit exitloop exittry extends external
syn keyword dblStatement fdl final finally find float flush for foreach forever form forms
syn keyword dblStatement freturn from get getrfa gets global go goff gon goto group guiwnd id if
syn keyword dblStatement implements import in incr ind indexed init inout inp input inrange int
syn keyword dblStatement integer interface internal key keynum krf left library line list literal
syn keyword dblStatement local localdata locase lock long lpnum lpque mask match merge method
syn keyword dblStatement mismatch mode module mreturn namespace new nextloop nocond nofield
syn keyword dblStatement nofields nofill nolf nolist nooffsets nop nopage noprec noprecision
syn keyword dblStatement norecord nosuffix nosummary numrec of off offerror offsets on onerror
syn keyword dblStatement open opt optional options out output outrange override page parent
syn keyword dblStatement partial pos posit position pr prefix print private property protect
syn keyword dblStatement protected prototype public purge put puts quad raiseevent range read
syn keyword dblStatement readonly reads rec record recsiz rectype recv reentrant ref rel relative
syn keyword dblStatement removehandler repeat repository req required resident restore return
syn keyword dblStatement returntype rev reverse rfa right round save sbyte screen sealed section
syn keyword dblStatement select send seq sequential set share short si sin sinp sinput skip sleep
syn keyword dblStatement sort stack stackdata start static staticdata step stop store string
syn keyword dblStatement structure su summary supd supdate temp tempfile then this threadlock
syn keyword dblStatement throw thru to truncate try uint ulong under unique unlock unsafe until
syn keyword dblStatement upcase upd update ushort using val value varargs virtual void volatile

" System library symbols 

" Synergy/DE System routines
syn keyword dblFunction abs acchr acesc addr ap_amisc ap_misc arg arga argdim argn argnum
syn keyword dblFunction argtype arrayct ascii ascr5 astrst astsav atrim ax_bind ax_call
syn keyword dblFunction ax_create ax_get ax_getint ax_hide ax_load ax_newval ax_set ax_show
syn keyword dblFunction ax_tkget ax_tkgetint ax_tkset begfl bin bit_is_clr bit_is_set bkstr
syn keyword dblFunction break btod char chnopn chopen cl_axsig cl_proc cl_sig cmdln cnv_data
syn keyword dblFunction cnv_ip cnv_pi compress copy cputime cputm crembx cvtiiv cvtint cvtizv
syn keyword dblFunction cvtzi dachr daesc data_decrypt data_encrypt data_saltiv datatype date
syn keyword dblFunction datecompiled datetime dbl$astrst dbl$astsav dbl$bkstr dbl$chopen
syn keyword dblFunction dbl$cputm dbl$crembx dbl$delmbx dbl$errmod dbl$errtxt
syn keyword dblFunction dbl$execute_image_routine dbl$exiterror dbl$getdfn dbl$getfnm
syn keyword dblFunction dbl$parse dbl$setctl dbl$setdfn dbl$tt_name_to_number
syn keyword dblFunction dbl$tt_number_to_name dbl$ttbrdcst dbl$ttchar dbl$ttname dbl$wkday
syn keyword dblFunction dbl$xargs dbl$xstat decml defined delet delmbx descr dflag dll_call
syn keyword dblFunction dll_close dll_open dotnet_tkaddctl dotnet_tkform dotnet_tkwin dtob
syn keyword dblFunction em_proc empbuf endfl envrn erlin erline ernum err_traceback errmod
syn keyword dblFunction error ertxt exec exite false fatal fill filnm flags flwid fork fp_add
syn keyword dblFunction fp_arccos fp_arcsin fp_arctan fp_cmp fp_cos fp_div fp_expe
syn keyword dblFunction fp_from_num fp_log10 fp_loge fp_mul fp_pow fp_sin fp_sqrt fp_sub
syn keyword dblFunction fp_tan fp_to_num free fstat fxsubr getcm getcrc getdfn getfa getfnm
syn keyword dblFunction getlog getrfa gline gtppn hex http_client_get http_client_post
syn keyword dblFunction http_method http_server_create http_server_delete http_server_receive
syn keyword dblFunction http_server_send implied init_ssql instr int integer isamc isclr
syn keyword dblFunction isinfo isinfoa iskey issts jbnam jbno jperiod keyval kill len line
syn keyword dblFunction lm_info lm_login lm_logout localize m mem_proc modname money ndate
syn keyword dblFunction nspc_add nspc_atos nspc_close nspc_delete nspc_find nspc_getdata
syn keyword dblFunction nspc_misc nspc_move nspc_open nspc_putdata nspc_reset nspc_stoa
syn keyword dblFunction numargs oct openelb option packed paint pak parse passed passthru
syn keyword dblFunction posrfa purge putcm qsort r5asc randm rc_api rc_apix rcb_addarg
syn keyword dblFunction rcb_call rcb_create rcb_delarg rcb_delete rcb_insarg rcb_setarg
syn keyword dblFunction rcb_setargs rcb_setfnc rcflg rcvid rdlen rdtrm recnum ref renam rfa
syn keyword dblFunction rnd round rsize rstat rstatd rterm runjb rvstr rx_continue
syn keyword dblFunction rx_debug_init rx_debug_start rx_get_errinfo rx_get_haltinfo
syn keyword dblFunction rx_rmt_endian rx_rmt_intsize rx_rmt_os rx_rmt_sysinfo rx_rmt_timout
syn keyword dblFunction rx_rxsubr rx_setrmtfnc rx_shutdown_remote rx_start_remote rxsubr
syn keyword dblFunction s_bld s_detab s_match s_parse s_wrap sdms_sel serial
syn keyword dblFunction set_priority_class set_xfpl_timeout setctl setdfn setlog shell size
syn keyword dblFunction spawn ss_accept ss_bind ss_clonesocket ss_close ss_connect ss_fatal
syn keyword dblFunction ss_gethostbyaddr ss_gethostbyname ss_gethostname ss_getpeername
syn keyword dblFunction ss_getserdblyname ss_getserdblyport ss_getsockname ss_htonl ss_htons
syn keyword dblFunction ss_inet_addr ss_inet_ntoa ss_listen ss_ntohl ss_ntohs ss_recv
syn keyword dblFunction ss_recdbluf ss_recvfrom ss_select ss_send ss_sendbuf ss_sendto
syn keyword dblFunction ss_socket ssc_bind ssc_blob ssc_cancel ssc_close ssc_cmd ssc_commit
syn keyword dblFunction ssc_connect ssc_define ssc_descsql ssc_execio ssc_execute ssc_getdbid
syn keyword dblFunction ssc_getemsg ssc_indicator ssc_init ssc_largecol ssc_mapmsg ssc_move
syn keyword dblFunction ssc_open ssc_option ssc_rebind ssc_release ssc_rollback ssc_sclose
syn keyword dblFunction ssc_sqllink ssc_strdef string stty success syn_atexit syn_chartostr
syn keyword dblFunction syn_escape_handle syn_freechn syn_getdir syn_getstate
syn keyword dblFunction syn_getwinobject syn_netparent syn_reportevent syn_setdir
syn keyword dblFunction syn_setstate syn_setwinobject syn_state syn_syserrtxt syn_uname
syn keyword dblFunction syn_unescape_handle syn_xname syncrypt synescape synhttp synsock
syn keyword dblFunction syserr sysid tb_button tb_info tb_toolbar tflsh time timezone tnmbr
syn keyword dblFunction traceback trclvls trim trimz true trunc truncate tt_name_to_number
syn keyword dblFunction tt_number_to_name ttbrdcst ttchar ttflgs ttmbx ttname ttsts typeof
syn keyword dblFunction unpak unsigned val variant versn vmcmd vmmsg vms w_area w_brdr
syn keyword dblFunction w_caption w_disp w_exit w_flds w_info w_init w_proc w_restore w_save
syn keyword dblFunction w_updt w_winuse wait win_proc win_stop wkday wpr_device wpr_execute
syn keyword dblFunction wpr_getdevice wpr_info wpr_print wpr_setdevice xaddr xargs xfpl_log
syn keyword dblFunction xfpl_regcleanup xstat xsubr xtrnl xyzzy zoned

" Synergy/DE UI Toolkit routines
syn keyword dblFunction ax_input ax_register ax_timeout ax_tkcid ax_tkctlid ax_tkdel
syn keyword dblFunction ax_tkevent_close ax_tkevent_left_click ax_tkevent_left_dblclk
syn keyword dblFunction ax_tkevent_maximize ax_tkevent_middle_click ax_tkevent_middle_dblclk
syn keyword dblFunction ax_tkevent_minimize ax_tkevent_move ax_tkevent_restore
syn keyword dblFunction ax_tkevent_right_click ax_tkevent_right_dblclk ax_tkevent_scroll
syn keyword dblFunction ax_tkevent_size ax_tksingle ax_tkwin ax_wantskey b_button b_buttonset
syn keyword dblFunction b_disable b_enable b_info c_container c_methax c_methctr c_methinp
syn keyword dblFunction c_methlst c_methnet c_methnop c_methsel c_methts c_methtxt c_process
syn keyword dblFunction dcnvrt_error dotnet_tkinput dtk_null e_enter e_exit e_font e_info
syn keyword dblFunction e_method e_sect e_state eu_appevent fs_append fs_close fs_init
syn keyword dblFunction fs_open fs_param fs_quit fs_read fs_write grid_create i_activate
syn keyword dblFunction i_context i_ctrcontext i_disable i_display i_dsparea i_dspfld
syn keyword dblFunction i_enable i_flddim i_fldinf i_fldmod i_fldprec i_fldsel i_fldsize
syn keyword dblFunction i_fldstrpos i_force i_frames i_getfld i_getstring i_info i_init
syn keyword dblFunction i_inpfld i_inpfld_p i_input i_ldinp i_next i_prompt i_putfld
syn keyword dblFunction i_readonly i_setdel i_setinf i_snapshot i_txtpos i_user ib_button
syn keyword dblFunction ib_buttonset ib_dictionary ib_dsp_area ib_end ib_field ib_input
syn keyword dblFunction ib_local ib_paint ib_rps_structure ib_set ib_stdbuttons ib_structure
syn keyword dblFunction inpdbg l_border l_button l_buttonset l_buttonstate l_chr l_chr_p
syn keyword dblFunction l_class l_classinfo l_create l_ctrcontext l_data l_delete l_disable
syn keyword dblFunction l_enable l_findspec l_findwnd l_global l_icon l_inpfld l_inpfld_p
syn keyword dblFunction l_input l_input_p l_method l_next l_place l_process l_queue l_remove
syn keyword dblFunction l_resize l_restart l_returnkey l_sect l_sectdraw l_select l_selstyle
syn keyword dblFunction l_state l_status l_user l_view ll_close ll_open ll_process ll_synch
syn keyword dblFunction lstscr m_column m_defcol m_disable m_enable m_info m_keytxt m_ldcol
syn keyword dblFunction m_popup m_process m_reset m_rplent m_scinfo m_signal m_text m_user
syn keyword dblFunction mb_blank mb_column mb_end mb_entry mb_line mb_list mb_text mnuscr
syn keyword dblFunction mouse_constructor mouse_identify o_txtrd ps_ldopt s_selbld s_seldflt
syn keyword dblFunction s_select s_selectcb s_selinf s_selld s_updatecb scr_closelibrary
syn keyword dblFunction scr_errorcount scr_errorout scr_ibuerror scr_openlibrary scr_process
syn keyword dblFunction scr_save scrcnvrt_error selscr t_edit t_edit_p t_getlin t_puttxt
syn keyword dblFunction t_setup t_txtfnd t_txtrd t_txtwrt t_view t_view_p t_wndrd t_wndwrt
syn keyword dblFunction tb_tkcreate tb_tkdel tb_tkglobal tb_tklog tkp_century tkp_chkfld
syn keyword dblFunction tkp_dspfld tkp_edtdsp tkp_help tkp_scripterr tkp_utils ts_process
syn keyword dblFunction ts_tabset u_abort u_abort_msg u_about u_addfil u_appclose u_bar
syn keyword dblFunction u_beep u_bldmsg u_charsb u_checkreqversion u_checkversion u_chkwnd
syn keyword dblFunction u_chninf u_chntyp u_chr u_close u_cmdlinopt u_cnvdat u_createsb
syn keyword dblFunction u_ctrcontext u_dcddat u_dcdtim u_delwnd u_dialog u_editkeys
syn keyword dblFunction u_editrend u_enumwnds u_fatal u_fileparse u_fileselect u_finish
syn keyword dblFunction u_flash u_fld u_fmtdat u_fmttim u_fndfil u_ftl_1 u_ftl_2 u_gblchn
syn keyword dblFunction u_gblwnd u_getchn u_getfilename u_gettxt u_getwnderr u_helprend
syn keyword dblFunction u_htmlhelp u_icon u_initchn u_initrend u_last_werr u_ldrend u_ldwnd
syn keyword dblFunction u_logwnd u_message u_minversion u_modrend u_months u_msg u_msgbox
syn keyword dblFunction u_msgbox_p u_msgstyle u_msgtext u_open u_parseversion u_popup
syn keyword dblFunction u_printquery u_printsetup u_quitchn u_redraw u_rejustify u_rend
syn keyword dblFunction u_reqversion u_resize u_resizesb u_restore u_rlschn u_rmvfil
syn keyword dblFunction u_rstrend u_save u_savelog u_savesettings u_savrend u_selpalette
syn keyword dblFunction u_setrnd u_start u_storend u_text u_unquote u_update u_updatesb
syn keyword dblFunction u_version u_wait u_wincells u_wincolor u_window u_winhelp u_wininfo
syn keyword dblFunction u_winmetrics u_wndevents u_wndfont u_wndstyle u_wndtype usr_newview
syn keyword dblFunction usr_start

" Synergy/DE XML API routines
syn keyword dblFunction cmdline_add cmdline_count cmdline_create cmdline_delete cmdline_error
syn keyword dblFunction cmdline_getpos cmdline_item cmdline_parsenext cmdline_parsevalue
syn keyword dblFunction cmdline_read cmdline_removequotes elem_assist parser_readcdata
syn keyword dblFunction xml_attr_addref xml_attr_copy xml_attr_create xml_attr_delete
syn keyword dblFunction xml_attr_destroy xml_attr_getname xml_attr_getrawvalue
syn keyword dblFunction xml_attr_getvalue xml_attr_getvaluehandle xml_attr_release
syn keyword dblFunction xml_attr_setname xml_attr_setvalue xml_attr_setvaluehandle
syn keyword dblFunction xml_attr_tostring xml_attrlist_add xml_attrlist_addlist
syn keyword dblFunction xml_attrlist_clear xml_attrlist_copy xml_attrlist_count
syn keyword dblFunction xml_attrlist_create xml_attrlist_delete xml_attrlist_find
syn keyword dblFunction xml_attrlist_index xml_attrlist_insert xml_attrlist_item
syn keyword dblFunction xml_attrlist_remove xml_dec_copy xml_dec_create xml_dec_delete
syn keyword dblFunction xml_declist_add xml_declist_clear xml_declist_copy xml_declist_create
syn keyword dblFunction xml_declist_delete xml_declist_find xml_declist_index
syn keyword dblFunction xml_declist_item xml_declist_lowadd xml_declist_remove
syn keyword dblFunction xml_declist_update xml_doc_adddeclaration xml_doc_copy xml_doc_create
syn keyword dblFunction xml_doc_delete xml_doc_getdeclaration xml_doc_getroot
syn keyword dblFunction xml_doc_removedeclaration xml_doc_setcomment xml_doc_setdeclaration
syn keyword dblFunction xml_doc_setroot xml_doc_tofile xml_doc_tostring
syn keyword dblFunction xml_elem_addattributes xml_elem_addchild xml_elem_addchildren
syn keyword dblFunction xml_elem_addref xml_elem_appendtext xml_elem_attributes
syn keyword dblFunction xml_elem_children xml_elem_copy xml_elem_create
syn keyword dblFunction xml_elem_create_string xml_elem_delete xml_elem_destroy
syn keyword dblFunction xml_elem_findattribute xml_elem_getattribute xml_elem_getattributenum
syn keyword dblFunction xml_elem_getelementsbyname xml_elem_getname xml_elem_getrawlen
syn keyword dblFunction xml_elem_gettext xml_elem_gettexthandle xml_elem_indentappend
syn keyword dblFunction xml_elem_release xml_elem_removeattribute xml_elem_removeattributes
syn keyword dblFunction xml_elem_removechild xml_elem_removechildren xml_elem_setattribute
syn keyword dblFunction xml_elem_setname xml_elem_settext xml_elem_settexthandle
syn keyword dblFunction xml_elem_tostring xml_elemlist_add xml_elemlist_addlist
syn keyword dblFunction xml_elemlist_clear xml_elemlist_copy xml_elemlist_count
syn keyword dblFunction xml_elemlist_create xml_elemlist_delete xml_elemlist_find
syn keyword dblFunction xml_elemlist_findtag xml_elemlist_index xml_elemlist_insert
syn keyword dblFunction xml_elemlist_item xml_elemlist_remove xml_option xml_parser_create
syn keyword dblFunction xml_parser_delete xml_parser_error xml_parser_parsefile
syn keyword dblFunction xml_parser_parsestring xml_parser_readattribute
syn keyword dblFunction xml_parser_readcomment xml_parser_readdeclaration xml_parser_readnode
syn keyword dblFunction xml_parser_readtag xml_parser_setlasterror xml_parser_substr
syn keyword dblFunction xml_parser_trimleading xml_parser_trimline xml_string_append
syn keyword dblFunction xml_string_appenddyn xml_string_appendhandle xml_string_clear
syn keyword dblFunction xml_string_copy xml_string_create xml_string_delete xml_string_find
syn keyword dblFunction xml_string_gethandle xml_string_getsize xml_string_skipblank
syn keyword dblFunction xml_string_substr xml_string_tofile xmlmem_appendstring xmlmem_string

" Synergy/DE Repository Subroutine Library
syn keyword dblFunction dd_alias dd_control dd_enum dd_exit dd_field dd_file dd_filespec
syn keyword dblFunction dd_format dd_init dd_key dd_name dd_relation dd_struct dd_tag
syn keyword dblFunction dd_template ddctl_init


" Needs work
syn keyword dblTypes Boolean Byte Char Date Decimal Double
syn keyword dblTypes Integer Long Object Short Single String
syn keyword dblTypes UInteger ULong UShort SByte

" Needs Work
syn match dblOperator "[()+.,\-/*=&]"
syn match dblOperator "[<>]=\="
syn match dblOperator "<>"
syn match dblOperator "\s\+_$"

" syn keyword dblOperator - | |= ! != # ## & &= && * *= / /= || + += < <= -= = == > >=
syn keyword dblOperator .and. .band. .bnand. .bnot. .bor. .bxor. .eq. .eqs. .equ. .ge. .ges.
syn keyword dblOperator .geu. .gt. .gts. .gtu. .is. .le. .les. .leu. .lt. .lts. .ltu. .ne.
syn keyword dblOperator .nes. .neu. .not. .or. .xor.

syn keyword dblConst True False Nothing


syn keyword dblSpecial .align .array .define .else .end .endc .endregion .function .ident .if
syn keyword dblSpecial .ifdef .iff .ifndef .ift .iftf .include .list .main .nodebug .nolist
syn keyword dblSpecial .noproto .page .pragma .proc .proto .region .start .subroutine .title
syn keyword dblSpecial .undefine main proc subroutine function endmain endsubroutine
syn keyword dblSpecial endfunction

syn keyword dblSpecial Me MyBase MyClass

syn keyword dblTodo contained TODO

"integer number, or floating point number without a dot.
syn match dblNumber "\<\d\+\>"
"floating point number, with dot
syn match dblNumber "\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match dblNumber "\.\d\+\>"

" String and Character contstants
syn region dblString start=+"+ end=+"+
syn region dblString start=+'+ end=+'+

syn region dblComment start=";" end="$" contains=dblTodo

syn region dblPreCondit start="^#If\s" end="Then$"
syn region dblPreCondit start="^#ElseIf\s" end="Then$"
syn match dblPreCondit "^#Else$"
syn match dblPreCondit "^#End If$"

syn region dblPreCondit start="^#Region\s\+\"" end="\"$"
syn match dblPreCondit "^#End Region$"

syn region dblPreCondit start="^#ExternalSource(" end=")$"
syn match dblPreCondit "^#End ExternalSource$"

syn region dblPreCondit start="^#Const\s" end="$"

syn region dblLineNumber	start="^\d" end="\s"

syn match dblTypeSpecifier "[a-zA-Z0-9][\$%&!#]"ms=s+1

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_dbl_syntax_inits")
  if version < 508
    let did_dbl_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink dblLineNumber Comment
  HiLink dblNumber Number
  HiLink dblConst Constant
  HiLink dblError Error
  HiLink dblStatement Statement
  HiLink dblString String
  HiLink dblComment Comment
  HiLink dblTodo Todo
  HiLink dblFunction Identifier
  HiLink dblMethods PreProc
  HiLink dblPreCondit PreCondit
  HiLink dblSpecial Special
  HiLink dblTypeSpecifier Type
  HiLink dblTypes Type
  HiLink dblOperator Operator

  delcommand HiLink
endif

let b:match_words = '\<Namespace\>:\<EndNamespace\>'
      \ . ',\<Class\>:\<EndClass\>'
      \ . ',\<Property\>:\<EndProperty\>'
      \ . ',\<Enum\>:\<EndEnum\>'
      \ . ',\<Function\>:\<EndFunction\>'
      \ . ',\<Subroutine\>:\<EndSubroutine\>'
      \ . ',\<Begin\>:\<End\>'
      \ . ',\<Method\>:\<EndMethod\>'
      \ . ',\<Case\>:\<EndCase\>'
      \ . ',\<Try\>:\<EndTry\>'
      
let b:current_syntax = "dbl"

