" Vim syntax file
" Language:		LuaJIT DynASM
" Homepage:		http://github.com/Buristan/vim-syntax-dynasm
" Maintainer:		Sergey Kaplun <sergey_v_kaplun@mail.ru>
" Latest Revision:	07 Febuary 2021
" Version:		0.01
" License:		Redistribute under the same terms as Vim itself
" Purpose:		LuaJIT Extension for DynASM Syntax Highlighting

" Note: It may be separate file inside after/syntax/include/.
" But anyway LuaJIT is the most significant dynasm user.

let s:ft_regexp = '\(vm_\)\@<=.\+\(\.'.expand("%:e").'\)\@='
let g:DascLuaJITFile = matchstr(expand("%:t"), s:ft_regexp)
unlet s:ft_regexp
if len(g:DascLuaJITFile) == 0
  " Not LuaJIT vm file.
  finish
endif

if !exists("dasc_luajit_no_ts8")
  " Simplify reading.
  setlocal tabstop=8
endif

" Extend these code sections as standard preproc macro.
syn match	ljDascPreproc		"\.\(code_sub\|code_op\)\>" contained

" Constants {{{

" Types.
syn keyword	ljDascConst		LJ_TNIL LJ_TFALSE LJ_TTRUE LJ_TLIGHTUD LJ_TSTR LJ_TUPVAL LJ_TTHREAD LJ_TPROTO LJ_TFUNC LJ_TTRACE LJ_TCDATA LJ_TTAB LJ_TUDATA LJ_TNUMX contained
syn keyword	ljDascConst		LJ_TISNUM LJ_TISTRUECOND LJ_TISPRI LJ_TISGCV LJ_TISTABUD contained
syn keyword	ljDascConst		LJ_GCVMASK contained

" Prototypes.
syn keyword	ljDascConst		PROTO_CHILD PROTO_VARARG PROTO_FFI PROTO_NOJIT PROTO_ILOOP PROTO_HAS_RETURN PROTO_FIXUP_RETURN PROTO_CLCOUNT PROTO_CLC_BITS PROTO_CLC_POLY PROTO_UV_LOCAL PROTO_UV_IMMUTABLE contained

" Fast functions.
syn keyword	ljDascConst		FF_LUA FF_C contained

" VM states.
syn keyword	ljDascConst		LJ_VMST_INTERP LJ_VMST_C LJ_VMST_GC LJ_VMST_EXIT LJ_VMST_RECORD LJ_VMST_OPT LJ_VMST_ASM LJ_VMST__MAX contained
" For st_vmstate macro.
syn keyword	ljDascConst		INTERP C GC EXIT RECORD OPT ASM __MAX contained
" LuaVela and Tarantool extensions.
if g:DascLuaJITFile ==# 'x86' && !(exists("dasc_luajit_no_extension_luavela")
   \&& exists("dasc_luajit_no_extension_tarantool"))
  syn keyword	ljDascConst		LJ_VMST_LFUNC LJ_VMST_FFUNC LJ_VMST_CFUNC contained
  " For st_vmstate macro. Avoid overlaping with 'CFUNC:'.
  syn match	ljDascConst		"\<\(LFUNC\|FFUNC\|CFUNC\)\>.\@!" contained
endif

" Metamethods.

" FFI metamethod.
syn keyword	ljDascConst		MM_new contained
" LJ_52 || LJ_HASFFI metamethods.
syn keyword	ljDascConst		MM_pairs MM_ipairs contained
" Usual metamethods.
syn keyword	ljDascConst		MM_index MM_newindex MM_gc MM_mode MM_eq MM_len MM_lt MM_le MM_concat MM_call MM_add MM_sub MM_mul MM_div MM_mod MM_pow MM_unm MM_metatable MM_tostring MM__MAX MM____ MM_FAST contained

" GC roots.
syn keyword	ljDascConst		GCROOT_MMNAME GCROOT_MMNAME_LAST GCROOT_BASEMT GCROOT_BASEMT_NUM GCROOT_IO_INPUT GCROOT_IO_OUTPUT GCROOT_MAX contained

" Hooks.
syn keyword	ljDascConst		HOOK_EVENTMASK HOOK_ACTIVE HOOK_ACTIVE_SHIFT HOOK_VMEVENT HOOK_GC HOOK_PROFILE contained

" GC colors.
syn keyword	ljDascConst		LJ_GC_WHITE0 LJ_GC_WHITE1 LJ_GC_BLACK contained
" GC Flags.
syn keyword	ljDascConst		LJ_GC_FINALIZED LJ_GC_WEAKKEY LJ_GC_WEAKVAL LJ_GC_CDATA_FIN LJ_GC_FIXED LJ_GC_SFIXED LJ_GC_WHITES LJ_GC_COLORS LJ_GC_WEAK contained
" LuaVela extensions.
if g:DascLuaJITFile ==# 'x86' && !exists("dasc_luajit_no_extension_luavela")
  syn keyword	ljDascConst		LJ_GC_CDATA_VAR UJ_GCO_TMPMARK UJ_GCO_IMMUTABLE UJ_GCO_SEALED contained
endif

" Frames.
syn keyword	ljDascConst		FRAME_LUA FRAME_C FRAME_CONT FRAME_VARG FRAME_LUAP FRAME_CP FRAME_PCALL FRAME_PCALLH FRAME_TYPE FRAME_P FRAME_TYPEP contained
" Ofsets.
syn keyword	ljDascConst		CFRAME_OFS_PREV CFRAME_OFS_PC CFRAME_OFS_L CFRAME_OFS_ERRF CFRAME_OFS_NRES CFRAME_OFS_MULTRES CFRAME_SIZE CFRAME_SIZE_JIT CFRAME_SHIFT_MULTRES contained

syn keyword	ljDascConst		CFRAME_RESUME CFRAME_UNWIND_FF CFRAME_RAWMASK contained

" lua.h defines.
" Indexes and statuses.
syn keyword	ljDascConst		LUA_MULTRET LUA_REGISTRYINDEX LUA_ENVIRONINDEX LUA_GLOBALSINDEX LUA_OK LUA_YIELD LUA_ERRRUN LUA_ERRSYNTAX LUA_ERRMEM LUA_ERRERR contained
" Basic types.
syn keyword	ljDascConst		LUA_TNONE LUA_TNIL LUA_TBOOLEAN LUA_TLIGHTUSERDATA LUA_TNUMBER LUA_TSTRING LUA_TTABLE LUA_TFUNCTION LUA_TUSERDATA LUA_TTHREAD contained
syn keyword	ljDascConst		LUA_MINSTACK contained
" GC options.
syn keyword	ljDascConst		LUA_GCSTOP LUA_GCRESTART LUA_GCCOLLECT LUA_GCCOUNT LUA_GCCOUNTB LUA_GCSTEP LUA_GCSETPAUSE LUA_GCSETSTEPMUL LUA_GCISRUNNING contained

" Event codes.
syn keyword	ljDascConst		LUA_HOOKCALL LUA_HOOKRET LUA_HOOKLINE LUA_HOOKCOUNT LUA_HOOKTAILRET contained

" Event masks.
syn keyword	ljDascConst		LUA_MASKCALL LUA_MASKRET LUA_MASKLINE LUA_MASKCOUNT contained

" LuaVela extension defines in dasc.
if g:DascLuaJITFile ==# 'x86' && !exists("dasc_luajit_no_extension_luavela")
  syn keyword	ljDascConst		TVS TVS_X2 TVB TVB_X2 contained
  syn keyword	ljDascConst		LOOP_COUNTER_OFFSET FORL_COUNTER_OFFSET ITERL_COUNTER_OFFSET PROLOGUE_COUNTER_OFFSET HOTCNT_COUNTER_OFFSET contained
endif
" end of Constants }}}

if !exists("dasc_luajit_no_extend_todo")
  " Contains in comments.
  syn keyword	dascTODO		Caveat Mnemo NB NYI NOTE Note NOBARRIER contained
endif

" Specific macroses.
if !exists("dasc_luajit_no_hl_macro")
  " For .ffunc highlighting.
  setlocal iskeyword+=.

  if g:DascLuaJITFile ==# 'x86' || g:DascLuaJITFile ==# 'x64'
    " x86 and x64 specific {{{
    " Vanilla LuaJIT and Tarantool.
    syn keyword	ljDascMacro		barrierback branchPC checkint checknum checkstr checktab checktp coroutine_resume_wrap ffgccheck ffstring_op \.ffunc \.ffunc_1 \.ffunc_2 \.ffunc_bit \.ffunc_bit_op \.ffunc_bit_sh fpop1 hotcall hotloop ins_A ins_AB_ ins_ABC ins_A_C ins_AD ins_AJ ins_AND ins_arith ins_arithdn ins_arithpost ins_arithpre ins_call ins_callt ins_next_ ins_NEXT jmp_comp math_extern math_extern2 math_minmax math_round restoreregs saveregs saveregs_ set_vmstate sseconst_1 sseconst_2p52 sseconst_abs sseconst_hi sseconst_m1 sseconst_sign sseconst_tobit vm_round contained
    " Matched macro and label name.
    syn match	ljDascMacro		">\@<!\<ins_next\>" contained

    " x86 specific.
    syn keyword	ljDascMacro		fcomparepp \.ffunc_nnr \.ffunc_nnsse \.ffunc_nsse contained
    " x64 specific.

    syn keyword	ljDascMacro		checkfunc checkinttp checknumber checknumtp checknumx checktp_nc checktptp cleartp \.ffunc_n \.ffunc_nn mov_false mov_true setint settp contained

    if g:DascLuaJITFile ==# 'x86' && !exists("dasc_luajit_no_extension_luavela")
      " LuaVela specific.
      syn keyword	ljDascMacro	checkfunc _checkimmutable checkimmutable_bc checkimmutable_ff checktag checktimeout fdup ffstring_case .ffunc_n .ffunc_nn ftsz2offs gco2s gco2sv gettag i2func i2gcr __i2gcr i2tvp inc_PC _ins_next iuva2r kitva2r math_extern_call movtv n2tvp n2tvp_r newtab2tv_ra newtab_gccheck num2s num2sv ptrdiff2nargs redispatch_static restore_base restore_PC restore_RA restore_RB restore_RC restore_RD restore_vmstate save_PC save_vmstate setnil settag setup_dispatch setup_kbase set_vmstate_cfunc set_vmstate_ffunc set_vmstate_lfunc sync_stack tbl_check_mm tbl_find_key contained
    endif

    if !exists("dasc_luajit_no_extension_tarantool")
      " Tarantool specific.
      syn keyword	ljDascMacro	restore_vmstate save_vmstate contained
    endif
    " end of x86 and x64 specific }}}
  elseif g:DascLuaJITFile ==# 'mips' || g:DascLuaJITFile ==# 'mips64'
    " mips and mips64 specific {{{
    syn keyword	ljDascMacro		barrierback bc_comp branch_RD call_extern call_intern coroutine_resume_wrap decode_OP1 decode_RA8a decode_RA8b decode_RB8a decode_RB8b decode_RD4b decode_RD8a decode_RD8b decode_RDtoRC8 ffgccheck ffstring_op \.ffunc \.ffunc_1 \.ffunc_2 \.ffunc_bit \.ffunc_bit_op \.ffunc_bit_sh \.ffunc_n \.ffunc_nn fpmod \.FPU hotcall hotcheck hotloop ins_arith ins_arithpre ins_call ins_callt ins_next_ ins_NEXT ins_next1 ins_NEXT1 ins_next2 ins_NEXT2 jmp_extern li_vmstate load_got math_extern math_extern2 math_minmax math_round NYI restoreregs_ret saveregs savex_ sfi2d sfmin_max sfpmod st_vmstate vm_round vm_round_hf contained
    " Matched macro and label name.
    syn match	ljDascMacro		">\@<!\<ins_next\>" contained

    if g:DascLuaJITFile ==# 'mips'
      " mips specific.
      syn keyword	ljDascMacro	decode_OP4a decode_OP4b decode_RC4a decode_RC4b contained
    endif

    if g:DascLuaJITFile ==# 'mips64'
      " mips64 specific.
      syn keyword	ljDascMacro	checkfunc checkint checknum checkstr checktab checktp cleartp decode_OP8a decode_OP8b decode_RC8a decode_RC8b gettp mov_false mov_true settp contained
    endif
    " end of mips and mips64 specific }}}
  elseif g:DascLuaJITFile ==# 'arm' || g:DascLuaJITFile ==# 'arm64' "aarch64
    " arm and arm64 specific {{{
    syn keyword	ljDascMacro		barrierback checkfunc checkstr checktab checktp coroutine_resume_wrap decode_RD ffgccheck ffstring_op \.ffunc \.ffunc_1 \.ffunc_2 \.ffunc_bit \.ffunc_bit_op \.ffunc_bit_sh \.ffunc_n \.ffunc_nn hotcall hotcheck hotloop ins_arithcheck_int ins_arithcheck_num ins_arithdn ins_arithfallback ins_arithfp ins_call ins_callt ins_next_ ins_NEXT math_extern math_extern2 math_minmax math_round mv_vmstate NYI saveregs st_vmstate contained
    " Matched macro and label name.
    syn match	ljDascMacro		">\@<!\<ins_next\>" contained

    if g:DascLuaJITFile ==# 'arm'
      " arm specific.
      syn keyword	ljDascMacro	checktpeq checktpne decode_OP decode_RA8 decode_RB8 decode_RC8 \.ffunc_d \.ffunc_dd ins_arithcheck ins_arithpost_fpu ins_arithpre ins_arithpre_fpu ins_next1 ins_NEXT1 ins_next2 ins_NEXT2 ins_next3 ins_NEXT3 \.IOS restoreregs_ret vm_round vm_trunc contained
    endif

    if g:DascLuaJITFile ==# 'arm64'
      " arm64 specific.
      syn keyword	ljDascMacro	checkint checknum checknumber decode_RA decode_RB decode_RC decode_RC8RD ins_arithcheck_nzdiv ins_arithhead ins_arithload ins_arithmod mov_false mov_true rest_ restoreregs save_ savex_ contained
    endif
    " end of arm and arm64 specific }}}
  elseif g:DascLuaJITFile ==# 'ppc'
    " ppc specific {{{
    syn keyword	ljDascMacro	addo32\. andix\. barrierback blex branch_RD checkfunc checknil checknum checkov checkstr checktab clrso coroutine_resume_wrap decode_OP1 decode_OP4 decode_OP8 decode_OPP decode_RA8 decode_RB8 decode_RC8 decode_RD4 decode_RD8 ffgccheck ffstring_op \.ffunc \.ffunc_1 \.ffunc_2 \.ffunc_bit \.ffunc_bit_op \.ffunc_bit_sh \.ffunc_n \.ffunc_nn fpmod \.FPU \.gpr64 hotcall hotcheck hotloop ins_arith ins_arithdn ins_arithfallback ins_arithfp ins_arithpre ins_call ins_callt ins_next_ ins_NEXT ins_next1 ins_NEXT1 ins_next2 ins_NEXT2 intmod li_vmstate lp lpx math_extern math_extern2 math_minmax math_round NYI rest_ restoreregs save_ saveregs savex_ sfi2d sfpmod stp st_vmstate subo32\. \.toc \.tocenv toint tonum_i tonum_u contained
    " Matched macro and label name.
    syn match	ljDascMacro		">\@<!\<ins_next\>" contained
    " end of ppc specific }}}
  endif
endif " !exists(dasc_luajit_no_hl_macro)

" Registers.
if !exists("dasc_luajit_no_hl_registers")
  if g:DascLuaJITFile ==# 'x86' || g:DascLuaJITFile ==# 'x64'
    " Intel and AMD registers coloring {{{
    syn keyword	ljDascHostByteReg	al ah bl bh cl ch dl dh r0b r1b r2b r3b r4b r5b r6b r7b r8b r9b r10b r11b r12b r13b r14b r15b contained
    syn keyword	ljDascHostWordReg	ax bx cx dx bp sp di si r0w r1w r2w r3w r4w r5w r6w r7w r8w r9w r10w r11w r12w r13w r14w r15w contained
    syn keyword	ljDascHostDwordReg	eax ebx ecx edx ebp esp edi esi r0d r1d r2d r3d r4d r5d r6d r7d r8d r9d r10d r11d r12d r13d r14d r15d contained
    syn keyword	ljDascHostQwordReg	rax rbx rcx rdx rbp rsp rdi rsi r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 contained
    syn keyword	ljDascHostFPReg		xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15 contained

    syn keyword	ljDascGuestByteReg	RAH RAL RCH RCL RDL CRETb contained
    syn keyword	ljDascGuestWordReg	RCW RDW XCHGw contained
    syn keyword	ljDascGuestDwordReg	PCd OP XCHGd AUX1d AUX2d CARG1d CARG2d CARG3d CARG4d CARG5d CARG6d FCARG1 FCARG2 CRETd TMPRd ITYPEd contained
    syn keyword	ljDascGuestQwordReg	XCHG AUX1 AUX2 CARG1 CARG2 CARG3 CARG4 CARG5 CARG6 CRET BASE TMPR ITYPE contained

    if g:DascLuaJITFile ==# 'x86'
      syn keyword	ljDascGuestDwordReg	BASE KBASE PC DISPATCH RA RB RC RD contained
      syn keyword	ljDascGuestDQwordReg	KBASEa PCa RAa RBa RCa RDa contained

      if exists("dasc_luajit_luavela_regcol")
        syn keyword	ljDascGuestWordReg	AUX1w contained
        syn keyword	ljDascGuestDwordReg	DISPATCHd KBASEd contained
        syn keyword	ljDascGuestQwordReg	DISPATCH BASE KBASE PC RAa RBa RCa RDa contained
        syn keyword	ljDascGuestFPReg	CARG1f contained
      endif
    else
      syn keyword	ljDascGuestDwordReg	KBASEd DISPATCHd RAd RBd RCd RDd contained
      syn keyword	ljDascGuestQwordReg	BASE KBASE PC DISPATCH RA RB RC RD contained
    endif

    syn cluster	dascExtra		add=ljDascGuestByteReg,ljDascGuestWordReg,ljDascGuestDwordReg,ljDascGuestQwordReg,ljDascGuestFPReg,ljDascHostByteReg,ljDascHostWordReg,ljDascHostDwordReg,ljDascHostQwordReg,ljDascGuestDQwordReg,ljDascHostFPReg

    hi def ljDascHostByteReg		term=bold cterm=bold ctermfg=Black ctermbg=Cyan
    hi def ljDascHostWordReg		term=bold cterm=bold ctermfg=Black ctermbg=Magenta
    hi def ljDascHostDwordReg		term=bold cterm=bold ctermfg=Black ctermbg=Green
    hi def ljDascHostQwordReg		term=bold cterm=bold ctermfg=Black ctermbg=Red
    hi def ljDascHostFPReg		term=bold cterm=bold ctermfg=Black ctermbg=Blue

    hi def ljDascGuestByteReg		term=bold cterm=bold ctermfg=Cyan ctermbg=NONE
    hi def ljDascGuestWordReg		term=bold cterm=bold ctermfg=Magenta ctermbg=NONE
    hi def ljDascGuestDwordReg		term=bold cterm=bold ctermfg=Green ctermbg=NONE
    hi def ljDascGuestQwordReg		term=bold cterm=bold ctermfg=Red ctermbg=NONE
    hi def ljDascGuestDQwordReg		term=bold cterm=bold ctermfg=Yellow ctermbg=NONE
    hi def ljDascGuestFPReg		term=bold cterm=bold ctermfg=Blue ctermbg=NONE
    " end of Intel and AMD registers coloring }}}
  elseif g:DascLuaJITFile ==# 'mips' || g:DascLuaJITFile ==# 'mips64'
    " mips and mips64 registers coloring {{{
    let b:i = 0
    while b:i < 32
      execute 'syn keyword ljDascHostGPReg r'.b:i.' contained'
      execute 'syn keyword ljDascHostFPReg f'.b:i.' contained'
      let b:i += 1
    endwhile
    unlet b:i
    syn keyword	ljDascHostGPReg		sp gp contained

    syn keyword	ljDascGuestGPReg	BASE KBASE PC DISPATCH LREG MULTRES JGL TISNUM TISNIL RA RB RC RD INS AT TMP0 TMP1 TMP2 TMP3 CFUNCADDR CARG1 CARG2 CARG3 CARG4 CRET1 CRET2 contained
    syn keyword	ljDascGuestFPReg	TOBIT FARG1 FARG2 FRET1 FRET2 contained
    if g:DascLuaJITFile ==# 'mips'
      syn keyword	ljDascGuestHI	SFRETHI SFARG1HI SFARG2HI contained
      syn keyword	ljDascGuestLO	SFRETLO SFARG1LO SFARG2LO contained
    else
      syn keyword	ljDascGuestFPReg FTMP0 FTMP1 FTMP2 FARG3 FARG4 FARG5 FARG6 FARG7 FARG8 contained
      syn keyword	ljDascGuestGPReg CARG5 CARG6 CARG7 CARG8 contained
    endif

    syn cluster	dascExtra		add=ljDascHostFPReg,ljDascHostGPReg,ljDascGuestHI,ljDascGuestLO,ljDascGuestGPReg,ljDascGuestFPReg
    hi def ljDascHostFPReg		term=bold cterm=bold ctermfg=Black ctermbg=Blue

    if g:DascLuaJITFile ==# 'mips'
      hi def ljDascHostGPReg		term=bold cterm=bold ctermfg=Black ctermbg=Green
      hi def ljDascGuestHI		term=bold cterm=bold ctermfg=Magenta ctermbg=NONE
      hi def ljDascGuestLO		term=bold cterm=bold ctermfg=Yellow ctermbg=NONE
      hi def ljDascGuestGPReg		term=bold cterm=bold ctermfg=Green ctermbg=NONE
    else
      hi def ljDascHostGPReg		term=bold cterm=bold ctermfg=Black ctermbg=Red
      hi def ljDascGuestGPReg		term=bold cterm=bold ctermfg=Red ctermbg=NONE
    endif
    hi def ljDascGuestFPReg		term=bold cterm=bold ctermfg=Blue ctermbg=NONE
    " end of mips and mips64 registers coloring }}}
  elseif g:DascLuaJITFile ==# 'arm'
    " arm registers coloring {{{
    syn keyword	ljDascGuestGPReg	MASKR8 KBASE PC DISPATCH LREG BASE RA RC RB OP INS CARG1 CARG2 CARG3 CARG4 CARG12 CARG34 CRET1 CRET2
    let b:i = 0
    while b:i < 32
      execute 'syn keyword ljDascHostGPReg r'.b:i.' contained'
      execute 'syn keyword ljDascHostFPReg s'.b:i.' contained'
      let b:i += 1
    endwhile
    let b:i = 0
    while b:i < 16
      execute 'syn keyword ljDascHostFPReg d'.b:i.' contained'
      let b:i += 1
    endwhile
    unlet b:i
    syn keyword	ljDascHostGPReg		sp lr contained

    syn keyword	ljDascHostFPReg		FTMP0 FTMP1 FTMP2 FARG3 FARG4 FARG5 FARG6 FARG7 FARG8 contained

    syn cluster	dascExtra		add=ljDascHostGPReg,ljDascHostFPReg,ljDascGuestGPReg
    hi def ljDascHostGPReg		term=bold cterm=bold ctermfg=Black ctermbg=Green
    hi def ljDascHostFPReg		term=bold cterm=bold ctermfg=Black ctermbg=Blue
    hi def ljDascGuestGPReg		term=bold cterm=bold ctermfg=Green ctermbg=NONE
    " end of arm registers coloring }}}
  elseif g:DascLuaJITFile ==# 'arm64' "aarch64
    " aarch64 registers coloring {{{
    let b:i = 0
    while b:i < 32
      execute 'syn keyword ljDascHostDwordReg w'.b:i.' contained'
      execute 'syn keyword ljDascHostQwordReg x'.b:i.' contained'
      execute 'syn keyword ljDascHostFPReg s'.b:i.' contained'
      let b:i += 1
    endwhile
    let b:i = 0
    while b:i < 16
      execute 'syn keyword ljDascHostFPReg d'.b:i.' contained'
      let b:i += 1
    endwhile
    unlet b:i
    syn keyword	ljDascHostQwordReg	sp fp contained

    syn keyword	ljDascGuestQwordReg	BASE KBASE PC GLREG LREG TISNUM TISNUMhi TISNIL RA RC RB INS ITYPE TMP0 TMP1 TMP2 TMP3 CARG1 CARG2 CARG3 CARG4 CARG5 CRET1 contained
    syn keyword	ljDascGuestDwordReg	ST_INTERP RAw RCw RBw INSw TMP0w TMP1w TMP2w TMP3w CARG1w CARG2w CARG3w CARG4w CARG5w CRET1w contained
    syn keyword	ljDascGuestFPReg	FARG1 FARG2 contained

    syn cluster	dascExtra		add=ljDascHostDwordReg,ljDascHostQwordReg,ljDascHostFPReg,ljDascGuestDwordReg,ljDascGuestQwordReg,ljDascGuestFPReg
    hi def ljDascHostDwordReg		term=bold cterm=bold ctermfg=Black ctermbg=Green
    hi def ljDascHostQwordReg		term=bold cterm=bold ctermfg=Black ctermbg=Red
    hi def ljDascHostFPReg		term=bold cterm=bold ctermfg=Black ctermbg=Blue

    hi def ljDascGuestDwordReg		term=bold cterm=bold ctermfg=Green ctermbg=NONE
    hi def ljDascGuestQwordReg		term=bold cterm=bold ctermfg=Red ctermbg=NONE
    hi def ljDascGuestFPReg		term=bold cterm=bold ctermfg=Blue ctermbg=NONE
    " end of aarch64 registers coloring }}}
  elseif g:DascLuaJITFile ==# 'ppc'
    " ppc registers coloring {{{
    let b:i = 0
    while b:i < 32
      execute 'syn keyword ljDascHostGPReg r'.b:i.' contained'
      execute 'syn keyword ljDascHostFPReg f'.b:i.' contained'
      let b:i += 1
    endwhile
    unlet b:i
    syn keyword	ljDascHostGPReg		sp contained

    syn keyword	ljDascGuestGPReg	BASE KBASE PC DISPATCH LREG MULTRES JGL TISNUM TISNIL ZERO RA RB RC RD INS TMP0 TMP1 TMP2 TMP3 SAVE0 SAVE1 CARG1 CARG2 CARG3 CARG4 CARG5 CRET1 CRET2 TOCREG ENVREG contained
    syn keyword	ljDascGuestFPReg	FARG1 FARG2 TOBIT TONUM contained

    syn cluster	dascExtra		add=ljDascHostGPReg,ljDascHostFPReg,ljDascGuestGPReg,ljDascGuestFPReg
    hi def ljDascHostGPReg		term=bold cterm=bold ctermfg=Black ctermbg=Green
    hi def ljDascHostFPReg		term=bold cterm=bold ctermfg=Black ctermbg=Blue

    hi def ljDascGuestGPReg		term=bold cterm=bold ctermfg=Green ctermbg=NONE
    hi def ljDascGuestFPReg		term=bold cterm=bold ctermfg=Blue ctermbg=NONE
    " end of ppc registers coloring }}}
  endif
endif " !exists("dasc_luajit_no_hl_registers")

" mips mips64 control hazards.
if g:DascLuaJITFile ==# 'mips' || g:DascLuaJITFile ==# 'mips64'
  syn match	ljDascMipsControlHazard	"\(\s*|\s*\)\@<=\zs\s*\.\ze\s"
  syn cluster	dascExtra		add=ljDascMipsControlHazard
  hi def link	ljDascMipsControlHazard	Exception
endif

" Links {{{
syn cluster	dascExtra		add=ljDascMacro,ljDascConst,ljDascPreproc
hi def link	ljDascMacro		Function
hi def link	ljDascConst		Constant
hi def link	ljDascPreproc		Special
" end of Links }}}

" vim: ts=8:fdm=marker:ff=unix:foldopen=all:foldclose=all
