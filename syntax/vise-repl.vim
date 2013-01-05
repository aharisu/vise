" Vim syntax file
" Language:	vise
" Last Change:	2013 Jan 05
" Maintainer:	aharisu <foo.yobina@gmail.com>

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"syn case ignore
syn case match

" this defines first for nested srfi-62 comment
" ( http://www.ac.cyberhome.ne.jp/~yakahaira/vimdoc/syntax.html#:syn-priority )
syn region schemeSrfi62CommentParen start="(" end=")" contains=schemeSrfi62CommentParen transparent
syn region schemeSrfi62CommentParen start="\[" end="\]" contains=schemeSrfi62CommentParen transparent

" Fascist highlighting: everything that doesn't fit the rules is an error...

"syn match	vise_replError	oneline    ![^ \t()\[\]";]*!
syn match	vise_replError	oneline    ")"

" Quoted and backquoted stuff

syn region vise_replQuoted matchgroup=Delimiter start="['`]" end=![ \t()\[\]";]!me=e-1 contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

syn region vise_replQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replQuoted matchgroup=Delimiter start="['`]#(" matchgroup=Delimiter end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

syn region vise_replStrucRestricted matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replStrucRestricted matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

" Popular vise_repl extension:
" using [] as well as ()
syn region vise_replQuoted matchgroup=Delimiter start="['`]\[" matchgroup=Delimiter end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replQuoted matchgroup=Delimiter start="['`]#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replStrucRestricted matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replStrucRestricted matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

syn region vise_replUnquote matchgroup=Delimiter start="," end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replUnquote matchgroup=Delimiter start=",@" end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

syn region vise_replUnquote matchgroup=Delimiter start=",(" end=")" contains=ALL
syn region vise_replUnquote matchgroup=Delimiter start=",@(" end=")" contains=ALL

syn region vise_replUnquote matchgroup=Delimiter start=",#(" end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replUnquote matchgroup=Delimiter start=",@#(" end=")" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

syn region vise_replUnquote matchgroup=Delimiter start=",\[" end="\]" contains=ALL
syn region vise_replUnquote matchgroup=Delimiter start=",@\[" end="\]" contains=ALL

syn region vise_replUnquote matchgroup=Delimiter start=",#\[" end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc
syn region vise_replUnquote matchgroup=Delimiter start=",@#\[" end="\]" contains=ALLBUT,vise_replStruc,vise_replSyntax,vise_replFunc

" R5RS vise_repl Functions and Syntax:

if version < 600
  set iskeyword=33,35-38,42,43,45-58,60-90,94,95,97-122,126,_
else
  setlocal iskeyword=33,35-38,42,43,45-58,60-90,94,95,97-122,126,_
endif

syn keyword vise_replSyntax lambda and or if cond defun defvar defmacro let let* letrec
syn keyword vise_replSyntax begin dolist while set! else
syn keyword vise_replSyntax quote quasiquote unquote unquote-splicing
syn keyword vise_replSyntax when unless
syn keyword vise_replSyntax dict try autocmd augroup
syn keyword vise_replSyntax let1 rlet1 if-let1

syn keyword vise_replFunc not eq?
syn keyword vise_replFunc list length
syn keyword vise_replFunc  abs append argv atan2 bufexists bufname byte2line ceil cindent complete confirm cosh cursor did_filetype empty eventhandler exp extend filewritable findfile fmod foldclosed foldtext function getbufline getcharmod getcmdtype getfperm getftype getmatches getqflist gettabvar getwinposy globpath haslocaldir histdel hlexists iconv input inputrestore insert items len line localtime map match matchdelete matchstr min mode nextnonblank pathshorten prevnonblank pumvisible readfile reltimestr remote_foreground remote_read remove repeat reverse search searchpair searchpos serverlist setcmdpos setloclist setpos setreg settabwinvar shellescape sin sort spellbadword split str2float strchars strftime string strpart strtrans submatch synconcealed synIDattr synstack tabpagebuflist tabpagewinnr taglist tanh tolower tr type undotree virtcol winbufnr winheight winnr winrestview winwidth
syn keyword vise_replFunc  acos argc asin browse buflisted bufnr byteidx changenr clearmatches complete_add copy count deepcopy diff_filler escape executable expand feedkeys filter float2nr fnameescape foldclosedend foldtextresult garbagecollect getbufvar getcmdline getcwd getfsize getline getpid getreg gettabwinvar getwinvar has hasmapto histget hlID indent inputdialog inputsave isdirectory join libcall line2byte log maparg matchadd matchend max mkdir mzeval nr2char pow printf range reltime remote_expr remote_peek remote_send rename resolve round searchdecl searchpairpos server2client setbufvar setline setmatches setqflist settabvar setwinvar simplify sinh soundfold spellsuggest sqrt str2nr strdisplaywidth stridx strlen strridx strwidth substitute synID synIDtrans system tabpagenr tagfiles tan tempname toupper trunc undofile values visualmode wincol winline winrestcmd winsaveview writefile
syn keyword vise_replFunc  add argidx atan browsedir bufloaded bufwinnr call char2nr col complete_check cos cscope_connection delete diff_hlID eval exists expr8 filereadable finddir floor fnamemodify foldlevel foreground get getchar getcmdpos getfontname getftime getloclist getpos getregtype getwinposx glob has_key histadd histnr hostname index inputlist inputsecret islocked keys libcallnr lispindent log10 mapcheck matcharg matchlist 
syn keyword vise_replFunc  return break continue
syn keyword vise_replFunc  + - * / % . && \|\| ! ~ & < <= > >= == != += -= .= is isnot ==# !=# ># >=# <# <=# ==? !=? >? >=? <? <=? =~ =~# #~? !~ !~# !~?
syn keyword vise_replFunc  ref subseq



" ... so that a single + or -, inside a quoted context, would not be
" interpreted as a number (outside such contexts, it's a vise_replFunc)

syn match	vise_replDelimiter	oneline    !\.[ \t\[\]()";]!me=e-1
syn match	vise_replDelimiter	oneline    !\.$!
" ... and a single dot is not a number but a delimiter

" This keeps all other stuff unhighlighted, except *stuff* and <stuff>:

syn match	vise_replOther	oneline    ,[a-z!$%&*/:<=>?^_~+@%-][-a-z!$%&*/:<=>?^_~0-9+.@%]*,
"syn match	vise_replError	oneline    ,[a-z!$%&*/:<=>?^_~+@%-][-a-z!$%&*/:<=>?^_~0-9+.@%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	vise_replOther	oneline    "\.\.\."
syn match	vise_replError	oneline    !\.\.\.[^ \t\[\]()";]\+!
" ... a special identifier

syn match	vise_replConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[ \t\[\]()";],me=e-1
syn match	vise_replConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*$,
syn match	vise_replError	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	vise_replConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[ \t\[\]()";],me=e-1
syn match	vise_replConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>$,
syn match	vise_replError	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

" Non-quoted lists, and strings:

syn region vise_replStruc matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
syn region vise_replStruc matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALL

syn region vise_replStruc matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region vise_replStruc matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALL

" Simple literals:
syn region vise_replString start=+\%(\\\)\@<!"+ skip=+\\[\\"]+ end=+"+

" Comments:

syn match	vise_replSrfi62Comment	oneline    ,#;[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,
syn match	vise_replError		oneline    ,#;[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,
syn match	vise_replSrfi62Comment	oneline    ,#;['`][a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,
syn match	vise_replError		oneline    ,#;['`][a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,
syn region vise_replSrfi62Comment matchgroup=Comment start="#;(" matchgroup=Comment end=")" contains=schemeSrfi62CommentParen
syn region vise_replSrfi62Comment matchgroup=Comment start="#;\[" matchgroup=Comment end="\]" contains=schemeSrfi62CommentParen
syn region vise_replSrfi62Comment matchgroup=Comment start="#;['`](" matchgroup=Comment end=")" contains=schemeSrfi62CommentParen
syn region vise_replSrfi62Comment matchgroup=Comment start="#;['`]\[" matchgroup=Comment end="\]" contains=schemeSrfi62CommentParen
syn match	vise_replComment	";.*$"

" Writing out the complete description of vise_repl numerals without
" using variables is a day's work for a trained secretary...

syn match	vise_replOther	oneline    ![+-][ \t\[\]()";]!me=e-1
syn match	vise_replOther	oneline    ![+-]$!
"
" This is a useful lax approximation:
syn match	vise_replNumber	oneline    "[-+0-9.][-#+/0-9a-f@i.boxesfdl]*"
syn match	vise_replError	oneline    ![-+0-9.][-#+/0-9a-f@i.boxesfdl]*[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*!
syn match	vise_replNumber	oneline    "#[-#+/0-9a-f@i.boxesfdl]+"
syn match	vise_replError	oneline    !#[-#+/0-9a-f@i.boxesfdl]+[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*!
syn match	vise_replNumber	oneline    "[-+]inf\.0"
syn match	vise_replError	oneline    "[-+]inf\.0[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*"
syn match	vise_replNumber	oneline    "+nan\.0"
syn match	vise_replError	oneline    "+nan\.0[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*"

syn match	vise_replBoolean	oneline    "#[tf]"
syn match	vise_replError	oneline    !#[tf][^ \t\[\]()";]\+!

syn match	vise_replChar	oneline    "#\\"
syn match	vise_replChar	oneline    "#\\."
syn match	vise_replError	oneline    !#\\.[^ \t\[\]()";]\+!
syn match	vise_replChar	oneline    "#\\space"
syn match	vise_replError	oneline    !#\\space[^ \t\[\]()";]\+!
syn match	vise_replChar	oneline    "#\\newline"
syn match	vise_replError	oneline    !#\\newline[^ \t\[\]()";]\+!

" regular vim commands
syn keyword vise_replCommand contained	a arga[dd] argu[ment] bd[elete] bN[ext] breakd[el] buf c cal[l] ce[nter] cg[etfile] cl cn cNf comc[lear] cope[n] cr[ewind] d d[elete] diffo diffsplit di[splay] ds[earch] ec e:e:e en endt[ry] exu[sage] filetype fix[del] for go[to] h hi if intro k la lan[guage] lch[dir] let@ lg[etfile] lla[st] lnew[er] lNf[ile] loc[kmarks] lr[ewind] lv[imgrep] ma[rk] messages mkv mv n new noautocmd on[ly] p:~ perld[o] popu[p] p[rint] promptr[epl] ptl[ast] ptr[ewind] py3file q[uit] r[ead] redraws[tatus] ret[ab] r:r:r ru[ntime] sba[ll] sbp[revious] scs sf[ind] sil[ent] sm[ap] sno[magic] so[urce] spellr[epall] st startr[eplace] sunme sw[apname] t tabf[ind] tabn[ext] ta[g] tf[irst] tn tp[revious] tu undoj[oin] up[date] vi vmapc[lear] win wN[ext] wundo xmapc[lear] xnoremenu
syn keyword vise_replCommand contained	ab argd[elete] as[cii] bel[owright] bo[tright] breakl[ist] bufdo cabc[lear] cat[ch] cex[pr] c[hange] cla[st] cN cnf[ile] comment co[py] cs de delf diffoff difft dj[ump] dsp[lit] echoe[rr] e:e:r endf endw[hile] f fin fo[ld] fu gr[ep] ha[rdcopy] hid[e] ij[ump] is[earch] keepa lad la[st] lcl[ose] lex[pr] lgr[ep] lli[st] lne[xt] lo lockv[ar] ls lvimgrepa[dd] marks mk mkvie[w] Mycmd N n[ext] noh[lsearch] o[pen] P p:gs? pp[op] P[rint] ps[earch] ptn pts[elect] pyf[ile] quita[ll] rec[over] reg[isters] retu[rn] ru rv[iminfo] sbf[irst] sbr[ewind] scscope sfir[st] sim[alt] sme snoreme s?pat?sub? spellu[ndo] sta[g] stj[ump] sunmenu sy ta tabfir[st] tabN[ext] tags th[row] tN tr tu[nmenu] undol[ist] v vie[w] vne[w] winc[md] wp[revious] wv[iminfo] xme xterm
syn keyword vise_replCommand contained	abc[lear] argdo au bf[irst] bp[revious] br[ewind] b[uffer] cad cb[uffer] cf[ile] changes cl[ist] cnew[er] cNf[ile] comp[iler] count cscope debug delf[unction] DiffOrig diffthis dl[ist] dwim echom[sg] el[se] endfo[r] ene[w] f[ile] fina[lly] foldc[lose] fun grepa[dd] h[elp] his[tory] il[ist] isp[lit] keepalt laddb[uffer] lat lcs lf[ile] lgrepa[dd] lmak[e] lN[ext] loadk lol[der] lt[ag] lw[indow] mat[ch] mkdir mkv[imrc] MyCommand nbc[lose] N[ext] nu[mber] opt[ions] pc[lose] p:h pr pro p:t ptN pu[t] py[thon] quote red Ren rew[ind] rub[y] sal[l] sbl[ast] sb[uffer] se[t] sh[ell] sl smenu snoremenu spe spellw[rong] star st[op] sus[pend] syn tab tabl[ast] tabo[nly] tc[l] tj[ump] tn[ext] t:r u unh[ide] ve vim[grep] vs[plit] windo wq x xmenu xunme
syn keyword vise_replCommand contained	abo[veleft] arge[dit] bad[d] bl[ast] br bro[wse] buffers caddb[uffer] cc cfir[st] chd[ir] clo[se] cn[ext] col[der] con cpf[ile] cstag debugg[reedy] delm[arks] diffp diffu[pdate] do e echon elsei[f] endfun Error filename fin[d] folddoc[losed] fu[nction] gs?pat?sub? helpf[ind] i imapc[lear] iuna[bbrev] keepj[umps] lad[dexpr] later lcscope lfir[st] lh[elpgrep] lmapc[lear] lnf loadkeymap lop[en] lua ma menut mk[exrc] mo mz nb[key] nkf o ownsyntax pe p:h:h p:r profd[el] pta[g] ptn[ext] pw[d] python3 r redi[r] Rena ri[ght] rubyd[o] san[dbox] sbm[odified] scrip setf[iletype] si sla[st] sn[ext] s@\n@\=\r" spelld[ump] sp[lit] start stopi[nsert] s?version?main? sync tabc[lose] tabm[ove] tabp[revious] tcld[o] tl[ast] tN[ext] tr[ewind] un unl verb[ose] vimgrepa[dd] w winp[os] wqa[ll] X XMLent xunmenu
syn keyword vise_replCommand contained	al[l] argg[lobal] ba[ll] bm[odified] brea[k] browseset bun[load] cad[dexpr] ccl[ose] cgetb[uffer] che[ckpath] cmapc[lear] cN[ext] colo[rvise_repl] conf[irm] cp[revious] cuna[bbrev] del di diffpatch dig doau ea e[dit] em[enu] endf[unction] ex files fini[sh] foldd[oopen] g gui helpg[rep] ia in j[oin] kee[pmarks] laddf[ile] lb[uffer] le[ft] lgetb[uffer] l[ist] lN lNf lo[adview] lpf[ile] luado mak[e] menut[ranslate] mks[ession] mod[e] mzf[ile] nbs[tart] nmapc[lear] ol[dfiles] p ped[it] po[p] pre[serve] prof[ile] ptf[irst] ptN[ext] py q re red[o] Renu rightb[elow] rubyf[ile] sa[rgument] sbn[ext] scripte[ncoding] setg[lobal] sig sl[eep] sN[ext] so spe[llgood] spr[evious] startg[replace] sts[elect] s?version?main?:p syncbind tabd[o] tabN tabr[ewind] tclf[ile] tm TOhtml try una[bbreviate] unlo[ckvar] ve[rsion] vi[sual] wa[ll] win[size] w[rite] xa[ll] XMLns xwininfo
syn keyword vise_replCommand contained	Allargs argl[ocal] bar bn[ext] breaka[dd] bu bw[ipeout] caddf[ile] cd cgete[xpr] checkt[ime] cmdname cnf com con[tinue] cq[uit] cw[indow] delc[ommand] diffg[et] diffpu[t] dig[raphs] dr[op] earlier e:e emenu* en[dif] exi[t] filet fir[st] foldo[pen] get gvim helpt[ags] iabc[lear] index ju[mps] l lan lc[d] lefta[bove] lgete[xpr] ll lne lnf[ile] locale lp[revious] luafile Man mes mksp[ell] m[ove] mz[vise_repl] ne noa omapc[lear] p: pe[rl] popu prev[ious] promptf[ind] ptj[ump] ptp[revious] py3 qa[ll] r:e redr[aw] res[ize] r:r rundo sav[eas] sbN[ext] scrip[tnames] setl[ocal] sign sm[agic] sni[ff] sor[t] spelli[nfo] sre[wind] star[tinsert] sun[hide] sv[iew] synlist tabe[dit] tabnew tabs te[aroff] tm[enu] to[pleft] ts[elect] u[ndo] uns[ilent] vert[ical] viu[sage] wh[ile] wn[ext] ws[verb] x[it] xnoreme y[ank]

syntax region vise_replMultilineComment start=/#|/ end=/|#/ contains=vise_replMultilineComment

" #/xxx/ are the special Gauche identifiers for regexp
syn region vise_replRegexp start=+\%(\\\)\@<!#/+ skip=+\\[\\/]+ end=+/+

" anything limited by |'s is identifier
syn match vise_replOther oneline    "|[^|]\+|"

" includ
syn match	vise_replInclude	oneline    "(include .*)"

syn keyword vise_replAutoEvent BufAdd
syn keyword vise_replAutoEvent BufCreate
syn keyword vise_replAutoEvent BufDelete
syn keyword vise_replAutoEvent BufEnter
syn keyword vise_replAutoEvent BufFilePost
syn keyword vise_replAutoEvent BufFilePre
syn keyword vise_replAutoEvent BufHidden
syn keyword vise_replAutoEvent BufLeave
syn keyword vise_replAutoEvent BufNew
syn keyword vise_replAutoEvent BufNewFile
syn keyword vise_replAutoEvent BufRead
syn keyword vise_replAutoEvent BufReadCmd
syn keyword vise_replAutoEvent BufReadPost
syn keyword vise_replAutoEvent BufReadPre
syn keyword vise_replAutoEvent BufUnload
syn keyword vise_replAutoEvent BufWinEnter
syn keyword vise_replAutoEvent BufWinLeave
syn keyword vise_replAutoEvent BufWipeout
syn keyword vise_replAutoEvent BufWrite
syn keyword vise_replAutoEvent BufWriteCmd
syn keyword vise_replAutoEvent BufWritePost
syn keyword vise_replAutoEvent BufWritePre
syn keyword vise_replAutoEvent Cmd-event
syn keyword vise_replAutoEvent CmdwinEnter
syn keyword vise_replAutoEvent CmdwinLeave
syn keyword vise_replAutoEvent Colorvise
syn keyword vise_replAutoEvent CursorHold
syn keyword vise_replAutoEvent CursorHoldI
syn keyword vise_replAutoEvent CursorMoved
syn keyword vise_replAutoEvent CursorMovedI
syn keyword vise_replAutoEvent EncodingChanged
syn keyword vise_replAutoEvent FileAppendCmd
syn keyword vise_replAutoEvent FileAppendPost
syn keyword vise_replAutoEvent FileAppendPre
syn keyword vise_replAutoEvent FileChangedRO
syn keyword vise_replAutoEvent FileChangedShell
syn keyword vise_replAutoEvent FileChangedShellPost
syn keyword vise_replAutoEvent FileEncoding
syn keyword vise_replAutoEvent FileReadCmd
syn keyword vise_replAutoEvent FileReadPost
syn keyword vise_replAutoEvent FileReadPre
syn keyword vise_replAutoEvent FileType
syn keyword vise_replAutoEvent FileWriteCmd
syn keyword vise_replAutoEvent FileWritePost
syn keyword vise_replAutoEvent FileWritePre
syn keyword vise_replAutoEvent FilterReadPost
syn keyword vise_replAutoEvent FilterReadPre
syn keyword vise_replAutoEvent FilterWritePost
syn keyword vise_replAutoEvent FilterWritePre
syn keyword vise_replAutoEvent FocusGained
syn keyword vise_replAutoEvent FocusLost
syn keyword vise_replAutoEvent FuncUndefined
syn keyword vise_replAutoEvent GUIEnter
syn keyword vise_replAutoEvent GUIFailed
syn keyword vise_replAutoEvent InsertChange
syn keyword vise_replAutoEvent InsertEnter
syn keyword vise_replAutoEvent InsertLeave
syn keyword vise_replAutoEvent MenuPopup
syn keyword vise_replAutoEvent QuickFixCmdPost
syn keyword vise_replAutoEvent QuickFixCmdPre
syn keyword vise_replAutoEvent RemoteReply
syn keyword vise_replAutoEvent SessionLoadPost
syn keyword vise_replAutoEvent ShellCmdPost
syn keyword vise_replAutoEvent ShellFilterPost
syn keyword vise_replAutoEvent SourceCmd
syn keyword vise_replAutoEvent SourcePre
syn keyword vise_replAutoEvent SpellFileMissing
syn keyword vise_replAutoEvent StdinReadPost
syn keyword vise_replAutoEvent StdinReadPre
syn keyword vise_replAutoEvent SwapExists
syn keyword vise_replAutoEvent Syntax
syn keyword vise_replAutoEvent TabEnter
syn keyword vise_replAutoEvent TabLeave
syn keyword vise_replAutoEvent TermChanged
syn keyword vise_replAutoEvent TermResponse
syn keyword vise_replAutoEvent User
syn keyword vise_replAutoEvent UserGettingBored
syn keyword vise_replAutoEvent VimEnter
syn keyword vise_replAutoEvent VimLeave
syn keyword vise_replAutoEvent VimLeavePre
syn keyword vise_replAutoEvent VimResized
syn keyword vise_replAutoEvent WinEnter
syn keyword vise_replAutoEvent WinLeave

" meddlesome
set ts=8 sts=2 sw=2 et nocindent lisp


" Synchronization and the wrapping up...

syn sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_vise_repl_syntax_inits")
  if version < 508
    let did_vise_repl_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink vise_replCommand		Statement
  HiLink vise_replAutoEvent		Type

  HiLink vise_replSyntax		Statement
  HiLink vise_replFunc		Function

  HiLink vise_replString		String
  HiLink vise_replChar		Character
  HiLink vise_replNumber		Number
  HiLink vise_replBoolean		Boolean

  HiLink vise_replDelimiter	Delimiter
  HiLink vise_replConstant		Constant

  HiLink vise_replComment		Comment
  HiLink vise_replMultilineComment	Comment
  HiLink vise_replError		Error

  HiLink vise_replRegexp		vise_replString
  HiLink schemeSrfi62Comment	vise_replComment
  HiLink vise_replInclude		Include

  delcommand HiLink
endif

let b:current_syntax = "vise_repl"
