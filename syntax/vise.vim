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

"syn match	viseError	oneline    ![^ \t()\[\]";]*!
syn match	viseError	oneline    ")"

" Quoted and backquoted stuff

syn region viseQuoted matchgroup=Delimiter start="['`]" end=![ \t()\[\]";]!me=e-1 contains=ALLBUT,viseStruc,viseSyntax,viseFunc

syn region viseQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseQuoted matchgroup=Delimiter start="['`]#(" matchgroup=Delimiter end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc

syn region viseStrucRestricted matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseStrucRestricted matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc

" Popular vise extension:
" using [] as well as ()
syn region viseQuoted matchgroup=Delimiter start="['`]\[" matchgroup=Delimiter end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseQuoted matchgroup=Delimiter start="['`]#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseStrucRestricted matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseStrucRestricted matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc

syn region viseUnquote matchgroup=Delimiter start="," end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseUnquote matchgroup=Delimiter start=",@" end=![ \t\[\]()";]!me=e-1 contains=ALLBUT,viseStruc,viseSyntax,viseFunc

syn region viseUnquote matchgroup=Delimiter start=",(" end=")" contains=ALL
syn region viseUnquote matchgroup=Delimiter start=",@(" end=")" contains=ALL

syn region viseUnquote matchgroup=Delimiter start=",#(" end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseUnquote matchgroup=Delimiter start=",@#(" end=")" contains=ALLBUT,viseStruc,viseSyntax,viseFunc

syn region viseUnquote matchgroup=Delimiter start=",\[" end="\]" contains=ALL
syn region viseUnquote matchgroup=Delimiter start=",@\[" end="\]" contains=ALL

syn region viseUnquote matchgroup=Delimiter start=",#\[" end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc
syn region viseUnquote matchgroup=Delimiter start=",@#\[" end="\]" contains=ALLBUT,viseStruc,viseSyntax,viseFunc

" R5RS vise Functions and Syntax:

if version < 600
  set iskeyword=33,35-38,42,43,45-58,60-90,94,95,97-122,126,_
else
  setlocal iskeyword=33,35-38,42,43,45-58,60-90,94,95,97-122,126,_
endif

autocmd BufEnter *.vise call s:buf_enter()
autocmd BufLeave *.vise call s:buf_leave()

let s:lispwords = ''
function! s:buf_enter()
  if empty(s:lispwords)
    let s:lispwords = &lispwords
    set lispwords=
    set lispwords+=lambda,and,or,if,cond,defun,defvar,defmacro,let,let*,letrec
    set lispwords+=begin,dolist,while,set!,else
    set lispwords+=quote,quasiquote,unquote,unquote-splicing
    set lispwords+=when,unless
    set lispwords+=dict,array,try,autocmd,augroup
    set lispwords+=let1,rlet1,if-let1

    "gauche word
    set lispwords+=case,define,do,=>,define-syntax,let-syntax,letrec-syntax,syntax-rules,and-let*,define-class,define-constant,define-generic,define-in-module,define-macro,define-method,include,receive,$,$*,$<<,$do,$do*,$or,^,^.,^_,^a,^b,^c,^d,^e,^f,^g,^h,^i,^j,^k,^l,^m,^n,^o,^p,^q,^r,^s,^t,^u,^w,^v,^x,^y,^z,begin0,case-lambda,cut,cute,dec!,define-^x,define-values,dotimes,get-keyword*,get-optional,guard,inc!,let*-values,let-args,let-keywords,let-keywords*,let-optionals*,let-string-start+end,let-values,let/cc,match,match-define,match-lambda,match-lambda*,match-let,match-let*,match-let1,match-letrec,pop!,program,push!,rec,require-extension,reset,rxmatch-case,rxmatch-cond,rxmatch-if,rxmatch-let,set!-values,shift,test*,until,unwind-protect,update!,values-ref,with-builder
  endif
endfunction

function! s:buf_leave()
  if !empty(s:lispwords)
    let &lispwords = s:lispwords
    let s:lispwords = ''
  endif
endfunction


syn keyword viseSyntax lambda and or if cond defun defvar defmacro let let* letrec
syn keyword viseSyntax begin dolist while set! else
syn keyword viseSyntax quote quasiquote unquote unquote-splicing
syn keyword viseSyntax when unless
syn keyword viseSyntax dict try autocmd augroup
syn keyword viseSyntax let1 rlet1 if-let1

syn keyword viseFunc not eq?
syn keyword viseFunc list length
syn keyword viseFunc  abs append argv atan2 bufexists bufname byte2line ceil cindent complete confirm cosh cursor did_filetype empty eventhandler exp extend filewritable findfile fmod foldclosed foldtext function getbufline getcharmod getcmdtype getfperm getftype getmatches getqflist gettabvar getwinposy globpath haslocaldir histdel hlexists iconv input inputrestore insert items len line localtime map match matchdelete matchstr min mode nextnonblank pathshorten prevnonblank pumvisible readfile reltimestr remote_foreground remote_read remove repeat reverse search searchpair searchpos serverlist setcmdpos setloclist setpos setreg settabwinvar shellescape sin sort spellbadword split str2float strchars strftime string strpart strtrans submatch synconcealed synIDattr synstack tabpagebuflist tabpagewinnr taglist tanh tolower tr type undotree virtcol winbufnr winheight winnr winrestview winwidth
syn keyword viseFunc  acos argc asin browse buflisted bufnr byteidx changenr clearmatches complete_add copy count deepcopy diff_filler escape executable expand feedkeys filter float2nr fnameescape foldclosedend foldtextresult garbagecollect getbufvar getcmdline getcwd getfsize getline getpid getreg gettabwinvar getwinvar has hasmapto histget hlID indent inputdialog inputsave isdirectory join libcall line2byte log maparg matchadd matchend max mkdir mzeval nr2char pow printf range reltime remote_expr remote_peek remote_send rename resolve round searchdecl searchpairpos server2client setbufvar setline setmatches setqflist settabvar setwinvar simplify sinh soundfold spellsuggest sqrt str2nr strdisplaywidth stridx strlen strridx strwidth substitute synID synIDtrans system tabpagenr tagfiles tan tempname toupper trunc undofile values visualmode wincol winline winrestcmd winsaveview writefile
syn keyword viseFunc  add argidx atan browsedir bufloaded bufwinnr call char2nr col complete_check cos cscope_connection delete diff_hlID eval exists expr8 filereadable finddir floor fnamemodify foldlevel foreground get getchar getcmdpos getfontname getftime getloclist getpos getregtype getwinposx glob has_key histadd histnr hostname index inputlist inputsecret islocked keys libcallnr lispindent log10 mapcheck matcharg matchlist 
syn keyword viseFunc  return break continue
syn keyword viseFunc  + - * / % . && \|\| ! ~ & < <= > >= == != += -= .= is isnot ==# !=# ># >=# <# <=# ==? !=? >? >=? <? <=? =~ =~# #~? !~ !~# !~?
syn keyword viseFunc  ref subseq



" ... so that a single + or -, inside a quoted context, would not be
" interpreted as a number (outside such contexts, it's a viseFunc)

syn match	viseDelimiter	oneline    !\.[ \t\[\]()";]!me=e-1
syn match	viseDelimiter	oneline    !\.$!
" ... and a single dot is not a number but a delimiter

" This keeps all other stuff unhighlighted, except *stuff* and <stuff>:

syn match	viseOther	oneline    ,[a-z!$%&*/:<=>?^_~+@%-][-a-z!$%&*/:<=>?^_~0-9+.@%]*,
"syn match	viseError	oneline    ,[a-z!$%&*/:<=>?^_~+@%-][-a-z!$%&*/:<=>?^_~0-9+.@%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	viseOther	oneline    "\.\.\."
syn match	viseError	oneline    !\.\.\.[^ \t\[\]()";]\+!
" ... a special identifier

syn match	viseConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[ \t\[\]()";],me=e-1
syn match	viseConstant	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*$,
syn match	viseError	oneline    ,\*[-a-z!$%&*/:<=>?^_~0-9+.@]*\*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

syn match	viseConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[ \t\[\]()";],me=e-1
syn match	viseConstant	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>$,
syn match	viseError	oneline    ,<[-a-z!$%&*/:<=>?^_~0-9+.@]*>[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,

" Non-quoted lists, and strings:

syn region viseStruc matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
syn region viseStruc matchgroup=Delimiter start="#(" matchgroup=Delimiter end=")" contains=ALL

syn region viseStruc matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region viseStruc matchgroup=Delimiter start="#\[" matchgroup=Delimiter end="\]" contains=ALL

" Simple literals:
syn region viseString start=+\%(\\\)\@<!"+ skip=+\\[\\"]+ end=+"+

" Comments:

syn match	schemeSrfi62Comment	oneline    ,#;[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,
syn match	viseError		oneline    ,#;[a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,
syn match	schemeSrfi62Comment	oneline    ,#;['`][a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*,
syn match	viseError		oneline    ,#;['`][a-z!$%&*/:<=>?^_~+@#%-][-a-z!$%&*/:<=>?^_~0-9+.@#%]*[^-a-z!$%&*/:<=>?^_~0-9+.@ \t\[\]()";]\+[^ \t\[\]()";]*,
syn region schemeSrfi62Comment matchgroup=Comment start="#;(" matchgroup=Comment end=")" contains=schemeSrfi62CommentParen
syn region schemeSrfi62Comment matchgroup=Comment start="#;\[" matchgroup=Comment end="\]" contains=schemeSrfi62CommentParen
syn region schemeSrfi62Comment matchgroup=Comment start="#;['`](" matchgroup=Comment end=")" contains=schemeSrfi62CommentParen
syn region schemeSrfi62Comment matchgroup=Comment start="#;['`]\[" matchgroup=Comment end="\]" contains=schemeSrfi62CommentParen
syn match	viseComment	";.*$"

" Writing out the complete description of vise numerals without
" using variables is a day's work for a trained secretary...

syn match	viseOther	oneline    ![+-][ \t\[\]()";]!me=e-1
syn match	viseOther	oneline    ![+-]$!
"
" This is a useful lax approximation:
syn match	viseNumber	oneline    "[-+0-9.][-#+/0-9a-f@i.boxesfdl]*"
syn match	viseError	oneline    ![-+0-9.][-#+/0-9a-f@i.boxesfdl]*[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*!
syn match	viseNumber	oneline    "#[-#+/0-9a-f@i.boxesfdl]+"
syn match	viseError	oneline    !#[-#+/0-9a-f@i.boxesfdl]+[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*!
syn match	viseNumber	oneline    "[-+]inf\.0"
syn match	viseError	oneline    "[-+]inf\.0[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*"
syn match	viseNumber	oneline    "+nan\.0"
syn match	viseError	oneline    "+nan\.0[^-#+/0-9a-f@i.boxesfdl \t\[\]()";][^ \t\[\]()";]*"

syn match	viseBoolean	oneline    "#[tf]"
syn match	viseError	oneline    !#[tf][^ \t\[\]()";]\+!

syn match	viseChar	oneline    "#\\"
syn match	viseChar	oneline    "#\\."
syn match	viseError	oneline    !#\\.[^ \t\[\]()";]\+!
syn match	viseChar	oneline    "#\\space"
syn match	viseError	oneline    !#\\space[^ \t\[\]()";]\+!
syn match	viseChar	oneline    "#\\newline"
syn match	viseError	oneline    !#\\newline[^ \t\[\]()";]\+!

" regular vim commands
syn keyword viseCommand contained	a arga[dd] argu[ment] bd[elete] bN[ext] breakd[el] buf c cal[l] ce[nter] cg[etfile] cl cn cNf comc[lear] cope[n] cr[ewind] d d[elete] diffo diffsplit di[splay] ds[earch] ec e:e:e en endt[ry] exu[sage] filetype fix[del] for go[to] h hi if intro k la lan[guage] lch[dir] let@ lg[etfile] lla[st] lnew[er] lNf[ile] loc[kmarks] lr[ewind] lv[imgrep] ma[rk] messages mkv mv n new noautocmd on[ly] p:~ perld[o] popu[p] p[rint] promptr[epl] ptl[ast] ptr[ewind] py3file q[uit] r[ead] redraws[tatus] ret[ab] r:r:r ru[ntime] sba[ll] sbp[revious] scs sf[ind] sil[ent] sm[ap] sno[magic] so[urce] spellr[epall] st startr[eplace] sunme sw[apname] t tabf[ind] tabn[ext] ta[g] tf[irst] tn tp[revious] tu undoj[oin] up[date] vi vmapc[lear] win wN[ext] wundo xmapc[lear] xnoremenu
syn keyword viseCommand contained	ab argd[elete] as[cii] bel[owright] bo[tright] breakl[ist] bufdo cabc[lear] cat[ch] cex[pr] c[hange] cla[st] cN cnf[ile] comment co[py] cs de delf diffoff difft dj[ump] dsp[lit] echoe[rr] e:e:r endf endw[hile] f fin fo[ld] fu gr[ep] ha[rdcopy] hid[e] ij[ump] is[earch] keepa lad la[st] lcl[ose] lex[pr] lgr[ep] lli[st] lne[xt] lo lockv[ar] ls lvimgrepa[dd] marks mk mkvie[w] Mycmd N n[ext] noh[lsearch] o[pen] P p:gs? pp[op] P[rint] ps[earch] ptn pts[elect] pyf[ile] quita[ll] rec[over] reg[isters] retu[rn] ru rv[iminfo] sbf[irst] sbr[ewind] scscope sfir[st] sim[alt] sme snoreme s?pat?sub? spellu[ndo] sta[g] stj[ump] sunmenu sy ta tabfir[st] tabN[ext] tags th[row] tN tr tu[nmenu] undol[ist] v vie[w] vne[w] winc[md] wp[revious] wv[iminfo] xme xterm
syn keyword viseCommand contained	abc[lear] argdo au bf[irst] bp[revious] br[ewind] b[uffer] cad cb[uffer] cf[ile] changes cl[ist] cnew[er] cNf[ile] comp[iler] count cscope debug delf[unction] DiffOrig diffthis dl[ist] dwim echom[sg] el[se] endfo[r] ene[w] f[ile] fina[lly] foldc[lose] fun grepa[dd] h[elp] his[tory] il[ist] isp[lit] keepalt laddb[uffer] lat lcs lf[ile] lgrepa[dd] lmak[e] lN[ext] loadk lol[der] lt[ag] lw[indow] mat[ch] mkdir mkv[imrc] MyCommand nbc[lose] N[ext] nu[mber] opt[ions] pc[lose] p:h pr pro p:t ptN pu[t] py[thon] quote red Ren rew[ind] rub[y] sal[l] sbl[ast] sb[uffer] se[t] sh[ell] sl smenu snoremenu spe spellw[rong] star st[op] sus[pend] syn tab tabl[ast] tabo[nly] tc[l] tj[ump] tn[ext] t:r u unh[ide] ve vim[grep] vs[plit] windo wq x xmenu xunme
syn keyword viseCommand contained	abo[veleft] arge[dit] bad[d] bl[ast] br bro[wse] buffers caddb[uffer] cc cfir[st] chd[ir] clo[se] cn[ext] col[der] con cpf[ile] cstag debugg[reedy] delm[arks] diffp diffu[pdate] do e echon elsei[f] endfun Error filename fin[d] folddoc[losed] fu[nction] gs?pat?sub? helpf[ind] i imapc[lear] iuna[bbrev] keepj[umps] lad[dexpr] later lcscope lfir[st] lh[elpgrep] lmapc[lear] lnf loadkeymap lop[en] lua ma menut mk[exrc] mo mz nb[key] nkf o ownsyntax pe p:h:h p:r profd[el] pta[g] ptn[ext] pw[d] python3 r redi[r] Rena ri[ght] rubyd[o] san[dbox] sbm[odified] scrip setf[iletype] si sla[st] sn[ext] s@\n@\=\r" spelld[ump] sp[lit] start stopi[nsert] s?version?main? sync tabc[lose] tabm[ove] tabp[revious] tcld[o] tl[ast] tN[ext] tr[ewind] un unl verb[ose] vimgrepa[dd] w winp[os] wqa[ll] X XMLent xunmenu
syn keyword viseCommand contained	al[l] argg[lobal] ba[ll] bm[odified] brea[k] browseset bun[load] cad[dexpr] ccl[ose] cgetb[uffer] che[ckpath] cmapc[lear] cN[ext] colo[rvise] conf[irm] cp[revious] cuna[bbrev] del di diffpatch dig doau ea e[dit] em[enu] endf[unction] ex files fini[sh] foldd[oopen] g gui helpg[rep] ia in j[oin] kee[pmarks] laddf[ile] lb[uffer] le[ft] lgetb[uffer] l[ist] lN lNf lo[adview] lpf[ile] luado mak[e] menut[ranslate] mks[ession] mod[e] mzf[ile] nbs[tart] nmapc[lear] ol[dfiles] p ped[it] po[p] pre[serve] prof[ile] ptf[irst] ptN[ext] py q re red[o] Renu rightb[elow] rubyf[ile] sa[rgument] sbn[ext] scripte[ncoding] setg[lobal] sig sl[eep] sN[ext] so spe[llgood] spr[evious] startg[replace] sts[elect] s?version?main?:p syncbind tabd[o] tabN tabr[ewind] tclf[ile] tm TOhtml try una[bbreviate] unlo[ckvar] ve[rsion] vi[sual] wa[ll] win[size] w[rite] xa[ll] XMLns xwininfo
syn keyword viseCommand contained	Allargs argl[ocal] bar bn[ext] breaka[dd] bu bw[ipeout] caddf[ile] cd cgete[xpr] checkt[ime] cmdname cnf com con[tinue] cq[uit] cw[indow] delc[ommand] diffg[et] diffpu[t] dig[raphs] dr[op] earlier e:e emenu* en[dif] exi[t] filet fir[st] foldo[pen] get gvim helpt[ags] iabc[lear] index ju[mps] l lan lc[d] lefta[bove] lgete[xpr] ll lne lnf[ile] locale lp[revious] luafile Man mes mksp[ell] m[ove] mz[vise] ne noa omapc[lear] p: pe[rl] popu prev[ious] promptf[ind] ptj[ump] ptp[revious] py3 qa[ll] r:e redr[aw] res[ize] r:r rundo sav[eas] sbN[ext] scrip[tnames] setl[ocal] sign sm[agic] sni[ff] sor[t] spelli[nfo] sre[wind] star[tinsert] sun[hide] sv[iew] synlist tabe[dit] tabnew tabs te[aroff] tm[enu] to[pleft] ts[elect] u[ndo] uns[ilent] vert[ical] viu[sage] wh[ile] wn[ext] ws[verb] x[it] xnoreme y[ank]

syntax region viseMultilineComment start=/#|/ end=/|#/ contains=viseMultilineComment

" #/xxx/ are the special Gauche identifiers for regexp
syn region viseRegexp start=+\%(\\\)\@<!#/+ skip=+\\[\\/]+ end=+/+

" anything limited by |'s is identifier
syn match viseOther oneline    "|[^|]\+|"

" includ
syn match	viseInclude	oneline    "(include .*)"

syn keyword viseAutoEvent BufAdd
syn keyword viseAutoEvent BufCreate
syn keyword viseAutoEvent BufDelete
syn keyword viseAutoEvent BufEnter
syn keyword viseAutoEvent BufFilePost
syn keyword viseAutoEvent BufFilePre
syn keyword viseAutoEvent BufHidden
syn keyword viseAutoEvent BufLeave
syn keyword viseAutoEvent BufNew
syn keyword viseAutoEvent BufNewFile
syn keyword viseAutoEvent BufRead
syn keyword viseAutoEvent BufReadCmd
syn keyword viseAutoEvent BufReadPost
syn keyword viseAutoEvent BufReadPre
syn keyword viseAutoEvent BufUnload
syn keyword viseAutoEvent BufWinEnter
syn keyword viseAutoEvent BufWinLeave
syn keyword viseAutoEvent BufWipeout
syn keyword viseAutoEvent BufWrite
syn keyword viseAutoEvent BufWriteCmd
syn keyword viseAutoEvent BufWritePost
syn keyword viseAutoEvent BufWritePre
syn keyword viseAutoEvent Cmd-event
syn keyword viseAutoEvent CmdwinEnter
syn keyword viseAutoEvent CmdwinLeave
syn keyword viseAutoEvent Colorvise
syn keyword viseAutoEvent CursorHold
syn keyword viseAutoEvent CursorHoldI
syn keyword viseAutoEvent CursorMoved
syn keyword viseAutoEvent CursorMovedI
syn keyword viseAutoEvent EncodingChanged
syn keyword viseAutoEvent FileAppendCmd
syn keyword viseAutoEvent FileAppendPost
syn keyword viseAutoEvent FileAppendPre
syn keyword viseAutoEvent FileChangedRO
syn keyword viseAutoEvent FileChangedShell
syn keyword viseAutoEvent FileChangedShellPost
syn keyword viseAutoEvent FileEncoding
syn keyword viseAutoEvent FileReadCmd
syn keyword viseAutoEvent FileReadPost
syn keyword viseAutoEvent FileReadPre
syn keyword viseAutoEvent FileType
syn keyword viseAutoEvent FileWriteCmd
syn keyword viseAutoEvent FileWritePost
syn keyword viseAutoEvent FileWritePre
syn keyword viseAutoEvent FilterReadPost
syn keyword viseAutoEvent FilterReadPre
syn keyword viseAutoEvent FilterWritePost
syn keyword viseAutoEvent FilterWritePre
syn keyword viseAutoEvent FocusGained
syn keyword viseAutoEvent FocusLost
syn keyword viseAutoEvent FuncUndefined
syn keyword viseAutoEvent GUIEnter
syn keyword viseAutoEvent GUIFailed
syn keyword viseAutoEvent InsertChange
syn keyword viseAutoEvent InsertEnter
syn keyword viseAutoEvent InsertLeave
syn keyword viseAutoEvent MenuPopup
syn keyword viseAutoEvent QuickFixCmdPost
syn keyword viseAutoEvent QuickFixCmdPre
syn keyword viseAutoEvent RemoteReply
syn keyword viseAutoEvent SessionLoadPost
syn keyword viseAutoEvent ShellCmdPost
syn keyword viseAutoEvent ShellFilterPost
syn keyword viseAutoEvent SourceCmd
syn keyword viseAutoEvent SourcePre
syn keyword viseAutoEvent SpellFileMissing
syn keyword viseAutoEvent StdinReadPost
syn keyword viseAutoEvent StdinReadPre
syn keyword viseAutoEvent SwapExists
syn keyword viseAutoEvent Syntax
syn keyword viseAutoEvent TabEnter
syn keyword viseAutoEvent TabLeave
syn keyword viseAutoEvent TermChanged
syn keyword viseAutoEvent TermResponse
syn keyword viseAutoEvent User
syn keyword viseAutoEvent UserGettingBored
syn keyword viseAutoEvent VimEnter
syn keyword viseAutoEvent VimLeave
syn keyword viseAutoEvent VimLeavePre
syn keyword viseAutoEvent VimResized
syn keyword viseAutoEvent WinEnter
syn keyword viseAutoEvent WinLeave

" meddlesome
set ts=8 sts=2 sw=2 et nocindent lisp


" Synchronization and the wrapping up...

syn sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_vise_syntax_inits")
  if version < 508
    let did_vise_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink viseCommand		Statement
  HiLink viseAutoEvent		Type

  HiLink viseSyntax		Statement
  HiLink viseFunc		Function

  HiLink viseString		String
  HiLink viseChar		Character
  HiLink viseNumber		Number
  HiLink viseBoolean		Boolean

  HiLink viseDelimiter	Delimiter
  HiLink viseConstant		Constant

  HiLink viseComment		Comment
  HiLink viseMultilineComment	Comment
  HiLink viseError		Error

  HiLink viseRegexp		viseString
  HiLink schemeSrfi62Comment	viseComment
  HiLink viseInclude		Include

  delcommand HiLink
endif

let b:current_syntax = "vise"
