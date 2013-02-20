" Vim plugin file
" Language:	vise
" Last Change:	2013 Feb 20
" Maintainer:	aharisu <foo.yobina@gmail.com>

function! s:genvise()
  echo "generating VimScript ..."
  let srcfile = expand('%:p')
  let outfile = expand('%:p:r') . '.vim'
  let genvise = globpath(&rtp, 'autoload/genvise.scm')
  let g:hoge = 'gosh ' . shellescape(genvise) . ' -o ' . shellescape(outfile) . ' ' . shellescape(srcfile)
  let ret =  system('gosh ' . shellescape(genvise) . ' -o ' . shellescape(outfile) . ' ' . shellescape(srcfile))
  if v:shell_error
    echohl Error
    for line in split(ret, '\n')
      echomsg substitute(line, '\t', '  ', 'g')
    endfor
    echohl None
  else
    redraw | echo "finish: " . outfile
  endif
endfunction
command! -buffer -nargs=0 Genvise :call s:genvise()
