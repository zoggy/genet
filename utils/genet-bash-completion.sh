# genet command line completion for bash.


_genet()
{
  local cur res
  cur=${COMP_WORDS[COMP_CWORD]}
  #echo ${COMP_WORDS[@]} >> log
  res=`genet-cheat ${COMP_CWORD} ${COMP_WORDS[@]}`
  COMPREPLY=( $(compgen -W "${res}" -- $cur) )
  return 0
}

complete -F _genet genet
