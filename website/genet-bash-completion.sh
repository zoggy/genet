# genet command line completion for bash.

_genet()
{
  local cur res
  COMP_WORDBREAKS=${COMP_WORDBREAKS//:}
  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}
  res=`genet-cheat ${COMP_CWORD} ${COMP_WORDS[@]}`
  #echo $res
  COMPREPLY=($( eval compgen ${res} "$cur"))
  return 0
}
#complete -f -F _genet genet
complete -F _genet genet
