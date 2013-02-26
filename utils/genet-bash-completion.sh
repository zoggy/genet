# genet command line completion for bash.

_genet()
{
  local cur res
  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}
  #echo ${COMP_WORDS[@]} >> log
  res=`genet-cheat ${COMP_CWORD} ${COMP_WORDS[@]}`
  #echo $res
  COMPREPLY=($( eval compgen ${res} "$cur"))
  #COMPREPLY=($( eval compgen -f "$cur"))
  return 0
}

complete -o filenames -F _genet genet
