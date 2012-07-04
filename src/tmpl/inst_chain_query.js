function myobject(v)
{
	this.value = v;
}
var the_input = new myobject('') ;
var the_chain = new myobject('') ;

function filter()
{
  var xmlhttp;
  if (window.XMLHttpRequest)
   {// code for IE7+, Firefox, Chrome, Opera, Safari
    xmlhttp=new XMLHttpRequest();
   }
  else
   {// code for IE6, IE5
    xmlhttp=new ActiveXObject('Microsoft.XMLHTTP');
   }
 query = '/inst-chains?query=1\x26input='+the_input.value+'\x26chain='+the_chain.value ;
  document.getElementById('query').innerHTML=query;
  xmlhttp.open('GET',query,false);
  xmlhttp.send();

  if (xmlhttp.status==200)
    {
     document.getElementById('qresult').innerHTML=xmlhttp.responseText;
    }
  else
    {
     document.getElementById('qresult').innerHTML='Error';
    }

}
function onChange(the_var, input)
{
  the_var.value = input.options[input.selectedIndex].value ;
  //alert(the_var.value + 'chain=' + the_chain.value + ' input=' + the_input.value);
  filter() ;
}