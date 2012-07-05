function myobject(v)
{
	this.value = v;
}
var the_input = new myobject('') ;
var the_chain = new myobject('') ;
var the_tools = {} ;

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
  // encode ampersands to prevent them from being escaped by xml templating system
  the_query = window.location.href +
     '?query=1\x26input='+the_input.value+'\x26chain='+the_chain.value+'\x26tools=' ;
  for (var k in the_tools) {
    // use hasOwnProperty to filter out keys from the Object.prototype
    if (the_tools.hasOwnProperty(k)) {
        v = the_tools[k] ;
        if (v != '') { the_query += v + ',' }
    }
  }
  document.getElementById('query').innerHTML=the_query;
  xmlhttp.open('GET',the_query,false);
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

function onToolChange (toolname, input)
{
  the_tools[toolname] = input.options[input.selectedIndex].value ;
  filter () ;
}

function onChange(the_var, input)
{
  the_var.value = input.options[input.selectedIndex].value ;
  //alert(the_var.value + 'chain=' + the_chain.value + ' input=' + the_input.value);
  filter() ;
}