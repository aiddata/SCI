BinCheck = function(b, naVal="NA")
{
  bSet = unique(b)
  bSet = bSet[!is.na(bSet)]
  
  if(any(as.integer(bSet) != bSet)) "con"
  else if (length(bSet) > 2) "con"
  else "bin"
}