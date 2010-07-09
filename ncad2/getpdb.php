<?php
   $handle = fopen("http://www.rcsb.org/pdb/files/1A3K.pdb", "r");
   $contents = "";
   while (!feof($handle)) {
       $contents .= fread($handle, 8192);
   }
   fclose($handle);
   header("Content-Type: text/plain");
   echo $contents;
?>
