<?php
  header('Content-Type: application/xml');
  $tmpFile = '/tmp/tmpFile.txt';
  $val = $_GET["fwd"];

  $curlHandle = curl_init($val);  
  $filePointer = fopen($tmpFile, "w");  
  curl_setopt($curlHandle, CURLOPT_FILE, $filePointer);  
  curl_exec($curlHandle);
  curl_close($curlHandle);
  fclose($filePointer); 

  $linesArr = file($tmpFile);  
  foreach($linesArr as $eachLine){
    echo($eachLine);
  }
?>
