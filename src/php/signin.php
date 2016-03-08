<?php
   if(empty($_GET["par"])) {
     exit();
   }

  // get Info
  $json = $_GET["par"];
  $signin = json_decode($json);
  
  $password = $signin->{'password'};

  if($password == "R1f23lp15") {
      echo '{ "ok" : true, "r_error" : ""}';
  } else {
      echo '{ "ok" : false, "r_error" : "wrong password"}';
  }
?>