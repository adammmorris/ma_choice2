<?php 
$filename = "counter.txt";
$handle= fopen($filename, "r"); // Open the file for reading 
$count = fread($handle, filesize($filename)); // Get the existing count 
ftruncate($handle,0); // Delete the current count in the text file
fclose($handle); // Close the file 
$count++; // This acts like "$count = $count + 1" -- Just adding 1 to $count
$handle = fopen($filename, "w"); //Open the file again, for WRITING this time
fwrite($handle, $count); // Write the new count to the file 
fclose($handle); // Close the file 
echo $count; // Output to update the views live
?>

