#/bin/bash
sudo echo "hi"
echo level 5 | sudo tee /proc/acpi/ibm/fan
echo $((16*1024*1024)) | sudo tee /proc/sys/vm/dirty_background_bytes
echo $((48*1024*1024)) | sudo tee /proc/sys/vm/dirty_bytes
