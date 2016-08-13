@echo off
call sftpsetup
curl\curl.exe -k --user %sftpUser%:%sftpPassw% %sftpServer%