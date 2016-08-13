@Echo off
call usedate
call sftpsetup
set xmlfile=out\Distr_dist_000_01_Customer_%YYYY%%MM%%DD%%HH%%NN%00.xml

XmlExport.exe %xmlfile% DISTR_CONN_ID ID=dist_000_01 2>>xmlout.err
if errorlevel 1 goto :XmlError
del xmlout.err

curl\curl.exe -k -s -S -v --libcurl libcurl.txt --user %sftpUser%:%sftpPassw%  -T %xmlfile% %sftpserver% 2>curlout.err
if errorlevel 1 goto :SftpError
del curlout.err
del libcurl.txt

echo ”®à¬¨à®¢ ­¨¥ XML ¨ ¢ëª« ¤ª  ­  SFTP § ¢¥àè¥­ë ¡¥§ ®è¨¡®ª
Exit

:XmlError
echo XML ERROR!!!
:: Çäåñü íàäî áû ðåàãèðîâàòü íà îøèáêó
echo -------------------------------------------------------------------------  >>  error.log
echo XML ERROR >> error.log
date /t >> error.log
time /t >> error.log
type xmlout.err >> error.log
exit

:SftpError
echo SFTP ERROR!!!
:: Çäåñü íàäî áû ðåàãèðîâàòü íà îøèáêó
echo -------------------------------------------------------------------------  >>  error.log
echo SFTP ERROR >> error.log
date /t >> error.log
time /t >> error.log
type curlout.err >> error.log
type libcurl.txt >> error.log
exit

