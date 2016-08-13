@Echo off

XmlExport.exe out\Distr_dist_000_01_Customer_20160212210329.xml DISTR_CONN_ID ID=dist_000_01 2>>xmlout.err
if errorlevel 1 goto :XmlError
del xmlout.err

curl\curl.exe -k -s -S -v --libcurl libcurl.txt --user test:testo  -T out\Distr_dist_000_01_Customer_20160212210329.xml sftp://192.168.1.88/trololo/ 2>curlout.err
if errorlevel 1 goto :SftpError
del curlout.err
del libcurl.txt

echo Формирование XML и выкладка на SFTP завершены без ошибок
Exit

:XmlError
echo XML ERROR!!!
:: Здесь надо бы реагировать на ошибку
echo -------------------------------------------------------------------------  >>  error.log
echo XML ERROR >> error.log
date /t >> error.log
time /t >> error.log
type xmlout.err >> error.log
exit

:SftpError
echo SFTP ERROR!!!
:: Здесь надо бы реагировать на ошибку
echo -------------------------------------------------------------------------  >>  error.log
echo SFTP ERROR >> error.log
date /t >> error.log
time /t >> error.log
type curlout.err >> error.log
type libcurl.txt >> error.log
exit

