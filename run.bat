@echo off
echo alte output.txt loeschen
rm "bin/output.txt"
echo output start > bin/output.txt

echo loop starten
for %%f in (input*.txt) do (
        echo input datei %%~nf kopieren...
		xcopy /Y %%~nf.txt "bin/input.txt"
		echo programm starten...
		start "bin" Main.exe
)
echo output kopieren
xcopy /f /Y "bin/output.txt" "output.txt"
pause