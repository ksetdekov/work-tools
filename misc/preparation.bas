Attribute VB_Name = "preparation"
Function Translit(ByVal txt As String) As String
    iRussian$ = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
    iTranslit = Array("", "a", "b", "v", "g", "d", "e", "jo", "zh", "z", "i", "jj", "k", _
                      "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "c", "ch", _
                      "sh", "zch", "''", "'y", "'", "eh", "ju", "ja")
    For iCount% = 1 To 33
        txt = Replace(txt, Mid(iRussian$, iCount%, 1), iTranslit(iCount%), , , vbTextCompare)
    Next
    Translit$ = txt
End Function

Function translitback(ByVal txt As String) As String
    iRussian$ = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
    iTranslit = Array("", "a", "b", "v", "g", "d", "e", "jo", "zh", "z", "i", "jj", "k", _
                      "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "c", "ch", _
                      "sh", "zch", "''", "'y", "'", "eh", "ju", "ja")
    For iCount% = 1 To 33
        txt = Replace(txt, iTranslit(iCount%), Mid(iRussian$, iCount%, 1), , , vbTextCompare)
    Next
    translitback$ = txt
End Function
Public Sub translitsback()
nrow = ActiveSheet.UsedRange.Rows.Count
ncol = ActiveSheet.UsedRange.Columns.Count
 currcol = 4
    currrow = 1
    

        Do While currrow <= nrow
            ActiveSheet.Cells(currrow, currcol) = translitback(ActiveSheet.Cells(currrow, currcol))
            currrow = currrow + 1
        Loop
   
End Sub

Public Sub preparation()
 '   ActiveWorkbook.Save
        toreopen = Application.ActiveWorkbook.Path & "\" & ActiveWorkbook.Name
' get file name
Filename = ActiveWorkbook.Name
If InStr(Filename, ".") > 0 Then
   Filename = Left(Filename, InStr(Filename, ".") - 1)
End If
' counter init
Dim x               As Integer
Dim MyTimer         As Double


sourcesheet = ActiveSheet.Name
nrow = ActiveSheet.UsedRange.Rows.Count
'nrow = 100
ncol = ActiveSheet.UsedRange.Columns.Count

    Sheets.Add After:=ActiveSheet
    ActiveSheet.Move Before:=Sheets(1)
    ActiveSheet.Name = "translit"
' copy unedited data
    Sheets(sourcesheet).Select
    Range("A1").Select
    Range(Selection, Selection.End(xlDown)).Select
    Range(Selection, Selection.End(xlToRight)).Select
    Selection.Copy
    Sheets("translit").Select
    ActiveSheet.Paste
    
    
    currcol = 1
    currrow = 1

' translit only 1 row and third column

    Do While currcol <= ncol
        Do While currrow <= 1
            ActiveSheet.Cells(currrow, currcol) = Translit(Sheets(sourcesheet).Cells(currrow, currcol))
            currrow = currrow + 1
        Loop
        currcol = currcol + 1
        currrow = 1
        x = currcol - 1
       DoEvents
         Application.StatusBar = "Progress, converting headers: " & x & " of " & ncol & " columns: " & Format(x / ncol, "0%")
    Loop
    
    
    currcol = 3
    currrow = 1
    

        Do While currrow <= nrow
            ActiveSheet.Cells(currrow, currcol) = Translit(Sheets(sourcesheet).Cells(currrow, currcol))
            currrow = currrow + 1
        Loop
   


       DoEvents
         Application.StatusBar = "Finished converting ID's"
Application.Wait (Now + TimeValue("00:00:02"))
    
    Application.StatusBar = False
    'saving
Application.DisplayAlerts = False

    ActiveWorkbook.SaveAs Filename:=Application.ActiveWorkbook.Path & "\" & Filename & "_converted.xlsm", _
    FileFormat:=xlOpenXMLWorkbookMacroEnabled, CreateBackup:=False
    ActiveWorkbook.SaveAs Filename:=Application.ActiveWorkbook.Path & "\" & Filename & "_converted.csv", FileFormat:=xlCSV, _
        CreateBackup:=False

        
                Workbooks.Open (toreopen)
                Workbooks(Filename & "_converted.csv").Close SaveChanges:=False
Application.DisplayAlerts = True


 
    
End Sub
