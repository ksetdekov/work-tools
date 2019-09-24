Attribute VB_Name = "Module1"
Sub Macro8()
Attribute Macro8.VB_ProcData.VB_Invoke_Func = " \n14"
'
' Macro1 Macro
'

'
Range(Selection, Selection.End(xlDown)).Select
Selection.NumberFormat = "0.00000000"
    Selection.AutoFilter
    Selection.AutoFilter Field:=1, Criteria1:= _
        "=1111111111111,00000000", Operator:=xlOr, Criteria2:="=9999999999999,00000000"
    Selection.NumberFormat = "0"
        ActiveSheet.ShowAllData
End Sub

Sub Macro4()
'
' Macro1 Macro
'

'
Range(Selection, Selection.End(xlDown)).Select
Selection.NumberFormat = "0.0000"
    Selection.AutoFilter
    Selection.AutoFilter Field:=1, Criteria1:= _
        "=1111111111111,0000", Operator:=xlOr, Criteria2:="=9999999999999,0000"
    Selection.NumberFormat = "0"
        ActiveSheet.ShowAllData
End Sub

Sub Macro1()
'
' Macro1 Macro
'

'
Range(Selection, Selection.End(xlDown)).Select
Selection.NumberFormat = "0.0"
    Selection.AutoFilter
    Selection.AutoFilter Field:=1, Criteria1:= _
        "=1111111111111,0", Operator:=xlOr, Criteria2:="=9999999999999,0"
    Selection.NumberFormat = "0"
        ActiveSheet.ShowAllData
End Sub

