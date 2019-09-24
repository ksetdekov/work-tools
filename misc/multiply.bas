Attribute VB_Name = "Module1"
Sub multipyby1()
'
' multipyby1 Macro
'

'
    Range("BB2").FormulaR1C1 = "1"
    Range("BB2").Copy
    Range(Selection, Selection.End(xlDown)).Select
    Selection.PasteSpecial Paste:=xlPasteAll, Operation:=xlMultiply, _
        SkipBlanks:=False, Transpose:=False
End Sub


