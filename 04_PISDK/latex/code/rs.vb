	Dim rs as ADODB.Recordset
	Set rs = ...

	rs.MoveFirst
	While(Not rs.EOF)
		rs. ...
		rs.MoveNext
	Wend