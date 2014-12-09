	Dim inicio, fim as Variant
		inicio = CDate(de.Value)
		fim    = CDate(ate.Value)
		
	Dim srv as Server
	Dim pts as PIPoints
	Dim pt as PIPoint
	Dim dt as PIData
	Dim vlrs as PIValues
	Dim rs as ADODB.Recordset

	Set srv  = Servers.DefaultServer
	Set pts  = srv.PIPoints
	Set pt   = pts.item(TagName(ld))
	Set dt   = pt.Data
	Set vlrs = dt.RecordedValues(inicio, fim)
	Set rs   = vlrs.Recordset
