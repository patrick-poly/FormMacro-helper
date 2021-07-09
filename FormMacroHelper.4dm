
// structure de l'object editor
// form : object -> Whole form - All modification in this object are ignored.
// name : form name
// formProperties : object ->The property of the current form.
// file : .4dform file object
// table : name
// currentPage.objects : object ->The current page
// currentSelection : collection -> Collection of the selected object name
// target : the name of the clicked object



Class constructor
	var $1 : Object
	
	This:C1470.editor:=$1.editor
	
Function strcmp
	var $1; $2 : Text
	var $0 : Boolean
	
	$0:=False:C215
	If (Length:C16($1)=Length:C16($2))
		$0:=Position:C15($1; $2; *)=1
	End if 
	
Function EncodeReservedFileCharacters
	var $1; $0 : Text
	var $c; $i; $reserved_count : Integer
	var $reserved; $hexa : Text
	
	$reserved:="1111111111"*3
	$reserved:=$reserved+"1100100100"
	$reserved:=$reserved+"0010000100"
	$reserved:=$reserved+"0000000010"
	$reserved:=$reserved+"1011000000"
	$reserved:=$reserved+("0000000000"*2)
	$reserved:=$reserved+"0010000000"
	$reserved:=$reserved+("0000000000"*2)
	$reserved:=$reserved+"000010000"
	
	$hexa:="0123456789ABCDEF"
	
	$reserved_count:=Length:C16($reserved)
	
	For ($i; 1; Length:C16($1))
		$c:=Character code:C91($1[[$i]])
		If ($c<$reserved_count) & ($reserved[[$c+1]]#"0")
			$0:=$0+"%"
			$0:=$0+$hexa[[($c\16)+1]]
			$0:=$0+$hexa[[($c%16)+1]]
		Else 
			$0:=$0+Char:C90($c)
		End if 
	End for 
	
Function DecodeReservedFileCharacters
	
	var $1; $0 : Text
	var $c; $v; $ii; $i; $len : Integer
	
	$len:=Length:C16($1)
	
	For ($i; 1; $len)
		If ($1[[$i]]="%")
			If (($i+2)<=$len)
				$v:=0
				For ($ii; 1; 2)
					$c:=Character code:C91($1[[$i+$ii]])
					Case of 
						: (($c>=Character code:C91("0")) & ($c<Character code:C91("9")))
							$v:=$v*16
							$v:=$v+($c-Character code:C91("0"))
						: (($c>=Character code:C91("A")) & ($c<Character code:C91("F")))
							$v:=$v*16
							$v:=$v+($c-Character code:C91("A"))+10
						: (($c>=Character code:C91("a")) & ($c<Character code:C91("f")))
							$v:=$v*16
							$v:=$v+($c-Character code:C91("a"))+10
					End case 
				End for 
				$0:=$0+Char:C90($v)
				$i:=$i+2
			Else 
				$0:=$0+$1[[$i]]
			End if 
		Else 
			$0:=$0+$1[[$i]]
		End if 
	End for 
	
	
Function _BuildObjectGroups
	var $0 : Object
	var $1 : Integer
	
	var $in_PageNumber : Integer
	
	$in_PageNumber:=$1
	
	var $pageGroup; $page; $allGroups : Object
	var $objects : Collection
	var $groupName; $objName; $objectType : Text
	var $done : Boolean
	var $foundInPage : Integer
	
	$done:=False:C215
	$allGroups:=Null:C1517
	$pageGroup:=Null:C1517
	$page:=This:C1470._GetPage($in_PageNumber)
	
	If ($page#Null:C1517)
		
		If (This:C1470.editor.form.editor#Null:C1517)
			$allGroups:=This:C1470.editor.form.editor.groups
		End if 
		
		If ($allGroups#Null:C1517)
			
			$pageGroup:=New object:C1471
			
			For each ($groupName; $allGroups)
				$objects:=$allGroups[$groupName]
				If ($objects.length>0)
					$done:=False:C215
					For each ($objName; $objects) Until ($done=True:C214)
						If ($objName#"")
							If (This:C1470.FormObjectExist($objName; True:C214; ->$foundInPage))
								If ($foundInPage=$in_PageNumber)
									OB SET:C1220($pageGroup; $groupName; $objects.copy())
								End if 
								$done:=True:C214
							End if 
						End if 
					End for each 
				End if 
			End for each 
		End if 
	End if 
	
	If ($pageGroup#Null:C1517)
		If (OB Is empty:C1297($pageGroup))
			CLEAR VARIABLE:C89($pageGroup)
		End if 
	End if 
	
	$0:=$pageGroup
	
Function _GetPage
	var $1 : Integer
	var $0 : Object
	$0:=Null:C1517
	
	If (($1>=0) & ($1<=This:C1470.GetPageCount()))
		$0:=This:C1470.editor.form.pages[$1]
	Else 
		If ($1=-1)
			$0:=This:C1470.editor.currentPage
		End if 
	End if 
	
Function _GetObjectInPage
	
	var $0; $1 : Object
	var $2 : Text
	var $3 : Pointer
	
	var $in_page : Object
	var $in_inName : Text
	var $out_objecttype : Pointer
	
	$in_page:=$1
	$in_inName:=$2
	$out_objecttype:=$3
	
	var $found : Boolean
	var $objectName; $objectType : Text
	var $object; $col : Object
	
	$out_objecttype->:=""
	
	If (($in_page#Null:C1517) & ($in_inName#""))
		
		If ($in_page.objects#Null:C1517)
			For each ($objectName; $in_page.objects) While ($found=False:C215)
				
				$object:=$in_page.objects[$objectName]
				$objectType:=$object.type
				
				//If (This.strcmp($objectName;$in_inName))
				If ($objectName=$in_inName)
					$found:=True:C214
					$0:=$object
					$out_objecttype->:=$objectType
				Else 
					
					If (This:C1470.strcmp($objectType; "listbox"))
						For each ($col; $object.columns) While ($found=False:C215)
							If ($col.name=$in_inName)
								//If (This.strcmp($col.name;$in_inName))
								$found:=True:C214
								$0:=$col
								$out_objecttype->:="columns"
							Else 
								If ($col.header.name=$in_inName)
									//If (This.strcmp($col.header.name;$in_inName))
									$found:=True:C214
									$0:=$col.header
									$out_objecttype->:="header"
								Else 
									If ($col.footer.name=$in_inName)
										//If (This.strcmp($col.footer.name;$in_inName))
										$found:=True:C214
										$0:=$col.footer
										$out_objecttype->:="footer"
									End if 
								End if 
							End if 
						End for each 
						
					End if 
				End if 
				
			End for each 
		End if 
	End if 
	
Function _GetObjectNameInPage
	
	var $0 : Collection
	var $1 : Object
	var $in_page : Object
	
	$in_page:=$1
	
	var $found : Boolean
	var $objectName; $objectType : Text
	var $object; $col : Object
	
	$0:=New collection:C1472
	
	If (($in_page#Null:C1517))
		
		If ($in_page.objects#Null:C1517)
			For each ($objectName; $in_page.objects)
				$object:=$in_page.objects[$objectName]
				
				$0.push($objectName)
				
				If (This:C1470.strcmp($object.type; "listbox"))
					// not a good practice, but
					// sub object can have empty or not defined name. In this case , unique name will be generated when form will be loaded
					
					For each ($col; $object.columns)
						If ($col.name#"")
							$0.push($col.name)
						End if 
						If (OB Is defined:C1231($col; "header"))
							If ($col.header.name#"")
								$0.push($col.header.name)
							End if 
						End if 
						If (OB Is defined:C1231($col; "footer"))
							If ($col.footer.name#"")
								$0.push($col.footer.name)
							End if 
						End if 
					End for each 
				End if 
			End for each 
		End if 
	End if 
	
Function GetObjectDefaultValues
	var $0 : Object
	var $1 : Text
	
	var $in_objecttype : Text
	
	$in_objecttype:=$1
	
	var $DefaultJsonFormPath; $txt : Text
	
	If (This:C1470.DefaultValues=Null:C1517)
		Case of 
			: (Is Windows:C1573)
				$DefaultJsonFormPath:=(Path to object:C1547(Application file:C491; Path is system:K24:25).parentFolder)+"Resources"+Folder separator:K24:12+"DefaultJsonForm.json"
			: (Is macOS:C1572)
				$DefaultJsonFormPath:=Application file:C491+Folder separator:K24:12+"Contents"+Folder separator:K24:12+"Resources"+Folder separator:K24:12+"DefaultJSONForm.json"
		End case 
		If ($DefaultJsonFormPath#"")
			$txt:=File:C1566($DefaultJsonFormPath; fk platform path:K87:2).getText()
			This:C1470.DefaultValues:=JSON Parse:C1218($txt; Is object:K8:27)
		End if 
	End if 
	
	If (This:C1470.DefaultValues#Null:C1517)
		If ($in_objecttype#"")
			$0:=This:C1470.DefaultValues[$in_objecttype]
		Else 
			$0:=This:C1470.DefaultValues
		End if 
	End if 
	
Function GetFormName
	var $0 : Text
	$0:=This:C1470.editor.name
	
Function GetPageCount
	var $0 : Integer
	$0:=This:C1470.editor.form.pages.length
	
Function GetCurrentPageNumber
	var $0 : Integer
	$0:=This:C1470.editor.currentPageNumber
	
	
Function GetObjectMethodFolder
	var $0 : Object
	$0:=This:C1470.editor.file.parent.folder("ObjectMethods")
	
Function GetFormFolder
	var $0 : Object
	$0:=This:C1470.editor.file.parent
	
Function GetSelection
	var $0 : Collection
	If (This:C1470.editor.currentSelection=Null:C1517)
		This:C1470.editor.currentSelection:=New collection:C1472
	End if 
	$0:=This:C1470.editor.currentSelection
	
Function GetSelectedObjectCount
	var $0 : Integer
	$0:=This:C1470.GetSelection().length
	
Function DeselectAll
	var $0 : Boolean
	This:C1470.GetSelection().clear()
	
Function SelectAll
	var $0 : Boolean
	var $col : Collection
	var $objects : Object
	var $property : Text
	
	$col:=This:C1470.GetSelection()
	$objects:=This:C1470.editor.currentPage.objects
	
	For each ($property; $objects)
		$col.push($property)
	End for each 
	
Function SelectObject
	var $0 : Boolean
	var $1 : Text
	var $col : Collection
	$col:=This:C1470.GetSelection()
	
	If ($1#"")
		If ($col.indexOf($1)=-1)
			$col.push($1)
		End if 
	End if 
	
Function DeselectObject
	var $0 : Boolean
	var $1 : Text
	var $col : Collection
	var $ind : Integer
	$col:=This:C1470.GetSelection()
	
	If ($1#"")
		$ind:=$col.indexOf($1)
		If ($ind#-1)
			$col.remove($ind)
		End if 
	End if 
	
Function IsSelected
	var $0 : Boolean
	var $1 : Text
	$0:=False:C215
	If ($1#"")
		$0:=(This:C1470.GetSelection().indexOf($1)#-1)
	End if 
	
	
Function GetFormObject
	var $0 : Object
	var $1 : Text
	var $2 : Integer
	var $3 : Pointer
	
	var $in_objectName : Text
	var $in_pageNumber : Integer
	var $out_objectType : Pointer
	
	$in_objectName:=$1
	$in_pageNumber:=$2
	$out_objectType:=$3
	
	var $page : Object
	var $pagelist : Collection
	var $ind : Integer
	
	// page number
	// >=0 look in editor.form[page number]  The returned object is readonly
	// -1 look in editor.currentPage  The returned object is readonly  
	// -2 look in editor.currentPage for current page , and editor.form for other.returned object can be r/o or r/w
	//  -3 look in editor.form   The returned object is readonly
	
	$out_objectType->:=""
	
	If ($in_objectName#"")
		
		Case of 
				
			: ($in_pageNumber=-1)
				$0:=This:C1470._GetObjectInPage(This:C1470.editor.currentPage; $in_objectName; $out_objectType)
				
			: ($in_pageNumber=-2)
				
				$pagelist:=This:C1470.editor.form.pages
				If ($pagelist#Null:C1517)
					$ind:=0
					For each ($page; $pagelist) While ($0=Null:C1517)
						If ($ind=This:C1470.GetCurrentPageNumber())
							$page:=This:C1470.editor.currentPage
						End if 
						If ($page#Null:C1517)
							$0:=This:C1470._GetObjectInPage($page; $in_objectName; $out_objectType)
						End if 
						$ind:=$ind+1
					End for each 
				End if 
			: ($in_pageNumber=-3)
				
				$pagelist:=This:C1470.editor.form.pages
				If ($pagelist#Null:C1517)
					$ind:=0
					For each ($page; $pagelist) While ($0=Null:C1517)
						$0:=This:C1470._GetObjectInPage($page; $in_objectName; $out_objectType)
					End for each 
				End if 
			Else 
				
				$pagelist:=This:C1470.editor.form.pages
				If ($pagelist#Null:C1517)
					If ($in_pageNumber<$pagelist.length)
						$0:=This:C1470._GetObjectInPage($pagelist[$in_pageNumber]; $in_objectName; $out_objectType)
					End if 
				End if 
				
		End case 
	End if 
	
	
Function GetFormObjectNames
	var $0 : Collection
	var $1 : Integer
	
	var $in_pageNumber : Integer
	
	$in_pageNumber:=$1
	
	var $page : Object
	var $pagelist; $col : Collection
	var $ind; $curpagenumber : Integer
	
	// page number
	// >=0 look in editor.form[page number] 
	// -1 look in editor.currentPage
	// -2 look in editor.currentPage for current page , and editor.form for other.
	//  -3 look in editor.form 
	
	$0:=New collection:C1472
	
	Case of 
			
		: ($in_pageNumber=-1)
			$0:=$0.concat(This:C1470._GetObjectNameInPage(This:C1470.editor.currentPage))
			
		: ($in_pageNumber=-2)
			
			$pagelist:=This:C1470.editor.form.pages
			If ($pagelist#Null:C1517)
				$ind:=0
				$curpagenumber:=This:C1470.GetCurrentPageNumber()
				For each ($page; $pagelist)
					If ($ind=$curpagenumber)
						$page:=This:C1470.editor.currentPage
					End if 
					If ($page#Null:C1517)
						$0:=$0.concat(This:C1470._GetObjectNameInPage($page))
					End if 
					$ind:=$ind+1
				End for each 
			End if 
		: ($in_pageNumber=-3)
			
			$pagelist:=This:C1470.editor.form.pages
			If ($pagelist#Null:C1517)
				$ind:=0
				For each ($page; $pagelist) While ($0=Null:C1517)
					$0:=$0.concat(This:C1470._GetObjectNameInPage($page))
				End for each 
			End if 
		Else 
			
			$pagelist:=This:C1470.editor.form.pages
			If ($pagelist#Null:C1517)
				If ($in_pageNumber<$pagelist.length)
					$0:=$0.concat(This:C1470._GetObjectNameInPage($pagelist))
				End if 
			End if 
			
	End case 
	
	
Function FormObjectExist
	
	var $0; $2 : Boolean
	var $1 : Text
	var $3 : Pointer
	
	var $in_inName : Text
	var $in_useCurrentPage : Boolean
	var $out_pageNumber : Pointer
	var $pageNumber : Integer
	
	
	$in_inName:=$1
	$in_useCurrentPage:=$2
	
	If (Count parameters:C259=3)
		$out_pageNumber:=$3
	End if 
	
	var $pagelist : Collection
	var $ind : Integer
	var $page; $object : Object
	var $objectType : Text
	
	$0:=False:C215
	
	If ($in_inName#"")
		If (This:C1470.editor.form.pages#Null:C1517)
			$pagelist:=This:C1470.editor.form.pages
			$ind:=0
			For each ($page; $pagelist) While ($0=False:C215)
				
				If (($in_useCurrentPage=True:C214) & ($ind=This:C1470.GetCurrentPageNumber()))
					$page:=This:C1470.editor.currentPage
					$ind:=-1
				Else 
					
				End if 
				
				If ($page#Null:C1517)
					$object:=This:C1470._GetObjectInPage($page; $in_inName; ->$objectType)
				End if 
				
				$0:=($object#Null:C1517)
				
				If (Not:C34($0))
					$ind:=$ind+1
				End if 
				
			End for each 
		End if 
	End if 
	If (Not:C34(Is nil pointer:C315($out_pageNumber)))
		$out_pageNumber->:=$ind
	End if 
	
	
Function MakeUniqueObjectName
	var $0; $1 : Text
	var $2 : Collection
	var $3 : Boolean
	
	var $in_wantedName : Text
	var $in_exclusions : Collection
	var $in_withFreeMethodFile : Boolean
	
	$in_wantedName:=$1
	$in_exclusions:=$2
	$in_withFreeMethodFile:=$3
	$0:=""
	
	var $tmpName; $name; $tmpScriptName; $methodfolderpath : Text
	var $names; $groups : Collection
	var $found : Boolean
	var $counter : Integer
	
	// important
	// this code don't use strict string compare (This.strcmp) 
	// because 4D object name is not case sensitive 
	
	
	If ($in_wantedName#"")
		
		$names:=This:C1470.GetFormObjectNames(-2)
		$groups:=This:C1470.GetAllGroupNames()
		
		If ($names.length>0)
			
			$counter:=0
			$tmpName:=$in_wantedName
			
			If ($in_withFreeMethodFile)
				$methodfolderpath:=This:C1470.GetObjectMethodFolder().platformPath
			End if 
			
			Repeat 
				$found:=False:C215
				
				For each ($name; $names) While ($found=False:C215)
					$found:=($tmpName=$name)
				End for each 
				
				// groups
				If (($groups.length>0) & ($found=False:C215))
					For each ($name; $groups) While ($found=False:C215)
						$found:=($tmpName=$name)
					End for each 
				End if 
				
				// look in exclusion list
				If (($in_exclusions#Null:C1517) & ($found=False:C215))
					For each ($name; $in_exclusions) While ($found=False:C215)
						$found:=($tmpName=$name)
					End for each 
				End if 
				
				// test method file name
				If (($in_withFreeMethodFile) & ($found=False:C215))
					$tmpScriptName:=This:C1470.EncodeReservedFileCharacters($tmpName)
					$found:=(Test path name:C476($methodfolderpath+$tmpScriptName+".4dm")=Is a document:K24:1)
				End if 
				
				If ($found)
					$counter:=$counter+1
					$tmpName:=$in_wantedName+String:C10($counter)
				Else 
					$0:=$tmpName
				End if 
				
			Until ($0#"")
			
		Else 
			$0:=$in_wantedName
		End if 
		
	End if 
	
Function InsertFormObjectAfter
	var $2 : Object
	var $1; $3 : Text
	var $0 : Boolean
	
	var $in_newObjectName; $in_afterObjectName : Text
	var $in_newObject : Object
	
	$in_newObjectName:=$1
	$in_afterObjectName:=$3
	$in_newObject:=$2
	
	var $objects; $newObjects : Object
	var $property : Text
	
	$0:=False:C215
	$objects:=This:C1470.editor.currentPage.objects
	
	If ($in_newObjectName#"")
		If (Not:C34(OB Is defined:C1231($objects; $in_newObjectName)))
			If ($in_afterObjectName="")
				$objects[$in_newObjectName]:=$in_newObject
				$0:=True:C214
			Else 
				$newObjects:=New object:C1471
				For each ($property; $objects)
					$newObjects[$property]:=$objects[$property]
					If (This:C1470.strcmp($property; $in_afterObjectName))
						$newObjects[$in_newObjectName]:=$in_newObject
					End if 
				End for each 
				This:C1470.editor.currentPage.objects:=$newObjects
				$0:=True:C214
			End if 
		End if 
	End if 
	
Function InsertFormObjectBefore
	var $2 : Object
	var $1; $3 : Text
	var $0 : Boolean
	
	var $in_newObjectName; $in_beforeObjectName : Text
	var $in_newObject : Object
	
	$in_newObjectName:=$1
	$in_beforeObjectName:=$3
	$in_newObject:=$2
	
	var $objects; $newObjects : Object
	var $newObjectName; $property : Text
	
	$0:=False:C215
	$objects:=This:C1470.editor.currentPage.objects
	
	If ($newObjectName#"")
		If (Not:C34(OB Is defined:C1231($objects; $in_beforeObjectName)))
			
			$newObjects:=New object:C1471
			
			If ($in_beforeObjectName="")
				$newObjects[$in_newObjectName]:=$in_newObject
			End if 
			
			For each ($property; $objects)
				If ($in_beforeObjectName#"")
					If (This:C1470.strcmp($property; $in_beforeObjectName))
						$newObjects[$in_newObjectName]:=$in_newObject
					End if 
				End if 
				$newObjects[$property]:=$objects[$property]
			End for each 
			This:C1470.editor.currentPage.objects:=$newObjects
			$0:=True:C214
		End if 
	End if 
	
Function RenameObject
	var $1; $2 : Text
	var $0 : Boolean
	
	var $in_OldName; $in_NewName : Text
	
	$in_OldName:=$1
	$in_NewName:=$2
	
	var $objects; $newObjects : Object
	var $property : Text
	var $ind : Integer
	
	$0:=False:C215
	
	If (This:C1470.FormObjectExist($in_OldName; True:C214))
		If (Not:C34(This:C1470.FormObjectExist($in_NewName; True:C214)))
			$objects:=This:C1470.editor.currentPage.objects
			$newObjects:=New object:C1471
			For each ($property; $objects)
				//If (This.strcmp($property;$in_OldName))
				If ($property=$in_OldName)
					$newObjects[$in_NewName]:=$objects[$property]
				Else 
					$newObjects[$property]:=$objects[$property]
				End if 
			End for each 
			
			This:C1470.editor.currentPage.objects:=$newObjects
			
			If (This:C1470.IsSelected($in_OldName))
				This:C1470.DeselectObject($in_OldName)
				This:C1470.SelectObject($in_NewName)
			End if 
			
			$ind:=This:C1470.FindInEntryOrder($in_OldName)
			If ($ind#-1)
				This:C1470.GetEntryOrder()[$ind]:=$in_NewName
			End if 
			$0:=True:C214
			
		End if 
	End if 
	
Function ObjectGetX
	var $1 : Object
	var $0 : Integer
	$0:=$1.left
	
Function ObjectGetY
	var $1 : Object
	var $0 : Integer
	$0:=$1.top
	
Function ObjectGetRight
	var $1 : Object
	var $0 : Integer
	$0:=$1.left+This:C1470.ObjectGetWidth($1)
	
Function ObjectGetBottom
	var $1 : Object
	var $0 : Integer
	$0:=$1.top+This:C1470.ObjectGetHeight($1)
	
Function ObjectGetWidth
	var $1 : Object
	var $0 : Integer
	If (OB Is defined:C1231($1; "right"))
		$0:=$1.right-$1.left
	Else 
		$0:=$1.width
	End if 
	
Function ObjectSetWidth
	var $1 : Object
	var $2 : Integer
	var $w; $r : Boolean
	
	$r:=OB Is defined:C1231($1; "right")
	$w:=OB Is defined:C1231($1; "width")
	If (($r=True:C214) | (($r=False:C215) & ($w=False:C215)))
		$1.right:=$1.left+$2
	End if 
	If ($w)
		$1.width:=$2
	End if 
	
Function ObjectGetHeight
	var $1 : Object
	var $0 : Integer
	
	If (OB Is defined:C1231($1; "bottom"))
		$0:=$1.bottom-$1.top
	Else 
		$0:=$1.height
	End if 
	
Function ObjectSetHeight
	var $1 : Object
	var $2 : Integer
	var $b; $h : Boolean
	
	$b:=OB Is defined:C1231($1; "bottom")
	$h:=OB Is defined:C1231($1; "height")
	If (($b=True:C214) | (($b=False:C215) & ($h=False:C215)))
		$1.bottom:=$1.top+$2
	End if 
	If ($h)
		$1.height:=$2
	End if 
	
	
Function ObjectSetX
	var $1 : Object
	var $2 : Integer
	var $r; $w : Boolean
	
	$r:=OB Is defined:C1231($1; "right")
	$w:=OB Is defined:C1231($1; "width")
	If ($w | $r)
		If ($r)
			$1.right:=$2+($1.right-$1.left)
			$1.left:=$2
		End if 
		If ($w)
			$1.left:=$2
		End if 
	Else 
		ASSERT:C1129(False:C215)
	End if 
	
Function ObjectSetY
	var $1 : Object
	var $2 : Integer
	var $b; $h : Boolean
	
	$b:=OB Is defined:C1231($1; "bottom")
	$h:=OB Is defined:C1231($1; "height")
	If ($b | $h)
		If ($b)
			$1.bottom:=$2+($1.bottom-$1.top)
			$1.top:=$2
		End if 
		If ($h)
			$1.top:=$2
		End if 
	Else 
		ASSERT:C1129(False:C215)
	End if 
	
Function ObjectSetPos
	var $1 : Object
	var $2; $3 : Integer
	
	This:C1470.ObjectSetX($1; $2)
	This:C1470.ObjectSetY($1; $3)
	
Function GetObjectMethodInfo
	
	// $1 object
	// $2 object name
	
	// 0 none
	// 1 std method : methode name match object name, object method file is the ObjectMethods folder
	// 2 project method
	// 3 custom
	
	var $0 : Integer
	var $1 : Object
	var $2 : Text
	
	var $in_object : Object
	var $in_objectName : Text
	var $encodedMethodFileName : Text
	
	$in_object:=$1
	$in_objectName:=$2
	
	var $method; $ext; $objmethod : Text
	var $l : Integer
	
	$0:=0
	
	If ($in_object#Null:C1517)
		If (OB Is defined:C1231($in_object; "method"))
			
			$encodedMethodFileName:=This:C1470.EncodeReservedFileCharacters($in_objectName)
			
			$method:=$in_object.method
			If ($method#"")
				If (Length:C16($method)>5)
					If ($method="@.4dm")
						If ($method="ObjectMethods/@")
							$objmethod:=Substring:C12($method; 0; Length:C16($method)-4)
							$l:=Length:C16("ObjectMethods/")
							$objmethod:=Substring:C12($method; $l+1; Length:C16($objmethod)-$l)
							If (This:C1470.strcmp($objmethod; $encodedMethodFileName))
								$0:=1
							Else 
								$0:=3
							End if 
						Else 
							$0:=3
						End if 
					Else 
						$0:=2
					End if 
				Else 
					$0:=2
				End if 
			Else 
				$0:=2
			End if 
		End if 
	End if 
	
	
	
Function InsertInEntryOrderAfter
	var $1; $2 : Text
	var $0 : Boolean
	
	var $in_ObjectName; $in_afterObjectName : Text
	
	$in_ObjectName:=$1
	$in_afterObjectName:=$2
	$0:=False:C215
	
	var $ind : Integer
	var $entryOrder : Collection
	
	If ($in_ObjectName#"")
		
		$entryOrder:=This:C1470.editor.currentPage.entryOrder
		
		If (This:C1470.editor.currentPage.entryOrder=Null:C1517)
			$entryOrder:=New collection:C1472
			This:C1470.editor.currentPage.entryOrder:=$entryOrder
		End if 
		
		If ($entryOrder.indexOf($in_ObjectName)=-1)
			If ($in_afterObjectName="")
				$ind:=$entryOrder.length
			Else 
				$ind:=$entryOrder.indexOf($in_afterObjectName)
			End if 
			If ($ind>=0)
				$entryOrder.insert($ind+1; $in_ObjectName)
				$0:=True:C214
			End if 
		End if 
	End if 
	
Function InsertInEntryOrderBefore
	var $1; $2 : Text
	var $0 : Boolean
	
	var $in_ObjectName; $in_beforeObjectName : Text
	
	$in_ObjectName:=$1
	$in_beforeObjectName:=$2
	$0:=False:C215
	
	var $ind : Integer
	var $entryOrder : Collection
	
	If ($in_ObjectName#"")
		
		$entryOrder:=This:C1470.editor.currentPage.entryOrder
		
		If ($entryOrder=Null:C1517)
			$entryOrder:=New collection:C1472
			This:C1470.editor.currentPage.entryOrder:=$entryOrder
		End if 
		
		If ($entryOrder.indexOf($in_ObjectName)=-1)
			If ($in_beforeObjectName="")
				$ind:=0
			Else 
				$ind:=$entryOrder.indexOf($in_beforeObjectName)
			End if 
			If ($ind>=0)
				$entryOrder.insert($ind; $in_ObjectName)
				$0:=True:C214
			End if 
		End if 
	End if 
	
	
Function FindInEntryOrder
	var $0 : Integer
	var $1 : Text
	
	var $in_ObjectName : Text
	
	$in_ObjectName:=$1
	$0:=-1
	
	var $col : Collection
	
	$col:=This:C1470.editor.currentPage.entryOrder
	
	If ($col#Null:C1517)
		$0:=$col.indexOf($in_ObjectName)
	End if 
	
Function ClearEntryOrder
	var $1 : Boolean
	var $reset : Boolean
	
	$reset:=False:C215
	If (Count parameters:C259=1)
		$reset:=$1
	End if 
	If ($reset)
		OB REMOVE:C1226(This:C1470.editor.currentPage; "entryOrder")
	Else 
		If (This:C1470.editor.currentPage.entryOrder#Null:C1517)
			This:C1470.editor.currentPage.entryOrder.clear()
		End if 
	End if 
	
Function GetEntryOrder
	var $0 : Collection
	
	$0:=This:C1470.editor.currentPage.entryOrder
	
	
Function GetAllGroupNames
	var $0 : Collection
	var $groups : Object
	var $name : Text
	
	$0:=New collection:C1472
	
	If (This:C1470.editor.editor#Null:C1517)
		$groups:=This:C1470.editor.form.editor.groups
	Else 
		If (This:C1470.editor.form.editor#Null:C1517)
			$groups:=This:C1470.editor.form.editor.groups
		End if 
	End if 
	
	If ($groups#Null:C1517)
		For each ($name; $groups)
			$0.push($name)
		End for each 
	End if 
	