# httpText: Replace "<" by "&lt;" and "\n" by "<br/>"
proc httpText text {
	encoding convertto utf-8 [regsub -all "\[\r\n\]" [regsub -all "\r\n" [regsub -all "<" $text "&lt;"] "<br/>"] "<br/>"]
}

# httpAttrText: Replace {"<>} and control characters by their &#...; representation
proc httpAttrText text {
	for {set i 0} {$i < [string length $text]} {incr i} {
		scan [string range $text $i $i] %c x
		if {$x < 32 || $x == 34 || $x == 60 || $x == 62} {
			set pre [string range $text 0 [expr $i-1]]
			set post [string range $text [expr $i+1] end]
			set ante [format {&#%d;} $x]
			set text "$pre$ante$post"
			incr i [expr [string length $ante]-1]
		}
	}
	encoding convertto utf-8 $text
}

# httpUnicodeText: Convert text in system specific encoding to unicode. Should be called for strings stored in scripts
proc httpUnicodeText text {
	encoding convertfrom [encoding system] $text
}

# httpNormalize: Converts text in uri format to unicode
proc httpNormalize text {
	set parts {}
	foreach part [lrange [split $text "?"] 0 1] {
		set queries {}
		foreach query [split $part "&"] {
			set fragments {}
			foreach fragment [lrange [split $query "="] 0 1] {
				set value [lindex [split $fragment "#"] 0]
				set value [join [split $value "+"] " "]
				set pos 0
				while {[set pos [string first "%" $value $pos]] >= 0} {
					if {[scan [string range $value [expr $pos+1] [expr $pos+2]] "%x" cval] == 1} {
						set value "[string range $value 0 [expr $pos-1]][format "%c" $cval][string range $value [expr $pos+3] end]"
					}
					incr pos
				}
				lappend fragments [encoding convertfrom utf-8 $value]
			}
			lappend queries $fragments
		}
		eval "lappend parts $queries"
	}
	return $parts
}

# httpCheckName: Checks whether the given name is the name of an array variable.
#	Returns 1 if array variable for the name is not valid, otherwise 0
proc httpCheckName {name {local 0}} {
	if {[uplevel [set level [httpGetLevel $name 2]] "catch {set $name}"] != 0 && ($local == 0 || $level == "#0")} {
		return 0
	}
	return 1
}

# httpInitWidget: Initializes some common widet variables
#	scripting: 0 or 1. If 1, widget allows scripting, dependent on script option
proc httpInitWidget {name {scripting 0}} {
	upvar [httpGetLevel $name] $name opt
	set opt(style) ""
	set opt(class) ""
	if {$scripting} {
		set opt(script) 0
	}
}

# httpCreateWidgetAttributes: Returns Standard attributes, depending on standard options
proc httpCreateWidgetAttributes {name} {
	upvar [httpGetLevel $name] $name opt
	set ret ""
	set alwayswithid "Entry ChkButton RdButton Menu"
	if {([array names opt -exact "script"] != "" && $opt(script)) || [lsearch -exact $alwayswithid $opt(type)] >= 0} {
		set ret [format { id="%s"} [regsub -all "::" $name "_"]]
	}
	if {$opt(class) != ""} {
		set ret [format {%s class="%s"} $ret [httpAttrText $opt(class)]]
	}
	if {$opt(style) != ""} {
		set ret [format {%s style="%s"} $ret [httpAttrText $opt(style)]]
	}
	return $ret
}

# httpLink: Define values for a link. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	label: Name of link label
#	target: link address (uri format)
proc httpLink {name args} {
	if {[httpCheckName $name]} {
		error "Cannot create link $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name"
	set opt(label) ""
	set opt(target) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpLink option {$elem} in link $name"
		}
		set val [lindex $elem 1]
		if {$option == "label"} {
			upvar [set level [httpGetLevel $val]] $val lbopt
			if {[uplevel $level "httpGet $val type"] != {Label}} {
				unset opt
				error "Invalid option label in link $name: $val is not a label widget"
			}
			if {$level == 1} {
				global httpBeautifyHtml
				set option "tagdata"
				set saveit $httpBeautifyHtml
				set httpBeautifyHtml 0
				set val [uplevel 1 "httpCreateLabel $val {}"]
				set httpBeautifyHtml $saveit
			}
		}
		set opt($option) $val
	}
	if {$opt(target) == "" || ($opt(label) == "" && [array names opt -exact tagdata] == "")} {
		unset opt
		error "Label and target must be specified for link $name"
	}
	set opt(name) $name
	set opt(type) "Link"
	set opt(create) "httpCreateLink"
	return $name
}

# httpCreateLink: Create html link 
#	returns the corresponding html document fragment
proc httpCreateLink {name vars} {
	if {[uplevel 1 "httpGet $name type"] != {Link}} {
		error "Bad httpCreateLink widget $name, must be of type Link"
	}
	array set postvars $vars
	upvar [httpGetLevel $name] $name opt
	if {$opt(label) != ""} {
		upvar #0 $opt(label) label
		set tag [httpCreateLabel $opt(label) $vars]
	} {
		set tag $opt(tagdata)
	}
	set ret "[httpDeli [httpGet postvars namespace]]<a[uplevel 1 "httpCreateWidgetAttributes $name"] href=\"[uplevel 1 "httpAttrText {$opt(target)}"]\">$tag</a>"
	set opt(vars) {}
	return $ret
}

# httpLabel: Define values for text of bitmap label. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	text: Label contents (text label) or alternate text (in case of bitmap)
#	bitmap: link to bitmap (only for non-text label)
#	entry: Name of entry if bound to an entry, a button or a check box.
proc httpLabel {name args} {
	if {[httpCheckName $name]} {
		error "Cannot create label $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set opt(text) ""
	set opt(bitmap) ""
	set opt(entry) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpLabel option $elem in label $name"
		}
		set val [lindex $elem 1]
		if {"script" == $option && [lsearch -exact {0 1} $val] < 0} {
			unset opt
			error "Bad httpLabel option {$elem} in label $name"
		}
		if {"entry" == $option} {
			upvar #0 $val lbopt
			if {[array names lbopt -exact "create"] != "create"} {
				unset opt
				error "Invalid option entry in link $name: $val is not a widget"
			}
		}
		set opt($option) $val
	}
	set opt(name) $name
	set opt(type) "Label"
	set opt(create) "httpCreateLabel"
	return $name
}

# httpChangeWidgetText: Change the text of a label that supports scripting
#	returns script to be sent to browser to perform the text change
proc httpChangeWidgetText {name text fd} {
	upvar [httpGetLevel $name] $name opt
	
	if {[httpGet $name "script"] != 1} {
		error "Bad widget $name, must support scripting"
	}
	switch -exact $opt(type) "Label" {
			set what "innerHTML"
		} "Entry" {
			set what "value"
		} "Menu" {
			set what "value"
		} "Button" {
			set what "innerHTML"
		} "TextArea" {
			set what "innerHTML"
		} default {
			error "Bad widget $name, must be label, entry, menu or button"
		}
	if {$opt(script)} {
		set ret "[httpDeli $fd]<script>document.getElementById('[regsub -all "::" $name "_"]').$what = '[regsub -all "'" [httpText $text] "'+\"'\"+'"]';</script>"
		if [catch {puts -nonewline $fd $ret} err] {
			log warning "httpChangeWidgetText: Cannot change text of widget $name: $err" $fd
		} {
			log debug "SEND WIDGET TEXT: $ret" "$fd"
		}
	}
	return ""
}

# httpChangeWidgetStyle: Change widget style for a widget that supports scripting.
#	name: Widget name
#	value: New value of style attribute
#	fd: Client socket handle to be used to send script
proc httpChangeWidgetStyle {name value fd} {
	upvar [httpGetLevel $name] $name opt
	
	if {[httpGet $name "script"] == "1"} {
		set ret "[httpDeli $fd]<script>document.getElementById('[regsub -all "::" $name "_"]').style.cssText = '[regsub -all "'" [httpText $value] "'+\"'\"+'"]';</script>"
		if [catch {puts -nonewline $fd $ret} err] {
			log warning "httpChangeWidgetStyle: Cannot change style of widget $name: $err" $fd
		} {
			log debug "SEND WIDGET STYLE: $ret" "$fd"
		}
	}
	return ""
}

# httpChangeWidgetClass: Change widget class for a widget that supports scripting.
#	name: Widget name
#	style: Name of CSS class object property to be changed
#	value: New value of class object property
#	fd: Client socket handle to be used to send script
proc httpChangeWidgetClass {name class fd} {
	upvar [httpGetLevel $name] $name opt
	
	if {[httpGet $name "script"] == "1"} {
		set ret "[httpDeli $fd]<script>document.getElementById('[regsub -all "::" $name "_"]').ckassName = '$class';</script>"
		if [catch {puts -nonewline $fd $ret} err] {
			log warning "httpChangeWidgetClass: Cannot change class of widget $name: $err" $fd
		} {
			log debug "SEND WIDGET CLASS: $ret" "$fd"
		}
	}
	return ""
}

# httpCreateLabel: Create html text that specifies the label
#	returns the corresponding html document fragment
proc httpCreateLabel {name vars} {
	if {[uplevel 1 "httpGet $name type"] != {Label}} {
		error "Bad httpLabel widget $name, must be of type Label"
	}
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	set ret [uplevel 1 "httpCreateWidgetAttributes $name"]
	if {$opt(bitmap) == ""} {
		if {$opt(entry) != ""} {
			set tagname "label"
			append ret " for=\"[regsub -all "::" $opt(entry) "_"]\""
			set mask "<%s%s>%s</%s>%s"
			set script ""
		} {
			set tagname "span"
			set mask "<%s%s>%s</%s>%s"
			set script ""
		}
		set ret [format $mask $tagname $ret [httpText $opt(text)] $tagname $script]
	} {
		set ret "<img$ret src=\"[httpAttrText $opt(bitmap)]\""
		if {$opt(text) != ""} {
			append ret " alt=\"[httpAttrText $opt(text)]\""
		} {
			append ret " alt=\"[httpAttrText "($opt(bitmap))"]\""
		}
		append ret " />"
	}
	set opt(vars) {}
	return "[httpDeli [httpGet postvars namespace]]$ret"
}


# httpEntry: Define values for an entry fied. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	value: Entry contents
#	rows:  Number of rows to be shown
#	columns: Number of columns for each row
#	maxlen: Maximum length of text in text area, in characters
#	readonly: Specifies that contents can be changed (0) or not (1)
#	wrap:  One of "hard" or "soft", specifies whether line wrapping characgters shall be submitted
#		(hard) or not (soft) 
#	hidden: Specifies whether widget shall be shown (0) or not (1)
#	varname: Name of variable that holds the contents of the text area
#	placeholder: String to be shown if entry is empty
proc httpTextArea {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create text area $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set opt(value) ""
	set opt(rows) 1
	set opt(columns) 80
	set opt(maxlen) ""
	set opt(placeholder) ""
	set opt(readonly) 0
	set opt(wrap) "hard"
	set allowedwrap {"hard" "soft"}
	set opt(hidden) 0
	set opt(varname) [regsub -all "::" $name "_"]
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpEntry option $elem in entry $name"
		}
		set val [lindex $elem 1]
		if {$option == "wrap" && [lsearch -exact $allowedwrap $val] < 0} {
			unset opt
			error "Bad httpTextArea wrap value ($elem) in text area $name"
		}
		if {[lsearch -exact {"readonly" "hidden"} $option] >= 0 && ([catch {expr $val + 1}] || $val < 0 || $val > 1)} {
			unset opt
			error "Bad httpTextArea $option value ($val) in text area $name"
		}
		if {[lsearch -exact {"rows" "columns" "maxlen"} $option] >= 0 && ([catch {expr $val + 1}] || $val <= 0)} {
			unset opt
			error "Bad httpTextArea $option value ($val) in text area $name"
		}
		set opt($option) $val
	}
	if {[lsearch -regexp [list "$opt(varname)"] {.*[^a-zA-Z0-9_].*}] >= 0} {
		set vn $opt(varname)
		unset opt
		error "Option varname $vn in text area $name is invalid"
	}
	set opt(name) $name
	set opt(type) "TextArea"
	set opt(create) "httpCreateTextArea"
	return $name
}

# htmpCreateTextArea: Create html text that specifies the text area
#	returns the corresponding html document fragment
proc httpCreateTextArea {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[uplevel 1 "httpGet $name type"] != {TextArea}} {
		error "Bad httpCreateTextArea widget $name, must be of type TextArea"
	}
	set ret [format {<textarea%s rows="%s" cols="%s" name="%s"} [uplevel 1 "httpCreateWidgetAttributes $name"] $opt(rows) $opt(columns) $opt(varname)]
	if {$opt(placeholder) != ""} {
		append ret " placeholder=\"[httpAttrText $opt(placeholder)]\""
	}
	if {$opt(maxlen) != ""} {
		append ret " maxlength=\"$opt(maxlen)\""
	}
	if $opt(hidden) {
		append ret " hidden"
	} elseif $opt(readonly) {
		append ret " readonly"
	}
	append ret ">"
	if {[array names postvars -exact $opt(varname)] == $opt(varname)} {
		append ret "[httpText $postvars($opt(varname))]"
	} elseif {$opt(value) != ""} {
		append ret "[httpText $opt(value)]"
	}
	append ret "</textarea>"
	set opt(vars) $opt(varname)
	return "[httpDeli [httpGet postvars namespace]]$ret"
}

# httpEntry: Define values for an entry fied. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	value: Entry contents
#	placeholder: String to be shown if entry is empty
#	size: Size of entry, in characters
#	view: Type of entry, may be one of button, color, date, datetime-local email, file, hidden, month, number, password, search, tel, text, time, url or week
#	precommand: Command to be executed when hidden entry has been clicked. It will be used as body of
#		a procedure with the arguments service, widget, socket and vars, where
#			service  Name of the service. A global array variable with that name represents the service.
# 				The  port holds the TCP port number of the service, element pagelist holds the names of all main 
#				widgets supported by the service, element sfd holds the server socket handle and element fds holds
#				a list of all active http requests, represented by the corresponding client socket handles.
#			widget Name of the hidden entry widget that has been pressed.
#			socket Socket handle to be used for communication with the client.
#			vars is the name of an array variable in the calling context that holds the values transmitted via
#				HTTP POST message. In case of HTTP GET, this array variable is empty.
#		precommand will be called in global context before the requested page will be sent to the client. It can
#		return one of the names contained within the pagelist element of the service to specify it as next page to
#		be displayed or an empty string for current page.
#	command: Command to be executed when hidden entry has been clicked. It will be used as body of
#		a procedure with the arguments service, widget, socket and vars, where
#			service  Name of the service. A global array variable with that name represents the service.
# 				The  port holds the TCP port number of the service, element pagelist holds the names of all main 
#				widgets supported by the service, element sfd holds the server socket handle and element fds holds
#				a list of all active http requests, represented by the corresponding client socket handles.
#			widget Name of the button widget that has been pressed. This is the name of the widget that holds the command.
#			socket Socket handle to be used for communication with the client.
#			vars is the name of an array variable in the calling context that holds the values transmitted via
#				HTTP POST message. In case of HTTP GET, this array variable is empty.
#		command will be called in global context after the page specified by precommand has been sent, before the
#		client socket will be closed. This allows value changes within the widgets via script snipets. 
proc httpEntry {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create entry $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set opt(value) ""
	set opt(placeholder) ""
	set opt(view) "text"
	set opt(size) 20
	set opt(varname) [regsub -all "::" $name "_"]
	set opt(command) ""
	set opt(precommand) ""
	set allowedOptions [array names opt]
	set allowedtypes "button color date datetime-local email file hidden month number password search tel text time url week"
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpEntry option $elem in entry $name"
		}
		set val [lindex $elem 1]
		if {$option == "view" && [lsearch -exact $allowedtypes $val] < 0} {
			unset opt
			error "Bad httpEntry view $elem in entry $name"
		}
		if {$option == "size" && ([catch {expr $val + 1}] || $val <= 0)} {
			unset opt
			error "Bad httpEntry size ($val) in entry $name"
		}
		set opt($option) $val
	}
	if {[lsearch -regexp [list "$opt(varname)"] {.*[^a-zA-Z0-9_].*}] >= 0} {
		set vn $opt(varname)
		unset opt
		error "Option varname $vn in entry $name is invalid"
	}
	set opt(name) $name
	set opt(type) "Entry"
	set opt(create) "httpCreateEntry"
	return $name
}

# htmpCreateEntry: Create html text that specifies the entry
#	returns the corresponding html document fragment
proc httpCreateEntry {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[uplevel 1 "httpGet $name type"] != {Entry}} {
		error "Bad httpCreateEntry widget $name, must be of type Entry"
	}
	set ret [format {<input%s type="%s" name="%s"} [uplevel 1 "httpCreateWidgetAttributes $name"] $opt(view) $opt(varname)]
	if {[array names postvars -exact $opt(varname)] == $opt(varname)} {
		append ret " value=\"[httpAttrText $postvars($opt(varname))]\""
	} elseif {$opt(value) != ""} {
		append ret " value=\"[httpAttrText $opt(value)]\""
	}
	if {$opt(placeholder) != ""} {
		append ret " placeholder=\"[httpAttrText $opt(placeholder)]\""
	}
	if {$opt(size) != ""} {
		append ret " size=$opt(size)"
	}
	set opt(vars) $opt(varname)
	append ret " />"
	return "[httpDeli [httpGet postvars namespace]]$ret"
}

# httpButton: Define values for a button. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	text: Button contents (text button)
#	label: Name of httpLabel variable describing the button label
#	precommand: Command to be executed when button has been pressed. It will be used as body of
#		a procedure with the arguments service, widget, socket and vars, where
#			service  Name of the service. A global array variable with that name represents the service.
# 				The  port holds the TCP port number of the service, element pagelist holds the names of all main 
#				widgets supported by the service, element sfd holds the server socket handle and element fds holds
#				a list of all active http requests, represented by the corresponding client socket handles.
#			widget Name of the button widget that has been pressed.
#			socket Socket handle to be used for communication with the client.
#			vars is the name of an array variable in the calling context that holds the values transmitted via
#				HTTP POST message. In case of HTTP GET, this array variable is empty.
#		precommand will be called in global context before the requested page will be sent to the client. It can
#		return one of the names contained within the pagelist element of the service to specify it as next page to
#		be displayed or an empty string for current page.
#	command: Command to be executed when button has been pressed. It will be used as body of
#		a procedure with the arguments service, widget, socket and vars, where
#			service  Name of the service. A global array variable with that name represents the service.
# 				The  port holds the TCP port number of the service, element pagelist holds the names of all main 
#				widgets supported by the service, element sfd holds the server socket handle and element fds holds
#				a list of all active http requests, represented by the corresponding client socket handles.
#			widget Name of the button widget that has been pressed. This is the name of the widget that holds the command.
#			socket Socket handle to be used for communication with the client.
#			vars is the name of an array variable in the calling context that holds the values transmitted via
#				HTTP POST message. In case of HTTP GET, this array variable is empty.
#		command will be called in global context after the page specified by precommand has been sent, before the
#		client socket will be closed. This allows value changes within the widgets via script snipets. 
proc httpButton {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create button $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set opt(text) ""
	set opt(label) ""
	set opt(command) ""
	set opt(precommand) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpButton option {$elem} in button $name"
		}
		set opt($option) [lindex $elem 1]
	}
	if {$opt(label) != ""} {
		upvar [set level [httpGetLevel $opt(label)]] $opt(label) lbopt
		if {[lsearch -exact {Label Block Table} [uplevel 1 "httpGet $opt(label) type"]] < 0} {
			set lb $opt(label)
			unset opt
			error "Invalid httpButton option label $lb in button $name"
		}
		if {$level == 1} {
			global httpBeautifyHtml
			set saveit $httpBeautifyHtml
			set httpBeautifyHtml 0
			set opt(tagdata) [uplevel 1 "$lbopt(create) $opt(label) {}"]
			set httpBeautifyHtml $saveit
			set opt(label) ""
		}
	}
	set opt(name) $name
	set opt(type) "Button"
	set opt(create) "httpCreateButton"
	return $name
}

# httpCreateButton: Create html text that specifies the button
#	returns the corresponding html document fragment
proc httpCreateButton {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[httpGet $name "type"] != {Button}} {
		error "Bad httpButton widget $name, must be of type Button"
	}
	set opt(vars) [set varname [regsub -all "::" $opt(name) "_"]]
	set ret "[httpDeli [httpGet postvars namespace] +]<button[httpCreateWidgetAttributes $name] type=\"submit\" name=\"$varname\" value=\"Pressed\">"
	if {$opt(label) != ""} {
		upvar #0 $opt(label) lbopt
		set lbl [$lbopt(create) $opt(label) $vars]
		lappend opt(vars) $lbopt(vars)
	} elseif {[httpGet $name "tagdata"] != ""} {
		set lbl $opt(tagdata)
	} {
		set lbl "[httpText $opt(text)]"
	}
	set opt(vars) [lsort -unique $opt(vars)]
	append ret "[httpDeli [httpGet postvars namespace]]$lbl"
	return "$ret[httpDeli [httpGet postvars namespace] -]</button>"
}

# httpCheckButton: Define values for an entry of a check button. Allowed options:
#	onvalue: Value if selected
#	varname: Name of variable that holds the current value
#	defaultvalue: Default value for varname. Will be set only if $varname does not exist.
proc httpCheckButton {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create check button $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	httpInitWidget $name 1
	set opt(onvalue) "ON"
	set opt(defaultvalue) ""
	set opt(varname) [regsub -all "::" $name "_"]
	set opt(button) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpCheckButton option {$elem} in check button $name"
		}
		set opt($option) [lindex $elem 1]
		if {$option == "button"} {
			upvar [set level [httpGetLevel $opt(button)]] $opt(button) btopt
			if {$level == 1 || [httpGet $opt(button) type] != "Entry" || [httpGet $opt(button) view] != "hidden"} {
				set bt $opt(button)
				unset opt
				error "Invalid httpCheckButton option button $bt in check button $name"
			}
		}
	}
	if {[lsearch -regexp [list "$opt(varname)"] {.*[^a-zA-Z0-9_].*}] >= 0} {
		set vn $opt(varname)
		unset opt
		error "Option varname $vn in check button $name is invalid"
	}
	if {$opt(onvalue) == "" || ($opt(defaultvalue) != "" && $opt(onvalue) != $opt(defaultvalue))} {
		unset opt
		error "Bad httpCheckButton options in check button $name: onvalue or defaultvalue do not fit the requirements"
	}
	set opt(name) $name
	set opt(type) "ChkButton"
	set opt(create) "httpCreateCheckButton"
	return $name
}

# httpCreateCheckButton: Create html text that specifies the check button
#	returns the corresponding html document fragment
proc httpCreateCheckButton {name vars} {
	upvar [httpGetLevel $name] $name opt
	if [llength $vars] {
		array set postvars $vars
	}
	
	if {[httpGet $name "type"] != {ChkButton}} {
		error "Bad httpCheckButton widget $name, must be of type ChkButton"
	}
	set checked ""
	if {[array names postvars -exact $opt(varname)] == $opt(varname)} {
		if {$postvars($opt(varname)) == $opt(onvalue)} {
			set checked " checked"
		}
	} elseif {[array names postvars] == "" && $opt(defaultvalue) == $opt(onvalue)} {
		set checked " checked"
	}
	set opt(vars) $opt(varname)
	set ret "[httpDeli [httpGet postvars namespace]]<input[httpCreateWidgetAttributes $name] type=\"checkbox\" name=\"$opt(varname)\" value=\"[httpAttrText $opt(onvalue)]\"$checked"
	if {[httpGet $name "button"] != ""} {
		upvar [httpGetLevel $opt(button)] $opt(button) btn
		append ret " onchange = \"btn = document.getElementById('$btn(varname)');"
		append ret " btn.type='submit'; btn.value='Pressed'; btn.click();btn.type='hidden';\""
	}
	return "$ret />"
}

# httpGet: Return value of array variable created by httpsrv extension. 
#	name: Name of array variable representin extension object.
#	what: Element name.
proc httpGet {name what} {
	if {[uplevel [set level [httpGetLevel $name]] "array names $name -exact $what"] == $what} {
		upvar $level $name opt
		return $opt($what)
	}
	return ""
}

# httpRadioButton: Define values for an entry of a selection menu. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	varname: Value of the name attribute, used to holds the selected value.
#	value: Value of button list if radio button has been selected
#	defaultvalue: Default value of $varname, will be set only if $varname does not exist
proc httpRadioButton {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create radio button $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	httpInitWidget $name 1
	set opt(value) ""
	set opt(varname) [regsub -all "::" $name "_"]
	set opt(defaultvalue) ""
	set opt(button) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpRadioButton option {$elem} in radio button $name"
		}
		set opt($option) [lindex $elem 1]
		if {$option == "button"} {
			upvar [set level [httpGetLevel $opt(button)]] $opt(button) btopt
			if {$level == 1 || [httpGet $opt(button) type] != "Entry" || [httpGet $opt(button) view] != "hidden"} {
				set bt $opt(button)
				unset opt
				error "Invalid httpRadioButton option button $bt in radio button $name"
			}
		}
	}
	if {[lsearch -regexp [list "$opt(varname)"] {.*[^a-zA-Z0-9_].*}] >= 0} {
		set vn $opt(varname)
		unset opt
		error "Option varname $vn in radio button $name is invalid"
	}
	set opt(name) $name
	set opt(type) "RdButton"
	set opt(create) "httpCreateRadioButton"
	return $name
}

# httpCreateRadioButton: Create html text that specifies a menu selection line
#	returns the corresponding html document fragment.
proc httpCreateRadioButton {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[httpGet $name "type"] != {RdButton}} {
		error "Bad httpRadioButton widget, must be of type RdButton"
	}
	set selected ""
	if {[array names postvars -exact $opt(varname)] == $opt(varname)} {
		if {$postvars($opt(varname)) == $opt(value)} {
			set selected " checked"
		}
	} elseif {[array names postvars] == "" && $opt(defaultvalue) == $opt(value)} {
		set selected " checked"
	}
	set opt(vars) $opt(varname)
	set ret "[httpDeli [httpGet postvars namespace]]<input[httpCreateWidgetAttributes $name] type=\"radio\" name=\"$opt(varname)\" value=\"[httpAttrText $opt(value)]\"$selected"
	if {[httpGet $name "button"] != ""} {
		upvar [httpGetLevel $opt(button)] $opt(button) btn
		append ret " onchange = \"btn = document.getElementById('$btn(varname)');"
		append ret " btn.type='submit'; btn.value='Pressed'; btn.click();btn.type='hidden';\""
	}
	return "$ret />" 
}

# httpMenu: Define values for a selection menu. Allowed options:
#	style: Style string in css format
#	class: Css class name
#	entries: A list containing value - text pairs, where the value specifies the selection value and
#		the text specifies the corresponding menu text.
#	button: A hidden entry widget, used as submit button on option change. Default: Nothing happens on option change.
#	defaultvalue: Default contents of varname (default: empty string)
proc httpMenu {name args} {
	if {[httpCheckName $name 1]} {
		error "Cannot create label $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	httpInitWidget $name 1
	set opt(entries) ""
	set opt(defaultvalue) ""
	set opt(button) ""
	set opt(varname) [regsub -all "::" $name "_"]
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpMenu option {$elem} in menu $name"
		}
		set opt($option) [lindex $elem 1]
		if {$option == "entries"} {
			foreach sel [lindex $elem 1] {
				if {[llength $sel] != 2} {
					unset opt
					error "Bad httpMenu selection $sel in menu $name"
				}
			}
		} elseif {$option == "button"} {
			upvar [set level [httpGetLevel $opt(button)]] $opt(button) btopt
			if {$level == 1 || [httpGet $opt(button) type] != "Entry" || [httpGet $opt(button) view] != "hidden"} {
				set bt $opt(button)
				unset opt
				error "Invalid httpMenu option button $bt in menu $name"
			}
		}
	}
	if {[lsearch -regexp [list "$opt(varname)"] {.*[^a-zA-Z0-9_].*}] >= 0} {
		set vn $opt(varname)
		unset opt
		error "Option varname $vn in menu $name is invalid"
	}
	set opt(name) $name
	set opt(type) "Menu"
	set opt(create) "httpCreateMenu"
	return $name
}

# htmpCreateMenu: Create html text that specifies a menu button
#	returns the corresponding html document fragment.
proc httpCreateMenu {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[httpGet $name "type"] != {Menu}} {
		error "Bad httpMenu widget, must be of type Menu"
	}
	if [llength $vars] {
		if {[array names postvars -exact $opt(varname)] == $opt(varname)} {
			set currvar $postvars($opt(varname))
		}
	}
	if [catch {set currvar}] {
		set currvar $opt(defaultvalue)
	}
	set ret "[httpDeli [httpGet postvars namespace] +]<select[httpCreateWidgetAttributes $name] name=\"$opt(varname)\""
	if {[httpGet $name "button"] != ""} {
		upvar [httpGetLevel $opt(button)] $opt(button) btn
		append ret " onchange = \"btn = document.getElementById('$btn(varname)');"
		append ret " btn.type='submit'; btn.value='Pressed'; btn.click();btn.type='hidden';\">"
	} {
		append ret ">"
	}
	append ret "[httpDeli [httpGet postvars namespace]]<button><selectedcontent></selectedcontent></button>"
	foreach se $opt(entries) {
		lassign $se val text
		append ret "[httpDeli [httpGet postvars namespace]]<option id=\"$opt(varname)\" value=\"[httpAttrText $val]\""
		if {$val == $currvar} {
			append ret " selected"
		}
		append ret ">[httpText $text]</option>"
	}
	append ret "[httpDeli [httpGet postvars namespace] -]</select>"
	set opt(vars) $opt(varname)
	return $ret
}


# httpBlock: Define values for a block that consists of several concatenated widgets. If style or class will be
#			 specified, a div block will be created.
#	style: Style string in css format
#	class: Css class name
#	elements: List of widget variables belonging to the block initially
proc httpBlock {name args} {
	if {[httpCheckName $name]} {
		error "Cannot create block $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set opt(elements) ""
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpBlock option $elem in block $name"
		}
		set value [lindex $elem 1]
		if {$option == {elements}} {
			set len [llength $value]
			for {set i 0} {$i < $len} {incr i} {
				set wd [lindex $value $i]
				upvar [set level [httpGetLevel $wd]] $wd wdopt
				if {[array names wdopt -exact "class"] != "class" || [array names wdopt -exact "create"] != "create"} {
					unset opt
					error "Bad httpBlock element $wd in block $name"
				} elseif {$level == 1} {
					global httpBeautifyHtml
					set saveit $httpBeautifyHtml
					set httpBeautifyHtml 0
					lset value $i [list "tag" [uplevel 1 "$wdopt(create) $wd {}"]]
					set httpBeautifyHtml $saveit
				}
			}
		}
		set opt($option) $value
	}
	set opt(name) $name
	set opt(type) "Block"
	set opt(create) "httpCreateBlock"
	return $name	
}

# httpBlockAppend: Add widget to the end of a block of widgets.
#	elements: A list of widget variables that shall be appended
#	tag:	Additional tag to be appended, e.g. <br/> or <p>
proc httpBlockAppend {name elements {tag {}}} {
	upvar [httpGetLevel $name] $name opt

	if {[uplevel 1 "httpGet $name type"] != {Block}} {
		error "Bad httpBlockAppend widget $name, must be of type Block"
	}
	foreach wd $elements {
		upvar [set level [httpGetLevel $wd]] $wd wdopt
		if {[array names wdopt -exact "class"] != "class" || [array names wdopt -exact "create"] != "create"} {
			error "Bad httpBlockAppend element $wd in block $name"
		}
		if {$level == "#0"} {
			lappend opt(elements) $wd
		} {
			global httpBeautifyHtml
			set saveit $httpBeautifyHtml
			set httpBeautifyHtml 0
			lappend opt(elements) [list "tag" [uplevel 1 "$wdopt(create) $wd {}"]]
			set httpBeautifyHtml $saveit
		}
	}
	if {$tag != ""} {
		lappend opt(elements) [list "tag" $tag]
	}
	return $name	
}

# httpCreateBlock: Create html text that specifies a block
#	returns the corresponding html document fragment.
proc httpCreateBlock {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[uplevel 1 "httpGet $name type"] != {Block}} {
		error "Bad httpBlock widget, must be of type Block"
	}
	if {$opt(style) != "" || $opt(class) != "" || $opt(script) == "1"} {
		set prefix "[httpDeli [httpGet postvars namespace] +]<span[uplevel 1 "httpCreateWidgetAttributes $name"]>"	
	} {
		set prefix ""
	}
	set ret ""
	set opt(vars) {}
	foreach elem $opt(elements) {
		if {[llength $elem] == 2 && [lindex $elem 0] == "tag"} {
			append ret "[httpDeli [httpGet postvars namespace]][lindex $elem 1]"
		} {
			upvar #0 $elem elemopt
			append ret [$elemopt(create) $elem $vars]
			append opt(vars) " " $elemopt(vars)
		}
	}
	set opt(vars) [lsort -unique $opt(vars)]
	if {$prefix != ""} {
		set retend "[httpDeli [httpGet postvars namespace] -]</span>"
	} {
		set retend ""
	}
	return "$prefix$ret$retend"
}

# httpTable: Defines values for a table containing other widgets
#	style: Style string in css format
#	class: Css class name
proc httpTable {name args} {
	if {[httpCheckName $name]} {
		error "Cannot create table $name: Object $name just exists"
	}
	upvar [httpGetLevel $name] $name opt
	uplevel 1 "httpInitWidget $name 1"
	set allowedOptions [array names opt]
	foreach elem $args {
		if {[llength $elem] != 2 || [lsearch -exact $allowedOptions [set option [lindex $elem 0]]] < 0} {
			unset opt
			error "Bad httpTable option $elem in table $name"
		}
		set opt($option) [lindex $elem 1]
	}
	set opt(name) $name
	set opt(type) "Table"
	set opt(matrix) ""
	set opt(create) "httpCreateTable"
	return $name
}

# httpPlace: Place a widget within a table
#	table: Table widget
#	entry: Widget to be placed
#	row: starting (and optional ending) row of widget
#	column: Starting (and optional ending) column of widget
proc httpPlace {table entry row column} {
	upvar [httpGetLevel $table] $table opt
	upvar [set level [httpGetLevel $entry]] $entry enopt
	if {[uplevel 1 "httpGet $table type"] != {Table}} {
		error "Bad httpPlace widget $table, must be of type Table"
	}
	if {[array exists enopt] == 0 || [array names enopt -exact "class"] != "class" || [array names enopt -exact "create"] != "create"} {
		error "Bad httpPlace element $entry - [array names enopt], must be widget"
	}
	set endrow [lindex $row end]
	set endcolumn [lindex $column end]
	set startrow [lindex $row 0]
	set startcolumn [lindex $column 0]
	if {$opt(matrix) == ""} {
		set matrix {{}}
	} {
		set matrix $opt(matrix)
	}
	if {[llength [lindex $matrix 0]] <= $endcolumn} {
		set max [llength $matrix]
		for {set x 0} {$x < $max} {incr x} {
			set newrow [lindex $matrix $x]
			for {set y [llength $newrow]} {$y <= $endcolumn} {incr y} {
				lappend newrow {}
			}
			lset matrix $x $newrow
		}
	}
	if {[set x [llength $matrix]] <= $endrow} {
		set newrow ""
		set max [llength [lindex $matrix 0]]
		for {set y 0} {$y < $max} {incr y} {
			lappend newrow {}
		}
		while {$x <= $endrow} {
			lappend matrix $newrow
			incr x
		}
	}
	for {set x $startrow} {$x <= $endrow} {incr x} {
		set currentrow [lindex $matrix $x]
		for {set y $startcolumn} {$y <= $endcolumn} {incr y} {
			if {[lindex $currentrow $y] != ""} {
				error "Bad row / column for widget $entry in table $table: $x / $y just in use"
			}
		}
	}
	for {set x $startrow} {$x <= $endrow} {incr x} {
		for {set y $startcolumn} {$y <= $endcolumn} {incr y} {
			if {$x == $startrow && $y == $startcolumn} {
				if {$level == 1} {
					global httpBeautifyHtml
					set saveit $httpBeautifyHtml
					set httpBeautifyHtml 0
					lset matrix $x $y [list ">[uplevel 1 "$enopt(create) $entry {}"]" $endrow $endcolumn]
					set httpBeautifyHtml $saveit
				} {
					lset matrix $x $y [list $entry $endrow $endcolumn]
				}
			} {
				lset matrix $x $y "BLOCKED"
			}
		}
	}
	set opt(matrix) $matrix
	return $table
}

# httpCreateTable: Create html text that specifies a table
#	returns the corresponding html document fragment.
proc httpCreateTable {name vars} {
	upvar [httpGetLevel $name] $name opt
	array set postvars $vars
	
	if {[uplevel 1 "httpGet $name type"] != {Table}} {
		error "Bad httpTable widget, must be of type Table"
	}
	set ret "[httpDeli [httpGet postvars namespace] +]<table[uplevel 1 "httpCreateWidgetAttributes $name"]>"
	set columns {}
	set rows {}
	for {set i 0} {$i < [llength $opt(matrix)]} {incr i} {
		set curcol 0
		set haveone 0
		for {set j 0} {$j < [llength [lindex $opt(matrix) $i]]} {incr j} {
			set elemlen [llength [lindex $opt(matrix) $i $j]]
			if {$elemlen == 3} {
				if {$curcol >= [llength $columns] || [lindex $columns $curcol] > $j} {
					set columns [linsert $columns $curcol $j]
				}
				set haveone 1
			}
			if {$curcol < [llength $columns] && [lindex $columns $curcol] <= $j} {
				incr curcol
			}
		}
		if {$haveone} {
			lappend rows $i
		}
	}
	set opt(vars) {}
	foreach i $rows {
		append ret "[httpDeli [httpGet postvars namespace] +]<tr>"
		foreach j $columns {
			set elem [lindex $opt(matrix) $i $j]
			if {[llength $elem] == 3} {
				set rowspan 0
				set len [llength $rows]
				for {set k [lsearch $rows $i]} {$k < $len && [lindex $rows $k] <= [lindex $elem 1]} {incr k} {
					incr rowspan
				}
				set colspan 0
				set len [llength $columns]
				for {set k [lsearch $columns $j]} {$k < $len && [lindex $columns $k] <= [lindex $elem 2]} {incr k} {
					incr colspan
				}
				append ret "[httpDeli [httpGet postvars namespace] +]<td"
				if {$colspan > 1} {
					append ret " colspan=\"$colspan\"" 
				}
				if {$rowspan > 1} {
					append ret " rowspan=\"$rowspan\""
				}
				if {[string first ">" [set widget [lindex $elem 0]]] != 0} {
					upvar #0 $widget elemopt
					append ret ">[$elemopt(create) $elemopt(name) $vars]"
					append opt(vars) " " $elemopt(vars)
				} {
					append ret "[httpDeli [httpGet postvars namespace]]$widget"
				}
				append ret "[httpDeli [httpGet postvars namespace] -]</td>"
			} elseif {$elem == ""} {
				append ret "[httpDeli [httpGet postvars namespace]]<td/>"
			}
		}
		append ret "[httpDeli [httpGet postvars namespace] -]</tr>"
	}
	set opt(vars) [lsort -unique $opt(vars)]
	return "$ret[httpDeli [httpGet postvars namespace] -]</table>"
}

# httpCreateService: Creates service structure
#	name: Name of array variable representing the service
#	port: TCP port to be used by the server socket
#	widget: Name of widget that represents the body of the HTML start page
#	title: Contents of the title tag of the HTML header belonging to widget
#	args: Arguments specifying page information, see httpAddPage for details
proc httpCreateService {name port widget title args} {
	upvar #0 $name srv $widget wdg
	if {[array names wdg -exact "class"] != "class" || [array names wdg -exact "create"] != "create"} {
		error "httpCreateAddress: $widget is not a valid widget"
	}
	if {[httpGet $name port] != ""} {
		error "httpCreateAddress: $name is not a valid service name"
	}
	set srv(port) $port
	set srv(pagelist) {}
	if [catch {eval httpAddPage "$name $widget \"$title\" $args"} err] {
		array unset srv
		error $err
	}
}

# httpAddPage: Adds a widget to page list of a service.
#	service: Name of array variable that contains port and page list of the service.
#	name: Name of a widget that will be added to the service as body of a HTML page.
#	title: The title of that page specified by widget.
#	args: Arguments specifying header information of the HTML page. Supported arguments are
#		list with 1 element: Name of a CSS file to be used for that page,
#		list with 2 elements: CSS class name followed by corresponding class values.
#		list with more elements: Command to be executed after page has been completed. This means after
#			the corresponding client socket has been closed. This command will be called via apply, using
#			the following parameters:
#				service: Service name.
#				page: Name of the page that has been completed.
#				vars: List containing name - value pairs from http post response or an empty list if no http post response is available.
# Adds a list containing four elements to the pagelist component of the specified service.
# The first element is the widget name, the second element is a list containing CSS file names used within
# the HTML head (usually 0 or 1 element), the third element is a list containing two-element lists containing
# a class name and the corresponding class values to be used within the HTML page head and the fourth element
# holds the command to be executed after page completion.
# The widget name, extended by ".html", will be used by the service to select this page for further processing.
# If no page will be specified, the service will provide the first page, added within httpCreateService.
proc httpAddPage {service name title args} {
	upvar #0 $service srv $name widget
	if {[httpGetLevel $name] == 1 || [array names widget -exact "class"] != "class" || [array names widget -exact "create"] != "create"} {
		error "httpAddPage: $name is not a valid widget"
	}
	if {[array names srv -exact "port"] != "port" || [array names srv -exact "pagelist"] != "pagelist"} {
		error "httpAddPage: $name is not a valid service"
	}
	if {$title != ""} {
		set widget(title) $title
	}
	set cssfile ""
	set cssclasses ""
	foreach arg $args {
		switch [llength $arg] 1 {
			if {[file exists $arg] * [file isfile $arg] * [file readable $arg] * (1 - [string match "*.css" $cssfile]) == 0} {
				error "Css file '$arg' is not a readable file or no css file"
			}
			lappend cssfile $arg
		} 2 {
			if {[llength [lindex $arg 0]] != 1 } {
				error "Invalid css class name '$arg'"
			}
			lappend cssclasses $arg
		} default {
			if {[lsearch -exact "precommand command postcommand" [set what [lindex $arg 0]]] < 0} {
				error "Invalid command type $what in '$arg', must start with precommand, command or postcommand"
			}
			if {[array names command $what] == $what} {
				error "Command type $what multiply defined"
			}
			set pos [string first $what $arg]
			set command($what) "[string range $arg 0 $pos-1][string range $arg $pos+[string length $what] end]"
		}
	}
	lappend srv(pagelist) [list $name [list $cssfile $cssclasses [array get command]]]
}

# httpStartService: Starts a service.
#	service: Name of service used in previous calls to httpCreateService and httpAddPage.
#	loglevel: Logging level, one of "none", "error", "info" or "debug". Specifies which kind
#		of logging information will be written to the log file specified by the httpIO handle.
proc httpStartService {service} {
	upvar #0 $service srv
	if {[array names srv -exact "port"] != "port" || [array names srv -exact "pagelist"] != "pagelist"} {
		error "httpStartService: $service is not a valid service"
	}
	if {[array names srv -exact "sfd"] == "sfd"} {
		error "httpStartService: $service has just been started"
	}
	if {[catch {socket -server "httpServer $service " $srv(port)} srv(sfd)]} {
		set err $srv(sfd)
		unset srv(sfd)
		error "Starting service failed: $err"
	} {
		set srv(fds) {}
	}
}

if {$tcl_version < 9.0} {
	set httpEncoding "binary"
} {
	set httpEncoding "iso8859-1"
} 

# The server proc
proc httpServer {service fd ip port} {
	upvar #0 $service srv
	global httpIO httpEncoding
	catch {namespace delete $fd}
	namespace eval $fd {
		variable post
		variable trigger
		variable pathqueries
		variable addedvars {}
		variable addedwidgets
		variable beautyspaces ""
	}
	set watchdog [after 15000 "
		catch {close $fd}
		log info {Request abort by watchdog: Invalid or missing header} $fd
	"]
	namespace upvar $fd post vars pathqueries what
	log info "httpServer for $service connected on port $srv(port) from $ip:$port" $fd
	fconfigure $fd -translation crlf -encoding $httpEncoding -buffering none -blocking 0
	getline $fd rqline
	set err [catch {
		set what [httpNormalize [string range [lindex $rqline 1] 1 end]]
		set file [lindex $what 0]
		eval "set file $file"
	} txt]
	if {$err || [llength $rqline] != 3 || [string range [lindex $rqline 2] 0 4] != "HTTP/"} {
		catch {namespace delete $fd}
		log warning "Request: $rqline, ignore invalid request, err: $txt" $fd
		after cancel $watchdog
		after 10000 "catch {close $fd}"
		return
	}
	log info "Request: $rqline, Object: $what" $fd
	while {[namespace exists $fd] && [getline $fd line] != 0} {
		log debug $line $fd
		if {[lindex [set line [split $line :]] 0] == "Content-Length" && [scan [lindex $line 1] "%d%s" len s] == 1} {
			set postlen $len
		}
	}
	if {[namespace exists $fd] == 0} return
	after cancel $watchdog
	lappend srv(fds) $fd
	if {[lindex $rqline 0] == "POST"} {
		if {[catch {expr $postlen + 0}]} {
			after 100 "catch {incr [set fd]::trigger}"
			if [catch {vwait [set fd]::trigger}] {
				log warning "Request timed out" $fd
				return
			}
			set erg [catch {read $fd; fconfigure $fd -blocking 1} params]
		} {
			set erg [catch {read $fd $postlen} params]
		}
		if {$erg != 0} {
			set what ".htm"
		} {
			log debug $params $fd
		}
	} {
		set params ""
	}
	fconfigure $fd -translation binary
	set widx -1
	if {$params != ""} {
		set varvals [lrange [httpNormalize ".?$params"] 1 end]
		set varnames "$service [set service]DynamicVars"
		if {[set widget [lindex $varvals [lsearch -index 0 -exact $varvals $service] 1]] == ""} {
			foreach widget $srv(pagelist) {
				upvar #0 [lindex $widget 0] wdg
				if {[array names wdg -exact "vars"] == "vars"} {
					foreach var $wdg(vars) {
						lappend varnames $var
					}
				}
			}
			set varnames [lsort -unique $varnames]
		} {
			upvar #0 $widget wdg
			if {[array names wdg -exact "vars"] == "vars"} {
				foreach var $wdg(vars) {
					lappend varnames $var
				}
			}
		}
		if {[set addvars [lindex $varvals [lsearch -index 0 -exact $varvals "[set service]DynamicVars"] 1]] != ""} {
			eval "lappend varnames $addvars"
			log debug "All varnames: $varnames" $fd
		}
		array unset vars
		foreach varval $varvals {
			set varname [lindex $varval 0]
			if {[set vpos [lsearch -exact $varnames $varname]] < 0} {
				set widx -1
				set file ""
				log warning "Invalid varname: $varname -> break" $fd
				break
			}
			set varnames [lreplace $varnames $vpos $vpos]
			set vars($varname) [lindex $varval 1]
			log debug "$varname <- $vars($varname)" $fd
		}
		if {[array names vars -exact $service] == $service} {
			foreach varname [array names vars] {
				if {$vars($varname) == "Pressed"} {
					upvar #0 $varname wdg
					if {[info exist wdg] == 0} {
						lassign [split $varname "_"] space name
						catch {namespace upvar $space $name wdg}
					}
					if {[array names wdg -exact "type"] == "type" && [array names wdg -exact "precommand"] == "precommand"} {
						if [catch {apply [list {service widget socket} $wdg(precommand)] $service $varname $fd} ret opt] {
							array set errorinfo $opt
							log error "Error in precommand:\n$errorinfo(-errorinfo)" "$fd - $varname"
						} {
							log info "Next widget: '$ret'" $fd
						}
						if {[set idx [lsearch -exact -index 0 $srv(pagelist) $ret]] >= 0} {
							set widx $idx
						}
					}
					set vars($varname) ""
					break
				}
			}
		}
	} {
		array unset vars
	}
	set vars(namespace) $fd
	if {$widx < 0} {
		if {$what == ""} {
			set widx 0
		} elseif {[string equal -nocase [set ext [file extension $what]] ".htm"] == 1 || [string equal -nocase $ext ".html"] == 1} {
			if {[string match -nocase "index" [set name [file rootname $what]]]} {
				set widx 0
			} {
				set widx [lsearch -exact -index 0 $srv(pagelist) [file rootname $what]]
			}
		} {
			set widx -1
		}
	}
	if {$widx >= 0} {
		upvar #0 [set widget [lindex $srv(pagelist) $widx 0]] wdg
		set cssfiles [lindex $srv(pagelist) $widx 1 0]
		set cssclasses [lindex $srv(pagelist) $widx 1 1]
		array set command [lindex $srv(pagelist) $widx 1 2]
		log info "Request complete" $fd
		if {[array names command "precommand"] == "precommand"} {
			if [catch {apply [list {service socket page} $command(precommand)] $service $fd $widget} ret opt] {
				array set errorinfo $opt
				log error "Error in precommand:\n$errorinfo(-errorinfo)" "$fd - $command(precommand)"
			}
		}
		set body "[httpDeli $fd]<!DOCTYPE html>"
		append body "[httpDeli $fd +]<html lang=\"de\">"
		append body "[httpDeli $fd +]<head>"
		append body "[httpDeli $fd]<meta charset=\"utf-8\"/>"
		if {[set title [httpGet widget "title"]] != ""} {
			append body "[httpDeli $fd]<title>[httpText $title]</title>"
		}
		foreach css $cssfiles {
			append body [format {<link href="%s" rel="stylesheet">} [encoding convertto utf-8 $css]]
		}
		if {[llength $cssclasses] > 0} {
			append body "[httpDeli $fd +]<style>"
			foreach css $cssclasses {
				append body "[httpDeli $fd][lindex $css 0] {[encoding convertto utf-8 [lindex $css 1]]}" 
			}
			append body "[httpDeli $fd -]</style>"
		}
		append body "[httpDeli $fd -]</head>"
		append body "[httpDeli $fd +]<body>"
		append body "[httpDeli $fd +]<form action=\"\" method=\"post\">"
		append body "[httpDeli $fd]<input type=\"hidden\" id=\"$service\" name=\"$service\" value=\"$widget\" />"
		append body "[httpDeli $fd]<input type=\"hidden\" id=\"[set service]DynamicVars\" name=\"[set service]DynamicVars\" />"
		append body [$wdg(create) $widget [array get vars]]
		set resp "HTTP/1.1 200 OK"
		append resp "\r\nContent-Type: text/html; charset=utf-8"
		append resp "$\r\nConnection: close"
		append resp "$\r\n\r\n$body"
		if [catch {puts -nonewline $fd $resp} err] {
			log warning "Error sending initial http response part: $err" $fd
		} {
			lappend [set fd]::addedwidgets $widget
			log debug "START OF PAGE $widget: $resp" $fd
			set finished 1
			if {[array names vars] != ""} {
				foreach varname [array names vars] {
					if {$vars($varname) == "Pressed"} {
						upvar #0 $varname wdg
						if {[info exist wdg] == 0} {
							lassign [split $varname "_"] space name
							catch {namespace upvar $space $name wdg}
						}
						if {[array names wdg -exact "type"] == "type" && [array names wdg -exact "command"] == "command"} {
							if [catch {apply [list {service widget socket} $wdg(command)] $service $varname $fd} ret opt] {
								array set errorinfo $opt
								log error "Error in command:\n$errorinfo(-errorinfo)" "$fd - $varname"
							} elseif {[scan $ret "%d%s" val s] == 1 && $val == 0} {
								set finished 0
							}
						}
						break
					}
				}
			}
			if {[array names command "command"] == "command" && $finished} {
				if [catch {apply [list {service socket page} $command(command)] $service $fd $widget} ret opt] {
					array set errorinfo $opt
					log error "Error in command:\n$errorinfo(-errorinfo)" "$fd - $command(command)"
				} elseif {[scan $ret "%d%s" val s] == 1 && $val == 0} {
					set finished 0
				}
			}
		}
		if $finished {
			finalizePageRequest $service $widget $fd
		}
	} elseif {[file readable $file] && [catch {open $file r} ffd] == 0 && [catch {file size $file} flen] == 0} {
		set ext [string range [file extension $file] 1 end]
		if {$ext == "css" || $ext == "html"} {
			set type "text/$ext; charset=utf-8"
		} elseif {$ext == "ico"} {
			set type "image/x-icon"
		} {
			set allowed [httpAuthCallback $what]
			if {$allowed && ($ext == "jpg" || $ext == "jpe" || $ext == "jpeg")} {
				set type "image/jpeg"
			} elseif {$allowed && ($ext == "tiff" || $ext == "tif")} {
				set type "image/tiff"
			} elseif {$allowed && ($ext == "bmp" || $ext == "gif" || $ext == "png")} {
				set type "image/$ext"
			} elseif {$allowed && ($ext == "mp4" || $ext == "mkv" || $ext == "mov")} {
				set type "video/mp4"
			} elseif {$allowed && ($ext == "webm")} {
				set type "video/webm"
			} {
				close $ffd
				log error "Resource invalid" $fd
				set resp "HTTP/1.1 403 Forbidden"
				append resp "\r\nContent-Type: text/html"
				append resp "\r\nConnection: close"
				if $allowed {
					append resp "\r\n\r\n<body>Resource /$file: Invalid file type</body>"
				} {
					append resp "\r\n\r\n<body>Resource locked: No authorization</body>"
				}
				if [catch {puts -nonewline $fd $resp} err] {
					log warning "Error sending http response: $err" $fd
				} {
					log debug "START INVALID FILE $file: $resp :END INVALID FILE $file" $fd
				}
				after 100 "
					catch {httpCloseClient $service $fd}
					catch {namespace delete $fd}"
				set type ""
			}
		}
		if {$type != ""} {
			log info "File $file present" $fd
			if {[string range $type 0 3] == "text"} {
				fconfigure $ffd -blocking 1 -buffering none -buffersize 1000 -encoding $httpEncoding -translation auto
				set flen 0
			} {
				fconfigure $ffd -blocking 1 -buffering none -buffersize 1000 -encoding $httpEncoding -translation binary
			}
			set resp "HTTP/1.1 200 OK"
			append resp "\r\nContent-Type: $type"
			if {$flen} {
				append resp "\r\nContent-Length: $flen"
			}
			append resp "\r\nConnection: close"
			append resp "\r\n\r\n"
			set erg [catch {
				fconfigure $fd -blocking 0
				puts -nonewline $fd $resp
				log info "Sending data..." $fd
				fileevent $fd writable [list apply [list {ifd ofd service} {
					if [catch {
						if {[eof $ifd] == 0} {
							puts -nonewline $ofd [read $ifd 1000]
						} {
							httpCloseClient $service $ofd
							close $ifd
							namespace delete $fd
						}
					} err] {
						log warning "Error sending http response for input handle $ifd: $err" $ofd
						catch {httpCloseClient $service $ofd}
						catch {close $ifd}
						catch {namespace delete $fd}
					}					
				}] $ffd $fd $service]
			}]
		}
	} {
		log error "Resource $file not found" $fd
		set resp "HTTP/1.1 404 Not Found"
		append resp "\r\nContent-Type: text/html; charset=utf-8"
		append resp "\r\nConnection: close"
		append resp "\r\n\r\n<body>Resource /$file not found</body>"
		if {[catch {puts -nonewline $fd $resp} err]} {
			log warning "Error sending http response: $err" $fd
		} {
			log debug  "START NOT FOUND: $resp :END NOT FOUND" $fd
		}
		after 100 "
			catch {httpCloseClient $service $fd}
			catch {namespace delete $fd}
		"
	}
}

proc finalizePageRequest {service widget socket {delay 100}} {
	if {$delay != 0} {
		set part "[httpDeli $socket -]</form>"
		append part "[httpDeli $socket -]</body>"
		append part "[httpDeli $socket -]</html>"
		if [catch {puts -nonewline $socket $part} err] {			
			log warning "Error sending final http response part: $err" $socket
		} {
			log debug "$part :END OF PAGE $widget" $socket
		}
		after $delay "finalizePageRequest $service $widget $socket 0"
	} {
		httpCloseClient $service $socket
		upvar #0 $service srv
		array set cmd [lindex $srv(pagelist) [lsearch -exact -index 0 $srv(pagelist) $widget] 1 2]
		uplevel 1 {
			if {[array names command "postcommand"] == "postcommand"} {
				if [catch {apply [list {service socket page} $command(postcommand)] $service $socket $widget} ret opt] {
					array set errorinfo $opt
					log error "Error in postcommand:\n$errorinfo(-errorinfo)" "$socket - $command(postcommand)"
				}
			}
		}
		catch {namespace delete $socket}
	}
}

# Append HTML code of a widget to 
proc httpAppendCurrentPage {name current fd widget} {
	upvar #0 $name srv httpIO err
	namespace upvar $fd post val addedvars vars addedwidgets widgets
	if {[array names srv -exact "port"] != "port"} {
		error "httpAppendCurrentPage: $name is not a valid service"
	} elseif {[lsearch -exact $srv(fds) $fd] < 0} {
		error "httpAppendCurrentPage: Socket handle invalid"
	} elseif {[lsearch -index 0 -exact $srv(pagelist) $current] < 0} {
		error "httpAppendCurrentPage: $current is not a valid page"
	}
	upvar #0 $current page 
	upvar [httpGetLevel $widget] $widget wdg
	if {[set create [httpGet $widget "create"]] == ""} {
		log error "httpAppendCurrentPage: $widget is not a valid widget" $fd
	} {
		set newvars $vars
		set data "[uplevel 1 "$create $widget {[array get val]}"]"
		append data "[httpDeli $fd]<script>"
		if {$widgets == ""} {
			set newvars [set vars {}]
			append data "$name.value = '$widget';"
			append data "document.getElementById('removeit').remove();"
		} {
			append newvars " $wdg(vars)"
			set newvars [lsort -unique $newvars]
		}
		append data "[set name]DynamicVars.value = '$newvars';"
		append data "</script>"
		if {[catch {puts $fd $data} msg] > 0} {
			log warning "Error sending additional http response part: $msg" $fd
		} {
			log debug $data $fd
			append vars " $newvars"
			append widgets " $widget"
		}
	}
}

# httpClearCurrentPage: Clear inner HTML code of a page widget
proc httpClearCurrentPage {name current fd} {
	upvar #0 $name srv httpIO err $current page
	if {[array names srv -exact "port"] != "port"} {
		error "httpClearCurrentPage: $name is not a valid service"
	} elseif {[lsearch -exact $srv(fds) $fd] < 0} {
		error "httpClearCurrentPage: Socket handle invalid"
	} elseif {[lsearch -index 0 -exact $srv(pagelist) $current] < 0} {
		error "httpClearCurrentPage: $current is not a valid page"
	} elseif {[httpGet $current "script"] != 1} {
		error "httpClearCurrentPage: $current does not support clear page function"
	}
	set data "[httpDeli $fd]<span id='removeit'><h1>Place Holder</h1></span>"
	append data "[httpDeli $fd +]<script>"
	namespace upvar $fd addedwidgets widgets
	foreach widget [lrange $widgets 0 end] {
		if {[httpGet $widget "script"] != 1} {
			log warning "Warning: Widget $widget will not be deactivated: Scripting not enabled" $fd
		} {
			append data "[httpDeli $fd]document.getElementById('[regsub -all "::" $widget "_"]').remove();"
		}
	}
	append data "[httpDeli $fd]$name.value = '';"
	append data "[httpDeli $fd -]</script>"
	if {[catch {puts $fd $data} msg] > 0} {
		log warning "Error sending clear page command: $msg" $fd
	} {
		log debug $data $fd
		set page(vars) ""
		set widgets {}
	}
}

# httpStopService: Closes all connections to the port, the service port and free the service's recources
proc httpStopService {service} {
	upvar #0 $service srv
	if {[array names srv -exact "port"] != "port"} {
		error "httpStopService: $service is not a valid service"
	}
	catch {close $srv(sfd)}
	foreach fd $srv(fds) {
		catch {
			fileevent $fd readable {}
			fileevent $fd writable {}
		}
		catch {close $fd}
		catch {namespace delete $fd}
	}
	unset srv(sfd)
	unset srv(fds)
}

# httpCloseClient: Close handle in service
proc httpCloseClient {service fd} {
	upvar #0 $service srv
	catch {
		fileevent $fd readable {}
		fileevent $fd writable {}
	}
	catch {close $fd}
	set l {}
	foreach f $srv(fds) {
		if {$f != $fd} {
			lappend l $f
		}
	}
	set srv(fds) $l
}

# Compute level for variable to be defined
proc httpGetLevel {name {level 1}} {
	if {[set pos [string last "::" $name]] >= 0} {
		set vname [string range $name $pos+2 end]
	} {
		set vname $name
	}
	if {[string first [string range "$vname " 0 0] "ABCDEFGHIJKLMNOPQRSTUVWXYZ"] >= 0} {
		return "#0"
	}
	return $level
}

# httpIO: global variable that holds the output streams for logging. Default is stderr
set httpIO stderr
# Supported logging types
set httpLoggingTypes {none error info warning debug}
# httpLogLevel: global variable that holds the logging level. Default 2 (info). Valid values are
# 0 - llength $httpLoggingTypes.
set httpLogLevel info

# Write logging message to log file if level is not above httpLogLevel
proc log {level text {source {}}} {
	global httpIO httpLogLevel httpLoggingTypes
	
	if {[set i [lsearch -exact $httpLoggingTypes $level]] >= 0} {
		set level $i
	} elseif {[scan $level "%d%s" i s] != 1} {
		set level -1
	}
	if {[scan $httpLogLevel "%d%s" type s] != 1} {
		set type [lsearch -exact $httpLoggingTypes $httpLogLevel]
	}
	if {$level <= $type && $level > 0} {
		set dt [clock format [clock seconds] -format "%Y-%m-%d %H:%M:%S"]
		set data [format "%s %s%s %s" $dt [lindex $httpLoggingTypes $level] " $source" $text]
		foreach logfd $httpIO {
			puts $logfd $data
		}
	}
}

proc httpAuthCallback {what} {
	return 1
}

proc getline {fd var} {
	fileevent $fd readable "fileevent $fd readable {};set [set fd]::trigger 0"
	vwait [set fd]::trigger
	if [catch {uplevel 1 "gets $fd $var"} retval] {
		log warning "Error reading line from socket: $retval" $fd
		uplevel 1 "set $var {}"
		set retval -1
	}
	return $retval
}

set httpBeautifyHtml 0

proc httpDeli {space {level " "}} {
	global httpBeautifyHtml
	
	if $httpBeautifyHtml {
		namespace upvar $space beautyspaces bs
		if {$level == "-"} {
			set bs [string range $bs $httpBeautifyHtml end]
		}
		set ret "\n$bs"
		if {$level == "+"} {
			append bs [string range "                    " 1 $httpBeautifyHtml]
		}
		return $ret
	} {
		return ""
	}
}
