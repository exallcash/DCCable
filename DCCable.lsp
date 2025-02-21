
(setq fieldCounter 0)
(setq scriptRuns 0)

(defun c:checkDC ()
 (princ "\nScript was used : ") 
 (princ scriptRuns) 
 (princ " times in this dwg")
 )

(defun c:checkDCreset ()
  (setq scriptRuns 0)
  )



(defun c:DCreset ()
  (setq fieldCounter 0)
)


(defun c:DCcable ()
  (setq fieldCounter (+ fieldCounter 1))
  (setq scriptRuns (+ scriptRuns 1))
  (setq polylineLengthC nil)


  (defun createPoint (pt)
    (setq ptWCS (trans pt 1 0))  
    (entmake (list '(0 . "POINT") (cons 10 ptWCS) (cons 8 "DCcable"))) 
  )

 
  (setq pt1 (getpoint "\nSpecify the first corner: "))
  (createPoint pt1)  

  (setq pt2 (getpoint "\nSpecify the opposite corner: "))
  (createPoint pt2)  

  (setq pointsList (list pt1))  
  (setq nextPoint nil)

  (while (setq nextPoint (getpoint "\nSpecify next points or press Enter to finish: "))
    (createPoint nextPoint) 
    (setq pointsList (append pointsList (list nextPoint)))
  )


  (setq pt3 (list (car pt1) (cadr pt2))) 
  (setq pt4 (list (car pt2) (cadr pt1))) 

  (if (not (tblsearch "layer" "DCcable"))
    (progn
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq layers (vla-get-Layers doc))
      (setq DCcable (vla-Add layers "DCcable"))
      
      (vla-put-Color DCcable 1)
    )
    (progn
      (setq acadObj (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadObj))
      (setq layers (vla-get-Layers doc))
      (setq DCcable (vla-Item layers "DCcable"))
      (vla-put-Color DCcable 1)
    )
  )

  (command "_Virto_CableTrayPath" pt1 pt3 "")
  (setq lastPolyline1 (entlast))

  (command "_Virto_CableTrayPath" pt1 pt4 "")
  (setq lastPolyline2 (entlast))

  (command "_Virto_CableTrayPath" pt3 pt2 "")
  (setq lastPolyline3 (entlast))

  (command "_Virto_CableTrayPath" pt4 pt2 "")
  (setq lastPolyline4 (entlast))

  (foreach polyline (list lastPolyline1 lastPolyline2 lastPolyline3 lastPolyline4)
    (vla-put-Layer (vlax-ename->vla-object polyline) "DCcable")
  )

  (setq len1 (/ (vlax-curve-getDistAtParam lastPolyline1 (vlax-curve-getEndParam lastPolyline1)) 1000.0)) 
  (setq len2 (/ (vlax-curve-getDistAtParam lastPolyline2 (vlax-curve-getEndParam lastPolyline2)) 1000.0)) 

  (command "_dimlinear" pt1 pt3 (polar pt1 (/ pi 2) 200.0))
  (setq dim1 (entlast))

  (command "_dimlinear" pt1 pt4 (polar pt1 0 200.0))
  (setq dim2 (entlast))

  (vla-put-TextOverride (vlax-ename->vla-object dim1) (strcat (rtos len1 2 2) " m")) 
  (vla-put-TextOverride (vlax-ename->vla-object dim2) (strcat (rtos len2 2 2) " m")) 

  (foreach dim (list dim1 dim2)
    (vla-put-Layer (vlax-ename->vla-object dim) "DCcable")
  )

  (if (>=   (length pointsList) 2)
    (progn
      (command "_Virto_CableTrayPath" pt2)
      (foreach pt (cdr pointsList) (command pt))
      (command "")
      (setq lastPolylineC (entlast))

      (vla-put-Layer (vlax-ename->vla-object lastPolylineC) "DCcable")

      (setq polylineLengthC (/ (vlax-curve-getDistAtParam lastPolylineC (vlax-curve-getEndParam lastPolylineC)) 1000.0))
    )
  )

  (setq centerX (/ (+ (car pt1) (car pt2)) 2))  
  (setq centerY (/ (+ (cadr pt1) (cadr pt2)) 2))  
  (setq centerPt (list centerX centerY 0.0)) 

  (setq centerPtWCS (trans centerPt 1 0))

  (setq height 500.0)

  (setq textContent (strcat (rtos len2 2 2) "\\P"
                            (rtos len1 2 2) "\\P"
                            (if polylineLengthC (strcat (rtos polylineLengthC 2 2)) ""))) 

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq mtextObj (vla-AddMText modelSpace (vlax-3d-point centerPtWCS) 0.0 textContent))

  (vla-put-AttachmentPoint mtextObj acAttachmentPointMiddleCenter)
  (vla-put-Layer mtextObj "DCcable")
  (vla-put-Height mtextObj (* height 2))
  (vla-put-BackgroundFill mtextObj :vlax-true) 

  (setq numberTextContent (strcat "\\U+2116" (itoa fieldCounter)))
  (setq upperCenterPtWCS (list (car centerPtWCS) (+ (cadr centerPtWCS) 6000.0) 0.0))


  (setq numberTextObj (vla-AddMText modelSpace (vlax-3d-point upperCenterPtWCS) 0.0 numberTextContent))
  (vla-put-AttachmentPoint numberTextObj acAttachmentPointMiddleCenter)
  (vla-put-Height numberTextObj (* height 3))
  (vla-put-Layer numberTextObj "DCcable")
  (vla-put-BackgroundFill numberTextObj :vlax-true) 

  (princ)
)
