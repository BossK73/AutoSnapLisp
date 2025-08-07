;;; ========================================================================================================================
;;; AutoSnap plugin for AutoCAD.
;;; A tool shall eliminate planar fragments of lines and polylines (except arc) in the selected graph.
;;; Author�����
;;; Copyright 2025 ��� All rights reserved.
;;; Version 0.3.36 (2025 Aug. 8th 3:24 UTC/GMT+08:00) published under Apache-2.0 license.
;;; This version has been tested on AutoCAD 2018 and 2024 (64 bit) on Windows 10 amd64 Platform over GB18030 encode method.
;;; Windows is a brand of Microsoft, AutoCAD is a brand of Autodesk.
;;; Contact heimamsn@live.cn if you have suggestions.
;;; ========================================================================================================================

;;; �������
(defun C:ASNAP (/ *error* doc ss fuzzDist processedEnts newLayer)
  ;; ��������
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\n����: " msg))
    )
    (princ)
  )
  
  ;; ��ȡ�ĵ�
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  ;; ����ASNAPͼ��
  (setq newLayer (create-asnap-layer doc))
  
  ;; ��ȡ�û�ѡ��
  (prompt "\nѡ��ֱ��/�����/ͼ�� (��ֱ�ӻس�ѡ��ȫ��): ")
  (setq ss (get-selection-set))
  
  ;; ��ȡģ������
  (setq fuzzDist nil)
  (while (not fuzzDist)
    (setq fuzzDist (get-fuzz-distance))
  )
  
  ;; ����ʵ��
  (setq processedEnts (process-entities ss fuzzDist newLayer))
  
  ;; �������ս��
  (draw-final-entities processedEnts newLayer)
  
  (princ "\n�������! ")
  (princ)
)

;;; ����ר��ͼ��
(defun create-asnap-layer (doc / layers layer)
  (setq layers (vla-get-Layers doc))
  (if (not (tblsearch "LAYER" "Asnap"))
    (progn
      (setq layer (vla-Add layers "Asnap"))
      (vla-put-Color layer 1) ; ��ɫ
      (vla-put-Lineweight layer acLnWt030) ; 0.3mm
    )
    (setq layer (vla-Item layers "Asnap"))
  )
  layer
)

;;; ��ȡ�û�ѡ��
(defun get-selection-set (/ ss)
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,INSERT"))))
  (if (not ss)
    (progn
      (prompt "\nδѡ����󣬽�����ģ�Ϳռ������ж���...")
      (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE,INSERT"))))
    )
  )
  ss
)

;;; ��ȡģ������
(defun get-fuzz-distance (/ input)
  (initget 1)
  (setq input (getreal "\n������ģ������: "))
  (if (not (numberp input))
    (progn
      (prompt "\n����: ������������! ")
      nil
    )
    input
  )
)

;;; ����ʵ��������
(defun process-entities (ss fuzzDist newLayer / count i ent processed result)
  (if (not ss)
    (progn
      (princ "\nδ�ҵ��ɴ���Ķ���!")
      nil
    )
    (progn
      (setq count (sslength ss))
      (setq i 0)
      (while (< i count)
        (setq ent (ssname ss i))
        (setq processed (process-single-entity ent fuzzDist newLayer))
        
        (if processed
          (setq result (cons processed result))
          (princ (strcat "\n�����޷������ʵ��: " (cdr (assoc 0 (entget ent)))))
        )
        
        (setq i (1+ i))
        (princ (strcat "\n�������: " (itoa i) "/" (itoa count)))
      )
      result
    )
  )
)

;;; ������ʵ��
(defun process-single-entity (ent fuzzDist newLayer / entType)
  (setq entType (cdr (assoc 0 (entget ent))))
  
  (cond
    ((= entType "LINE")
      (process-line ent fuzzDist))
    
    ((= entType "LWPOLYLINE")
      (process-lwpolyline ent fuzzDist))
    
    ((= entType "INSERT")
      (process-block ent fuzzDist newLayer))
    
    (T nil)
  )
)

;;; ����ֱ��
(defun process-line (ent fuzzDist / data start end newStart newEnd)
  (setq data (entget ent))
  
  ;; ��ȡ����������յ�
  (setq start (cdr (assoc 10 data)))
  (setq end (cdr (assoc 11 data)))
  
  (setq newStart (snap-point start fuzzDist))
  (setq newEnd (snap-point end fuzzDist))
  
  ;; ����Ƿ��˻�Ϊ��
  (if (equal newStart newEnd 1e-6)
    nil ; �����˻�����
    (list
      (cons 0 "LINE")
      (cons 10 newStart)
      (cons 11 newEnd)
    )
  )
)

;;; ��������
(defun process-lwpolyline (ent fuzzDist / data vertices closed newVerts i pt prevPt filteredVerts)
  (setq data (entget ent))
  (setq closed (if (= 1 (logand 1 (cdr (assoc 70 data)))) T nil))
  
  ;; ��ȡ�����б�
  (setq vertices (vl-remove-if-not '(lambda (x) (= (car x) 10)) data))
  (setq newVerts nil)
  
  ;; ����ÿ������
  (foreach v vertices
    (setq pt (snap-point (cdr v) fuzzDist))
    (setq newVerts (cons pt newVerts))
  )
  (setq newVerts (reverse newVerts))
  
  ;; �Ƴ������غϵ�
  (setq filteredVerts nil)
  (foreach pt newVerts
    (if (or (null (car filteredVerts)) 
            (not (equal pt (car filteredVerts) 1e-6)))
      (setq filteredVerts (cons pt filteredVerts))
    )
  )
  (setq filteredVerts (reverse filteredVerts))
  
  ;; ���պ϶���ߵ���β��
  (if (and closed 
          (> (length filteredVerts) 2)
          (not (equal (car filteredVerts) (last filteredVerts) 1e-6)))
    (setq filteredVerts (append filteredVerts (list (car filteredVerts))))
  )
  ;; ����������
  (cond
    ((< (length filteredVerts) 2) ; ���㲻��
      nil)
    ((= (length filteredVerts) 2) ; ֻ�������㣬תΪֱ��
      (list
        (cons 0 "LINE")
        (cons 10 (car filteredVerts))
        (cons 11 (cadr filteredVerts))
      ))
    (T
      (append
        (list
          (cons 0 "LWPOLYLINE")
          (cons 100 "AcDbEntity")
          (cons 100 "AcDbPolyline")
          (cons 90 (length filteredVerts))
          (cons 70 (if closed 1 0))
          (cons 38 0.0) ; Z�������
        )
        (mapcar '(lambda (pt) (cons 10 pt)) filteredVerts)
      )
    )
  )
)

;;; ����ͼ��
(defun process-block (ent fuzzDist newLayer / blkDef blkName newName newDef entData)
  (setq entData (entget ent))
  (setq blkName (cdr (assoc 2 entData)))
  (setq blkDef (tblobjname "BLOCK" blkName))
  
  ;; �����¿鶨��
  (setq newName (strcat blkName "_ASNAP"))
  
  (if (setq oldBlk (tblobjname "BLOCK" newName))
    (progn
      (entdel oldBlk) ; ɾ���鶨��
      (entdel (entnext oldBlk)) ; ɾ����Ӧ��ENDBLK
      (princ (strcat "\n�Ѹ��´��ڵĿ�: " newName))
    )
  )
  
  ;; �������ʵ��
  (setq newDef (process-block-definition blkDef fuzzDist newLayer newName))
  (if newDef
    (progn
      (setq insertData (list (cons 0 "INSERT") (cons 2 newName)))
      ;; ��Ӳ�������������
      (setq insertData (append insertData (list (assoc 10 entData))))
      (if (assoc 41 entData) (setq insertData (append insertData (list (assoc 41 entData)))))
      (if (assoc 42 entData) (setq insertData (append insertData (list (assoc 42 entData)))))
      (if (assoc 43 entData) (setq insertData (append insertData (list (assoc 43 entData)))))
      (if (assoc 50 entData) (setq insertData (append insertData (list (assoc 50 entData)))))
      insertData
    )
    nil
  )
)

;;; ����鶨��
(defun process-block-definition (blkDef fuzzDist newLayer newName / ent newEnts result)
  (setq ent (entnext blkDef))
  (setq newEnts nil)
  (while (and ent (not (= (cdr (assoc 0 (entget ent))) "ENDBLK")))
    (setq result (process-single-entity ent fuzzDist newLayer))
    (if result
      (setq newEnts (cons result newEnts)))
    (setq ent (entnext ent))
  )
  (if newEnts
    (progn
      ;; ǿ�ƴ����¿鶨��
      (entmake (list (cons 0 "BLOCK") (cons 2 newName) (cons 70 0) (cons 10 (list 0.0 0.0 0.0))))
      (foreach entData (reverse newEnts)
        (entmake entData)
      )
      (entmake '((0 . "ENDBLK")))
      newName ; �����¿���
    )
    nil
  )
)

;;; ������뺯��
(defun snap-point (pt fuzzDist / x y)
  (setq x (car pt)
        y (cadr pt))
  (list
    (if (zerop fuzzDist) x (* fuzzDist (fix (+ (/ x fuzzDist) 0.5)))) ; X��������
    (if (zerop fuzzDist) y (* fuzzDist (fix (+ (/ y fuzzDist) 0.5)))) ; Y��������
    0.0
  )
)

;;; ��������ʵ��
(defun draw-final-entities (entList newLayer / entData uniqueEnts entStr seen)
  (if (not entList)
    (princ "\nû�пɻ��Ƶ�ʵ��!")
    (progn
      ;; ȥ���ظ�ʵ��
      (setq uniqueEnts nil seen (list))
      (foreach entData entList
        (if (and (listp entData) (assoc 0 entData))
          (progn
            (setq entStr (vl-princ-to-string entData))
            (if (not (member entStr seen))
              (progn
                (setq uniqueEnts (cons entData uniqueEnts))
                (setq seen (cons entStr seen))
              )
            )
          )
        )
      )
      ;; ����Ψһ����Ч��ʵ��
      (foreach entData (reverse uniqueEnts)
        (if (and (listp entData) (assoc 0 entData))
          (entmake
            (append
              entData
              (list
                (cons 8 "Asnap")
                (cons 62 256) ; �����ɫ
                (cons 370 -1) ; ����߿�
              )
            )
          )
          (princ "\n������Чʵ������")
        )
      )
    )
  )
)

(princ "\n[��ʾ] AutoSnap ƽ���������������0.3.36�棩�ѱ����ء�\n")
(princ "������������Ӱ��ԭͼ�������ñ��ݣ�����ʹ�á�\n")
(princ "��ǰ�汾����64λ Windows 10ƽ̨�ϵ�64λAutoCAD 2018��2024�г�����֤������֤������ƽ̨�Ͱ汾�ϵĿ����ԡ�\n")
(princ "��Ȩ���� 2025 �������������Ȩ����Windows��΢���ע���̱꣬AutoCAD��ŷ�ؿ˵�ע���̱ꡣ\n")
(princ "Դ����ʹ��Apache-2.0��ɣ���ӭ��ϵheimamsn@live.cn����Ľ������\n")
(princ "���벢�������� \"ASNAP\" �Կ�ʼ�ԣ���Σ�ֱ��ȡ����\n\n")
