;;; Copyright 2025 李靖康 All rights reserved.
(princ "\n[提示] 已加载 AutoSnap 插件 (版本 0.1.0)\n")
(princ "Copyright 2025 李靖康 All rights reserved.\n")
(princ "运行 \"ASNAP\" 命令以开始规整图形\n\n")

(defun c:ASNAP (/ *error* main ss snapDist processedEntities 
                          round snapPoint processEntity processLine 
                          processPolyline processBlock getBlockTransformMatrix 
                          transformPoint processBlockEntity processTransformedLine 
                          processTransformedPolyline)

  ;;; --- 错误处理 --- ;;;
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\n错误: " msg))
    )
    (if (and (not (null ss)) (ssp ss)) (ssfree ss))
    (princ)
  )

  ;;; --- 主函数 --- ;;;
  (defun main ()
    (setq processedEntities (list))
    
    ;; 创建或设置目标图层
    (if (not (tblsearch "LAYER" "Snapped"))
      (command "_.-layer" "_m" "Snapped" "")
    )
    (setvar "CLAYER" "Snapped")
    
    ;; 获取并验证模糊距离
    (while (progn
             (initget 7 "25 50 100")
             (setq snapDist (getdist "\n请输入模糊距离 (25, 50或100): "))
             (cond
               ((null snapDist) nil) ; 用户取消
               ((not (member snapDist '(25.0 50.0 100.0)))
                (princ "\n错误: 模糊距离必须是25, 50或100! 请重新输入.")
                t) ; 继续循环
               (t nil) ; 有效输入，退出循环
             )
           )
    )
    (if (null snapDist) (exit))
    
    ;; 选择图形中所有实体
    (setq ss (ssget "X" '((0 . "LINE,LWPOLYLINE,POLYLINE,INSERT"))))
    (if (null ss)
      (progn
        (princ "\n警告: 图形中没有找到可处理的实体!")
        (exit)
      )
    )
    
    ;; 处理所有实体
    (setq idx 0)
    (repeat (sslength ss)
      (setq ent (ssname ss idx))
      (processEntity ent snapDist)
      (setq idx (1+ idx))
    )
    
    (princ "\n端点规整完成! 新图形绘制在 [Snapped] 图层")
  )

  ;;; --- 核心函数 --- ;;;
  (defun round (x) (fix (+ x (if (minusp x) -0.5 0.5))))

  (defun snapPoint (pt dist)
    (list
      (* dist (round (/ (car pt) dist)))
      (* dist (round (/ (cadr pt) dist)))
      0.0
    )
  )

  (defun processEntity (ent dist)
    (setq entType (cdr (assoc 0 (entget ent))))
    (cond
      ((= entType "LINE") (processLine ent dist))
      ((wcmatch entType "LWPOLYLINE,POLYLINE") (processPolyline ent dist))
      ((= entType "INSERT") (processBlock ent dist))
    )
  )

  ;; 处理直线
  (defun processLine (ent dist / data p1 p2 np1 np2)
    (setq data (entget ent))
    (setq p1 (cdr (assoc 10 data)))
    (setq p2 (cdr (assoc 11 data)))
    (setq np1 (snapPoint p1 dist)
          np2 (snapPoint p2 dist))
    
    (if (not (equal np1 np2 1e-6))
      (entmakex
        (list
          '(0 . "LINE")
          (cons 10 np1)
          (cons 11 np2)
          (cons 8 "Snapped")
        )
      )
    )
  )

  ;; 处理多段线
  (defun processPolyline (ent dist / data closed newPts lastPt newEnt)
    (setq data (entget ent))
    (setq closed (if (= (cdr (assoc 70 data)) 1) t nil))
    
    ;; 收集顶点
    (setq newPts (list))
    (setq lastPt nil)
    
    ;; 处理轻量多段线 (LWPOLYLINE)
    (if (= (cdr (assoc 0 data)) "LWPOLYLINE")
      (progn
        (setq idx 0)
        (while (setq vtx (assoc 10 data))
          (setq data (cdr (member vtx data)))
          (setq pt (cdr vtx))
          (setq newPt (snapPoint pt dist))
          
          (if (or (null lastPt) (not (equal newPt lastPt 1e-6)))
            (progn
              (setq newPts (cons newPt newPts))
              (setq lastPt newPt)
            )
          )
        )
      )
      
      ;; 处理旧式多段线 (POLYLINE)
      (progn
        (setq vtxEnt (entnext ent))
        (while (and vtxEnt (= (cdr (assoc 0 (entget vtxEnt))) "VERTEX"))
          (setq vtxData (entget vtxEnt))
          (setq pt (cdr (assoc 10 vtxData)))
          (setq newPt (snapPoint pt dist))
          
          (if (or (null lastPt) (not (equal newPt lastPt 1e-6)))
            (progn
              (setq newPts (cons newPt newPts))
              (setq lastPt newPt)
            )
          )
          (setq vtxEnt (entnext vtxEnt))
        )
      )
    )
    
    (setq newPts (reverse newPts))
    
    ;; 创建新多段线
    (if (>= (length newPts) 2)
      (progn
        (setq newEnt 
          (entmakex
            (append
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (cons 8 "Snapped")
                (cons 90 (length newPts))
                (cons 70 (if (and closed 
                                  (>= (length newPts) 3)
                                  (not (equal (car newPts) (last newPts) 1e-6)))
                           1 0))
              )
              (apply 'append 
                (mapcar '(lambda (pt) (list (cons 10 pt))) newPts)
              )
            )
          )
        )
      )
    )
  )

  ;;; --- 图块处理 --- ;;;
  (defun processBlock (ent dist / blkName blkDef subEnt mat)
    (setq data (entget ent))
    (setq blkName (cdr (assoc 2 data)))
    (setq mat (getBlockTransformMatrix ent))
    
    (setq blkDef (tblobjname "BLOCK" blkName))
    (setq subEnt (entnext blkDef))
    
    (while subEnt
      (processBlockEntity subEnt mat dist)
      (setq subEnt (entnext subEnt))
    )
  )

  (defun getBlockTransformMatrix (ent / data insPoint scale rotAngle xdir ydir)
    (setq data (entget ent))
    (setq insPoint (cdr (assoc 10 data)))
    (setq scale (cdr (assoc 41 data)))
    (if (null scale) (setq scale 1.0))
    (setq rotAngle (cdr (assoc 50 data)))
    (if (null rotAngle) (setq rotAngle 0.0))
    (setq xdir (list (cos rotAngle) (sin rotAngle) 0.0))
    (setq ydir (list (- (sin rotAngle)) (cos rotAngle) 0.0))
    (list
      (list (* scale (car xdir)) (* scale (cadr xdir)) 0.0 (car insPoint))
      (list (* scale (car ydir)) (* scale (cadr ydir)) 0.0 (cadr insPoint))
      '(0.0 0.0 1.0 0.0)
      '(0.0 0.0 0.0 1.0)
    )
  )

  (defun transformPoint (pt matrix)
    (list
      (+ (* (car pt) (caar matrix)) 
         (* (cadr pt) (caadr matrix)) 
         (caddar matrix) 
         (car (cadddr matrix)))
      (+ (* (car pt) (cadar matrix)) 
         (* (cadr pt) (cadadr matrix)) 
         (cadddr (cadr matrix)) 
         (cadr (cadddr matrix)))
      0.0
    )
  )

  (defun processBlockEntity (ent matrix dist)
    (setq entType (cdr (assoc 0 (entget ent))))
    (cond
      ((= entType "LINE")
        (processTransformedLine ent matrix dist)
      )
      ((wcmatch entType "LWPOLYLINE,POLYLINE")
        (processTransformedPolyline ent matrix dist)
      )
      ((= entType "INSERT")
        (processBlock ent dist)
      )
    )
  )

  (defun processTransformedLine (ent matrix dist / data p1 p2 np1 np2)
    (setq data (entget ent))
    (setq p1 (cdr (assoc 10 data)))
    (setq p2 (cdr (assoc 11 data)))
    (setq tp1 (transformPoint p1 matrix)
          tp2 (transformPoint p2 matrix)
          np1 (snapPoint tp1 dist)
          np2 (snapPoint tp2 dist))
    
    (if (not (equal np1 np2 1e-6))
      (entmakex
        (list
          '(0 . "LINE")
          (cons 10 np1)
          (cons 11 np2)
          (cons 8 "Snapped")
        )
      )
    )
  )

  (defun processTransformedPolyline (ent matrix dist / data closed newPts lastPt newEnt)
    (setq data (entget ent))
    (setq closed (if (= (cdr (assoc 70 data)) 1) t nil))
    
    ;; 收集顶点
    (setq newPts (list))
    (setq lastPt nil)
    
    ;; 处理轻量多段线 (LWPOLYLINE)
    (if (= (cdr (assoc 0 data)) "LWPOLYLINE")
      (progn
        (while (setq vtx (assoc 10 data))
          (setq data (cdr (member vtx data)))
          (setq pt (cdr vtx))
          (setq tp (transformPoint pt matrix))
          (setq newPt (snapPoint tp dist))
          
          (if (or (null lastPt) (not (equal newPt lastPt 1e-6)))
            (progn
              (setq newPts (cons newPt newPts))
              (setq lastPt newPt)
            )
          )
        )
      )
      
      ;; 处理旧式多段线 (POLYLINE)
      (progn
        (setq vtxEnt (entnext ent))
        (while (and vtxEnt (= (cdr (assoc 0 (entget vtxEnt))) "VERTEX"))
          (setq vtxData (entget vtxEnt))
          (setq pt (cdr (assoc 10 vtxData)))
          (setq tp (transformPoint pt matrix))
          (setq newPt (snapPoint tp dist))
          
          (if (or (null lastPt) (not (equal newPt lastPt 1e-6)))
            (progn
              (setq newPts (cons newPt newPts))
              (setq lastPt newPt)
            )
          )
          (setq vtxEnt (entnext vtxEnt))
        )
      )
    )
    
    (setq newPts (reverse newPts))
    
    ;; 创建新多段线
    (if (>= (length newPts) 2)
      (progn
        (setq newEnt 
          (entmakex
            (append
              (list
                '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                (cons 8 "Snapped")
                (cons 90 (length newPts))
                (cons 70 (if (and closed 
                                  (>= (length newPts) 3)
                                  (not (equal (car newPts) (last newPts) 1e-6)))
                           1 0))
              )
              (apply 'append 
                (mapcar '(lambda (pt) (list (cons 10 pt))) newPts)
              )
            )
          )
        )
      )
    )
  )

  ;;; --- 执行主函数 --- ;;;
  (main)
  (princ)

)

