;;; ========================================================================================================================
;;; AutoSnap plugin for AutoCAD.
;;; A tool shall eliminate planar fragments of lines and polylines (except arc) in the selected graph.
;;; Author：李靖康
;;; Copyright 2025 李靖康 All rights reserved.
;;; Version 0.3.36 (2025 Aug. 8th 3:24 UTC/GMT+08:00) published under Apache-2.0 license.
;;; This version has been tested on AutoCAD 2018 and 2024 (64 bit) on Windows 10 amd64 Platform over GB18030 encode method.
;;; Windows is a brand of Microsoft, AutoCAD is a brand of Autodesk.
;;; Contact heimamsn@live.cn if you have suggestions.
;;; ========================================================================================================================

;;; 主命令函数
(defun C:ASNAP (/ *error* doc ss fuzzDist processedEnts newLayer)
  ;; 错误处理函数
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\n错误: " msg))
    )
    (princ)
  )
  
  ;; 获取文档
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  ;; 创建ASNAP图层
  (setq newLayer (create-asnap-layer doc))
  
  ;; 获取用户选择集
  (prompt "\n选择直线/多段线/图块 (或直接回车选择全部): ")
  (setq ss (get-selection-set))
  
  ;; 获取模糊距离
  (setq fuzzDist nil)
  (while (not fuzzDist)
    (setq fuzzDist (get-fuzz-distance))
  )
  
  ;; 处理实体
  (setq processedEnts (process-entities ss fuzzDist newLayer))
  
  ;; 绘制最终结果
  (draw-final-entities processedEnts newLayer)
  
  (princ "\n操作完成! ")
  (princ)
)

;;; 创建专用图层
(defun create-asnap-layer (doc / layers layer)
  (setq layers (vla-get-Layers doc))
  (if (not (tblsearch "LAYER" "Asnap"))
    (progn
      (setq layer (vla-Add layers "Asnap"))
      (vla-put-Color layer 1) ; 红色
      (vla-put-Lineweight layer acLnWt030) ; 0.3mm
    )
    (setq layer (vla-Item layers "Asnap"))
  )
  layer
)

;;; 获取用户选择集
(defun get-selection-set (/ ss)
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,INSERT"))))
  (if (not ss)
    (progn
      (prompt "\n未选择对象，将处理模型空间中所有对象...")
      (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE,INSERT"))))
    )
  )
  ss
)

;;; 获取模糊距离
(defun get-fuzz-distance (/ input)
  (initget 1)
  (setq input (getreal "\n请输入模糊距离: "))
  (if (not (numberp input))
    (progn
      (prompt "\n错误: 必须输入数字! ")
      nil
    )
    input
  )
)

;;; 处理实体主函数
(defun process-entities (ss fuzzDist newLayer / count i ent processed result)
  (if (not ss)
    (progn
      (princ "\n未找到可处理的对象!")
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
          (princ (strcat "\n跳过无法处理的实体: " (cdr (assoc 0 (entget ent)))))
        )
        
        (setq i (1+ i))
        (princ (strcat "\n处理进度: " (itoa i) "/" (itoa count)))
      )
      result
    )
  )
)

;;; 处理单个实体
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

;;; 处理直线
(defun process-line (ent fuzzDist / data start end newStart newEnd)
  (setq data (entget ent))
  
  ;; 获取并处理起点终点
  (setq start (cdr (assoc 10 data)))
  (setq end (cdr (assoc 11 data)))
  
  (setq newStart (snap-point start fuzzDist))
  (setq newEnd (snap-point end fuzzDist))
  
  ;; 检查是否退化为点
  (if (equal newStart newEnd 1e-6)
    nil ; 忽略退化的线
    (list
      (cons 0 "LINE")
      (cons 10 newStart)
      (cons 11 newEnd)
    )
  )
)

;;; 处理多段线
(defun process-lwpolyline (ent fuzzDist / data vertices closed newVerts i pt prevPt filteredVerts)
  (setq data (entget ent))
  (setq closed (if (= 1 (logand 1 (cdr (assoc 70 data)))) T nil))
  
  ;; 获取顶点列表
  (setq vertices (vl-remove-if-not '(lambda (x) (= (car x) 10)) data))
  (setq newVerts nil)
  
  ;; 处理每个顶点
  (foreach v vertices
    (setq pt (snap-point (cdr v) fuzzDist))
    (setq newVerts (cons pt newVerts))
  )
  (setq newVerts (reverse newVerts))
  
  ;; 移除连续重合点
  (setq filteredVerts nil)
  (foreach pt newVerts
    (if (or (null (car filteredVerts)) 
            (not (equal pt (car filteredVerts) 1e-6)))
      (setq filteredVerts (cons pt filteredVerts))
    )
  )
  (setq filteredVerts (reverse filteredVerts))
  
  ;; 检查闭合多段线的首尾点
  (if (and closed 
          (> (length filteredVerts) 2)
          (not (equal (car filteredVerts) (last filteredVerts) 1e-6)))
    (setq filteredVerts (append filteredVerts (list (car filteredVerts))))
  )
  ;; 返回新数据
  (cond
    ((< (length filteredVerts) 2) ; 顶点不足
      nil)
    ((= (length filteredVerts) 2) ; 只有两个点，转为直线
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
          (cons 38 0.0) ; Z坐标归零
        )
        (mapcar '(lambda (pt) (cons 10 pt)) filteredVerts)
      )
    )
  )
)

;;; 处理图块
(defun process-block (ent fuzzDist newLayer / blkDef blkName newName newDef entData)
  (setq entData (entget ent))
  (setq blkName (cdr (assoc 2 entData)))
  (setq blkDef (tblobjname "BLOCK" blkName))
  
  ;; 创建新块定义
  (setq newName (strcat blkName "_ASNAP"))
  
  (if (setq oldBlk (tblobjname "BLOCK" newName))
    (progn
      (entdel oldBlk) ; 删除块定义
      (entdel (entnext oldBlk)) ; 删除对应的ENDBLK
      (princ (strcat "\n已更新存在的块: " newName))
    )
  )
  
  ;; 处理块内实体
  (setq newDef (process-block-definition blkDef fuzzDist newLayer newName))
  (if newDef
    (progn
      (setq insertData (list (cons 0 "INSERT") (cons 2 newName)))
      ;; 添加插入点和其他属性
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

;;; 处理块定义
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
      ;; 强制创建新块定义
      (entmake (list (cons 0 "BLOCK") (cons 2 newName) (cons 70 0) (cons 10 (list 0.0 0.0 0.0))))
      (foreach entData (reverse newEnts)
        (entmake entData)
      )
      (entmake '((0 . "ENDBLK")))
      newName ; 返回新块名
    )
    nil
  )
)

;;; 坐标对齐函数
(defun snap-point (pt fuzzDist / x y)
  (setq x (car pt)
        y (cadr pt))
  (list
    (if (zerop fuzzDist) x (* fuzzDist (fix (+ (/ x fuzzDist) 0.5)))) ; X四舍五入
    (if (zerop fuzzDist) y (* fuzzDist (fix (+ (/ y fuzzDist) 0.5)))) ; Y四舍五入
    0.0
  )
)

;;; 绘制最终实体
(defun draw-final-entities (entList newLayer / entData uniqueEnts entStr seen)
  (if (not entList)
    (princ "\n没有可绘制的实体!")
    (progn
      ;; 去除重复实体
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
      ;; 绘制唯一且有效的实体
      (foreach entData (reverse uniqueEnts)
        (if (and (listp entData) (assoc 0 entData))
          (entmake
            (append
              entData
              (list
                (cons 8 "Asnap")
                (cons 62 256) ; 随层颜色
                (cons 370 -1) ; 随层线宽
              )
            )
          )
          (princ "\n跳过无效实体数据")
        )
      )
    )
  )
)

(princ "\n[提示] AutoSnap 平面正交规整插件（0.3.36版）已被加载。\n")
(princ "后续操作可能影响原图，请做好备份，谨慎使用。\n")
(princ "当前版本已在64位 Windows 10平台上的64位AutoCAD 2018与2024中初步验证，不保证在其他平台和版本上的可用性。\n")
(princ "版权所有 2025 李靖康，保留所有权利。Windows是微软的注册商标，AutoCAD是欧特克的注册商标。\n")
(princ "源代码使用Apache-2.0许可，欢迎联系heimamsn@live.cn提出改进意见。\n")
(princ "输入并运行命令 \"ASNAP\" 以开始对（多段）直线取整。\n\n")
