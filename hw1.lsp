; CS 161 Spring 2013: HW1 skeleton

; HELPER FUNCTIONS

(defun REPLACE-SF-HELPER (slot filler frame)
	(cond
		((null frame) (list (list slot filler)))
		((eq slot (first (first frame))) (cons (list slot filler) (rest frame)))
		(t (cons (first frame) (REPLACE-SF-HELPER slot filler (rest frame))))))

(defun REPLACE-PTH-HELPER (path filler frame)
	(cond
		((eq (first frame) (first path))
			(cons (first frame) 
				(list (REPLACE-PTH (rest path) filler (second frame)))))
		(t frame)))
		
(defun ISEQUALP (frame1 frame2)
	(cond
		((null frame1) t)
		((CONTAINSP frame2 (first frame1))
			(ISEQUALP (rest frame1) frame2))
		(t nil)))

(defun CONTAINSP (frame element)
	(cond
		((null frame) nil)
		((COMPARE-FRMS element (first frame)) t)
		(t (ELEMENTP element (rest frame)))))		
		
; FUNCTION: FILLER
; PURPOSE:  Return the FILLER of the given SLOT in the given FRAME
; OUTPUT:   A FILLER (FRAME or GAP), according to slot in frame, or NIL if not
;           present
; INPUTS:   slot: atom
;           frame: FRAME

(defun FILLER (slot frame)
	(cond 
		((null (rest frame)) nil)
		((eq (first (second frame)) slot) (second (second  frame)))
		(t (FILLER slot (cons (first frame) (rest (rest frame)))))))

; -----------------------------------------------------------------------------

; FUNCTION: PATH-SL
; PURPOSE:  Return a FILLER accessed by following a path of named SLOTS within
;           the given CONCEPT
; OUTPUT:   FILLER of the CONCEPT at the given path, or NIL if not present
; INPUTS:   slots: list (path of SLOTs to follow)
;           concept: FRAME

(defun PATH-SL (slots concept)
	(cond
		((null slots) concept)
		(t (PATH-SL (rest slots) (FILLER (first slots) concept)))))

; -----------------------------------------------------------------------------

; FUNCTION: PATH-PS
; PURPOSE:  Return a FILLER accessed by following a path of predicate-slot
;           pairs, where each predicate in the concept must match the
;           predicates in the path. 
; OUTPUT:   FILLER of the CONCEPT at the given path. If any predicate fails to
;           match, return FAIL. If any SLOT in the path does not exist, return
;           NIL.
; INPUTS:   path: a list of alternating predicate names and slot names,
;           beginning with a PRED and ending with a SLOTh
;           concept: instantiated FRAME

(defun PATH-PS (path concept)
	(cond 
		((null path) concept)
		((eq (first path) (first concept))
			(cond
				((null (rest path)) nil)
				(t (PATH-PS (rest (rest path)) (FILLER (second path) concept)))))
		(t 'FAIL)))

; -----------------------------------------------------------------------------

; FUNCTION: GAPVALS
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, except that
;           any GAPs have been replaced by their values
; OUTPUT:   FRAME, with all GAPs replaced
; INPUTS:   frame: FRAME to evaluate GAPs in

(defun GAPVALS (frame)
	(cond
		((listp frame) 
			(cond
				((null frame) nil)
				(t (cons (first frame) (mapcar 'GAPVALS (rest frame))))))
		(t (GAPVALS (eval frame)))))

; -----------------------------------------------------------------------------

; FUNCTION: REPLACE-SF
; PURPOSE:  Return a FRAME which is a copy of the referenced frame, with the
;           designated SLOT filled in with the given FILLER (or added if the
;           SLOT does not exist)
; OUTPUT:   Copy of FRAME with the top-level SLOT slot filled with filler
; INPUTS:   slot: atom (name of slot to fill)
;           filler: FILLER (value to put in SLOT)
;           frame: FRAME (frame to be modified)

(defun REPLACE-SF (slot filler frame)
	(cons (first frame) (REPLACE-SF-HELPER slot filler (rest frame))))

; -----------------------------------------------------------------------------

; FUNCTION: REPLACE-PTH
; PURPOSE:  Return a FRAME which is a copy of the reference frame, with the
;           SLOT given in the designated PATH filled in with the given FILLER
;           (or added if that SLOT does not exist)
; OUTPUT:   Copy of FRAME with SLOT in path filled with filler
; INPUTS:   path: list (path of SLOTS to follow)
;           filler: FILLER (value to put in SLOT referenced by path)
;           frame: FRAME (frame to be modified) 

(defun REPLACE-PTH (path filler frame) 
	(cond
		((null (rest path)) (REPLACE-SF (first path) filler frame)
		(t (cons (first frame) 
			(mapcar (REPLACE-PTH-HELPER path filler) (rest frame)))))))

; -----------------------------------------------------------------------------

; FUNCTION: COMPARE-FRMS
; PURPOSE:  Boolean predicate which compares two frames
; OUTPUT:   T if frames have same slot-filler structure (order may vary),
;           NIL otherwise
; INPUTS:   frame1: FRAME (first frame to compare)
;           frame2: FRAME (second frame to compare)
		
(defun COMPARE-FRMS (frame1 frame2)
	(cond
		((equal (first frame1) (first frame2))
			(ISEQUALP (rest frame1) (rest frame2)))
		(t nil)))
