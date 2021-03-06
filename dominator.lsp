;================================================
;Dominator!!
;==============================================================

;main function==============================================================
(defun c:GiveMeDominator (/ ID)
  (graphscr)(osnapoff)
  (if (null rpoint) (load "util.lsp"))
  (setq ID (getreal "\n ID checking... <ENTER YOUR ID> : "))
  (princ "\n ID Correct!")
  (gun-base ID)
  (gun-front-base ID)
  (gun-front-bottom ID)
  (gun-handA ID)
  (gun-handB ID)
  (gun-handC ID)
  (gun-handD ID)
  (gun-handE ID)
  (gun-handF ID)
  (gun-trigger ID)
  (gun-top ID)
  (gun-toptop ID)
  (gun-front-top ID)
  (gun-front-top-things ID)
  (gun-front-arrow ID)
  (princ "\n Here is the Dominator! Carefully use it!")
  ;(command "zoom" "e" "zoom" "0.5x")
  (princ)
)
;gunbase drawing======================================================================
(defun gun-base (ID / ip p1 p2)
  (setq ip '(0 0)
	p1 '(8.5 14.7224)
	p2 '(8.5 24.7224)
	p3 '(-0.0505 48.2147)
	p4 '(-14.8226 50.8195)
	p5 '(-27.813 43.3195)
	p6 '(-37.813 43.3195)
	p7 '(-37.813 48.3195)
	p8 '(-27.813 48.3195)
	p9 '(-14.8226 55.8195)
	p10 '(-11.4024 65.2164)
	p11 '(-13.9024 69.5465)
	p12 '(-17.7326 72.7605)
	p13 '(-27.7326 72.7605)
	p14 '(-36.3929 77.7605)
	p15 '(-44.2714 76.3713)
	c1 '(-54.8018 81.3467)
	p16 '(-50.4868 92.1645)
	p17 '(-185.9693 80.3113)
	p18 '(-192.3971 87.9718)
	p19 '(-188.0559 112.592)
	p20 '(11.9441 112.592)
	p21 '(49.5318 98.9112)
	p22 '(30.2481 75.9298)
	p23 '(31.9846 66.0818)
	p24 '(46.9846 40.101)
	p25 '(46.9846 17.101)
	c2 '(78.5698 -63.9081)
	p26 '(14.3969 -5.2401)
	p27 '(5 -8.6603)
	)
  (command "pline" ip p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 "a" "ce" c1 p16 "L"
	   p17 p18 p19 p20 p21 p22 p23 p24 p25 "a" "ce" c2 p26 "L" p27 "c")
  (setq e1 (entlast))
  (command "extrude" e1 "" -10 "") (setq e1 (entlast))
  )
;gun front base drawing===============================================================================
(defun gun-front-base (ID / p28 p29 p30)
  (setq p28 '(-41.8426 43.9203)
	p29 '(-52.772 45.8313)
	c3 '(-55.6614 33.7855)
 	p30 '(-66.0415 40.5436)
	p31 '(-73.1257 38.0047)
	p32 '(-87.1356 46.8267)
	p33 '(-171.068 -8.9952)
	p34 '(-187.1237 25.9225)
	p35 '(-79.267 59.4842)
	p36 '(-84.4487 66.1965)
	p37 '(-92.7011 68.3061)
	p38 '(-189.1641 42.6444)
	p39 '(-74.9592 80.2438)
	c4 '(-67.126 72.7132)
	p40 '(-58.7613 67.9574)
	p41 '(-62.4872 63.8579)
	c5 '(-55.2203 59.7213)
	p42 '(-56.3909 51.4419)
	p43 '(-41.8426 48.1782)
	littleangle -10
	)
  (command "pline" p28 p29 "a" "ce" c3 p30 "L" p31 p32 p33 p34 p35 p36 p37 p38 p39
	   "a" "ce" c4 "a" littleangle p40 "L" p41 "a" "ce" c5 p42 "L" p43 "c")
  (setq e2 (entlast))
  (command "extrude" e2 "" -10 "") (setq e2 (entlast))
  (command "rotate" e2 "" p6 -13);back to original
    (princ)
)

;======================================================================
(defun gun-trigger (ID / ip p1 p2)
  (setq p44 '(-22.198 56.9756)
        p45 '(-16.9729 62.5052)
        p46 '(-19.1515 64.7178)
        p47 '(-19.1515 72.7605)
        p48 '(-25.3673 72.7605)
        p49 '(-23.7991 67.2753)
        p50 '(-24.435 62.9888)
        p51 '(-25.5492 59.5345)
        p52 '(-31.3827 58.2118)
        p53 '(-30.0325 57.1973)
        p54 '(-26.2911 57.1973)
	)
  (command "ucs" '(0 0 -2.5) "")
  (command "pline" p44 p45 p46 p47 p48 p49 p50 p51 p52 p53 p54 "c")
  (setq e3 (entlast))
  (command "extrude" e3 "" -5 "") (setq e3 (entlast))
  (command "ucs" '(0 0 2.5) "")
)
;======================================================================
(defun gun-handA (ID / ip p1 p2)
  (setq p55 '(8.9256 6.8509)
        c6 '(13.2684 5.2471)
        p56 '(14.8752 0.9054)
        p57 '(29.4557 14.1873)
        p58 '(43.1359 19.1665)
        p59 '(43.3484 35.3041)
        p60 '(30.3867 62.2709)
        p61 '(24.6496 62.9079)
        p62 '(8.9762 45.6892)
        p63 '(15.8745 25.6984)
	)
  (command "pline" p55 "a" "ce" c6 p56 "L" p57 p58 p59 p60 p61 p62 p63 "c")
  (setq e4 (entlast))
  (command "extrude" e4 "" 2.5 "") (setq e4 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p55 "a" "ce" c6 p56 "L" p57 p58 p59 p60 p61 p62 p63 "c")
  (setq e4b (entlast))
  (command "extrude" e4b "" -2.5 "") (setq e4b (entlast))
  (command "ucs" '(0 0 +10) "")
)
;======================================================================
(defun gun-handB (ID / ip p1 p2)
  (setq p64 '(3.8374 45.0563)
        p65 '(35.7875 85.4364)
        p66 '(31.6481 86.0456)
        p67 '(25.3909 81.9732)
        p68 '(19.3904 85.757)
        p69 '(12.8764 81.4281)
        p70 '(4.9826 85.3402)
        p71 '(3.4103 81.4602)
        p72 '(-5.2113 78.6239)
        p73 '(-6.4227 74.1894)
        p74 '(-1.6672 65.7321)
        p75 '(-4.327 51.1534)
        p75 '(-2.3926 51.1534)
        p76 '(0.2454 52.022)
        p77 '(3.5668 49.8646)
        p78 '(2.8916 46.035)
	)
  (command "pline" p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 "c")
  (setq e5 (entlast))
  (command "extrude" e5 "" 4.2 "") (setq e5 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 "c")
  (setq e5b (entlast))
  (command "extrude" e5b "" -4.2 "") (setq e5b (entlast))
  (command "ucs" '(0 0 +10) "")
)

;======================================================================
(defun gun-handC (ID / ip p1 p2)
  (setq p79 '(5.6731 59.8508)
        p80 '(7.3375 53.199)
        p81 '(21.1635 71.2846)
        p82 '(25.9699 75.1982)
        p83 '(25.7326 77.8666)
        p84 '(24.1898 80.0013)
        p85 '(-0.2789 76.6421)
        p86 '(-0.907 73.0407)
        p87 '(3.1474 71.8402)
        p88 '(-0.907 58.0348)
	)
  (command "ucs" '(0 0 4.2) "")
  (command "pline" p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 "c")
  (setq e6 (entlast))
  (command "extrude" e6 "" 1.5 "") (setq e6 (entlast))
  (command "ucs" '(0 0 -18.4) "")
  (command "pline" p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 "c")
  (setq e6b (entlast))
  (command "extrude" e6b "" -1.5 "") (setq e6b (entlast))
  (command "ucs" '(0 0 +14.2) "")
)

;======================================================================
(defun gun-handD (ID / ip p1 p2)
  (setq p89 '(26.1067 85.597)
        p90 '(37.9344 89.791)
        p91 '(54.0627 93.5335)
        p92 '(38.3856 101.1834)
        p93 '(0.686 101.1834)
        p94 '(-26.7364 89.9816)
        p95 '(-19.1057 83.2909)
        p96 '(-15.9132 85.153)
        p97 '(-6.3241 81.2123)
        p98 '(2.0443 85.3148)
        p99 '(5.6944 89.4097)
        p100 '(12.5621 85.2157)
        p101 '(19.239 89.781)
	)
  (command "pline" p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100 p101 "c")
  (setq e7 (entlast))
  (command "extrude" e7 "" 3.5 "") (setq e7 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100 p101 "c")
  (setq e7b (entlast))
  (command "extrude" e7b "" -3.5 "") (setq e7b (entlast))
  (command "ucs" '(0 0 +10) "")
)
;======================================================================
(defun gun-handE (ID / ip p1 p2)
  (setq p102 '(-39.6836 85.8156)
        p103 '(-21.9852 85.8156)
        p104 '(-26.7364 89.9816)
        p105 '(-38.9062 89.9816)
        p106 '(-50.4877 112.5936)
        p107 '(-61.062 99.9931)
	)
  (command "pline" p102 p103 p104 p105 p106 p107 "c")
  (setq e8 (entlast))
  (command "extrude" e8 "" 1 "") (setq e8 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p102 p103 p104 p105 p106 p107 "c")
  (setq e8b (entlast))
  (command "extrude" e8b "" -1 "") (setq e8b (entlast))
  (command "ucs" '(0 0 +10) "")
)

;======================================================================
(defun gun-handF (ID / ip p1 p2)
  (setq p109 '(25.9611 45.4161)
        c7 '(24.1826 44.7789)
        p110 '(22.4081 44.1306)
        p111 '(29.2847 25.1237)
        c8 '(31.0632 25.1237)
        p112 '(32.8377 26.4092)
	)
  (command "ucs" '(0 0 2.5) "")
  (command "pline" p109 "a" "ce" c7 p110 "L" p111 "a" "ce" c8 p112 "L" "c")
  (setq e9 (entlast))
  (command "extrude" e9 "" 1 "") (setq e9 (entlast))
  (command "ucs" '(0 0 -15) "")
  (command "pline" p109 "a" "ce" c7 p110 "L" p111 "a" "ce" c8 p112 "L" "c")
  (setq e9b (entlast))
  (command "extrude" e9b "" -1 "") (setq e9b (entlast))
  (command "ucs" '(0 0 12.5) "")
)

;======================================================================
(defun gun-top (ID / ip p1 p2)
  (setq p113 '(35.874 105.5418)
        p114 '(49.543 124.5131)
        p115 '(35.5892 131.7223)
        p116 '(-54.3983 131.7223)
        p117 '(-65.5992 118.0629)
        p118 '(-58.1003 103.5498)
        p119 '(-45.1907 103.5498)
	p119c '(-25.5485 92.8822)
        p120 '(-19.8119 114.0789)
        p121 '(18.6358 114.0789)
	)
  (command "pline" p113 p114 p115 p116 p117 p118 p119 p120 p121 "c")
  (setq e10 (entlast))
  (command "extrude" e10 "" 4.2 "") (setq e10 (entlast))
  (command "move" e10 "" p119 p119c)
  (command "ucs" '(0 0 -10) "")
  (command "pline" p113 p114 p115 p116 p117 p118 p119 p120 p121 "c")
  (setq e10b (entlast))
  (command "extrude" e10b "" -4.2 "") (setq e10b (entlast))
  (command "move" e10b "" p119 p119c)
  (command "ucs" '(0 0 10) "")
)
;======================================================================
(defun gun-toptop (ID / ip p1 p2)
  (setq p122 '(-39.7561 129.715)
	p122c '(-34.7561 121.0547)
	ptoptopmovenew p122c
	p123 '(33.5183 139.7572)
	p124 '(42.9392 134.3678)
	p125 '(45.2794 129.715)
	)
  (command "pline" p122 p123 p124 p125 "c")
  (setq e11 (entlast))
  (command "extrude" e11 "" -10 "") (setq e11 (entlast))
  (command "move" e11 "" p122 p122c)
)
;======================================================================
(defun gun-front-top (ID / ip p1 p2)
  (setq p126 '(-69.9363 93.2045)
	p127 '(-56.0141 111.8847)
	p128 '(-78.1228 109.8485)
	p129 '(-79.9042 116.2275)
	p130 '(-202.822 123.6448)
	p131 '(-215.5888 91.8985)
	p132 '(-206.9786 76.4704)
	p133 '(-203.4002 85.8061)
	p134 '(-187.9897 90.2335)
	)
  (command "pline" p126 p127 p128 p129 p130 p131 p132 p133 p134 "c")
  (setq e12 (entlast))
  (command "extrude" e12 "" 5 "") (setq e12 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p126 p127 p128 p129 p130 p131 p132 p133 p134 "c")
  (setq e12b (entlast))
  (command "extrude" e12b "" -5 "") (setq e12b (entlast))
  (command "ucs" '(0 0 10) "")
)
;======================================================================
(defun gun-front-bottom (ID / ip p1 p2)
  (setq p135 '(-200.9631 24.8607)
	c9 '(-192.3607 29.0924)
    	p136 '(-191.8455 19.5193)
	p137 '(-86.4777 41.0742)
	p138 '(-76.7417 56.9933)
	p139 '(-90.5498 62.8496)
	p140 '(-101.3155 56.4914)
	p141 '(-107.7923 79.7545)
	p142 '(-75.7901 98.6689)
	p143 '(-86.883 107.3059)
	p144 '(-203.1716 88.8125)
	c10 '(-202.2292 82.887)
	p145 '(-208.1418 81.8666)
	)
  (command "pline" p135 "a" "ce" c9 p136 "L"
	   p137 p138 p139 p140 p141 p142 p143
	   p144 "a" "ce" c10 p145 "L" "c")
  (setq e13 (entlast))
  (command "extrude" e13 "" 6 "") (setq e13 (entlast))
  (command "ucs" '(0 0 -10) "")
  (command "pline" p135 "a" "ce" c9 p136 "L"
	   p137 p138 p139 p140 p141 p142 p143
	   p144 "a" "ce" c10 p145 "L" "c")
  (setq e13b (entlast))
  (command "extrude" e13b "" -6 "") (setq e13b (entlast))
  (command "ucs" '(0 0 10) "")
)

;======================================================================
(defun gun-front-arrow (ID / ip p1 p2)
  (setq p146 '(-166.4978 90.2806)
    	p147 '(-163.5109 90.4781)
	p148 '(-164.4053 104.0035)
	p149 '(-161.9868 106.3941)
	p150 '(-158.9147 104.1619)
	p151 '(-158.0408 90.9471)
	p152 '(-155.7218 87.2039)
	p153 '(-154.5753 69.8671)
	p154 '(-134.1958 35.3433)
	p155 '(-144.5527 35.0314)
	p156m '(-165.276 71.8057)
	p157m '(-149.3189 74.4027)
	p158m '(-132.3347 78.3986)
	)
  		;==front===
  (command "ucs" '(0 0 6) "")
  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e14 (entlast))
  (command "extrude" e14 "" 1 "") (setq e14 (entlast))

  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e15 (entlast))
  (command "extrude" e15 "" 1 "") (setq e15 (entlast))
  (command "move" e15 "" p156m p157m)

  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e16 (entlast))
  (command "extrude" e16 "" 1 "") (setq e16 (entlast))
  (command "move" e16 "" p156m p158m)
            ;==back===
  (command "ucs" '(0 0 -22) "")
  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e14b (entlast))
  (command "extrude" e14b "" -1 "") (setq e14b (entlast))

  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e15b (entlast))
  (command "extrude" e15b "" -1 "") (setq e15b (entlast))
  (command "move" e15b "" p156m p157m)

  (command "pline" p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156m "c")
  (setq e16b (entlast))
  (command "extrude" e16b "" -1 "") (setq e16b (entlast))
  (command "move" e16b "" p156m p158m)

  (command "ucs" '(0 0 10) "")
)
;======================================================================
(defun gun-front-top-things (ID / ip p1 p2)
  (setq p159 '(-103.3836 94.784)
	p160 '(-78.4804 98.2197)
	p161 '(-83.7944 102.9987)
	p162 '(-106.4258 103.5874)
	p163 '(-111.786 98.2197)

	p164 '(-104.8054 99.0376)
	p165 '(-86.1771 98.3055)
	p166 '(-86.0861 100.6218)
	p167 '(-104.7144 101.3538)
	)
  (command "ucs" '(0 0 5) "")
  (command "pline" p159 p160 p161 p162 p163 "c")(setq e17 (entlast))
  (command "extrude" e17 "" 1 "") (setq e17 (entlast))
  (command "pline" p164 p165 p166 p167 "c") (setq e17m (entlast))
  (command "extrude" e17m "" 1 "") (setq e17m (entlast))
					
  (command "ucs" '(0 0 -20) "")
  (command "pline" p159 p160 p161 p162 p163 "c")(setq e17b (entlast))
  (command "extrude" e17b "" 1 "") (setq e17b (entlast))
  (command "pline" p164 p165 p166 p167 "c") (setq e17mb (entlast))
  (command "extrude" e17mb "" 1 "") (setq e17mb (entlast))
  (command "ucs" '(0 0 15) "")
)

;transform==============================================================================
(defun c:transform( / dth delta)
    (princ "\nStarting transform! ")
    	(setq dth 0
	      delta 1
	)
    (while (and (setq key (cadr (grread))) (/= 13 key) )
    (if (= key 8)  ;<back space> is pressed
        (progn
	  (setq delta 1)
	  (setq dth (+ dth delta))
	  (setq ptoptopmove '(-7.262 2.0016)); ? ? / 5
	  (setq pfronttopmove '(-3.0158 1.4325));-30.1575 14.3248 / 10
	  (setq deltaC -13);-180 / 15
	  (setq pfrontbottommoveout '(0 5)) ;0 10/ 2
	  (setq pfrontbottommoveout2 '(0 -5)) ;0 -10/ 2
	  (setq pfrontbottommoveforward '(-8.019 -1.0193));-16.038 -2.0385 / 2
	  (setq pfrontarrowout '(0 5)) ;0 5 /2
	  (setq pfrontarrowout2 '(0 -5)) ;0 -5 /2
	 )
       (progn
          (setq delta -1)
	  (setq dth (+ dth delta))
	  (setq ptoptopmove '(7.262 -2.0016))
	  (setq pfronttopmove '(3.0158 -1.4325))
	  (setq deltaC 13)
	  (setq pfrontbottommoveout '(0 -5))
	  (setq pfrontbottommoveout2 '(0 5))
	  (setq pfrontbottommoveforward '(8.019 1.0193))
	  (setq pfrontarrowout '(0 -5)) 
	  (setq pfrontarrowout2 '(0 5)) 
	)
    )
    (if (<= dth 0)
      	(progn
          (setq delta 0)
	  (setq dth 0)
	  (setq deltaC 0)
	  (setq pfrontbottommoveout '(0 0))
	  (setq pfrontbottommoveout2 '(0 0))
	  (setq pfrontarrowout2 '(0 0))
	  (setq pfrontarrowout '(0 0)) 
	 )
    )
    (if (>= dth 4)
      	(progn
	  (setq pfrontbottommoveout '(0 0))
	  (setq pfrontbottommoveout2 '(0 0))
	  (setq pfrontarrowout2 '(0 0))
	  (setq pfrontarrowout '(0 0)) 
	 )
    )

    (if (<= dth 10)
      	(progn
	  (setq pfronttopmove '(0 0))
	 )
    )
    (if (<= dth 15)
      	(progn
	  (setq ptoptopmove '(0 0))
	 )
    )
    (if (>= dth 15)
      	(progn
	  (setq deltaC 0)
	 )
    )
    (if (<= dth 17)
      	(progn
	  (setq pfrontbottommoveforward '(0 0))
	 )
    )
    (if (>= dth 20)
      	(progn
          (setq delta 0)
	  (setq dth 20)
	  (setq ptoptopmove '(0 0))
	  (setq pfronttopmove '(0 0))
	  (setq pfrontbottommoveforward '(0 0))
	 )
    )
    
    (command "rotate" e2 "" p6 delta);part one rotate 15 degree
    (command "move" e10 e10b e11 "" '(0 0) ptoptopmove)
    (command "move" e12 e12b e17 e17b e17m e17mb "" '(0 0) pfronttopmove)


    (command "rotate" e13 e13b e14 e14b e15 e15b e16 e16b "" '(-148 59.5429) deltaC)
    (command "move" e13 e13b e14 e14b e15 e15b e16 e16b "" '(0 0) pfrontbottommoveforward)
      
    (command "ucs" "x" 90)
    (command "move" e13 e14 e15 e16 "" '(0 0) pfrontbottommoveout)
    (command "move" e13b e14b e15b e16b "" '(0 0) pfrontbottommoveout2)
    
    (command "move" e14 e15 e16 "" '(0 0) pfrontarrowout)
    (command "move" e14b e15b e16b "" '(0 0) pfrontarrowout2)
    (command "ucs" "x" -90)
  );end-while
)


;kill==============================================================================
(defun c:killpeople( / dth delta)
    (setq head '(-800 100 0)
	  body-top '(-800 -68)
	  body-mid '(-800 -168)
	  body-bot '(-800 -468)
	  cbody-mid '(0 0)
	  cright-hand '(-100 -100)
	  cleft-hand '(100 -100)
	  cbody-bot '(0 -300)
	  cright-leg '(-100 -400)
	  cleft-leg '(100 -400)
	  right-eye '(20 0)
	  right-eye1 '(65 45)
	  right-eye2 '(110 0)
	  left-eye '(-20 0)
	  left-eye1 '(-65 45)
	  left-eye2 '(-110 0)
	  mouse '(0 -120)
	  right-mouse '(65 -90)
	  left-mouse '(-65 -90)
	  line1s '(-10.808 86.8416)
	  line1e '(-203.0344 86.8416)
	  line2s '(-31.1956 78.1088)
	  line2e '(-224.8783 78.1088)
	  line3s '(-10.808 69.3761)
	  line3e '(-203.0344 69.3761)
	  )
    (command "sphere" head 168)(setq b1 (entlast))
    (command "line" line1s line1e "")(setq b11 (entlast))
    (command "line" line2s line2e "")(setq b12 (entlast))
    (command "line" line3s line3e "")(setq b10 (entlast))
    (command "line" body-top body-bot "")(setq b2 (entlast))
    (command "ucs" '(-800 -168 0) "")
    (command "ucs" "y" 90)
    (command "line" cbody-mid cright-hand "")(setq b3 (entlast))
    (command "line" cbody-mid cleft-hand "")(setq b4 (entlast))
    (command "line" cbody-bot cright-leg "")(setq b5 (entlast))
    (command "line" cbody-bot cleft-leg "")(setq b6 (entlast))
    
    (command "ucs" "y" -90)
    (command "ucs" '(800 168 0) "")
    (command "ucs" '(-632 150 0) "")
    (command "ucs" "y" -90)
    (command "pline" right-eye right-eye1 right-eye2 "")(setq b7 (entlast))
    (command "pline" left-eye left-eye1 left-eye2 "")(setq b8 (entlast))
    (command "pline" right-mouse mouse left-mouse "")(setq b9 (entlast))
    (command "ucs" "y" 90)
    (command "ucs" '(632 -150 0) "")

    (princ "\nStarting shotting ")
    (setq counter 0)
    (while (and (setq key (cadr (grread))) (/= 13 key) )
    (if (= key 8)  ;<back space> is pressed
        (progn
	  (command "move" b10 b11 b12 "" '(0 0) '(-50 0))
	  
	  (if (> counter 8)
      	  (progn
	    (command "move" b1 b7 b8 b9 "" '(0 0) '(-50 50))
	    (command "move" b2 "" '(0 0) '(-50 -50))
	    (command "ucs" "y" -90)
	    (command "move" b4 "" '(0 0) '(-10 10))
	    (command "move" b3 "" '(0 0) '(10 10))
	    (command "move" b5 "" '(0 0) '(10 -10))
	    (command "move" b6 "" '(0 0) '(-10 -10))
	    (command "rotate" b2 "" '(0 0) 30)
	    (command "rotate" b1 b7 b8 b9 "" '(0 0) -60)
	    (command "move" b2 "" '(0 0) '(-10 10))
	    (command "ucs" "y" 90)
	   )
          )
	  (setq counter (+ counter 1))
	 )
    )
    )
)