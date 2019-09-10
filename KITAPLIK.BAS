' ------------------------------------------
' - www.bilgin.ofisi.com		   -
' - www.geocities.com/elmek6               -
' - elmek6@yahoo.com                       -
' ------------------------------------------
' - written by ôvÅná METE             1997 -
' ------------------------------------------


'Programç QB /L TURK üeklinde yÅklemelisiniz...
'Programç derlemek iáin ise
'BC KiTAPLIK.BAS/E/X/O/T/C:512;
'LINK
'sorulan sorulara
'Object Modules [.obj]: /ex /noe /nod:brun45.lib KiTAPLIK.BAS (yalnçzca dosya ismide yetiyor)
'Libraries [.LIB]: bcom45+
'Libraries [.LIB]: Turk+
'Libraries [.LIB]: Qb
'Program qbasic'de áalçüacaksa tançtçm... kçsmçnç kullanamazsçnçz
'programç derlerken komut satçrçnda yukarda belirtildißi gibi iüaretleyiniz
'Programç komut satçrçnda derlemeden înce RENKAYAR prosedÅrÅndeki rem satçrçnç
'kaldçrçnçz...

'ProsedÅrler
 DECLARE SUB Virus ()                          'Dosyada virÅs ara
 DECLARE SUB PPrint (i)                        'Yan-Sîn-Bit òülemi
 DECLARE SUB Waits (Time)                      'Bekletme komutu
 DECLARE SUB EPrint (c%, a)                    'Program kçsaltmasç
 DECLARE SUB TurkFont (BYVAL Kar%)             'TÅrkáe karekterler
 DECLARE SUB OFont (Text$, x, y, Fore)         'Grafik ekrançnda yazç kullanma
 DECLARE SUB EWrite (w%, q%, i$, Time)         'Sesli yazma
 DECLARE SUB EkranaDegiskenYaz (c!, i$)        'Program kçsalmasç
 DECLARE SUB Cerceve (MenuDikey, MenuYatay, MenuYuksek, MenuGenis, i$)

'Fonksiyonlar
 DECLARE FUNCTION Klavye$ ()                   'Klavye kçsaltma deßeri
 DECLARE FUNCTION Orijinal ()                  'Program orijinalmi
 DECLARE FUNCTION EgaVgaMi% ()                 'Ekrançn tipi ne ?
 DECLARE FUNCTION BiosDate$ ()                 'Bios tarihi
 DECLARE FUNCTION BlockList (b)                'Blok listeleme
 DECLARE FUNCTION Decode$ (i$, y!)             'String áevirme
 DECLARE FUNCTION MsgBox! (x, y, z)            'Mesaj yorumlama komutu
 DECLARE FUNCTION EInput$ (a, b, c, d$, E)     'ôzel input komutu
 DECLARE FUNCTION DateValid% (Month%, Day%, Year%)   'Tarih geáerlimi True=1

'Alt yordamlarda paylaüçlacak deßerler
 COMMON SHARED q%, q$

'Sabitler
 CONST Renk1% = 15, Renk2% = 1
 CONST Renk3% = 14, Renk4% = 0
 CONST Renk5% = 11, Renk6% = 15

 CONST File1$ = "Kayitlar.DAT"
 CONST File2$ = "Kitaplar.DAT"
 CONST File3$ = "Kitaplik.Exe"

 CONST True! = -1
 CONST False = 0
 CONST TAndF = 1

 Ben1$ = "ô" + CHR$(118) + "Å" + CHR$(110) + "á" + " M" + CHR$(69) + "T" + CHR$(69)
 Ben2$ = CHR$(71) + CHR$(105) + CHR$(122) + CHR$(101) + CHR$(109)
 Ben3$ = "Ä" + CHR$(105) + "ß" + CHR$(100) + CHR$(101) + "m B" + CHR$(65) + "û" + CHR$(84) + CHR$(85) + "¶"


'Hata yorumlama...
 ON ERROR GOTO HATA


'Yapçlandçrma...
 SCREEN 9
 SCREEN 0
 LOCATE , , 0, 4


'TÅrkáe karekterleri yÅkle
 Kar% = EgaVgaMi%
 IF Kar% <> 0 THEN TurkFont (Kar%)
 LOCATE 13, 13

'Video adaptîrÅ tipini yorumla...!
 SELECT CASE Kar%
	CASE 16: PRINT "VGA ekran kartç bulundu ve TÅrkáe yazçlçm yÅkleniyor..."
	CASE 14: PRINT "EGA ekran kartç bulundu ve TÅrkáe yazçlçm yÅkleniyor..."
	CASE 0!: PRINT "Bu programin calisabilmesi icin en az bir Ega ekran kartina"
		 PRINT "sahip olmalisiniz...":  END
 END SELECT
 Waits .4  'Please wait...


'Atamalar
 Alfa = 1
 Gama = 1
 Ex% = False

'Etiketler
'Replay [0..8]
'Repeat [01..16]
'PlayBack [01..05]

IF False = Orijinal THEN
 COLOR Renk1%, Renk2%:   CLS
 Cerceve 2!, 2, 15, 74, "[ Korsan Programlar ]"
  LOCATE 3!, 4: PRINT "  Bu program kopya, kopya programlar yeni áçkan yasaya gîre suá iügal"
  LOCATE 4!, 4: PRINT "ediyor. Sizde yasal suá iülememek iáin bu programç derhal silin ya da"
  LOCATE 5!, 4: PRINT "bu programç bize ulaütçrçn, karüçlçßçnda bu programçn yasal kullançcçsç"
  LOCATE 6!, 4: PRINT "olun"
 'LOCATE 7!, 4: PRINT
  LOCATE 8!, 4: PRINT "  Program kÅtÅphane girdilerini kontrol etmek iáin yazçlmçütçr"
 'LOCATE 9!, 4: PRINT
  LOCATE 10, 4: PRINT "Bu Programçn Yasal Kullançcçsç Olmak òáin LÅtfen Bizi Arayçn"
  LOCATE 11, 4: PRINT "Telefonlarçmçz: (0-324) 325 92 42,   (0-324) 482 21 44"
  LOCATE 12, 4: PRINT "Adresimiz: Pirireis mah. Silifke cad. Umay apt. No: 462 Kat:3/5"
  LOCATE 13, 4: PRINT "Mersin Of TURKEY                    (C&R) 1997 " + Ben2$ + "soft Design"
  LOCATE 14, 4: PRINT "Eßer " + Ben2$ + " yazçlçm evine Åye deßilseniz, Åcretsiz broüÅr ve disketinizi"
  LOCATE 15, 4: PRINT "almayç unutmayçnçz. Her zamanki gibi mutluluk ve saßlçk sizinle olsun!"
  LOCATE 16, 4: PRINT "Devam edebilmek iáin herhangi bir tuüa basçnçz"
  LOCATE 17, 1
  i$ = INPUT$(1)
  SYSTEM
END IF
b = Orijinal
IF b = False THEN PRINT "Yemez!!!": END
 Virus


'The paramater on the program
 IF COMMAND$ <> "" THEN
    COLOR Renk1%, Renk2%: CLS
    Cerceve 2!, 2, 15, 74, "[ Kitaplçk ]"
     LOCATE 3!, 4: PRINT "  Bu program ufak kÅtÅphaneler iáin ideal bir dosya tabanç sistemi oluüturur"
     LOCATE 4!, 4: PRINT "  Kullançmç áok kolay (Easy-User). Ve bu program patentli..."
    i$ = INPUT$(1)
    END
 END IF

'˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛˛
'Starting

 OPEN "R", #1, File1$, 68
FIELD #1, 2 AS D1$, 2 AS D2$, 4 AS D3$, 23 AS D4$, 37 AS D5$

 OPEN "R", #2, File2$, 60
FIELD #2, 23 AS D6$, 37 AS D7$


Replay0:
'MenÅler diziliyor
 Menu$(1) = "  Dosya  "
 Menu$(2) = "  Kayçt  "
 Menu$(3) = "  Arüiv  "
 Menu$(4) = "  Yardçm  "

'Ekran ilk rengini alçyor
 COLOR Renk1%, Renk2%
 CLS

'òlk menÅ aáçlçyor
 COLOR , Renk6%
 LOCATE 1&, 1: PRINT "  Dosya    Kayçt    Arüiv    Yardçm                                             "
 LOCATE 25, 1: PRINT " Written By " + Ben1$ + " & By " + Ben2$ + " software designed...      Boü bellek :"; FRE(-1);
 GOSUB Replay2
 IF Orp% = True THEN Orp% = False: RETURN

Replay1: 'pull down menÅnÅn Åst kçsmç

 'Klavye deßeri okunuyor
  SELECT CASE Klavye
	 CASE ">" 'bar saßa gidiyor
		   Alfa = Alfa + 1
		   IF Alfa > 4 THEN Alfa = 1
		   GOSUB Replay2
	 CASE "<" 'bar sola gidiyor
		   Alfa = Alfa - 1
		   IF Alfa < 1 THEN Alfa = 4
		   GOSUB Replay2
	 CASE "*", "+", "-": GOTO Replay3
	 CASE "|"
	      COLOR Renk1%, Renk4%
	      CLS
	      q% = 1
	      EWrite 1, 1, "Kitaplçk programç versiyon 2.0", .04
	      EWrite 2, 1, "Derleyen " + Ben2$ + " Yazçlçm & Opr. " + Ben1$, .03
	      EWrite 3, 1, "Oparation System Activated...", .05
	      LOCATE 5
	      END
  END SELECT
GOTO Replay1
'replay 1 end


Replay2:
  LOCATE 1, 9 * Gama - 8: PRINT Menu$(Gama)
  Gama = Alfa
  COLOR Renk3%, Renk4%
  LOCATE 1, 9 * Alfa - 8: PRINT Menu$(Alfa)
  COLOR Renk1%, Renk6%
RETURN


Replay3:
  SELECT CASE Alfa
	 CASE 1
	      Cerceve 2, 1, 4, 28, ""
	      Menu$(1) = " 1. Bu programdan áçkçü     "
	      Menu$(2) = " 2. Yeni kayçt kÅtÅßÅ yapma "
	      Menu$(3) = " 3. Yeni arüiv kÅtÅßÅ yapma "
	      Teta = 3:       Beta = 2
	 CASE 2
	      Cerceve 2, 10, 4, 24, ""
	      Menu$(1) = " 1. Kayçtlara gîz atma  "
	      Menu$(2) = " 2. Yeni kayçt ekleme   "
	      Menu$(3) = " 3. Teslimatlar         "
	      Teta = 3:       Beta = 11
	 CASE 3
	      Cerceve 2, 19, 4, 31, ""
	      Menu$(1) = " 1. Arüive gîz atma            "
	      Menu$(2) = " 2. Arüive yeni kayçt ekleme   "
	      Menu$(3) = " 3. Arüivin áçkdçsçnç alma     "
	      Teta = 3:       Beta = 20
	 CASE 4
	      Cerceve 2, 28, 6, 26, ""
	      Menu$(1) = " 1. Kullançm klavuzu      "
	      Menu$(2) = " 2. Programçn îzellikleri "
	      Menu$(3) = " 3. Program hakkçnda      "
	      Menu$(4) = " 4. Programçn yazarç      "
	      Menu$(5) = " 5. Tançtçm...            "
	      Teta = 5:       Beta = 29
  END SELECT

  FOR K% = 1 TO Teta
      LOCATE 2 + K%, Beta
      PRINT Menu$(K%)
  NEXT

  Menu = Alfa
  Alfa = 1
  Gama = 1
  GOSUB Replay5

Replay4:
  SELECT CASE INKEY$
	 CASE CHR$(0) + CHR$(77) 'Saßa ilerleniyor
		   Alfa = Menu + 1
		   IF Alfa > 4 THEN Alfa = 1
		   Gama = Menu
		   Orp% = True
		   GOSUB Replay0
		   GOSUB Replay3
		   GOTO Replay0
	 CASE CHR$(0) + CHR$(75) 'Sola ilerleniyor
		   Alfa = Menu - 1
		   IF Alfa < 1 THEN Alfa = 4
		   Gama = Menu
		   Orp% = True
		   GOSUB Replay0
		   GOSUB Replay3
		   GOTO Replay0
	 CASE CHR$(0) + CHR$(72) 'Yukarç gidiliyor
		   Alfa = Alfa - 1
		   IF Alfa < 1 THEN Alfa = Teta
		   GOSUB Replay5
	 CASE CHR$(0) + CHR$(80) 'Aüüaßç iniliyor
		   Alfa = Alfa + 1
		   IF Alfa > Teta THEN Alfa = 1
		   GOSUB Replay5
	 CASE CHR$(13): GOTO Replay6
	 CASE CHR$(27): Alfa = Menu: GOTO Replay0
	 CASE "1": IF Teta > 1 THEN Alfa = 1: GOTO Replay6'Gereksiz ama
	 CASE "2": IF Teta > 2 THEN Alfa = 2: GOTO Replay6' gîze hoü
	 CASE "3": IF Teta > 3 THEN Alfa = 3: GOTO Replay6' geliyor
	 CASE "4": IF Teta > 4 THEN Alfa = 4: GOTO Replay6
	 CASE "5": IF Teta > 5 THEN Alfa = 5: GOTO Replay6
	 CASE ELSE: GOTO Replay4
  END SELECT
GOTO Replay4


Replay5:
  LOCATE 2 + Gama, Beta: PRINT Menu$(Gama)
  Gama = Alfa
  COLOR Renk3%, Renk2%
  LOCATE 2 + Alfa, Beta: PRINT Menu$(Alfa)
  COLOR Renk1%, Renk6%
RETURN

Replay6:
  SELECT CASE Menu
	 CASE 1'dosya
	      SELECT CASE Alfa
		     CASE 1 'áçkçü
Repeat08:                 COLOR Renk1%, Renk2%
			  CLS
			  Cerceve 2, 2, 3, 74, "[Kitaplçk programç versiyon 2.0]"
			   LOCATE 3, 4: PRINT "Bu programçn patent hakkç sayçn " + Ben3$ + "'a aittir"
			   LOCATE 4, 4: PRINT "Written By " + Ben2$ + "soft design & By " + Ben1$
			  Cerceve 10, 29, 2, 20, ""
			   COLOR 30, 8: EWrite 10, 31, "Gizem Soft Design!", .1
			  LOCATE 20, 1
			   COLOR 15, 1
			    END
		     CASE 2
			  COLOR Renk1%, Renk2% 'yeni kayçt dosyasç
			  CLS
			  Cerceve 2, 2, 4, 74, "[ Yeni bir kayçt kÅtÅßÅ oluüturmak Åzerisiniz ]"
			   LOCATE 3, 4: PRINT "Yeni bir kayçt kÅtÅßÅ aáarak eski kayçt kÅtÅßÅnÅ ve iáindeki bÅtÅn"
			   LOCATE 4, 4: PRINT "bilgileri kaybedeceksiniz, yinede devam etmek istiyormusunuz ?"
			   w = MsgBox(5, 16, -2)
			   IF w = 1 THEN
			      CLOSE #1:     KILL File1$
			      COLOR Renk1%, Renk2%: CLS
			      Cerceve 2, 2, 4, 74, "[ Yeni bir kayçt kÅtÅßÅ oluüturuldu ]"
			       LOCATE 3, 4: PRINT "Yeni kayçt kÅtÅßÅ aáçldç."
			       LOCATE 4, 4: PRINT "Eski kayçt kÅtÅßÅndeki bÅtÅn bilgiler silindi"
			       LOCATE 5, 4: PRINT "Program üimdi tekrar aáçlacak. Devam etmek iáin bir tuüa basçnçz"
			       i$ = INPUT$(1)
			       RUN
			   END IF
		     CASE 3
			  COLOR Renk1%, Renk2% 'yeni arüiv dosyasç
			  CLS
			  Cerceve 2, 2, 4, 74, "[ Yeni bir arüiv dosyasç oluüturmak Åzerisiniz ]"
			   LOCATE 3, 4: PRINT "Yeni bir arüiv dosyasç aáarak eski arüiv dosyasçnç ve iáindeki bÅtÅn"
			   LOCATE 4, 4: PRINT "bilgileri kaybedeceksiniz, yinede devam etmek istiyormusunuz ?"
			   w = MsgBox(5, 16, -2)
			   IF w = 1 THEN
			      CLOSE #2:     KILL File2$
			      COLOR Renk1%, Renk2%: CLS
			      Cerceve 2, 2, 4, 74, "[ Yeni bir arüiv dosyasç kÅtÅßÅ oluüturuldu ]"
			       LOCATE 3, 4: PRINT "Yeni arüiv dosyasç aáçldç."
			       LOCATE 4, 4: PRINT "Eski arüiv dosyasçndaki bÅtÅn bilgiler silindi"
			       LOCATE 5, 4: PRINT "Program üimdi tekrar aáçlacak. Devam etmek iáin bir tuüa basçnçz"
			       i$ = INPUT$(1)
			       RUN
			   END IF
	      END SELECT
	 CASE 2'kayçt
	      SELECT CASE Alfa
		     CASE 1  'Kayçtlara gîz atma +++
Repeat05: 'hçzlç baülatma
			  COLOR Renk1%, Renk2%
			  CLS
			  Cerceve 2!, 2, 7, 74, "[ Kayçt Listelemek ]"
			   LOCATE 3!, 4: PRINT "Barç Åzerinde iülem yapçlacak kaydçn ÅstÅne getirip aüüaßçdaki tuülardan"
			   LOCATE 4!, 4: PRINT "birisine basçn. (Barç hareket ettirmek iáin yîn tuülarçnç kullançn)"
			   LOCATE 5!, 4: PRINT "DELete   :   Aktif kaydç siler"
			   LOCATE 6!, 4: PRINT "SPACE bar:   Aktif kayçdda deßiüiklik yapar"
			   LOCATE 7!, 4: PRINT "INSert   :   òstenilen kaydç arar, bulursa aktif yapar"
			   LOCATE 8!, 4: PRINT "ESCape   :   Ana menÅ"
			  Cerceve 21, 2, 2, 74, "[ Aktif kayçdçn kayçt numarasç ]"
			   LOCATE 22, 4: PRINT "Kayçt no: 1"
			  a = 1: b = 0
			  Cerceve 12, 2, 6, 74, "[ Kayçt Listesi ]"
			   IF Ez% > 0 THEN q% = Ez%: Ez% = 0: a = q% ELSE q% = 1
			   GOSUB BlockList
Repeat04:                 i$ = INKEY$: IF i$ = "" THEN GOTO Repeat04
			 SELECT CASE i$
				CASE CHR$(0) + CHR$(77), CHR$(0) + CHR$(80): a = a + 1
				CASE CHR$(0) + CHR$(72), CHR$(0) + CHR$(75): a = a - 1
				CASE CHR$(0) + CHR$(82)'insert
				     Cerceve 21, 2, 2, 74, "[ Kayçt aratma ]"
				     Ex% = TAndF
				     GOSUB Repeat07
				CASE CHR$(32)'space bar
				     Ex% = True
				     Ez% = q%
				     GOSUB Repeat07
				     Cerceve 21, 2, 2, 74, "[ Aktif kayçdçn kayçt numarasç ]"
				      LOCATE 22, 4: PRINT "Kayçt no:"
				      GOTO Repeat05
				CASE CHR$(0) + CHR$(83)'delete
				     Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdç silmek Åzerisiniz ]"
				      LOCATE 22, 4: PRINT q%; "Nolu kayçt silinecek..."
				     SELECT CASE MsgBox(22, 27, -2)
					    CASE 0: Cerceve 21, 2, 2, 74, "[ Aktif kayçdçn kayçt numarasç ]"
						    LOCATE 22, 4: PRINT "Kayçt no:"
					    CASE 1
						 b = LOF(1) \ 68
						 OPEN "TEMP!!!.$$$" FOR BINARY AS #3
						      FOR a = 1 TO b
							  GET 1, a
							  IF a = q% THEN GOTO Repeat06
							  i$ = D1$ + D2$ + D3$ + D4$ + D5$
							  PUT 3, LOF(3) + 1, i$
							  LOCATE 22, 4: PRINT "Tampon boüaltma iülemi yapçlçyor. LÅtfen bekleyiniz >"; a
Repeat06:                                             NEXT
						      LOCATE 22, 4: PRINT "Silme iülemi tamamlandç. Dosya boüluksuz olarak aktarçldç"
						      w% = MsgBox(22, 68, -1)
						 CLOSE #1
						 CLOSE #3
						 KILL File1$
						 NAME "TEMP!!!.$$$" AS File1$
						 OPEN "R", #1, File1$, 68
						 FIELD #1, 2 AS D1$, 2 AS D2$, 4 AS D3$, 23 AS D4$, 37 AS D5$
						 GOTO Repeat05
					    CASE 2: Cerceve 21, 2, 2, 74, "[ Aktif kayçdçn kayçt numarasç ]"
						    LOCATE 22, 4: PRINT "Kayçt no:"
				     END SELECT
				CASE CHR$(13): 'enter
				CASE CHR$(27): GOTO Replay7: 'escape
				CASE ELSE: SOUND 800, .2: GOTO Repeat04
			 END SELECT

			  IF a < 1 THEN a = 1: SOUND 600, .5
			  q% = a
			  GOSUB BlockList
			  IF w% = -1 THEN a = a - 1: q% = q% - 1
			     LOCATE 22, 13: PRINT q%
			  GOTO Repeat04
		     CASE 2' Yeni kayçt ---
Repeat07:                'ikinci rename iülemi
			  COLOR Renk1%, Renk2%: CLS
			  S1 = 4
			  IF Ex% = True THEN
			     Cerceve 2, 2, 7, 74, "[ Kayçtda Deßiüiklik ]"
			      LOCATE 3, 4: PRINT " Bu kçsçm ile aktif kaydçn Åzerinde deßiüiklik yapabilirsiniz."
			      LOCATE 4, 4: PRINT " ( Deßiüiklik yapçlacak kaydçn numarasç"; Ez%; ")"
			      LOCATE 5, 4: PRINT " Deßiüen kayçt Kayçt Listesi menÅsÅnde aktif kayçt olacaktçr."
			     'LOCATE 6, 4: PRINT
			      LOCATE 7, 4: PRINT "Bloklar arasç iülemlerde bu tuülarç kullanabilirsiniz"
			      LOCATE 8, 4: PRINT "Key: Insert, Delete, Back Space, Yîn tuülarç, Enter, Tab, Shift+Tab"
			  ELSEIF Ex% = TAndF THEN
			     Cerceve 2, 2, 7, 74, "[ Kayçt Arama ]"
			      LOCATE 3, 4: PRINT " Bu kçsçmla înceden var olan kayçtlardan bir tanesini áok hçzlç"
			      LOCATE 4, 4: PRINT "olarak arayabilirsiniz. Bulunan kayçt Kayçt Listeleme menÅsÅnde"
			      LOCATE 5, 4: PRINT "aktif kayçt olarak gîzÅkecektir."
			     'LOCATE 6, 4: PRINT
			      LOCATE 7, 4: PRINT "Bilmedißiniz yerleri boü bçrakçnçz. Bloklar arasç iülemleri iáin"
			      LOCATE 8, 4: PRINT "Key: Insert, Delete, Back Space, Yîn tuülarç, Enter, Tab, Shift+Tab"
			      S1 = 5
			  ELSEIF Ex% = False THEN
			     Cerceve 2, 2, 7, 74, "[ Yeni kayçt ]"
			      LOCATE 3, 4: PRINT " Bu kçsçm ile yeni kayçt girebilirsiniz. ûimdi gireceßiniz kayçt bilgi"
			      LOCATE 4, 4: PRINT "kitaplçk dosyasçnçn sonuna eklenecek."
			      LOCATE 5, 4: PRINT "( Yeni kayçt numarasç:"; LOF(1) \ 68 + 1; ")"
			     'LOCATE 6, 4: PRINT
			      LOCATE 7, 4: PRINT "Bloklar arasç iülemlerde bu tuülarç kullanabilirsiniz"
			      LOCATE 8, 4: PRINT "Key: Insert, Delete, Back Space, Yîn tuülarç, Enter, Tab, Shift+Tab, ESC"
			  END IF
			  Cerceve 12, 2, 6, 74, "[ Kayçt Bilgileri ]"
			  'LOCATE 13, 4: PRINT
			   LOCATE 14, 4: PRINT "Kitabçn alçnçü tarihi : [  /  /    ]"
			   LOCATE 15, 4: PRINT "Kitabç alan kiüinin...: [                       ]"
			   LOCATE 16, 4: PRINT "Kitabçn ismi, yazarç  : [                                     ]"
			  'LOCATE 17, 4: PRINT
			   IF Ex% = True THEN
			      GET 1, Ez%
			      E1$ = D1$
			      E2$ = D2$
			      E3$ = D3$
			      E4$ = D4$
			      E5$ = D5$
			      LOCATE 15, 29: PRINT E4$
			      LOCATE 16, 29: PRINT E5$
			   ELSE
			      E1$ = MID$(DATE$, 4, 2)'gÅn
			      E2$ = MID$(DATE$, 1, 2)'ay
			      E3$ = MID$(DATE$, 7, 4)'yçl
			      E4$ = "":    E5$ = ""
			   END IF
			   LOCATE 14, 29: PRINT E1$ + "/" + E2$ + "/" + E3$
PlayBack01:                    E1$ = EInput(14, 29, 2!, E1$, S1): IF q% = True THEN SOUND 800, .3: GOTO PlayBack01
PlayBack02:                    E2$ = EInput(14, 32, 2!, E2$, S1): IF q% = True THEN GOTO PlayBack01
PlayBack03:                    E3$ = EInput(14, 35, 4!, E3$, S1): IF q% = True THEN GOTO PlayBack02
PlayBack04:                    E4$ = EInput(15, 29, 23, E4$, 1): IF q% = True THEN GOTO PlayBack03
			       E5$ = EInput(16, 29, 37, E5$, 1): IF q% = True THEN GOTO PlayBack04
			    IF DateValid(VAL(E2$), VAL(E1$), VAL(E3$)) = 0 THEN
			       Cerceve 21, 2, 2, 74, "[ Girdißiniz tarih geáerli deßil ]"
			       LOCATE 22, 5: PRINT "Tarihi dikkatli ve [gÅn-ay-yçl] sistemine gîre girin!"
			       i = MsgBox(22, 68, -1)
			       COLOR , 1
			       LOCATE 21, 1
			       PRINT STRING$(320, 32);
			       GOTO PlayBack01
			    END IF
			  Cerceve 21, 2, 2, 74, "[ Bu bilgileri onaylçyormusunuz ? ]"
			   w = MsgBox(22, 11, -3)
			  SELECT CASE w
				 CASE 1
					 IF Ex% = True THEN
					    q% = LOF(1) \ 68 + 1
					    LSET D1$ = E1$
					    LSET D2$ = E2$
					    LSET D3$ = E3$
					    LSET D4$ = E4$
					    LSET D5$ = E5$
					    Cerceve 21, 2, 2, 74, "[ Bilgiler doßru bir biáimde dosyaya yerleütirildi ]"
					     LOCATE 22, 10: PRINT Ez%; "Nolu kayçt doßru bir üekilde kaydedildi"
					    PUT 1, Ez%
					    w = MsgBox(22, 68, -1)
					    Ex% = False
					    RETURN
					 ELSEIF Ex% = TAndF THEN
						COLOR Renk1%, Renk2%: CLS
						Cerceve 2, 2, 3, 74, "[ BÅyÅk/KÅáÅk Harf Duyarlçlçßç ]"
						 LOCATE 3, 4: PRINT "  Girdißiniz bilgiler arançrken bÅyÅk/kÅáÅk harf uyumuna dikkat"
						 LOCATE 4, 4: PRINT "edilmiyecek ?"
						 S1 = MsgBox(4, 27, -2)
						Cerceve 2, 2, 5, 74, "[ Arama Hassasiyeti ]"
						 LOCATE 3, 4: PRINT "  Girdißiniz bilgiler arançrken kelime olarak aranmayacak. Bunu"
						 LOCATE 4, 4: PRINT "onaylçyormusunuz ?"
						 LOCATE 5, 4: PRINT "(Eßer bu seáeneßi onaylamazsançz arama iüleminde bahsedilen {Bilmedißiniz}"
						 LOCATE 6, 4: PRINT "yerleri boü bçrakabilirsiniz} satçrç yÅrÅrlÅßÅnÅ kaybedecektir)"
						 s2 = MsgBox(4, 27, -2)
						COLOR Renk1%, Renk2%: CLS
						Cerceve 2, 2, 7, 74, "[ Arama òülemi Baüladç ]"
						 LOCATE 3, 4: PRINT "  Arama iülemi sçrasçnda her 5 grupta bir ekran durup sizden"
						 LOCATE 4, 4: PRINT "bir iülem beklemektedir. Bu iülemler; ESCape tuüu: Dißer gruba"
						 LOCATE 5, 4: PRINT "geüiái saßlar, ENTER tuüu: östÅnde bulundußunuz kayçdç kayçt listeleme"
						 LOCATE 6, 4: PRINT "bîlÅmÅnde listeler."
						 LOCATE 7, 4: PRINT "  ûu anda aranan kayçdçn numarasç: ?/?"
						 LOCATE 8, 4: PRINT "Key: Up, Down, Left, Right, Esc, Enter"
						Cerceve 12, 2, 6, 74, "[ Eüleüenlerin Listesi ]"
					       'Seáme metodu uygulançyor
						IF s2 = 1 THEN
						   E1$ = LTRIM$(RTRIM$(E1$))
						   E2$ = LTRIM$(RTRIM$(E2$))
						   E3$ = LTRIM$(RTRIM$(E3$))
						   E4$ = LTRIM$(RTRIM$(E4$))
						   E5$ = LTRIM$(RTRIM$(E5$))
						ELSE
						   Exc$ = E1$ + E2$ + E3$ + E4$ + E5$
						END IF

					       'Seáme metodu uygulançyor
						IF S1 = 1 THEN
						   IF s2 = 1 THEN E4$ = UCASE$(E4$): E5$ = UCASE$(E5$) ELSE Exc$ = UCASE$(Exc$)
						END IF

						z = LOF(1) \ 68
						a = 0
						FOR x = 1 TO z
						    LOCATE 4, 52: PRINT x
						    GET 1, x
						    IF (s2 = 2) AND ((S1 = 1 AND UCASE$(D1$ + D2$ + D3$ + D4$ + D5$) = Exc$) OR (S1 = 2 AND D1$ + D2$ + D3$ + D4$ + D5$ = Exc$)) THEN
						       a = a + 1
						       Menu$(a) = D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$
						       LOCATE 12 + a, 4: PRINT Menu$(a)
						    END IF
						    IF s2 = 1 AND INSTR(UCASE$(D1$), E1$) > 0 AND INSTR(UCASE$(D2$), E2$) > 0 AND INSTR(UCASE$(D3$), E3$) > 0 AND INSTR(UCASE$(D4$), E4$) > 0 AND INSTR(UCASE$(D5$), E5$) > 0 THEN
						       a = a + 1
						       Menu$(a) = D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$
						       LOCATE 12 + a, 4: PRINT Menu$(a)
						    END IF
						    IF a = 5 THEN
						       a = 0
						       c = 1: b = 1
						       'LOCATE 12, 7: PRINT "Bu sayfa doldu, sayfadaki kayçtlar arasç hareket edebilirsiniz ]"
Repeat16:                                              COLOR , 7: EkranaDegiskenYaz b, Menu$(b)
						       COLOR , 1: EkranaDegiskenYaz c, Menu$(c)
						       b = c
						       SELECT CASE Klavye$
							      CASE "+", "<": c = c - 1: IF c < 1 THEN c = 5: GOTO Repeat16
							      CASE "-", ">": c = c + 1: IF c > 5 THEN c = 1: GOTO Repeat16
							      CASE "*":
							      CASE "|": GOTO Repeat16
						       END SELECT
						       
						       COLOR 15, 7
						    END IF
						NEXT

					 ELSEIF Ex% = False THEN
						q% = LOF(1) \ 68 + 1
						LSET D1$ = E1$
						LSET D2$ = E2$
						LSET D3$ = E3$
						LSET D4$ = E4$
						LSET D5$ = E5$
						Cerceve 21, 2, 2, 74, "[ Bilgiler doßru bir biáimde dosyaya yerleütirildi ]"
						 PUT 1, q%
						 LOCATE 22, 10: PRINT "Yeni kayçdçn numarasç:"; q%
						w = MsgBox(22, 68, -1)
						GOTO Repeat07 'istek Åzerine sekme yapçlçyor
					 END IF
				 CASE 2:  COLOR , 1
					 LOCATE 21, 1
					  PRINT STRING$(320, 32);
					 GOTO PlayBack01
				 CASE 0, 3: GOTO Replay7
			  END SELECT
		     CASE 3'teslimatlar
			  COLOR Renk1%, Renk2%: CLS
			  Cerceve 2, 2, 4, 74, "[ Teslimatlar ]"
			   LOCATE 3, 4: PRINT "Bu kçsçmda seri olarak teslimat tarihi dolan kayçtlar listelenir."
			   LOCATE 3, 4: PRINT "Aüüaßçdaki gîrdÅßÅnÅz yerde gÅnÅ dolan kayçtlar yer alçyor burayç"
			   LOCATE 4, 4: PRINT "hareket ettirmek iáin enter tuüuna basçnçz.  òülem sçrasç:    1"
			  Cerceve 9, 2, 6, 74, "[ Teslimatlarçn Listesi ]"
			   c = 0
			   b = LOF(1) \ 68
			   IF b = 0 THEN
			      LOCATE 12, 4: PRINT "Bu kçsçmçn áalçüabilmesi iáin en az bir tane kayçt olmasç gerekir."
			      F = MsgBox(12, 69, -1)
			      GOTO Replay7
			   END IF
			   E1$ = MID$(DATE$, 4, 2)'gÅn
			   E2$ = MID$(DATE$, 1, 2)'ay
			   E3$ = MID$(DATE$, 7, 4)'yçl
			   FOR a = 1 TO b
			       LOCATE 4, 65: PRINT a
			       GET 1, a
			       GOSUB BlockCompare
			       IF d = 0 THEN
				  c = c + 1
				  LOCATE 9 + c, 4: PRINT D4$ + " - " + D5$ + " ˛bugÅn"
			       END IF
			       IF d > 0 THEN
				  c = c + 1
				  LOCATE 9 + c, 4: PRINT D4$ + " - " + D5$ + " ˛"; d; "gÅn"
			       END IF
			       IF c > 4 THEN
				  c = 0
				  LOCATE 9, 5: PRINT "[ Dißer sayfaya geámek iáin enter         ]"
				  F = MsgBox(9, 39, -1)
				  Cerceve 9, 2, 6, 74, "[ Teslimatlarçn Listesi ]"
			       END IF
			   NEXT
			   LOCATE 9, 5: PRINT "[ Baüka teslimat tarihi dolan kayçt bulunamadç          ]"
			   F = MsgBox(9, 53, -1)
	      END SELECT
	 CASE 3'arüiv
	      SELECT CASE Alfa
		     CASE 1'arüive gîz atma
Repeat10:                'hçzlç baülatma
			  COLOR Renk1%, Renk2%
			  CLS
			  Cerceve 2!, 2, 7, 74, "[ Arüiv Listelemek ]"
			   LOCATE 3!, 4: PRINT "östÅnde iülem yapacaßçnçz kayçdçn ÅstÅne barç getirip seáiniz"
			   LOCATE 4!, 4: PRINT "DELete   :   östÅnde bulundußunuz kayçdç siler"
			   LOCATE 5!, 4: PRINT "SPACE bar:   östÅnde bulundußunuz kayçtta deßiüikli yapar"
			   LOCATE 6!, 4: PRINT "INSert   :   östÅnde iülem yapçlacak kayçdç aratçr"
			   LOCATE 7!, 4: PRINT "ESCape   :   Ana menÅye dîner"
			   LOCATE 8!, 4: PRINT "Barç hareket ettirmek iáin yîn tuülarçnç kullançnçz"
			  Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdçn numarasç ve eriüimi ]"
			   LOCATE 22, 4: PRINT "Dosyançn kayçt numarasç: 1              1             60"
			  a = 1: b = 0
			  Cerceve 12, 2, 6, 74, "[ Arüiv Listesi ]"
			   IF Ez% > 0 THEN q% = Ez%: Ez% = 0: a = q% ELSE q% = 1
			   GOSUB BlockList2
Repeat12:                 i$ = INKEY$: IF i$ = "" THEN GOTO Repeat12
			 SELECT CASE i$
				CASE CHR$(0) + CHR$(77), CHR$(0) + CHR$(80): a = a + 1
				CASE CHR$(0) + CHR$(72), CHR$(0) + CHR$(75): a = a - 1
				CASE CHR$(0) + CHR$(82)'insert
				CASE CHR$(0) + CHR$(83)'delete
				     Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdç silmek Åzerisiniz ]"
				      LOCATE 22, 4: PRINT q%; "Nolu kayçt silinecek..."
				     SELECT CASE MsgBox(22, 27, -2)
					    CASE 0: Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdçn numarasç ve eriüimi ]"
						    LOCATE 22, 4: PRINT "Dosyançn kayçt numarasç:"
					    CASE 1
						 b = LOF(2) \ 60
						 OPEN "TEMP!!!.$$$" FOR BINARY AS #3
						      FOR a = 1 TO b
							  GET 2, a
							  IF a = q% THEN GOTO Repeat11
							  i$ = D6$ + D7$
							  PUT 3, LOF(3) + 1, i$
							  LOCATE 22, 4: PRINT "Tampon boüaltma iülemi yapçlçyor. LÅtfen bekleyiniz >"; b - a
Repeat11:                                             NEXT
						      LOCATE 22, 4: PRINT "Silme iülemi tamamlandç. Dosya boüluksuz olarak aktarçldç"
						      w% = MsgBox(22, 68, -1)
						 CLOSE #2
						 CLOSE #3
						 KILL File2$
						 NAME "TEMP!!!.$$$" AS File2$
						 OPEN "R", #2, File2$, 60
						 FIELD #2, 23 AS D6$, 37 AS D7$
						 GOTO Repeat10
					    CASE 2: Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdçn numarasç ve eriüimi ]"
						    LOCATE 22, 4: PRINT "Dosyançn kayçt numarasç:"
				     END SELECT
				CASE CHR$(32)'space bar
				     Ex% = True
				     Ez% = q%
				     GOSUB Repeat13
				     Ex% = False
				     Ez% = q%
				     Cerceve 21, 2, 2, 74, "[ özerinde bulundußunuz kayçdçn numarasç ve eriüimi ]"
				      LOCATE 22, 4: PRINT "Dosyançn kayçt numarasç:"
				      GOTO Repeat10
				CASE CHR$(13): 'enter
				CASE CHR$(27): GOTO Replay7: 'escape
				CASE ELSE: SOUND 800, .2: GOTO Repeat12
			 END SELECT

			  IF a < 1 THEN a = 1: SOUND 600, .5
			  q% = a
			  GOSUB BlockList2
			  IF w% = -1 THEN a = a - 1: q% = q% - 1
			     LOCATE 22, 28: PRINT q%; "   ", 60 * q% - 59, 60 * q% - 60 + 60; "   "
			  GOTO Repeat12
		     CASE 2'arüive yeni kayçt girme
Repeat13:                 COLOR Renk1%, Renk2%: CLS
			  IF Ex% = True THEN
			     Cerceve 2, 2, 7, 74, "[ Kayçt Deßiüiklißi ]"
			      LOCATE 3, 4: PRINT " Bu kçsçmla eskiden girili olan bir kayçd deßiütirilid. Seáili (eski)"
			      LOCATE 4, 4: PRINT "kayçt üimdi girdißiniz (yeni) kayçt ile deßiütirilir. Yani yeni kayçt"
			      LOCATE 5, 4: PRINT "eski kayçdçn ÅstÅne yazçlçr"
			     'LOCATE 6, 4: PRINT
			      LOCATE 7, 4: PRINT "Aüüaßçdaki anahtar tuülarç kullanabilirsiniz"
			      LOCATE 8, 4: PRINT "KEY: ESCape, DELete, BACK space, TAB, SHIFT+TAB, LEFT, RIGHT, ENTER"
			  ELSE
			     Cerceve 2, 2, 7, 74, "[ Yeni kayçt ]"
			      LOCATE 3, 4: PRINT " Yeni kayçt ekleme seáeneßini yeni bir kayçt gireceßiniz zaman"
			      LOCATE 4, 4: PRINT "kullançn. Bu seáenek ile girdißiniz îzellikleri arüivinize katmçü"
			      LOCATE 5, 4: PRINT "olurunuz, yani listelerde girdißiniz her üey gîzÅkÅr."
			     'LOCATE 6, 4: PRINT
			      LOCATE 7, 4: PRINT "Aüüaßçdaki anahtar tuülarç kullanabilirsiniz"
			      LOCATE 8, 4: PRINT "KEY: ESCape, DELete, BACK space, TAB, SHIFT+TAB, LEFT, RIGHT, ENTER"
			  END IF
			  Cerceve 12, 2, 6, 74, "[ Kayçt giriüi ]"
			   LOCATE 14, 4: PRINT "Kitabçn yazarç        : [                       ]"
			   LOCATE 15, 4: PRINT "Kitabçn ismi, konusu  : [                                     ]"
			  IF Ex% = True THEN
			     GET 2, Ez%
			     E6$ = D6$
			     E7$ = D7$
			     LOCATE 14, 29: PRINT E6$
			     LOCATE 15, 29: PRINT E7$
			  ELSE
			     E6$ = "": E7$ = ""
			  END IF
PlayBack05:                E6$ = EInput(14, 29, 23, E6$, 1)
			   E7$ = EInput(15, 29, 37, E7$, 1): IF q% = True THEN GOTO PlayBack05
			  Cerceve 21, 2, 2, 74, "[ Bu bilgilerin kayçtlara geámesini istiyormusunuz ? ]"
			   w = MsgBox(22, 11, -3)
			  SELECT CASE w
				 CASE 0: IF Ex% = True THEN RETURN
					 GOTO Replay7
				 CASE 1: LSET D6$ = E6$
					 LSET D7$ = E7$
					 Cerceve 21, 2, 2, 74, "[ Bilgiler doßru bir biáimde dosyaya yerleütirildi ]"
					 IF Ex% = True THEN
					    PUT 2, Ez%
					     LOCATE 22, 10: PRINT "Yeni kayçdçn numarasç:"; Ez%
					     w = MsgBox(22, 68, -1)
					     RETURN
					 ELSE
					    PUT 2, LOF(2) \ 60 + 1
					     LOCATE 22, 10: PRINT "Yeni kayçdçn numarasç:"; LOF(2) \ 60 + 1
					     w = MsgBox(22, 68, -1)
					 END IF
				 CASE 2:  COLOR , 1
					 LOCATE 21, 1
					  PRINT STRING$(320, 32);
					 GOTO PlayBack05
				 CASE 3: IF Ex% = True THEN RETURN
					GOTO Replay7
			  END SELECT
			  IF Ex% = True THEN RETURN
			  i$ = INPUT$(1)

		     CASE 3'arüiv áçkdçsçnç almak
			  IF LOF(2) \ 60 = 0 THEN
			       COLOR Renk1%, Renk2%: CLS
			     Cerceve 2, 2, 3, 74, "[ DîkÅm Alçnamçyor ]"
			      LOCATE 3, 4: PRINT "Bu kçsmç kullanabilmeniz iáin en az bir tane hazçr kayçt olmasç lazçm."
			      LOCATE 4, 4: PRINT "Devam edebilmek iáin herhangi bir tuüa basçnçz"
			      z = MsgBox(4, 69, -1)
			      GOTO Replay7
			  END IF

			  COLOR Renk1%, Renk2%: CLS
			    Cerceve 2, 2, 8, 74, "[ Arüivin Äçkdçsçnç Almak ]"
			     LOCATE 3, 4: PRINT "  Bu kçsçmla kitaplarçn isimlerinin ve yazarlarçnçn áçkdçsçnç yazçcçya"
			     LOCATE 4, 4: PRINT "veya bir dosyaya aktarabilirsiniz."
			     LOCATE 5, 4: PRINT "Kayçtlarç yazçcçya dîkmek isterseniz EVET, kayçtlarç bir dosyaya aktarmak"
			     LOCATE 6, 4: PRINT "iáinde HAYIR seáeneßini kullnçnçz."
			     LOCATE 7, 4: PRINT " evet : Kayçtlar yazçcçya aktarçlacak"
			     LOCATE 8, 4: PRINT " hayçr: Kayçtlar istedißiniz bir dosyaya aktarçlacak"
			     LOCATE 9, 4: PRINT " iptal: Ana menÅye dînÅü."
			    S1 = MsgBox(7, 4, 3)
			   SELECT CASE S1
				    CASE 1'yazçcçya
					 COLOR Renk1%, Renk2%: CLS
					 Cerceve 2, 2, 3, 74, "[ Kayçtlar Yazçcçya Gînderiliyor ]"
					  LOCATE 3, 4: PRINT "LÅtfen bekleyiniz. Kayçtlar seri olarak yazçcçya gînderiliyor..."
					  LOCATE 4, 4: PRINT "Bilgiler okunuyor. LÅtfen bekleyiniz...."
					 Waits .3
					 z = LOF(2) \ 60
					 LPRINT "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
					 FOR a = 1 TO z
					     GET 2, a
					     LPRINT "≥" + D5$ + "≥" + D6$ + "≥"
					 NEXT
					 LPRINT "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
					 Cerceve 2, 2, 3, 74, "[ Bilgiler Yazçcçya Aktarçldç ]"
					  LOCATE 3, 4: PRINT "Transfer sçrasçnda hiá bir sorun áçkmadç. Bilgiler iletildi"
					  LOCATE 4, 4: PRINT "Devam edebilmek iáin herhangi bir tuüa basçnçz"
					  z = MsgBox(15, 69, -1)
				    CASE 2'dosyaya
					 COLOR Renk1%, Renk2%: CLS
					 Cerceve 2, 2, 3, 74, "[ Kayçtlar Dosyaya Yazdçrçlçyor ]"
					  LOCATE 3, 4: PRINT "Kitaplarçn listesi bu dosyaya text (metin) modunda yazçlacak..."
					  LOCATE 4, 4: PRINT "Kayçrlarçn dîkÅleceßi dosyançn ismini giriniz   [........ ...]"
					  E1$ = EInput(4, 53, 12, "Kitaplar.TXT", 3)
					  E1$ = RTRIM$(LTRIM$(E1$))
					  LOCATE 3, 4: PRINT "                                                               "
					  LOCATE 3, 4: PRINT "Bilgilerin aktarçlacaßç dosyançn ismi: " + E1$
					  LOCATE 4, 4: PRINT "Devam etmek istiyormusunuz ?                                   "
					  z = MsgBox(4, 27, -2)
					  LOCATE 4, 4: PRINT "                                                               "
					  IF z = 2 THEN GOTO Replay7
					  z = LOF(2) \ 60
1 :                                       OPEN E1$ FOR OUTPUT AS #3'dosya yazçlamadan hata oluüursa
					       FOR a = 1 TO z
						   GET 2, a
						   WRITE #3, D5$, RTRIM$(D6$)
						   LOCATE 4, 4: PRINT "Okunan, yazçlan blok:"; a, z - a; "  "
					       NEXT
					  CLOSE #3
					  LOCATE 3, 4: PRINT "Bilgiler hiá bir sorun áçkmadan " + E1$ + " isimli dosyaya kaydedildi"
					  LOCATE 4, 4: PRINT "Devam etmek iáin herhangi bir tuüa basçnçz..."
					  z = MsgBox(4, 69, -1)
				    CASE 3: GOTO Replay7'iptal
			     END SELECT
			     i$ = INPUT$(1)
	      END SELECT
	 CASE 4'yardçm
	      SELECT CASE Alfa
		     CASE 1'Kullanma Klavuzu
			    COLOR Renk1%, Renk2%
			    CLS
			    Cerceve 2!, 2, 21, 74, "[ Kullanma Klavuzu ]"
			     LOCATE 3!, 4: PRINT "Dosya menÅsÅ (Dosya iülemleri iáin)"
			     LOCATE 4!, 4: PRINT " Bu program áçkçü: Programdan áçkmançzç saßlar"
			     LOCATE 5!, 4: PRINT " Yeni kayçt kÅtÅßÅ yapma: òái boü kayçt kÅtÅßÅ yapar"
			     LOCATE 6!, 4: PRINT " Yeni arüiv kÅtÅßÅ yapma: òái boü arüiv kÅtÅßÅ yapar"
			     LOCATE 7!, 4: PRINT "Kayçt menÅsÅ (Kayçt iülemleri iáin)"
			     LOCATE 8!, 4: PRINT " Kayçtlara gîz atmak: Kayçtlarç ekranda listeler. (Ara, Sil, Deßiütir)"
			     LOCATE 9!, 4: PRINT " Yeni kayçt girme: Kayçtlara yeni bir bilgi giriüi iáin kullançlçr"
			     LOCATE 10, 4: PRINT " Geciken teslimatlar: östÅnden bir hafta geáen kayçtlar"
			     LOCATE 11, 4: PRINT "Arüiv menÅsÅ (Arüivleme iülemleri iáin)"
			     LOCATE 12, 4: PRINT " Arüive gîz atmak: Arüivi ekranda listeler. (Arama, Silme, Deßiütirme)"
			     LOCATE 12, 4: PRINT " Arüive yeni kayçt girme: Arüiv kÅtÅßÅne yeni bir kayçt girmek iáin"
			     LOCATE 13, 4: PRINT " Arüivin áçkdçsçnç almak: Arüivi yazçcçya veya istenilen dosyaya aktarçr"
			     LOCATE 14, 4: PRINT "Yardçm menÅsÅ (kullançm bilgisi)"
			     LOCATE 15, 4: PRINT " Kullanma klavuzu: Bu rehber sayfa..."
			     LOCATE 16, 4: PRINT " Programçn îzellikleri: Programçn bazç îzellikleri"
			     LOCATE 17, 4: PRINT " Program hakkçnda: Programçn telifi hakkçnda"
			     LOCATE 18, 4: PRINT " Programçn yazarç: Benim gîrÅü, dÅüÅnce ve isteklerim"
			     LOCATE 19, 4: PRINT " Tançtçm...: Programçn en can alçcç noktasç      The Best DEMONSTRATION"
			    'LOCATE 20, 4: PRINT
			     LOCATE 21, 4: PRINT " Program Pull-Down menÅ ile áalçüçp Easy-User sisteminde."
			     LOCATE 22, 4: PRINT " Gerekli ayrçntçlarç iülem sayfalarçnda bulabilirsiniz..."
			     i$ = INPUT$(1)
		     CASE 2'programçn îzellikleri
			    COLOR Renk1%, Renk2%
			    CLS
			    Cerceve 2!, 2, 15, 74, "[ Programçn ôzellikleri ]"
			     LOCATE 3!, 4: PRINT "Program tamamen +Easy User+ modunda yazçldç, kullançmç áok kolay"
			     LOCATE 4!, 4: PRINT "Bilgi tabanç áok kuvvetli, hiá bir üekilde boü alan yok. Dosyalar"
			     LOCATE 5!, 4: PRINT " doßrudan eriüimli (max:2,147,483,647)"
			     LOCATE 6!, 4: PRINT "òsterseniz kitaplarçn listesini dosyaya ve yazçcçya dîkebilirsiniz"
			     LOCATE 7!, 4: PRINT "Program áalçüçrken kendi îzel TÅrkáe klavye setini yÅklÅyor,"
			     LOCATE 8!, 4: PRINT " ve gerekli bÅtÅn ayarlamalarç yapçyor (ekran, ram, eriüim, virÅs, hata)"
			     LOCATE 9!, 4: PRINT "GîrdÅßÅnÅz gibi program áok gÅvenli fakat sizinde gÅvenlißiniz iáin!!!"
			     LOCATE 10, 4: PRINT "Bu Programçn Yasal Kullançcçsç Olmak òáin LÅtfen Bizi Arayçn"
			     LOCATE 11, 4: PRINT "Telefonlarçmçz: (0-324) 325 92 42,   (0-324) 482 21 44"
			     LOCATE 12, 4: PRINT "Adresimiz: Pirireis mah. Silifke cad. Umay apt. No: 462 Kat:3/5"
			     LOCATE 13, 4: PRINT "Mersin Of TURKEY                    (C&R) 1997 " + Ben2$ + "soft Design"
			     LOCATE 14, 4: PRINT "Eßer " + Ben2$ + " yazçlçm evine Åye deßilseniz, Åcretsiz broüÅr ve disketinizi"
			     LOCATE 15, 4: PRINT "almayç unutmayçnçz. Her zamanki gibi mutluluk ve saßlçk sizinle olsun!"
			     LOCATE 16, 4: PRINT "Devam edebilmek iáin herhangi bir tuüa basçnçz"
			     i$ = INPUT$(1)
		     CASE 3'program hakkçnda
			    COLOR Renk1%, Renk2%
			    CLS
			    Cerceve 2!, 2, 10, 74, "[ Program hakkçnda  ˛Kitaplçk V2.0˛ ]"
			     LOCATE 3!, 4: PRINT "Bu programla kÅtÅphane girdilerini kontrol edebilirsiniz"
			     LOCATE 4!, 4: PRINT "serisinin en genci olan bu versiyonda bir sÅrÅ yenilik var."
			     LOCATE 5!, 4: PRINT "program Easy User modunda yazçldç..."
			     LOCATE 6!, 4: PRINT "Bu programçn telif hakkç:"
			     LOCATE 7!, 4: PRINT "2. AtatÅrk EML. ôßretmenlerinden Sayçn: " + Ben3$ + "'a"
			     LOCATE 8!, 4: PRINT "aittir. Her ne üekilde olursa olsun kçsmen ya da tamaman"
			     LOCATE 9!, 4: PRINT "áoßaltçlamaz. Yasal kullançcç olmak istiyorsançz lÅtfen"
			     LOCATE 10, 4: PRINT "˛Programçn îzellikleri seáeneßini okuyunuz. òyi áalçümalar"
			     LOCATE 11, 4: PRINT "Devam edebilmek iáin herhangi bir tuüa basçnçz"
			     i$ = INPUT$(1)
		     CASE 4: COLOR Renk1%, Renk2%'program
			     CLS
			     Cerceve 2!, 2, 17, 74, "[ Programçn Yazarç ]"
			      LOCATE 3!, 4: PRINT "  Halen bir lisede okumaktayçm. Bilgisayarlara karüç áocukluktan gelen"
			      LOCATE 4!, 4: PRINT "bir sempatim var, ufakken atariye olan dÅükÅnlÅßÅm bir Commodore 64 ile"
			      LOCATE 5!, 4: PRINT "sîndÅrÅldÅ. O zamanlar tek bildißim üey ismimi yazmaktç artçk ilerlettim"
			      LOCATE 6!, 4: PRINT "ismimi yazarken bÅyÅk ve kÅáÅk harfleri aynç anda kullanabiliyorum..."
			      LOCATE 7!, 4: PRINT "  ôvÅná METE: Geráekleri konuümak gerekirse bu gÅnlere gelebilmek iáin"
			      LOCATE 8!, 4: PRINT "áok áile áektim. Äekemeyenler, kçskananlar, menfaaitáiler vb. bu tÅr"
			      LOCATE 9!, 4: PRINT "engellerle áok karüçlaütçm fakat yçlmadçm, yçlmayacaßçmda înÅme geáen"
			      LOCATE 10, 4: PRINT "engellere artçk alçütçm. Ve bana dur diyenlere meydan okuyorum, onlar"
			      LOCATE 11, 4: PRINT "kendilerine tahmin edebilirler. Sayçn îßretmenim hatçrlçyormusunuz bana"
			      LOCATE 12, 4: PRINT "bana saydçnçz boü ve anlamsçz tehditleri, îßretmenlik yalnçzca dersini"
			      LOCATE 13, 4: PRINT "verip áçkmakla bitmez, îßretmen îßrenciye hayat verir, hayatç îßretir."
			      LOCATE 14, 4: PRINT " Anlayabiliyormusunuz bilmiyorum ama anlamaya áalçüçn..."
			      LOCATE 15, 4: PRINT ""
			      LOCATE 16, 4: PRINT " Ayrçyeten bu áok sevdißim hocalarçmada sÅprizim var eßer bu programçn"
			      LOCATE 17, 4: PRINT "kopyasçnç alçrlarsa kendilerine bÅyÅk bir sÅrprizim olacak. TÅrkiye"
			      LOCATE 18, 4: PRINT "Cumhureiyetinin SanatkÉrlara verdißi yetkiye dayanarak."
			      i$ = INPUT$(1)
		     CASE 5'demonstration
			  'Demo one
			    WIDTH 40
			     SCREEN 13, 255
			     COLOR 16
			    LOCATE 10, 13: PRINT "Program Designer"
			    LOCATE 11, 13: PRINT "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
			    LOCATE 12, 13: PRINT " Gizem Software "
			    PPrint (16)
			    LOCATE 10, 13: PRINT "-  Writted By  -"
			    LOCATE 11, 13: PRINT "  ƒƒƒƒƒƒƒƒƒƒƒƒ  "
			    LOCATE 12, 13: PRINT "   ôvÅná METE   "
			    PPrint (16)
			    CLS
			    LOCATE 10, 13: PRINT "Please Wait!"
			    DEF SEG = &HA000'screen graphic segment (Default Segment=A000)
			      FOR i = 0 TO 199
				  LINE (0, i)-(319, i), (i / 199) * 191
			      NEXT
			    FOR i = 0 TO 63'BGR
				PALETTE i, 256 * (63 - i) + i
				PALETTE i + 64, 65536 * i + (63 - i)
				PALETTE i + 128, 65536 * (63 - i) + 256 * i
			    NEXT
			    COLOR 255
			    OFont "ThE BeST DeMOnsTraTiON", 65, 1, 255
			    OFont "I'm mad", 130, 10, 254
			    OFont "What are you ?", 100, 20, 253
			    OFont "Hay baby", 130, 30, 252
			    OFont "My name is ALBeRT", 90, 40, 251
			    OFont "Hit any keyboard to continue", 45, 190, 250
			    PPrint (255)
			    PPrint (254)
			    PPrint (253)
			    PPrint (254)
			    PPrint (252)
			    PPrint (251)
			    PPrint (250)
			    PPrint (250)
			    WHILE "" = INKEY$
			    WEND
		  GOTO Replay8
	      END SELECT
  END SELECT
Replay8: 'Eßer program buradan baülarsa; grafik modu aáçk ve kapatçlmasç gerekir
 SCREEN 9
 SCREEN 0
 CLS
 TurkFont (Kar%)

Replay7:
 Alfa = Menu
 Gama = Alfa
GOTO Replay0


BlockList:
w% = LOF(1) \ 68
IF w% = 0 THEN
   LOCATE 22, 4: PRINT "Dosyada üu anda hiá kayçt olmadçßç iáin burayç kullanamazsçnçz."
   a = MsgBox(22, 69, -1)
   GOTO Replay7
END IF

IF q% > w% THEN SOUND 600, .5: w% = -1: RETURN

COLOR , 1
 GET 1, q%: LOCATE 15, 4: PRINT D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$
COLOR , 7
 IF q% + 1 <= w% THEN GET 1, q% + 1: LOCATE 16, 4: PRINT D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$ ELSE EPrint 16, 3
 IF q% + 2 <= w% THEN GET 1, q% + 2: LOCATE 17, 4: PRINT D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$ ELSE EPrint 17, 4
 IF q% - 1! > 0! THEN GET 1, q% - 1: LOCATE 14, 4: PRINT D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$ ELSE EPrint 14, 1
 IF q% - 2! > 0! THEN GET 1, q% - 2: LOCATE 13, 4: PRINT D1$ + "/" + D2$ + "/" + D3$ + "≥" + D4$ + "≥" + D5$ ELSE EPrint 13, 1
RETURN

BlockList2:
w% = LOF(2) \ 60
IF w% = 0 THEN
   LOCATE 22, 4: PRINT "Dosyada üu anda hiá kayçt olmadçßç iáin burayç kullanamazsçnçz."
   a = MsgBox(22, 69, -1)
   GOTO Replay7
END IF

IF q% > w% THEN SOUND 600, .5: w% = -1: RETURN

COLOR , 1
 GET 2, q%: LOCATE 15, 7: PRINT D6$; "   ≥   " + D7$
COLOR , 7
 IF q% + 1 <= w% THEN GET 2, q% + 1: LOCATE 16, 7: PRINT D6$ + "   ≥   " + D7$ ELSE LOCATE 16, 7: PRINT "..........................≥........................................"
 IF q% + 2 <= w% THEN GET 2, q% + 2: LOCATE 17, 7: PRINT D6$ + "   ≥   " + D7$ ELSE LOCATE 17, 7: PRINT "..........................≥........................................"
 IF q% - 2 > 0 THEN : GET 2, q% - 2: LOCATE 13, 7: PRINT D6$ + "   ≥   " + D7$ ELSE LOCATE 13, 7: PRINT "..........................≥........................................"
 IF q% - 1 > 0 THEN : GET 2, q% - 1: LOCATE 14, 7: PRINT D6$ + "   ≥   " + D7$ ELSE LOCATE 14, 7: PRINT "..........................≥........................................"
RETURN


BlockCompare: 'Tarih dolup dolmadçßç kontrol ediliyor, BETA UYARLAMA
 d = -1
 s2 = VAL(D2$) MOD 2
 IF s2 = 1 THEN S1 = 31
 IF s2 = 0 THEN S1 = 30

 D1 = VAL(D1$): D2 = VAL(D2$): D3 = VAL(D3$)
 E1 = VAL(E1$): E2 = VAL(E2$): E3 = VAL(E3$)

 IF D2 = 2 AND D3 MOD 4# = 0 THEN S1 = 29
 IF D2 = 2 AND D3 MOD 4 <> 0 THEN S1 = 28

 IF E1 > D1 + 7 THEN d = -(D1 - E1 + 7)
 IF E1 = D1 + 7 THEN d = 0

 IF E2 > D2 AND S1 - D1 + E1 > 7 THEN d = S1 - D1 + E1 - 7
 IF d = -1 AND E2 > D2 AND S1 - D1 + E1 = 7 THEN d = 0

 IF E3 > D3 AND -(D1 - E1 + (D2 - E2 - 12) * 30 - 5) > 7 THEN d = -(D1 - E1 + (D2 - E2 - 12) * 30 - 5)
 IF E3 > D3 AND -(D1 - E1 + (D2 - E2 - 12) * 30 - 5) = 7 THEN d = 0
RETURN

HATA:
 PCOPY 1, 0
 COLOR 15, 1
 CLS
 SELECT CASE ERR
	CASE 4!: i$ = "Bilgi dçüç DATA var..."
	CASE 6!: i$ = "Yapçlan matematiksel iülemin sonucunda taüma meydana geldi (o=1)"
	CASE 7!: i$ = "Yeterli hafçza yok. Programçn áalçümasç iáin geleneksel belleßi artçrçn"
	CASE 11: i$ = "Yapçlan matematiksel iülemde sçfçr sçfçra bîlÅnmeye áalçüçldç (z=?)"
	CASE 13: i$ = "Kullançcç hatasçndan meydana gelen bir tÅr uyuümazlçßç hatasç"
	CASE 17: i$ = "Program tekrar baülatçlamçyor"
	CASE 21: i$ = "Yazçlamaz hata!!!"
	CASE 23: i$ = "Tampona ayrçlan saha yetersiz"
	CASE 24: i$ = "Yazçcç Ånitesi hazçr deßil"
	CASE 25: i$ = "Yazçcç Ånitesi arçzalç"
	CASE 27: i$ = "Yazçcçda kÉßçt yok"
	CASE 28: i$ = "Yazçlamaz hata!!!"
	CASE 31 TO 49: i$ = "Yazçlamaz Hata!!! (diskten dolayç oluütu)"
	CASE 50: i$ = "Tançmlanan kayçt alanlarçnda hata var"
	CASE 51: i$ = "òásel bir hata oluütu!!!"
	CASE 52: i$ = "Dosya numarasç hatalç!!!"
	CASE 53: i$ = "Gerekli olan dosyalardan biri bulunamadç"
	CASE 54: i$ = "Dosya referans numarasç hatasç!!!"
	CASE 55: i$ = "TÅm dosyalar aáçk!!!"
	CASE 56: i$ = "Yazçlamaz hata!!!"
	CASE 57: i$ = "önite ile baßlantç kurulamadç"
	CASE 58: i$ = "Dosyadan zaten var (olmamasç gereken dosya mevcut)"
	CASE 59, 60: i$ = "HATA"
	CASE 61: i$ = "Disket tamamen dolu (diskette yeterli boü yer yok)"
	CASE 63: i$ = "Hatalç kayçt adç!!!"
	CASE 64: i$ = "Hatalç dosya adç!!!"
	CASE 68: i$ = "önite hazçr deßil (bilgi alçnamadç, baßlantç kurulamçyor)"
	CASE 70: i$ = "Disk yazmaya karüç korumalç (diskin korumasçnç aáçn)"
	CASE 71: i$ = "Disket yok (disketi sÅrÅcÅye yerleütirin)"
	CASE 72: i$ = "Diskin alanlarçnda hata var (diskde bozukluklar var)"
	CASE 74: i$ = "Yeni isimlecek disk yok"
	CASE 75: i$ = "ôzel kÅtÅphane ismi yok"
	CASE 76: i$ = "ôzel kÅtÅphane iáin isim yok"
	CASE ELSE: i$ = "Belirlenemeyen bir hata oluütu"
 END SELECT
 COLOR 15, 4
 CLS
 Cerceve 2, 2, 8, 74, "[ Programçn Äalçümasç Esnasçnda Bir Hata òle Karüçlaüçldç ]"
  LOCATE 3, 4: PRINT "Program áalçümasç esnasçnda bir hata ile karüçlaüdç."
  LOCATE 4, 4: PRINT "Hatançn kodu:"; ERR; "      Hatançn kullandçßç etiket:"; ERL
  LOCATE 5, 4: PRINT "HATA: " + i$
  LOCATE 6, 4: PRINT "Programç tekrar áalçütçrmak istiyormusunuz"
  LOCATE 7, 4: PRINT "evet   : Program hafçzada yinelenecek"
  LOCATE 8, 4: PRINT "hayçr  : Program kapatçlacak"
  LOCATE 9, 4: PRINT "iptal  : Program kaldçßç yerden áalçümaya devam edecek"
  SELECT CASE MsgBox(7, 4, 3)
	 CASE 0: GOTO Repeat08
	 CASE 1'program hafçzada yineleniyor
		COLOR , 1
		CLS
		Cerceve 2, 2, 3, 74, "[ Program Hafçzada Yineleniyor ]"
		 LOCATE 3, 4: PRINT "Program hafçzada yineleniyor. Terar bir hata olursa programdan áçkçp"
		 LOCATE 4, 4: PRINT "donançmçnçzç ve yazçlçmçnçzç kontrol edin."
		 F! = MsgBox(4, 69, -1)
		 RESUME Repeat09
Repeat09:        RUN
	 CASE 2: GOTO Repeat08'program kapatçlçyor
	 CASE 3'program kaldçßç yerden devam ediyor
		COLOR , 1
		CLS
		Cerceve 2, 2, 3, 74, "[ Program Kaldçßç Yerden Äalçümaya Devam Edecek ]"
		 LOCATE 3, 4: PRINT "Program kaldçßç yerden áalçümaya tekrar devam edecek. Tekrar bir hata"
		 LOCATE 4, 4: PRINT "ile karüçlaüçrsançz bu sefer programç tekrar baülatmayç seáin."
		 F! = MsgBox(4, 69, -1)
		 PCOPY 0, 1
		 RESUME NEXT
  END SELECT
END

FUNCTION BiosDate$
	 DEF SEG = &HF000
	 FOR a = &HFFF5 TO &HFFFF - 3
	     i$ = i$ + CHR$(PEEK(a))
	 NEXT
	 BiosDate$ = Decode(i$, 150)
END FUNCTION

SUB Cerceve (MenuDikey, MenuYatay, MenuYuksek, MenuGenis, i$)

 COLOR , 8
 FOR i = 1 TO MenuYuksek + 1
     LOCATE MenuDikey + i, MenuYatay + MenuGenis + 2
      PRINT "  ";
 NEXT

 LOCATE MenuDikey + MenuYuksek + 1, MenuYatay + 2
  PRINT SPACE$(MenuGenis);

 COLOR 15, 7
 FOR i = 1 TO MenuYuksek - 1
     LOCATE MenuDikey + i, MenuYatay
      PRINT "≥" + SPACE$(MenuGenis) + "≥"
 NEXT

 LOCATE MenuDikey, MenuYatay
  PRINT "⁄" + STRING$(MenuGenis, "ƒ") + "ø"
 LOCATE MenuDikey + MenuYuksek, MenuYatay
  PRINT "¿" + STRING$(MenuGenis, "ƒ") + "Ÿ";

'Bu satçrlar iptal edildi... (ôzel ekran kullançlmadçßç iáin)
'DEF SEG = &HB800
'   FOR I = 1 TO MenuGenis + 1
'       POKE ((MenuDikey + MenuYuksek) * 160 + (MenuYatay + I) * 2 + 1), 9
'   NEXT       hata var yanlçü formÅl --> 80*y+x
'   FOR I = 1 TO MenuYuksek + 1
'       TR = ((MenuDikey + I - 1) * 160 + (MenuYatay + MenuGenis + 1) * 2 + 1)
'       POKE TR, 9
'       POKE TR + 2, 9
'   NEXT
'DEF SEG

 LOCATE MenuDikey, MenuYatay + 3: PRINT i$
 COLOR Renk1%, Renk6%

END SUB

FUNCTION DateValid% (Month%, Day%, Year%)
	 SELECT CASE Month%
		CASE 4, 6, 9, 11:           IF Day% < 1 OR Day% > 30 THEN DateValid% = 0: EXIT FUNCTION
		CASE 1, 3, 5, 7, 8, 10, 12: IF Day% < 1 OR Day% > 31 THEN DateValid% = 0: EXIT FUNCTION
		CASE 2
					    IF Year% MOD 4 = 0 THEN
					       IF Day% < 1 OR Day% > 29 THEN DateValid% = 0: EXIT FUNCTION
					    ELSE
					       IF Day% < 1 OR Day% > 28 THEN DateValid% = 0: EXIT FUNCTION
					    END IF
		CASE ELSE:                  DateValid% = 0: EXIT FUNCTION
		END SELECT
		DateValid% = 1
END FUNCTION

FUNCTION Decode$ (i$, y)
FOR x = 1 TO LEN(i$)
    MID$(i$, x, 1) = CHR$(ASC(MID$(i$, x, 1)) + y)
NEXT
Decode$ = i$
END FUNCTION

FUNCTION EInput$ (a, b, c, d$, E)

'A = 10  locate a
'B = 10  locate  ,b
'C=20    Input(C)
'D$      "Untitled"
i$ = d$'(istek) escape tuüuna basçlçrsa bilgi selinmesin, ekisi olsun

q% = False
SELECT CASE E
       CASE 2:    c$ = "1234567890@qwertyuopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM!'^%&()=?_#${[]}\*-+*/.,:;<>| " + CHR$(34)               ' English key set
       CASE 3:    c$ = "1234567890@qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM!%&()_#${[]}*-+*.<>|"                                   ' File Name
       CASE 4:    c$ = "1234567890"                                                                                                            ' Numeric key set
       CASE 5:    c$ = "1234567890 "                                                                                                           ' EXCLUSIVE
       CASE ELSE: c$ = "1234567890@qwertyuçopßÅasdfghjklüizxcvbnmîáQWERTYUIOP¶öASDFGHJKLûòZXCVBNMôÄ!'^%&()=?_#${[]}\*-+*/.,:;<>| " + CHR$(34)  ' Turkish key set
END SELECT
d$ = d$ + SPACE$(c - LEN(d$))
F = 1
JL = 0
DosyaNe:
    z$ = "": IF JL = 0 THEN LOCATE , , 1, 7 ELSE LOCATE , , 1, 4, 7
    b$ = d$
    IF F > c THEN F = c
    LOCATE a, b: COLOR 15, 1: PRINT b$
    LOCATE a, b + F - 1, 1


    DO
	  a$ = INKEY$
    LOOP WHILE a$ = ""

    z = ASC(RIGHT$(a$, 1)): IF LEN(a$) <> 2 GOTO Ds1
    IF z = 72 OR z = 15 THEN q% = True: GOTO Quit.EInput'up, tab
    IF z = 80 THEN GOTO Quit.EInput'down
    IF z = 77 AND F < c THEN F = F + 1
    IF z = 75 AND F > 1 THEN F = F - 1
    IF z = 83 THEN d$ = LEFT$(d$, F - 1) + MID$(d$, F + 1, c - F) + " "
    IF z = 82 THEN IF JL = 0 THEN JL = 1 ELSE JL = 0
    GOTO DosyaNe

Ds1:
   IF ASC(a$) = 27 THEN d$ = i$: GOTO Quit.EInput
   IF ASC(a$) = 13 OR ASC(a$) = 9 THEN GOTO Quit.EInput

   IF ASC(a$) = 8 THEN
      IF F = 1 GOTO DosyaNe
      IF F = c AND MID$(d$, F, 1) <> " " THEN F = F + 1
      MID$(d$, F - 1, 1) = " ": F = F - 1: GOTO DosyaNe
   END IF

   IF INSTR(1, c$, a$) > 0 THEN
      IF JL = 0 THEN
	 d$ = d$ + SPACE$(c - LEN(d$))
	 MID$(d$, F, 1) = a$
      ELSE
	 d$ = LEFT$(d$, F - 1) + a$ + MID$(d$, F, c - F)
      END IF
    IF F = c THEN GOTO Quit.EInput
    IF F < c THEN F = F + 1
   ELSE : SOUND 800, .6
   END IF
   GOTO DosyaNe

Quit.EInput:
     d$ = d$ + SPACE$(c - LEN(d$))
     COLOR Renk1%, 7
     LOCATE a, b, 0
     PRINT d$
     EInput$ = d$
    'Exit Function
END FUNCTION

SUB EkranaDegiskenYaz (c, i$)
    LOCATE 12 + c, 4: PRINT i$
END SUB

SUB EPrint (c%, a)
    LOCATE c%, 4: PRINT "ƒƒƒƒƒƒƒƒƒƒ≥ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≥ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
END SUB

SUB EWrite (w%, q%, i$, Time!)

    FOR w! = 1 TO LEN(i$)
	Waits Time!
	SOUND 800, .1
	LOCATE 1 + w%, q%: PRINT LEFT$(i$, w!)
    NEXT

END SUB

FUNCTION Klavye$
Dongu01:
 i$ = INKEY$: IF i$ = "" THEN GOTO Dongu01
 SELECT CASE i$
	CASE CHR$(0) + CHR$(75): Klavye$ = "<"  'left
	CASE CHR$(0) + CHR$(77): Klavye$ = ">"  'right
	CASE CHR$(0) + CHR$(72): Klavye$ = "+"  'up
	CASE CHR$(0) + CHR$(80): Klavye$ = "-"  'down
	CASE CHR$(13):           Klavye$ = "*"  'enter
	CASE CHR$(27):           Klavye$ = "|"  'escape
	CASE ELSE'üu anda iptal edildi
		 ' IF Q% = True AND INSTR(1, A$, I$) > 0 THEN
		 '    Klavye$ = I$
		 ' ELSE
		      SOUND 700, .3
		      GOTO Dongu01
		 ' END IF
 END SELECT
END FUNCTION

'mesage box for windows type
FUNCTION MsgBox (x, y, z)

'Kullançlar deßiükenler
'X!: Mesajçn baülangçá satçrç
'Y!: Mesajçn baülangçá sÅtunu
'Z!: Mesajçn boyutu
'W!: Kobay deßiüken
'P!: MenÅ yatay durumda hareket etsin
'Q1: MenÅnÅn eski yeri
'Q2: MenÅnÅn yeni yeri
'I$: Klavye tamponun deßeri

'Kullançlarn etiketler
'Repeat01: Klavyeden seri deßer okumak iáin
'Repeat02: Madde imini seri olarak kullanabilmek iáin

'Deßiükenler tançmlançyor
 DIM i$(1 TO 3)
 Q2 = 1
 Q1 = 1
 p! = 0

'Seáilen menÅ sistemi yÅkleniyor
 SELECT CASE z
	CASE 1, -1: 'Tamam
		    'I$(1) = "Tamam"
		     COLOR 31, 1
		     LOCATE x, y: PRINT " Tamam "
		     WHILE INKEY$ = "": WEND
		     GOTO Label01
	CASE 2, -2: 'Evet-Hayçr
		     i$(1) = " Evet  "
		     i$(2) = " Hayçr "
	CASE 3, -3: 'Evet-Hayçr-òptal
		     i$(1) = " Evet  "
		     i$(2) = " Hayçr "
		     i$(3) = " òptal "
	CASE ELSE: MsgBox = -1: GOTO Label01'!!!HATA!!!
 END SELECT

'MenÅ yataysa ayarlarç deßiütir
 IF z < 1# THEN
    z = -z
    p = -1
    y = y! + 7
 END IF

'Ekrana mesaj kutularç yerleütiriliyor
 COLOR 7, Renk4%
 IF p = 0! THEN
    x = x - 1
    FOR w = 1 TO z
	LOCATE x + w, y: PRINT i$(w)
    NEXT
 END IF
 IF p = -1 THEN
    FOR w = 1 TO z
	LOCATE x, y * w: PRINT i$(w)
    NEXT
 END IF

'Ekranda ilk madde olan evet imleniyor
 IF p = 0! THEN GOSUB Repeat02
 IF p = -1 THEN GOSUB Repeat03

Repeat01: 'Tekrarlama etiketi
 'Basçlan tuüa gîrev aktarçmç yapçlçyor
  i$ = INKEY$
  SELECT CASE i$
	 CASE CHR$(0) + CHR$(72), CHR$(0) + CHR$(75), CHR$(0) + CHR$(15): Q1 = Q1 - 1'down,left
	 CASE CHR$(0) + CHR$(80), CHR$(0) + CHR$(77), CHR$(9): Q1 = Q1 + 1'up,right
	 CASE CHR$(13), CHR$(32): MsgBox = Q1: GOTO Label01       'enter, space bar
	 CASE CHR$(27): IF i$(3) = " òptal " THEN MsgBox = 3! ELSE MsgBox = 2!
			GOTO Label01'escape
	 CASE ELSE: GOTO Repeat01                                 'repeat
  END SELECT

 'Deßerler aüçldçßçnda kenar sçáramasç yapçlçyor
  IF Q1 > z THEN Q1 = 1
  IF Q1 < 1 THEN Q1 = z

 'MenÅ hareketi yapçlçyor
  IF p = 0! THEN GOSUB Repeat02
  IF p = -1 THEN GOSUB Repeat03

GOTO Repeat01

Repeat02: 'MenÅ dikey hareket ediyor
  COLOR 7, Renk4%: LOCATE x + Q2, y: PRINT i$(Q2)
  COLOR Renk1%, Renk2%: LOCATE x + Q1, y: PRINT i$(Q1)
  Q2 = Q1
RETURN

Repeat03: 'MenÅ yatay hareket ediyor
  COLOR 7, Renk4%: LOCATE x, y * Q2: PRINT i$(Q2)
  COLOR Renk1%, Renk2%: LOCATE x, y * Q1: PRINT i$(Q1)
  Q2 = Q1
RETURN

Label01: 'Äçkçlçrken ayarlarda resetleniyor
  COLOR Renk1%, Renk6%
EXIT FUNCTION

END FUNCTION

SUB OFont (Text$, x, y, Fore)
    DEF SEG = &HFFA6
    FOR Letter = 1 TO LEN(Text$)
	address = (8 * ASC(MID$(Text$, Letter))) + 14
	FOR height = 0 TO 7
	    mask = PEEK(address + height) * 128
	    LINE (x + curntx + 1, y + height + 1)-(x + 9 + curntx, y + height + 1), Fore, , mask
	NEXT
	curntx = curntx + 8
    NEXT
END SUB

FUNCTION Orijinal
2 :      OPEN File3$ FOR BINARY AS #3
3 :           a = LOF(3) - 10
	      i$ = "../../.."
4 :           GET 3, a, i$
	 CLOSE #3
	 IF BiosDate = i$ THEN Orijinal = True
	 IF BiosDate <> i$ THEN Orijinal = False
END FUNCTION

SUB PPrint (i)
    FOR x = 0 TO 63
	PALETTE i, 65536 * x + 256 * x + x
	SOUND 32767, .2
    NEXT
    SOUND 32767, 1
    FOR x = 63 TO 1 STEP -1
	PALETTE i, 65536 * x + 256 * x + x
	SOUND 32767, .2
    NEXT
END SUB

SUB Virus
5 :      OPEN File3$ FOR BINARY AS #3
	      i$ = "###"
6 :           GET 3, LOF(3) - 2, i$
	 S1 = LOF(3)
	 CLOSE #3

7 :      s2 = 65536 * ASC(MID$(i$, 1, 1)) + 256 * ASC(MID$(i$, 2, 1)) + ASC(MID$(i$, 3, 1))

	 IF s2 <> S1 THEN
	    COLOR Renk1%, Renk2%
	    CLS
	    Cerceve 2!, 2, 12, 74, "[ Bu Programda VirÅs Olabilir ]"
	     LOCATE 3!, 4: PRINT "  BÅyÅk bir ihtimalle bu dosyaya virÅs bulaümçü. Dosyançn boyutunda"
	     LOCATE 4!, 4: PRINT S1 - s2; "bayt bÅyÅme var. Dosyayç anti virÅs programçyla test edin"
	     LOCATE 5!, 4: PRINT "virÅs var ve silindiyse bu programç tekrar áalçütçrçp bu hata mesajçnçn"
	     LOCATE 6!, 4: PRINT "áçkmadçßçnç onaylayçn."
	    'LOCATE 8!, 4: PRINT
	     LOCATE 9!, 4: PRINT "BòZE HABERVERòN"
	     LOCATE 10, 4: PRINT "  Kayçtlç kullançcç oldußunuz iáin bu programçn orijinal halini bizden"
	     LOCATE 11, 4: PRINT "alabilirsiniz. Bozulmuü olan programç ve Gizem yazçlçm evi yaka kartçnçn"
	     LOCATE 12, 4: PRINT "fotokopisini bize ulaütçrçn yeni disketiniz en kçsa zamanda elinize"
	     LOCATE 13, 4: PRINT "ulaüacaktçr."
	     i$ = INPUT$(1)
	     SYSTEM
	 ELSE
	      CLS
	      LOCATE 13, 13: PRINT "Program test ediliyor..."
	      Waits .1
	      LOCATE 13, 13: PRINT "Programda virÅs bulunamadç..."
	      Waits .3
	 END IF
END SUB

SUB Waits (Time)
    OldTime = TIMER
    WHILE NOT TIMER > OldTime + Time
    WEND
END SUB

