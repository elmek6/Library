DECLARE SUB TurkFont (BYVAL kar%)
DECLARE FUNCTION EgaVgaMi% ()
kar% = EgaVgaMi%
IF kar% <> 0 THEN TurkFont (kar%)

SELECT CASE kar%
        CASE 16: PRINT "VGA ekran kartç ve TÅrkáe karakterler : ¶ßòçÄáûü"
        CASE 14: PRINT "EGA ekran kartç ve TÅrkáe karakterler : ¶ßòçÄáûü"
        CASE 0: PRINT "Ekran kartç EGA-VGA degildir."
END SELECT

END



