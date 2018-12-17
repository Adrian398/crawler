Gruppe 7: Jonas Pirner, Patrick Schiemann, Marlo Wockenfuß, Raphael Wandrey
V0.9


### VORBEMERKUNG ###

Um die Methode "register_google()" des "ggmap"-Pakets nutzen zu können, ist die neueste Version vonnöten (nicht im CRAN-Repository enthalten). Der Installationsbefehl hierfür lautet: devtools::install_github("dkahle/ggmap").



### BEGRÜNDUNGEN FÜR DAS AUSLASSEN (BZW. SCRAPEN MITTELS FACEBOOK) DER FOLGENDEN LINKS ###


159 - Labyrinth:					
Es gibt Veranstaltungen die sich wöchentlich wiederholen (z.B. jeden Freitag – Friday Night Rock) und Veranstaltungen die an einem bestimmten Datum stattfinden. Des Weiteren werden keine einheitliche CSS Tags benutzt. Die Facebook Seite gibt die wöchentlichen Events hingegen dediziert an. Daher wurde die Veranstaltungen mit dem Facebookscraper ausgelesen.

162 - Rathaus: Das Rathaus listet keine Veranstaltungen und verweist auch auf keine anderen.

167 - Standard Würzburg: Das Standard stellt seine Veranstaltungen lediglich als Bild zur Verfügung. Nur die Öffnungszeiten lassen sich auf der Seite auslesen. Allerdings sind die Veranstaltungen auch auf ihrer Facebook Seite zu finden, welche wir mit unserem Facebookscraper auslesen konnten.
							
169 - Café Schönborn: Das Café Schönborn veranstaltet keine Events. Weder auf Facebook noch auf ihrer eigenen Seite sind Veranstaltungen abgebildet. Einzig die Öffnungszeiten lassen sich auf ihrer Seite auslesen.

171 - Schützenhof: Die Veranstaltungen sind lediglich in .jpg aufgelistet. Die Veranstaltungen werden nicht auf Facebook gelistet.

172 - Amt für Versorgung und Familienförderung:	Auf der Seite existieren keine Veranstaltungen - es sind nur Informationen zu Dienstleistungen (Blindengeld, Betreuungsgeld, usw.) zu finden.

176 - Corso-Kinocenter:	Das Kino existiert seit 2009 nicht mehr in Würzburg. Das unter der Website anzutreffende Kino liegt in 42477 Radevormwald (NRW).

177 - Kunstforum: Die Seite existiert nicht.					
 
181 - Galerie Bernhard Schwanitz: Die von Herrn Schwanitz einzig gelistete Veranstaltung stammt aus dem Jahr 2016 und fand in Frankfurt statt. Aufgrund der jetzigen Darstellung der gelisteten Veranstaltung, ist davon auszugehen, dass neu eingepflegte nicht mehr ausgelesen werden könnten. Daher verzichteten wir die Seite zu scrapen.
