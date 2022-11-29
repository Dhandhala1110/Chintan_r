Hinweise zum Assistenzsystem und Programmcode

WICHTIG!!!
Bei erstmaligem Ausführen des Programms Variable "firstuse" in Datei app.R Zeile 2 auf TRUE setzen. Dient zum Installieren der benötigten Bibliotheken. Bei weiteren Starts kann die Variable auf FALSE gesetzt werden.

Start der Applikation aus R-Studio
    Start in einem R-Studio Fenster, ausführen der Datei app.R
    Start in einem Browser Fenster, ausführen der Datei start.R

Start der Applikation über Ikon 
	'Wissensspeicher.cmd' ausführen --> Dateipfade innerhalb der Befehls-Datei anpassen!!!

    
Benutzeroberfläche:
    Aufteilung in 4 Tabs(Gesamt Struktur, Prozesseingriff, Fehlerübersicht, Parameter Lernen)
    Tab Beschreibungen:
        - Gesamt Struktur: Darstellen der beiden Haupt Bayes Netze "Vorformling" und "Blasformteil", Aktualisierung der Grafik durch Button 'Aktualisieren' oder Tab-Wechsel, Interaktiver Graph somit ist Zoomen und Verschieben von Knoten möglich, durch Klick auf einen Knoten werden alle weiteren Knoten mit direkter Kante hervorgehoben und alle anderen werden ausgegraut
        
        - Prozesseingriff: Ermittlung der Fehlerursache, Auswahlmöglichkeit des Ursachetyps wahrscheinlichste (default) oder einflussstärkste, Auswahl des benötigten Fehlerbildes über zwei Drop-Down Menüs, Start der Vorhersage durch Schaltfläche 'Fehlerursachenermittlung', Prozess abbrechen durch Schaltfläche 'Abbrechen', vorhergesagte Ursache wird textuell ausgegeben und auf der rechten Seite graphisch dargstellt (grün Fehlerknoten, rot Ursacheknoten), unten links Tabellenausgabe mit absteigenden Ursachen, Rückmeldung an System ob Fehler behoben wurde oder weitere Ursache benötigt wird, Fehler nicht behoben (Schaltfläche 'Nein') und einer manuellen Ursache System erfragt Knotenzustand vor Eingriff ab, Fehler behoben (Schaltfäche 'Ja') System fragt offene übrige manuelle Ursachen Zustände über ein Pop-Up Fenster ab
            Bei Start der Vorersage sowie jeder weiteren Betätigung der Schaltfächen 'Nein' und 'Ja' wird ein Datenabzug der CPU Daten angestoßen --> Aktuell nicht umgesetzt. Es werden zufällig Daten generiert und verwendet.
            Datenaufbereitung muss noch implementiert werden, erhaltene CPU Daten in die vorgesehenen Klassen einteilen
        
        - Fehlerübersicht: Anzeige von einzelnem Fehlerbild, Selektieren des Fehlerbildes über zwei Drop-Down-Menüs (Vorformling/ Blasformteil und Fehlerbild) möglich, Über Butten 'Ausagbe' wird die Grafik erneuert, blau hinterlegt wird der Fehlerknoten, alle weiteren Ursacheknoten werden mit weißer Hintergrundfarbe dargestellt, kein interaktiver Graph somit Zoom und Verschieben nicht möglich
        
        - Parameter Lernen: Bayes Netz kann mit Datensatz trainiert werden, Bayes Netz Auswahl zwischen den beiden Hauptnetzen "Vorformling" und "Blasformteil", über Button 'Durchsuchen' entsprechend vorgesehenen Datensatz (Layoutformat Spalten durch ; trennen und keine Zeilennumerierung) zum Trainieren auswählen, hochgeladene Daten werden als Tabelle dargestellt und als csv-Datei unter "Data_archive/data_download.csv" gespeichert, Training wird über Schaltfläche 'Parameter Lernen' gestartet (greift auf abgespeicherten Datensatz zurück, nach Training erscheint ein Hinweis das sich Parameter des Bayes Netzes geändert haben
        
        
Programmcode ist in zwei Abschnitte aufgeteilt. 
    Abschnitt 1: ui; Dort wird das Layout definiert und gestaltet --> Platzieren von Ein- und Ausgaben auf dem Bildschirm
                Für jeden Tab gibt es einen eigenen Abschnitt tabItem(tabName = <tabname>...
                Mittel inputID/ outputID wird innerhalb des server Bereiches auf die Objekte zugegriffen
    Abschnitt 2: server; Hier werden Funktionen und Algorithmen, die eigentliche Logik ausgeführt. Es besteht Zugriff auf Daten und definierte Funktionen (functions.R)
                Optische Einteilung/ Abgrenzung der einzelnen Bereiche der Tabs
                
    Funktionen sind in der Datei functions.R definiert. Dort sind die Funktionen in inhaltlische Abschnitte zusammengefasst und gegliedert.

    In der main.R Datei werden Bibliotheken sowie eigene definierte Funktionen geladen, Python gestartet und falls nicht vorhanden die zwei txt-Dateien für die Modell-Strings der beiden Haupt Bayes Netze erstellt.
