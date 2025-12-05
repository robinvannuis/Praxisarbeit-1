#let Star-Schema = link(<grd:star_schema>)[Star-Schema]
= Einleitung

== Motivation

In einer zunehmend datengetriebenen Unternehmenslandschaft gewinnen analytische Auswertungen und Berichte durchgehend an Bedeutung. Entscheidungen darüber welche Features oder Software entwickelt werden soll, basieren zumeist noch auf groben Schätzungen. Diese Entscheidungen sollen jedoch fundiert auf Daten und Erfahrungen entstehen, wodurch man sowohl Zeit als auch Ressourcen sparen möchte.

Eine zentrale Möglichkeit, diese Datenlücke zu schließen, besteht darin, die Nutzung verschiedener Features systematisch zu erfassen und deren Entwicklung über Zeit zu analysieren. Durch historische Auswertungen lässt sich ableiten, welche Features tatsächlich Mehrwert schaffen, wo Optimierungsbedarf besteht und welche neuen Funktionalitäten entwickelt werden sollten.

Daraus ergibt sich die zentrale Fragestellung:

Wie können wir Metadaten so aufbereiten, dass historische Analysen zur Feature-Nutzung aussagekräftige Erkenntnisse für datenbasierte Entscheidungen im Hinblick auf Entwicklungen liefern?

Dieser Prozess nennt sich analytisches Feature Reporting und orientiert sich methodisch an den vier Hauptpunkten der SAP-Discovery-Strategie:
1. Wer ist der Enduser und welche Aufgaben/Verantwortungen hat er?
2. Welches Problem ist zu lösen und wie bringt dies den User weiter?
3. Welche Funktionen gilt es zu bündeln und was ist die Priorität für SAP und den User?
4. Welche Strategien sind am effektivsten für die Umsetzung?

== Zielsetzung

Das zentrale Ziel meines Projektes ist die Entwicklung eines Systems für ein analytisches Feature Reporting, das durch Metadaten-basierte Versionskontrolle die historische Entwicklung der Feature-Nutzung transparent macht und somit datenbasierte Entscheidungen ermöglicht.

Um dieses Hauptziel zu erreichen, implementiere ich eine automatisierte Datenspeicherung mit Versionskontrolle, die in regelmäßigen Intervallen festgelegte Daten aus Quelltabellen ausliest und in dedizierte Archivtabellen überführt, wobei jede Auslesung eine neue, eindeutig identifizierbare Version erstellt. Diese Tabellen verbinde ich zu einem sogenannten #Star-Schema, mit welchem die Daten nun deutlich übersichtlicher und besser zu analysieren sind.

Darauf aufbauend entwickle ich ein Query-Framework, mit dem man die gespeicherten historischen Daten über vordefinierte Abfragen auslesen kann. Dadurch sollen die Daten, sowohl historisch, als auch über verschiedene Versionen hinweg analysiert und verarbeitet werden können.

Diese Analysen sollen dabei helfen datenbasiert zu entschieden, welche Features und Objekte weiter entwickelt werden sollen und welche eher weniger Aufmerksamkeit benötigen und nur noch bedingt gewartet werden müssen.