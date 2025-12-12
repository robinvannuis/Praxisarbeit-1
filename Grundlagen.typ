#import "utils.typ": codefigure, codefigurefile

#let ABAP = link(<grd:abap>)[ABAP]
#let SQL = link(<grd:sql>)[SQL]
#let CDS = link(<grd:cds>)[CDS]
#let CDS-View = link(<grd:cds_views>)[CDS View]
#let CDS-Views = link(<grd:cds_views>)[CDS Views]
#let CDS-Annotationen = link(<grd:cds_annotations>)[CDS Annotationen]
#let Enumeration = link(<grd:enumeration>)[Enumeration]
#let Metadatentabelle = link(<grd:metadatentabelle>)[Metadatentabelle]
#let Star-Schema = link(<grd:star_schema>)[Star-Schema]
#let Queries = link(<grd:queries>)[Queries]
#let OLAP-Würfel = link(<grd:olap_würfel>)[OLAP-Würfel]
#let Assoziation = link(<grd:assoziation>)[Assoziation]
#let Kardinalitäten = link(<grd:kardinalitaeten>)[Kardinalitäten]

= Grundlagen
== ABAP <grd:abap>
Das @ABAP ist eine von SAP SE entwickelte objektorientierte Programmiersprache, die speziell für betriebswirtschaftliche Anwendungen konzipiert wurde. Ursprünglich stand das Akronym für "Allgemeiner Berichts und Aufbereitungs-Prozessor", wurde jedoch erst 1996 zu "Advanced Business (and) Application Programming" umbenannt, um den erweiterten Funktionsumfang der Sprache zu reflektieren. #cite(<abap_history>)

Die Sprache wurde in den 1980er Jahren entwickelt und ist direkt mit dem SAP-System verbunden. @ABAP ermöglicht es Entwicklern, komplexe betriebswirtschaftliche Anwendungen zu erstellen, die nahtlos in die SAP-Umgebung integriert sind. Als objektorientierte Programmiersprache bietet @ABAP moderne Programmierkonzepte wie Klassen, Vererbung und Polymorphismus. #cite(<abap_keyword_doc>)

Ein besonderes Merkmal von @ABAP ist die enge Integration mit Datenbanksystemen. Die Sprache verfügt über eingebaute zu #SQL ähnliche Befehle (ABAP SQL genannt), die eine effiziente Datenmanipulation ermöglichen. Dies macht @ABAP besonders geeignet für die Entwicklung von Anwendungen, die große Mengen an Datensätzen verarbeiten müssen.

@ABAP wird hauptsächlich in SAP R/3 und SAP S/4HANA Systemen eingesetzt und ermöglicht die Entwicklung verschiedener Anwendungstypen, darunter Reports, Dialoge, Formulare und Web-Services. Die Sprache unterstützt sowohl prozedurale als auch objektorientierte Programmierung und bietet umfangreiche Bibliotheken für typische Geschäftsanwendungen wie Finanzwesen, Personalwesen und Logistik. #cite(<abap_history>)

Ein wichtiger Vorteil von @ABAP ist die plattformunabhängige Ausführung innerhalb der SAP-Umgebung. Programme werden in einem eigenen virtuellen System, dem ABAP Application Server, ausgeführt, was Portabilität zwischen verschiedenen Betriebssystemen und Datenbanken gewährleistet. Darüber hinaus bietet die Entwicklungsumgebung "ABAPDevelopmentTools" integrierte Werkzeuge für Debugging, Performance-Analyse und automatisierte Tests, die die Entwicklung und Wartung von Anwendungen erheblich erleichtern. #cite(<abap_evolution>)

== SQL <grd:sql>

Die @SQL ist die weltweit am besten etablierte und populärste Sprache für Datenabfragen und ist aus der modernen Datenverarbeitung nicht mehr wegzudenken. Die Sprache bietet umfassende Funktionen zum Erstellen, Manipulieren und Verknüpfen von Datenbanken und ermöglicht sowohl einfache Abfragen, als auch komplexe Datenoperationen. Anders als vollständige Programmiersprachen wird @SQL typischerweise als eingebettete Abfragesprache in andere Sprachen wie Python, Java oder C#"#" integriert. Dies ermöglicht Entwicklern, die Stärken beider Welten zu kombinieren: Die logische Programmstruktur der Ausgangssprache und die spezialisierte Datenbankmanipulation durch @SQL. Die meisten Datenbanksysteme, von MySQL bis hin zu PostgreSQL, orientieren sich an einheitlichen SQL-Standards. Diese Standardisierung erhöht nicht nur die Kompatibilität zwischen verschiedenen Systemen, sondern erleichtert auch den Wissenstransfer für Entwickler. #cite(<sql>)

== CDS <grd:cds>

@CDS:pl stellen die zentrale Infrastruktur zur semantischen Datenmodellierung im SAP-System dar. Sie ermöglichen es, Datenmodelle direkt auf der Datenbankebene zu definieren und dadurch Daten effizient, sicher und performant bereitzustellen. Ein zentrales Element der @CDS sind die sogenannten #CDS-Views, mit denen sich Daten aus unterschiedlichen Tabellen logisch zusammenfassen, anreichern und für verschiedene Anwendungsszenarien nutzbar machen lassen.

Im Folgenden werde ich näher auf verschiedene @CDS:pl eingehen und wie diese für mein Projekt relevant sind.

=== CDS Views <grd:cds_views>

CDS Views (Core Data Services Views) sind ein zentraler Bestandteil der #CDS und dienen zur semantischen und logischen Modellierung von Daten im SAP-System. Es handelt sich um virtuelle Datenmodelle, das heißt, die Daten werden nicht physisch gespeichert, sondern nur aus den zugrundeliegenden Datenbanktabellen gelesen.

CDS Views können sowohl im SAP HANA Produkt, als auch im #ABAP#"-System" definiert werden. Ein großer Vorteil ist, dass sie Tabellenbeziehungen und Verknüpfungen direkt in der Definition abbilden, sodass keine manuelle Verknüpfung der Tabellen in Programmen mehr nötig ist.

Ein weiterer wichtiger Aspekt ist die Wiederverwendbarkeit: Ein einmal definierter CDS View kann in mehreren Anwendungen, wie zum Beispiel Fiori-Apps, OData Services, analytischen Berichten oder anderen CDS Views, verwendet werden.

Darüber hinaus bieten CDS Views die Möglichkeit, Annotationen zu nutzen, um zum Beispiel Sicherheitsregeln, UI-Eigenschaften, analytische Kennzahlen oder ODataServices zu definieren. Dadurch stellen sie eine Brücke zwischen der Datenbankebene und der Anwendungsebene dar und bilden die Grundlage für viele moderne S/4HANA-Anwendungen.

=== CDS Annotationen <grd:cds_annotations>

Annotationen an sich sind Anmerkungen oder Vermerke und beim Programmieren werden sie unter anderem als Strukturierungsmittel verwendet. In den @CDS werden Annotationen benutzt um #CDS-Views mit Metadaten anzureichern. Außerdem dienen sie zur semantischen Beschreibung von Entitäten, als Ersatz für Eigenschaften aus der alten SAP GUI und geben Informationen über aufsetzende Entwicklungsframeworks.

Es gibt zwei verschieden Typen von Annotationen, man differenziert zwischen ViewAnnotationen und Element-Annotationen. View-Annotationen sind Vermerke auf ein gesamtes View, wohingegen sich Element-Annotationen nur auf ein einzelnes Element beziehen.

Der Aufbau wie Annotationen im Code angegeben sind folgt einer bestimmten Syntax und wird immer durch ein '@' am Anfang als Annotation gekennzeichnet. Danach kommt der jeweilige Namespace (z.B. UI) und der Name der Annotation (z.B. selectionField). Die einzelnen Elemente werden immer durch einen Punkt getrennt und die Werte sind nach einem Doppelpunkt anzugeben. Zeichenketten werden in einfache Anführungszeichen gesetzt, während sogenannte #Enumeration#"en" mit einem Hash-Zeichen gekennzeichnet sind, zum Beispiel #"#STANDARD". #cite(<cds_annotations>)

Die Struktur eine Annotation lässt sich konkret wie folgt darstellen:
#codefigure(caption: "Annotationsstruktur")[```cds
@<Namespace>.<Name>: <Wert>
```]
Ein Beispiel für eine Annotation könnte so aussehen:
#codefigure(caption: "Beispiel einer Annotation")[```cds
@UI.selectionField: [{ position: 10 }]
```]

==== Enumeration <grd:enumeration>
Enumerationen in CDS definieren einen eigenen, stark typisierten Datentyp mit einem festgelegten Satz zulässiger Werte. Eine Enumeration besteht aus einer Liste benannter Konstanten, die jeweils einem technischen Wert zugeordnet sind. Felder, die diesen Enumerations-Typ verwenden, dürfen ausschließlich diese definierten Werte annehmen.

Dies erhöht die Qualität der Daten und erleichtert Validierungen, da nur gültige, klar benannte Werte gespeichert werden können. Sie werden typischerweise in Statusfeldern, Kategorien oder Klassifikationen benutzt und werden mit der folgenden Syntax referenziert: #"#BEISPIELENUMERATION"

== Metadatentabelle <grd:metadatentabelle>

Eine Metadatentabelle ist eine Datenstruktur, die Metadaten - also sozusagen "Daten über Daten" - speichern. In ihr wird unter anderem die Herkunft, Bedeutung und Art von verschiedensten Daten gespeichert. Sie ist dafür da eine nachvollziehbare Beschreibung und Steuerung von Datenmodellen bereitzustellen, sodass man diese interpretieren, wiederverwenden, oder automatisieren kann. Außerdem kann man mithilfe von Metadatentabellen genau nachvollziehen was die Daten machen und wozu sie benutzt werden.

== Star-Schema <grd:star_schema>

Ein Star-Schema ist ein mehrdimensionales Datenmodell, in dem Daten in einer Datenbank so verknüpft und organisiert werden können, dass sie besser zu verstehen und analysieren sind. Man kann Star-Schemas auf viele verschiedene Datenmodelle, wie "Data Warehouses", Datenbanken oder "Data Marts" anwenden. Sie eignen sich außerdem sehr gut zur Verarbeitung von großen Datenmengen, da sie speziell auf diese optimiert sind.

In der Mitte eines Star-Schemas findet man eine einzelne "Faktentabelle", die verschiedene Daten aus den anderen Tabellen enthält. Sie ist mit den anderen Tabellen, durch verschiedenste Fremdschlüssel, wie zum Beispiel Zeitpunkte oder andere "Dimensionen", verknüpft. Dadurch kann es dazu kommen, dass Daten denormalisiert werden, das heißt einige Tabellen werden mit redundanten Spalten erweitert. Auch wenn dies ein wenig kontraproduktiv wirkt, hilft es dabei die Abfragen und Bearbeitungszeit zu verkürzen. Außerdem sisnd Star-Schema nicht so sehr auf Joins zwischen den einzelnen Tabellen angewiesen und eignen sich somit besser für einfache Datenabfragen. Die Struktur von Star-Schemata ist des weiteren sehr leicht zu verstehen und ermöglichen dem Enduser ein einfaches Auffinden benötigter Daten.

== Queries <grd:queries>
In der Datenanalyse werden sogenannte Queries (Singular: Query) genutzt, um Daten anhand bestimmter Kriterien auszulesen. Sie erstellen Listenausgaben von Datenbanken gruppiert auf Daten wie zum Beispiel Zeit- oder Preisspannen. Darüber hinaus kann man Queries gut benutzen, um Trends, Muster oder Änderungen in den Daten zu erkennen. Innerhalb von SAP können Queries auch ohne besondere Programmierkenntnisse erstellt werden und lassen sich für viele verschiedene Arten von Berichten wie Grundlisten, Statistiken oder Ranglisten nutzen. Man kann sie außerdem in andere Systeme exportieren oder aus diesen importieren. Queries sind ein Teil der #CDS und werden als "transient-view-entity" definiert. #cite(<cds_queries>)

Die Definition eines Queries kann zum Beispiel so aussehen:

#codefigure(caption: "Beispiel Query")[```cds
EndUserText.label: 'Example Query'
@AccessControl.authorizationCheck: #NOT_ALLOWED
define transient view entity example_query
  provider contract analytical_query
  as projection on example_view
{
  @Enduser.label: 'Field1 caption'
  Field1,
  Field2,
  @AnalyticsDetails.query.axis: #COLUMNS
  Field3
}
```]


Die Felder die in einer Query angegeben sind, müssen vorher auch innerhalb der View implementiert worden sein. Wenn ein Feld innerhalb des Views als "Measure" (Kennzahl) definiert wurde, kann dies in der graphischen Oberfläche unter einer gesonderten Auswahl "Measures" ausgewählt werden. Dadurch lassen sich zum Beispiel mehrere Zähler auf einer Achse der Tabelle anzeigen und miteineander vergleichen. Diese Kennzahlen lassen sich ganz einfach in der Query Definition erstellen, ohne die darunterliegende Queries verändern zu müssen.

Als Gegenstück zu den Kennzahlen gibt es Dimensionen, in denen sich die Attribute anzeigen und gruppieren lassen. Die Dimensionen lassen sich unterschiedlich anordnen und zeigen so die einzelnen Kennzahlen in Abhängingkeit von verschiedenen Faktoren.

== OLAP Würfel <grd:olap_würfel>
Ein OLAP Würfel auch Feature-Cube genannt, ist ein logisches Datenmodell zur Darstellung von Daten. Im SAP Kontext ähnelt es vom Aufbau und der Definition her einer #CDS-View. In ihm werden Daten wie Elemente in einem mehrdimensionalen Würfel angeordnet. Die einzelnen Dimensionen erlauben einen geordneten Zugriff auf die Daten und erlauben für Operationen wie zum Beispiel "Slicing" (dem Ausschneiden von Scheiben aus dem Würfel), "Dicing" (dem Erstellen eines kleineren Würfels durch Teileinschränkungen auf einer oder mehreren Dimensionen), "Drill-Down" (dem "Hereinzoome" an detailiertere Werte, oder "Roll-Up" (dem Gegenteil des "Drill-Downs", es wird "herausgezoomt" um auf eine höhere Hierarchiestufe zu verdichten).

Es gibt noch viele weitere Operationen die häufig in Cubes benutzt werden, die für mein Projekt jedoch nicht relevant sind.

== Assoziation <grd:assoziation>
Eine Assoziation (zu Englisch: association) ist eine Möglichkeit in ABAP #CDS Tabellen anhand von gemeinsamen Feldern zu verknüpfen. Sie ähnelt im Kern einer "Join"-Verknüpfung und kann verschiedene #Kardinalitäten haben. Man definiert eine Assoziation, indem man eine Zieltabelle, eine Kardinalität und Felder angibt, anhand von denen die Tabellen verknüpft werden sollen. Dadurch wird den einzelnen Zeilen in Tabelle A eine bestimmte Anzahl aus Wert von Tabelle B zugeordnet. Im Normalfall entspricht eine Assoziation einem "left-outer-join", das heißt es werden alle Zeilen aus Tabelle A und die Zeilen aus Tabelle B, die den Fremdschlüssel aus A implementieren, zurückgegeben. Falls eine Zeile aus Tabelle B keinen übereinstimmenden Wert für einen Datensatz aus Tabelle A hat, werden die Spalten aus Tabelle B auf "null" gesetzt. Angegeben wird eine Assoziation nach folgendem Schema:

#codefigure(caption: "Beispiel Assoziation")[```cds
association [Kardinalität] to Zieltabelle as Alias
  on Quelle.Feld1 = Ziel.Feld2
  and Quelle.Feld3 = Ziel.Feld4
```]


== Kardinalitäten <grd:kardinalitaeten>
Eine Kardinalität gibt an wie Fremdschlüssel die Zeilen zwischen Tabellen verknüpfen und kann die Formen eins-zu-eins (1 : 1), eins-zu-viele (1 : n) oder viele-zu-viele (n : m) haben. Durch Kardinalitäten können effizienter Abfragen auf Datenbanken erstellt werden.

Eins-zu-eins (1 : 1): Eine Zeile in Tabelle A kann mit maximal eine Zeile aus Tabelle B verknüpft werden und anders herum.

Eins-zu-viele (1 : n): Eine Zeile in Tabelle A kann mit beliebig vielen Zeilen aus Tabelle B verknüpft werden. Eine Zeile in Tabelle B kann jedoch nur mit genau einer Zeile aus Tabelle A verknüpft werden.

Viele-zu-viele (n : m): Eine Zeile in Tabelle A kann mit beliebig vielen Zeilen aus Tabelle B verknüpft werden und anders herum.

In CDS-Views gibt es zwei verschiedene Syntax um Kardinalitäten anzugeben. 

Bei der ersten wird nur die "Zielkardinalität" angegeben und gibt einen minimalen und einen maximalen Wert an, die diese annehemen kann. Eine eins-zu-eins Kardinalität kann dort mehrere Formen annehmen und sieht entweder so [1..1] oder so [1] aus. [1..1] steht in dem Falle dafür, dass jeder Zeile von Tabelle A mindestens eine, aber auch maximal eine Zeile von Tabelle B zugewiesen werden kann. Eine eins-zu-viele Beziehung würde in CDS entweder so [1..#"*"] oder so [#"*"] aussehen. Eine Kardinalität anzugeben ist optional und kann ausgelassen werden. Der Standardwert in so einem Fall ist (1 : n).

Bei der zweiten (neueren) Syntax werden sowohl die Quell- als auch die Zielkardinalität angegeben. Diese Syntax sieht dann zum Beispiel so aus: "OF EXACT ONE TO EXACT ONE" für eine eins-zu-eins Beziehung, "OF EXACT ONE TO MANY" für eine eins-zu-viele Beziehung und "OF MANY TO MANY" für eine viele-zu-viele Beziehung. #cite(<cds_cardinality>)