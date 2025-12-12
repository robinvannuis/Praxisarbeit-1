// LTeX: enabled=false
#import "template.typ": caption_with_source, project
#import "@preview/codly:1.3.0": *
#import "utils.typ": codefigure, codefigurefile
#show: codly-init

#import "@preview/codly-languages:0.1.10": *
#codly(languages: codly-languages)

#show: project.with(
  // 'de' for german or 'en' for english.
  // Pull Requests for other languages are welcome! :)
  lang: "de",

  // Shows a line for a signature, if set to `false`,
  is_digital: true,

  // Display the confidentiality clause
  confidentiality_clause: true,

  ///
  /// Cover Sheet
  ///
  // Full title of your project
  title_long: "Analytisches Feature Reporting mit integrierter Versionskontrolle auf Basis von Metadaten",
  // Shortened title, which is displayed in the header on every page
  title_short: "Analytisches Feature Reporting",
  // The type of your project
  thesis_type: "Projektarbeit 1 (T3_1000)",
  // Other information about you and your project
  firstname: "Robin",
  lastname: "van Nuis",
  signature_place: "Karlsruhe",
  matriculation_number: "8771293",
  course: "TINF25B2",
  submission_date: "31. Dezember 2025",
  processing_period: "20.10.2025 - 31.12.2025",
  supervisor_company: "Kolja Groß",
  supervisor_university: "Prof. Dr. Sebastian Ritterbusch",

  // Disable the abstract by changing to `abstract: ()`
  // To load the abstract from a file use include("abstract.typ") instead of [...]`
  // If you only want one language, leave the comma at the end -> `("de", "Deutsch", []),` its necessary for syntax of the template
  abstract: (
    ("de", "Deutsch", include "abstracts/abstract_german.typ"),
    ("en", "English", include "abstracts/abstract_english.typ"),
  ),

  // Path/s to references - either .yaml or .bib files
  // * for `.yaml` files see: [hayagriva](https://github.com/typst/hayagriva)
  library_paths: "library.bib",

  // Specify acronyms here.
  // The key is used to reference the acronym.
  // The short form is used every time and the long form is used
  // additionally the first time you reference the acronym.
  acronyms: (
    (key: "CDS", short: "CDS", long: "Core-Data-Service"),
    (key: "ABAP", short: "ABAP", long: "Advanced Business (and) Application Program"),
    (key: "SQL", short: "SQL", long: "Standard Query Language"),
    (key: "ERP", short: "ERP", long: "Enterprise Resource Planning"),
    (key: "UUID", short: "UUID", long: "Universally Unique Identifier"),
    (key: "MB", short: "MB", long: "Mega-Byte"),
    (key: "GB", short: "GB", long: "Giga-Byte"),
  ),
)

// LTeX: enabled=true
// You can now start writing :)
#let acro-long(key) = acronyms.find(a => a.key == key).long
#let acro-short(key) = acronyms.find(a => a.key == key).short
#let CDS = link(<grd:cds>)[CDS]
#let CDS-View = link(<grd:cds_views>)[CDS-View]
#let Metadatentabelle = link(<grd:metadatentabelle>)[Metadatentabelle]
#let Queries = link(<grd:queries>)[Queries]
#let Star-Schema = link(<grd:star_schema>)[Star-Schema]
#let OLAP-Würfel = link(<grd:olap_würfel>)[OLAP-Würfel]
#let Assoziation = link(<grd:assoziation>)[Assoziation]

#include "Einleitung.typ"
#include "Grundlagen.typ"




= Bisheriger Stand

Grundlage des Projektes ist sind die verschiedenen - bereits existierenden - Tabellen im internen System. Aus diesen #Metadatentabelle#"n" stammen die Daten, die es gilt zu verarbeiten und zu analysieren. Des weiteren existieren bereits zwei #CDS-View#"s", die die Daten analysieren. Diese beziehen sich jedoch nur auf Laufzeitanalysen und nicht auf die - für mein Projekt benötigten - Veränderungen jener Daten und sind somit nicht relevant für meine Arbeit.

Die Tabellen auf die ich im Folgenden zugreifen werde speichern Informationen über verschiedene Objekte / Teilobjekte und die Beziehungen zwischen den Objekten. Es gibt verschiedene Arten von Tabellen, wovon die meisten unterschiedlich aufgebaut sind und verschiedenste Aufgaben haben. Daher muss ich für jede Tabelle verschiedene eigene Tabellen zum Speichern der Daten erstellen. Auch die Selektion der Daten muss für jede Tabelle leicht angepasst werden.

Das Problem bei den Daten liegt darin, dass alle bereits existierenden Tabellen nur Momentanaufnahmen speichern. Das heißt, ich kann immer nur auf die aktuellsten Daten zugreifen und nicht auf jene aus vorherigen "Versionen". Da es ein Ziel des Projektes ist, Trends in den Daten zu erkennen und zu analysieren, reichen die bisherigen Tabellen nicht aus.

Aus diesem Grund baue ich mir eigene Tabellen um die Daten historisch zu speichern. Jedoch können meine Tabellen auch kein genaues Abbild der bisherigen Tabellen sein, da die Datenmenge sonst deutlich zu groß werden. Als Beispiel: Eine Tabelle, die ursprünglicherweise 100 @MB groß war, erstellt nach einem monatlichen Abruf und Speichern der Daten über 10 Jahre, eine Tabelle die nun 240.000 @MB groß ist, oder auch 240 @GB.


= Implementierung

== Feature Reporting

=== Sammeln der historischen Daten

Der erste Teil des Feature Reportings ist es, die Daten historisch zu sammeln und zu speichern. Dafür benötigt sind die oben angesprochenen - bereits existierenden - Tabellen. Es existieren nahezu unendlich viele verschiedene Tabellen. Daher musste ich als erstes festlegen, welche Tabellen gebraucht werden. Da ich das Feature Reporting für SAP interne Daten erstelle, basiert die Auswahl der Tabellen auf Objekten, die in den Teams rund um @ABAP genutzt werden. Das sind die Teams: "ABAP CDS 1", "ABAP CDS 2" und "ABAP DDIC (Data-Dictionary)". Aus dieser Information lässt sich jedoch noch nicht schließen, welche Objekttypen den Teams gehören. Um daran zu kommen, gibt es Listen mit den Usernamen der Mitglieder der Teams, welche ich mit den Autoren der Objekttypen, verglichen habe. Der erste Schritt von diesem Prozess ist es die verschiedenen Mitglieder der Teams zu einer Liste zusammenzufassen. Es gibt bereits einzelne Mitgliederlisten der drei Teams, welche ich nun zu einer einzelnen "member_list" zusammenfasse:
#codefigure(caption: "Mitgliederliste füllen", reference: "fill_members")[```cds
method fill_members.
  member_list = get_members_of( co_team-dictionary ).
  append lines of get_members_of( co_team-cds1 ) to member_list.
  append lines of get_members_of( co_team-cds2 ) to member_list.
endmethod.
```]
Da ich nun die Mitgliederlisten zusammengefasst habe, kann ich diese mit den Autoren der Objekte vergleichen um an die Liste von Objekttypen zu kommen, die zu diesen Teams gehören. Dazu kommen noch ein paar weitere Datentypen, die nicht speziell zu den Teams gehören, jedoch trotzdem wichtig für das Projekt sind. Diese füge ich im Nachhinein zur Liste von Objekten hinzu.
#codefigure(caption: "Objekttypen speichern", reference: "rep_object_list")[```cds
method get_objecttypes.
  select object_types
  from tadir
  inner join @team_members as members on tadir~author = members~table_line
  where tadir~object = 'SVAL'
  into table @data(object_type_names).

  append 'ABL' to object_type_names.
  append 'DDLS' to object_type_names.
  append 'DRAS' to object_type_names.
  append 'DSFD' to object_type_names.
  append 'DSFI' to object_type_names.
  append 'DRTY' to object_type_names.
  append 'TABL' to object_type_names.

  return object_type_names.
endmethod.
```]
Diese dadurch ermittelten Objekttypen speichere ich in einer von mir erstellten Tabellen. Anhand von dieser Tabelle erstelle ich später ein #CDS-View, um die gesamten Daten zu verknüpfen.

Außerdem gibt es für jeden der dadurch ermittelten Objekttypen weitere #Metadatentabelle#"n", welche die einzelnen Objekttypen näher beschreiben. Meist ist dies eine sogenannte "Header" Tabelle und eine "Fields" Tabelle. In einer "Header" Tabelle werden grundlegende Informationen über die Objekttypen gespeichert, zum Beispiel das originale System, der Ersteller, oder der Objektname im Objekteverzeichnis. In einer "Fields" Tabelle findet man alle möglichen Informationen über die einzelnen Felder eines Objektes, zum Beispiel der Name des zugehörigen Objektes, ob das Feld ein Schlüsselattribut ist, oder den Datentyp.

Da diese Tabellen auch viele - für mein Projekt irrelevante - Informationen enthalten, musste zunächst auch noch festgelegt werden, welche Spalten und Datensätze überhaupt nützlich sind und gespeichert werden sollten. Diese Auswahl basiert vor allem auf der Überprüfung der einzelnen Felder. Es gibt viele historische Felder, die mittlerweile redundant sind oder nicht mehr genutzt werden. Diese beiden Arten von Felder sind demnach nicht mehr relevant, sodass dafür keine Ressourcen zum Speichern aufgewendet werden sollten.

=== Speichern der Daten in eigenen Tabellen <main:speicherung>

Da die gespeicherten Daten im Nachhinein historisch nachvollziehbar sein sollen, muss es einen Informationssatz geben, der speichert wann und von wem die Daten gespeichert wurden. Um dies zu erfüllen habe ich eine eigene Tabelle zum Speichern von Versionen erstellt. Diese Tabelle enthält die Felder: "versionID", "saved_on", "author". "VersionID" ist das Schlüsselattribut vom Typ "char" und ist maximal 20 Zeichen lang. "Saved_on" ist vom Typ "timestamp" und speichert den - bis auf die Sekunde genauen - Zeitpunkt des Abruf. "Author" ist vom gleichnamigen Typ "author" und speichert den SAP internen Username der Person, die den Datenabruf durchgeführt hat. Diese Tabelle rufe ich in den weiteren Tabellen, durch eine Referenz auf das Schlüsselattribut "versionid" auf.
#codefigure(caption: "Versionstabelle", reference: "versiontable")[```cds
  key versionid : versionid not null;
  saved_on      : timestamp;
  author        : author;
```]

Die Daten werden in meinen eigenen Tabellen gespeichert, welche ich auf Basis der bisher vorhandenen Tabellen gebaut habe, jedoch habe ich einige der - für mein Projekt irrelevanten - Spalten zum Sparen von Ressourcen ausgelassen.
Als Beispiel ist hier der Datenabruf aus einer der bereits existierenden Tabelle mit den Feldern:
#codefigure(caption: "Bereits existierende Tabelle", reference: "dddras_header")[```cds
key aspect_name : dd_dras_name not null;
key as4local    : as4local not null;
as4user         : as4user;
as4date         : as4date;
as4time         : as4time;
aspect_name_raw : dd_dras_name_raw;
```]
Diese Daten werden in einer zugehörigen - von mir erstellten - Tabelle, mit den folgenden Feldern, gespeichert:
#codefigure(caption: "Meine zugehörige Tabelle", reference: "zrvn_dddras_head")[```cds
key uuid        : sysuuid_c32 not null;
key aspect_name : dd_dras_name not null;
as4user         : as4user;
as4date         : as4date;
as4time         : as4time;
aspect_name_raw : dd_dras_name_raw;
version         : versionid;
```]
Das Feld "as4local" aus der Originaltabelle ist für mich nur zum Teil relevant. Es speichert den Aktivierungsstatus von einzelnen Objekten. Für mich relevant sind nur aktive Objekte, weshalb ich die Daten so filtere, dass nur Daten für as4local = #"'A'" gespeichert werden. Da jeder Eintrag nun as4local = #"'A'" enthält, brauche ich den Wert des Feldes nicht mehr in meiner Tabelle speichern.

Die neu dazugekommenen Felder "uuid" und "version" weise ich bei jeder Abspeicherung zu. Der @UUID wird durch einen Abruf einer externen Methode erstellt. Dies erstellt einen 16 Byte @UUID im Hex Format.
#codefigure(caption: "UUID erstellen", reference: "create_uuid")[```cds
uuid = cl_system_uuid=>create_uuid_c32_static( ).
```]

Das Feld "version" soll dazu dienen das historische Speichern der Daten nachvollziehen zu können. Dafür erstelle ich mit folgender Methode jedes Mal, wenn die Daten neu abgerufen und gespeichert werden, einen neuen Eintrag in der "versiontable".
#codefigure(caption: "Neue Version erstellen", reference: "new_version")[```cds
method new_version.
  select versionid from versiontable
  into @data(current_version)
  order by saved_on ascending.
  endselect.

  if current_version is initial.
    curr_version = 1.
  else.
    curr_version = current_version + 1.
  endif.

  data(lv_date) = sy-datum.
  data(lv_time) = sy-uzeit.

  convert date lv_date time lv_time into time stamp data(lv_ts) time zone sy-zonlo.
  append value #( versionid = curr_version
                  saved_on = lv_ts
                  author = sy-uname  ) to version_table.

  insert versiontable from table @version_table.
  return curr_version.
endmethod.
```]

Der Datenabruf geschieht über eine Methode "pull_data", welche unter anderem die oben genannte Methode "new_version" (#ref(<new_version>)) aufruft. Außerdem gibt es für jede Tabelle eine seperate Methode, die die Daten abruft und in meiner dazugehörigen Tabelle speichert. Diese werden auch durch die "pull_data" Methode aufgerufen.

Hier ist zur Veranschaulichung eine Methode, die die Daten aus der oben genannten Tabelle zieht und in meiner zugehörigen Tabelle speichert:
#codefigure(caption: "Daten selektieren", reference: "dddras_header_select")[```cds
select  aspect_name,
        as4user,
        as4date,
        as4time,
        aspect_name_raw,
        @current_version as version,
        @( cl_system_uuid=>create_uuid_c32_static( ) ) as uuid
from dddras_header
where as4local = 'A'
into corresponding fields of table @lt_dddras_header_d.

insert zrvn_dddras_head from table @lt_dddras_header_d.
```]
In dem select-Statement werden die einzelnen Felder - die ich speichern möchte - definiert. Außerdem wird der Methode ein Wert "current_version" als Übergabeparameter übergeben. Dieser wird durch das select-Statement in dem Feld "version" gespeichert. Das Feld "uuid" wird über den Methodenaufruf erstellt und gespeichert, den ich bereits hier (#ref(<create_uuid>)) erklärt habe. Danach werden die Daten erst in einer lokalen Tabelle gespeichert - erkennbar durch "lt#"_"" vor dem Bezeichner - und danach erst in meiner zugehörige Tabelle eingefügt.

Für die Analyse brauche ich in verschiedenen Tabellen noch Zähler, die zum Beispiel die Anzahl an genutzten Funktionen zählen. Da man diese nicht direkt in der Definition einer Tabelle erstellen kann, benötigt jede Tabelle noch eine "Dimension-View" welche "über" der Tabelle liegt. Diese dient sozusagen als "Abbild" der Tabelle und ermöglicht die Implementierung von Aggregationen, wie zum Beispiel einem Zähler.

== Datenanalyse

=== Aufbau des Datenmodell

Um die genauere Speicherung und Verarbeitung der Daten nachzuvollziehen, ist es nötig zu verstehen wie genau die Tabellen aufgebaut sind. Das gesamte Tabellenschema basiert auf einer Art #OLAP-Würfel, einem mehrdimensionalen relationalen Datenmodell. Als sogenannte Hauptdimension liegt meine Versionstabelle, die die Version des Datenabrufes speichert. Da jeder Datenaufruf anhand derselben Kriterien geschieht, kann man grundsätzlich beschreiben, wie die weiteren Dimensionen aussehen werden. Als nächstes gibt es die Daten aus der "tadir"-Tabelle, welche  alle Objekttypen enthält, die innerhalb des SAP-Systems existieren und genutzt werden. Diese Objekttypen an sich bilden eine weitere Dimension. Jeder dieser Objekttypen enthält verschiedene Objekte, die durch weitere #Metadatentabelle#"n" näher beschrieben werden. Diese Tabellen werden "Header"- und "Field"-Tabellen genannt und bilden eine zusätzliche Dimension. Die Felder der Metadatentabellen speichern verschiedene Informationen über die Objekte, anhand von welchen ich die Tabellen weiter verknüpfen kann. Verknüpfungen und Vergleiche innerhalb einer Dimension funktionieren ohne Probleme und lassen sich ohne weitere Komplikationen durchführen. Für einige Abfragen und Aufrufe, muss ich jedoch Informationen aus verschiedenen Dimensionen miteineander verknüpfen. Da dies nicht mehr über einfache Abfragen geht, muss ich einen Teil der Informationen von der einen Dimension in eine andere verschieben. Dies funktioniert, indem ich beide Informationspaare in einer "Dimension-View" zusammenfasse. Diese erstelle ich innerhalb der gewünschten Dimension und kann sie innerhalb des OLAP-Würfels aufrufen. Dadurch lassen sich beispielsweise Funktionsnamen mit denen der skalaren Funktionen vergleichen, um zu überprüfen wo die Schnittmenge liegt und wie viele Elemente sie enthält.

=== OLAP-Würfel erstellen

Um die Daten in dem OLAP-Würfel organisiert zu speichern und verarbeiten zu können, muss ich diesen erst einmal erstellen. In diesem Würfel binde ich alle von meinen Tabellen ein und verknüpfe diese über die gemeinsamen Attribute Objekttyp, Objektname, der Version. Durch diese Verknüpfungen können die Daten aus den verschiedenen Tabellen den zugehörigen Daten aus den anderen Tabellen zugeordnet werden. Als Basis für den #OLAP-Würfel nutzte ich die Daten, die ich in meiner obersten "Header"-Tabelle speichere. Das sind die Obejkttypen und die zugehörigen Objektnamen. Da es für die einzelnen Objekttypen mehrere Tabellen gibt, die diese weiter beschreiben, binde ich sie durch sogenannte #Assoziation#"en" an und lasse mir die Felder anzeigen.

Eine solche #Assoziation kann zum Beispiel so ausssehen:
#codefigure(caption: "Anbindung der Headertabelle für 'DRAS'", reference: "association_dddras_head")[```cds
association [*] to dras_header as _DrasHead
  on  $projection.Version    = _DrasHead.version
  and $projection.ObjectType = 'DRAS'
  and $projection.ObjectName = _DrasHead.aspect_name
```]
Als erstes Verknüpfungskriterium benutze ich die Version die ich mir in @new_version erstelle. Als nächstes Filtere ich die Daten aus der "Headertabelle" anhand des Obejkttypen, sodass nur noch Datensätze vom Type 'DRAS' gelesen werden. Das letzte Filterkriterium ist der Name der untergeordneten Objekte, die mit den Aspektnamen aus der 'DRAS'-Tabelle übereinstimmen müssen. Durch diese Abfragen verknüpfe ich die Tabellen in den verschiedenen Dimensionen und kann so die Informationen leichter abrufen. Alle Assoziationen in meinem Feature-Cube basieren auf diesem Schema.

Im unteren Teil des OLAP-Würfel werden die Felder angegeben, die ich aufrufen und benutzten möchte. Diese Auswahl wird erst später nach der Definition der Queries vollständig, da ich erst dann genau weiß, welche Felder benötigt werden. Felder die ich in jedem Fall brauche sind die Key-Felder der assoziierten Tabellen. Außerdem muss jede Assoziation selbst in der Liste der Felder angegeben werden.

=== Queries definieren

Um die von mir gespeicherten Daten nun auch besser analysieren zu können benötige ich sogenannte #Queries. Bei der Definition der #Queries habe ich mich zum Start an jenen orientiert, die von einem vorherigen Studenten erstellt wurden. Sie haben alle verschieden "Aufgaben" und implementieren daher verschiedene Felder. Die meisten Queries bauen auf der Zählung von verschiedene Informationen auf, weshalb es von Nöten war in einigen der Tabellen Zählervariablen für verschiedene Elemente einzubauen. Es gibt zum Beispiel einen Zähler für Funktionen, den man später gruppieren kann und somit die Anzahl an Funktionen für ein bestimmten Quelltyp sich anzeigen lassen kann. Außerdem kann man sich die Ergebnisse nach Datum/ Zeitpunkt der Speicherung gruppieren lassen und so Trends in den Daten visualisieren.

Das erste von mir definierte Query dient dazu die Anzahl der genutzten Assoziationen, in Abhängigkeit von verschiedenen Faktoren wie der Version, dem Paket oder der Software-Komponente anzeigen und kalkulieren zu lassen.

Der nächste von mir definierte Query funktioniert ähnlich wie der ersten, außer dass er die Anzahl der Elemente in Abhängigkeit von der Version, dem Paket oder zum Beispiel der Software-Komponente anzeigt.

Alle weiteren Queries funktionieren sehr ähnlich zu den beiden bisher beschriebenen, analysieren jedoch unterschiedliche Werte. Ein letzter Query auf den ich eingehen möchte dient dazu die Anzahl der Funktionen zu bestimmen. Außerdem bestimmt der Query - in Abhängigkeit zu bestimmten Kriterien - die Anzahl der Funktionen, die Anzahl der skalaren Funktionen und den entsprechenden Anteil an skalaren Funktionen.

Die Ergebnisse der Queries kann man sich in zwei möglichen Formen anzeigen lassen, in Tabellenform und in einem Balkendiagramm, wodurch man verschieden Blickrichtungen bekommt und die Daten unterschiedlich analysieren kann. 

== Auswertungen

Für die Auswertung der Daten habe ich mir verschiedene Versionen der Daten erstellt und gespeichert. Aufgrund von Problemen mit der Verarbeitung der Queries, musste ich die Anzahl der Datensätze limitieren, weshalb ich nur Daten von ca. 10000 Objekten speichern konnte. Dies hat natürlich Auswirkungen auf die Qualität der Auswertung, da sich die Datenmenge bei jedem Datenabruf verändert und unterschiedliche Ausschnitte aus dem gesamten Datensatz gespeichert werden. Außerdem gab es einige Probleme mit dem internen System XT7, weshalb ich all meine erstellten Objekte (Tabellen, Views, Queries) in das System ODE übertragen musste. Dadurch sind alle bis zu dem Zeitpunkt der Übertragung erstellten Daten verloren gegangen und die nun von mir erstellten Versionen wurden nicht im - wie ursprünglicherweise geplant - wöchentlichen Abstand erstellt, sondern in einem Abstand von wenigen Stunden. Dies hat natürlich auch Auswirkungen auf die Qualität der Auswertung, da sich in so einem kurzen Zeitraum kaum Veränderungen in den Daten ergeben.

Dennoch lassen sich aus den erstellten Versionen einige Trends erkennen:

=== Query für Assoziationen
Es lässt sich zum Beispiel erkennen, dass die Anzahl der Assoziationen mit jeder neuen Version leicht ansteigt. Dies lässt sich dadurch erklären, dass mit der Zeit immer mehr Objekte erstellt werden, die wiederum Assoziationen benötigen um in Cube-Views - also #OLAP-Würfel#"n" - eingebunden zu werden.

Zwischen den Versionen 1 und 2 lagen ca. 2 Stunden Zeit, weshalb in dieser zeit nur sehr wenige Objekte und entsprechend Assoziationen hinzugekommen sind. Die Veränderungen hängen auch sehr stark von den Arbeitszeiten der Entwickler ab, weshalb es in manchen Versionen kaum Veränderungen gibt, während es in anderen Versionen wieder mehr sind. So gab es beispielsweise zwischen Version 1 und 2 in fast allen Source Typen einen kleinen Anstieg an genutzten Assoziationen, während zwischen den Versionen 2 und 3 in manchen Source Typen sogar ein leichter Rückgang zu verzeichnen war.

Interessant ist jedoch zu sehen, dass es in Version 4 nur extrem geringe Veränderungen in den Daten gab, im Vergleich zu Version 3. Am "Time-Stamp" der Speicherung lässt sich erkennen, dass die Version 4 erst am nächsten Tag erstellt wurde. Demnach ist es verwunderlihc, dass es in der Zeit keine Veränderungen gab. Es lässt sich jedoch dadurch erklären, dass das System "ODE", in welchem die Daten gespeichert und abgerufen werden, lediglich ein Test-System ist, in dem in der Nacht nur wenige Entwickler an den Objekten arbeiten und somit auch kaum neue Objekte oder Assoziationen erstellt wurden.

Siehe folgende Grafik:

#figure(
  image("/assets/Assoziation Query.png", width: 70%),
  caption: [Ergebnisse der Assoziations-Query],
)

Die Veränderungen an der Anzahl der Assoziations hängt außerdem sehr stark von der Software-Komponente ab. In manchen Komponenten gibt es einen deutlichen Anstieg an genutzten Assoziationen, während in anderen Komponenten kaum Veränderungen zu verzeichnen sind.

=== Query für Funktionen und durchschnittliche Anzahl an Funktionen

Auch bei der Anzahl der Funktionnen gibt es einige Veränderungen. Die unterscheiden sich jedoch stark von den Veränderungen bei den Assoziationen. Während es bei den Assoziationen meist eher kleinere Veränderungen in den Daten gab, gibt es bei den Funktionen teils deutliche Sprünge. So steigt die Anzahl der Funktionen aus dem Source Type "W" zum Beispiel von 1.612 Elementen in der ersten Version auf 2.172 Elementen in der zweiten Version und 4.140 Elementen in der dritten Version an.

Spannend zu sehen ist jedoch das sich die für diesen Source Type die durchschnittliche Anzahl an Funktionen pro Enität nur marginal von 3,558 auf 4,052 und 4,059 ändert. Dies lässt darauf schließen, dass in der Zeit zwischen den Versionen vor allem neue Enitäten erstellt wurden, die jedoch im Durchschnitt eine ähnliche Anzahl an Funktionen besitzen wie die bereits existierenden Enitäten.

Bei dem Source Type "P" ist die Veränderung der Anzahl an Funktionen sowol in der absoluten Anzahl als auch im Durchschnitt pro Enität extremer.

Siehe Abbildung:
#figure(
  image("/assets/Average Function Query.png", width: 100%),
  caption: [Ergebnisse des Average-Function-Query],
)

Hier kann man sogar erkennen, dass es keine Veränderungen in den Anzahlen der Funktionen zwischen der dritten und vierten Version gab.

=== Query für skalare Funktionen

In der Analyse der Anzahl der Funktionen und der skalaren Funktionen lassen sich in den ersten drei Versionen nur sehr wenige Veränderungen erkennen. Dies liegt daran, dass in dem kurzen Zeitraum nur wenig neue skalare Funktionen erstellt wurden. Lediglich im Source Type "W" unter der Software-Komponente "SAP_Basis" gibt es einen deutlichen Anstieg an Funktionen zwischen der zweiten und der dritten Version. Dies lässt sich vor allem auf die Art des Datenabrufes zurückführen, da ich die Anzahl der Datensätze aus Speicher- und Verarbeitungsgründen auf 10.000 Objekte limitieren musste. Das kann dazu führen, dass bei den unterschiedlichen Versionen, verschiedene Teilmengen aus den ursprünglichen Daten abgezogen werden. Hätte ich die Daten über einen längeren Zeitraum und mit einer größeren Datenmenge abrufen können, wären die Ergebnisse vermutlich aussagekräftiger gewesen.

#figure(
  image("/assets/Scalar Function Query Results.png", width: 100%),
  caption: [Ergebnisse des Scalar-Function-Query],
)

Auch hier ist zu erkennen, dass es in der vierten Version keine Veränderungen, im Vergleich zur dritten Version, gab.

=== Query für die Anzahl der Elemente

Im Gegensatz zu den vorherigen Queries, gibt es bei der Anzahl der Elemente in den verschiedenen Source Typen und Software-Komponenten keine Veränderungen. Das heißt es sind keine neuen Elemente hinzugekommen oder entfernt worden. Dies lässt sich vorallem durch die kurzen Abrufintervalle erklären, da es in so einem kurzen Zeitraum sehr unwahrscheinlich ist, dass derart wichtige Veränderungen im System vorgenommen werden.

#figure(
  image("/assets/Elements Query.png", width: 90%),
  caption: [Ergebnisse des Element-Count-Query],
)

= Fazit

Das Projekt, welches ich in dieser Arbeit beschrieben habe, dient dazu die historische Entwicklung der Feature-Nutzung innerhalb von der internen ABAP #CDS-View#"s" zu speichern und zu analysieren. Durch die automatischen Speicherung der Daten in regelmäßigen Abständen in meine eigenen Tabellen und der Zuweisung einer Versionsnummer, ist es nun möglich die Daten historisch zu analysieren und abzurufen. In Kombination dazu habe ich einen #OLAP-Würfel erstellt, der die verschiedenen Tabellen miteinander verknüpft und es mir ermöglicht analytische #Queries zu erstellen, die diese Daten nach bestimmten Kriterien auswerten und visualisieren. Dieses Framework ermöglicht es dem Team nun Einblicke in die genaue Entwicklung und Nutztung der verschienen #CDS -Objekte zu bekommen und datenbasiert die Entscheidungen zu treffen, welche Funktionen, Objekte und Elemente weiterentwickelt werden sollten.

Die Integration des Datenabrufes erfolgte mit Absprache mit meinem Betreuer Kolja Groß, der mir bei der Implementierung und den technischen Herausforderungen zur Seite stand. Durch regelmäßige Meetings und Updates konnte ich sicherstellen, dass die Anforderungen des Teams erfüllt wurden und das Projekt in die richtige Richtung ging.

Es gab einige Herausforderungen und Probleme, die während der Implementierung des Datenabrufes auftraten, wie zum Beispiel lange Verarbeitungszeiten beim Zugriff auf die Daten, wordurch es nötig geworden ist die Datenmenge zu limitieren. Auch die Übertragung der erstellten Objekte in das System ODE führte zu Datenverlusten, da es nicht möglich war die bisher gespeicherten Daten zu übertragen. Dies und anfängliche Ungenauigkeiten im Code führten zu vermehrten Datenverlusten, die die schlussendliche Qualität der erhobenen Daten und folglich auch der Auswertung beeinflusst haben.

Trotz dieser Herausforderungen konnte ich ein funktionierendes System erstellen, welches die Anforderungen des Projektes erfüllt. Für zukünftige Ausarbeitungen des System  kann es sinnvoll sein, den Prozess der Datenspeicherung zu optimieren, um die Limitierung der Datenmenge entfernen zu können. Außerdem können weitere Tabellen und Objekttypen in das System integriert werden, um nicht nur den Teams rund um die ABAP #CDS#"s" Einblicke in die Objektnutzung zu geben, sondern auch anderen Teams und Abteilungen innerhalb des Unternehmens.