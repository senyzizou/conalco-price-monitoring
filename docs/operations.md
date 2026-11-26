# Conalco-Betrieb

Der zunächst manuelle Workflow verwendet einen Standard-Runner. Das R-Skript
stellt AGB-, Listing- und Konfigurationsreferenzen aus einem festgelegten
vorhandenen privaten Roharchiv bereit. Hashes für Archiv, Sidecar und die drei
benötigten Mitglieder stehen in `config/conalco_reference_pins.json`.
Bei Abweichung bricht der Start vor der Erhebung ab. Aus dem Archiv wird
kein R-Code ausgeführt. Die Referenzen liegen nur im temporären Runner.

Der Tageslauf stellt den letzten validierten Ergebniscommit einschließlich
der Archivkette wieder her, prüft den Berliner Erhebungstag und verwendet die
bestehende gemeinsame Writer-Sperre. Danach prüft er die neue Erhebung,
archiviert die Originalantworten und veröffentlicht das Ergebnisinkrement.
Erst der Tagescommit macht einen Abschluss gültig. Wissenschaftliche
Ableitungen bleiben in R. Das versionierte Schema steht unter `config/`.

## Inbetriebnahme nach Freigabe

1. Das kanonische Repository und `main` als Standardbranch konfigurieren.
   Änderungen an Workflow, R-Code und Pin-Datei auf main kontrollieren.
   Fork- und Pull-Request-Workflows ohne produktive Berechtigungen belassen.
2. Das Environment `conalco-ukebox` auf main beschränken und die drei WebDAV-Secrets
   `UKEBOX_WEBDAV_URL`, `UKEBOX_USERNAME`, `UKEBOX_APP_PASSWORD` begrenzen.
   `CONALCO_REFERENCE_ARCHIVE` ist im selben Environment der private
   Archivpfad zum festgelegten Referenzroharchiv. Er wird nur beim Start
   des Preflights oder der Erhebung verwendet. Zugänge auf dieses Projekt
   und die benötigten Lese-, Archivierungs- und Sperroperationen begrenzen.
3. Nach Freigabe `CONALCO_OPERATIONS_ENABLED=true` setzen. Manueller
   `preflight` umfasst einen kleinen privaten WebDAV-Schreib-/Lesetest
   und den validierenden Restore. Danach einen separat freigegebenen
   neuen Berliner Erhebungstag mit `full` prüfen. Den Tagescommit und
   Signale zur Bereinigung privat kontrollieren.
4. Erst nach erfolgreichem Test den Zeitplan separat freigeben. Für
   unbeaufsichtigte Ausführung darf das Environment keine tägliche
   Genehmigung verlangen. Der Standardbranch bleibt geschützt.

Die privaten R-Diagnosen werden während eines Jobs in seiner temporären
Umgebung gehalten. Öffentliche Logs enthalten feste Betriebsstatusmeldungen.
Der Workflow legt keine GitHub-Artefakte oder Caches an. Bei mehrdeutigem
Upload zuvor den privaten Serverzustand klären. Die bestehende Wiederaufnahme
verwendet einen eindeutig validierten privaten Roharchivbeleg. Für den
separat beauftragten Dispatch `recover` wird der Belegpfad vorübergehend
als Environment-Secret `CONALCO_RECOVERY_ARCHIVE` bereitgestellt. Der
reguläre Tageslauf benötigt dieses Secret nicht.

## Prüfungen

`Rscript --vanilla scripts/conalco_actions.R --tests-fast` prüft den
öffentlichen Kern mit synthetischen Eingaben und simuliertem WebDAV.
`--tests-extended` prüft die Skriptstarts. Der technische Offline-Preflight
`--offline` setzt die geprüften privaten Referenzdateien voraus.
Originalantwort-, historische Publikations- und reale Fixture-Regressionen
bleiben im unveränderten privaten Prüfkandidaten ausführbar. Keine privaten
Fixtures in einen öffentlichen Checkout übernehmen.
