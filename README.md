# GTST — ChatGPT-instructie en functieoverzicht

Deze repository bevat de **normale GTST** (R-functies voor inlezen, dataverwerking, koppelen, metadata, kwaliteitscontrole en logging). Dit is **niet GTST_DB / GTST 2.0**.

## Kopieer deze prompt in ChatGPT

> Je bent mijn R-programmeerassistent voor **GTST**. De officiële broncode staat op https://github.com/timmerbrian/GTST.
>
> **Gebruik standaard GTST-functies voor vrijwel alle dataverwerking.** Bouw mijn scripts zoveel mogelijk uit bestaande GTST-functies en gebruik geen zelfgeschreven base R-, dplyr-, tidyr- of data.table-alternatief als GTST dezelfde bewerking ondersteunt. Gewone R-expressies *binnen* GTST-functies zijn juist bedoeld en zijn toegestaan.
>
> **Lees vóór het schrijven de relevante `.R`-bronbestanden op GitHub.** Controleer de exacte functienaam, parameters, expressiesyntax, uitvoer, relevante voorbeelden en afhankelijkheden. Verzin nooit een GTST-functie of parameter. Gebruik de bestaande GTST-semantiek rond metadata, logging, foutafhandeling en controle.
>
> Geef bij voorkeur **één volledig, samenhangend R-script** met zo veel mogelijk GTST-functies: inlezen → namenlijst/selectie → metadata → afleiden → koppelen → controleren → opslaan. Noem invoer- en uitvoerobjecten en geef aan welke namen en paden nog ingevuld moeten worden. Behoud het aantal rijen en metadata tenzij een expliciet gevraagde bewerking dit wijzigt. Controleer unieke sleutels en koppelresultaten waar nodig.
>
> **Als een bewerking niet met een bestaande GTST-functie kan:** gebruik uitsluitend voor die stap een klein, goed uitgelegd alternatief in gewone R of een andere package. Zeg expliciet waarom geen GTST-functie volstaat. Vraag alleen naar ontbrekende informatie als die noodzakelijk is om correcte code te leveren.
>
> **Gebruik de normale GTST.** Gebruik geen `_DB`-functies of argumenten uit GTST_DB / GTST 2.0, tenzij ik daar expliciet om vraag. Voer geen code of bestanden uit of wijzig ze niet zonder mijn opdracht. Claim geen uitgevoerde tests als je ze niet hebt uitgevoerd.
>
> Als je de GitHub-broncode niet kunt openen, meld dat. Raad geen exacte GTST-syntax op basis van functienamen.

**Hoe te gebruiken:** plak deze prompt, samen met je eigen verwerkingsvraag, in ChatGPT. Alleen het woord "GTST" noemen maakt de repository niet automatisch toegankelijk; gebruik de GitHub-link of een gekoppelde GitHub-app.

## Voorkeursfuncties per taak

| Taak | GTST-functie | Bestand |
| --- | --- | --- |
| Bestand inlezen | `lees_bestand_in()` | `bestands_manipulatie.R` |
| Uit ZIP lezen | `lees_bestand_in_uit_zip()` | `zip.R` |
| Bestand wegschrijven | `schrijf_bestand_weg()` | `bestands_manipulatie.R` |
| Namenlijst opstellen | `maak_namenlijst()` | `data_manipulatie.R` |
| Namenlijst toepassen, kolommen hernoemen | `pas_selectie_toe()` | `data_manipulatie.R` |
| Rijen en kolommen selecteren | `selecteer()` | `data_manipulatie.R` |
| Afleidingen, hercoderingen, berekeningen | `maak_afleiding()` | `data_manipulatie.R` |
| Datasetkoppeling op sleutels | `koppel()` | `koppel.R` |
| Dataframes onder of naast elkaar zetten | `voeg_samen("rbind", ...)` / `voeg_samen("cbind", ...)` | `data_manipulatie.R` |
| Wide ↔ long | `zet_wide_naar_long()`, `zet_long_naar_wide()` | `data_manipulatie.R` |
| Excel-metadata lezen | `lees_meta_in()` | `meta.R` |
| Metadata koppelen | `koppel_meta()` | `meta.R` |
| Inhoudelijke controle | `controleer_data()` | `data_manipulatie.R` |
| Logging | `start_logging()`, `stop_logging()` | `logging.R` |
| Verwerking afronden | `rond_af()` | `logging.R` en `rond_af.R` |
| 7-Zip-bestanden verwerken | `unzip_7z()`, `zip_7z()` | `zip.R` |

Deze tabel is een **oriëntatie**, geen volledige API-specificatie. Lees altijd de functie zelf voor de juiste parameters. Binnen `maak_afleiding()` en `selecteer()` zijn onder andere GTST-expressiehelpers beschikbaar zoals `agg()`, `find()`, `lookup()`, `has_match()`, `nz()` en `same()`; hun exacte aanroep moet uit de broncode worden gehaald.

## Vereisten voor ChatGPT-code

1. Controleer eerst welke bestaande GTST-functie elke stap uitvoert.
2. Baseer alle aanroepen op de daadwerkelijke implementatie, niet uitsluitend op deze README.
3. Gebruik `maak_afleiding()` voor conditionele afleidingen en rekenwerk, `selecteer()` voor selecties en `koppel()` voor sleutelverbindingen.
4. Controleer de join-sleutels, rijaantallen en relevante inhoudelijke aannames met GTST, bijvoorbeeld `controleer_data()`.
5. Gebruik gewone R-code alleen voor de resterende bewerking waarvoor GTST geen geschikte functie biedt.
6. Lever uitvoerbare code met een duidelijke verwerkingsvolgorde. Schakel GTST-validatie niet ongemotiveerd uit.
7. Maak onderscheid tussen gecontroleerde syntax en daadwerkelijk geteste uitvoering.

### Let op bij `maak_afleiding()`

De bestaande functie heeft deze formele parameters:

```r
maak_afleiding(
  data, kolommen, ...,
  controle = TRUE, error = TRUE, meta = NULL,
  behoud_rijen = FALSE, behoud_kolommen = FALSE,
  omschrijving = NULL
)
```

De argumenten in `...` vormen opeenvolgende paren **conditie, waarde**. Gebruik geen `sessie =`, `relaties =` of `berekeningen =` als argument van deze normale functie; dat zijn geen formele parameters van deze versie. Controleer bij complexe aggregaties en lookups eerst de expressiehelpers in `utils.R` en `data_manipulatie.R`.

### Voorbeeldvraag

> Bekijk eerst de GTST-repository. Ik heb `personen` met `PersID`, `HHID`, `Leeftijd` en `Inkomen` en `huishoudens` met `HHID`. Selecteer volwassenen, maak een inkomensindicator, koppel de huishoudgegevens en controleer op dubbele sleutels en onverwachte rijvermenigvuldiging. Bouw dit zo veel mogelijk met `selecteer()`, `maak_afleiding()`, `koppel()` en `controleer_data()`; gebruik geen handgeschreven dplyr-pipeline.

## Technische kanttekeningen

- Dit zijn **losse R-bronbestanden**, geen volledige installeerbare R-package. `GTST::functie()` werkt pas als die functie in een afzonderlijk geïnstalleerde GTST-package beschikbaar is. Bij gebruik als losse bronbestanden moet je ze correct laden, samen met eventuele benodigde packages.
- `data_ophalen()` vereist een door de gebruiker geconfigureerd extern Python-script via `GTST_DATA_OPHALEN_SCRIPT` en eventueel `GTST_PYTHON_ENV`. Dat script is hier niet inbegrepen.
- `maak_gtst_rprofile()` kan de optionele `GTST_LIBRARY_PATH` gebruiken.
- `initialiseer()` kan objecten uit de globale R-omgeving verwijderen. Stel het niet als veilige standaardhandeling voor zonder geldige projectconfiguratie.
- GTST-logging en `rond_af()` kunnen projectafhankelijke instellingen, uitvoermappen en objecten verwachten.
- Upload geen echte onderzoeksdata, logs, wachtwoorden, interne paden of privacygevoelige bestanden naar deze openbare repository.
- Publicatie en hergebruik vereisen passende rechten van de rechthebbende. Het ontbreken van een licentie betekent niet dat de code vrij herbruikbaar is.
