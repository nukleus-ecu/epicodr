# Value labels that define missing values

missing_labels_default <- c("Keine Informationen verfügbar",
                            "Keine Information verfügbar",
                            "Keine Informationen vorhanden",
                            "Unbekannt", "Unbekannt (Bitte nachtragen)",
                            "Keine Angabe", "N.A.", "Weiß nicht/keine Angabe",
                            "Wert nicht vorhanden", "Nicht erhoben",
                            "Nicht probiert, nicht durchgeführt",
                            "Nicht auswertbar",
                            "Weiß nicht", "Weiß nicht/Unbekannt",
                            "Trifft nicht zu",
                            "Nicht erhoben",
                            "Keine Informationen verfügbar",
                            "Nicht durchgeführt",
                            "Unbekannt (nicht bestimmbar)",
                            "Keine Angabe möglich")

usethis::use_data(missing_labels_default, overwrite = TRUE)

# stringi::stri_escape_unicode(missing_labels_default)
