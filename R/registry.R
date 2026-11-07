#' The dbparser parsers the server offers
#'
#' One entry per exported \code{dbparser} parser. \code{licence} and
#' \code{licence_url} are those each parser records in its
#' \code{source_metadata}; a test checks them against the parsers' output on
#' the bundled sample files, so they cannot drift.
#'
#' @keywords internal
#' @return named list of parser entries
parser_registry <- function() {
  list(
    parseDrugBank = list(
      source      = "DrugBank",
      reads       = "The DrugBank full database XML (or the zip it comes in), downloaded under your own DrugBank account and licence.",
      input       = "file",
      tables      = c("drugs", "salts", "products", "references", "cett"),
      licence     = NA_character_,
      licence_url = "https://trust.drugbank.com/drugbank-trust-center/drugbank-terms-of-service",
      since       = "2.0.0",
      new_args    = FALSE,
      sample      = NULL),
    parseOnSIDES = list(
      source      = "OnSIDES",
      reads       = "A folder with the OnSIDES CSV files (product_label.csv, product_adverse_effect.csv, ...), or a zip of that folder.",
      input       = "dir",
      tables      = c("product_label", "product_adverse_effect",
                      "product_to_rxnorm", "vocab_meddra_adverse_effect",
                      "vocab_rxnorm_ingredient", "vocab_rxnorm_product",
                      "vocab_rxnorm_ingredient_to_product", "high_confidence"),
      licence     = "CC-BY-4.0",
      licence_url = "https://github.com/tatonetti-lab/onsides/blob/main/LICENSE-DATA",
      since       = "2.2.1",
      new_args    = FALSE,
      sample      = c("onside.zip")),
    parseTWOSIDES = list(
      source      = "TWOSIDES",
      reads       = "TWOSIDES.csv.gz (or a zip holding the CSV).",
      input       = "file",
      tables      = "drug_drug_interactions",
      licence     = NA_character_,
      licence_url = NA_character_,
      since       = "2.2.1",
      new_args    = FALSE,
      sample      = c("twoside_raw.zip")),
    parseHPO = list(
      source      = "Human Phenotype Ontology (Harmonizome)",
      reads       = "Harmonizome's HPO gene_attribute_edges.txt, plain or gzip-compressed.",
      input       = "file",
      tables      = c("associations", "genes", "phenotypes"),
      licence     = "CC-BY-NC-SA-4.0",
      licence_url = "https://maayanlab.cloud/Harmonizome/terms",
      since       = "2.3.0",
      new_args    = FALSE,
      sample      = c("extdata", "hpo_gene_attribute_edges.txt")),
    parseHGNC = list(
      source      = "HUGO Gene Nomenclature Committee (HGNC)",
      reads       = "hgnc_complete_set.txt; give withdrawn.txt as second_path.",
      input       = "file",
      tables      = c("gene", "gene_name", "withdrawn"),
      licence     = "CC0-1.0",
      licence_url = "https://www.genenames.org/about/license/",
      since       = "2.3.0",
      new_args    = TRUE,
      sample      = c("extdata", "hgnc", "hgnc_complete_set.txt"),
      second_sample = c("extdata", "hgnc", "withdrawn.txt")),
    parseHealthCanadaDPD = list(
      source      = "Health Canada Drug Product Database (DPD)",
      reads       = "A folder holding Health Canada's allfiles*.zip (or their unzipped folders) and inactive.zip.",
      input       = "dir",
      tables      = c("drug_product", "active_ingredients", "companies",
                      "form", "packaging", "pharmaceutical_std", "route",
                      "schedule", "status", "therapeutic_class",
                      "veterinary_species", "biosimilars",
                      "inactive_products"),
      licence     = "OGL-Canada-2.0",
      licence_url = "https://open.canada.ca/en/open-government-licence-canada",
      since       = "2.3.0",
      new_args    = TRUE,
      sample      = c("extdata", "health_canada_dpd")),
    parseOpenFDA = list(
      source      = "openFDA (U.S. Food and Drug Administration)",
      reads       = "drug-drugsfda-0001-of-0001.json.zip and/or drug-ndc-0001-of-0001.json.zip (zipped or unzipped). The file name says which is which.",
      input       = "file",
      tables      = c("drugsfda_application", "drugsfda_product",
                      "drugsfda_active_ingredient", "drugsfda_submission",
                      "drugsfda_submission_property_type",
                      "drugsfda_application_doc", "drugsfda_openfda",
                      "ndc_product", "ndc_active_ingredient", "ndc_route",
                      "ndc_pharm_class", "ndc_packaging", "ndc_openfda"),
      licence     = "CC0-1.0",
      licence_url = "https://open.fda.gov/license/",
      since       = "2.3.0",
      new_args    = TRUE,
      sample      = c("extdata", "openfda", "drug-drugsfda-0001-of-0001.json.zip"),
      second_sample = c("extdata", "openfda", "drug-ndc-0001-of-0001.json.zip"))
  )
}


#' Path of a bundled dbparser sample file, or NULL
#' @param parts path parts below dbparser's installed folder
#' @keywords internal
#' @return character(1) or NULL
sample_path <- function(parts) {
  if (is.null(parts)) {
    return(NULL)
  }
  path <- do.call(system.file, c(as.list(parts), package = "dbparser"))
  if (!nzchar(path)) NULL else path
}
