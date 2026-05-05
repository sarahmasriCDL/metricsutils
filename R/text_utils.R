#' Tag venture descriptions into structured categories
#'
#' Parses a venture description and assigns relevant tags across CDL stream
#' theses and their subcategories. Tags are identified using keyword-based
#' matching against a predefined dictionary covering Climate, Compute, BME,
#' Minerals, and Defence streams.
#'
#' @param description A character string containing the venture description.
#'
#' @return A named list with one element per matched stream (e.g. \code{Climate},
#' \code{Compute}, \code{BME}, \code{Minerals}, \code{Defence}). Each element is
#' a character vector of matched subcategory labels within that stream.
#' Streams with no matches are omitted from the result.
#'
#' @details
#' This function uses rule-based NLP through dictionary and synonym matching.
#' A subcategory tag is assigned if any keyword associated with it is found
#' as a whole word in the venture description (case-insensitive).
#'
#' @examples
#' tag_venture_description("We develop satellite-based earth observation tools
#'   for environmental monitoring using edge AI processors.")
#'
#' @export
tag_venture_description <- function(description) {
  description <- tolower(description)
  
  stream_thesis <- list(
    
    # ── Climate ────────────────────────────────────────────────────────────────
    "Climate: Energy & Power Generation" = c(
      "energy", "power generation", "low-carbon", "low carbon", "microgrid", "grid",
      "renewable", "solar", "wind", "battery", "electrification",
      "next-generation energy", "scalable energy"
    ),
    "Climate: Infrastructure & Industrial Efficiency" = c(
      "industrial efficiency", "energy efficiency", "infrastructure",
      "resource utilization", "optimizing performance", "energy consumption"
    ),
    "Climate: Circular Materials" = c(
      "circular", "recycling", "waste stream", "circular production",
      "regenerating waste", "reusing", "waste reuse"
    ),
    "Climate: Water & Land Use" = c(
      "water security", "water tech", "land use", "ecosystem",
      "restoration", "agriculture", "land restoration"
    ),
    "Climate: Carbon Management" = c(
      "carbon", "emissions", "carbon capture", "carbon storage",
      "decarbonize", "decarbonise", "carbon mineralization",
      "industrial footprint", "repurposing emissions"
    ),
    "Climate: Climate Resilience & Dual-Use" = c(
      "climate resilience", "adaptation", "critical infrastructure",
      "climate-driven", "dual-use", "anticipating", "mitigating climate"
    ),
    
    # ── Compute ────────────────────────────────────────────────────────────────
    "Compute: Robotics" = c(
      "robot", "robotics", "humanoid", "collaborative robot",
      "mobile robot", "autonomous robot"
    ),
    "Compute: Industrial Automation & Process Intelligence" = c(
      "industrial automation", "process intelligence", "process analytics",
      "digital twin", "simulation environment", "workplace safety",
      "core automation", "automation and control"
    ),
    "Compute: Physical Sensing & Actuation" = c(
      "sensor", "sensing", "computer vision", "localization", "mapping",
      "actuation", "physical sensing"
    ),
    "Compute: Industrial Networks" = c(
      "5g", "6g", "iot", "connectivity", "industrial network",
      "connected devices", "edge computing", "low-latency infrastructure"
    ),
    "Compute: Edge AI Hardware & Optimization" = c(
      "edge ai", "fpga", "asic", "processor", "accelerator",
      "on-device", "model inference", "on-device model", "chips"
    ),
    "Compute: Orchestration & Coordination" = c(
      "orchestration", "coordination", "edge computing platform",
      "low-latency", "edge infrastructure"
    ),
    "Compute: Quantum-Enabled Technologies" = c(
      "quantum sensing", "quantum navigation", "quantum imaging",
      "gravimetry", "magnetometry", "quantum communication",
      "quantum-enabled"
    ),
    "Compute: Next-Gen Compute Foundations" = c(
      "neuromorphic", "biological computing", "quantum computing",
      "next-gen compute", "next generation compute"
    ),
    "Compute: Infrastructure Security" = c(
      "pqc", "post-quantum cryptography", "hardware security module",
      "secure communications chip", "quantum-safe", "hardware security",
      "infrastructure security"
    ),
    
    # ── BME ────────────────────────────────────────────────────────────────────
    "BME: Digital Biomodeling & Simulation" = c(
      "computational model", "simulated biomedical", "digital twin",
      "virtual clinical trial", "disease progression", "personalized care",
      "biomedical data", "ai model health", "digital biomodel"
    ),
    "BME: Biofabrication & Disease Modeling" = c(
      "biofabrication", "bioprinting", "3d tissue", "tissue engineering",
      "microfluidics", "organ-on-a-chip", "artificial organ",
      "laboratory automation", "tissue model"
    ),
    "BME: Diagnostics & Therapeutics" = c(
      "diagnostic", "therapeutics", "drug delivery", "pharmaceutical",
      "personalized medicine", "genomic", "biomarker", "companion diagnostic",
      "digital therapeutic"
    ),
    "BME: Therapeutic Devices & Surgical Tools" = c(
      "implantable", "point-of-care", "non-invasive", "patient support device",
      "assistive surgical", "robotic surgical", "surgical tool",
      "therapeutic device", "drug delivery device"
    ),
    "BME: Wearable & Assistive Technologies" = c(
      "wearable", "biosensor", "monitoring device", "robotic device",
      "prosthetic", "orthotics", "remote monitoring", "rehabilitation device"
    ),
    "BME: Medical Imaging" = c(
      "medical imaging", "diagnostic imaging", "molecular imaging",
      "remote patient diagnostic", "ai-enhanced imaging",
      "portable diagnostic", "image-guided surgery"
    ),
    "BME: Hospital Operations & Care Delivery" = c(
      "clinical decision support", "hospital management", "clinical workflow",
      "interoperability", "in vitro", "molecular diagnostic",
      "care delivery", "hospital operations"
    ),
    "BME: Digital Health" = c(
      "digital health", "electronic record", "telemedicine", "mobile health",
      "virtual care", "health data analytics", "patient engagement",
      "ar/vr health", "public health decision"
    ),
    "BME: Health Equity" = c(
      "health equity", "hospital-at-home", "community-based care",
      "women's health", "remote patient monitoring",
      "mobile health solution", "home-testing", "home testing kit"
    ),
    
    # ── Minerals ───────────────────────────────────────────────────────────────
    "Minerals: Ore Body Knowledge" = c(
      "geophysics", "geochemistry", "drilling data", "hyperspectral",
      "xrf", "libs", "quantum sensing", "emf", "novel sensor",
      "ar/vr mining", "digital twin mining", "ai/ml mining", "5g mining"
    ),
    "Minerals: Extraction & Novel Drilling" = c(
      "alternative drilling", "in-situ recovery", "modular mining",
      "core scanner", "low-grade ore", "salar brine", "deep-sea mining",
      "space mining", "novel drilling"
    ),
    "Minerals: Waste to Value" = c(
      "urban mining", "biomining", "recycling", "circular economy",
      "tailings reprocessing", "genomics mining", "waste to value"
    ),
    "Minerals: Mining Operations" = c(
      "fleet management", "ghg management", "high density ev",
      "energy transition mining", "data intelligence mining",
      "mining operations", "mine operation"
    ),
    "Minerals: Autonomous Mining" = c(
      "autonomous mining", "mine design", "mine automation",
      "sensors iot mining", "ai ml mining", "mining robotics"
    ),
    "Minerals: Processing" = c(
      "hydrometallurgy", "processing analytics", "processing hardware",
      "electrolysis", "leaching", "mineral processing"
    ),
    "Minerals: Risk" = c(
      "tailings monitoring", "supply chain traceability",
      "predictive analytics mining", "geopolitical risk",
      "community planning mining", "safety monitoring mining"
    ),
    "Minerals: Remediation" = c(
      "water tech mining", "carbon mineralization", "geological carbon capture",
      "phytomining", "land restoration", "biomining recovery",
      "tailings remediation", "genomics remediation"
    ),
    "Minerals: Refining" = c(
      "smelting alternative", "biorefining", "electrorefining",
      "electrowinning", "solvent extraction", "molten salt electrolysis",
      "hydrogen-based reduction", "alloying", "downstream metals"
    ),
    
    # ── Defence ────────────────────────────────────────────────────────────────
    "Defence: Space & ISR" = c(
      "earth observation", "satellite", "pnt", "space-based sensing",
      "space-based", "isr", "satellite communications",
      "space-based intelligence", "surveillance reconnaissance"
    ),
    "Defence: Autonomy & Robotics" = c(
      "drone", "uncrewed", "human-machine teaming", "autonomous vehicle",
      "unmanned", "uas", "uav", "autonomous system defence"
    ),
    "Defence: AI, C4ISR & Cyber" = c(
      "command and control", "c4isr", "secure communications",
      "sensor fusion", "cyber", "ai/ml analytics defence",
      "situational awareness", "cybersecurity defence"
    ),
    "Defence: Advanced Sensors & Electronic Warfare" = c(
      "rf sensing", "eo/ir", "signal intelligence", "electronic warfare",
      "directed energy", "radar", "airspace monitoring", "early warning"
    ),
    "Defence: Integrated Air & Space Defence" = c(
      "counter-uas", "early warning system", "airspace monitoring",
      "directed energy laser", "integrated air", "iasd"
    ),
    "Defence: Energy & Infrastructure Resilience" = c(
      "deployable power", "hardened infrastructure", "microgrid defence",
      "resilient infrastructure defence"
    ),
    "Defence: Logistics, Sustainment & Training" = c(
      "supply chain technology", "simulation training", "predictive maintenance",
      "logistics defence", "sustainment", "training simulation"
    ),
    "Defence: Operator Performance & Protection" = c(
      "medtech defence", "biodefence", "advanced materials defence",
      "soldier system", "operator protection", "operator performance"
    ),
    "Defence: Asture Environment Operations" = c(
      "harsh environment", "resource-constrained", "remote location",
      "remote navigation", "remote communications", "remote robotics"
    )
  )
  
  # Match each subcategory against the description
  matched <- purrr::keep(
    purrr::map(stream_thesis, function(keywords) {
      pattern <- paste0("\\b(", paste(stringr::str_escape(keywords), collapse = "|"), ")\\b")
      any(stringr::str_detect(description, stringr::regex(pattern, ignore_case = TRUE)))
    }),
    isTRUE
  )
  
  matched_tags <- names(matched)
  
  if (length(matched_tags) == 0) {
    return(list())
  }
  
  # Split "Stream: Subcategory" into a nested list grouped by stream
  streams <- sub(":.*", "", matched_tags)
  subcategories <- sub("^[^:]+: ", "", matched_tags)
  
  result <- split(subcategories, streams)
  result <- lapply(result, unname)
  
  result
}




#' Convert newline-separated string to comma-separated string
#'
#' Takes a string with substrings separated by newlines and returns a
#' single string with those substrings joined by a comma and space.
#'
#' @param input A character string containing substrings separated by
#'   newline characters (\code{\\n}).
#'
#' @return A single character string with each line joined by \code{", "}.
#'   Returns an empty string if \code{input} is \code{""}.
#'
#' @details
#' Splits \code{input} on \code{\\n} and collapses the resulting character
#' vector with \code{", "} as the separator. Only the first element of
#' \code{input} is processed; passing a vector of length > 1 will silently
#' use only the first element.
#'
#' @examples
#' new_line_to_commas("apple\nbanana\ncherry")
#' # [1] "apple, banana, cherry"
#'
#' new_line_to_commas("only one line")
#' # [1] "only one line"
#'
#' @export
new_line_to_commas <- function(input) {
  # Split by newlines and join with commas
  output <- paste(strsplit(input, "\n")[[1]], collapse = ", ")
  return(output)
}
