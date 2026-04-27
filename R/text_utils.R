#' Tag venture descriptions into structured categories
#'
#' Parses a venture description and assigns relevant tags across three dimensions:
#' technology, product type, and industry. Tags are identified using keyword-based
#' matching against a predefined dictionary with synonym expansion.
#'
#' @param description A character string containing the venture description.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{tech}{Character vector of detected technology tags.}
#'   \item{product}{Character vector of detected product-type tags.}
#'   \item{industry}{Character vector of detected industry tags.}
#' }
#'
#' @details
#' This function uses rule-based NLP through dictionary and synonym matching.
#' Tags are assigned if any keyword or synonym associated with a tag is found
#' in the venture description.
#'
#' @export
tag_venture_description <- function(description) {
  description <- tolower(description)
  
  tag_rules <- list(
    tech = list(
      "AI" = c(
        "ai", "artificial intelligence", "machine learning", "ml",
        "deep learning", "neural network", "large language model", "llm",
        "inference", "predictive model", "models", "onnx",
        "computer vision", "natural language processing", "nlp"
      ),
      
      "Robotics" = c(
        "robot", "robots", "robotics", "robotic",
        "autonomous system", "autonomous systems",
        "biohybrid robot", "robotic control"
      ),
      
      "IoT" = c(
        "iot", "internet of things", "sensor", "sensors",
        "connected device", "connected devices",
        "bluetooth", "mesh network", "edge device", "smart device"
      ),
      
      "Cloud Infrastructure" = c(
        "cloud", "cloud-based", "infrastructure", "deployment",
        "devops", "kubernetes", "container", "containers",
        "storage", "ceph", "distributed storage", "application hosting"
      ),
      
      "Data Security" = c(
        "encryption", "encrypted", "data protection", "privacy",
        "cybersecurity", "security", "access controls",
        "access control", "audit", "audit-ready", "compliance",
        "sensitive data", "confidential computing"
      ),
      
      "BioTech" = c(
        "biotech", "biotechnology", "biohybrid", "stem cells",
        "ipsc", "induced pluripotent stem cells", "neurons",
        "living neurons", "cell-based", "synthetic biology"
      ),
      
      "Geospatial" = c(
        "geospatial", "lidar", "point cloud", "3d scan",
        "3d scanning", "mapping", "spatial data", "remote sensing"
      )
    ),
    
    product = list(
      "SaaS" = c(
        "saas", "software-as-a-service", "software as a service",
        "cloud software", "subscription software"
      ),
      
      "Platform" = c(
        "platform", "operating system", "os", "toolset",
        "solution", "software platform"
      ),
      
      "Automation" = c(
        "automation", "automates", "automating", "automated",
        "workflow automation", "process automation", "streamlines",
        "optimizes", "optimises"
      ),
      
      "Analytics" = c(
        "analytics", "diagnostic", "diagnostics", "real-time visibility",
        "data-driven", "insights", "intelligence", "forecasting",
        "prediction", "predictive"
      ),
      
      "Developer Tools" = c(
        "devops", "developer", "developers", "production-grade applications",
        "application deployment", "software delivery", "onnx",
        "api", "apis", "sdk", "infrastructure-as-code"
      ),
      
      "Enterprise Software" = c(
        "enterprise software", "enterprise", "white-labelled",
        "white-labeled", "dashboard", "business software",
        "b2b software"
      ),
      
      "Monitoring" = c(
        "monitoring", "fault detection", "diagnostic", "diagnostics",
        "logs", "audit logs", "real-time monitoring",
        "observability", "alerting"
      )
    ),
    
    industry = list(
      "Manufacturing" = c(
        "manufacturing", "manufacturer", "factory",
        "industrial", "circuit board", "pcb", "supply chain"
      ),
      
      "Energy" = c(
        "energy", "ev charger", "ev chargers", "ev infrastructure",
        "electric vehicle", "electric vehicles", "charging network",
        "charger network", "grid", "utilities"
      ),
      
      "Foodservice" = c(
        "foodservice", "food service", "restaurant", "restaurants",
        "kitchen", "recipe", "recipe costing", "production planning",
        "labeling", "labelling", "menu"
      ),
      
      "Enterprise" = c(
        "enterprise", "enterprises", "corporate", "businesses",
        "organizations", "organisations", "workforce"
      ),
      
      "Healthcare / Life Sciences" = c(
        "healthcare", "health care", "life sciences", "stem cells",
        "ipsc", "neurons", "clinical", "medical", "biomedical"
      ),
      
      "Connectivity" = c(
        "connectivity", "network", "networks", "bluetooth",
        "mesh network", "decentralized mesh", "outages",
        "connected devices"
      )
    )
  )
  
  purrr::map(tag_rules, function(category_rules) {
    names(category_rules)[
      purrr::map_lgl(category_rules, function(keywords) {
        pattern <- paste0("\\b(", paste(stringr::str_escape(keywords), collapse = "|"), ")\\b")
        any(stringr::str_detect(description, stringr::regex(pattern, ignore_case = TRUE)))
      })
    ]
  })
}