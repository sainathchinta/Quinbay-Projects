package com.gdn.partners.pcu.external.model;

public interface UtilityApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/utility";
  String REPUBLISH_L3_TO_AGP = "/{productSku}/republishL3ToAgp";
  String REPUBLISH_L4_TO_AGP = "/{itemSku}/republishL4ToAgp";
  String REPUBLISH_L5_TO_AGP = "/{itemSku}/republishL5ToAgp/{pickupPointCode}";
  String RETRY_VIDEO_COMPRESSION = "/{videoId}/retry-compression";
  String GENERATE_FINGERPRINT = "/{videoId}/generateFingerprint";
  String REINDEX_BRAND_COLLECTION_BY_BRAND_REQUEST_CODE = "/{brandRequestCode"
    + "}/reindexByBrandRequestCode";
}
