package com.gdn.partners.pcu.external.model;

public interface DistributionInfoApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/distribution-info";
  String GET_DISTRIBUTION_INFO = "/{productCode}";
  String UPDATE_DISTRIBUTION_INFO = "/{productCode}";
}
