package com.gdn.partners.pcu.internal.model;

public interface BusinessPartnerApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/businessPartners";
  String BUSINESS_PARTNER_FILTER = "/filter";
  String BUSINESS_PARTNER_LIST = "/getActiveMerchantList";
  String BUSINESS_PARTNER_FLAGS = "/getFlags";
}
