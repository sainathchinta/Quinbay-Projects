package com.gdn.partners.pcu.master.model;

public interface AutoQcConfigApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/auto-qc-config/";
  String UPDATE_AUTO_APPROVAL_RULES_BY_RULE_NAME = "{ruleName}";
  String GET_AUTO_APPROVAL_RULES = "/getRules";
}
