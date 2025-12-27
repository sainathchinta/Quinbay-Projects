package com.gdn.partners.pcu.internal.model;

public interface IPRApiPath {
    String BASE_PATH = Constants.CONTEXT_PATH + "/api/ipr";
    String IPR_PRODUCT_LIST = "/filter/summary";
    String IPR_REVIEWERS = "/reviewers";
    String IPR_PRODUCT_DETAIL = "/{productSku}/detail";
    String UPDATE_ASSIGNEE = "/update-assignee";
    String PRIMARY_FILTER = "/primary-filter-counts";
    String SUSPENSION_IN_PROGRESS = "/suspension-in-progress";
    String PERFORM_IPR_ACTION = "/action";
    String HISTORY = "/history";
    String IPR_PRODUCTS_DOWNLOAD = "/bulk-download";
}
