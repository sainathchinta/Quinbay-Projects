package com.gdn.partners.pcu.internal.model;

/**
 * @author Navya Naveli
 */

public interface MasterCatalogApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/master-sku";

  String IN_REVIEW_PATH = "/in-review-items";
  String GET_ALL_ITEMS = "/get-all-items";
  String MASTER_SKU_DETAILS = "/{itemSku}/detail";
  String FETCH_ITEMS_MAPPED_TO_MASTER_SKU = "/{masterSku}/item-sku-list";
  String COMPARE_ANCHORS = "/{anchorId1}/{anchorId2}/compare-anchors";
  String GET_HISTORY_DETAILS_FOR_ITEM = "/{itemSku}/view-history";
  String GET_MASTER_SKU_REVIEW_CONFIG = "/config";
  String PERFORM_CLUSTER_REVIEW_ACTION = "/{masterSku}/clusterAction";
  String ACCEPT_REJECT_ACTION = "/{anchorId1}/{anchorId2}/accept-reject-anchors";
  String CHANGE_ASSIGNEE = "/change-assignee";
  String REVIEWERS = "/reviewers";
  String MASTER_SKU_ITEMS_DOWNLOAD = "/master-sku-items-download";
  String BULK_DOWNLOAD_IN_REVIEW = "/download/in-review";
  String BULK_UPLOAD = "/bulk-upload";
  String FETCH_INDICATIVE_PRICE = "/indicative-price";
}
