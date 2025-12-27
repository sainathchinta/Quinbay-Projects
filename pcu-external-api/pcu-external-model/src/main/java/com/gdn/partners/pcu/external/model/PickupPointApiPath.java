package com.gdn.partners.pcu.external.model;

public interface PickupPointApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/pickupPoint";
  String FETCH_PICKUP_POINTS_BY_CODES = "/pickup-point-details-from-codes";
  String VALIDATE_DELETION_BY_PRODUCT_SKU_AND_PICK_UP_POINT =
    "/{productSku}/validateDeletionByProductSkuAndPickupPoint";
}
