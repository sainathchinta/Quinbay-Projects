package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.PickupPoint;

import java.util.List;

public interface PickupPointService {

  /**upsert pickup point data
   *
   * @param storeId
   * @param pickupPoint
   * @param username
   * @return
   */
  void upsertPickupPoint(String storeId, PickupPoint pickupPoint, String username);

  /**find list of pickup point data with cnc activated false
   *
   * @param storeId
   * @param pickupPointCodes list of pickup point codes
   * @return
   */
  List<PickupPoint> findPickupPointListByPickupPointCodeInAndCncActivatedFalse(String storeId,
      List<String> pickupPointCodes);

}
