package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.PickupPoint;

public interface PickupPointRepositoryCustom {

  void upsertPickupPoint(String storeId, PickupPoint pickupPoint, String username);
}
