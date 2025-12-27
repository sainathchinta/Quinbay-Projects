package com.gdn.mta.product.repository;

import com.gdn.x.businesspartner.dto.PickupPointResponse;

public interface PickupPointRepository {
  
  PickupPointResponse findByPickupPointCode(String pickupPointCode) throws Exception;

}
