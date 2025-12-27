package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.PickupPoint;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;

public interface PickupPointRepository
    extends MongoRepository<PickupPoint, String>, PickupPointRepositoryCustom {

  List<PickupPoint> findByStoreIdAndPickupPointCodeInAndCncActivatedFalse(String storeId,
      List<String> pickupPointCodes);
}
