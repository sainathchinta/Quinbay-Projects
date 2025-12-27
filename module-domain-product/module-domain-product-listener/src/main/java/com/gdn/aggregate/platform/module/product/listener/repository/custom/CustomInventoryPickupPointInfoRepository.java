package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import java.util.List;
import java.util.Set;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

@Component("ProductCustomInventoryPickupPointInfoRepository")
public interface CustomInventoryPickupPointInfoRepository extends MongoRepository<CustomInventoryPickupPointInfo, String> {
  List<CustomInventoryPickupPointInfo> findByItemSkuIn(Set<String> itemSkus);
}
