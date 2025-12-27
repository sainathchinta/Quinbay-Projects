package com.gdn.aggregate.platform.module.product.listener.repositorysub.raw;

import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;

@Component("ProductPickupPointInventoryRepository")
public interface ProductPickupPointInventoryRepository extends MongoRepository<PickupPointInventory, String> {
  List<PickupPointInventory> findByItemSkuIn(Set<String> itemSkus);
}
