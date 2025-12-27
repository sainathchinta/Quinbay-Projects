package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

@Repository
public interface ItemPickupPointMigrationRepository extends MongoRepository<ItemPickupPointMigration, String>, ItemPickupPointMigrationRepositoryCustom {

  ItemPickupPointMigration findByItemSku(String itemSku);

  List<ItemPickupPointMigration> findByItemSkuIn(Set<String> itemSkuList);
}
