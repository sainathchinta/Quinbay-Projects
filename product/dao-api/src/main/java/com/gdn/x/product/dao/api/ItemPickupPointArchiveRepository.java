package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.ItemPickupPointArchive;

public interface ItemPickupPointArchiveRepository extends MongoRepository<ItemPickupPointArchive, String>, ItemPickupPointArchiveCustomRepository {

}
