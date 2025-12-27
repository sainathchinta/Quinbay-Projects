package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.ItemArchive;

public interface ItemArchiveRepository extends MongoRepository<ItemArchive, String>, ItemArchiveCustomRepository {

}
