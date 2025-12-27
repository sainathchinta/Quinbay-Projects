package com.gdn.x.product.dao.api;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.ProductArchive;

public interface ProductArchiveRepository extends MongoRepository<ProductArchive, String>, ProductArchiveCustomRepository {

}
