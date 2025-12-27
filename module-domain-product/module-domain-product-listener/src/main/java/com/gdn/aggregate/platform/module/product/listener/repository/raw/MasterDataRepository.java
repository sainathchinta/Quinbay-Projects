package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;

public interface MasterDataRepository extends MongoRepository<MasterData, String> {

}
