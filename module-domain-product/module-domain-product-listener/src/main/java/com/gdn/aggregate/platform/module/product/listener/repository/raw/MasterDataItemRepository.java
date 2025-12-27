package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.List;
import java.util.Set;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;

public interface MasterDataItemRepository extends MongoRepository<MasterDataItem, String> {
  List<MasterDataItem> findByIdIn(Set<String> ids);

  void deleteByProductCode(String productCode);

}
