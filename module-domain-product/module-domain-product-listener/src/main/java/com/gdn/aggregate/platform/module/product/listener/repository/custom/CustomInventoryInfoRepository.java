package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import java.util.List;
import java.util.Set;

import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryInfo;

@Component("ProductCustomInventoryInfoRepository")
public interface CustomInventoryInfoRepository extends MongoRepository<CustomInventoryInfo, String> {

  CustomInventoryInfo findFirstByItemSkuAndType(String itemSku, String type);

  List<CustomInventoryInfo> findByItemSkuIn(Set<String> itemSkus);
}
