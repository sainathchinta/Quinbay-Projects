package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import java.util.List;
import java.util.Set;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

@Component("ProductCustomMerchantRepository")
public interface CustomMerchantRepository extends MongoRepository<CustomMerchant, String> {
  List<CustomMerchant> findByIdIn(Set<String> id);
}
