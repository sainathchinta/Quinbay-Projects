package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.List;

import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface CheapestPriceDayRepository extends MongoRepository<CheapestPriceDay, String> {
  List<CheapestPriceDay> findAllByProductSku(String productSku);
}
