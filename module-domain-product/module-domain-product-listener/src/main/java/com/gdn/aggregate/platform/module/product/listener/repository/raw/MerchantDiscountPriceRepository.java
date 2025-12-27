package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface MerchantDiscountPriceRepository extends MongoRepository<MerchantDiscountPrice, String> {

}
