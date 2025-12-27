package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

@Component("ProductCustomBusinessPartnerPickupPointRepository")
public interface CustomBusinessPartnerPickupPointRepository extends MongoRepository<CustomBusinessPartnerPickupPoint, String> {

}
