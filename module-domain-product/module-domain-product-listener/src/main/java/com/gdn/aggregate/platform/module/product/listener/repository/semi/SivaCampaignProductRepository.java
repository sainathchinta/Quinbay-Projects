package com.gdn.aggregate.platform.module.product.listener.repository.semi;

import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;
import java.util.stream.Stream;

public interface SivaCampaignProductRepository extends MongoRepository<SivaCampaignProduct, String> {

  Stream<SivaCampaignProduct> streamAllByCampaignCodeInAndActiveTrue(List<String> campaignCodes);

  List<SivaCampaignProduct> findAllByIdIn(List<String> ids);

}
