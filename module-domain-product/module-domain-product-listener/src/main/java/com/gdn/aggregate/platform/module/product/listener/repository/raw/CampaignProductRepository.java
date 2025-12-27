package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

public interface CampaignProductRepository extends MongoRepository<CampaignProduct, String> {

    Stream<CampaignProduct> streamAllBySku_ItemSkuAndSku_PickupPointCode(String itemSku, String pickupPointCode);

    Stream<CampaignProduct> streamAllByCampaignCodeSessionIdInAndActiveTrue(List<String> campaignCodeSessionIds);

    List<CampaignProduct> findAllByCampaignCodeAndTagLabelNot(String campaignCode, String tagLabel, Pageable pageable);

    List<CampaignProduct> findAllByIdIn(List<String> ids);

    List<CampaignProduct> findBySku_ItemSkuIn(Set<String> productSku);
}
