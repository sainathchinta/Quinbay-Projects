package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;

public interface AdjustmentProductQuotaRepository extends MongoRepository<AdjustmentProductQuota, String> {

  Stream<AdjustmentProductQuota> streamAllByItemSkuAndCampaignCode(String itemSku, String campaignCode);

  Stream<AdjustmentProductQuota> streamAllByItemSkuAndPickupPointCodeAndCampaignCode(String itemSku, String pickupPointCode, String campaignCode);

  List<AdjustmentProductQuota> findByItemSkuIn(Set<String> itemSkus);
}
