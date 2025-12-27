package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

public interface AdjustmentProductRepository extends MongoRepository<AdjustmentProduct, String> {

    @Query(value = "{'itemSku' : ?0, 'endDate': {$gt : ?1}}", fields = "{'campaignCode' : 1}")
    Stream<AdjustmentProduct> getNotEndedCampaignCodesByItemSku(String itemSku, Long endDate);

    @Query(value = "{'itemSku' : ?0, 'pickupPointCode' : ?1, 'endDate': {$gt : ?2}}", fields = "{'campaignCode' : 1}")
    Stream<AdjustmentProduct> getNotEndedCampaignCodesByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode, Long endDate);

    AdjustmentProduct findFirstByItemSkuAndPickupPointCodeAndCampaignCodeAndEndDateAfterOrderByPriorityAscValueDesc(String itemSku, String pickupPointCode, String campaignCode, Long endDate);

    AdjustmentProduct findFirstByItemSkuAndPickupPointCodeAndActivatedTrueAndPromoCampaignAndStartDateBeforeAndEndDateAfterOrderByPriorityAscValueDesc(String itemSku, String pickupPointCode, boolean promoCampaign, Long startDate, Long endDate);

    Stream<AdjustmentProduct> streamAllByItemSkuAndPickupPointCodeAndActivatedTrueAndPromoCampaignAndStartDateBeforeAndEndDateAfterOrderByPriorityAscValueDesc(String itemSku, String pickupPointCode, boolean promoCampaign, Long startDate, Long endDate);

    List<AdjustmentProduct> findByItemSkuIn(Set<String> itemSkus);
}
