package com.gdn.aggregate.platform.module.product.listener.repository.raw;

import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

import java.util.List;
import java.util.stream.Stream;

public interface FlashsaleProductRepository extends MongoRepository<FlashsaleProduct, String> {

  @Query(value = "{'itemSku' : ?0, 'schedule.end': {$gt : ?1}}", fields = "{'campaignCode' : 1}")
  Stream<FlashsaleProduct> getNotEndedCampaignCodesByItemSku(String itemSku, Long endDate);

  @Query(value = "{'itemSku' : ?0, 'pickupPointCode' : ?1, 'schedule.end': {$gt : ?2}}", fields = "{'campaignCode' : 1}")
  Stream<FlashsaleProduct> getNotEndedCampaignCodesByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode, Long endDate);

  FlashsaleProduct findFirstByItemSkuAndCampaignCodeAndSchedule_EndAfterOrderBySchedule_StartAsc(String itemSku, String campaignCode, Long endDate);

  FlashsaleProduct findFirstByItemSkuAndPickupPointCodeAndCampaignCodeAndSchedule_EndAfterOrderBySchedule_StartAsc(String itemSku, String pickupPointCode, String campaignCode, Long endDate);

  boolean existsByProductSkuAndItemSkuNotNull(String productSku);

  FlashsaleProduct findFirstByProductSkuAndItemSkuNull(String productSku);

  Stream<FlashsaleProduct> streamAllByProductSkuAndSchedule_TimeBasedAndSchedule_EndAfter(String productSku, boolean timeBased, long end);

  List<FlashsaleProduct> findAllByProductSku(String productSku);
}
