package com.gdn.partners.pbp.repository.eventstore;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;

public interface Level2InventoryEventStoreRepository extends
    JpaRepository<Level2InventoryEventStore, String> {
  List<Level2InventoryEventStore> findByStoreIdAndLevel2IdAndMerchantCode(String storeId,
      String level2Id, String merchantCode);

  Page<Level2InventoryEventStore> findByStoreIdAndMerchantCode(String storeId, String merchantCode,
      Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query("DELETE FROM EventStore e WHERE e.id IN (SELECT pie.id FROM #{#entityName} pie WHERE pie.level2Id = :itemSku AND pie.eventTimestamp < :lastEventTimestamp)")
  void deleteByItemSkuAndEventTimestampLessThan(@Param("itemSku") String itemSku,
      @Param("lastEventTimestamp") Date lastEventTimestamp);

  @Query("SELECT e FROM #{#entityName} e WHERE e.level2Id = :itemSku AND e.eventTimestamp < :lastEventTimestamp")
  List<Level2InventoryEventStore> findByItemSkuAndEventTimestampLessThan(
      @Param("itemSku") String itemSku, @Param("lastEventTimestamp") Date lastEventTimestamp);

}
