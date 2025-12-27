package com.gdn.partners.pbp.repository.eventstore;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;

public interface ProductItemEventStoreRepository extends
    JpaRepository<ProductItemEventStore, String> {
  List<ProductItemEventStore> findByStoreIdAndItemSku(String storeId, String itemSku);

  Page<ProductItemEventStore> findByStoreIdAndItemSku(String storeId, String itemSku,
      Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query("DELETE FROM EventStore e WHERE e.id IN (SELECT pie.id FROM #{#entityName} pie WHERE pie.itemSku = :itemSku AND pie.eventTimestamp < :lastEventTimestamp)")
  void deleteByItemSkuAndEventTimestampLessThan(
      @Param("itemSku") String itemSku, @Param("lastEventTimestamp") Date lastEventTimestamp);

  @Query("SELECT e FROM #{#entityName} e WHERE e.itemSku = :itemSku AND e.eventTimestamp < :lastEventTimestamp")
  List<ProductItemEventStore> findByItemSkuAndEventTimestampLessThan(
      @Param("itemSku") String itemSku, @Param("lastEventTimestamp") Date lastEventTimestamp);

}
