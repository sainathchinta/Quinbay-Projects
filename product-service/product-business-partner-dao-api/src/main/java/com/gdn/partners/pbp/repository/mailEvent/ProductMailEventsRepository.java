package com.gdn.partners.pbp.repository.mailEvent;

import com.gdn.partners.pbp.entity.mailEvent.ProductMailEvents;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

@Repository
public interface ProductMailEventsRepository extends JpaRepository<ProductMailEvents, String> {

  String DELETE_MAIL_EVENTS_BY_DAYS = "DELETE FROM prd_product_mail_events WHERE id IN (SELECT id"
    + " FROM prd_product_mail_events WHERE created_date < CURRENT_TIMESTAMP - INTERVAL '1 day' * :interval LIMIT :batchSize)";

  @Query("Select Distinct p.businessPartnerCode from ProductMailEvents p where p.storeId = ?1 and "
      + "p.createdDate >= ?2")
  List<String> findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(String storeId, Date date);

  List<ProductMailEvents> findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(
      String storeId, String businessPartnerCode, ProductMailEventsEnum events, Date createdDate);

  List<ProductMailEvents> findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(
      String storeId, String businessPartnerCode, List<ProductMailEventsEnum> events, Date createdDate);

  List<ProductMailEvents> findByStoreIdAndProductSkuAndEventsAndCreatedDateGreaterThan(String storeId,
      String productSku, ProductMailEventsEnum events, Date createdDate);

  ProductMailEvents findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(String storeId,
      String productSku, ProductMailEventsEnum events);

  ProductMailEvents findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(String storeId,
      String productSku, ProductMailEventsEnum events);

  @Query("Select Distinct p.businessPartnerCode from ProductMailEvents p where p.storeId = ?1 and "
      + "p.createdDate >= ?2 and p.events in ?3")
  List<String> findDistinctBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(String storeId, Date date,
      List<ProductMailEventsEnum> events);

  @Modifying
  @Transactional
  @Query(value = ProductMailEventsRepository.DELETE_MAIL_EVENTS_BY_DAYS, nativeQuery = true)
  int deleteOldRecords(@Param("interval") int interval, @Param("batchSize") int batchSize);


}
