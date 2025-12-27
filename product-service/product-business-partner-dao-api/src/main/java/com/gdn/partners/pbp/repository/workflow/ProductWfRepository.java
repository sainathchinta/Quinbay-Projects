package com.gdn.partners.pbp.repository.workflow;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.mta.product.entity.ProductWfState;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ProductWfRepository extends JpaRepository<ProductWf, String>{

  ProductWf findByStoreIdAndProductCodeAndStateAndMarkForDeleteFalse(String storeId,
      String productCode, String state);

  List<ProductWf> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId,
      String productCode);

  List<ProductWf> findAllByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId, List<String> productCodes);

  @Query("SELECT NEW com.gdn.mta.product.entity.ProductWfState (pc.productCode, pc.state, pc.postLive) "
      + "FROM ProductCollection pc where "
      + "pc.state = 'IN_PROGRESS' and pc.postLive = true and pc.markForDelete = false and "
      + "pc.stuckProductRetryCount < :retryCount and pc.updatedDate < :updatedTime and pc.imageResized = true "
      + "order by pc.createdDate")
  Page<ProductWfState> getProductCodesByState(@Param("retryCount") Integer retryCount,
      @Param("updatedTime") Date updatedDate, Pageable pageable);

  @Query("SELECT NEW com.gdn.mta.product.entity.ProductWfState (pc.productCode, pc.state, pc.createdDate, pc.updatedDate) "
      + "FROM ProductCollection pc where pc.storeId = :storeId and "
      + "pc.markForDelete = false and pc.stuckProductRetryCount >= :retryCount and pc.state = 'IN_PROGRESS' "
      + "order by pc.createdDate DESC")
  List<ProductWfState> getProductAboveCronJobRetryCount(@Param("storeId") String storeId,
      @Param("retryCount") Integer retryCount);

  long deleteByStoreIdAndProductCode(String storeId, String productCode);
}
