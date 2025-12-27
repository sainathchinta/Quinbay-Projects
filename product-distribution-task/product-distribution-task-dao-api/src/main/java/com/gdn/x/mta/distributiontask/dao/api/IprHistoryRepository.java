package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.IPRHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;


public interface IprHistoryRepository extends JpaRepository<IPRHistory, String> {

  /**
   * Find history of IPR product by storeId and productSku sorted by created date desc
   *
   * @param storeId
   * @param productSku
   * @return
   */
  Page<IPRHistory> findByStoreIdAndProductSkuAndMarkForDeleteFalseOrderByCreatedDateDesc(
      String storeId, String productSku, Pageable pageable);
}
