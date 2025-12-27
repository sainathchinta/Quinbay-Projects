package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.ProductIPR;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Date;
import java.util.List;

public interface IprRepository extends JpaRepository<ProductIPR, String> {

  Page<ProductIPR> findByStoreIdAndBusinessPartnerCodeAndStateInAndMarkForDeleteFalse(
    String storeId, String businessPartnerCode, List<String> state, Pageable pageable)
    throws Exception;

  /**
   * to find existing product for product sku
   *
   * @param productSku String
   * @return ProductIPR
   */
  ProductIPR findByProductSku(String productSku);

  /**
   * find product by product sku and mfd flag
   *
   * @param productSku    String
   * @param markForDelete boolean
   * @return ProductIPR
   */
  ProductIPR findByProductSkuAndMarkForDelete(String productSku, boolean markForDelete);

  /**
   * to find existing products for given list of productSkus
   *
   * @param productSkus
   * @return
   */
  List<ProductIPR> findByProductSkuIn(List<String> productSkus);

  /**
   * Find list of evidence requested product before x days
   *
   * @param storeId
   * @param state
   * @param evidenceRequesedtDate
   * @return
   */
  Page<ProductIPR> findByStoreIdAndStateAndEvidenceRequestedDateBeforeAndMarkForDeleteFalse(
    String storeId, String state, Date evidenceRequesedtDate, Pageable pageable);
}
