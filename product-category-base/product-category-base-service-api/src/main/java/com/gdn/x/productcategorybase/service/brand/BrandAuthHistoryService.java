package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface BrandAuthHistoryService {

  /**
   * @param brandAuthHistoryRequest
   * @param storeId
   * @param pageable
   * @return
   */
  Page<BrandAuthHistoryResponse> findByBrandCodeAndSellerCode(String storeId,
    BrandAuthHistoryRequest brandAuthHistoryRequest, Pageable pageable);

  /**
   *
   * @param storeId
   * @param brandAuthHistoryRequest
   * @param page
   * @param size
   * @return
   */
  Page<BrandAuthHistoryResponse> getBrandAuthHistory(String storeId,
    BrandAuthHistoryRequest brandAuthHistoryRequest, int page, int size);

  /**
   * @param brandAuthorisationHistory
   * @return
   */
  BrandAuthorisationHistory saveBrandAuthHistory(
    BrandAuthorisationHistory brandAuthorisationHistory);


  /**
   * Generate brand auth history
   *
   * @param brandAuthorisationHistory
   * @param
   */
  BrandAuthDomainEventModel generateBrandAuthHistoryDomainEventModel(
    BrandAuthorisationHistory brandAuthorisationHistory);

}
