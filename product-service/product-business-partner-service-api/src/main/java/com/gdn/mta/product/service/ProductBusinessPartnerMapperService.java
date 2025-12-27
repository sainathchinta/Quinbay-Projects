/**
 * 
 */
package com.gdn.mta.product.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;

/**
 * @author Poornima
 */
public interface ProductBusinessPartnerMapperService {

  /**
   * @param storeId store id
   * @param activated activated flag
   * @param viewable viewable flag
   * @param isSearch is search flag
   * @param searchCriteria the search criteria
   * @param pageable page information
   * @return page of product business partner mapper
   * @throws Exception when failed to find product business partner mapper
   */
  Page<ProductBusinessPartnerMapper> findByActivatedAndViewableTrue(String storeId,
      boolean activated, boolean viewable, boolean isSearch, String searchCriteria,
      Pageable pageable) throws Exception;
}
