package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.domain.event.model.BrandHistoryEventModel;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface BrandWipHistoryService {

  /**
   * Generates brandWipHistory for any brandWip changes
   * @param brandWip
   * @param additionalDescription
   * @param username
   * @return
   */
  void generateBrandWipHistory(BrandWip brandWip, String additionalDescription, String username);

  /**
   * Fetches brand wip history by brand request code
   * @param brandRequestCode
   * @param pageable
   * @return
   */
  Page<BrandWipHistoryResponse> findByBrandRequestCode(String brandRequestCode, Pageable pageable);

  /**
   * Fetches brand wip history by brand code
   * @param brandCode
   * @param pageable
   * @return
   */
  Page<BrandWipHistoryResponse> findByBrandCode(String brandCode, Pageable pageable);

  /**
   * Save brand wip history
   * @param brandHistoryEventModel
   */
  void saveBrandWipHistory(BrandHistoryEventModel brandHistoryEventModel);
}
