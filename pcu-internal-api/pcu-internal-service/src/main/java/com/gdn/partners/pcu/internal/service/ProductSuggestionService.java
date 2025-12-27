package com.gdn.partners.pcu.internal.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;

public interface ProductSuggestionService {

  /**
   * Get supported Blibli Categories by Pristine
   *
   * @return
   */
  Map<String, Set<String>> getSupportedBlibliCategoriesByPristine();

  /**
   * get suggested product codes for screening product
   *
   * @param productCode
   * @param category
   * @param pageable
   * @return
   * @throws Exception
   */
  List<ProductCodeResponse> getPCBProductCodes(String productCode, String category, Pageable pageable);
}
