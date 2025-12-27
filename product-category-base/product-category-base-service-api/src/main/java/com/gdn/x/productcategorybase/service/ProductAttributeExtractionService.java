package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.dto.CategoryProductAttributeExtractedDTO;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;
import com.gdn.x.productcategorybase.entity.ProductAttributeExtracted;

import java.util.List;

public interface ProductAttributeExtractionService {

  /**
   * Backfill product for attribute extraction by status
   * @param storeId
   * @param username
   * @param status
   */
  List<CategoryProductAttributeExtractedDTO> publishProductsForBackfillingByStatus(String storeId, String username, String status);

  /** publish AttributeExtractionEvent
   * @param storeId
   * @param username
   * @param category
   * @param updatedProductAttributeExtracted
  */
  void publishAttributeExtractionEvent(String storeId, String username, Category category,
    ProductAttributeExtracted updatedProductAttributeExtracted);

    /**
     * Calls matrix system and update extracted attribute
     * @param productAttributeExtractionModel
     */
  void updateExtractedAttributesForBackfilling(ProductAttributeExtractionModel productAttributeExtractionModel);

  /**
   * Add products to attribute extraction table
   *
   * @param productCode
   * @param category
   */
  void addProductsToProductAttributeExtraction(String productCode, Category category);

  /**
   * Fetch and set products from attribute extraction table
   *
   * @param productCode not null
   * @param category not null
   * @param storeId storeId
   */
  ProductAttributeExtracted fetchAndSetProductAttributeExtraction(String productCode,
    Category category, String storeId);

  /**
   * save products to attribute extraction table
   *
   * @param productAttributeExtracted not null
   */
  void saveProductToAttributeExtraction(ProductAttributeExtracted productAttributeExtracted);
}
