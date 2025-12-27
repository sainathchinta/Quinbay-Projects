package com.gdn.mta.product.service;

import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gda.mta.product.dto.CategoryProductCodeMappingRequest;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gda.mta.product.dto.RecategorizationRequest;
import com.gdn.mta.product.entity.Recategorization;

/**
 * Created by hardikbohra on 10/05/18.
 */
public interface RecategorizationService {

  /**
   * to save recategorization
   *
   * @param recategorization must not be blank
   * @return recatId
   * @throws Exception
   */
  String save(RecategorizationRequest recategorization) throws Exception;

  /**
   * to find recategorization by recatId
   *
   * @param recatId must not be blank
   * @return recategorization
   * @throws Exception
   */
  Recategorization findById(String recatId) throws Exception;

  /**
   * to save audit mapping of user and businessPartner to category
   *
   * @param saveRequest must not be blank
   * @throws Exception
   */
  void saveCategoryToUserMapping(CategoryUserMappingRequest saveRequest) throws Exception;

  /**
   * to save audit mapping of product code to category
   *
   * @param saveRequest must not be blank
   * @throws Exception
   */
  void saveCategoryToProductCodeMapping(CategoryProductCodeMappingRequest saveRequest) throws Exception;

  /**
   * to save audit mapping of product sku to category
   *
   * @param saveRequest must not be blank
   * @throws Exception
   */
  void saveCategoryToProductSkuMapping(CategoryProductSkuMappingRequest saveRequest) throws Exception;

  /**
   * to save audit mapping of product sku to sales catalog
   *
   * @param saveRequest must not be blank
   * @throws Exception
   */
  void saveProductSkuToSalesCatalogMapping(ProductSkuToSalesCatalogMappingRequest saveRequest) throws Exception;

  /**
   * to process category and product code mapping
   *
   * @param recategorizationRequest must not be blank
   */
  void processCategoryToProductCodeMapping(BulkDataForRecategorizationRequest recategorizationRequest);
}
