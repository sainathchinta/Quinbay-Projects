package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.CategoryCodeAndUmkmFlagDTO;
import com.gdn.x.productcategorybase.repository.CategoryReferenceRepository;
import com.gdn.x.productcategorybase.service.CategoryReferenceService;

@Service
public class CategoryReferenceServiceImpl implements CategoryReferenceService {

  private static final String SALES_CATALOG_CODE = "12051";

  @Autowired
  private CategoryReferenceRepository categoryReferenceRepository;

  @Value("${b2b.sales.catalog.code}")
  private String b2bSalesCatalogCode;

  @Override
  public ProductSalesCategoryMapping getSalesCategoryReferenceByMasterCategory(String oldCategoryId,
      String newCategoryId, boolean ignoreHalalCategories) {
    ProductSalesCategoryMapping productSalesCategoryMapping = new ProductSalesCategoryMapping();
    List<String> newUmkmSalesCategoryCode = new ArrayList<>();
    List<String> newSalesCategoryCode = new ArrayList<>();
    List<String> newB2bSalesCategoryCode = new ArrayList<>();
    List<String> oldSalesCategoryCodes = new ArrayList<>();
    List<String> oldB2bSalesCategoryCode = new ArrayList<>();
    if (StringUtils.isEmpty(oldCategoryId) || StringUtils.isEmpty(newCategoryId)) {
      return null;
    }
    List<CategoryCodeAndUmkmFlagDTO> responseForOldSalesCategories;
    if (ignoreHalalCategories) {
      responseForOldSalesCategories = categoryReferenceRepository
          .findAllSalesCategoryCodesByMasterCategoryIdAndHalalCategory(oldCategoryId, false);
    } else {
      responseForOldSalesCategories = categoryReferenceRepository.findAllSalesCategoryCodesByMasterCategoryId(oldCategoryId);
    }
    responseForOldSalesCategories.forEach(categoryCodeAndUmkmFlagObject -> {
      if (b2bSalesCatalogCode.equals(categoryCodeAndUmkmFlagObject.getCatalog().getCatalogCode())) {
        oldB2bSalesCategoryCode.add(String.valueOf(categoryCodeAndUmkmFlagObject.getCategoryCode()));
      } else if (SALES_CATALOG_CODE.equals(categoryCodeAndUmkmFlagObject.getCatalog().getCatalogCode())) {
        oldSalesCategoryCodes.add(String.valueOf(categoryCodeAndUmkmFlagObject.getCategoryCode()));
      }
    });
    List<CategoryCodeAndUmkmFlagDTO> response;
    if (ignoreHalalCategories) {
      response = categoryReferenceRepository.findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryIdAndHalalCategory(
          newCategoryId, false);
    } else {
      response = categoryReferenceRepository.findSalesCategoryCodesByMarkForDeleteFalseAndMasterCategoryId(
          newCategoryId);
    }
    response.forEach(categoryCodeAndUmkmFlagObject -> {
      if (categoryCodeAndUmkmFlagObject.isUmkm()) {
        newUmkmSalesCategoryCode.add(String.valueOf(categoryCodeAndUmkmFlagObject.getCategoryCode()));
      } else {
        newSalesCategoryCode.add(String.valueOf(categoryCodeAndUmkmFlagObject.getCategoryCode()));
      }
      if (b2bSalesCatalogCode.equals(categoryCodeAndUmkmFlagObject.getCatalog().getCatalogCode())) {
        newB2bSalesCategoryCode.add(String.valueOf(categoryCodeAndUmkmFlagObject.getCategoryCode()));
      }
    });
    newSalesCategoryCode.removeAll(newB2bSalesCategoryCode);
    productSalesCategoryMapping.setOldSalesCategoryCodes(oldSalesCategoryCodes);
    productSalesCategoryMapping.setOldB2bSalesCategoryCodes(oldB2bSalesCategoryCode);
    productSalesCategoryMapping.setNewSalesCategoryCodes(newSalesCategoryCode);
    productSalesCategoryMapping.setNewUmkmSalesCategoryCodes(newUmkmSalesCategoryCode);
    productSalesCategoryMapping.setNewB2bSalesCategoryCodes(newB2bSalesCategoryCode);
    return productSalesCategoryMapping;
  }

  @Override
  public List<String> getSalesCategoryIdByMasterCategoryId(String storeId, String categoryId) {
    return categoryReferenceRepository.findIdsByStoreIdAndMasterCategoryReferenceId(storeId, categoryId);
  }

  @Override
  public List<String> getMasterCategoryIdBySalesCategoryId(String storeId, String categoryId) {
    return categoryReferenceRepository.findIdsByStoreIdAndSalesCategoryReferenceId(storeId, categoryId);
  }
}
