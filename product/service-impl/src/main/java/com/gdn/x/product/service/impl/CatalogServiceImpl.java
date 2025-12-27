package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

@Service
public class CatalogServiceImpl implements CatalogService {

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Override
  public List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchy(String username,
      String requestId, List<String> categoryCodes) throws Exception {
    List<List<CategoryResponse>> categoryDatas =
        this.productHelperService.constructListOfCategoriesListOfProduct(requestId, username,
            categoryCodes);
    return this.objectConverterService.convertToListOfItemCatalog(categoryDatas);
  }

  @Override
  public List<ItemCatalogVOV2> getItemCatalogsWithCategoryHierarchyV2(String username, String requestId,
      List<String> categoryCodes) throws Exception {
    List<List<CategoryResponse>> categoryDatas =
        this.productHelperService.constructListOfCategoriesListOfProduct(requestId, username,
            categoryCodes);
    return this.objectConverterService.convertToListOfItemCatalogV2(categoryDatas);
  }

  @Override
  public List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchy(String username,
      String requestId, Product product) throws Exception {
    List<String> categoryCodes = new ArrayList<>();
    if (Objects.isNull(product.getMasterCatalog()) && Objects.nonNull(product.getMasterDataProduct())) {
      product.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
    }
    if (Objects.nonNull(product.getMasterCatalog()) && Objects.nonNull(product.getMasterCatalog().getCategory())) {
      categoryCodes.add(product.getMasterCatalog().getCategory().getCategoryCode());
    }
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      for (Category salesCategory : salesCatalog.getListOfCategories()) {
        categoryCodes.add(salesCategory.getCategoryCode());
      }
    }
    return this.getItemCatalogsWithCategoryHierarchy(username, requestId, categoryCodes);
  }

  @Override
  public List<ItemCatalogVO> getItemCatalogsWithCategoryHierarchyExistsInPCB(String username,
      String requestId, List<String> categoryCodes) throws Exception {
    List<List<CategoryResponse>> categoryDatas = this.productHelperService
        .getCategoryResponseListByCategoryCodesForProducts(requestId, username, categoryCodes);
    return this.objectConverterService.convertToListOfItemCatalog(categoryDatas);
  }

  @Override
  public Map<String, List<ItemCatalogVO>> getCategoryCodeToItemCatalogsMap(String username, String requestId,
      List<String> categoryCodes) {
    return this.productHelperService
        .constructMapOfCategoriesAndCategoryCode(requestId, username, categoryCodes).entrySet()
        .stream()
        .collect(Collectors.toMap(Map.Entry::getKey,
            entry -> objectConverterService.
                convertToListOfItemCatalog(Collections.singletonList(entry.getValue()))));
  }

  @Override
  public List<ItemCatalogVO> getItemCatalogsByProduct(String username,
      String requestId, Product product) throws Exception {
    List<String> categoryCodes = new ArrayList<String>();
    if (!product.isSynchronized() && Objects.nonNull(product.getMasterDataProduct())
        && Objects.nonNull(product.getMasterDataProduct().getMasterCatalog()) &&
        Objects.nonNull(product.getMasterDataProduct().getMasterCatalog().getCategory()) &&
        StringUtils.isNotEmpty(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode())) {
      categoryCodes.add(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
    } else if (StringUtils.isNotEmpty(product.getCategoryCode())) {
      categoryCodes.add(product.getCategoryCode());
    }
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      for (Category salesCategory : salesCatalog.getListOfCategories()) {
        categoryCodes.add(salesCategory.getCategoryCode());
      }
    }
    return this.getItemCatalogsWithCategoryHierarchy(username, requestId, categoryCodes);
  }
}
