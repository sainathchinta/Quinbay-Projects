package com.gdn.partners.pcu.master.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.request.CategoryHierarchyServiceRequest;
import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.master.service.CatalogService;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.Lists;

@Service
public class CatalogServiceImpl implements CatalogService {

  @Autowired
  private PCBFeign pcbFeign;

  private static final Integer PAGE = 0;
  private static final Integer SIZE = Integer.MAX_VALUE;

  @Override
  public List<CatalogDetailResponse> getCatalogSummaryByCatalogType(String catalogType) {
      ResponseHelper.validateCatalogType(catalogType);
      GdnRestListResponse<CatalogResponse> catalogResponses =
          pcbFeign.getCatalogSummaryByCatalogType(catalogType, PAGE, SIZE);
      ResponseHelper.validateResponse(catalogResponses);
      return ResponseHelper.toCatalogDetailResponseList(catalogResponses.getContent());
  }

  @Override
  public List<CategoryWebResponse> getSubCategoriesByCatalogIdAndParentCategoryId(
      GetSubCategoriesServiceRequest request) {
    ResponseHelper.validateCatalogType(request.getCatalogType());
    GdnRestListResponse<CategoryDTO> categoryDTOGdnRestListResponse =
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(request.getCatalogId(), request.getParentId(), PAGE, SIZE,
            request.getFilterType(), request.getDocumentFilterType(), request.isIgnoreB2bExclusive(),
            request.isFilterHalalCategory());
    ResponseHelper.validateResponse(categoryDTOGdnRestListResponse);
    List<CategoryDTO> categoryDTOS = categoryDTOGdnRestListResponse.getContent();
    categoryDTOS = this.getCategoryDTOSBasedOnCatalogType(request.getCatalogType(), categoryDTOS);
    categoryDTOS = this.removeNonInventoryAndInactiveResponse(categoryDTOS, request.getHideNonInventory(),
        request.getFilterType());
    return ResponseHelper.toCategoryDetailResponseList(categoryDTOS);
  }

  /**
   * get Category DTOS Based On Catalog Type
   *
   * @param catalogType
   * @param categoryDTOS
   * @return
   */
  private List<CategoryDTO> getCategoryDTOSBasedOnCatalogType(String catalogType, List<CategoryDTO> categoryDTOS) {
    List<CategoryDTO> categoryDTOList = new ArrayList<>();
    if (CatalogType.SALES_CATALOG.name().equals(catalogType) || CatalogType.B2B_SALES_CATALOG.name().equals(catalogType)) {
        categoryDTOList = categoryDTOS.stream().sorted(
            Comparator.comparing(CategoryResponse::getSequence, Comparator.nullsLast(Comparator.naturalOrder())))
          .collect(Collectors.toList());
    } else if (CatalogType.MASTER_CATALOG.name().equals(catalogType)) {
      categoryDTOList =
          categoryDTOS.stream().sorted(Comparator.comparing(CategoryResponse::getName, String.CASE_INSENSITIVE_ORDER))
              .collect(Collectors.toList());
    }
    return categoryDTOList;
  }

  /**
   * Remove non inventory and inactive responses
   *
   * @param categoryDTOS
   * @param hideNonInventory
   * @param filterType
   * @return
   */
  private List<CategoryDTO> removeNonInventoryAndInactiveResponse(List<CategoryDTO> categoryDTOS, boolean hideNonInventory,
      String filterType) {
    Stream<CategoryDTO> categoryDTOStream = categoryDTOS.stream();
    if(hideNonInventory){
      categoryDTOStream = categoryDTOStream
          .filter(categoryDTO -> !Constants.NON_INVENTORY_CATEGORY_CODE.equals(categoryDTO.getCategoryCode()));
    }
    if (Constants.ACTIVE.equals(filterType)) {
      categoryDTOStream = categoryDTOStream.filter(CategoryResponse::isActivated);
    } else if(Constants.IN_ACTIVE.equals(filterType)){
      categoryDTOStream = categoryDTOStream.filter(categoryDTO -> !categoryDTO.isActivated());
    }
    return categoryDTOStream.collect(Collectors.toList());
  }

  @Override
  public List<List<CategorySearchWebResponse>> getListOfCategoryHierarchyByCategoryName(
      CategoryHierarchyServiceRequest categoryHierarchyServiceRequest) {
    GdnRestListResponse<CategoryResponse> categories = pcbFeign
        .findCategorySummaryByName(categoryHierarchyServiceRequest.getCategoryName(), 0, Integer.MAX_VALUE,
            categoryHierarchyServiceRequest.getFilterType(), categoryHierarchyServiceRequest.getDocumentFilterType());
    ResponseHelper.validateResponse(categories);
    List<CategoryResponse> categoryResponses =
        filterCategoryResponseByFilterTypeAndCatalogId(categories, categoryHierarchyServiceRequest.getCatalogId());
    return categoryResponses.stream()
        .map(this::getListOfCategoryHierarchy)
        .collect(Collectors.toList());
  }

  /**
   * Filter categoryResponses based on catalogId
   *
   * @param categories
   * @param catalogId
   * @return list of category responses
   */
  private List<CategoryResponse> filterCategoryResponseByFilterTypeAndCatalogId(
      GdnRestListResponse<CategoryResponse> categories, String catalogId) {
    return categories.getContent().stream()
        .filter(categoryResponse -> catalogId.equals(categoryResponse.getCatalog().getId()))
        .collect(Collectors.toList());
  }

  /**
   * Get list of category id searched by categoryResponse
   *
   * @param categoryResponse
   * @return list of categoryIds
   */

  private List<CategorySearchWebResponse> getListOfCategoryHierarchy(CategoryResponse categoryResponse) {
    List<CategorySearchWebResponse> categorySearchWebResponses = generateListOfCategoryHierarchy(categoryResponse);
    if (0 == categoryResponse.getChildCount()) {
      categorySearchWebResponses.stream()
          .filter(CategorySearchWebResponse -> CategorySearchWebResponse.getId().equals(categoryResponse.getId()))
          .forEach(CategorySearchWebResponse -> CategorySearchWebResponse.setLeafNode(true));
    }
    return Lists.reverse(categorySearchWebResponses);
  }

  /**
   * Generate list of categorySearchWebResponses recursively
   *    * Generated pattern = child -> parent -> grandparent -> ...
   *
   * @param categoryResponse
   * @return
   */
  private List<CategorySearchWebResponse> generateListOfCategoryHierarchy(CategoryResponse categoryResponse) {
    List<CategorySearchWebResponse> categorySearchWebResponses = new ArrayList<>();
    GdnRestSingleResponse<CategoryDetailResponse> categoryDetailResponse = null;
    if(StringUtils.isNotEmpty(categoryResponse.getParentCategoryId())){
      categoryDetailResponse = pcbFeign.getCategoryDetail(categoryResponse.getParentCategoryId());
      ResponseHelper.validateResponse(categoryDetailResponse);
    }
    CategorySearchWebResponse searchWebResponse = new CategorySearchWebResponse();
    BeanUtils.copyProperties(categoryResponse, searchWebResponse);
    categorySearchWebResponses.add(searchWebResponse);
    if (Objects.isNull(categoryDetailResponse)) {
      return categorySearchWebResponses;
    }
    categorySearchWebResponses.addAll(generateListOfCategoryHierarchy(categoryDetailResponse.getValue()));
    return categorySearchWebResponses;
  }
}
