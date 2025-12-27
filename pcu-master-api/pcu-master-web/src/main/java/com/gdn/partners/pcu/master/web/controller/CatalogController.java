package com.gdn.partners.pcu.master.web.controller;

import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.CatalogApiPath;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.service.CatalogService;
import com.gdn.partners.pcu.master.web.controller.util.ConverterUtil;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;


@Tag(name = "Catalog API")
@RestController
@RequestMapping(value = CatalogApiPath.BASE_PATH)
public class CatalogController {

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Get category summary based on category id")
  @GetMapping(value = CatalogApiPath.GET_SUB_CATEGORIES_BY_CATEGORY_ID, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<CategoryWebResponse> getCategorySummaryByCategoryId(
      @PathVariable("catalogType") String catalogType, @PathVariable("catalogId") String catalogId,
      @PathVariable("parentId") String parentId, @RequestParam(defaultValue = "false") boolean hideNonInventory,
      @RequestParam(defaultValue = "ALL") String filterType,
      @RequestParam(defaultValue = "ALL") String documentFilterType,
      @RequestParam(required = false, defaultValue = "true") boolean ignoreB2bExclusive,
      @RequestParam(defaultValue = "false") boolean filterHalalCategory) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    if (StringUtils.isEmpty(parentId) || StringUtils.isBlank(parentId)) {
      parentId = null;
    }
    List<CategoryWebResponse> categoryWebResponses = null;
    if (StringUtils.isNotBlank(catalogType) && StringUtils.isNotBlank(catalogId)) {
      GetSubCategoriesServiceRequest getSubCategoriesServiceRequest =
          ConverterUtil.getCategorySummaryRequest(catalogType, catalogId, parentId, hideNonInventory, filterType,
              documentFilterType, ignoreB2bExclusive, filterHalalCategory);
      categoryWebResponses =
          catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(getSubCategoriesServiceRequest);
      return new ListBaseResponse<>(null, null, true, requestId, categoryWebResponses,
          new Metadata(0, Integer.MAX_VALUE, Long.valueOf(categoryWebResponses.size())));
    } else {
      return new ListBaseResponse<>(ErrorMessages.EMPTY_CATALOG_ID_OR_CATALOG_TYPE,
          ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, categoryWebResponses, new Metadata());
    }
  }

  @Operation(summary = "Get catalog summary based on catalog id")
  @GetMapping(value = CatalogApiPath.GET_CATALOG_INFO_BY_CATALOG_TYPE, produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<CatalogDetailResponse> getCatalogSummaryBasedOnCatalogType(
      @PathVariable("catalogType") String catalogType) {
    List<CatalogDetailResponse> catalogDetailResponses = catalogService.getCatalogSummaryByCatalogType(catalogType);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        catalogDetailResponses, new Metadata());
  }

  @Operation(summary = "Get List of category hierarchy by category name")
  @GetMapping(value = CatalogApiPath.FILTER_BY_CATALOG_ID_AND_CATEGORY_NAME_AND_STATE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  //@Authorize(mode = "INTERNAL", accessibilities = {"INTERNAL_MASTER_ATTRIBUTE_FILTER"})
  public ListBaseResponse<List<CategorySearchWebResponse>> getListOfCategoryHierarchyByCategoryName(
      @PathVariable String catalogId, @RequestParam("categoryName") String categoryName,
      @RequestParam(defaultValue = "ALL") String filterType,
      @RequestParam(defaultValue = "ALL") String documentFilterType) {
    String requestId = this.clientParameterHelper.getRequestId();
    if (StringUtils.isNotEmpty(catalogId) && StringUtils.isNotEmpty(categoryName)) {
      List<List<CategorySearchWebResponse>> listOfCategoryIdsHierarchy = catalogService.getListOfCategoryHierarchyByCategoryName(
          ConverterUtil.toCategoryHierarchyServiceRequest(categoryName, catalogId, filterType, documentFilterType));
      return new ListBaseResponse<List<CategorySearchWebResponse>>(null, null, true, requestId,
          ConverterUtil.removeDuplicateListEntry(listOfCategoryIdsHierarchy), new Metadata());
    } else {
      return new ListBaseResponse<List<CategorySearchWebResponse>>(ErrorMessages.ERR_EMPTY_CATALOG_ID_OR_CATEGORY_NAME,
          ErrorCategory.INVALID_FORMAT.getMessage(), false, requestId, Collections.EMPTY_LIST, new Metadata());
    }
  }
}
