package com.gdn.partners.pcu.external.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.CategoryService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.CategoryWebResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 14/12/2018 AD.
 */
@Slf4j
@Service
public class CategoryServiceImpl implements CategoryService {

  private static final double DAYS_HOURS = 24.0;

  @Value("${default.category.batch.size}")
  private int categoryBatchSize;

  @Value("${default.internal.activation.period:72}")
  private int internalActivationPeriod;

  @Value("${sort.allowed.attribute.values}")
  private boolean sortAllowedAttributeValues;

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public String findInternalActivationIntervalInDaysByCategoryCode(String categoryCode) {
    GdnRestListResponse<CategoryResponse> categoryResponses =
        pcbFeign.filterCategoryHierarchyByCategoryCode(categoryCode);
    ResponseHelper.validateResponse(categoryResponses);
    Integer internalActivationInterval = categoryResponses.getContent().stream()
        .filter(categoryResponse-> Objects.isNull(categoryResponse.getParentCategoryId())).findFirst()
        .map(CategoryResponse :: getInternalActivationInterval)
        .orElse(internalActivationPeriod);
    return String.valueOf((int)Math.ceil((internalActivationInterval/DAYS_HOURS)));
  }

  @Override
  public CategoryDetailResponse getCategoryDetail(String categoryId) {
    GdnRestSingleResponse<CategoryDetailResponse> response = pcbFeign.getCategoryDetail(categoryId);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public AttributeResponse getAttributeDetail(String attributeId) {
    GdnRestSingleResponse<AttributeResponse> response =
        pcbFeign.getAttributeDetail(attributeId, sortAllowedAttributeValues);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public List<CategoryWebResponse> getCategoriesByCategoryCodes(List<String> categoryCodes) throws Exception {
    int size = categoryCodes.size();
    int i = 0;
    List<CategoryWebResponse> categoryWebResponses = new ArrayList<>();
    //implementing batching to fetch the category details for category Codes.
    while (i * categoryBatchSize < size) {
      CategoryMultipleIdRequest bulkRequest = new CategoryMultipleIdRequest();
      //Check to avoid ArrayIndexOutOfBoundException when fetching the last batch of category codes
      if (((i + 1) * categoryBatchSize) >= size) {
        bulkRequest.setCategoryCode(categoryCodes.subList(i * categoryBatchSize, size));
      } else {
        bulkRequest.setCategoryCode(categoryCodes.subList(i * categoryBatchSize, (i + 1) * categoryBatchSize));
      }
      log.info("Getting the category details for the request : {}", bulkRequest);
      GdnRestListResponse<CategoryDTO> response = pcbFeign
          .getCategoriesByCategoryCodes(0, Integer.MAX_VALUE, Boolean.TRUE.toString(),
              new ObjectMapper().writeValueAsString(bulkRequest));
      ResponseHelper.validateResponse(response);
      categoryWebResponses.addAll(ResponseHelper.toCategoryWebResponseList(response.getContent()));
      i++;
    }
    categoryWebResponses.stream().forEach(
        categoryWebResponse -> ResponseHelper.clearPIIConfidentialInformationForCategoryResponse(categoryWebResponse));
    return categoryWebResponses;
  }
}
