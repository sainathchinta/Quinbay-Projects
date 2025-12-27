package com.gdn.partners.pcu.internal.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.ContextStartedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.dto.response.CategoryParentResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 15/01/2019 AD.
 */
@Service
@Slf4j
public class CategoryServiceImpl implements CategoryService{

  private Map<String, String> categoryToFinalParentMap;

  private static final double DAYS_HOURS = 24.0;
  private static final String NEW_SPAN = "newSpan";

  @Value("${default.internal.activation.period:72}")
  private int internalActivationPeriod;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

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

  /**
   * Get category to final parent map when the deployment is finished
   *
   */
  @EventListener(ContextStartedEvent.class)
  public void onApplicationEvent() {
    fetchCategoryToFinalParentMap();
  }

  private void fetchCategoryToFinalParentMap() {
    categoryToFinalParentMap = new HashMap<>();
    GdnRestListResponse<CategoryParentResponse> response = pcbFeign.getCategoriesAndFinalCategoryMapping();
    ResponseHelper.validateResponse(response);
    for (CategoryParentResponse categoryParentResponse : response.getContent()) {
      categoryToFinalParentMap
          .put(categoryParentResponse.getCategoryId(), categoryParentResponse.getParentCategoryId());
    }
    log.info("Category to final parent mapping: total {} ", categoryToFinalParentMap.size());
  }

  /**
   * To get category to final parent map
   * @return
   */
  @Override
  public Map<String, String> getCategoryToFinalParentMap() {
    if(Objects.isNull(categoryToFinalParentMap)) {
      fetchCategoryToFinalParentMap();
    }
    return categoryToFinalParentMap;
  }

  @Override
  public String getFinalParentCategoryAndUpdateMap(String categoryId) {
    GdnRestSingleResponse<SingleObjectResponse> response = pcbFeign.getFinalParentCategory(categoryId);
    ResponseHelper.validateResponse(response);
    String finalParentCategory = String.valueOf(response.getValue().getValue());
    log.debug("updating category mapping with category {} and final parent category {}", categoryId,
        finalParentCategory);
    updateCategoryToFinalParentMap(categoryId, finalParentCategory);
    if (StringUtils.isEmpty(finalParentCategory)) {
      log.error("Not able to find parent category of category {} after calling database", categoryId);
    }
    return finalParentCategory;
  }

  @Override
  public List<CategoryTreeNodeResponse> fetchCategoryTreeWithReviewConfig() {
    GdnRestListResponse<CategoryTreeNodeResponse> response = this.pcbFeign.getCategoryTreeWithReviewConfig();
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }

  @Override
  public void saveRestrictedKeywordFile(MultipartFile multipartFile, String processType, String requestId,
      String storeId, String username) throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(multipartFile, requestId, processType);
    BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel =
        RequestHelper.toBulkRestrictedKeywordUploadModel(storeId,
            new StringBuilder(baseDirPath).append(multipartFile.getOriginalFilename()).toString(), processType,
            requestId, username);
    kafkaPublisher.send(DomainEventName.BULK_RESTRICTED_KEYWORD_UPLOAD_EVENT,
        bulkRestrictedKeywordUploadModel.getBulkProcessCode(), bulkRestrictedKeywordUploadModel);
  }

  /**
   * Update category to final parent map
   *
   * @param categoryId
   * @param finalParentCategoryId
   */
  private void updateCategoryToFinalParentMap(String categoryId, String finalParentCategoryId) {
    this.categoryToFinalParentMap.put(categoryId, finalParentCategoryId);
  }

}
