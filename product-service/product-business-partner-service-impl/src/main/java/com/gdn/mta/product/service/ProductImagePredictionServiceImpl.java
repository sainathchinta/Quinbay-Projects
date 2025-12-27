package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.PredictionCategoryMappingUpdateRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.ProductImagePredictionRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
@Lazy
public class ProductImagePredictionServiceImpl implements ProductImagePredictionService {

  private static final String IMAGE = "IMAGE";
  private static final String TEXT = "TEXT";
  private static final int MINIMUM_THRESHOLD = 0;
  private static final int MAXIMUM_THRESHOLD = 100;

  @Autowired
  private ProductImagePredictionRepository productImagePredictionRepository;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductOutbound productOutbound;

  @Override
  @Transactional(readOnly = false)
  public void insert(ProductImagePrediction productImagePrediction) {
    validateProductSystemParameterRequest(productImagePrediction);
    productImagePredictionRepository.save(productImagePrediction);
    getProductImagePredictionService().evictPredictionTypeCache(productImagePrediction.getStoreId());
  }

  private void validateProductSystemParameterRequest(ProductImagePrediction productImagePrediction) {
    checkArgument(StringUtils.isNotBlank(productImagePrediction.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productImagePrediction.getPredictionType()),
        ErrorMessages.PREDICTION_TYPE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productImagePrediction.getDisplayName()),
        ErrorMessages.PREDICTION_NAME_MUST_BE_EMPTY);
    checkArgument(
        !(productImagePrediction.isForceReview() && productImagePrediction.isNeedRevisionEnabled()),
        ErrorMessages.BOTH_FORCE_REVIEW_AND_NEED_REVISION_ENABLED_CANNOT_BE_TRUE);
  }

  @Override
  @Transactional(readOnly = false)
  public void update(ProductImagePrediction productImagePrediction) {
    validateProductSystemParameterRequest(productImagePrediction);
    ProductImagePrediction response = productImagePredictionRepository
        .findByStoreIdAndPredictionType(productImagePrediction.getStoreId(),
            productImagePrediction.getPredictionType());
    checkState(Objects.nonNull(response), ErrorMessages.IMAGE_PREDICTION_TYPE_NOT_FOUND);
    response.setDisplayNameIn(productImagePrediction.getDisplayNameIn());
    response.setConfidenceThreshold(productImagePrediction.getConfidenceThreshold());
    response.setPredictionWeightage(productImagePrediction.getPredictionWeightage());
    response.setForceReview(productImagePrediction.isForceReview());
    response.setMarkForDelete(productImagePrediction.isMarkForDelete());
    response.setNeedRevisionConfidenceThreshold(productImagePrediction.getNeedRevisionConfidenceThreshold());
    response.setNeedRevisionEnabled(productImagePrediction.isNeedRevisionEnabled());
    productImagePredictionRepository.save(response);
  }

  @Override
  @Transactional(readOnly = false)
  public ProductImagePrediction updateImagePredictionAndCategoryMapping(String storeId,
      ProductImagePredictionAndCategoryMappingRequest productImagePrediction) throws Exception {
    ProductImagePrediction response = productImagePredictionRepository.findByStoreIdAndPredictionType(storeId,
        productImagePrediction.getPredictionType());
    validateRequestFromExistingProductImagePrediction(productImagePrediction, response);
    response.setForceReview(productImagePrediction.isRuleEnabled());
    response.setConfidenceThreshold(productImagePrediction.getConfidenceThreshold());
    response.setTextConfidenceThreshold(productImagePrediction.getTextConfidenceThreshold());
    ProductImagePrediction responseReturn = productImagePredictionRepository.save(response);
    if (CollectionUtils.isNotEmpty(productImagePrediction.getCategoryMappings())) {
      updateCategoryMapping(response, productImagePrediction.getCategoryMappings());
    }
    return responseReturn;
  }

  @Override
  @CacheEvict(value = CacheKeys.PRODUCT_IMAGE_PREDICTION, key = "#productImagePrediction.predictionType")
  public void cacheEvictForUpdateImagePredictionAndCategoryMapping(ProductImagePrediction productImagePrediction)
      throws Exception {
    getProductImagePredictionService().evictPredictionTypeCache(productImagePrediction.getStoreId());
  }

  private void validateRequestFromExistingProductImagePrediction(
      ProductImagePredictionAndCategoryMappingRequest request, ProductImagePrediction response) {
    checkArgument(Objects.nonNull(response), ErrorMessages.IMAGE_PREDICTION_TYPE_NOT_FOUND);
    checkArgument(response.isCompareCategory(),
        ErrorMessages.CANNOT_UPDATE_PRODUCT_IMAGE_PREDICTION_AND_CATEGORY_MAPPING_AS_COMPARE_CATEGORY_IS_FALSE);
    if (IMAGE.equals(response.getType())) {
      checkArgument((request.getConfidenceThreshold() > MINIMUM_THRESHOLD
              && request.getTextConfidenceThreshold() == MINIMUM_THRESHOLD
              && request.getConfidenceThreshold() <= MAXIMUM_THRESHOLD),
          ErrorMessages.THRESHOLD_VALUE_INVALID_FOR_TYPE_IMAGE);
    } else if (TEXT.equals(response.getType())) {
      checkArgument((request.getConfidenceThreshold() == MINIMUM_THRESHOLD
              && request.getTextConfidenceThreshold() > MINIMUM_THRESHOLD
              && request.getTextConfidenceThreshold() <= MAXIMUM_THRESHOLD),
          ErrorMessages.THRESHOLD_VALUE_INVALID_FOR_TYPE_TEXT);
    } else {
      checkArgument((request.getConfidenceThreshold() > MINIMUM_THRESHOLD
              && request.getTextConfidenceThreshold() > MINIMUM_THRESHOLD
              && request.getConfidenceThreshold() <= MAXIMUM_THRESHOLD
              && request.getTextConfidenceThreshold() <= MAXIMUM_THRESHOLD),
          ErrorMessages.THRESHOLD_VALUE_INVALID_FOR_TYPE_IMAGE_AND_TEXT);
    }
  }

  private void updateCategoryMapping(ProductImagePrediction productImagePrediction,
      List<PredictionCategoryMappingUpdateRequest> categoryMappings) {
    List<PredictionCategoryMappingRequest> predictionCategoryMappingRequestList = new ArrayList<>();
    for (PredictionCategoryMappingUpdateRequest categoryMapping : categoryMappings) {
      PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
      predictionCategoryMappingRequest.setPredictionId(productImagePrediction.getId());
      predictionCategoryMappingRequest.setCategoryCode(categoryMapping.getCategoryCode());
      predictionCategoryMappingRequest.setMarkForDelete(categoryMapping.isMarkForDelete());
      predictionCategoryMappingRequest.setStoreId(productImagePrediction.getStoreId());
      predictionCategoryMappingRequestList.add(predictionCategoryMappingRequest);
    }
    productOutbound.upsertPredictionCategoryMapping(predictionCategoryMappingRequestList);
  }

  @Override
  @CacheEvict(value = CacheKeys.PRODUCT_IMAGE_PREDICTION, key = "#predictionType")
  @Transactional(readOnly = false)
  public void delete(String storeId, String predictionType) {
    ProductImagePrediction response = findByStoreIdAndPredictionType(storeId, predictionType);
    checkState(Objects.nonNull(response), ErrorMessages.IMAGE_PREDICTION_TYPE_NOT_FOUND);
    productImagePredictionRepository.deleteById(response.getId());
    getProductImagePredictionService().evictPredictionTypeCache(response.getStoreId());
  }

  @Override
  @Cacheable(value = CacheKeys.PRODUCT_IMAGE_PREDICTION, key = "#predictionType", unless = "#result == null")
  public ProductImagePrediction findByStoreIdAndPredictionType(String storeId, String predictionType) {
    return this.productImagePredictionRepository.findByStoreIdAndPredictionType(storeId, predictionType);
  }

  @Override
  public List<ProductImagePrediction> findByStoreIdAndForceReviewTrue(String storeId) {
    return this.productImagePredictionRepository.findByStoreIdAndForceReviewTrue(storeId);
  }

  @Override
  public List<ProductImagePrediction> findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(String storeId) {
    return this.productImagePredictionRepository.findByStoreIdAndForceReviewTrueAndPredictionConsideredTrue(storeId);
  }

  @Override
  public ImageQcProcessedResponse findProductImagePredictionResponseByStoreIdAndProductCode(String storeId,
      String productCode) {
    return getImageQcProcessedResponse(storeId, productCode);
  }

  private ImageQcProcessedResponse getImageQcProcessedResponse(String storeId, String productCode) {
    ImageQcProcessedResponse imageQcProcessedResponse = new ImageQcProcessedResponse();
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(storeId, productCode);
    if (ConverterUtil.getImageQcResponse(productCode, imageQcProcessedResponse, response,
        getProductImagePredictionService().getListOfActivePredictionTypes(storeId))) {
      return imageQcProcessedResponse;
    }
    return null;
  }

  @Override
  public ImageQcProcessedAndBrandResponse findProductImagePredictionAndBrandResponseByStoreIdAndProductCode(
      String storeId, String productCode) {
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(getImageQcProcessedResponse(storeId, productCode));
    ProductCollection productCollection = productLevel1CollectionService.findByProductCode(storeId, productCode);
    imageQcProcessedAndBrandResponse.setBrandApprovalStatus(productCollection.getBrandApprovalStatus().name());
    imageQcProcessedAndBrandResponse.setBrandCode(productCollection.getBrandCode());
    imageQcProcessedAndBrandResponse.setProductType(productLevel1CollectionService
        .getProductTypeBasedOnProductCodeOrId(productCollection.getProductCode(), productCollection.getProductId()));
    return imageQcProcessedAndBrandResponse;
  }

  @Override
  public List<ProductImagePredictionAndCategoryMappingResponse> getImagePredictionAndCategoryMapping(String storeId,
      List<String> predictionTypeList) throws Exception {
    validatePredictionTypeRequest(storeId, predictionTypeList);
    List<ProductImagePrediction> productImagePredictionList =
        productImagePredictionRepository.findByStoreIdAndPredictionTypeInAndMarkForDeleteFalse(storeId,
            predictionTypeList);
    Map<String, List<CategoryCodeAndNameResponse>> predictionIdCategoryCodeMap =
        getPredictionIdAndCategoryMap(productImagePredictionList);
    List<ProductImagePredictionAndCategoryMappingResponse> responseList =
        ConverterUtil.getProductImagePredictionAndCategoryMappingResponses(productImagePredictionList,
            predictionIdCategoryCodeMap);
    return responseList;
  }

  private Map<String, List<CategoryCodeAndNameResponse>> getPredictionIdAndCategoryMap(
      List<ProductImagePrediction> productImagePredictionList) {
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    List<String> predictionIdList = new ArrayList<>();
    productImagePredictionList.stream()
        .forEach(productImagePrediction -> predictionIdList.add(productImagePrediction.getId()));
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    GdnRestListResponse<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCode =
        productOutbound.getPredictionIdAndCategoryCode(predictionIdsRequest);
    Map<String, List<CategoryCodeAndNameResponse>> predictionIdCategoryCodeMap = new HashMap<>();
    predictionIdAndCategoryCode.getContent().stream().forEach(
        predictionIdAndCategoryCodeResponse -> predictionIdCategoryCodeMap.put(
            predictionIdAndCategoryCodeResponse.getPredictionId(),
            predictionIdAndCategoryCodeResponse.getCategoryCodeAndNameResponseList()));
    return predictionIdCategoryCodeMap;
  }

  private void validatePredictionTypeRequest(String storeId, List<String> predictionTypeList) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(predictionTypeList), ErrorMessages.PREDICTION_TYPE_LIST_MUST_NOT_BE_EMPTY);
    for (String predictionType : predictionTypeList) {
      checkArgument(StringUtils.isNotBlank(predictionType), ErrorMessages.PREDICTION_TYPE_MUST_NOT_BE_BLANK);
    }
  }

  @Override
  @Cacheable(value = CacheKeys.PRODUCT_IMAGE_PREDICTION_TYPE, key = "#storeId", unless = "#result == null")
  public List<PredictionTypeResponse> getDifferentPredictionType(String storeId) {
    return productImagePredictionRepository.findDistinctPredictionTypeNameWhereStoreIdAndMarkForDeleteFalse(storeId);
  }

  private ProductImagePredictionService getProductImagePredictionService() {
    return applicationContext.getBean(ProductImagePredictionService.class);
  }

  @Override
  @CacheEvict(value = CacheKeys.PRODUCT_IMAGE_PREDICTION_TYPE, key = "#storeId")
  public void evictPredictionTypeCache(String storeId) {
  }

  @Override
  public List<String> getListOfActivePredictionTypes(String storeId) {
    return productImagePredictionRepository.findDistinctPredictionTypeWhereStoreIdAndMarkForDeleteFalse(storeId);
  }

  @Override
  public List<ProductImagePredictionResponse> findByStoreId(String storeId) {
    List<ProductImagePrediction> productImagePredictionList = productImagePredictionRepository.findByStoreId(storeId);
    log.debug("response from db = {} ", productImagePredictionList);
    return ConverterUtil.toListOfProductImagePredictionResponse(productImagePredictionList);
  }

  @Override
  public List<ProductImagePredictionResponse> findByStoreIdAndPredictionConsideredTrue(String storeId) {
    List<ProductImagePrediction> productImagePredictionList =
        productImagePredictionRepository.findByStoreIdAndPredictionConsideredTrue(storeId);
    return ConverterUtil.toListOfProductImagePredictionResponse(productImagePredictionList);
  }

  @Override
  public List<ProductImagePrediction> findByStoreIdAndMarkForDeleteFalse(String storeId) {
    return productImagePredictionRepository.findByStoreIdAndMarkForDeleteFalse(storeId);
  }
}