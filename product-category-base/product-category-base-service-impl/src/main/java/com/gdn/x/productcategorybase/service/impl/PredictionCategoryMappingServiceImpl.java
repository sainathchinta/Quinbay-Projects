package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeAndNameResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredictionCategoryMapping;
import com.gdn.x.productcategorybase.repository.PredictionCategoryMappingRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.PredictionCategoryMappingService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PredictionCategoryMappingServiceImpl implements PredictionCategoryMappingService {

  private static final String HYPHEN = "-";

  @Autowired
  private PredictionCategoryMappingRepository predictionCategoryMappingRepository;

  @Autowired
  private CategoryService categoryService;

  private List<PredictionCategoryMapping> findPredictionCategoryMappingByStoreIdAndPredictionIdAndCategoryCodeList(
      String storeId, String predictionId, List<String> categoryCodeList) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    return predictionCategoryMappingRepository.findByStoreIdAndPredictionIdAndCategoryCodeIn(storeId, predictionId,
        categoryCodeList);
  }

  @Override
  public void upsertPredictionCategoryMapping(String storeId, List<PredictionCategoryMappingRequest> requestList)
      throws Exception {
    List<PredictionCategoryMapping> predictionCategoryMappingList = new ArrayList<>();
    List<String> categoryCodeList = new ArrayList<>();
    for (PredictionCategoryMappingRequest request : requestList) {
      validateRequest(requestList, request);
      categoryCodeList.add(request.getCategoryCode());
    }
    List<PredictionCategoryMapping> predictionCategoryMappingSavedList =
        findPredictionCategoryMappingByStoreIdAndPredictionIdAndCategoryCodeList(storeId,
            requestList.get(0).getPredictionId(), categoryCodeList);
    Map<String, PredictionCategoryMapping> predictionCategoryMappingMap =
        ConverterUtil.predictionCategoryMappingListToMap(predictionCategoryMappingSavedList);
    for (PredictionCategoryMappingRequest request : requestList) {
      if (predictionCategoryMappingMap.containsKey(request.getPredictionId() + HYPHEN + request.getCategoryCode())) {
        PredictionCategoryMapping predictionCategoryMappingExisting =
            predictionCategoryMappingMap.get(request.getPredictionId() + HYPHEN + request.getCategoryCode());
        if (request.isMarkForDelete() != predictionCategoryMappingExisting.isMarkForDelete()) {
          predictionCategoryMappingExisting.setMarkForDelete(request.isMarkForDelete());
          predictionCategoryMappingList.add(predictionCategoryMappingExisting);
        }
      } else {
        if (!request.isMarkForDelete()) {
          PredictionCategoryMapping predictionCategoryMappingNew = ConverterUtil.getPredictionCategoryMapping(request);
          predictionCategoryMappingList.add(predictionCategoryMappingNew);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(predictionCategoryMappingList)) {
      predictionCategoryMappingRepository.saveAll(predictionCategoryMappingList);
    }
  }

  private void validateRequest(List<PredictionCategoryMappingRequest> requestList, PredictionCategoryMappingRequest request) {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getPredictionId())),
        ErrorMessage.PREDICTION_ID_MUST_NOT_BE_NULL_OR_EMPTY.getMessage());
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCategoryCode())),
        ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_NULL_OR_EMPTY.getMessage());
    GdnPreconditions.checkArgument(
        (StringUtils.equals(request.getPredictionId(), requestList.get(0).getPredictionId())),
        ErrorMessage.CANNOT_UPDATE_OR_ADD_MULTIPLE_PREDICTION_ID_AT_ONCE.getMessage());
  }

  @Override
  public List<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCodeResponse(String storeId,
      List<String> predictionIdList) throws Exception {
    validateRequestParams(storeId, predictionIdList);
    List<PredictionCategoryMapping> predictionCategoryMappingList =
        predictionCategoryMappingRepository.findByStoreIdAndPredictionIdInAndMarkForDeleteFalse(storeId,
            predictionIdList);
    List<PredictionIdAndCategoryCodeResponse> predictionIdAndCategoryCodeResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(predictionCategoryMappingList)) {
      Map<String, String> categoryCodeAndCategoryNameMap =
          getCategoryCodeAndNameMap(storeId, predictionCategoryMappingList);
      Map<String, List<CategoryCodeAndNameResponse>> predictionIdAndCategoryCodesMap =
          ConverterUtil.getPredictionIdAndCategoryCodesMap(predictionCategoryMappingList,
              categoryCodeAndCategoryNameMap);
      predictionIdAndCategoryCodeResponseList =
          ConverterUtil.getPredictionIdAndCategoryCodeResponses(predictionIdAndCategoryCodesMap);
    }
    return predictionIdAndCategoryCodeResponseList;
  }

  private Map<String, String> getCategoryCodeAndNameMap(String storeId,
      List<PredictionCategoryMapping> predictionCategoryMappingList) throws Exception {
    List<String> categoryCode = new ArrayList<>();
    predictionCategoryMappingList.stream()
        .forEach(predictionCategoryMapping -> categoryCode.add(predictionCategoryMapping.getCategoryCode()));
    List<Category> categoryList = categoryService.findByStoreIdAndCategoryCodes(storeId, categoryCode);
    Map<String, String> categoryCodeAndCategoryNameMap = new HashMap<>();
    categoryList.stream()
        .forEach(category -> categoryCodeAndCategoryNameMap.put(category.getCategoryCode(), category.getName()));
    return categoryCodeAndCategoryNameMap;
  }

  private void validateRequestParams(String storeId, List<String> predictionIdList) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(predictionIdList),
        ErrorMessage.PREDICTION_ID_LIST_MUST_BE_NULL_OR_EMPTY.getMessage());
    for (String predictionId : predictionIdList) {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(predictionId),
          ErrorMessage.PREDICTION_ID_MUST_NOT_BE_NULL_OR_EMPTY.getMessage());
    }
  }

  public List<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(String storeId, String categoryCode) {
    List<PredictionCategoryMapping> predictionCategoryMappingList =
        predictionCategoryMappingRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    return predictionCategoryMappingList.stream().map(
        predictionCategoryMapping -> new ProductPredictionCategoryMappingResponse(
            predictionCategoryMapping.getPredictionId())).collect(Collectors.toList());
  }
}
