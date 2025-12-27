package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.entity.ProductOptimisationSuggestionFeedback;
import com.gdn.partners.product.analytics.entity.ProductOptimiseFeedback;
import com.gdn.partners.product.analytics.entity.ProductOptimisationSuggestions;
import com.gdn.partners.product.analytics.entity.SystemParameter;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.dto.SuggestionImpactDto;
import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.model.enums.ProductOptimisationDetailsStatus;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.ProductOptimisationRepository;
import com.gdn.partners.product.analytics.repository.ProductOptimiseFeedbackRepository;
import com.gdn.partners.product.analytics.repository.SystemParameterRepository;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationListResponse;
import com.gdn.partners.product.analytics.web.model.ProductOptimisationSuggestionResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.product.analytics.service.cache.ProductOptimisationCacheableService;
import com.gdn.partners.product.analytics.service.impl.util.ValidationUtil;
import com.gdn.partners.product.analytics.web.model.ProductCountsWebResponse;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.product.analytics.web.model.request.SuggestionFeedbackRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductChangeEventModel;
import model.ProductOptimisationEventModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;


@Service
@Slf4j
@RequiredArgsConstructor
public class ProductOptimisationServiceImpl implements ProductOptimisationService {

  private final ProductOptimisationRepository productOptimisationRepository;
  private final ProductOptimisationCacheableService productOptimisationCacheableService;
  private final ProductOptimiseFeedbackRepository productOptimiseFeedbackRepository;
  private final SystemParameterRepository systemParameterRepository;
  private final ObjectMapper objectMapper;
  private final KafkaTopicProperties kafkaTopicProperties;
  private final KafkaPublisher kafkaPublisher;

  @Override
  public ProductCountsWebResponse getProductCounts(String storeId, String sellerCode) {
    String productCount = String.valueOf(
        productOptimisationCacheableService.findProductCountCacheablesBySellerCode(sellerCode));
    return ProductCountsWebResponse.builder().productCount(Integer.parseInt(productCount)).build();
  }

  @Override
  public void removeDeletedProduct(ProductChangeEventModel productChangeEventModel) {
    ProductOptimisationDetails productOptimisationDetails =
        productOptimisationRepository.findByProductSkuAndMarkForDelete(
            productChangeEventModel.getProductSku(), false);
    if (Objects.nonNull(productOptimisationDetails)) {
      String sellerCode = productOptimisationDetails.getSellerCode();
      Set<String> productChangeTypes =
          new HashSet<>(productChangeEventModel.getProductChangeEventType());
      if (CollectionUtils.isNotEmpty(productChangeTypes)) {
        if (productChangeTypes.contains(Constants.SUSPEND_FLAG_CHANGE)
            && productChangeEventModel.isSuspended()) {
          log.info("Update product as deleted for suspended product with productSku:{}",
              productOptimisationDetails.getProductSku());
          updateDeletedProduct(productOptimisationDetails);
        } else if (productChangeTypes.contains(Constants.ARCHIVE_FLAG_CHANGE)
            && productChangeEventModel.isArchived()) {
          log.info("Update product as deleted for archived product with productSku:{}",
              productOptimisationDetails.getProductSku());
          updateDeletedProduct(productOptimisationDetails);
        } else if (productChangeTypes.contains(Constants.FORCE_REVIEW_FLAG_CHANGE)
            && productChangeEventModel.isForceReview()) {
          log.info("Update product as deleted for taken down product with productSku:{}",
              productOptimisationDetails.getProductSku());
          updateDeletedProduct(productOptimisationDetails);
        } else if (productChangeTypes.contains(Constants.PRODUCT_REJECTED)) {
          log.info("Update product as deleted for rejected product with productSku:{}",
              productOptimisationDetails.getProductSku());
          updateDeletedProduct(productOptimisationDetails);
        }
      }
      //clear cache after product deleted for updating count
      log.info("Sending cache clear. Event: {}, Seller Code {} ",
        kafkaTopicProperties.getSellerCacheClearEventName(), sellerCode);
      kafkaPublisher.send(kafkaTopicProperties.getSellerCacheClearEventName(), sellerCode,
        sellerCode);
    }
  }

  @Override
  public void updateStatusForProductOptimisation(
      ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest) {
    ProductOptimisationDetails productOptimisationDetails =
      productOptimisationRepository.findByProductSku(
        productOptimisationUpdateStatusRequest.getProductSku());
    ProductOptimiseFeedback productOptimiseFeedback =
        productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(
            productOptimisationUpdateStatusRequest.getProductSku(), false);
    ValidationUtil.checkParameter(Objects.nonNull(productOptimisationDetails), ErrorCode.PRODUCT_NOT_FOUND);
    if (Objects.isNull(productOptimiseFeedback)) {
      productOptimiseFeedback = new ProductOptimiseFeedback();
      BeanUtils.copyProperties(productOptimisationDetails, productOptimiseFeedback, "version");
    }
    if (Boolean.TRUE.equals(productOptimisationUpdateStatusRequest.getViewed())) {
      productOptimisationDetails.setStatusInDb(ProductOptimisationDetailsStatus.VIEWED.getValue());
      productOptimiseFeedback.setStatus(ProductOptimisationDetailsStatus.VIEWED);
    } else if (Boolean.FALSE.equals(productOptimisationUpdateStatusRequest.getViewed())) {
      productOptimisationDetails.setStatusInDb(ProductOptimisationDetailsStatus.EDITED.getValue());
      productOptimiseFeedback.setStatus(ProductOptimisationDetailsStatus.EDITED);
    } else {
      productOptimisationDetails.setStatusInDb(ProductOptimisationDetailsStatus.NEW.getValue());
      productOptimiseFeedback.setStatus(ProductOptimisationDetailsStatus.NEW);
    }
    productOptimiseFeedback.setFieldUpdated(
        (productOptimisationUpdateStatusRequest.getFieldEdited() == null
            || productOptimisationUpdateStatusRequest.getFieldEdited().isEmpty()) ?
            Strings.EMPTY :
            String.join(Constants.COMMA, productOptimisationUpdateStatusRequest.getFieldEdited()));
    productOptimiseFeedbackRepository.save(productOptimiseFeedback);
    productOptimisationRepository.save(productOptimisationDetails);
  }

  private void updateDeletedProduct(ProductOptimisationDetails productOptimisationDetails) {
    productOptimisationDetails.setMarkForDelete(true);
    productOptimisationDetails.setProductDeleted(true);
    productOptimisationRepository.save(productOptimisationDetails);
  }

  @Override
  public Page<ProductOptimisationListResponse> fetchProductOptimisationList(String storeId,
      ProductOptimisationListRequest productOptimisationListRequest, int page, int size) {
    Map<String, SuggestionImpactDto> suggestionImpactDtoMap = new HashMap<>();
    Page<ProductOptimisationDetails> productOptimisationDetails =
        productOptimisationRepository.fetchProductOptimisationListWithFilterApplied(storeId,
            productOptimisationListRequest, PageRequest.of(page, size));
    List<ProductOptimisationListResponse> productOptimisationListResponses =
      productOptimisationDetails.getContent().stream()
        .map(details -> toProductOptimisationResponse(details, suggestionImpactDtoMap))
        .collect(Collectors.toList());
    return new PageImpl<>(productOptimisationListResponses, PageRequest.of(page, size),
        productOptimisationDetails.getTotalElements());
  }

  @Override
  public void clearSellerLevelCache(String sellerCode) {
    if (StringUtils.isNotEmpty(sellerCode)) {
      productOptimisationCacheableService.evictCacheBySellerCode(sellerCode);
    }
  }

  @Override
  @Async
  public void submitSuggestionFeedback(
      ProductOptimisationFeedbackRequest productOptimisationFeedbackRequest) {
    boolean feedbackReceived = true;
    if (CollectionUtils.isNotEmpty(productOptimisationFeedbackRequest.getSuggestionFeedback())) {
      ProductOptimisationDetails productOptimisationDetails =
          productOptimisationRepository.findByProductSkuAndMarkForDelete(
              productOptimisationFeedbackRequest.getProductSku(), false);
      if (Objects.isNull(productOptimisationDetails)) {
        log.error("Product optimization details not found for SKU: {}",
            productOptimisationFeedbackRequest.getProductSku());
        return;
      }
      ProductOptimiseFeedback productOptimiseFeedback =
          productOptimiseFeedbackRepository.findByProductSkuAndMarkForDelete(
              productOptimisationFeedbackRequest.getProductSku(), false);

      if (Objects.isNull(productOptimiseFeedback)) {
        productOptimiseFeedback = new ProductOptimiseFeedback();
        BeanUtils.copyProperties(productOptimisationFeedbackRequest, productOptimiseFeedback);
      }

      List<ProductOptimisationSuggestionFeedback> suggestionFeedbackRequests =
          productOptimisationFeedbackRequest.getSuggestionFeedback().stream()
              .map(this::mapToProductOptimisationSuggestionFeedback).toList();

      productOptimiseFeedback.setSuggestionFeedback(suggestionFeedbackRequests);
      productOptimiseFeedbackRepository.save(productOptimiseFeedback);

      /*
       * Updates the feedback type for each product optimization suggestion based on the provided
       * suggestion feedback.The method creates a map of suggestion types to suggestions, checks
       * that if suggestions are null, an empty list is used.
       * It then updates the feedback type of each suggestion that matches the feedback provided in
       * the request.
       */
      Map<String, ProductOptimisationSuggestions> suggestionMap = new HashMap<>();
      List<ProductOptimisationSuggestions> suggestions =
          Optional.ofNullable(productOptimisationDetails.getSuggestions())
              .orElseGet(ArrayList::new);
      for (ProductOptimisationSuggestions suggestion : suggestions) {
        suggestionMap.put(suggestion.getSuggestionType(), suggestion);
      }
      for (SuggestionFeedbackRequest feedback : productOptimisationFeedbackRequest.getSuggestionFeedback()) {
        String suggestionName = feedback.getSuggestionName();
        ProductOptimisationSuggestions suggestion = suggestionMap.get(suggestionName);
        if (Objects.nonNull(suggestion)) {
          feedbackReceived = feedback.getFeedbackType() && feedbackReceived;
          suggestion.setFeedbackType(feedback.getFeedbackType());
        }
      }
      setStatusInDbForFeedback(productOptimisationDetails, feedbackReceived);
      productOptimisationRepository.save(productOptimisationDetails);
    }
  }

  @Override
  public List<ProductOptimisationSuggestionResponse> showSuggestionDetails(String storeId,
      String productSku) {
    List<ProductOptimisationSuggestionResponse> productOptimisationSuggestionResponses = new ArrayList<>();
    ProductOptimisationDetails productOptimisationDetails =
        productOptimisationRepository.findByProductSkuAndMarkForDelete(
            productSku, false);
    if(Objects.nonNull(productOptimisationDetails)) {
      for (ProductOptimisationSuggestions suggestion : productOptimisationDetails.getSuggestions()) {
        ProductOptimisationSuggestionResponse response = new ProductOptimisationSuggestionResponse();
        BeanUtils.copyProperties(suggestion, response);
        productOptimisationSuggestionResponses.add(response);
      }
    }
    return productOptimisationSuggestionResponses;
  }

  private ProductOptimisationSuggestionFeedback mapToProductOptimisationSuggestionFeedback(
      SuggestionFeedbackRequest request) {
    ProductOptimisationSuggestionFeedback feedback = new ProductOptimisationSuggestionFeedback();
    BeanUtils.copyProperties(request, feedback);
    return feedback;
  }

  @Override
  public void upsertProductOptimisationData(ProductOptimisationEventModel optimisationEventModel) {
    String productSku = optimisationEventModel.getProductSku();
    ProductOptimisationDetails productOptimisationDetails =
      productOptimisationRepository.findByProductSkuAndMarkForDelete(productSku, false);
    if (Objects.nonNull(productOptimisationDetails) && !optimisationEventModel.isMarkForDelete()) {
      BeanUtils.copyProperties(optimisationEventModel, productOptimisationDetails, "suggestions");
      populateSuggestions(productOptimisationDetails, optimisationEventModel);
    } else if (Objects.nonNull(productOptimisationDetails)) {
      log.info("Marking product sku as mfd true {} ", productSku);
      productOptimisationDetails.setMarkForDelete(true);
    } else {
      productOptimisationDetails = new ProductOptimisationDetails();
      BeanUtils.copyProperties(optimisationEventModel, productOptimisationDetails);
      populateSuggestions(productOptimisationDetails, optimisationEventModel);
    }
    productOptimisationDetails.setStoreId(Constants.STORE_ID_VALUE);
    productOptimisationRepository.save(productOptimisationDetails);
  }

  private ProductOptimisationListResponse toProductOptimisationResponse(
      ProductOptimisationDetails productOptimisationDetails, Map<String, SuggestionImpactDto> suggestionImpactDtoMap) {
    ProductOptimisationListResponse productOptimisationListResponse =
        new ProductOptimisationListResponse();
    BeanUtils.copyProperties(productOptimisationDetails, productOptimisationListResponse);
    productOptimisationListResponse.setImage(productOptimisationDetails.getImageUrl());
    productOptimisationListResponse.setSuggestions(
      toProductOptimisationSuggestionResponseList(productOptimisationDetails.getSuggestions(),
        suggestionImpactDtoMap));
    return productOptimisationListResponse;
  }

  private List<ProductOptimisationSuggestionResponse> toProductOptimisationSuggestionResponseList(
    List<ProductOptimisationSuggestions> productOptimisationSuggestions,
    Map<String, SuggestionImpactDto> suggestionImpactDtoMap) {
    if (Objects.isNull(productOptimisationSuggestions)) {
      return new ArrayList<>();
    }
    return productOptimisationSuggestions.stream().map(
        suggestion -> toProductOptimisationSuggestionResponse(suggestion, suggestionImpactDtoMap))
      .collect(Collectors.toList());
  }

  private ProductOptimisationSuggestionResponse toProductOptimisationSuggestionResponse(
    ProductOptimisationSuggestions productOptimisationSuggestions,
    Map<String, SuggestionImpactDto> suggestionImpactDtoMap) {
    ProductOptimisationSuggestionResponse productOptimisationSuggestionResponse =
      new ProductOptimisationSuggestionResponse();
    BeanUtils.copyProperties(productOptimisationSuggestions, productOptimisationSuggestionResponse);
    populateImpactSuggestionName(productOptimisationSuggestions.getSuggestionType(),
      productOptimisationSuggestionResponse, suggestionImpactDtoMap);
    return productOptimisationSuggestionResponse;
  }


  private void populateSuggestions(ProductOptimisationDetails optimisationDetails,
    ProductOptimisationEventModel eventModel) {
    Map<String, ProductOptimisationSuggestions> productSuggestionMap = new HashMap<>();
    eventModel.getSuggestions().forEach(
      suggestionsModel -> productSuggestionMap.computeIfAbsent(suggestionsModel.getSuggestionType(),
          suggestionType -> ProductOptimisationSuggestions.builder().suggestionType(suggestionType)
            .suggestionMetadata(new ArrayList<>()).build()).getSuggestionMetadata()
        .add(suggestionsModel.getSuggestionMetadata()));

    optimisationDetails.setSuggestions(new ArrayList<>(productSuggestionMap.values()));
  }

  private void populateImpactSuggestionName(String suggestionType,
    ProductOptimisationSuggestionResponse optimisationSuggestionResponse,
    Map<String, SuggestionImpactDto> suggestionImpactDtoMap) {
    SuggestionImpactDto suggestionImpactDto = new SuggestionImpactDto();
    if (!suggestionImpactDtoMap.containsKey(suggestionType)) {
      SystemParameter systemParameter =
        systemParameterRepository.findByStoreIdAndVariable(Constants.STORE_ID_VALUE,
          suggestionType);
      if (Objects.nonNull(systemParameter)) {
        suggestionImpactDto = mapSuggestionImpact(systemParameter.getValue());
        suggestionImpactDtoMap.put(suggestionType, suggestionImpactDto);
      }
    } else {
      suggestionImpactDto = suggestionImpactDtoMap.get(suggestionType);
    }
    BeanUtils.copyProperties(suggestionImpactDto, optimisationSuggestionResponse);
  }

  private SuggestionImpactDto mapSuggestionImpact(String value) {
    try {
      return objectMapper.readValue(value, SuggestionImpactDto.class);
    } catch (Exception ex) {
      log.error("Error mapping suggestion impact for value: {}. Exception: {}", value,
        ex.getMessage(), ex);
      return new SuggestionImpactDto();
    }
  }

  private void setStatusInDbForFeedback(ProductOptimisationDetails productOptimisationDetails,
    boolean feedbackType) {
    productOptimisationDetails.setStatusInDb(feedbackType ?
      ProductOptimisationDetailsStatus.EDITED.getValue() :
      ProductOptimisationDetailsStatus.NEGATIVE_FEEDBACK.getValue());
  }
}