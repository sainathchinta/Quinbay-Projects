package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.client.helper.ClientParameterHelper;
import com.gdn.partners.product.analytics.entity.AttributeValueExtractions;
import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.entity.ProductAttributeFeedback;
import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.model.enums.ProductAttributeExtractionsStatus;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.ProductAttributeExtractionsRepository;
import com.gdn.partners.product.analytics.repository.ProductAttributeFeedbackRepository;
import com.gdn.partners.product.analytics.service.DsExtractedAttributeService;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import com.gdn.partners.product.analytics.service.cache.DsExtractedAttributeCacheableService;
import com.gdn.partners.product.analytics.service.impl.util.ValidationUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductAttributeFeedbackEventModel;
import model.AttributeValueExtractionsEventModel;
import model.ProductAttributeExtractionsEventModel;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class ProductAttributeExtractionsServiceImpl implements ProductAttributeExtractionsService {

  private final ProductAttributeExtractionsRepository productAttributeExtractionsRepository;

  private final KafkaProducerService kafkaProducerService;

  private final KafkaTopicProperties kafkaTopicProperties;

  private final DsExtractedAttributeCacheableService dsExtractedAttributeCacheableService;
  private final ProductAttributeFeedbackRepository productAttributeFeedbackRepository;

  private final ClientParameterHelper clientParameterHelper;
  private final DsExtractedAttributeService dsExtractedAttributeService;

  @Override
  @Async
  public void publishEventsForProductAttributeExtractions(String storeId, int batchSize) {
    List<ProductAttributeExtractions> productAttributeExtractionsList =
        productAttributeExtractionsRepository.fetchProductAttributeExtractions(storeId, batchSize);
    if (CollectionUtils.isNotEmpty(productAttributeExtractionsList)) {
      productAttributeExtractionsList.forEach(this::publishEventForIndividualProductAttributeExtractions);
    }
  }

  @Override
  public void publishEventsForProductAttributeExtractionsByProductSku(String storeId, List<String> productSkuList) {
    List<ProductAttributeExtractions> productAttributeExtractionsList =
        productAttributeExtractionsRepository.findByStoreIdAndProductSkuInAndStatus(storeId,
            productSkuList, ProductAttributeExtractionsStatus.NEW.name());
    if (CollectionUtils.isNotEmpty(productAttributeExtractionsList)) {
      productAttributeExtractionsList.forEach(this::publishEventForIndividualProductAttributeExtractions);
    }
  }

  private void publishEventForIndividualProductAttributeExtractions(
      ProductAttributeExtractions productAttributeExtractions) {
    try {
      kafkaProducerService.publishMessageForProductAttributeExtractions(
          convertToProductAttributeExtractionsEventModel(productAttributeExtractions),
          kafkaTopicProperties.getProductAttributeExtractionsValidationEventName());
      productAttributeExtractions.setStatus(ProductAttributeExtractionsStatus.PICKED.name());
      productAttributeExtractionsRepository.save(productAttributeExtractions);
    } catch (Exception ex) {
      log.error("Error publishing event {} with payload :{}, error-",
          kafkaTopicProperties.getProductAttributeExtractionsValidationEventName(),
          productAttributeExtractions, ex);
    }

  }

  public ProductAttributeExtractionsEventModel convertToProductAttributeExtractionsEventModel(
      ProductAttributeExtractions productAttributeExtractions) throws Exception {
    return ProductAttributeExtractionsEventModel.builder()
        .storeId(clientParameterHelper.getStoreId())
        .productId(productAttributeExtractions.getProductId())
        .productSku(productAttributeExtractions.getProductSku())
        .addedDate(productAttributeExtractions.getAddedDate()).attributeValueExtractions(
            convertToAttributeValueExtractionsEventModel(
                productAttributeExtractions.getAttributeValueExtractions())).build();
  }

  public List<AttributeValueExtractionsEventModel> convertToAttributeValueExtractionsEventModel(
      List<AttributeValueExtractions> attributeValueExtractionsList) {
    if (CollectionUtils.isNotEmpty(attributeValueExtractionsList)) {
      return attributeValueExtractionsList.stream().map(
          attributeValueExtractions -> AttributeValueExtractionsEventModel.builder()
              .attributeName(attributeValueExtractions.getAttributeName())
              .value(attributeValueExtractions.getAttributeValue())
              .valueEn(attributeValueExtractions.getAttributeValueEnglish()).build()).toList();
    }
    return new ArrayList<>();
  }

  public void validateAndPublishPCBEventsForProductAttributeExtractions(
      ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel)
      throws InvocationTargetException, IllegalAccessException {
    if (CollectionUtils.isNotEmpty(
        productAttributeExtractionsEventModel.getAttributeValueExtractions())) {
      ProductAttributeExtractionsEventModel validatedProductAttributeExtractionsEventModel =
          new ProductAttributeExtractionsEventModel();
      BeanUtils.copyProperties(validatedProductAttributeExtractionsEventModel,
          productAttributeExtractionsEventModel);
      List<AttributeValueExtractionsEventModel> validatedAttributeValueExtractionsEventModel =
          new ArrayList<>();
      for (AttributeValueExtractionsEventModel attributeValueExtractionsEventModel :
          productAttributeExtractionsEventModel.getAttributeValueExtractions()) {
        DSExtractionEntity dsExtractionEntity =
            dsExtractedAttributeCacheableService.fetchDSExtractionsByName(
                attributeValueExtractionsEventModel.getAttributeName());
        if (Objects.nonNull(dsExtractionEntity) && !dsExtractionEntity.isMarkForDelete()) {
          attributeValueExtractionsEventModel.setAttributeCode(
              dsExtractionEntity.getAttributeCode());
          validatedAttributeValueExtractionsEventModel.add(attributeValueExtractionsEventModel);
        }
      }
      if (CollectionUtils.isNotEmpty(validatedAttributeValueExtractionsEventModel)) {
        validatedProductAttributeExtractionsEventModel.setAttributeValueExtractions(
            validatedAttributeValueExtractionsEventModel);
        kafkaProducerService.publishMessageForProductAttributeExtractions(
            validatedProductAttributeExtractionsEventModel,
            kafkaTopicProperties.getProductAttributeExtractionsEventName());
      }
    }
  }

  @Override
  public void updateFeedbackForProductAttribute(ProductAttributeFeedbackEventModel eventModel)
    throws InvocationTargetException, IllegalAccessException {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(eventModel.getProductCode()),
      ErrorCode.PRODUCT_CODE_EMPTY.getMessage());
    log.info("Saving feedback {} for product code {} ",eventModel, eventModel.getProductCode());
    ProductAttributeFeedback productAttributeFeedback = new ProductAttributeFeedback();
    BeanUtils.copyProperties(productAttributeFeedback, eventModel);
    DSExtractionEntity dsExtractionEntity =
        dsExtractedAttributeService.getDsExtractedAttributeByAttributeCode(
            eventModel.getAttributeCode());
    if (Objects.nonNull(dsExtractionEntity)) {
      productAttributeFeedback.setAttributeName(dsExtractionEntity.getDsAttributeName());
    }
    productAttributeFeedbackRepository.save(productAttributeFeedback);
  }
}