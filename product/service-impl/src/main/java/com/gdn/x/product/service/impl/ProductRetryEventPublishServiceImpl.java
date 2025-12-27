package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.dao.api.ProductRetryEventPublishRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.rest.web.model.dto.ProductScoreRetryDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.service.api.ArchiveAndDeleteRejectedMerchantProductDataService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.event.listener.AdjustmentProductChangeListenerV2;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.promotion.domain.event.config.PromotionDomainEventName;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class ProductRetryEventPublishServiceImpl implements ProductRetryEventPublishService {

  private static final String INVALID_ITEM_SKU_FORMAT = "Invalid item sku format : ";
  private static final String X_PRODUCT_RETRY_REQUEST = "x-product-retry";
  private static final String INVALID_PRODUCT_SKU_FORMAT = "Invalid product sku format : ";
  @Autowired
  private ProductRetryEventPublishRepository productRetryEventPublishRepository;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  @Lazy
  private AdjustmentProductChangeListenerV2 adjustmentListenerV2;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ArchiveAndDeleteRejectedMerchantProductDataService archiveAndDeleteRejectedMerchantProductData;

  @Autowired
  private PromotionOutbound promotionOutbound;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  ProductCacheableService productCacheableService;

  @Value("${max.retry.publish}")
  private int maxRetryPublish;

  @Value("${send.failed.retry.product.summary.email.address}")
  private String sendFailedRetryProductSummaryEmailAddress;

  @Override
  public ProductRetryEventPublish insertToRetryPublish(ProductRetryEventPublish productRetryEventPublish) {
    return productRetryEventPublishRepository.save(productRetryEventPublish);
  }

  @Override
  @Async
  public void schedulerRetryPublish(String storeId) {
    int limit = Integer.parseInt(
        systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE)
            .getValue());
    List<ProductRetryEventPublish> productRetryEventPublishList =
        productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, limit);
    List<ProductRetryEventPublish> failedList = new ArrayList<>();
    for (ProductRetryEventPublish productRetryEventPublish : productRetryEventPublishList) {
      switch (productRetryEventPublish.getTopicName()) {
        case (ProductDomainEventName.ITEM_CHANGE_EVENT_NAME): {
          try {
            clearCacheAndPublish(storeId, productRetryEventPublish);
          } catch (Exception ex) {
            log.error("Error while retry publishing for : {}", productRetryEventPublish, ex);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        case (PromotionDomainEventName.ADJUSTMENT_PRODUCT_CHANGE_EVENT_NAME_V2): {
          try {
            retryDiscountAdjustmentEvent(storeId, productRetryEventPublish);
          } catch (Exception e) {
            log.error("Error while retry publishing for : {}", productRetryEventPublish, e);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        case (DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME): {
          try {
            retryProductScoreRegenerateEvent(productRetryEventPublish);
          } catch (Exception e) {
            log.error("Error while retry publishing for : {}", productRetryEventPublish, e);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        case (ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT): {
          try {
            retryArchiveAndDeleteRejectedMerchantProductEvent(productRetryEventPublish);
          } catch (Exception e) {
            log.error("Error while retry publishing for : {} , error - ", productRetryEventPublish,
              e);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        case (ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR) : {
          try {
            retryL3SolrReindexing(storeId, productRetryEventPublish);
          } catch (Exception e) {
            log.error("Error while retry publishing for : {}", productRetryEventPublish, e);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        case (ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING) : {
          try {
            retryMasterSkuMapping(productRetryEventPublish);
          } catch (Exception e) {
            log.error("Error while retry publishing for : {}", productRetryEventPublish, e);
            CommonUtil.setStatusOnFailure(productRetryEventPublish, failedList, maxRetryPublish);
            this.insertToRetryPublish(productRetryEventPublish);
          }
          break;
        }
        default:
          log.info("Unknown Type encountered for retry Event : {} ",productRetryEventPublish.getTopicName());
      }

    }
    if (CollectionUtils.isNotEmpty(failedList)) {
      sendFailedRetryProductsMail(failedList);
    }
  }

  private void retryL3SolrReindexing(String storeId,
    ProductRetryEventPublish productRetryEventPublish) {
    checkArgument(this.skuValidator.isProductSku(productRetryEventPublish.getIdentifier()),
      INVALID_PRODUCT_SKU_FORMAT.concat(productRetryEventPublish.getIdentifier()));
    log.info("Proceeding with Retry Attempt count : {} , for Solr Reindex Event : {} ",
      productRetryEventPublish.getRetryCount(), productRetryEventPublish.getIdentifier());
    Product product =
      productCacheableService.findProductByStoreIdAndProductSku(storeId,
        productRetryEventPublish.getIdentifier());
    DeltaReindexToSolrEventModel deltaReindexToSolrEventModel = new DeltaReindexToSolrEventModel();
      deltaReindexToSolrEventModel.setProduct(objectConverterService.convertToProductEventModel(product));
      log.info("publishing reindex product event : {} for product sku : {} ", ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, product.getProductSku());
    this.kafkaPublisher.send(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR,
      deltaReindexToSolrEventModel.getProductSku(), deltaReindexToSolrEventModel);
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
  }

  private void retryArchiveAndDeleteRejectedMerchantProductEvent(ProductRetryEventPublish productRetryEventPublish)
      throws Exception {
    ProductCodeDomainEventModel productCodeDomainEventModel =
        this.objectMapper.readValue(productRetryEventPublish.getIdentifier(), ProductCodeDomainEventModel.class);
    archiveAndDeleteRejectedMerchantProductData.archiveAndDeleteRejectedMerchantProductDataRetry(
        productCodeDomainEventModel);
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
    log.info("Retry attempt to archive and delete rejected merchant product for : {} was success ",
        productRetryEventPublish.getIdentifier());
  }

  private void retryMasterSkuMapping(ProductRetryEventPublish productRetryEventPublish)
      throws Exception {
    log.info("Proceeding with Retry Attempt count : {} , for master sku update : {} ",
        productRetryEventPublish.getRetryCount(), productRetryEventPublish.getIdentifier());
    MasterSkuMappingEventModel masterSkuMapping =
        objectMapper.readValue(productRetryEventPublish.getIdentifier(), MasterSkuMappingEventModel.class);
    itemService.updateMasterSku(masterSkuMapping);
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
    log.info("Retry attempt to update master sku for item : {} was success ",
        masterSkuMapping.getItemSku());
  }

  private void retryProductScoreRegenerateEvent(ProductRetryEventPublish productRetryEventPublish) throws Exception {
    ProductScoreRetryDTO productScoreRetryDTO =
        objectMapper.readValue(productRetryEventPublish.getIdentifier(), ProductScoreRetryDTO.class);
    productService.generateProductScoreByProductSku(productScoreRetryDTO.getStoreId(),
      productScoreRetryDTO.getProductSku(), productScoreRetryDTO.getProductCode(),
      productScoreRetryDTO.getRequestId(), productScoreRetryDTO.getUserName(),
      productScoreRetryDTO.isUpdateCategory(), null);
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
    log.info("Retry attempt to score regeneration Event for : {} was success ",
        productRetryEventPublish.getIdentifier());
  }

  private void retryDiscountAdjustmentEvent(String storeId,
    ProductRetryEventPublish productRetryEventPublish) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSku(productRetryEventPublish.getIdentifier()),
      INVALID_ITEM_SKU_FORMAT.concat(productRetryEventPublish.getIdentifier()));
    List<AdjustmentProductChange> adjustmentProductChangeList;
    log.info("Proceeding with Retry Attempt count : {} , for Adjustment Event : {} ",
      productRetryEventPublish.getRetryCount(), productRetryEventPublish.getIdentifier());
    if (Objects.nonNull(productRetryEventPublish.getSecondaryIdentifier())) {
      List<AdjustmentProductChangeResponseVO> adjustmentProductBySkuAndPickupPointCode = promotionOutbound.getAdjustmentProductBySkuAndPickupPointCode(
          productRetryEventPublish.getStoreId(), Collections.singletonList(
            ItemPickupPointRequest.builder().itemSku(productRetryEventPublish.getIdentifier())
              .pickupPointCode(productRetryEventPublish.getSecondaryIdentifier()).build()));
      adjustmentProductChangeList = objectConverterService.convertToAdjustmentProductChangeList(
        adjustmentProductBySkuAndPickupPointCode, Collections.emptyList());
    } else {
      List<AdjustmentProductResponse> adjustmentProduct =
          promotionOutbound.getAdjustmentProduct(X_PRODUCT_RETRY_REQUEST,
          Constants.DEFAULT_USERNAME,
          Collections.singletonList(productRetryEventPublish.getIdentifier()));
      adjustmentProductChangeList =
        objectConverterService.convertToAdjustmentProductChangeList(Collections.emptyList(),
          adjustmentProduct);
    }
    if(CollectionUtils.isNotEmpty(adjustmentProductChangeList)) {
      log.info("Retrying Adjustment Event processing for Item : {} ,with request : {} ",
        adjustmentProductChangeList.get(0).getProductSku(), adjustmentProductChangeList.get(0));
      ItemPickupPoint itemPickupPoint =
        adjustmentListenerV2.processAdjustmentEventV2(adjustmentProductChangeList.get(0));
      if (Objects.nonNull(itemPickupPoint) && (productRetryEventPublish.isClearCache())) {
        cacheEvictHelperService.evictItemPickupPointData(storeId, itemPickupPoint,
          adjustmentProductChangeList.get(0).getPickupPointCode());
      }
    }
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
    log.info("Retry attempt to process Adjustment Event for : {} was success ",
      productRetryEventPublish.getIdentifier());
  }

  private void clearCacheAndPublish(String storeId, ProductRetryEventPublish productRetryEventPublish) {
    Item item = itemService.findByStoreIdAndItemSku(storeId, productRetryEventPublish.getIdentifier());
    if (productRetryEventPublish.isClearCache()) {
      cacheEvictHelperService.evictItemData(storeId, item);
    }
    if (item.isMarkForDelete()) {
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
    }
    saveAndPublishService.publishListOfItems(Arrays.asList(item));
    productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FINISHED);
    insertToRetryPublish(productRetryEventPublish);
    log.info("Cache cleared and published for : {}", productRetryEventPublish.getIdentifier());
  }

  /**
   * sending email containing summary products after repetitive failure on retry publish
   *
   * @param productRetryEventPublishes
   */
  public void sendFailedRetryProductsMail(List<ProductRetryEventPublish> productRetryEventPublishes) {
    MessageEmailRequest email =
        CommonUtil.setMailMessage(productRetryEventPublishes, sendFailedRetryProductSummaryEmailAddress);
    try {
      log.info("sending mail for failed publishes : {}", email);
      kafkaPublisher.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT, email);
      for (ProductRetryEventPublish productLevel3FailedEntity : productRetryEventPublishes) {
        productLevel3FailedEntity.setMarkForDelete(true);
      }
      productRetryEventPublishRepository.saveAll(productRetryEventPublishes);
      log.info("Email for Failed Retry Products is sent : {}", email.getMessageTo());
    } catch (Exception e) {
      log.error("Error while sending mail for failed retry products", e);
    }
  }
}
