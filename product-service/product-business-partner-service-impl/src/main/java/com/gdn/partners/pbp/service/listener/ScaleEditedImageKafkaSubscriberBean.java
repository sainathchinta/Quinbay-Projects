package com.gdn.partners.pbp.service.listener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseServiceImpl;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ScaleEditedImageKafkaSubscriberBean {

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductImageQcProcessingResponseServiceImpl productImageQcProcessingResponseService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductDistributionService productDistributionService;

  @Value("${approve.image.channel.id.default}")
  private String channelId;

  @Value("${approve.image.authenticator.id.default}")
  private String authenticatorId;

  @Value("${relax.edited.flag.check.image.resize.event}")
  private boolean relaxEditedFlagCheckImageResizeEvent;

  private String productCode;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.GRAPHIC_EDITED_IMAGE_SCALE_STATUS_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ScaleEditedImagesResponse scaleEditedImagesResponse =
        objectMapper.readValue(message, ScaleEditedImagesResponse.class);
    log.info("[ScaleEditedImageKafkaSubscriber] Retrieved message from Topic: {}, Message: {}",
        DomainEventName.GRAPHIC_EDITED_IMAGE_SCALE_STATUS_EVENT, scaleEditedImagesResponse);
    final String requestId = Constants.XGP_USER + UUID.randomUUID().toString();
    String storeId = Constants.DEFAULT_STORE_ID;
    try {
      if (CollectionUtils.isNotEmpty(scaleEditedImagesResponse.getImageResponses())) {
        productCode = scaleEditedImagesResponse.getProductCode();
        storeId = scaleEditedImagesResponse.getStoreId();
        String clientId = null;
        boolean success = Boolean.TRUE;
        for (ScaleImageResponse imageResponse : scaleEditedImagesResponse.getImageResponses()) {
          if (!imageResponse.isSuccess()) {
            success = Boolean.FALSE;
            log.error("ImageProcessing not successful from XGP, product-code : {}", productCode);
            break;
          }
          ConverterUtil.trimLocationPaths(imageResponse);
          clientId = imageResponse.getClientId();
        }
        log.info("subscribe image from kafka subscribe-image for : {}", productCode);
        setMandatoryParameters(scaleEditedImagesResponse.getStoreId(), clientId,
            scaleEditedImagesResponse.getUsername(), requestId);
        if (success) {
          this.approveImage(storeId, productCode, scaleEditedImagesResponse.getImageResponses());
        } else {
          log.warn("image result is not success for : {}", productCode);
          this.productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.PROCESS_IMAGE_FAILED, null);
        }
      }
    } catch (Exception e) {
      log.error("Error while listening to event : {} for product : {} {}",
          DomainEventName.GRAPHIC_EDITED_IMAGE_SCALE_STATUS_EVENT, productCode, e.getMessage(), e);
      if (ObjectUtils.isNotEmpty(productCode)) {
        productService.setReviewPendingFlagToTrue(storeId, this.productCode);
      }
    }
  }

  /**
   * set mandatory parameters in MDC
   *
   * @param storeId
   * @param clientId
   * @param username
   * @param requestId
   */
  private void setMandatoryParameters(String storeId, String clientId, String username, String requestId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, this.authenticatorId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, this.channelId);
  }

  /**
   * this method will check if product is eligible for Image Approval and take action
   *
   * @param storeId
   * @param productCode
   * @param imageResponses
   * @throws Exception
   */
  private void approveImage(String storeId, String productCode, List<ScaleImageResponse> imageResponses)
      throws Exception {
    ProductCollection productCollection = productWfService.getProductCollectionByProductCode(productCode);
    if (productCollection.isEdited() || relaxEditedFlagCheckImageResizeEvent) {
      ActivateImageResponse activateImageResponse =
          updateActiveImagesAndGetActivateImageResponse(productCode, imageResponses);
      if (activateImageResponse.isActive()) {
        productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(productCode));
        String productSku = productService.isProductActivationNeeded(storeId, productCollection.getProductId());
        if (StringUtils.isNotBlank(productSku)) {
          productLevel3Service.takeDownOrReactivateProduct(storeId, productSku, false, null, null);
        }
        ProductCollection updatedProductCollection = productService.updateReviewType(storeId, productCode, null);
        productService.updateSolrProductCollection(updatedProductCollection);
        productService.updateImageQcDataAfterVendorApproval(storeId, productCode);
        productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
        removeProductFromPDT(productCode, productCollection.getId());
        //Image QC Response will be made MFD true, Edited false and main image at L4 is Updated
        xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, productCode, false);
      } else {
        log.warn("Images not activated Successfully for product {}", productCode);
        productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
            SaveHistoryConstants.IMAGE_NOT_ACTIVATED_SUCCESSFULLY, null);
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
            "Images not activated Successfully for product" + productCode);
      }
    }
  }

  private ActivateImageResponse updateActiveImagesAndGetActivateImageResponse(String productCode,
      List<ScaleImageResponse> imageResponses) throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(productCode);
    Set<ActivateImageRequest> activateImageRequests =
        imageResponses.stream().map(imageResponse -> getActivateImageRequest(productCode, imageResponse))
            .collect(Collectors.toSet());
    productActivateImageRequest.setImageRequests(activateImageRequests);
    log.info("Update product images request sending to PCB for productCode : {}", productCode);
    return this.productService.updateProductImagesName(productActivateImageRequest, false);
  }

  /**
   * create Active Image request
   *
   * @param productCode
   * @param imageResponse
   * @return
   */
  private ActivateImageRequest getActivateImageRequest(String productCode, ScaleImageResponse imageResponse) {
    ActivateImageRequest request = new ActivateImageRequest();
    request.setProductCode(productCode);
    request.setHashCode(imageResponse.getHashCode());
    request.setFilenames(imageResponse.getImagePathLocation());
    request.setCommonImage(imageResponse.isCommonImage());
    return request;
  }

  /**
   * remove product from PDT if eligible to delete
   *
   * @throws Exception
   */
  private void removeProductFromPDT(String productCode, String id) throws Exception {
    log.info("Remove product from PDT for productCode : {}, id : {} ", productCode, id);
    publishEventForDeleteProductFromReviewProductCollection(id);
    this.productDistributionService
        .removeProductFromPDT(UUID.randomUUID().toString(), GdnMandatoryRequestParameterUtil.getUsername(),
            new RemoveProductRequest(productCode));
  }

  private void publishEventForDeleteProductFromReviewProductCollection(String id) {
    if (StringUtils.isNotEmpty(id)) {
      SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
          new SolrReviewProductCollectionDeleteEvent();
      List<String> ids = new ArrayList<>();
      ids.add(id);
      solrReviewProductCollectionDeleteEvent.setIds(ids);
      kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
          solrReviewProductCollectionDeleteEvent);
    }
  }
}
