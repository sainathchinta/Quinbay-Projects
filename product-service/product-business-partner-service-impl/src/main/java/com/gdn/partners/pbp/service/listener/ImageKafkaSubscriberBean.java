package com.gdn.partners.pbp.service.listener;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.newrelic.api.agent.Trace;

/**
 * Created by akshay.bhatt on 09/24/18
 */
@Service
public class ImageKafkaSubscriberBean {

  private static final Logger LOGGER = LoggerFactory.getLogger(ImageKafkaSubscriberBean.class);

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductWfService productWfService;

  @Lazy
  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductDistributionService productDistributionService;

  @Value("${approve.image.channel.id.default}")
  private String channelId;

  @Value("${approve.image.authenticator.id.default}")
  private String authenticatorId;

  @Value("${retry.create.switch}")
  private boolean retryCreateSwitch;

  @Value("${check.state.for.XGP.event.listener.image.activation.flow}")
  private boolean checkStateForXGPEventListenerImageActivationFlow;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  /**
   * This method listen "approve image request" message from MTA. approve image if all product
   * images are active Send message with product details to MTA whether image is active ot not
   *
   * @param
   * @throws Exception
   */
  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScaleStatusEventNoPriority()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    LOGGER.info("[ImageKafkaSubscriber] Retrieved message from Topic: {}, Message: {}",
        kafkaTopicProperties.getImageScaleStatusEventNoPriority(), bulkImageProcessResponse);
    LOGGER.info("Product-workflow-tracker : proccesed images received from X-GP for productCode : {}",
        bulkImageProcessResponse.getGroupCode());
    final String requestId = Constants.XGP_USER + UUID.randomUUID().toString();
    kafkaEventHelper(bulkImageProcessResponse, requestId);
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScaleStatusEventPriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedStatusPrioritySeller1(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    LOGGER.info("[ImageKafkaSubscriber] Retrieved message from Topic: {}, Message: {}",
            kafkaTopicProperties.getImageScaleStatusEventPriority1(), bulkImageProcessResponse);
    LOGGER.info("Product-workflow-tracker : proccesed images received from X-GP for productCode : {}",
            bulkImageProcessResponse.getGroupCode());
    final String requestId = Constants.XGP_USER + UUID.randomUUID().toString();
    kafkaEventHelper(bulkImageProcessResponse, requestId);
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getImageScaleStatusEventPriority2()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedStatusPrioritySeller2(String message) throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = objectMapper.readValue(message, BulkImageProcessResponse.class);
    LOGGER.info("[ImageKafkaSubscriber] Retrieved message from Topic: {}, Message: {}",
        kafkaTopicProperties.getImageScaleStatusEventPriority2(), bulkImageProcessResponse);
    LOGGER.info("Product-workflow-tracker : proccesed images received from X-GP for productCode : {}",
            bulkImageProcessResponse.getGroupCode());
    final String requestId = Constants.XGP_USER + UUID.randomUUID().toString();
    kafkaEventHelper(bulkImageProcessResponse, requestId);
  }

  public void kafkaEventHelper(BulkImageProcessResponse bulkImageProcessResponse,
    String requestId) throws Exception {
    boolean skipReview = Boolean.FALSE;
    String productCode = StringUtils.EMPTY;
    try{
      if (CollectionUtils.isNotEmpty(bulkImageProcessResponse.getImageResponses())) {
        String storeId = bulkImageProcessResponse.getStoreId();
        String clientId = null;
        productCode = bulkImageProcessResponse.getGroupCode();
        boolean success = Boolean.TRUE;
        for (ImageResponse imageResponse : bulkImageProcessResponse.getImageResponses()) {
          if (!imageResponse.isSuccess()) {
            success = Boolean.FALSE;
            LOGGER.warn("ImageProcessing not successful from XGP, product-code : {}", productCode);
          }
          trimLocationPaths(imageResponse);
          clientId = imageResponse.getClientId();
        }
        LOGGER.info("methodName={} action={} service=MTA status=INFO ref=product-creation"
            + " productCode={} desc=subscribe image from kafka subscribe-image", productCode);
        ProductCollection productCollection =
            productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
        String username = null;
        skipReview = productCollection.isSkipReview();
        if (skipReview) {
          username = Constants.DEFAULT_USERNAME;
        } else {
          username =
              productDistributionTaskRepository.getDetailsForAnyProduct(requestId, Constants.XGP_USER, productCode).getUpdatedBy();
        }
        setMandatoryParameters(bulkImageProcessResponse.getStoreId(), clientId, username, requestId);
        if (success) {
          this.approveImage(storeId, productCollection, bulkImageProcessResponse.getImageResponses(), skipReview);
        } else {
          LOGGER.warn("methodName={} action={} service=MTA status=WARN ref=product-creation "
              + "productCode={} desc=Received empty list from XGP, move failed product to"
              + " final QC and reject image, listen, image-result-is-not-success", productCode);
          if (!skipReview) {
            this.productDistributionTaskRepository.moveFailedProductToQC(requestId, Constants.XGP_USER, productCode);
            this.productWfService.rejectImage(productCode);
            this.productService.saveProductHistory(storeId, productCode, Constants.DEFAULT_USERNAME,
                SaveHistoryConstants.PROCESS_IMAGE_FAILED, null);
          }
        }
      }
    } catch (Exception e) {
      if (!skipReview) {
        this.productDistributionTaskRepository.moveFailedProductToQC(requestId, Constants.XGP_USER, productCode);
        this.productWfService.rejectImage(productCode);
      }
      LOGGER.error("methodName={} action={} service=PBP status=ERROR ref=product-creation product-code={} "
          + "desc=ImageProcessing is error message={}", "listen", "subscribe-image", productCode, e.getMessage(), e);
    }
  }

  private void trimLocationPaths(ImageResponse imageResponse) {
    if (StringUtils.isNotEmpty(imageResponse.getImagePathLocation())) {
      imageResponse.setImagePathLocation(
          imageResponse.getImagePathLocation().replace(Constants.FULL_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
      imageResponse.setImagePathLocation(
          imageResponse.getImagePathLocation().replace(Constants.MEDIUM_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
      imageResponse.setImagePathLocation(imageResponse.getImagePathLocation()
          .replace(Constants.THUMBNAIL_IMAGE_PREFIX, Constants.DELIMITER_SLASH));
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
  private void setMandatoryParameters(String storeId, String clientId,
      String username, String requestId) {
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
   * @param productCollection
   * @param imageResponses
   * @param skipReview
   * @throws Exception
   */
  private void approveImage(String storeId, ProductCollection productCollection, List<ImageResponse> imageResponses,
      boolean skipReview) throws Exception {
    ProductResponse productResponse =
        this.productService.findProductBasicDetailByProductCode(productCollection.getProductCode());
    if (getProductStatus(productCollection, productResponse)) {
      this.createActivateImageResponse(storeId, productCollection, imageResponses, skipReview);
      productService.publishProductStatusEventByProductCode(productCollection.getProductCode(), ProductStatus.ACTIVE,
          StringUtils.EMPTY);
    } else {
      //Post live product activation
      if (!productCollection.isReviewPending() && productCollection.isPostLive() && !productCollection.isSkipReview()) {
        //will send skip review as false as this part will not be called for skip review true products
        ActivateImageResponse activateImageResponse;

          activateImageResponse =
            updateActiveImagesAndGetActivateImageResponse(productCollection.getProductCode(), imageResponses, false);

        if (activateImageResponse.isActive()) {
          productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE,
              Arrays.asList(productCollection.getProductCode()));
          productService.saveProductHistory(storeId, productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL_POST_ACTIVATION, null);
          try {
            productService.updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID,
                productCollection.getProductCode());
          } catch (Exception e) {
            LOGGER.error("Error when updating image qc productCode : {} ", productCollection.getProductCode(), e);
          }
          // TODO Post-live check
          syncActiveProducts(storeId, productCollection);
          removeProductFromPDT(productCollection.getProductCode(), productCollection.getId());
          xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, productCollection.getProductCode(),
              false);
        }
        else {
          LOGGER.warn("Images not activated Successfully for product {}", productCollection.getProductCode());
          productService.saveProductHistory(storeId, productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
              SaveHistoryConstants.IMAGE_NOT_ACTIVATED_SUCCESSFULLY, null);
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
              "Images not activated Successfully for product" + productCollection.getProductCode());
        }
      } else {
        //Product already active,
        if (!productCollection.isReviewPending() && !productCollection.isSkipReview()) {
          productService.updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID,
              productCollection.getProductCode());
          removeProductFromPDT(productCollection.getProductCode(), productCollection.getId());
        }
      }
    }
  }

  private boolean getProductStatus(ProductCollection productCollection, ProductResponse productResponse) {
    return checkStateForXGPEventListenerImageActivationFlow ?
        ((Objects.nonNull(productResponse) && !productResponse.isViewable()) || (!Constants.ACTIVE.equals(
            productCollection.getState()))) :
        (Objects.nonNull(productResponse) && !productResponse.isViewable());
  }

  private void syncActiveProducts(String storeId, ProductCollection productCollection) throws Exception {
    if (retryCreateSwitch) {
      productServiceWrapper.processProductVendorSearchAutoHeal(storeId, productCollection.getProductCode());
    }
  }

  /**
   * this method approve Product since images are active
   *
   * @throws Exception
   */
  private void approveProductWithActiveImage(ProductCollection productCollection) throws Exception {
    try {
      this.productWfService.approveImage(productCollection.getProductCode());
      if(!productCollection.isReviewPending() && !productCollection.isSkipReview()) {
        productService.updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, productCollection.getProductCode());
        removeProductFromPDT(productCollection.getProductCode(), productCollection.getId());
      }
    } catch (Exception ex) {
      LOGGER.error(
          "methodName=listen action=approve-img-failed service=PBP status=ERROR ref=product-activation productCode={} "
              + "desc=error while approving image for final activation of product",
          productCollection.getProductCode(), ex);
      throw ex;
    }
  }

  /**
   * create active image response by updating ProductImage Name in PCB
   *
   * @param imageResponses
   * @throws Exception
   */
  private void createActivateImageResponse(String storeId, ProductCollection productCollection,
      List<ImageResponse> imageResponses, boolean skipReview)
      throws Exception {
    ActivateImageResponse activateImageResponse =
        updateActiveImagesAndGetActivateImageResponse(productCollection.getProductCode(), imageResponses, skipReview);
    LOGGER
        .info("Product-workflow-tracker : Response from PCB after updating images activeImages : {}, productCode : {}",
            activateImageResponse.isActive(), activateImageResponse.getProductCode());
    if (activateImageResponse.isActive()) {
      productService.saveProductHistory(storeId, productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.PROCESS_IMAGE_SUCCESSFUL, null);
      approveProductWithActiveImage(productCollection);
    } else {
      LOGGER.info("Product-workflow-tracker : Images not activated Successfully for productCode {}",
          productCollection.getProductCode());
      productService.saveProductHistory(storeId, productCollection.getProductCode(), Constants.DEFAULT_USERNAME,
          SaveHistoryConstants.IMAGE_NOT_ACTIVATED_SUCCESSFULLY, null);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          "Images not activated Successfully for product" + productCollection.getProductCode());
    }
  }

  private ActivateImageResponse updateActiveImagesAndGetActivateImageResponse(String productCode,
      List<ImageResponse> imageResponses, boolean skipReview) throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(productCode);
    Set<ActivateImageRequest> activateImageRequests =
        imageResponses.stream().map(imageResponse -> getActivateImageRequest(productCode, imageResponse))
            .collect(Collectors.toSet());
    productActivateImageRequest.setImageRequests(activateImageRequests);
    LOGGER.info("Product-workflow-tracker : Update product images request sent to PCB for productCode : {}",
        productCode);
    return this.productService.updateProductImagesName(productActivateImageRequest, skipReview);
  }

  /**
   * create Active Image request
   * @param productCode
   * @param imageResponse
   * @return
   */
  private ActivateImageRequest getActivateImageRequest(String productCode, ImageResponse imageResponse) {
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
    LOGGER.info("Product-workflow-tracker : remove product from PDT for productCode : {}", productCode);
    publishReviewProductCollectionDeleteEvent(id);
    this.productDistributionService.removeProductFromPDT(UUID.randomUUID().toString(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        new RemoveProductRequest(productCode));
  }

  private void publishReviewProductCollectionDeleteEvent(String id) {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        new SolrReviewProductCollectionDeleteEvent();
    solrReviewProductCollectionDeleteEvent.setIds(Collections.singletonList(id));
    kafkaProducer.send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        solrReviewProductCollectionDeleteEvent);
  }
}
