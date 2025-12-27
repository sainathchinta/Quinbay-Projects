package com.gdn.partners.pbp.service.listener;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageInformationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.ApproveImageMessageConstants;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.XGPImageInfoDomainEvent;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.newrelic.api.agent.Trace;

/**
 * Created by akshay.bhatt on 03/04/18
 */
@Service
public class ApproveImageKafkaSubscriber {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApproveImageKafkaSubscriber.class);

  private static final String MTA_USER = "MTA_USER";
  
  private static final String DEFAULT_STORE_ID = "10001";

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${approve.image.channel.id.default}")
  private String channelId;

  @Value("${approve.image.authenticator.id.default}")
  private String authenticatorId;

  private String productCode;

  /**
   * This method listen "approve image request" message from MTA. approve image if all product
   * images are active Send message with product details to MTA whether image is active ot not
   *
   * @param message
   * @throws Exception
   */
  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_APPROVE_IMAGE_REQUEST, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    XGPImageInfoDomainEvent xgpImageInfoDomainEvent = objectMapper.readValue(message, XGPImageInfoDomainEvent.class);
    LOGGER.debug("[ApproveImageKafkaConsumer] Retrieved message from Topic: {}, Message: {}",
        DomainEventName.PRODUCT_APPROVE_IMAGE_REQUEST, xgpImageInfoDomainEvent);
    try {
      if (xgpImageInfoDomainEvent != null) {
        setMandatoryParameters(xgpImageInfoDomainEvent);
        approveImage(xgpImageInfoDomainEvent);
      } else {
        this.productService.publishProductDetailsEvent(this.productCode,
            ApproveImageMessageConstants.EMPTY_MESSAGE_RECEIVED);
      }
    } catch (Exception ex) {
      LOGGER.error(
          "methodName=listen action=approve-image service=MTA status=ERROR ref=product-creation "
              + "product-code={} desc=ImageProcessing failed", this.productCode, ex);
      this.productService.publishProductDetailsEvent(this.productCode, ex.getMessage());
    }
  }

  /**
   * set mandatory parameters in MDC
   *
   * @param xgpImageInfoDomainEvent
   */
  private void setMandatoryParameters(XGPImageInfoDomainEvent xgpImageInfoDomainEvent) {
    String requestId = MTA_USER + UUID.randomUUID().toString();
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        xgpImageInfoDomainEvent.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        xgpImageInfoDomainEvent.getClientId());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        xgpImageInfoDomainEvent.getUsername());
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, this.authenticatorId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, this.channelId);
  }

  /**
   * this method will check if product is eligible for Image Approval and take action
   *
   * @param xgpImageInfoDomainEvent
   * @throws Exception
   */
  private void approveImage(XGPImageInfoDomainEvent xgpImageInfoDomainEvent) throws Exception {
    this.productCode = xgpImageInfoDomainEvent.getProductCode();
    ProductResponse productResponse = this.productService.findProductBasicDetailByProductCode(this.productCode);
    if (!productResponse.isViewable()) {
      this.createActivateImageResponse(xgpImageInfoDomainEvent);
    } else {
      this.productService.publishProductDetailsEvent(this.productCode,
          ApproveImageMessageConstants.PRODUCT_VIEWABLE_ALREADY);
    }
  }

  /**
   * this method approve Product since images are active
   *
   * @throws Exception
   */
  private void approveProductWithActiveImage() throws Exception {
    try {
      this.productWfService.approveImage(this.productCode);
      this.productService.publishProductDetailsEvent(this.productCode,
          ApproveImageMessageConstants.PRODUCT_VIEWABLE);
    } catch (ApplicationException e) {
      if (ErrorCategory.INVALID_STATE.equals(e.getErrorCodes())) {
        LOGGER.error(
            "methodName=listen action=approve-img-failed service=MTA status=ERROR ref=product-creation productCode={} "
                + "desc=error while approving image for final activation of product. Retry approving image",
            this.productCode, e);
        try {
          this.productWfService.approveImage(this.productCode);
          this.productService.publishProductDetailsEvent(this.productCode,
              ApproveImageMessageConstants.PRODUCT_VIEWABLE);
        } catch (Exception ex) {
          LOGGER.error(
              "methodName=listen action=retry-approve-img-failed service=MTA status=ERROR ref=product-creation productCode={} "
                  + "desc=retry error while approving image for final activation of product",
              this.productCode, ex);
          this.productService.publishProductDetailsEvent(this.productCode, ex.getMessage());
        }
      } else {
        LOGGER.error(
            "methodName=listen action=approve-img-failed service=MTA status=ERROR ref=product-creation productCode={} "
                + "desc=error while approving image for final activation of product. Retry approving image",
            this.productCode, e);
        this.productService.publishProductDetailsEvent(this.productCode, e.getMessage());
      }
    }
  }

  /**
   * create active image response by updating ProductImage Name in PCB
   *
   * @param xgpImageInfoDomainEvent
   * @throws Exception
   */
  private void createActivateImageResponse(XGPImageInfoDomainEvent xgpImageInfoDomainEvent)
      throws Exception {
    ProductActivateImageRequest productActivateImageRequest = new ProductActivateImageRequest();
    productActivateImageRequest.setProductCode(xgpImageInfoDomainEvent.getProductCode());
    Set<ActivateImageRequest> activateImageRequests = new HashSet();
    for (ImageInformationRequest imageInformationRequest : xgpImageInfoDomainEvent.getImageInformationRequests()) {
      ActivateImageRequest request = new ActivateImageRequest();
      request.setProductCode(xgpImageInfoDomainEvent.getProductCode());
      request.setHashCode(imageInformationRequest.getHashCode());
      request.setFilenames(imageInformationRequest.getImagePathLocation());
      activateImageRequests.add(request);
    }
    productActivateImageRequest.setImageRequests(activateImageRequests);
    LOGGER.info("Product-workflow-tracker : Update image filename request sent to PCB : {}", productCode);
    ActivateImageResponse activateImageResponse =
        this.productService.updateProductImagesName(productActivateImageRequest, false);
    if (activateImageResponse.isActive()) {
      saveProductHistory(this.productCode, "System", "Process Image Successful", null);
      approveProductWithActiveImage();
    } else {
      this.productService.publishProductDetailsEvent(this.productCode,
          ApproveImageMessageConstants.IMAGE_NOT_ACTIVE);
    }
  }

  private void saveProductHistory(String productCode, String username, String activity,
      String notes) {
    final Calendar now = Calendar.getInstance();
    final ProductHistory productHistory = new ProductHistory();
    productHistory.setDescription(activity);
    productHistory.setNotes(notes);
    productHistory.setState(5);
    productHistory.setStoreId(DEFAULT_STORE_ID);
    productHistory.setCreatedDate(now.getTime());
    productHistory.setUpdatedDate(now.getTime());
    productHistory.setCreatedBy(username);
    productHistory.setUpdatedBy(username);
    try {
      this.productService.saveProductHistory(productCode, productHistory);
    } catch (Exception e) {
      LOGGER.error("error while saveProductHistory with productCode: {}", productCode);
    }
  }
}
