package com.gdn.mta.product.service.domainevent;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.mta.domain.event.modal.PriceInfoDTO;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.mta.domain.event.modal.ProductStatusDomainEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.mta.product.service.ProductPublisherService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

/**
 * @author febryo.lesmana
 */

@Service
@Slf4j
public class ProductPublisherServiceBean implements ProductPublisherService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductPublisherServiceBean.class);

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Autowired
  @Lazy
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${price.info.screening.approval.enabled}")
  private boolean priceInfoScreeningApprovalEnabled;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Override
  public ScreeningProductApprovalEvent publish(String productCode, String businessPartnerCode,
      String businessPartnerName, String updatedBy, boolean postLive, boolean restrictedKeywordsPresent,
      String restrictedKeywordsByFieldJson, int prioritySeller, boolean trustedSeller, String productId,
      ProductCollection productCollection) throws Exception {
    LOGGER.info("Publishing screening approved event for productCode: {}, merchantCode: {}", productCode,
        businessPartnerCode);
    boolean imageQcCheck = false;
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, productCode);
    if (Objects.nonNull(response)) {
      imageQcCheck = true;
    }
    ProductBusinessPartner productBusinessPartner = Optional.ofNullable(
            productBusinessPartnerService.findFirstByStoreIdAndProductId(Constants.DEFAULT_STORE_ID, productId))
        .orElse(new ProductBusinessPartner());
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        ScreeningProductApprovalEvent.builder().productCode(productCode).merchantCode(businessPartnerCode)
            .merchantName(businessPartnerName).updatedBy(updatedBy).postLive(postLive)
            .restrictedKeywordsPresent(restrictedKeywordsPresent).imageQcCheck(imageQcCheck)
            .b2bActivated(productBusinessPartner.isB2bActivated()).b2cActivated(productBusinessPartner.isB2cActivated())
            .restrictedKeywordsDetected(
            ConverterUtil.toRestrictedKeywordsByFieldResponseFromJson(restrictedKeywordsByFieldJson)).trustedSeller(trustedSeller).build();
    if (priceInfoScreeningApprovalEnabled) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productBusinessPartner.getProductItemBusinessPartners();
      List<PriceInfoDTO> priceInfoList =
          ConverterUtil.getPriceInfoFromProductItemBusinessPartner(productItemBusinessPartnerList,
              priceInfoMaxVariantLimit);
      screeningProductApprovalEvent.setPriceInfo(priceInfoList);
    }
    if (Objects.nonNull(productCollection)) {
      screeningProductApprovalEvent.setProductCreationType(productCollection.getProductCreationType());
    }
    SellerDetailResponse sellerDetailResponse = null;
    try {
      sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(businessPartnerCode);
    }catch (Exception e)
    {
      LOGGER.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
          businessPartnerCode, e);
    }    if (Objects.nonNull(sellerDetailResponse)) {
      screeningProductApprovalEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
    }
    String screeningApprovalEventName = kafkaTopicProperties.getVendorCombinedEventNoPriority();
    if (prioritySeller == PrioritySeller.PRIORITY_1.getPrioritySeller()) {
      screeningApprovalEventName = kafkaTopicProperties.getVendorCombinedEventPriority1();
    } else if (prioritySeller == PrioritySeller.PRIORITY_2.getPrioritySeller()) {
      screeningApprovalEventName = kafkaTopicProperties.getVendorCombinedEventPriority2();
    }
    log.info("Publishing event : {} for productCode : {} and payload : {}",
        screeningApprovalEventName, screeningProductApprovalEvent.getProductCode(),
        screeningProductApprovalEvent);
    kafkaProducer.send(screeningApprovalEventName, screeningProductApprovalEvent.getProductCode(),
        AddProductToVendorCombinedEventModel.builder().screeningProductApprovalEvent(screeningProductApprovalEvent)
            .build());
    return screeningProductApprovalEvent;
  }

  @Override
  public ImageResizeEvent publishProductImageResizeEvent(String productCode, String storeId) {
    LOGGER.info("Publishing image resize event for productCode: {}", productCode);
    ImageResizeEvent imageResizeEvent = ImageResizeEvent.builder().productCode(productCode).storeId(storeId).build();
    kafkaProducer.send(kafkaTopicProperties.getImageResizeEventNoPriority(), imageResizeEvent.getProductCode(),
        imageResizeEvent);
    return imageResizeEvent;
  }

  @Override
  public ImageResizeEvent publishProductImageResizeEventForPrioritySeller(String productCode, String storeId,
      int prioritySeller) {
    ImageResizeEvent imageResizeEvent = ImageResizeEvent.builder().productCode(productCode).storeId(storeId).build();
    if (prioritySeller == 1) {
      kafkaProducer.send(kafkaTopicProperties.getImageResizeEventPriority1(), imageResizeEvent.getProductCode(),
          imageResizeEvent);
      LOGGER.info("Publishing image resize event for prioritySeller1 for productCode: {}", productCode);
    } else {
      kafkaProducer.send(kafkaTopicProperties.getImageResizeEventPriority2(), imageResizeEvent.getProductCode(),
          imageResizeEvent);
      LOGGER.info("Publishing image resize event for prioritySeller2 for productCode: {}", productCode);
    }
    return imageResizeEvent;
  }

  @Override
  public ProductStatusDomainEvent publishProductStatusDomainEvent(ProductStatusDomainEvent productStatusDomainEvent) {
    LOGGER.info("Publishing product status event for productCode: {}, message :{}",
        productStatusDomainEvent.getProduct().getProductCode(), productStatusDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_STATUS_MAIL_EVENT, productStatusDomainEvent.getProduct().getProductCode(),
        productStatusDomainEvent);
    return productStatusDomainEvent;
  }

  @Override
  public EditedImageResizeEvent publishEditImageResizeEvent(EditedImageResizeEvent editedImageResizeEvent) {
    LOGGER.info("Publishing edit image resize event for productCode: {}, event: {}", editedImageResizeEvent.getProductCode(), editedImageResizeEvent);
    kafkaProducer.send(DomainEventName.EDITED_IMAGE_RESIZE_EVENT, editedImageResizeEvent);
    return editedImageResizeEvent;
  }

  @Override
  public EditedImageResizeEvent publishReviseImageResizeEvent(EditedImageResizeEvent editedImageResizeEvent) {
    LOGGER.info("Publishing revise image resize event for productCode: {}, event: {}",
        editedImageResizeEvent.getProductCode(), editedImageResizeEvent);
    kafkaProducer.send(DomainEventName.REVISE_IMAGE_RESIZE_EVENT, editedImageResizeEvent.getProductCode(),
        editedImageResizeEvent);
    return editedImageResizeEvent;
  }

  @Override
  public AddRevisedProductToPDTEvent publishRevisedProductToPDT(
      AddRevisedProductToPDTEvent addRevisedProductToPDTEvent) {
    LOGGER.info("Publishing revised product to PDT. addRevisedProductToPDTEvent = {} ", addRevisedProductToPDTEvent);
    SellerDetailResponse sellerDetailResponse = null;
    try {
      sellerDetailResponse = productAnalyticsOutbound.getSellerDetail(addRevisedProductToPDTEvent.getMerchantCode());
    } catch (Exception e) {
      LOGGER.error("Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
          addRevisedProductToPDTEvent.getMerchantCode(), e);
    }
    if (Objects.nonNull(sellerDetailResponse)) {
      addRevisedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
    }
    kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), addRevisedProductToPDTEvent.getProductCode(),
        AddProductToVendorCombinedEventModel.builder().addRevisedProductToPDTEvent(addRevisedProductToPDTEvent)
            .build());
    return addRevisedProductToPDTEvent;
  }

  @Override
  public ProductQCRetryEvent publishProductQCRetryEvent(ProductQCRetryEvent productQCRetryEvent) {
    LOGGER.info("Publishing qc retry event to PDT. productQCRetryEvent = {} ", productQCRetryEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_QC_RETRY_EVENT, productQCRetryEvent.getProductCode(), productQCRetryEvent);
    return productQCRetryEvent;
  }

  @Override
  public PDTDimensionRefreshEventModel publishProductDimensionRefreshEvent(
      PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel) {
    LOGGER.info("Publishing dimension refresh event = {} ", pdtDimensionRefreshEventModel);
    kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(),
        pdtDimensionRefreshEventModel.getProductCode(),
        AddProductToVendorCombinedEventModel.builder().pdtDimensionRefreshEventModel(pdtDimensionRefreshEventModel)
            .build());
    return pdtDimensionRefreshEventModel;
  }
}
