package com.gdn.partners.pbp.service.listener;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ScaleImageInformationRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetailList;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ImageKafkaSubscriberBeanTest {

  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_LOCATION_PATH = "image.jpg";
  private static final String DEFAULT_CLIENT_ID = "DEV";
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String FILE_LOCATION = "/88/MTA-100001/test.jpg";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String HASH_CODE = "hashCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String USER_NAME = "userName";
  private BulkImageProcessResponse bulkImageProcessResponse;
  private ObjectMapper mapper;

  @InjectMocks
  private ImageKafkaSubscriberBean imageKafkaSubscriberBean;

  @Mock
  ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  ProductService productService;

  @Mock
  ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  ProductWfService productWfService;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<RemoveProductRequest> removeProductRequestArgumentCaptor;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductDistributionService productDistributionService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ProductResponse productBasicDetailsResponse;
  private ProductCollection productCollection;
  private ActivateImageResponse activateImageResponse;
  private DistributionProductDetailResponse distributionProductDetailResponse;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    distributionProductDetailResponse = this.generateDistributionProductDetailResponse();
    ImageResultDetailList imageResultDetailList = this.generateImageResultDetailList();
    bulkImageProcessResponse = generateBulkImageProcessResponse();
    productBasicDetailsResponse = generateProductBasicDetailResponse();
    ScaleImageInformationRequest scaleImageInformationRequest = this.generateScaleImageInformationRequest();
    Mockito.when(this.productDistributionTaskRepository
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()))).thenReturn(distributionProductDetailResponse);
    Mockito
        .when(productService.findProductBasicDetailByProductCode(Mockito.eq(bulkImageProcessResponse.getGroupCode())))
        .thenReturn(productBasicDetailsResponse);
    activateImageResponse = new ActivateImageResponse();
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    productCollection = generateProductCollectionResponse();
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(productCollection);
    mapper = new ObjectMapper();
  }

  private ProductResponse generateProductBasicDetailResponse() {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setViewable(Boolean.FALSE);
    productResponse.setActivated(Boolean.TRUE);
    return productResponse;
  }

  private ProductCollection generateProductCollectionResponse(){
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setState("IN_PROGRESS");
    return productCollection;
  }

  @Test
  public void onDomainEventConsumedSuccessFalse() throws Exception {
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedStatusPrioritySeller1SuccessFalse() throws Exception {
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumedStatusPrioritySeller1(message);
    Mockito.verify(this.productDistributionTaskRepository)
            .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
            .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.anyString(), Mockito.eq(null));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
            .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedStatusPrioritySeller2SuccessFalse() throws Exception {
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumedStatusPrioritySeller2(message);
    Mockito.verify(this.productDistributionTaskRepository)
            .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
            .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.anyString(), Mockito.eq(null));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
            .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedSuccessTrue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedSuccessTrueWithScalingSkipped() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    bulkImageProcessResponse.getImageResponses().forEach(imageResponse -> imageResponse.setClientId(Constants.PBP_SCALING_SKIPPED_ACTION));
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
      .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
      Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
      Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
      Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
      Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productService)
      .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
      .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(productService)
      .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
      .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedPrioritySeller1SuccessTrue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumedStatusPrioritySeller1(message);
    Mockito.verify(this.productDistributionTaskRepository)
            .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
            Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
            Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productService)
            .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.kafkaProducer)
            .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
                    new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(productService)
            .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
            .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventPriority1();
  }

  @Test
  public void onDomainEventConsumedPrioritySeller2SuccessTrue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumedStatusPrioritySeller2(message);
    Mockito.verify(this.productDistributionTaskRepository)
            .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
                    Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
            Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
            Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productService)
            .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.kafkaProducer)
            .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
                    new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(productService)
            .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
            .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventPriority2();
  }

  @Test
  public void onDomainEventConsumedPostLiveActivationTrue() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "retryCreateSwitch", true);
    productCollection.setPostLive(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(Mockito.anyString(),
        Mockito.any())).thenReturn(Arrays.asList(new ProductBusinessPartner()));
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.any(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.any(), Mockito.eq(null));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productDistributionService)
        .removeProductFromPDT(Mockito.any(), Mockito.any(), removeProductRequestArgumentCaptor.capture());
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.any(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
    Assertions.assertEquals(PRODUCT_CODE, removeProductRequestArgumentCaptor.getValue().getProductCode());
  }


  @Test
  public void onDomainEventConsumedPostLiveActivationTrue1() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "retryCreateSwitch", true);
    productCollection.setPostLive(true);
    productCollection.setProductCode(PRODUCT_CODE);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(Mockito.any(),
        Mockito.any())).thenReturn(new ArrayList<>());
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.any(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.any(), Mockito.eq(null));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productDistributionService)
        .removeProductFromPDT(Mockito.any(), Mockito.any(), removeProductRequestArgumentCaptor.capture());
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.any(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productServiceWrapper)
        .processProductVendorSearchAutoHeal(GdnMandatoryRequestParameterUtil.getStoreId(),
            productCollection.getProductCode());
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
    Assertions.assertEquals(PRODUCT_CODE, removeProductRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void onDomainEventConsumedPostLiveActivationExceptionTrue() throws Exception {
    productCollection.setPostLive(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    Mockito.when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    Mockito.doThrow(RuntimeException.class).when(productService)
        .updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productDistributionService)
        .removeProductFromPDT(Mockito.anyString(), Mockito.anyString(), removeProductRequestArgumentCaptor.capture());
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
    Assertions.assertEquals(PRODUCT_CODE, removeProductRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void onDomainEventConsumedSuccessWithSkipReviewTrue() throws Exception {
    productCollection.setSkipReview(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    Mockito.when(
        this.productCollectionRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(productCollection);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(true));
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
  }

  @Test
  public void onDomainEventConsumedPostLiveActivationFailure() throws Exception {
    productCollection.setPostLive(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    Mockito.when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedSuccessPostliveTrue() throws Exception {
    productCollection.setReviewPending(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService)
        .publishProductStatusEventByProductCode(eq(PRODUCT_CODE), eq(ProductStatus.ACTIVE), eq(StringUtils.EMPTY));
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedProductAlreadyActive() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedImageActiveFalse() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedException() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.TRUE);
    Mockito.doThrow(new RuntimeException()).when(productDistributionService)
        .removeProductFromPDT(Mockito.anyString(),
            Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
            Mockito.any(RemoveProductRequest.class));
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(bulkImageProcessResponse.getGroupCode());
    Mockito.verify(this.productWfService).approveImage(productCollection.getProductCode());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }
  @Test
  public void onDomainEventConsumedIsViewableTrueAndStateNotActive() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "checkStateForXGPEventListenerImageActivationFlow", true);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    productCollection.setProductCode(bulkImageProcessResponse.getGroupCode());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(
            productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(productCollection.getProductCode());
    Mockito.verify(productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedIsViewableTrueAndStateActive() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "checkStateForXGPEventListenerImageActivationFlow", true);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    productCollection.setState("ACTIVE");
    productCollection.setProductCode(bulkImageProcessResponse.getGroupCode());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(productCollection.getProductCode());
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedWhenProductResponseFromPCBNullTest() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "checkStateForXGPEventListenerImageActivationFlow", true);
    Mockito.when(productService.findProductBasicDetailByProductCode(Mockito.eq(bulkImageProcessResponse.getGroupCode())))
        .thenReturn(null);
    productCollection.setState("ACTIVE");
    productCollection.setProductCode(bulkImageProcessResponse.getGroupCode());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(productCollection.getProductCode());
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedWhenCheckStateForXGPEventListenerImageActivationFlowFalseTest() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "checkStateForXGPEventListenerImageActivationFlow", false);
    Mockito.when(productService.findProductBasicDetailByProductCode(Mockito.eq(bulkImageProcessResponse.getGroupCode())))
        .thenReturn(null);
    productBasicDetailsResponse.setViewable(Boolean.TRUE);
    productCollection.setState("ACTIVE");
    productCollection.setProductCode(bulkImageProcessResponse.getGroupCode());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
            new SolrReviewProductCollectionDeleteEvent(Collections.singletonList(productCollection.getId())));
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(productCollection.getProductCode());
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @Test
  public void onDomainEventConsumedWhenCheckStateForXGPEventListenerImageActivationFlowTrueTest() throws Exception {
    ReflectionTestUtils.setField(imageKafkaSubscriberBean, "checkStateForXGPEventListenerImageActivationFlow", true);
    productCollection.setProductCode(bulkImageProcessResponse.getGroupCode());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, distributionProductDetailResponse.getUpdatedBy());
    bulkImageProcessResponse.getImageResponses().get(0).setSuccess(Boolean.TRUE);
    activateImageResponse.setActive(Boolean.FALSE);
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(
            productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    this.imageKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(this.productDistributionTaskRepository)
        .getDetailsForAnyProduct(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(productService).findProductBasicDetailByProductCode(productCollection.getProductCode());
    Mockito.verify(productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean());
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(bulkImageProcessResponse.getStoreId()),
        Mockito.eq(bulkImageProcessResponse.getGroupCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).rejectImage(Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    Mockito.verify(this.productDistributionTaskRepository)
        .moveFailedProductToQC(Mockito.anyString(), Mockito.eq(Constants.XGP_USER),
            Mockito.eq(bulkImageProcessResponse.getGroupCode()));
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString());
    verify(kafkaTopicProperties).getImageScaleStatusEventNoPriority();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productDistributionTaskRepository, productWfService);
    Mockito.verifyNoMoreInteractions(this.productService, solrReviewProductCollectionService);
    Mockito.verifyNoMoreInteractions(productCollectionRepository);
    Mockito.verifyNoMoreInteractions(productOutbound, objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(productDistributionService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  private DistributionProductDetailResponse generateDistributionProductDetailResponse() throws Exception {
    DistributionProductDetailResponse distributionProductDetailResponse = new DistributionProductDetailResponse();
    distributionProductDetailResponse.setUpdatedBy(DEFAULT_USERNAME);
    return distributionProductDetailResponse;
  }

  private ImageResultDetailList generateImageResultDetailList() throws Exception {
    ImageResultDetail imageResultDetail1 = new ImageResultDetail();
    imageResultDetail1.setImagePathLocation(DEFAULT_LOCATION_PATH);
    imageResultDetail1.setClientId(DEFAULT_CLIENT_ID);
    ImageResultDetail imageResultDetail2 = new ImageResultDetail();
    imageResultDetail2.setImagePathLocation(DEFAULT_LOCATION_PATH);
    imageResultDetail2.setRequestId(DEFAULT_REQUEST_ID);
    imageResultDetail2.setSuccess(true);
    imageResultDetail2.setClientId(DEFAULT_CLIENT_ID);
    ImageResultDetailList imageResultDetailList = new ImageResultDetailList();
    imageResultDetailList.setResultList(new ArrayList<ImageResultDetail>());
    imageResultDetailList.getResultList().add(imageResultDetail1);
    imageResultDetailList.getResultList().add(imageResultDetail2);
    return imageResultDetailList;
  }

  private ImageResponse generateImageResponse() throws Exception {
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setImagePathLocation(FILE_LOCATION);
    imageResponse.setErrorMessage(ERROR_MESSAGE);
    imageResponse.setClientId(DEFAULT_CLIENT_ID);
    imageResponse.setHashCode(HASH_CODE);
    imageResponse.setSuccess(Boolean.TRUE);
    return imageResponse;
  }

  private BulkImageProcessResponse generateBulkImageProcessResponse() throws Exception {
    BulkImageProcessResponse bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setGroupCode(PRODUCT_CODE);
    bulkImageProcessResponse.setImageResponses(Arrays.asList(generateImageResponse()));
    bulkImageProcessResponse.setStoreId(STORE_ID);
    bulkImageProcessResponse.setUsername(USER_NAME);
    return bulkImageProcessResponse;
  }

  private ScaleImageInformationRequest generateScaleImageInformationRequest() throws Exception {
    ScaleImageInformationRequest scaleImageInformationRequest = new ScaleImageInformationRequest();
    scaleImageInformationRequest.setRequestId(DEFAULT_REQUEST_ID);
    return scaleImageInformationRequest;
  }

}