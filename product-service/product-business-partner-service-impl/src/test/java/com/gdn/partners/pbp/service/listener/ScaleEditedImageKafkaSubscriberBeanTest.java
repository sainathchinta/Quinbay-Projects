package com.gdn.partners.pbp.service.listener;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
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

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.domain.event.modal.ApproveProductResponse;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductDistributionService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseServiceImpl;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;

public class ScaleEditedImageKafkaSubscriberBeanTest {

  private static final String HASH_CODE = "hashCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String IMAGE_LOCATION = "imageLocation";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "clientId";
  private static final String USER_NAME = "userName";
  private static final String ID = "id";
  private static final String REQUEST_ID = "requserId";
  private static final String IMAGE_QC_RESPONSE_EDITED_FALSE = "{\"timestamp\":1624520957759,\"productCode\":\"MTA-0496193\",\"images\":[{"
      + "\"locationPath\":\"/filestore/mta/images/source/MTA-0496193/resize/puma_apiautomation_full01.jpg\",\"hashCode\":\"46815a07c4f8ccff3361cd1c7ddeb08f\",\"predi"
      + "ctions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"display"
      + "Name\":\"Blur\",\"present\":false,\"confidence\":35},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionT"
      + "ype\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}],\"edited\":false}],\"success\":true,\"errorMessage\":\"\"}";
  private static final String IMAGE_QC_RESPONSE_EDITED_TRUE = "{\"timestamp\":1624520957759,\"productCode\":\"MTA-0496193\",\"images\":[{"
      + "\"locationPath\":\"/filestore/mta/images/source/MTA-0496193/resize/puma_apiautomation_full01.jpg\",\"hashCode\":\"46815a07c4f8ccff3361cd1c7ddeb08f\",\"predi"
      + "ctions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"display"
      + "Name\":\"Blur\",\"present\":false,\"confidence\":35},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":0},{\"predictionT"
      + "ype\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}],\"edited\":true}],\"success\":true,\"errorMessage\":\"\"}";

  @InjectMocks
  private ScaleEditedImageKafkaSubscriberBean scaleEditedImageKafkaSubscriberBean;

  private ScaleEditedImagesResponse scaleEditedImagesResponse;
  private ProductCollection productCollection;
  private ActivateImageResponse activateImageResponse;
  private ObjectMapper mapper;
  private ProductImageQcProcessingResponse productImageQcProcessingResponse;

  @Mock
  private ProductService productService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductImageQcProcessingResponseServiceImpl productImageQcProcessingResponseService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductDistributionService productDistributionService;

  @Captor
  private ArgumentCaptor<ProductImageQcProcessingResponse> productImageQcProcessingResponseArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils
        .setField(this.scaleEditedImageKafkaSubscriberBean, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            "");
    ReflectionTestUtils
        .setField(this.scaleEditedImageKafkaSubscriberBean, GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, "");
    ReflectionTestUtils.setField(this.scaleEditedImageKafkaSubscriberBean, "relaxEditedFlagCheckImageResizeEvent", true);
    ScaleImageResponse scaleImageResponse =
        ScaleImageResponse.builder().hashCode(HASH_CODE).clientId(CLIENT_ID).imagePathLocation(IMAGE_LOCATION)
            .errorMessage(null).isActive(true).success(true).build();
    ScaleImageResponse scaleImageResponse1 =
        ScaleImageResponse.builder().hashCode(HASH_CODE).clientId(CLIENT_ID).imagePathLocation(null).errorMessage(null)
            .isActive(false).success(false).build();
    List<ScaleImageResponse> scaleImageResponses = new ArrayList<>();
    scaleImageResponses.add(scaleImageResponse);
    scaleImageResponses.add(scaleImageResponse1);
    this.scaleEditedImagesResponse =
        ScaleEditedImagesResponse.builder().imageResponses(scaleImageResponses).productCode(PRODUCT_CODE)
            .storeId(STORE_ID).username(USER_NAME).build();
    productCollection = new ProductCollection();
    activateImageResponse = new ActivateImageResponse();
    mapper = new ObjectMapper();
    Mockito.when(this.productService.publishProductDetailsEvent(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new ApproveProductResponse());
    Mockito.doNothing().when(productWfService).approveImage(PRODUCT_CODE);
    Mockito.doNothing().when(productService)
        .saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(productOutbound.republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_CODE)).thenReturn(PRODUCT_SKU);
    productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
    productImageQcProcessingResponse.setProductCode(PRODUCT_CODE);
    productImageQcProcessingResponse.setImageQcResponse(IMAGE_QC_RESPONSE_EDITED_FALSE);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productBusinessPartnerService, this.productOutbound);
    Mockito.verifyNoMoreInteractions(this.productService, this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.productWfService, this.objectMapper);
    Mockito.verifyNoMoreInteractions(this.solrReviewProductCollectionService, this.productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(this.productLevel3Service, this.productImageQcProcessingResponseService,
        this.productDistributionService);
  }

  @Test
  public void listenWithNullObject() throws Exception {
    scaleEditedImagesResponse = new ScaleEditedImagesResponse();
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class)).thenReturn(null);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
  }

  @Test
  public void listenWith_SuccessFalse() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
  }

  @Test
  public void listenWith_SuccessTrue() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void listenWith_ProductCodeEmpty() throws Exception {
    ReflectionTestUtils.setField(this.scaleEditedImageKafkaSubscriberBean, "relaxEditedFlagCheckImageResizeEvent", true);
    scaleEditedImagesResponse.setProductCode(StringUtils.EMPTY);
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(false);
    productCollection.setReviewPending(false);
    Mockito.when(productWfService.getProductCollectionByProductCode(StringUtils.EMPTY)).thenThrow(RuntimeException.class);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(StringUtils.EMPTY);
  }

  @Test
  public void listenWithEditedFalse() throws Exception {
    ReflectionTestUtils.setField(this.scaleEditedImageKafkaSubscriberBean, "relaxEditedFlagCheckImageResizeEvent", false);
    scaleEditedImagesResponse.setProductCode(StringUtils.EMPTY);
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(false);
    productCollection.setReviewPending(false);
    Mockito.when(productWfService.getProductCollectionByProductCode(StringUtils.EMPTY)).thenThrow(RuntimeException.class);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(StringUtils.EMPTY);
  }

  @Test
  public void listenWith_ActivateImageResponseTrueAndIdNull() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(true);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void listenWith_ActivateImageResponseTrue() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    productCollection.setId(ID);
    activateImageResponse.setActive(true);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
            productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService)
        .saveProductHistory(eq(scaleEditedImagesResponse.getStoreId()), eq(scaleEditedImagesResponse.getProductCode()),
            eq(Constants.DEFAULT_USERNAME), Mockito.anyString(), eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), eq(false));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(this.productDistributionService)
        .removeProductFromPDT(Mockito.anyString(), eq(GdnMandatoryRequestParameterUtil.getUsername()),
            Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(eq(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
            Mockito.any(SolrReviewProductCollectionDeleteEvent.class));
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void listenWith_ActivateImageResponseImageQCResponseNullTrue() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(true);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void listenWith_ActivateImageResponseImageQCResponseBlankTrue() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(true);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.productImageQcProcessingResponse.setImageQcResponse(StringUtils.EMPTY);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productLevel3Service).takeDownOrReactivateProduct(STORE_ID, PRODUCT_SKU, false, null, null);
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void listenWith_ActivateImageResponseTrue_Null() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(true);
    Mockito.when(productService.isProductActivationNeeded(STORE_ID, PRODUCT_CODE)).thenReturn(null);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productOutbound).republishProduct(Constants.UPDATE_OPERATION_TYPE, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productService).updateReviewType(STORE_ID, PRODUCT_CODE, null);
    Mockito.verify(this.productDistributionService).removeProductFromPDT(Mockito.anyString(),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()),
        Mockito.any(RemoveProductRequest.class));
    Mockito.verify(productService).isProductActivationNeeded(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService).updateSolrProductCollection(productCollectionArgumentCaptor.capture());
    Mockito.verify(productService).updateImageQcDataAfterVendorApproval(STORE_ID, PRODUCT_CODE);
    verify(xProductOutbound).generateProductScoreByProductSkuOrProductCode(null, PRODUCT_CODE, false);
  }

  @Test
  public void listenWith_isEditedFalse() throws Exception {
    ReflectionTestUtils.setField(this.scaleEditedImageKafkaSubscriberBean, "relaxEditedFlagCheckImageResizeEvent", false);
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(false);
    productCollection.setReviewPending(true);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(true);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
  }

  @Test
  public void listenWith_ActivateImageResponseFalse() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(true);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(false);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
        productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void listenWithActivateImageResponseFalse() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    productCollection.setEdited(false);
    productCollection.setReviewPending(false);
    productCollection.setProductId(PRODUCT_CODE);
    activateImageResponse.setActive(false);
    Mockito.when(productWfService.getProductCollectionByProductCode(PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.when(
            productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.anyBoolean()))
        .thenReturn(activateImageResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleEditedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
    Mockito.verify(this.productWfService).getProductCollectionByProductCode(PRODUCT_CODE);
    Mockito.verify(this.productService)
        .updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(this.productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
  }
}
