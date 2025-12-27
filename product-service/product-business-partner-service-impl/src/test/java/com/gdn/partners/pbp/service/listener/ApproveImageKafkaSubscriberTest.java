package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageInformationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.ApproveImageMessageConstants;
import com.gdn.mta.domain.event.modal.ApproveProductResponse;
import com.gdn.mta.domain.event.modal.XGPImageInfoDomainEvent;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ApproveImageKafkaSubscriberTest {

  private static final String HASH_CODE = "hashCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String IMAGE_LOCATION = "imageLocation";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "clientId";
  private static final String USER_NAME = "userName";
  private static final String REQUEST_ID = "requserId";

  @InjectMocks
  private ApproveImageKafkaSubscriber approveImageKafkaSubscriber;

  private XGPImageInfoDomainEvent xgpImageInfoDomainEvent;
  private com.gdn.x.productcategorybase.dto.response.ProductResponse productResponse;
  private ActivateImageResponse activateImageResponse;
  private ObjectMapper mapper;

  @Mock
  private ProductService productService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(this.approveImageKafkaSubscriber,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, "");
    ReflectionTestUtils.setField(this.approveImageKafkaSubscriber,
        GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, "");
    ImageInformationRequest imageInformationRequest = new ImageInformationRequest();
    imageInformationRequest.setHashCode(HASH_CODE);
    imageInformationRequest.setClientId(CLIENT_ID);
    imageInformationRequest.setImagePathLocation(IMAGE_LOCATION);
    this.xgpImageInfoDomainEvent = new XGPImageInfoDomainEvent();
    List<ImageInformationRequest> imageInformationRequests =  new ArrayList<>();
    imageInformationRequests.add(imageInformationRequest);
    this.xgpImageInfoDomainEvent.setImageInformationRequests(imageInformationRequests);
    this.xgpImageInfoDomainEvent.setProductCode(PRODUCT_CODE);
    this.xgpImageInfoDomainEvent.setStoreId(STORE_ID);
    this.xgpImageInfoDomainEvent.setClientId(CLIENT_ID);
    productResponse = new ProductResponse();
    productResponse.setActivated(true);
    productResponse.setViewable(true);
    activateImageResponse = new ActivateImageResponse();
    mapper = new ObjectMapper();
    Mockito.when(this.productService.findProductBasicDetailByProductCode(PRODUCT_CODE))
        .thenReturn(productResponse);
    Mockito.when(this.productService.publishProductDetailsEvent(Mockito.anyString(),
        Mockito.anyString())).thenReturn(new ApproveProductResponse());
    Mockito.doNothing().when(productWfService).approveImage(PRODUCT_CODE);
    Mockito.doNothing().when(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.productWfService, this.objectMapper);
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(null);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(null);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);    Mockito.verify(this.productService).publishProductDetailsEvent(null,
        ApproveImageMessageConstants.EMPTY_MESSAGE_RECEIVED);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableFalseImageActiveTrue() throws Exception {
    productResponse.setViewable(false);
    activateImageResponse.setActive(true);
    Mockito.when(productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false)))
        .thenReturn(activateImageResponse);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productWfService).approveImage(PRODUCT_CODE);
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
    Mockito.verify(productService)
        .publishProductDetailsEvent(PRODUCT_CODE, ApproveImageMessageConstants.PRODUCT_VIEWABLE);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableFalseImageActiveFalse() throws Exception {
    productResponse.setViewable(false);
    activateImageResponse.setActive(false);
    Mockito.when(productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false)))
        .thenReturn(activateImageResponse);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService)
        .publishProductDetailsEvent(PRODUCT_CODE, ApproveImageMessageConstants.IMAGE_NOT_ACTIVE);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableFalseImageActiveTrueExceptionCode3() throws Exception {
    productResponse.setViewable(false);
    activateImageResponse.setActive(true);
    Mockito.when(productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false)))
        .thenReturn(activateImageResponse);
    ApplicationException ex = new ApplicationException();
    ex.setErrorCodes(ErrorCategory.INVALID_STATE);
    Mockito.doThrow(ex).when(productWfService).approveImage(PRODUCT_CODE);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService)
        .publishProductDetailsEvent(PRODUCT_CODE, ex.getMessage());
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
    Mockito.verify(productWfService, Mockito.times(2)).approveImage(PRODUCT_CODE);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableFalseImageActiveTrueApplicationException() throws Exception {
    productResponse.setViewable(false);
    activateImageResponse.setActive(true);
    Mockito.when(productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false)))
        .thenReturn(activateImageResponse);
    ApplicationException ex = new ApplicationException();
    ex.setErrorCodes(ErrorCategory.DATA_NOT_FOUND);
    Mockito.doThrow(ex).when(productWfService).approveImage(PRODUCT_CODE);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService)
        .publishProductDetailsEvent(PRODUCT_CODE, ex.getMessage());
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
    Mockito.verify(productWfService).approveImage(PRODUCT_CODE);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableFalseImageActiveTrueException() throws Exception {
    productResponse.setViewable(false);
    activateImageResponse.setActive(true);
    Mockito.when(productService.updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false)))
        .thenReturn(activateImageResponse);
    Exception ex = new Exception();
    Mockito.doThrow(ex).when(productWfService).approveImage(PRODUCT_CODE);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService).updateProductImagesName(Mockito.any(ProductActivateImageRequest.class), Mockito.eq(false));
    Mockito.verify(productService).publishProductDetailsEvent(PRODUCT_CODE, ex.getMessage());
    Mockito.verify(productService).saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
    Mockito.verify(productWfService).approveImage(PRODUCT_CODE);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }

  @Test
  public void listenWithViewableTrue() throws Exception {
    productResponse.setViewable(true);
    activateImageResponse.setActive(true);
    Mockito.when(productService.updateProductImageName(Mockito.any(ActivateImageRequest.class)))
        .thenReturn(activateImageResponse);
    String message = mapper.writeValueAsString(xgpImageInfoDomainEvent);
    Mockito.when(objectMapper.readValue(message, XGPImageInfoDomainEvent.class))
        .thenReturn(xgpImageInfoDomainEvent);
    this.approveImageKafkaSubscriber.onDomainEventConsumed(message);
    Mockito.verify(productService).findProductBasicDetailByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .publishProductDetailsEvent(PRODUCT_CODE,ApproveImageMessageConstants.PRODUCT_VIEWABLE_ALREADY);
    verify(objectMapper).readValue(message, XGPImageInfoDomainEvent.class);
  }
}
