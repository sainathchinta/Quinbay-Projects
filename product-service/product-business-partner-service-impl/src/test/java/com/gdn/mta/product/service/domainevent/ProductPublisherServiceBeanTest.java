package com.gdn.mta.product.service.domainevent;

import static org.mockito.ArgumentMatchers.any;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ProductCreationType;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductImageQcProcessingResponseService;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class ProductPublisherServiceBeanTest {
  private EditedImageResizeEvent editedImageResizeEvent;
  private ImageResizeEvent imageResizeEvent;

  @InjectMocks
  private ProductPublisherServiceBean instance;
  @Mock
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ProductDetailResponse productDetailResponse;
  private ProductImageQcProcessingResponse productImageQcProcessingResponse;
  private SellerDetailResponse sellerDetailResponse;

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_PRODUCT_ID = "44fa4b52-7920-45a0-8ef9-3cbe63bd3594";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-" + UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_NAME = "Produk";
  private static final byte[] DEFAULT_PRODUCT_DESC = "Deskripsi".getBytes();
  private static final byte[] DEFAULT_PRODUCT_LONG_DESC = "Deskripsi Panjang".getBytes();
  private static final String DEFAULT_PRODUCT_BRAND = "Brand";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-123";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "default filename";
  private static final String RESTRICTED_KEYWORDS = "restrictedKeywords";
  private static final Double PRICE = 100.0;
  private static final String PRODUCT_ITEM_SKU = "SKU";

  private RestrictedKeywordsByField restrictedKeywordsByField;
  private String restrictedKeywordJson;


  public ProductDetailResponse createDummyProductDetailResponse() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse(
        new ProductResponse.Builder().productCode(DEFAULT_PRODUCT_CODE).name(DEFAULT_PRODUCT_NAME).length(1.0)
            .width(1.0).weight(1.0).height(1.0).shippingWeight(1.0).description(DEFAULT_PRODUCT_DESC)
            .longDescription(DEFAULT_PRODUCT_LONG_DESC).brand(DEFAULT_PRODUCT_BRAND).uniqueSellingPoint(null).uom(null)
            .activated(false).viewable(true).productStory(null).productType(null).url(null).images(null)
            .specificationDetail(null).build());
    productDetailResponse.setId(DEFAULT_PRODUCT_ID);
    ProductAttributeResponse productAttributeResponse_ = new ProductAttributeResponse();
    AttributeResponse attributeResponse =
        new AttributeResponse(null, null, AttributeType.DESCRIPTIVE_ATTRIBUTE.toString(), false);
    productAttributeResponse_.setAttribute(attributeResponse);
    productAttributeResponse_.setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    productAttributeResponse_.getProductAttributeValues().add(
        new ProductAttributeValueResponse(null, "test", DescriptiveAttributeValueType.SINGLE, null, DEFAULT_STORE_ID));
    productAttributeResponse_.getProductAttributeValues().add(
        new ProductAttributeValueResponse(new AllowedAttributeValueResponse("test", 1, DEFAULT_STORE_ID), "test",
            DescriptiveAttributeValueType.SINGLE, null, DEFAULT_STORE_ID));
    productAttributeResponse_.getProductAttributeValues().add(
        new ProductAttributeValueResponse(null, "test", DescriptiveAttributeValueType.PREDEFINED,
            new PredefinedAllowedAttributeValueResponse("test", 0, DEFAULT_STORE_ID), DEFAULT_STORE_ID));
    ProductAttributeValueResponse productAttributeValueResponseDeleted =
        new ProductAttributeValueResponse(null, "test", DescriptiveAttributeValueType.PREDEFINED,
            new PredefinedAllowedAttributeValueResponse("test", 0, DEFAULT_STORE_ID), DEFAULT_STORE_ID);
    productAttributeValueResponseDeleted.setMarkForDelete(true);
    productAttributeResponse_.getProductAttributeValues().add(productAttributeValueResponseDeleted);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    ProductItemResponse productItemResponseWithImage =
        new ProductItemResponse("Produk 1 - Produk Item 1", null, null, null, null, null, UUID.randomUUID().toString(),
            null, true, false, null, null);
    productItemResponseWithImage.setImages(new ArrayList<Image>());
    productItemResponseWithImage.getImages().add(new Image(true, "1234", 0));
    Image imageDeleted = new Image();
    imageDeleted.setMarkForDelete(true);
    productItemResponseWithImage.getImages().add(imageDeleted);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setAttributeResponse(attributeResponse);
    productItemResponseWithImage.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValue));
    productDetailResponse.getProductItemResponses().add(productItemResponseWithImage);
    ProductItemResponse productItemResponseDeleted = new ProductItemResponse();
    productItemResponseDeleted.setMarkForDelete(true);
    productDetailResponse.getProductItemResponses().add(productItemResponseDeleted);
    productDetailResponse.getProductCategoryResponses()
        .add(new ProductCategoryResponse(new CategoryResponse(), DEFAULT_STORE_ID));
    ProductCategoryResponse productCategoryResponseDeleted =
        new ProductCategoryResponse(new CategoryResponse(), DEFAULT_STORE_ID);
    productCategoryResponseDeleted.setMarkForDelete(true);
    productDetailResponse.getProductCategoryResponses().add(productCategoryResponseDeleted);
    ProductAttributeResponse productAttributeResponseDeleted = new ProductAttributeResponse();
    productAttributeResponseDeleted.setMarkForDelete(true);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse_);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponseDeleted);
    productDetailResponse.setImages(new ArrayList<Image>());
    productDetailResponse.getImages().add(new Image(true, "1234", 0));
    productDetailResponse.getImages().add(imageDeleted);

    return productDetailResponse;
  }


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    sellerDetailResponse = new SellerDetailResponse();
    sellerDetailResponse.setSellerBadge("gold");
    this.productDetailResponse = createDummyProductDetailResponse();
    productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
    productImageQcProcessingResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    editedImageResizeEvent = new EditedImageResizeEvent();
    restrictedKeywordsByField = new RestrictedKeywordsByField(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
        Arrays.asList(RESTRICTED_KEYWORDS));
    restrictedKeywordJson = new ObjectMapper().writeValueAsString(Arrays.asList(restrictedKeywordsByField));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImageQcProcessingResponseService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void publish() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCreationType(ProductCreationType.FLOW1.getProductCreationType());
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(sellerDetailResponse);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
            DEFAULT_CLIENT_ID, Boolean.FALSE, Boolean.FALSE, restrictedKeywordJson, 0,
          false, DEFAULT_PRODUCT_ID, productCollection);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.any(),
            Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertFalse(screeningProductApprovalEvent.isPostLive());
    Assertions.assertFalse(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertFalse(screeningProductApprovalEvent.getRestrictedKeywordsDetected().isEmpty());
    Assertions.assertEquals(screeningProductApprovalEvent.getSellerBadge(),sellerDetailResponse.getSellerBadge());
  }

  @Test
  public void publishImageQcCheckNonNull() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(sellerDetailResponse);
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        DEFAULT_PRODUCT_CODE)).thenReturn(productImageQcProcessingResponse);
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventPriority1())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
            DEFAULT_CLIENT_ID, Boolean.FALSE, Boolean.FALSE, StringUtils.EMPTY,
            PrioritySeller.PRIORITY_1.getPrioritySeller(), false, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventPriority1();
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1), Mockito.any(),
            Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertFalse(screeningProductApprovalEvent.isPostLive());
    Assertions.assertFalse(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertTrue(screeningProductApprovalEvent.isImageQcCheck());
    Assertions.assertTrue(screeningProductApprovalEvent.getRestrictedKeywordsDetected().isEmpty());
    Assertions.assertFalse(screeningProductApprovalEvent.isTrustedSeller());
    Assertions.assertEquals(sellerDetailResponse.getSellerBadge(),screeningProductApprovalEvent.getSellerBadge());
  }

  @Test
  public void publishImageQcCheckNonNull_forTrustedSellerTest() throws Exception {
    Mockito.when(productImageQcProcessingResponseService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
      DEFAULT_PRODUCT_CODE)).thenReturn(productImageQcProcessingResponse);
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(sellerDetailResponse);
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventPriority1())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
      instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
        DEFAULT_CLIENT_ID, Boolean.FALSE, Boolean.FALSE, StringUtils.EMPTY,
        PrioritySeller.PRIORITY_1.getPrioritySeller(), true, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventPriority1();
    Mockito.verify(productImageQcProcessingResponseService)
      .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1), Mockito.any(),
        Mockito.any());
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertFalse(screeningProductApprovalEvent.isPostLive());
    Assertions.assertFalse(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertTrue(screeningProductApprovalEvent.isImageQcCheck());
    Assertions.assertTrue(screeningProductApprovalEvent.getRestrictedKeywordsDetected().isEmpty());
    Assertions.assertTrue(screeningProductApprovalEvent.isTrustedSeller());
  }

  @Test
  public void publishPostLiveProduct() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(sellerDetailResponse);
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventPriority2())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
            DEFAULT_CLIENT_ID, Boolean.TRUE, Boolean.TRUE, restrictedKeywordJson,
            PrioritySeller.PRIORITY_2.getPrioritySeller(), false, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventPriority2();
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2), Mockito.any(),
            Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertTrue(screeningProductApprovalEvent.isPostLive());
    Assertions.assertTrue(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(RESTRICTED_KEYWORDS,
        screeningProductApprovalEvent.getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
  }

  @Test
  public void publishPriceInfoProduct() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId(GdnBaseEntity.ID);
    ReflectionTestUtils.setField(instance, "priceInfoScreeningApprovalEnabled", true);
    ReflectionTestUtils.setField(instance, "priceInfoMaxVariantLimit", 10);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductBusinessPartnerId(GdnBaseEntity.ID);
    productItemBusinessPartner.setProductItemId(DEFAULT_PRODUCT_ID);
    productItemBusinessPartner.setPrice(PRICE);
    productItemBusinessPartner.setGdnProductItemSku(PRODUCT_ITEM_SKU);
    List<ProductItemBusinessPartner> productItemBusinessPartnerList = new ArrayList<>();
    productItemBusinessPartnerList.add(productItemBusinessPartner);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(productBusinessPartner);
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(sellerDetailResponse);
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventPriority2())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_BUSINESS_PARTNER_NAME, DEFAULT_CLIENT_ID, Boolean.TRUE, Boolean.TRUE,
            restrictedKeywordJson, PrioritySeller.PRIORITY_2.getPrioritySeller(), false,
            DEFAULT_PRODUCT_ID, null);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventPriority2();
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2),
            Mockito.any(), Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertNotNull(screeningProductApprovalEvent.getPriceInfo());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertTrue(screeningProductApprovalEvent.isPostLive());
    Assertions.assertTrue(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(RESTRICTED_KEYWORDS,
        screeningProductApprovalEvent.getRestrictedKeywordsDetected().get(0).getKeywords().get(0));
  }

  @Test
  public void publishWithNullItemAttributes() throws Exception {
    productDetailResponse.getProductItemResponses().forEach(productItemResponse -> {
      productItemResponse.setProductItemAttributeValueResponses(null);
    });
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(sellerDetailResponse);
    ScreeningProductApprovalEvent screeningProductApprovalEvent=instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
        DEFAULT_CLIENT_ID, false, false, restrictedKeywordJson, 0, false, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.any(),
            Mockito.any());
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(sellerDetailResponse.getSellerBadge(), screeningProductApprovalEvent.getSellerBadge());
  }

  @Test
  public void publishEditImageResizeEventTest() {
    instance.publishEditImageResizeEvent(editedImageResizeEvent);
    Mockito.verify(kafkaProducer).send(DomainEventName.EDITED_IMAGE_RESIZE_EVENT, editedImageResizeEvent);
  }

  @Test
  public void publishException() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.doThrow(RuntimeException.class).when(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
            DEFAULT_CLIENT_ID, Boolean.FALSE, Boolean.FALSE, restrictedKeywordJson, 0,
            false, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.any(),
            Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertFalse(screeningProductApprovalEvent.isPostLive());
    Assertions.assertFalse(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertFalse(screeningProductApprovalEvent.getRestrictedKeywordsDetected().isEmpty());
  }

  @Test
  public void publishRevisedProductToPDTTestForException() {
    Mockito.doThrow(RuntimeException.class).when(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent = new AddRevisedProductToPDTEvent();
    addRevisedProductToPDTEvent.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent1=instance.publishRevisedProductToPDT(addRevisedProductToPDTEvent);
    Mockito.verify(kafkaProducer).send(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT, null,
        AddProductToVendorCombinedEventModel.builder().addRevisedProductToPDTEvent(addRevisedProductToPDTEvent)
            .build());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void testPublishProductImageResizeEventForPrioritySeller_WithPrioritySellerFor1() {
    Mockito.when(kafkaTopicProperties.getImageResizeEventPriority1())
        .thenReturn(DomainEventName.IMAGE_RESIZE_PRIORITY_SELLER_EVENT_1);
    ImageResizeEvent result =
        instance.publishProductImageResizeEventForPrioritySeller(DEFAULT_PRODUCT_ID, DEFAULT_STORE_ID, 1);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.IMAGE_RESIZE_PRIORITY_SELLER_EVENT_1, DEFAULT_PRODUCT_ID, result);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority1();
  }

  @Test
  public void testPublishProductImageResizeEventForPrioritySeller_WithPrioritySellerFor2() {
    Mockito.when(kafkaTopicProperties.getImageResizeEventPriority2())
        .thenReturn(DomainEventName.IMAGE_RESIZE_PRIORITY_SELLER_EVENT_2);
    ImageResizeEvent result =
        instance.publishProductImageResizeEventForPrioritySeller(DEFAULT_PRODUCT_ID, DEFAULT_STORE_ID, 2);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.IMAGE_RESIZE_PRIORITY_SELLER_EVENT_2, DEFAULT_PRODUCT_ID, result);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventPriority2();
  }

  @Test
  public void publishReviseImageResizeEventTest() {
    instance.publishReviseImageResizeEvent(editedImageResizeEvent);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.REVISE_IMAGE_RESIZE_EVENT, editedImageResizeEvent.getProductCode(),
            editedImageResizeEvent);
  }

  @Test
  public void publishForNull() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any())).thenReturn(null);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(null);
    ScreeningProductApprovalEvent screeningProductApprovalEvent =
        instance.publish(DEFAULT_PRODUCT_CODE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_NAME,
            DEFAULT_CLIENT_ID, Boolean.FALSE, Boolean.FALSE, restrictedKeywordJson, 0,
            false, DEFAULT_PRODUCT_ID, null);
    Mockito.verify(productImageQcProcessingResponseService)
        .findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.any(),
            Mockito.any());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, screeningProductApprovalEvent.getProductCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, screeningProductApprovalEvent.getMerchantCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME, screeningProductApprovalEvent.getMerchantName());
    Assertions.assertEquals(DEFAULT_CLIENT_ID, screeningProductApprovalEvent.getUpdatedBy());
    Assertions.assertFalse(screeningProductApprovalEvent.isPostLive());
    Assertions.assertFalse(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    Assertions.assertFalse(screeningProductApprovalEvent.getRestrictedKeywordsDetected().isEmpty());
  }

  @Test
  public void publishRevisedProductToPDTTestForNull() {
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent = new AddRevisedProductToPDTEvent();
    addRevisedProductToPDTEvent.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent1=instance.publishRevisedProductToPDT(addRevisedProductToPDTEvent);
    Mockito.verify(kafkaProducer).send(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT, null,
        AddProductToVendorCombinedEventModel.builder().addRevisedProductToPDTEvent(addRevisedProductToPDTEvent)
            .build());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void publishRevisedProductToPDTTest() {
    Mockito.when(productAnalyticsOutbound.getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(sellerDetailResponse);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent = new AddRevisedProductToPDTEvent();
    addRevisedProductToPDTEvent.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent1=instance.publishRevisedProductToPDT(addRevisedProductToPDTEvent);
    Mockito.verify(kafkaProducer).send(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT, null,
        AddProductToVendorCombinedEventModel.builder().addRevisedProductToPDTEvent(addRevisedProductToPDTEvent)
            .build());
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void productQCRetryEventTest() {
    instance.publishProductQCRetryEvent(new ProductQCRetryEvent());
    Mockito.verify(kafkaProducer).send(DomainEventName.PRODUCT_QC_RETRY_EVENT, null, new ProductQCRetryEvent());
  }

  @Test
  public void publishProductDimensionRefreshEventTest() {
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().productCode(DEFAULT_PRODUCT_CODE).build();
    instance.publishProductDimensionRefreshEvent(pdtDimensionRefreshEventModel);
    Mockito.verify(kafkaProducer).send(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT, DEFAULT_PRODUCT_CODE,
        AddProductToVendorCombinedEventModel.builder().pdtDimensionRefreshEventModel(pdtDimensionRefreshEventModel)
            .build());
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void publishProductImageResizeEventTest() {
    Mockito.when(kafkaTopicProperties.getImageResizeEventNoPriority()).thenReturn(DomainEventName.IMAGE_RESIZE_EVENT);
    instance.publishProductImageResizeEvent(DEFAULT_PRODUCT_CODE, DEFAULT_STORE_ID);
    Mockito.verify(kafkaTopicProperties).getImageResizeEventNoPriority();
    Mockito.verify(kafkaProducer).send(DomainEventName.IMAGE_RESIZE_EVENT, DEFAULT_PRODUCT_CODE,
        new ImageResizeEvent(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE));
  }
}