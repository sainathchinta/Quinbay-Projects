package com.gdn.partners.pcu.external.web.controller.util;


import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.external.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BundleRecipeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DimensionAndUomWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DistributionItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointCreateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PreOrderWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWholesalePriceWebRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

public class ConverterUtilTest {

  private static final String IMAGE_FILE_NAME = "IMAGE_FILE_NAME";
  private static final String IMAGE_FILE_NAME_JPEG = "IMAGE_FILE_NAME.jpeg";
  private static final String IMAGE_FILE_NAME_PNG = "IMAGE_FILE_NAME.png";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String VALUE = "value";
  private static final String STORE_ID = "storeId";
  private static final String USER_NAME = "userName";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";
  private static final String USP_WITH_SPECIAL_CHARACTERS_AND_SPACES =
      "• Case Premium Bagian Belakang Dove Transparan , Kelebihan :~Bagian belakang dove.\n"
          + "~Ada crack / Anti jatuh.\n" + "~Bagian belakang transparan tampil elegant\n"
          + "~Anti Pecah / Sobek karena pakai bahan lentur.\n" + "~Penuh warna glosy di List sekelilingnya\n"
          + "~Slim dan pas di gengam di tangan\n" + "~Anti lecet karena menggunakan Kualitas bahan terbaik\n"
          + "~Lepas pasang sangat mudah , tidak keras / kasar.";
  private static final String PREORDER_TYPE = "preOrderType";
  private static final Integer PREORDER_VALUE = 10;
  public static final String VIDEO_URL = "video-url";

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
  }

  @Test
  public void toUploadImageRequestTest() {
    UploadImageRequest request =
        ConverterUtil.toUploadImageRequest(IMAGE_FILE_NAME, null, null, Boolean.TRUE, Boolean.FALSE, IMAGE_FILE_NAME);
    assertEquals(request.getImageFileName(), IMAGE_FILE_NAME);
    assertEquals(request.isActive(), Boolean.TRUE);
  }

  @Test
  public void toProductCreationRequestTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setProductItemRequests(Arrays.asList(ProductItemCreationWebRequest.builder()
        .productItemAttributeValueRequests(Arrays.asList(ProductItemAttributeValueWebRequest
            .builder().attribute(AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
                .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).variantCreation(true).build())
            .value(VALUE).build())).sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setImagesUpdated(true);
    productCreationWebRequest.setOnline(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(new ProductItemLogisticsWebRequest());
    productCreationWebRequest.setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    PreOrderWebRequest preOrderWebRequest =
        PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productCreationWebRequest.setPreOrder(preOrderWebRequest);
    ProductCreationRequest request = ConverterUtil.toProductCreationRequest(
        productCreationWebRequest, mandatoryParameterHelper, false, true);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertEquals(productItemAttributeValueRequest.getAttribute().isVariantCreation(), true);
    assertTrue(request.isForReview());
    assertTrue(request.isImagesUpdated());
    assertTrue(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertTrue(request.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, request.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, request.getPreOrder().getPreOrderValue());
    assertTrue(request.isOnline());
  }

  @Test
  public void toProductCreationRequestMppTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setProductItemRequests(Collections.singletonList(ProductItemCreationWebRequest.builder()
        .productItemAttributeValueRequests(Collections.singletonList(ProductItemAttributeValueWebRequest.builder()
            .attribute(AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
                .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).variantCreation(true).build()).value(VALUE)
            .build())).sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setImagesUpdated(true);
    productCreationWebRequest.setOnline(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoUrl(VIDEO_URL);
    productCreationWebRequest.setVideoAddEditRequest(videoAddEditRequest);
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(new ProductItemLogisticsWebRequest());
    productCreationWebRequest.setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    PreOrderWebRequest preOrderWebRequest =
        PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productCreationWebRequest.setPreOrder(preOrderWebRequest);
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setFbbActivated(true);
    productCreationWebRequest.getProductItemRequests().get(0).setPickupPoints(Collections
        .singletonList(pickupPointCreateWebRequest));
    ProductCreationRequest request = ConverterUtil.toProductCreationRequest(
        productCreationWebRequest, mandatoryParameterHelper, true, false);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertTrue(productItemAttributeValueRequest.getAttribute().isVariantCreation());
    assertTrue(request.isForReview());
    assertTrue(request.isImagesUpdated());
    assertTrue(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertTrue(request.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, request.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, request.getPreOrder().getPreOrderValue());
    assertTrue(request.isOnline());
    assertTrue(request.getProductItemRequests().get(0).getPickupPoints().get(0).isFbbActivated());
    assertEquals(VIDEO_URL, request.getVideoAddEditRequest().getVideoUrl());
  }

  @Test
  public void toProductCreationRequestMpp_withCncViewConfigsTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setProductItemRequests(Collections.singletonList(ProductItemCreationWebRequest.builder()
      .productItemAttributeValueRequests(Collections.singletonList(ProductItemAttributeValueWebRequest.builder()
        .attribute(AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
          .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).variantCreation(true).build()).value(VALUE)
        .build())).sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setImagesUpdated(true);
    productCreationWebRequest.setOnline(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(new ProductItemLogisticsWebRequest());
    productCreationWebRequest.setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    PreOrderWebRequest preOrderWebRequest =
      PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productCreationWebRequest.setPreOrder(preOrderWebRequest);
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setFbbActivated(true);
    pickupPointCreateWebRequest.setCncBuyable(true);
    pickupPointCreateWebRequest.setCncDisplay(true);
    productCreationWebRequest.getProductItemRequests().get(0).setPickupPoints(Collections
      .singletonList(pickupPointCreateWebRequest));
    ProductCreationRequest request = ConverterUtil.toProductCreationRequest(
      productCreationWebRequest, mandatoryParameterHelper, true, false);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
      request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertTrue(productItemAttributeValueRequest.getAttribute().isVariantCreation());
    assertTrue(request.isForReview());
    assertTrue(request.isImagesUpdated());
    assertTrue(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertTrue(request.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, request.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, request.getPreOrder().getPreOrderValue());
    assertTrue(request.isOnline());
    assertTrue(request.getProductItemRequests().get(0).getPickupPoints().get(0).isFbbActivated());
    assertTrue(request.getProductItemRequests().get(0).getPickupPoints().get(0).isCncBuyable());
    assertTrue(request.getProductItemRequests().get(0).getPickupPoints().get(0).isCncDisplay());
  }

  @Test
  public void toProductCreationRequestMppBundleRecipeNotEmptyTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    Set<BundleRecipeWebRequest> bundleRecipeWebRequestSet = new HashSet<>();
    BundleRecipeWebRequest bundleRecipeWebRequest = new BundleRecipeWebRequest();
    bundleRecipeWebRequest.setItemSku(SOURCE_ITEM_CODE);
    bundleRecipeWebRequest.setQuantity(10);
    bundleRecipeWebRequestSet.add(bundleRecipeWebRequest);
    productCreationWebRequest.setProductItemRequests(Collections.singletonList(
        ProductItemCreationWebRequest.builder().bundleRecipe(bundleRecipeWebRequestSet)
            .productItemAttributeValueRequests(Collections.singletonList(ProductItemAttributeValueWebRequest.builder()
                .attribute(AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
                    .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).variantCreation(true).build()).value(VALUE)
                .build())).sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setImagesUpdated(true);
    productCreationWebRequest.setOnline(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(new ProductItemLogisticsWebRequest());
    productCreationWebRequest.setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    PreOrderWebRequest preOrderWebRequest =
        PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productCreationWebRequest.setPreOrder(preOrderWebRequest);
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setFbbActivated(true);
    productCreationWebRequest.getProductItemRequests().get(0)
        .setPickupPoints(Collections.singletonList(pickupPointCreateWebRequest));
    DistributionItemWebRequest distributionItemWebRequest = new DistributionItemWebRequest();
    distributionItemWebRequest.setExpiry(true);
    productCreationWebRequest.getProductItemRequests().get(0)
        .setDistributionItemInfoRequest(distributionItemWebRequest);
    DimensionAndUomWebRequest dimensionAndUomWebRequest = new DimensionAndUomWebRequest();
    dimensionAndUomWebRequest.setWeight(10.0);
    dimensionAndUomWebRequest.setUpcEanList(Collections.singleton(PREORDER_TYPE));
    productCreationWebRequest.getProductItemRequests().get(0).setDimensionsAndUOMRequest(
        Collections.singletonList(dimensionAndUomWebRequest));
    ProductCreationRequest request =
        ConverterUtil.toProductCreationRequest(productCreationWebRequest, mandatoryParameterHelper, true, true);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertTrue(productItemAttributeValueRequest.getAttribute().isVariantCreation());
    assertTrue(request.isForReview());
    assertEquals(PREORDER_TYPE,
        request.getProductItemRequests().get(0).getDimensionsAndUOMRequest().get(0).getUpcEanList().iterator().next());
    assertEquals(10.0,
        request.getProductItemRequests().get(0).getDimensionsAndUOMRequest().get(0).getWeight());
    assertTrue(request.getProductItemRequests().get(0).getDistributionItemInfoRequest().isExpiry());
    assertTrue(request.isImagesUpdated());
    assertTrue(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertTrue(request.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, request.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, request.getPreOrder().getPreOrderValue());
    assertTrue(request.isOnline());
    assertTrue(request.getProductItemRequests().get(0).getPickupPoints().get(0).isFbbActivated());
  }

  @Test
  public void toProductCreationRequestWithWholesalePriceTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    ProductItemWholesalePriceWebRequest productItemWholesalePriceWebRequest = new ProductItemWholesalePriceWebRequest();
    productItemWholesalePriceWebRequest.setQuantity(2);
    productCreationWebRequest.setProductItemRequests(Arrays.asList(ProductItemCreationWebRequest.builder()
        .productItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceWebRequest))
        .productItemAttributeValueRequests(Arrays.asList(ProductItemAttributeValueWebRequest.builder().attribute(
            AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
                .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).build()).value(VALUE).build())).build()));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    DistributionItemWebRequest distributionItemWebRequest = new DistributionItemWebRequest();
    distributionItemWebRequest.setExpiry(true);
    productCreationWebRequest.getProductItemRequests().get(0)
        .setDistributionItemInfoRequest(distributionItemWebRequest);
    ProductCreationRequest request = ConverterUtil.toProductCreationRequest(
        productCreationWebRequest, mandatoryParameterHelper, false, true);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertEquals(request.getProductItemRequests().get(0).getProductItemWholesalePriceRequests().size(), 1);
    assertTrue(request.isForReview());
  }
  
  @Test
  public void toProductBusinessPartnerServiceRequest() {
    ProductBusinessPartnerWebRequest productBusinessPartnerWebRequest =
        new ProductBusinessPartnerWebRequest();
    List<ProductItemBusinessPartnerWebRequest> productItemBusinessPartnerWebRequests =
        new ArrayList<>();
    productItemBusinessPartnerWebRequests.add(new ProductItemBusinessPartnerWebRequest());
    productBusinessPartnerWebRequest
        .setProductItemBusinessPartners(productItemBusinessPartnerWebRequests);
    ProductBusinessPartnerServiceRequest request = ConverterUtil
        .toProductBusinessPartnerServiceRequest(productBusinessPartnerWebRequest, mandatoryParameterHelper);
    assertEquals(request.getProductItemBusinessPartners().size(), 1);
    assertEquals(
        request.getProductItemBusinessPartners().get(0).getProductItemLogisticsWebRequests().size(),
        0);
  }

  @Test
  public void getFilterUSPTest() {
    String filterUSP = ConverterUtil.getFilterUSP(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES);
    Assertions.assertTrue(USP_WITH_SPECIAL_CHARACTERS_AND_SPACES.length() > filterUSP.length());
  }

  @Test
  void toProductCreationRequestNullAttributeFailCheckTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    productCreationWebRequest.setProductItemRequests(Arrays.asList(
        ProductItemCreationWebRequest.builder().productItemAttributeValueRequests(Arrays.asList(
                ProductItemAttributeValueWebRequest.builder().attribute(
                    AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE).attributeType(null)
                        .variantCreation(true).build()).value(VALUE).build()))
            .sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    Assertions.assertThrows(ValidationException.class,
        () -> ConverterUtil.toProductCreationRequest(productCreationWebRequest,
            mandatoryParameterHelper, false, false));
  }

  @Test
  public void toProductCreationRequest_withPreOrderQuotaTest() {
    ProductCreationWebRequest productCreationWebRequest = new ProductCreationWebRequest();
    productCreationWebRequest.setProductItemRequests(Arrays.asList(ProductItemCreationWebRequest.builder()
      .productItemAttributeValueRequests(Arrays.asList(ProductItemAttributeValueWebRequest
        .builder().attribute(AttributeWebRequest.builder().attributeCode(ATTRIBUTE_CODE)
          .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE).variantCreation(true).build())
        .value(VALUE).build())).sourceItemCode(SOURCE_ITEM_CODE).isContentChanged(true).build()));
    PickupPointCreateWebRequest pickupPointCreateWebRequest = new PickupPointCreateWebRequest();
    pickupPointCreateWebRequest.setPreOrderQuota(10);
    productCreationWebRequest.getProductItemRequests().get(0).setPickupPoints(Collections.singletonList(pickupPointCreateWebRequest));
    productCreationWebRequest.setForReview(true);
    productCreationWebRequest.setImagesUpdated(true);
    productCreationWebRequest.setOnline(true);
    productCreationWebRequest.setB2cActivated(true);
    productCreationWebRequest.setB2bActivated(false);
    List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests = new ArrayList<>();
    productItemLogisticsWebRequests.add(new ProductItemLogisticsWebRequest());
    productCreationWebRequest.setProductItemLogisticsWebRequests(productItemLogisticsWebRequests);
    PreOrderWebRequest preOrderWebRequest =
      PreOrderWebRequest.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();
    productCreationWebRequest.setPreOrder(preOrderWebRequest);
    ProductCreationRequest request = ConverterUtil.toProductCreationRequest(
      productCreationWebRequest, mandatoryParameterHelper, false, false);
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
      request.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0);
    assertEquals(productItemAttributeValueRequest.getValue(), VALUE);
    assertEquals(productItemAttributeValueRequest.getAttribute().getAttributeCode(), ATTRIBUTE_CODE);
    assertEquals(productItemAttributeValueRequest.getAttribute().isVariantCreation(), true);
    assertTrue(request.isForReview());
    assertTrue(request.isImagesUpdated());
    assertTrue(request.getProductItemRequests().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertTrue(request.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, request.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, request.getPreOrder().getPreOrderValue());
    assertTrue(request.isOnline());
  }
}
