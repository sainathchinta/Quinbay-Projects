package com.gdn.partners.pcu.internal.service.impl.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.DescriptiveAttributeValueTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.ItemNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.NeedRevisionNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductItemRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(SpringExtension.class)
class ConverterUtilTest {

  public static final String ACTIVE = "ACTIVE";
  private static final String ITEM_SKU = "MTA-0000001-0001";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "category_code";
  private static final String BRAND_CODE = "brandCode";
  private static final String VIDEO_URL = "video_url";
  private static final String IMAGE_LOCATION_PATH = "location-path";
  private static final String ATTRIBUTE_NAME = "Warna";
  private static final String NOTES = "notes";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private ProductWebRequest productWebRequestForVendorUpdate;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @BeforeEach
  public void setup() throws IOException {
    File file = new File(
      Thread.currentThread().getContextClassLoader().getResource("VendorUpdateTestRequest.json")
        .getFile());
    ObjectMapper objectMapper = new ObjectMapper();
    productWebRequestForVendorUpdate = objectMapper.readValue(file, ProductWebRequest.class);


  }


  @Test
  void convertProductWebRequestToDistributionProductRequestTest() {
    ItemNotesWebRequest itemNotesWebRequest = new ItemNotesWebRequest();
    itemNotesWebRequest.setSkuCode(ITEM_SKU);
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(new NeedRevisionNotesWebRequest());
    productWebRequestForVendorUpdate.getNeedRevisionNotes()
      .setItemNotes(Arrays.asList(itemNotesWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(
        productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS,
      distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
  }

  @Test
  void convertProductWebRequestToDistributionProductRequestDifferentKeyTest() {
    ItemNotesWebRequest itemNotesWebRequest = new ItemNotesWebRequest();
    itemNotesWebRequest.setSkuCode("MTA");
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(new NeedRevisionNotesWebRequest());
    productWebRequestForVendorUpdate.getNeedRevisionNotes()
      .setItemNotes(Arrays.asList(itemNotesWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(
        productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    assertProductImages(distributionProductDetailRequest);
    assertProductAttributes(distributionProductDetailRequest);
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS,
      distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
  }

  @Test
  void convertProductWebRequestToDistributionProductRequestOriginalImageTest() {
    productWebRequestForVendorUpdate.getImages().get(0).setOriginalImage(true);
    productWebRequestForVendorUpdate.getProductItems().get(0).getImages().get(0)
      .setOriginalImage(true);
    productWebRequestForVendorUpdate.setMarginExceed(true);
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    productWebRequestForVendorUpdate.setProductCategories(Arrays.asList(productCategoryWebRequest));
    DistributionProductDetailRequest distributionProductDetailRequest =
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(
        productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    assertTrue(distributionProductDetailRequest.getProductImages().get(0).getOriginalImage());
    assertTrue(
      distributionProductDetailRequest.getProductItems().get(0).getProductItemImages().get(0)
        .getOriginalImage());
    assertTrue(distributionProductDetailRequest.isMarginExceeded());
    assertEquals(CATEGORY_CODE, distributionProductDetailRequest.getCategoryCode());
  }

  @Test
  void convertProductWebRequestToDistributionProductRequestWithProductNotesTest() {
    productWebRequestForVendorUpdate.setNotes(NOTES);
    productWebRequestForVendorUpdate.setNeedRevisionNotes(
      NeedRevisionNotesWebRequest.builder().vendorNotes(Arrays.asList("notes")).allVariants(true)
        .contentAdditionalNotes("content notes").imagesAdditionalNotes("image notes")
        .vendorErrorFields(Arrays.asList("url")).build());
    DistributionProductDetailRequest distributionProductDetailRequest =
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(
        productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS,
      distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
    Assertions.assertTrue(distributionProductDetailRequest.getProductNotes().getAllVariants());
  }

  @Test
  void convertProductWebRequestToDistributionProductRequestWithProductItemNotesTest() {
    productWebRequestForVendorUpdate.setNotes(NOTES);
    ItemNotesWebRequest itemNotesWebRequest =
      ItemNotesWebRequest.builder().skuCode(ITEM_SKU).itemName(GENERATED_ITEM_NAME)
        .itemSku(ITEM_SKU).itemNumber(1).vendorNotes(Arrays.asList("image"))
        .vendorErrorFields(Arrays.asList("image")).build();
    productWebRequestForVendorUpdate.setNeedRevisionNotes(
      NeedRevisionNotesWebRequest.builder().vendorNotes(Arrays.asList("notes")).allVariants(true)
        .contentAdditionalNotes("content notes").imagesAdditionalNotes("image notes")
        .vendorErrorFields(Arrays.asList("url")).itemNotes(Arrays.asList(itemNotesWebRequest))
        .build());
    DistributionProductDetailRequest distributionProductDetailRequest =
      ConverterUtil.convertProductWebRequestToDistributionProductRequest(
        productWebRequestForVendorUpdate, clientParameterHelper);
    assertEquals(distributionProductDetailRequest.getProductName(), PRODUCT_NAME);
    assertEquals(distributionProductDetailRequest.getVideoUrl(), VIDEO_URL);
    //Test Product Images
    assertProductImages(distributionProductDetailRequest);
    //Test Product Attributes
    assertProductAttributes(distributionProductDetailRequest);
    //Test Product Item
    assertProductItems(distributionProductDetailRequest);
    assertEquals(NOTES, distributionProductDetailRequest.getNotes());
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    Assertions.assertEquals(BRAND_CODE, distributionProductDetailRequest.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS,
      distributionProductDetailRequest.getBrandApprovalStatus());
    assertTrue(distributionProductDetailRequest.isEdited());
    Assertions.assertEquals(1,
      distributionProductDetailRequest.getProductItems().get(0).getItemNotes().getVendorNotes()
        .size());
    Assertions.assertEquals(ITEM_SKU,
      distributionProductDetailRequest.getProductItems().get(0).getItemNotes().getSkuCode());
  }

  @Test
  void testToProductAttributeRequests_WithNullInput() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = null;

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithEmptyList() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = Collections.emptyList();

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithValidInput() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(false)
            .build())
        .productAttributeValues(List.of(ProductAttributeValueWebRequest.builder()
            .descriptiveAttributeValue("Test Value")
            .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
            .build()))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(storeId, result.get(0).getStoreId());
  }

  @Test
  void testToProductAttributeRequests_WithSkippableAttributes() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute with empty value that should be skipped
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(ProductAttributeValueWebRequest.builder()
            .descriptiveAttributeValue("-")
            .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
            .build()))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedAttribute() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined attribute
    ProductAttributeWebRequest predefinedAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)
            .dsExtraction(false)
            .build())
        .productAttributeValues(List.of(ProductAttributeValueWebRequest.builder()
            .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                .value("Test Predefined Value")
                .build())
            .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
            .build()))
        .build();
    
    productAttributeWebRequests.add(predefinedAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(storeId, result.get(0).getStoreId());
  }

  @Test
  void testToProductAttributeRequests_WithNonExtractionEnabled() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute with extraction disabled
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(false)
            .build())
        .productAttributeValues(List.of(ProductAttributeValueWebRequest.builder()
            .descriptiveAttributeValue("-")
            .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
            .build()))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithDescriptiveAllEmptyValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute with all empty values
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithDescriptiveMixedValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute with mixed values
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("Valid Value")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedAllEmptyValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined attribute with all empty values
    ProductAttributeWebRequest predefinedAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedMixedValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined attribute with mixed values
    ProductAttributeWebRequest predefinedAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("Valid Value")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithOtherAttributeType() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create an attribute with a different type
    ProductAttributeWebRequest otherAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(ProductAttributeValueWebRequest.builder()
            .descriptiveAttributeValue("-")
            .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.NONE)
            .build()))
        .build();
    
    productAttributeWebRequests.add(otherAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithDescriptiveMultivalueAllEmpty() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive multivalue attribute with all empty values
    ProductAttributeWebRequest descriptiveMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithDescriptiveMultivalueAllHyphen() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive multivalue attribute with all hyphen values
    ProductAttributeWebRequest descriptiveMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("-")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("-")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithDescriptiveMultivalueMixedValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive multivalue attribute with mixed values
    ProductAttributeWebRequest descriptiveMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue("Valid Value")
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedMultivalueAllEmpty() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined multivalue attribute with all empty values
    ProductAttributeWebRequest predefinedMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedMultivalueAllHyphen() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined multivalue attribute with all hyphen values
    ProductAttributeWebRequest predefinedMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("-")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("-")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithPredefinedMultivalueMixedValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined multivalue attribute with mixed values
    ProductAttributeWebRequest predefinedMultivalueAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_MULTIVALUE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build(),
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value("Valid Value")
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedMultivalueAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  void testToProductAttributeRequests_WithNullValues() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a descriptive attribute with null values
    ProductAttributeWebRequest descriptiveAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .descriptiveAttributeValue(null)
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.SINGLE)
                .build()
        ))
        .build();
    
    // Create a predefined attribute with null values
    ProductAttributeWebRequest predefinedAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(PredefinedAllowedAttributeValueWebRequest.builder()
                    .value(null)
                    .build())
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(descriptiveAttribute);
    productAttributeWebRequests.add(predefinedAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testToProductAttributeRequests_WithNullPredefinedAllowedAttributeValue() {
    // Given
    String storeId = "store123";
    List<ProductAttributeWebRequest> productAttributeWebRequests = new ArrayList<>();

    // Create a predefined attribute with null predefinedAllowedAttributeValue
    ProductAttributeWebRequest predefinedAttribute = ProductAttributeWebRequest.builder()
        .attribute(AttributeWebRequest.builder()
            .attributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)
            .dsExtraction(true)
            .build())
        .productAttributeValues(List.of(
            ProductAttributeValueWebRequest.builder()
                .predefinedAllowedAttributeValue(null)
                .descriptiveAttributeValueType(DescriptiveAttributeValueTypeWeb.PREDEFINED)
                .build()
        ))
        .build();
    
    productAttributeWebRequests.add(predefinedAttribute);

    // When
    List<ProductAttributeRequest> result = ConverterUtil.toProductAttributeRequests(storeId, productAttributeWebRequests);

    // Then
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void toProductRequestWebpEnabledTest() {
    productWebRequestForVendorUpdate.getImages().getFirst().setType(Constants.NEW_IMAGE_TYPE);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setType(Constants.NEW_IMAGE_TYPE);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setLocationPath("image.jpeg");
    productWebRequestForVendorUpdate.setProductCategories(new ArrayList<>());
    productWebRequestForVendorUpdate.setProductAttributes(new ArrayList<>());
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequestForVendorUpdate, clientParameterHelper, false,
        true);
    verify(clientParameterHelper, times(1)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals("image.webp",
      response.getProductItems().getFirst().getImages().getFirst().getLocationPath());
    assertEquals("location-path.webp", response.getImages().getFirst().getLocationPath());
  }

  @Test
  public void toProductRequestWebpEnabledNotNewTest() {
    productWebRequestForVendorUpdate.getImages().getFirst().setType(null);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setType(null);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setLocationPath("image.jpeg");
    productWebRequestForVendorUpdate.setProductCategories(new ArrayList<>());
    productWebRequestForVendorUpdate.setProductAttributes(new ArrayList<>());
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequestForVendorUpdate, clientParameterHelper, false,
        true);
    verify(clientParameterHelper, times(1)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    assertEquals(PRODUCT_NAME, response.getName());
    assertEquals("image.jpeg",
      response.getProductItems().getFirst().getImages().getFirst().getLocationPath());
    assertEquals("location-path", response.getImages().getFirst().getLocationPath());
  }

  @Test
  public void toProductRequestWebpDisbaledTest() {
    productWebRequestForVendorUpdate.getImages().getFirst().setType(Constants.NEW_IMAGE_TYPE);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setType(Constants.NEW_IMAGE_TYPE);
    productWebRequestForVendorUpdate.getProductItems().getFirst().getImages().getFirst()
      .setLocationPath("image.jpeg");
    productWebRequestForVendorUpdate.setProductCategories(new ArrayList<>());
    productWebRequestForVendorUpdate.setProductAttributes(new ArrayList<>());
    ProductRequest response =
      ConverterUtil.toProductRequest(productWebRequestForVendorUpdate, clientParameterHelper, false,
        false);
    verify(clientParameterHelper, times(1)).getUsername();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    assertEquals(PRODUCT_NAME, response.getName());
  }



  private void assertProductImages(
    DistributionProductDetailRequest distributionProductDetailRequest) {
    assertTrue(distributionProductDetailRequest.getProductImages().size() == 1);
    final DistributionProductImageRequest imageRequest =
      distributionProductDetailRequest.getProductImages().get(0);
    assertEquals(imageRequest.getLocationPath(), IMAGE_LOCATION_PATH);
    assertTrue(imageRequest.getSequence() == 0);
    assertTrue(imageRequest.isMainImage());
    assertFalse(imageRequest.isEdited());
  }

  private void assertProductItems(
    DistributionProductDetailRequest distributionProductDetailRequest) {
    assertNotNull(distributionProductDetailRequest.getProductItems());
    assertTrue(distributionProductDetailRequest.getProductItems().size() == 1);
    final DistributionProductItemRequest distributionProductItemRequest =
      distributionProductDetailRequest.getProductItems().get(0);
    assertEquals(GENERATED_ITEM_NAME, distributionProductItemRequest.getGeneratedItemName());
    assertEquals(ITEM_SKU, distributionProductItemRequest.getSkuCode());
    assertTrue(0 == distributionProductItemRequest.getDangerousGoodsLevel());
    final DistributionProductAttributeRequest productItemAttribute =
      distributionProductItemRequest.getProductItemAttributes().get(0);
    assertEquals(ATTRIBUTE_CODE, productItemAttribute.getAttributeCode());
    assertEquals(ATTR_VALUE, productItemAttribute.getValue());
    final DistributionProductImageRequest imageRequest =
      distributionProductItemRequest.getProductItemImages().get(0);
    assertEquals(IMAGE_LOCATION_PATH, imageRequest.getLocationPath());
    assertTrue(imageRequest.isEdited());
  }

  private void assertProductAttributes(
    DistributionProductDetailRequest distributionProductDetailRequest) {
    assertTrue(distributionProductDetailRequest.getProductAttributes().size() == 23);
    final DistributionProductAttributeRequest distributionProductAttributeRequest =
      distributionProductDetailRequest.getProductAttributes().get(0);
    System.out.println(distributionProductAttributeRequest);
    assertEquals(ATTRIBUTE_CODE, distributionProductAttributeRequest.getAttributeCode());
    assertEquals(ATTRIBUTE_NAME, distributionProductAttributeRequest.getName());
    assertEquals(ATTR_VALUE, distributionProductAttributeRequest.getValue());
  }
}
