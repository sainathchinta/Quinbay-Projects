package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.dto.ProductHistoryDTO;
import com.gdn.x.mta.distributiontask.model.type.DifficultyLevel;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.PageInfo;
import com.google.api.services.youtube.model.VideoListResponse;

public class ProductUtilsTest {

  private static final String STORE_ID = "storeId";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String PRODUCT_NAME = "productName";

  private static final String PRODUCT_CODE = "MTA-1000001";

  private static final String PRODUCT_CODE_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}$";
  private static final String PRODUCT_DESCRIPTION = "desc";
  private static final String URL = "url";
  private static final String USP = "usp";
  private static final String ATTRIBUTE_CODE = "attr";
  private static final String ATTRIBUTE_CODE_1 = "attr1";
  private static final String ATTRIBUTE_CODE_2 = "attr2";
  private static final String ATTRIBUTE_CODE_3 = "attr3";
  private static final String ATTRIBUTE_VALUE = "val";
  private static final String ATTRIBUTE_NAME = "attrname";
  private static final String ATTRIBUTE_VALUE2 = "val2";
  private static final String ATTRIBUTE_VALUE3 = "val3";
  private static final String LOCATION = "loc";
  private static final String PRODUCT_NAME1 = "productName1";
  private static final String DESCRIPTION1 = "desc1";
  private static final String URL1 = "url1";
  private static final String USP1 = "usp1";
  private static final String SKU_CODE = "skuCode";
  private static final String ITEM_NAME = "itemName";
  private static final String LOCATION1 = "loc1";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_CODE1 = "category_code1";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String CATEGORY_NAME1 = "CATEGORY_NAME1";
  private static final String BLUR = "blur";
  private static final String ATTRIBUTE_NAME2 = "attributeName2";
  private static final String ATTRIBUTE_NAME3 = "attributeName3";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "brandApprovalStatus";
  private static final String YOUTUBE_URL = "https://www.youtube.com/watch?v=P1xAhgKTqDA";
  private static final String YOUTUBE_INVALID_URL = "https://www.youtube.com/watch?v";
  private static final String API_KEY = "apiKey";
  private static final String VIDEO_LIST = "snippet,contentDetails,statistics";
  private static final String PRODUCT_NOTES = "productNotes";
  private static final String ITEM_NOTES = "itemNotes";
  private static final Double LENGTH = 1.0;
  private static final Double WIDTH = 1.0;
  private static final Double HEIGHT = 1.0;
  private static final Double WEIGHT = 1.0;
  private static final Double SHIPPING_WEIGHT = 1.0;
  private static final Integer DG_LEVEL  = 1;
  private static final Integer PRODUCT_TYPE  = 1;
  private static final String LOCATION_PATH_1 = "LocationPath1";
  private static final String LOCATION_PATH_2 = "LocationPath2";

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private YouTube youTube;

  @Mock
  private YouTube.Videos videos;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private YouTube.Videos.List list;
  private VideoListResponse videoListResponse;

  @InjectMocks
  private ProductUtils instance;

  private Product newProduct;

  private Product oldProduct;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    String hash = "1234";
    oldProduct = new Product();
    oldProduct.setProductImages(new ArrayList<ProductImage>());
    oldProduct.setProductAttributes(new ArrayList<ProductAttribute>());
    oldProduct.setProductItems(new ArrayList<ProductItem>());
    oldProduct.getProductImages().add(new ProductImage(oldProduct, "location", 0, true));
    oldProduct.getProductItems().add(
        new ProductItem(oldProduct, "upcCode", "skuCode", "generatedItemName", hash.getBytes(),
            "storeId"));
    oldProduct.getProductAttributes()
        .add(new ProductAttribute(oldProduct, "attributeCode", "name", "value", "attributeType"));
    oldProduct.setImageViolations(BLUR);
    oldProduct.setProductPredictionScore(11);

    newProduct = new Product();
    newProduct.setProductImages(new ArrayList<ProductImage>());
    newProduct.setProductAttributes(new ArrayList<ProductAttribute>());
    newProduct.setProductItems(new ArrayList<ProductItem>());
    newProduct.getProductImages().add(new ProductImage(newProduct, "location", 0, true));
    newProduct.getProductItems().add(
        new ProductItem(newProduct, "upcCode", "skuCode", "generatedItemName", hash.getBytes(),
            "storeId"));
    newProduct.getProductAttributes()
        .add(new ProductAttribute(oldProduct, "attributeCode", "name", "value", "attributeType"));

    ReflectionTestUtils.setField(instance, "youTubeDataApiKey", API_KEY);
    videoListResponse = new VideoListResponse();
    PageInfo pageInfo = new PageInfo();
    pageInfo.setTotalResults(2);
    videoListResponse.setPageInfo(pageInfo);
  }

  private Product createProduct() {
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    ProductItemImage productItemMainImage = new ProductItemImage();
    productItemMainImage.setMainImage(true);
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImage(false);
    productItemImage.setMarkForDelete(true);
    productItemImageList.add(productItemImage);
    productItemImageList.add(productItemMainImage);
    List<ProductItemAttribute> productItemAttributeList = new ArrayList<>();
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setAttributeCode("productItemAttributeCode");
    productItemAttribute.setCreatedDate(new Date());
    ProductItemAttribute deleteproductItemAttribute = new ProductItemAttribute();
    deleteproductItemAttribute.setAttributeCode("deleteproductItemAttributeCode");
    deleteproductItemAttribute.setMarkForDelete(true);
    productItemAttributeList.add(productItemAttribute);
    productItemAttributeList.add(deleteproductItemAttribute);
    List<ProductItem> productItemList = new ArrayList<>();
    ProductItem productItem = new ProductItem();
    productItem.setId("id");
    productItem.setSkuCode("skuCode");
    productItem.setProductItemAttributes(productItemAttributeList);
    productItem.setProductItemImages(productItemImageList);
    productItem.setCreatedDate(new Date());
    ProductItem deleteproductItem = new ProductItem();
    deleteproductItem.setId("id");
    deleteproductItem.setSkuCode(SKU_CODE_1);
    deleteproductItem.setMarkForDelete(true);
    productItemList.add(productItem);
    productItemList.add(deleteproductItem);
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeCode("attribute_code1");
    productAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    ProductAttribute predifineProductAttribute = new ProductAttribute();
    predifineProductAttribute.setAttributeCode("attribute_code2");
    predifineProductAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    ProductAttribute descriptiveProductAttribute = new ProductAttribute();
    descriptiveProductAttribute.setAttributeCode("attribute_code3");
    descriptiveProductAttribute.setMarkForDelete(true);
    descriptiveProductAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttributeList.add(productAttribute);
    productAttributeList.add(predifineProductAttribute);
    productAttributeList.add(descriptiveProductAttribute);
    List<ProductImage> productImageList = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImage.setMainImage(true);
    ProductImage deleteproductImage = new ProductImage();
    deleteproductImage.setMarkForDelete(true);
    productImageList.add(productImage);
    productImageList.add(deleteproductImage);
    Product product = new Product.Builder().productCode("productCode").productItems(productItemList)
        .productAttributes(productAttributeList).productImages(productImageList).storeId(STORE_ID)
        .restrictedKeywordsPresent(Boolean.TRUE).build();
    return product;
  }

  @Test
   void testRegenerateProductReplacementDetails() {
    Product product = this.instance.regenerateProductReplacementDetails(oldProduct, newProduct);
    Assertions.assertNotNull(product);
  }

  @Test
   void regenerateProductImageDetailsTest() {
    Product product = createProduct();
    product.getProductImages().get(0).setEdited(true);
    this.instance.regenerateProductImageDetails(product);
  }

  @Test
   void replaceProductImageDetailsTestOk() throws IOException {
    String message = "{\n" + "    \"vendorNotes\": [\"Incomplete or inappropriate content\"],\n"
        + "    \"vendorErrorFields\": [\"url\", \"description\"],\n" + "    \"contentAdditionalNotes\": \"notes\",\n"
        + "    \"allVariants\": true,\n" + "    \"imagesAdditionalNotes\": \"notes\"}";
    Product oldProduct = createProduct();
    oldProduct.setProductNotes(message);
    Product newProduct = createProduct();
    newProduct.getProductImages().get(0).setActive(true);
    newProduct.getProductImages().get(0).setEdited(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
    newProduct.getProductItems().get(1).setId("id1");
    newProduct.getProductItems().get(0).setItemNotes("{\n" + "\"vendorNotes\" : [\"Incomplete or inappropriate content\"],\n"
        + "\"contentAdditionalNotes\" : \"notes\",\n" + "\"vendorErrorFields\" : [\"Video Url\", \"description\"],\n"
        + "\"allVaraints\" : true,\n" + "\"imagesAdditionalNotes\" : \"notes\",\n"
        + "\"imageReason\" : [\"Blur or inappropriate images\"]\n" + "}");
    newProduct.setProductNotes(message);
    Mockito.when(objectMapper.readValue(message, ProductNotesRequest.class)).thenReturn(ProductNotesRequest.builder()
        .build());
    this.instance.replaceProductImageDetails(oldProduct, newProduct);
    Mockito.verify(objectMapper, Mockito.times(2)).readValue(message, ProductNotesRequest.class);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(ProductNotesRequest.class));
  }

  @Test
   void replaceProductImageAndProductItemImagesTestOk() {
    Product oldProduct = createProduct();
    Product newProduct = createProduct();
    newProduct.getProductImages().get(0).setActive(true);
    newProduct.getProductImages().get(0).setEdited(true);
    newProduct.getProductItems().get(0).getProductItemImages().get(0).setEdited(true);
    this.instance.replaceProductImageAndProductItemImages(oldProduct, newProduct);
  }

  @Test
   void replaceProductDetailsTestOk() {
    Product oldProduct = createProduct();
    Product newProduct = createProduct();
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(BLUR);
    oldProduct.setEdited(true);
    newProduct.setBrandCode(BRAND_CODE);
    newProduct.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    newProduct.setEdited(true);
    oldProduct.setPostLive(true);
    newProduct.setPostLive(false);
    oldProduct.setProductNotes(PRODUCT_NOTES);
    oldProduct.getProductItems().get(0).setItemNotes(ITEM_NOTES);
    Product product = this.instance.replaceProductDetails(oldProduct, newProduct, true);
    Assertions.assertEquals(BLUR, product.getImageViolations());
    Assertions.assertEquals(11, product.getProductPredictionScore());
    Assertions.assertEquals(BRAND_CODE, product.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, product.getBrandApprovalStatus());
    Assertions.assertTrue(product.isEdited());
    Assertions.assertFalse(product.isPostLive());
    Assertions.assertEquals(PRODUCT_NOTES, product.getProductNotes());
    Assertions.assertEquals(ITEM_NOTES, product.getProductItems().get(0).getItemNotes());
  }

  @Test
   void replaceProductDetailsTestOk1() {
    Product oldProduct = createProduct();
    Product newProduct = createProduct();
    oldProduct.setProductPredictionScore(11);
    oldProduct.setImageViolations(BLUR);
    newProduct.setBrandCode(BRAND_CODE);
    newProduct.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    newProduct.setEdited(true);
    oldProduct.setPostLive(false);
    newProduct.setPostLive(true);
    Product product = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(BLUR, product.getImageViolations());
    Assertions.assertEquals(11, product.getProductPredictionScore());
    Assertions.assertEquals(BRAND_CODE, product.getBrandCode());
    Assertions.assertEquals(BRAND_APPROVAL_STATUS, product.getBrandApprovalStatus());
    Assertions.assertFalse(product.isEdited());
    Assertions.assertTrue(product.isPostLive());
  }

  @Test
   void replaceProductDetailsDifficultyLevelTestOk() {
    Product oldProduct = createProduct();
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    Product product = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(DifficultyLevel.NA, product.getImageDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.HIGH, product.getContentDifficultyLevel());
    Assertions.assertTrue(product.isRestrictedKeywordsPresent());
  }

  @Test
   void replaceProductDetailsAddDeleteSwitch() {
    ReflectionTestUtils.setField(instance, "addDeleteVariantSwitch", true);
    Product oldProduct = createProduct();
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    newProduct.getProductItems().get(0).setSkuCode(ITEM_NAME);
    Product product = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(DifficultyLevel.NA, product.getImageDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.HIGH, product.getContentDifficultyLevel());
    Assertions.assertTrue(product.isRestrictedKeywordsPresent());
  }

  @Test
   void replaceProductDetailsAddDeleteSwitchOff() {
    ReflectionTestUtils.setField(instance, "addDeleteVariantSwitch", false);
    Product oldProduct = createProduct();
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    newProduct.getProductItems().get(0).setSkuCode(ITEM_NAME);
    Product product = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(DifficultyLevel.NA, product.getImageDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.HIGH, product.getContentDifficultyLevel());
    Assertions.assertTrue(product.isRestrictedKeywordsPresent());
  }

  @Test
   void replaceProductImageDetailsDifficultyLevelTestOk() throws IOException {
    String message = "{\n" + "    \"vendorNotes\": [\"Incomplete or inappropriate content\"],\n"
        + "    \"vendorErrorFields\": [\"url\", \"description\"],\n" + "    \"contentAdditionalNotes\": \"notes\",\n"
        + "    \"allVariants\": true,\n" + "    \"imagesAdditionalNotes\": \"notes\"}";
    Product oldProduct = createProduct();
    oldProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setProductNotes(message);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.TRUE);
    newProduct.setProductNotes(message);
    Mockito.when(objectMapper.readValue(message, ProductNotesRequest.class)).thenReturn(ProductNotesRequest.builder()
        .build());
    Product product = this.instance.replaceProductImageDetails(oldProduct, newProduct);
    Assertions.assertEquals(DifficultyLevel.NA, product.getContentDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.NO_EFFORT, product.getImageDifficultyLevel());
    Assertions.assertFalse(product.isRestrictedKeywordsPresent());
    Mockito.verify(objectMapper, Mockito.times(2)).readValue(message, ProductNotesRequest.class);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(ProductNotesRequest.class));
  }

  @Test
   void replaceProductImageDetailsExistingNotesNullOk() throws IOException {
    String message = "{\n" + "    \"vendorNotes\": [\"Incomplete or inappropriate content\"],\n"
        + "    \"vendorErrorFields\": [\"url\", \"description\"],\n" + "    \"contentAdditionalNotes\": \"notes\",\n"
        + "    \"allVariants\": true,\n" + "    \"imagesAdditionalNotes\": \"notes\"}";
    Product oldProduct = createProduct();
    oldProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setProductNotes(null);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.TRUE);
    newProduct.setProductNotes(message);
    Product product = this.instance.replaceProductImageDetails(oldProduct, newProduct);
    Assertions.assertEquals(DifficultyLevel.NA, product.getContentDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.NO_EFFORT, product.getImageDifficultyLevel());
    Assertions.assertFalse(product.isRestrictedKeywordsPresent());
  }

  @Test
   void replaceProductImageDetailsNewProductNotesNullOk() throws IOException {
    String message = "{\n" + "    \"vendorNotes\": [\"Incomplete or inappropriate content\"],\n"
        + "    \"vendorErrorFields\": [\"url\", \"description\"],\n" + "    \"contentAdditionalNotes\": \"notes\",\n"
        + "    \"allVariants\": true,\n" + "    \"imagesAdditionalNotes\": \"notes\"}";
    Product oldProduct = createProduct();
    oldProduct.setRestrictedKeywordsPresent(Boolean.FALSE);
    oldProduct.setImageDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setContentDifficultyLevel(DifficultyLevel.NA);
    oldProduct.setProductNotes(message);
    Product newProduct = createProduct();
    newProduct.setImageDifficultyLevel(DifficultyLevel.NO_EFFORT);
    newProduct.setContentDifficultyLevel(DifficultyLevel.HIGH);
    newProduct.setRestrictedKeywordsPresent(Boolean.TRUE);
    newProduct.setProductNotes(null);
    newProduct.getProductItems().get(0).setItemNotes("{}");
    Product product = this.instance.replaceProductImageDetails(oldProduct, newProduct);
    Assertions.assertEquals(DifficultyLevel.NA, product.getContentDifficultyLevel());
    Assertions.assertEquals(DifficultyLevel.NO_EFFORT, product.getImageDifficultyLevel());
    Assertions.assertFalse(product.isRestrictedKeywordsPresent());
  }


  @Test
   void replaceProductDetailsTestOk_WithDifferentCreatedDate() {
    Product oldProduct = createProduct();
    Product newProduct = createProduct();
    newProduct.setStoreId(null);
    ProductItem productItem = newProduct.getProductItems().get(0);
    productItem.setCreatedDate(null);
    productItem.getProductItemAttributes().get(0).setCreatedDate(null);
    oldProduct.setProductNotes(PRODUCT_NOTES);
    oldProduct.getProductItems().get(0).setItemNotes(ITEM_NOTES);
    Product result = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(oldProduct.getProductName(), newProduct.getProductName());
    Assertions.assertNotNull(oldProduct.getStoreId());
    Assertions.assertNotNull(oldProduct.getProductItems().get(0).getCreatedDate());
    Assertions.assertEquals(newProduct.getProductItems().get(0).getProductItemAttributes().size(),
        result.getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertNull(result.getProductNotes());
    Assertions.assertNull(result.getProductItems().get(0).getItemNotes());
  }

  @Test
   void replaceProductDetailsTestWithAttributeModification() {
    Product oldProduct = createProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setValue(ATTRIBUTE_VALUE2);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    oldProduct.getProductAttributes().add(productAttribute);
    Product newProduct = createProduct();
    newProduct.setStoreId(null);
    ProductItem productItem = newProduct.getProductItems().get(0);
    productItem.setCreatedDate(null);
    productItem.getProductItemAttributes().get(0).setCreatedDate(null);
    Product result = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(oldProduct.getProductName(), newProduct.getProductName());
    Assertions.assertNotNull(oldProduct.getStoreId());
    Assertions.assertNotNull(oldProduct.getProductItems().get(0).getCreatedDate());
    Assertions.assertEquals(newProduct.getProductItems().get(0).getProductItemAttributes().size(),
        result.getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(2, result.getProductAttributes().stream().filter(productAttribute1 -> productAttribute1.isMarkForDelete()).count());
  }

  @Test
   void replaceProductDetailsTestWithAdditionAttributeModification() {
    Product oldProduct = createProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setValue(ATTRIBUTE_VALUE2);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    Product newProduct = createProduct();
    newProduct.getProductAttributes().add(productAttribute);
    newProduct.setStoreId(null);
    ProductItem productItem = newProduct.getProductItems().get(0);
    productItem.setCreatedDate(null);
    productItem.getProductItemAttributes().get(0).setCreatedDate(null);
    Product result = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(oldProduct.getProductName(), newProduct.getProductName());
    Assertions.assertNotNull(oldProduct.getStoreId());
    Assertions.assertNotNull(oldProduct.getProductItems().get(0).getCreatedDate());
    Assertions.assertEquals(newProduct.getProductItems().get(0).getProductItemAttributes().size(),
        result.getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(1, result.getProductAttributes().stream().filter(productAttribute1 -> productAttribute1.isMarkForDelete()).count());
  }

  @Test
   void replaceProductDetailsTestWithItemAttributeModification() {
    Product oldProduct = createProduct();
    ProductItemAttribute productAttribute = new ProductItemAttribute();
    productAttribute.setValue(ATTRIBUTE_VALUE2);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    oldProduct.getProductItems().get(0).getProductItemAttributes().add(productAttribute);
    Product newProduct = createProduct();
    newProduct.setStoreId(null);
    ProductItem productItem = newProduct.getProductItems().get(0);
    productItem.setCreatedDate(null);
    productItem.getProductItemAttributes().get(0).setCreatedDate(null);
    Product result = this.instance.replaceProductDetails(oldProduct, newProduct, false);
    Assertions.assertEquals(oldProduct.getProductName(), newProduct.getProductName());
    Assertions.assertNotNull(oldProduct.getStoreId());
    Assertions.assertNotNull(oldProduct.getProductItems().get(0).getCreatedDate());
    Assertions.assertEquals(3, result.getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(1, result.getProductItems().get(0).getProductItemAttributes().stream()
        .filter(productItemAttribute -> !productItemAttribute.isMarkForDelete()).count());
    Assertions.assertEquals(1, result.getProductAttributes().stream().filter(productAttribute1 -> productAttribute1.isMarkForDelete()).count());
  }

  @Test
   void replaceProductDetailsTestWithAddedItemAttributeModification() {
    Product oldProduct = createProduct();
    ProductItemAttribute productAttribute = new ProductItemAttribute();
    productAttribute.setValue(ATTRIBUTE_VALUE2);
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    Product newProduct = createProduct();
    newProduct.setStoreId(null);
    ProductItem productItem = newProduct.getProductItems().get(0);
    productItem.setCreatedDate(null);
    productItem.getProductItemAttributes().get(0).setCreatedDate(null);
    newProduct.getProductItems().get(0).getProductItemAttributes().add(productAttribute);
    Product result = this.instance.replaceProductDetails(oldProduct, newProduct, true);
    Assertions.assertEquals(oldProduct.getProductName(), newProduct.getProductName());
    Assertions.assertNotNull(oldProduct.getStoreId());
    Assertions.assertNotNull(oldProduct.getProductItems().get(0).getCreatedDate());
    Assertions.assertEquals(3, result.getProductItems().get(0).getProductItemAttributes().size());
    Assertions.assertEquals(2, result.getProductItems().get(0).getProductItemAttributes().stream()
        .filter(productItemAttribute -> !productItemAttribute.isMarkForDelete()).count());
    Assertions.assertEquals(1, result.getProductAttributes().stream().filter(productAttribute1 -> productAttribute1.isMarkForDelete()).count());
  }

  @Test
   void initializeAllProductDetailsTestOk() {
    Product product = createProduct();
    this.instance.initializeAllProductDetails(product);
    Assertions.assertNotNull(product);
  }

  @Test
   void initializeAllProductDetailsWithMFDTrueTestOk() {
    Product product = createProduct();
    this.instance.initializeProductDetailsWithMFDTrue(product);
    Assertions.assertNotNull(product);
  }

  @Test
   void getProductDetailChangesTest() {
    Product product = getProduct();

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute3.setValue(ATTRIBUTE_VALUE);
    productAttribute3.setName(ATTRIBUTE_NAME3);
    productAttribute3.setVariantCreation(false);
    productAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product.getProductAttributes().add(productAttribute3);
    product.setLength(10.0);
    product.setWeight(10.0);
    product.setHeight(10.0);
    product.setWidth(10.0);
    product.setProductType(1);

    Product product1 = getProduct1();
    product1.setLength(11.0);
    product1.setWeight(11.0);
    product1.setHeight(11.0);
    product1.setWidth(11.0);
    product1.setProductType(2);

    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute4.setValue(ATTRIBUTE_VALUE3);
    productAttribute4.setName(ATTRIBUTE_NAME3);
    productAttribute4.setVariantCreation(false);
    productAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product1.getProductAttributes().add(productAttribute4);
    product1.setEdited(true);

    product.setBrand(BRAND_CODE);
    product1.setBrand(BRAND_APPROVAL_STATUS);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(13, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME3);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE3);
    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(7).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(7).getSkuName());
    Assertions.assertEquals(0, productDetailChanges.stream()
        .filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2)).count());
  }

  @Test
   void getProductDetailChangesProductTypeNullTest() {
    Product product = getProduct();

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute3.setValue(ATTRIBUTE_VALUE);
    productAttribute3.setName(ATTRIBUTE_NAME3);
    productAttribute3.setVariantCreation(false);
    productAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product.getProductAttributes().add(productAttribute3);
    product.setLength(10.0);
    product.setWeight(10.0);
    product.setHeight(10.0);
    product.setWidth(10.0);
    product.setProductType(null);

    Product product1 = getProduct1();
    product1.setLength(11.0);
    product1.setWeight(11.0);
    product1.setHeight(11.0);
    product1.setWidth(11.0);
    product1.setProductType(null);

    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute4.setValue(ATTRIBUTE_VALUE3);
    productAttribute4.setName(ATTRIBUTE_NAME3);
    productAttribute4.setVariantCreation(false);
    productAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product1.getProductAttributes().add(productAttribute4);
    product1.setEdited(true);
    product.setBrand(BRAND_CODE);
    product1.setBrand(BRAND_CODE);

    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(12, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME3);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE3);
    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(7).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(7).getSkuName());
    Assertions.assertEquals(0, productDetailChanges.stream()
        .filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2)).count());
  }

  @Test
   void getProductDetailChangesProductTypeNull1Test() {
    Product product = getProduct();

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute3.setValue(ATTRIBUTE_VALUE);
    productAttribute3.setName(ATTRIBUTE_NAME3);
    productAttribute3.setVariantCreation(false);
    productAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product.getProductAttributes().add(productAttribute3);
    product.setLength(10.0);
    product.setWeight(10.0);
    product.setHeight(10.0);
    product.setWidth(10.0);
    product.setProductType(null);

    Product product1 = getProduct1();
    product1.setLength(11.0);
    product1.setWeight(11.0);
    product1.setHeight(11.0);
    product1.setWidth(11.0);
    product1.setProductType(2);

    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute4.setValue(ATTRIBUTE_VALUE3);
    productAttribute4.setName(ATTRIBUTE_NAME3);
    productAttribute4.setVariantCreation(false);
    productAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product1.getProductAttributes().add(productAttribute4);
    product1.setEdited(true);


    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(12, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME3);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE3);
    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(7).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(7).getSkuName());
    Assertions.assertEquals(0, productDetailChanges.stream()
        .filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2)).count());
  }

  @Test
   void getProductDetailChangesProductTypeNull2Test() {
    Product product = getProduct();

    ProductAttribute productAttribute3 = new ProductAttribute();
    productAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute3.setValue(ATTRIBUTE_VALUE);
    productAttribute3.setName(ATTRIBUTE_NAME3);
    productAttribute3.setVariantCreation(false);
    productAttribute3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product.getProductAttributes().add(productAttribute3);
    product.setLength(10.0);
    product.setWeight(10.0);
    product.setHeight(10.0);
    product.setWidth(10.0);
    product.setProductType(1);

    Product product1 = getProduct1();
    product1.setLength(11.0);
    product1.setWeight(11.0);
    product1.setHeight(11.0);
    product1.setWidth(11.0);
    product1.setProductType(null);

    ProductAttribute productAttribute4 = new ProductAttribute();
    productAttribute4.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute4.setValue(ATTRIBUTE_VALUE3);
    productAttribute4.setName(ATTRIBUTE_NAME3);
    productAttribute4.setVariantCreation(false);
    productAttribute4.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    product1.getProductAttributes().add(productAttribute4);
    product1.setEdited(true);


    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(12, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME3);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE3);
    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(7).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(7).getSkuName());
    Assertions.assertEquals(0, productDetailChanges.stream()
        .filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2)).count());
  }

  @Test
   void getProductDetailChangesTest_ItemAttributeBrand() {
    Product product = getProduct();
    product.getProductItems().get(0).getProductItemAttributes().get(0).setName(Constants.BRAND);
    Product product1 = getProduct1();
    final List<ProductHistoryDTO> productDetailChanges = instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(6, productDetailChanges.size());
  }

  @Test
   void getProductDetailChangesUSPTest() {
    Product oldProduct = getProduct();
    Product newProduct = getProduct1();
    oldProduct.setUniqueSellingPoint(null);
    newProduct.setUniqueSellingPoint(null);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(oldProduct, newProduct);
    Assertions.assertEquals(6, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(5).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(5).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(5).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(5).getSkuName());
    Assertions.assertEquals(0,
        productDetailChanges.stream().filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2))
            .count());
  }

  @Test
   void getProductDetailChangesNewUSPTest() {
    Product oldProduct = getProduct();
    Product newProduct = getProduct1();
    oldProduct.setUniqueSellingPoint(null);
    newProduct.setUniqueSellingPoint(USP);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(oldProduct, newProduct);
    Assertions.assertEquals(7, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_URL);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), URL);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), URL1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());
    Assertions.assertTrue(StringUtils.isEmpty(productDetailChanges.get(3).getOldValue()));
    Assertions.assertEquals(USP, productDetailChanges.get(3).getNewValue());
    Assertions.assertEquals(Constants.PRODUCT_USP, productDetailChanges.get(3).getField());
    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(6).getSkuName());
    Assertions.assertEquals(0,
        productDetailChanges.stream().filter(p -> p.getField().equals(Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME2))
            .count());

  }

  @Test
   void getProductDetailChangesTest_WithNullVideoUrl() {
    Product product = getProduct();
    product.setVideoUrl(null);
    Product product1 = getProduct1();
    product1.setVideoUrl(null);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(6, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), DESCRIPTION1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(5).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(5).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(5).getNewValue(), ATTRIBUTE_VALUE2);
    Assertions.assertNotNull(productDetailChanges.get(5).getSkuName());
  }

  @Test
   void getProductDetailChangesTestWithAddedAndDeletedAttributesTest() {
    Product product = getProduct();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttribute.setValue(ATTRIBUTE_VALUE);
    productAttribute.setName(ATTRIBUTE_NAME);
    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setAttributeCode(ATTRIBUTE_CODE_3);
    productAttribute1.setValue(ATTRIBUTE_VALUE2);
    productAttribute1.setName(ATTRIBUTE_NAME);
    productAttribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    Product product1 = getProduct1();
    product.getProductAttributes().add(productAttribute);
    product1.getProductAttributes().add(productAttribute1);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(productDetailChanges.size(), 9);
    Assertions.assertEquals(productDetailChanges.get(1).getField(), Constants.PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getOldValue(), PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getNewValue(), DESCRIPTION1);
    Assertions.assertNull(productDetailChanges.get(1).getSkuName());


    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ADDED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertNull(productDetailChanges.get(6).getOldValue());
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE2);

    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_DELETED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertNull(productDetailChanges.get(7).getNewValue());

  }

  @Test
   void getProductDetailChangesTestWithAddedAndDeletedItemAttributesTest() {
    Product product = getProduct();
    ProductItemAttribute productAttribute = new ProductItemAttribute();
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttribute.setValue(ATTRIBUTE_VALUE);
    productAttribute.setName(ATTRIBUTE_NAME);
    ProductItemAttribute productAttribute1 = new ProductItemAttribute();
    productAttribute1.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute1.setValue(ATTRIBUTE_VALUE2);
    productAttribute1.setName(ATTRIBUTE_NAME);
    productAttribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    Product product1 = getProduct1();
    product.getProductItems().get(0).getProductItemAttributes().add(productAttribute);
    product1.getProductItems().get(0).getProductItemAttributes().add(productAttribute1);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(productDetailChanges.size(), 9);
    Assertions.assertEquals(productDetailChanges.get(1).getField(), Constants.PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getOldValue(), PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getNewValue(), DESCRIPTION1);
    Assertions.assertNull(productDetailChanges.get(1).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE2);

    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_DELETED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(7).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertNull(productDetailChanges.get(7).getNewValue());

    Assertions.assertEquals(productDetailChanges.get(8).getField(),
        Constants.HISTORY_ADDED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertNull(productDetailChanges.get(8).getOldValue());
    Assertions.assertEquals(productDetailChanges.get(8).getNewValue(), ATTRIBUTE_VALUE2);

  }

  @Test
   void getProductDetailChangesTestWithNullValue() {
    Product product = getProduct();
    ProductItemAttribute productAttribute = new ProductItemAttribute();
    productAttribute.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    productAttribute.setValue(null);
    productAttribute.setName(ATTRIBUTE_NAME);
    ProductItemAttribute productAttribute1 = new ProductItemAttribute();
    productAttribute1.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttribute1.setValue(null);
    productAttribute1.setName(ATTRIBUTE_NAME);
    productAttribute1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
    Product product1 = getProduct1();
    product.getProductItems().get(0).getProductItemAttributes().add(productAttribute);
    product1.getProductItems().get(0).getProductItemAttributes().add(productAttribute1);
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(productDetailChanges.size(), 9);
    Assertions.assertEquals(productDetailChanges.get(1).getField(), Constants.PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getOldValue(), PRODUCT_DESCRIPTION);
    Assertions.assertEquals(productDetailChanges.get(1).getNewValue(), DESCRIPTION1);
    Assertions.assertNull(productDetailChanges.get(1).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(6).getField(),
        Constants.HISTORY_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertEquals(productDetailChanges.get(6).getOldValue(), ATTRIBUTE_VALUE);
    Assertions.assertEquals(productDetailChanges.get(6).getNewValue(), ATTRIBUTE_VALUE2);

    Assertions.assertEquals(productDetailChanges.get(7).getField(),
        Constants.HISTORY_DELETED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertNull(productDetailChanges.get(7).getOldValue());
    Assertions.assertNull(productDetailChanges.get(7).getNewValue());

    Assertions.assertEquals(productDetailChanges.get(8).getField(),
        Constants.HISTORY_ADDED_ATTRIBUTE + ATTRIBUTE_NAME);
    Assertions.assertNull(productDetailChanges.get(8).getOldValue());
    Assertions.assertNull(productDetailChanges.get(8).getNewValue());
  }

  @Test
   void getImageChangesTest() throws IOException {
    final List<ProductHistoryDTO> productDetailChanges =
        instance.getImageChanges(getProduct(), getProduct1());
    Assertions.assertEquals(3, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.PRODUCT_IMAGES);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), LOCATION);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), LOCATION1);
    Assertions.assertNull(productDetailChanges.get(0).getSkuName());

    Assertions.assertEquals(productDetailChanges.get(2).getField(), Constants.PRODUCT_IMAGES);
    Assertions.assertEquals(productDetailChanges.get(2).getOldValue(), LOCATION);
    Assertions.assertEquals(productDetailChanges.get(2).getNewValue(), LOCATION1);
    Assertions.assertNotNull(productDetailChanges.get(2).getSkuName());
  }

  @Test
   void getImageChangesTest2() throws IOException {
    ProductItem productItem1 = new ProductItem();
    productItem1.setGeneratedItemName(ITEM_NAME);

    ProductItem productItem2 = new ProductItem();
    productItem2.setGeneratedItemName(ITEM_NAME);

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setMainImage(true);
    productItemImage1.setProductItem(productItem1);
    productItemImage1.setLocationPath(LOCATION);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setMainImage(true);
    productItemImage2.setProductItem(productItem2);
    productItemImage2.setLocationPath(LOCATION1);

    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setMainImage(true);
    productItemImage3.setOriginalImage(true);
    productItemImage3.setProductItem(productItem2);
    productItemImage3.setLocationPath(LOCATION1);

    productItem1.setProductItemImages(List.of(productItemImage1));
    productItem2.setProductItemImages(Arrays.asList(productItemImage2, productItemImage3));

    Product exsistingProduct = getProduct();
    exsistingProduct.setProductItems(List.of(productItem1));

    Product newProduct = getProduct1();
    newProduct.setProductItems(List.of(productItem2));

    final List<ProductHistoryDTO> productDetailChanges =
        instance.getImageChanges(exsistingProduct, newProduct);

    Assertions.assertEquals(2, productDetailChanges.size());
  }

  @Test
   void getImageChangesTest3() throws IOException {
    ProductItem productItem1 = new ProductItem();
    productItem1.setGeneratedItemName(ITEM_NAME);

    ProductItem productItem2 = new ProductItem();
    productItem2.setGeneratedItemName(ITEM_NAME);

    ProductItem productItem3 = new ProductItem();
    productItem3.setGeneratedItemName(ITEM_NAME+ "2");

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setMainImage(true);
    productItemImage1.setProductItem(productItem1);
    productItemImage1.setLocationPath(LOCATION);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setMainImage(true);
    productItemImage2.setProductItem(productItem2);
    productItemImage2.setLocationPath(LOCATION);

    productItem1.setProductItemImages(List.of(productItemImage1));
    productItem2.setProductItemImages(List.of(productItemImage2));

    Product exsistingProduct = getProduct();
    exsistingProduct.setProductItems(Arrays.asList(productItem1, productItem3));

    Product newProduct = getProduct1();
    newProduct.setProductItems(List.of(productItem2));

    final List<ProductHistoryDTO> productDetailChanges =
        instance.getImageChanges(exsistingProduct, newProduct);

    Assertions.assertEquals(1, productDetailChanges.size());
  }

  private Product getProduct1() {
    Product product1 = new Product();
    product1.setProductAttributes(Collections.emptyList());
    product1.setProductName(PRODUCT_NAME1);
    product1.setDescription(DESCRIPTION1.getBytes());
    product1.setVideoUrl(URL1);
    product1.setUniqueSellingPoint(USP1);
    product1.setCategoryCode(CATEGORY_CODE);
    product1.setLength(10.0);
    product1.setWidth(10.0);
    product1.setHeight(10.0);
    product1.setWeight(10.0);
    product1.setProductType(1);
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(LOCATION1);
    product1.setProductImages(Collections.singletonList(productImage1));

    ProductItem productItem1 = new ProductItem();
    productItem1.setDangerousGoodsLevel(1);
    productItem1.setSkuCode(SKU_CODE);
    productItem1.setGeneratedItemName(ITEM_NAME);
    product1.setProductItems(Collections.singletonList(productItem1));

    ProductItemImage itemImage1 = new ProductItemImage();
    itemImage1.setLocationPath(LOCATION1);
    itemImage1.setMainImage(true);
    productItem1.setProductItemImages(Collections.singletonList(itemImage1));

    ProductAttribute productAttribute1 = new ProductAttribute();
    productAttribute1.setAttributeCode(ATTRIBUTE_CODE);
    productAttribute1.setValue(ATTRIBUTE_VALUE2);
    productAttribute1.setName(ATTRIBUTE_NAME);
    productAttribute1.setVariantCreation(false);
    productAttribute1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    product1.setProductAttributes(new ArrayList<>(List.of(productAttribute1)));

    ProductAttribute productAttributeVariant = new ProductAttribute();
    productAttributeVariant.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeVariant.setValue(ATTRIBUTE_VALUE2);
    productAttributeVariant.setName(ATTRIBUTE_NAME2);
    productAttributeVariant.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttributeVariant.setVariantCreation(true);
    product1.getProductAttributes().add(productAttributeVariant);

    ProductItemAttribute productItemAttribute1 = new ProductItemAttribute();
    productItemAttribute1.setAttributeCode(ATTRIBUTE_CODE);
    productItemAttribute1.setName(ATTRIBUTE_NAME);
    productItemAttribute1.setValue(ATTRIBUTE_VALUE2);
    productItem1.setProductItemAttributes(new ArrayList<>(List.of(productItemAttribute1)));
    return product1;
  }

  private Product getProduct() {
    Product product = new Product();
    product.setProductName(PRODUCT_NAME);
    product.setDescription(PRODUCT_DESCRIPTION.getBytes());
    product.setVideoUrl(URL);
    product.setUniqueSellingPoint(USP);
    product.setCategoryCode(CATEGORY_CODE);
    product.setLength(10.0);
    product.setWidth(10.0);
    product.setHeight(10.0);
    product.setWeight(10.0);
    product.setProductType(1);

    ProductItem productItem = new ProductItem();
    productItem.setDangerousGoodsLevel(0);
    productItem.setSkuCode(SKU_CODE);
    productItem.setGeneratedItemName(ITEM_NAME);
    product.setProductItems(Collections.singletonList(productItem));

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productAttribute.setValue(ATTRIBUTE_VALUE);
    productAttribute.setName(ATTRIBUTE_NAME);
    productAttribute.setVariantCreation(false);
    productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    product.setProductAttributes(new ArrayList<>(List.of(productAttribute)));

    ProductAttribute productAttributeVariant = new ProductAttribute();
    productAttributeVariant.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeVariant.setValue(ATTRIBUTE_VALUE);
    productAttributeVariant.setName(ATTRIBUTE_NAME2);
    productAttributeVariant.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
    productAttributeVariant.setVariantCreation(true);
    product.getProductAttributes().add(productAttributeVariant);

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(LOCATION);
    product.setProductImages(Collections.singletonList(productImage));

    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    productItemAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productItemAttribute.setName(ATTRIBUTE_NAME);
    productItemAttribute.setValue(ATTRIBUTE_VALUE);
    productItem.setProductItemAttributes(new ArrayList<>(List.of(productItemAttribute)));

    ProductItemImage itemImage = new ProductItemImage();
    itemImage.setLocationPath(LOCATION);
    itemImage.setMainImage(true);
    productItem.setProductItemImages(Collections.singletonList(itemImage));
    return product;
  }

  @Test
   void replaceProductDetailsTest() {
    Product oldProduct = createProduct();
    Product newProduct = createProduct();
    oldProduct.getProductAttributes().get(1).setVariantCreation(true);
    newProduct.getProductAttributes().get(1).setVariantCreation(true);
    oldProduct.getProductAttributes().get(1).setValue("oldValue");
    newProduct.getProductAttributes().get(1).setValue("newValue");
    Product replaceProduct = this.instance.replaceProductDetails(oldProduct, newProduct, true);
    Assertions.assertEquals("oldValue", replaceProduct.getProductAttributes().get(1).getValue());
  }

  @Test
   void toJsonTest() throws JsonProcessingException {
    Mockito.when(objectMapper.writeValueAsString(Mockito.any()))
        .thenReturn(DESCRIPTION1);
    final Product product = getProduct();
    final String json = instance.toJson(product);
    Mockito.verify(this.objectMapper).writeValueAsString(product);
    Assertions.assertEquals(DESCRIPTION1, json);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.objectMapper);
    Mockito.verifyNoMoreInteractions(this.youTube);
    Mockito.verifyNoMoreInteractions(this.productServiceRepository);
  }

  @Test
   void validateYouTubeUrlWithValidUrlTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.when(list.execute()).thenReturn(videoListResponse);
    boolean youTubeUrlResponse = instance.validateYouTubeUrl(YOUTUBE_URL, youTube);
    Mockito.verify(youTube, Mockito.times(2)).videos();
    Mockito.verify(list).execute();
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
   void validateProtectedBrandTest() throws Exception {
    Mockito.when(productServiceRepository.validateProtectedBrand(BRAND_CODE, BRAND_CODE)).thenReturn(true);
    boolean youTubeUrlResponse = instance.validateProtectedBrand(BRAND_CODE, BRAND_CODE);
    Mockito.verify(productServiceRepository).validateProtectedBrand(BRAND_CODE, BRAND_CODE);
    Assertions.assertTrue(youTubeUrlResponse);
  }

  @Test
   void getBrandCodeByNameTest() throws Exception {
    Mockito.when(productServiceRepository.filterByBrandName(BRAND_CODE)).thenReturn(new BrandResponse());
    String brandName = instance.getBrandCodeByBrandName(BRAND_CODE);
    Mockito.verify(productServiceRepository).filterByBrandName(BRAND_CODE);
    Assertions.assertNull(brandName);
  }

  @Test
   void validateProtectedBrandExceptionTest() {
    Mockito.when(productServiceRepository.validateProtectedBrand(BRAND_CODE, BRAND_CODE)).thenThrow(RuntimeException.class);
    try {
      instance.validateProtectedBrand(BRAND_CODE, BRAND_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(productServiceRepository).validateProtectedBrand(BRAND_CODE, BRAND_CODE);
    }
  }

  @Test
   void getBrandCodeByNameExceptionTest() {
    Mockito.when(productServiceRepository.filterByBrandName(BRAND_CODE)).thenThrow(RuntimeException.class);
    try {
      instance.getBrandCodeByBrandName(BRAND_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(productServiceRepository).filterByBrandName(BRAND_CODE);
    }
  }

  @Test
   void validateYouTubeUrlWithInvalidUrlTest() throws Exception {
    boolean youTubeUrlResponse = instance.validateYouTubeUrl(YOUTUBE_INVALID_URL, youTube);
    Assertions.assertFalse(youTubeUrlResponse);
  }

  @Test
   void validateYouTubeUrlWithValidUrlExceptionTest() throws Exception {
    Mockito.when(youTube.videos()).thenReturn(videos);
    Mockito.when(youTube.videos().list(VIDEO_LIST)).thenReturn(list);
    Mockito.doThrow(IOException.class).when(list).execute();
    boolean youTubeUrlResponse = instance.validateYouTubeUrl(YOUTUBE_URL, youTube);
    Assertions.assertFalse(youTubeUrlResponse);
    Mockito.verify(list).execute();
    Mockito.verify(youTube, Mockito.times(2)).videos();
  }

  @Test
   void getProductDetailChangesCategoryTest() {
    Product product = getProduct();
    product.setCategoryName(CATEGORY_NAME);
    Product product1 = getProduct();
    product1.setCategoryCode(CATEGORY_CODE1);
    product1.setCategoryName(CATEGORY_NAME1);
    final List<ProductHistoryDTO> productDetailChanges = instance.getProductDetailChanges(product, product1);
    Assertions.assertEquals(1, productDetailChanges.size());
    Assertions.assertEquals(productDetailChanges.get(0).getField(), Constants.CATEGORY);
    Assertions.assertEquals(productDetailChanges.get(0).getOldValue(), CATEGORY_NAME);
    Assertions.assertEquals(productDetailChanges.get(0).getNewValue(), CATEGORY_NAME1);
  }

  @Test
   void removeOriginalImagesFromProductTest() {
    Product product = getProductWithOriginalImages();
    instance.removeOriginalImagesFromProduct(product);
    Assertions.assertEquals(1, product.getProductImages().size());
    Assertions.assertEquals(1, product.getProductItems().get(0).getProductItemImages().size());
  }

  private Product getProductWithOriginalImages() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setOriginalImage(true);
    ProductImage productImage2 = new ProductImage();
    productImage2.setOriginalImage(false);
    ProductImage productImage3 = new ProductImage();
    productImage3.setOriginalImage(null);
    List<ProductImage> productImages = new ArrayList<>();
    productImages.add(productImage1);
    productImages.add(productImage2);

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setOriginalImage(true);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setOriginalImage(false);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage2.setOriginalImage(null);
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItemImages.add(productItemImage1);
    productItemImages.add(productItemImage2);
    ProductItem productItem = new ProductItem();
    productItem.setProductItemImages(productItemImages);

    Product product = new Product();
    product.setProductImages(productImages);
    product.setProductItems(List.of(productItem));

    return product;
  }

  @Test
   void setProductDimensionsAndProductTypeAndDgLevelTest() {
    Product product = new Product();
    product.setProductItems(List.of(new ProductItem()));
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        PDTDimensionRefreshEventModel.builder().length(LENGTH).width(WIDTH).height(HEIGHT).weight(WEIGHT)
            .shippingWeight(SHIPPING_WEIGHT).dangerousGoodsLevel(DG_LEVEL).productType(PRODUCT_TYPE).build();
    instance.setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Assertions.assertEquals(LENGTH, product.getLength());
    Assertions.assertEquals(WIDTH, product.getWidth());
    Assertions.assertEquals(HEIGHT, product.getHeight());
    Assertions.assertEquals(WEIGHT, product.getWeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, product.getShippingWeight());
    Assertions.assertEquals(DG_LEVEL, product.getProductItems().get(0).getDangerousGoodsLevel());
  }

  @Test
   void setProductDimensionsAndProductTypeAndDgLevel_nullDgLevelTest() {
    Product product = new Product();
    product.setProductItems(List.of(new ProductItem()));
    product.getProductItems().get(0).setDangerousGoodsLevel(DG_LEVEL);
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
      PDTDimensionRefreshEventModel.builder().length(LENGTH).width(WIDTH).height(HEIGHT).weight(WEIGHT)
        .shippingWeight(SHIPPING_WEIGHT).productType(PRODUCT_TYPE).build();
    instance.setProductDimensionsAndProductTypeAndDgLevel(product, pdtDimensionRefreshEventModel);
    Assertions.assertEquals(LENGTH, product.getLength());
    Assertions.assertEquals(WIDTH, product.getWidth());
    Assertions.assertEquals(HEIGHT, product.getHeight());
    Assertions.assertEquals(WEIGHT, product.getWeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, product.getShippingWeight());
    Assertions.assertEquals(DG_LEVEL, product.getProductItems().get(0).getDangerousGoodsLevel());
  }

  @Test
   void setCommonImageFlagForProductAndItemImagesTest() {
    Product product = getProductForCommonImageTest();

    instance.setCommonImageFlagForProductAndItemImages(product);

    Assertions.assertTrue(product.getProductImages().get(0).isCommonImage());
    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertTrue(
        product.getProductItems().get(0).getProductItemImages().get(0).isCommonImage());
    Assertions.assertFalse(
        product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
    Assertions.assertTrue(
        product.getProductItems().get(1).getProductItemImages().get(0).isCommonImage());
  }

  @Test
   void setCommonImageFlagForProductAndItemImagesNullItemImageTest() {
    Product product = getProductForCommonImageTest();
    product.setProductItems(new ArrayList<>());

    instance.setCommonImageFlagForProductAndItemImages(product);


    Assertions.assertFalse(product.getProductImages().get(0).isCommonImage());
    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
  }

  private Product getProductForCommonImageTest() {
    ProductImage productImage1 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage1.setLocationPath(LOCATION_PATH_1);
    productImage2.setLocationPath(LOCATION_PATH_2);

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    productItemImage2.setLocationPath(LOCATION_PATH_2);

    ProductItem productItem1 = new ProductItem();
    ProductItem productItem2 = new ProductItem();
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2));
    productItem2.setProductItemImages(List.of(productItemImage1));

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2));
    product.setProductItems(Arrays.asList(productItem1, productItem2));

    return product;
  }

  @Test
   void isProductCodeTest() {
    ReflectionTestUtils.setField(instance, "vendorSearchAutoHealRegex", PRODUCT_CODE_PATTERN);
    Assertions.assertFalse(instance.isProductCode(PRODUCT_NAME));
    Assertions.assertTrue(instance.isProductCode(PRODUCT_CODE));
  }

  @Test
   void copyProductItemAttributesFromPCB() {
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemAttributes(List.of(productItemAttribute));
    ProductItem productItem2 = new ProductItem();
    productItem2.setProductItemAttributes(List.of(productItemAttribute));
    instance.copyProductItemAttributesFromPCB(productItem1, productItem2, true);
    instance.copyProductItemAttributesFromPCB(productItem1, productItem2, false);
  }

  @Test
   void productContainsEmptyItemAttributes() {
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemAttributes(List.of(productItemAttribute));
    ProductItem productItem2 = new ProductItem();
    productItem2.setProductItemAttributes(List.of(productItemAttribute));
    instance.copyProductItemAttributesFromPCB(productItem1, productItem2, true);
    instance.copyProductItemAttributesFromPCB(productItem1, productItem2, false);
  }

  @Test
   void productContainsEmptyItemAttributesTest() {
    Product product = new Product();
    ProductItemAttribute productItemAttribute = new ProductItemAttribute();
    ProductItem productItem = new ProductItem();
    productItem.setProductItemAttributes(new ArrayList<>());
    product.setProductItems(List.of(productItem));
    Assertions.assertTrue(instance.productContainsEmptyItemAttributes(product));

    productItemAttribute.setMarkForDelete(true);
    productItem.setProductItemAttributes(List.of(productItemAttribute));
    Assertions.assertTrue(instance.productContainsEmptyItemAttributes(product));

    productItemAttribute.setMarkForDelete(false);
    productItem.setProductItemAttributes(List.of(productItemAttribute));
    Assertions.assertFalse(instance.productContainsEmptyItemAttributes(product));
  }
}
