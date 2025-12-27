package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.TreeMap;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ProductContentUtilTest {

  private static String DEFAULT_PRODUCT_NAME = "PRODUCT NAME";
  private static String DEFAULT_BRAND = "BRAND";
  private static String DEFAULT_DESCRIPTION = "DESCRIPTION";
  private static String DEFAULT_UNIQUE_SELLING_POINT = "UNIQUE SELLING POINT";
  private static String DEFAULT_PRODUCT_STORY = "PRODUCT_STORY";
  private static Double DEFAULT_LENGTH = 1D;
  private static Double DEFAULT_WIDTH = 1D;
  private static Double DEFAULT_HEIGHT = 1D;
  private static Double DEFAULT_WEIGHT = 1D;
  private static String DEFAULT_SKU_CODE = "MTA-0000001-00001";
  private static Integer DEFAULT_DANGEROUS_GOOD_LEVEL = 1;
  private static String DEFAULT_UPC_CODE = UUID.randomUUID().toString();
  private static String DEFAULT_PRODUCT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static String DEFAULT_PRODUCT_ATTRIBUTE_ID_2 = UUID.randomUUID().toString();
  private static String DEFAULT_PRODUCT_ATTRIBUTE_ID_3 = UUID.randomUUID().toString();
  private static String DEFAULT_ATTRIBUTE_NAME = "ATTRIBUTE 1";
  private static String DEFAULT_ATTRIBUTE_NAME_2 = "ATTRIBUTE 2";
  private static String DEFAULT_ATTRIBUTE_NAME_3 = "ATTRIBUTE 3";
  private static String DEFAULT_ATTRIBUTE_NAME_4 = "ATTRIBUTE 4";
  private static String ASSERT_ERROR_MSG = "NOT Equal";
  private static String FAMILY_COLOUR_ATTRIBUTE_CODE = "FAM-00001";
  private static String FAMILY_COLOUR_ATTRIBUTE_VALUE= "Merak";


  private ProductDetailResponse generateProductDetailResponse() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(ProductContentUtilTest.DEFAULT_PRODUCT_NAME);
    productDetailResponse.setBrand(ProductContentUtilTest.DEFAULT_BRAND);
    productDetailResponse.setDescription(ProductContentUtilTest.DEFAULT_DESCRIPTION.getBytes());
    productDetailResponse
        .setUniqueSellingPoint(ProductContentUtilTest.DEFAULT_UNIQUE_SELLING_POINT);
    productDetailResponse.setProductStory(ProductContentUtilTest.DEFAULT_PRODUCT_STORY);
    productDetailResponse.setLength(ProductContentUtilTest.DEFAULT_LENGTH);
    productDetailResponse.setWidth(ProductContentUtilTest.DEFAULT_WIDTH);
    productDetailResponse.setHeight(ProductContentUtilTest.DEFAULT_HEIGHT);
    productDetailResponse.setWeight(ProductContentUtilTest.DEFAULT_WEIGHT);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ProductContentUtilTest.DEFAULT_SKU_CODE);
    productItemResponse.setGeneratedItemName(ProductContentUtilTest.DEFAULT_PRODUCT_NAME);
    productItemResponse.setDangerousGoodsLevel(ProductContentUtilTest.DEFAULT_DANGEROUS_GOOD_LEVEL);
    productItemResponse.setUpcCode(ProductContentUtilTest.DEFAULT_UPC_CODE);
    productDetailResponse.getProductItemResponses().add(productItemResponse);
    ProductAttributeResponse productAttributeResponse1 = new ProductAttributeResponse();
    productAttributeResponse1.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID);
    productAttributeResponse1.setAttribute(new AttributeResponse());
    productAttributeResponse1.getAttribute().setAttributeType(
        AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productAttributeResponse1.getAttribute().setName(ProductContentUtilTest.DEFAULT_ATTRIBUTE_NAME);
    productAttributeResponse1
        .setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    productAttributeResponse1.getProductAttributeValues().add(new ProductAttributeValueResponse());
    productAttributeResponse1.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("VALUE");
    ProductAttributeResponse productAttributeResponse2 = new ProductAttributeResponse();
    productAttributeResponse2.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID_2);
    productAttributeResponse2.setAttribute(new AttributeResponse());
    productAttributeResponse2.getAttribute().setAttributeType(
        AttributeType.PREDEFINED_ATTRIBUTE.name());
    productAttributeResponse2.getAttribute().setName(
        ProductContentUtilTest.DEFAULT_ATTRIBUTE_NAME_2);
    productAttributeResponse2
        .setProductAttributeValues(new ArrayList<ProductAttributeValueResponse>());
    productAttributeResponse2.getProductAttributeValues().add(new ProductAttributeValueResponse());
    productAttributeResponse2.getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueResponse());
    productAttributeResponse2.getProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().setValue("VALUE");
    ProductAttributeResponse productAttributeResponse3 = new ProductAttributeResponse();
    productAttributeResponse3.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID_3);
    productAttributeResponse3.setAttribute(new AttributeResponse());
    productAttributeResponse3.getAttribute().setAttributeType(
        AttributeType.DEFINING_ATTRIBUTE.name());
    productAttributeResponse3.getAttribute().setName(
        ProductContentUtilTest.DEFAULT_ATTRIBUTE_NAME_3);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse1);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse2);
    productDetailResponse.getProductAttributeResponses().add(productAttributeResponse3);
    return productDetailResponse;
  }

  private ProductRequest generateProductRequest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setName(ProductContentUtilTest.DEFAULT_PRODUCT_NAME + "TEST");
    productRequest.setBrand(ProductContentUtilTest.DEFAULT_BRAND + "TEST");
    productRequest.setDescription((ProductContentUtilTest.DEFAULT_DESCRIPTION + "TEST").getBytes());
    productRequest.setUniqueSellingPoint(ProductContentUtilTest.DEFAULT_UNIQUE_SELLING_POINT
        + "TEST");
    productRequest.setProductStory(ProductContentUtilTest.DEFAULT_PRODUCT_STORY + "TEST");
    productRequest.setLength(ProductContentUtilTest.DEFAULT_LENGTH + 1);
    productRequest.setWidth(ProductContentUtilTest.DEFAULT_WIDTH + 1);
    productRequest.setHeight(ProductContentUtilTest.DEFAULT_HEIGHT + 1);
    productRequest.setWeight(ProductContentUtilTest.DEFAULT_WEIGHT + 1);
    productRequest.setProductItems(new ArrayList<ProductItemRequest>());
    productRequest.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setSkuCode(ProductContentUtilTest.DEFAULT_SKU_CODE);
    productItemRequest
        .setDangerousGoodsLevel(ProductContentUtilTest.DEFAULT_DANGEROUS_GOOD_LEVEL + 1);
    productItemRequest.setUpcCode(ProductContentUtilTest.DEFAULT_UPC_CODE + "TEST");
    productRequest.getProductItems().add(productItemRequest);
    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID);
    productAttributeRequest1.setAttribute(new AttributeRequest());
    productAttributeRequest1.getAttribute().setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeRequest1
        .setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest1.getProductAttributeValues().add(new ProductAttributeValueRequest());
    productAttributeRequest1.getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("TEST");
    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID_2);
    productAttributeRequest2.setAttribute(new AttributeRequest());
    productAttributeRequest2.getAttribute().setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeRequest2
        .setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
    productAttributeRequest2.getProductAttributeValues().add(new ProductAttributeValueRequest());
    productAttributeRequest2.getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeRequest2.getProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().setValue("TEST");
    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setId(ProductContentUtilTest.DEFAULT_PRODUCT_ATTRIBUTE_ID_3);
    productAttributeRequest3.setAttribute(new AttributeRequest());
    productAttributeRequest3.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productRequest.getProductAttributes().add(productAttributeRequest1);
    productRequest.getProductAttributes().add(productAttributeRequest2);
    productRequest.getProductAttributes().add(productAttributeRequest3);
    return productRequest;
  }

  @Test
  public void getProductContentDifferenceTest() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    request.setDescription(productData.getDescription());
    ProductContentUtil.getProductContentDifference(request, productData);
  }
  
  @Test
  public void getProductContentDifferenceWhenSpecificationNull() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    request.setDescription(null);
    request.setProductStory(null);
    productData.setDescription(null);
    productData.setProductStory(null);
    
    List<ProductFieldHistory> response = 
        ProductContentUtil.getProductContentDifference(request, productData);
    
    Assertions.assertNotNull(response);
  }
  
  @Test
  public void getProductContentDifferenceWhenOriginSpecificationNull() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    productData.setProductStory(null);
    
    List<ProductFieldHistory> response = 
        ProductContentUtil.getProductContentDifference(request, productData);
    
    Assertions.assertNotNull(response);
  }
  
  @Test
  public void getProductContentDifferenceWhenSourceSpecificationNull() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    request.setProductStory(null);
    
    List<ProductFieldHistory> response = 
        ProductContentUtil.getProductContentDifference(request, productData);
    
    Assertions.assertNotNull(response);
  }

  @Test
  public void getProductContentDifferenceTestWithOldProductAttributeNull() throws Exception {
    ProductDetailResponse oldProduct = generateProductDetailResponse();
    ProductRequest newProduct = generateProductRequest();
    oldProduct.getProductAttributeResponses().get(0).setAttribute(null);
    List<ProductFieldHistory> productFieldHistories =
        ProductContentUtil.getProductContentDifference(newProduct, oldProduct);
    Assertions.assertNotEquals(ASSERT_ERROR_MSG, oldProduct.getName(), newProduct.getName());
  }

  @Test
  public void getProductContentDifferenceTestWitholdProductPredefinedAllowedAttributeValueResponseNull()
      throws Exception {
    ProductDetailResponse oldProduct = generateProductDetailResponse();
    ProductRequest newProduct = generateProductRequest();
    oldProduct.getProductAttributeResponses().get(0).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    List<ProductFieldHistory> productFieldHistories =
        ProductContentUtil.getProductContentDifference(newProduct, oldProduct);
  }

  @Test
  public void getProductContentDifferenceTestWithNewProductPredefinedAllowedAttributeValueResponseNull()
      throws Exception {
    ProductDetailResponse oldProduct = generateProductDetailResponse();
    ProductRequest newProduct = generateProductRequest();
    oldProduct.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueResponse());
    oldProduct.getProductAttributeResponses().get(0).getAttribute()
        .setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    List<ProductFieldHistory> productFieldHistories =
        ProductContentUtil.getProductContentDifference(newProduct, oldProduct);
  }

  @Test
  public void getProductContentDifference_2_Test() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    request.getProductItems().get(0)
        .setDangerousGoodsLevel(ProductContentUtilTest.DEFAULT_DANGEROUS_GOOD_LEVEL);
    request.getProductItems().get(0).setUpcCode(ProductContentUtilTest.DEFAULT_UPC_CODE);
    request.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("VALUE");
    request.getProductAttributes().get(1).getProductAttributeValues().get(0)
        .getPredefinedAllowedAttributeValue().setValue("VALUE");
    ProductContentUtil.getProductContentDifference(request, productData);
  }
  
  @Test
  public void getProductContentDifference_3_Test() throws Exception {
    ProductDetailResponse productData = generateProductDetailResponse();
    ProductRequest request = generateProductRequest();
    productData.getProductAttributeResponses().get(0).getProductAttributeValues().clear();
    request.getProductAttributes().get(1).getProductAttributeValues().clear();
    ProductContentUtil.getProductContentDifference(request, productData);
  }
  
  @SuppressWarnings("unused")
  @Test
  public void constructorTest() throws Exception {
    ProductContentUtil productContentUtil = new ProductContentUtil();
  }



  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnDefiningWarnaTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(2, productItemCreationRequest.getProductItemAttributeValueRequests().size());
    Assertions.assertEquals(FAMILY_COLOUR_ATTRIBUTE_CODE, productItemCreationRequest.getProductItemAttributeValueRequests().get(1).getAttribute().getAttributeCode());

  }


  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnDescriptiveWarnaTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest1.setVariantCreation(true);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(2, productItemCreationRequest.getProductItemAttributeValueRequests().size());
    Assertions.assertEquals(FAMILY_COLOUR_ATTRIBUTE_CODE, productItemCreationRequest.getProductItemAttributeValueRequests().get(1).getAttribute().getAttributeCode());
  }


  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnNonDefiningWarnaTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(1, productItemCreationRequest.getProductItemAttributeValueRequests().size());
  }


  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnNoWarnaTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(DEFAULT_ATTRIBUTE_NAME);
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(1, productItemCreationRequest.getProductItemAttributeValueRequests().size());
  }

  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnCategoryDontHaveFamilyColourTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest1.setVariantCreation(true);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(DEFAULT_ATTRIBUTE_NAME);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(1, productItemCreationRequest.getProductItemAttributeValueRequests().size());
  }

  @Test
  public void autoFillFamilyColourItemAttributeSwitchOnItemAttributeHasFamilyColourTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attributeRequest1.setVariantCreation(true);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeRequest2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest2 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest2.setAttribute(attributeRequest2);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(
        new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1, productItemAttributeValueRequest2)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, true);

    Assertions.assertEquals(2, productItemCreationRequest.getProductItemAttributeValueRequests().size());
  }

  @Test
  public void autoFillFamilyColourItemAttributeSwitchOffDefiningWarnaTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setName(Constants.WARNA);
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setProductAttributeName(Constants.WARNA);
    productAttributeRequest1.setAttribute(attributeRequest1);

    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>(Arrays.asList(productItemAttributeValueRequest1)));

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest1));
    productRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(FAMILY_COLOUR_ATTRIBUTE_CODE);
    attributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());

    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setCategoryAttributes(Arrays.asList(categoryAttributeResponse));

    ProductContentUtil.autoFillFamilyColourItemAttribute(productRequest, categoryDetailResponse, FAMILY_COLOUR_ATTRIBUTE_CODE, false);

    Assertions.assertEquals(1, productItemCreationRequest.getProductItemAttributeValueRequests().size());
  }

  @Test
  public void validateAttributeMapInProductItemRequestTest() {
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest1 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest1.setAttributesMap(new TreeMap<>());

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest2 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest2.setAttributesMap(new TreeMap<>());

    ProductContentUtil.validateAttributeMapInProductItemRequest(true,
        Arrays.asList(productVariantPriceStockAndImagesRequest1, productVariantPriceStockAndImagesRequest2));
  }

  @Test
  public void validateAttributeMapInProductItemRequestNullTest() {
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest1 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest1.setAttributesMap(null);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest2 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest2.setAttributesMap(null);

    ProductContentUtil.validateAttributeMapInProductItemRequest(true,
        Arrays.asList(productVariantPriceStockAndImagesRequest1, productVariantPriceStockAndImagesRequest2));
  }

  @Test
  public void validateAttributeMapInProductItemRequestMisMatchFirstTest() {
    TreeMap<String, String> treeMap = new TreeMap<>();
    treeMap.put(FAMILY_COLOUR_ATTRIBUTE_CODE, FAMILY_COLOUR_ATTRIBUTE_VALUE);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest1 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest1.setAttributesMap(treeMap);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest2 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest2.setAttributesMap(null);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ProductContentUtil.validateAttributeMapInProductItemRequest(true,
          Arrays.asList(productVariantPriceStockAndImagesRequest1, productVariantPriceStockAndImagesRequest2));
    });
  }

  @Test
  public void validateAttributeMapInProductItemRequestMisMatchSecondTest() {
    TreeMap<String, String> treeMap = new TreeMap<>();
    treeMap.put(FAMILY_COLOUR_ATTRIBUTE_CODE, FAMILY_COLOUR_ATTRIBUTE_VALUE);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest1 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest1.setAttributesMap(null);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest2 =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest2.setAttributesMap(treeMap);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ProductContentUtil.validateAttributeMapInProductItemRequest(true,
          Arrays.asList(productVariantPriceStockAndImagesRequest1, productVariantPriceStockAndImagesRequest2));
    });
  }
}
