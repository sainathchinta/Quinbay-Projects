package com.gdn.mta.product.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ProductResubmitChangeUtilTest {

  private ProductDetailResponse mockOldProduct;
  private ProductCreationRequest mockNewProduct;

  @BeforeEach
  public void setUp() throws Exception {
    mockOldProduct = getProductOldProduct();
    mockNewProduct = getNewProduct();
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
  public void getProductDiff() {
  }

  private ProductDetailResponse getProductOldProduct() {
    ProductDetailResponse oldProduct = new ProductDetailResponse();
    oldProduct.setName("testProduct1");
    oldProduct.setBrand("Samsung");
    oldProduct.setDescription("<p>some description</p>".getBytes());
    oldProduct.setHeight(1.2D);
    oldProduct.setLength(1.2D);
    oldProduct.setWeight(3D);
    oldProduct.setWidth(1.2D);
    oldProduct.setProductStory("<p>product story ....</p>");
    oldProduct.setUniqueSellingPoint("<p>usp</p>");
    oldProduct.setProductAttributeResponses(getOldProductAttributes());
    oldProduct.setProductItemResponses(getOldProductItems());
    return oldProduct;
  }

  private Set<ProductItemResponse> getOldProductItems() {
    ProductItemResponse item = new ProductItemResponse();
    item.setGeneratedItemName("Product Item1");
    item.setId("123");
    item.setDangerousGoodsLevel(2);
    item.setUpcCode("abc");
    Set<ProductItemResponse> itemList = new HashSet<>();
    itemList.add(item);
    return itemList;
  }

  private List<ProductAttributeResponse> getOldProductAttributes() {
    List<ProductAttributeResponse> productAttributeList = new ArrayList<>();
    ProductAttributeResponse attr1 = new ProductAttributeResponse();
    attr1.setProductAttributeName("product Attr1");
    attr1.setId("id1");
    ProductAttributeValueResponse val1 = new ProductAttributeValueResponse();
    val1.setId("123");
    val1.setDescriptiveAttributeValue("test");
    val1.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);

    ProductAttributeValueResponse val2 = new ProductAttributeValueResponse();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValueResponse predefinedVal = new PredefinedAllowedAttributeValueResponse();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValueResponse> valueList = new ArrayList<>();
    valueList.add(val1);
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    productAttributeList.add(attr1);
    return productAttributeList;
  }

  private ProductCreationRequest getNewProduct() {
    ProductCreationRequest newProduct = new ProductCreationRequest();
    newProduct.setName("testProduct1");
    newProduct.setBrand("Samsung");
    newProduct.setDescription("<p>some description</p>".getBytes());
    newProduct.setHeight(1.2D);
    newProduct.setLength(1.2D);
    newProduct.setWeight(3D);
    newProduct.setWidth(1.2D);
    newProduct.setProductStory("<p>product story ....</p>");
    newProduct.setUniqueSellingPoint("<p>usp</p>");
    newProduct.setProductAttributes(getProductAttributes());
    newProduct.setProductItemRequests(getProductItems());
    return newProduct;
  }

  private List<ProductItemCreationRequest> getProductItems() {
    ProductItemCreationRequest item = new ProductItemCreationRequest();
    item.setProductItemId("123");
    item.setUpcCode("abc");
    List<ProductItemCreationRequest> itemList = new ArrayList<>();
    itemList.add(item);
    return itemList;
  }

  private List<ProductAttributeRequest> getProductAttributes() {
    List<ProductAttributeRequest> productAttributeList = new ArrayList<>();
    ProductAttributeRequest attr1 = new ProductAttributeRequest();
    attr1.setProductAttributeName("product Attr1");
    attr1.setId("id1");
    ProductAttributeValueRequest val1 = new ProductAttributeValueRequest();
    val1.setId("123");
    val1.setDescriptiveAttributeValue("test");
    val1.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE);

    ProductAttributeValueRequest val2 = new ProductAttributeValueRequest();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValueRequest predefinedVal = new PredefinedAllowedAttributeValueRequest();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValueRequest> valueList = new ArrayList<>();
    valueList.add(val1);
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    productAttributeList.add(attr1);
    return productAttributeList;
  }

  @Test
  public void getProductDiff_nameChange() {
    mockNewProduct.setName("new Name");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
  }

  @Test
  public void getProductDiff_nullCheck() {
    mockNewProduct.setName("new Name");
    mockOldProduct.setBrand(null);
    mockOldProduct.setName(null);
    mockOldProduct.setDescription(null);
    mockOldProduct.setUniqueSellingPoint(null);
    mockOldProduct.setProductStory(null);
    mockOldProduct.getProductItemResponses().iterator().next().setUpcCode(null);
    mockNewProduct.setDescription(null);
    mockNewProduct.setProductStory("");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
  }

  @Test
  public void getProductDiff_AttrValuesChange() {

    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Changed Descriptive Value");

    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(CollectionUtils.isNotEmpty((Collection) histList.get(0).getNewValue()));
    assertTrue(CollectionUtils.isNotEmpty((Collection) histList.get(0).getOldValue()));
  }

  @Test
  public void getProductDiff_ItemGoodsLimitChanged() {
    mockNewProduct.getProductItemRequests().get(0).setUpcCode("a");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getNewValue() != null);
    assertTrue(histList.get(0).getOldValue() != null);
  }

  @Test
  public void getProductDiff_ItemUPCCodeChanged() {
    mockNewProduct.getProductItemRequests().get(0).setUpcCode("000111AAA");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getNewValue() != null);
    assertTrue(histList.get(0).getOldValue() != null);
  }

  @Test
  public void getProductDiff_productDimensionChanged() {
    mockNewProduct.setHeight(3D);
    mockNewProduct.setWidth(5D);
    mockNewProduct.setLength(3D);
    mockNewProduct.setWeight(5D);
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 4);
  }

  @Test
  public void getProductDiff_productDescriptionChanged() {
    mockNewProduct.setDescription("<p>changed description</p>".getBytes());
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getFieldName().equals(ProductChangeUtil.DESC_FIELD));
    assertForParagraphChars(histList);
  }

  private void assertForParagraphChars(List<ProductFieldHistory> histList) {
    assertTrue(!StringUtils.contains(histList.get(0).getOldValue().toString(), "<p>"));
    assertTrue(!StringUtils.contains(histList.get(0).getNewValue().toString(), "<p>"));
    assertTrue(!StringUtils.contains(histList.get(0).getOldValue().toString(), "</p>"));
    assertTrue(!StringUtils.contains(histList.get(0).getNewValue().toString(), "</p>"));
  }

  @Test
  public void getProductDiff_productStoryChanged() {
    mockNewProduct.setProductStory("story changed");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getFieldName().equals(ProductChangeUtil.PRODUCT_STORY_FIELD));
    assertForParagraphChars(histList);
  }

  @Test
  public void getProductDiff_productUSPChanged() {
    mockNewProduct.setUniqueSellingPoint("<p>USP changed</p>");
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getFieldName().equals(ProductChangeUtil.USP_FIELD));
    assertForParagraphChars(histList);
  }

  @Test
  public void getProductDiff_moreThanOneItemChanged() {
    mockNewProduct.setProductStory("story changed");
    mockNewProduct.setDescription("changed description".getBytes());
    mockNewProduct.getProductItemRequests().get(0).setUpcCode("000111AAA");
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Changed Descriptive Value");

    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(4, histList.size());
  }

  @Test
  public void getProductDiff_NothingChanged() {
    List<ProductFieldHistory> histList = ProductResubmitChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }
}