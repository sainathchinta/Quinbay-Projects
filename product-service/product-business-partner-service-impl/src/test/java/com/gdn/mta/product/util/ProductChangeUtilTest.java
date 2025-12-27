package com.gdn.mta.product.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

/**
 * Created by Kesha on 11/04/16.
 */
public class ProductChangeUtilTest {
  private static final String ATTRIBUTE_DELETED_AFTER_CATEGORY_CHANGE =
      "This attribute was deleted after category change";
  private static final String PROMO_SKU = "Promo Sku";
  private static final String CATEGORY = "Category";
  private static final String ID = "id";
  private static final String NO_COLOR = "No-Colour";
  private static final String NON_FAMILY_COLOR = "Non-Family Colour";
  protected static final String URL_VIDEO = "Url video";
  private static final String BRAND = "brand";
  private static final String BRAND_CHANGE_HISTORY_NOTE = "{field: 'Brand', oldValue: Samsung, newValue: brand}";
  private static final String VAT_APPLICABLE = "Vat Applicable";
  private static final String ITEM_NAME = "Product Item1";
  private static final String ITEM_NAME_2 = "Product Item2";
  private static final String ITEM_ID = "itemId";

  private Product mockOldProduct;
  private Product mockNewProduct;
  private ProductChangeUtil mockProductChangeUtil;


  @BeforeEach
  public void setUp() throws Exception {
    mockOldProduct = getProduct();
    mockNewProduct = getProduct();
    mockProductChangeUtil = new ProductChangeUtil();
  }

  @AfterEach
  public void tearDown() throws Exception {
    mockNewProduct = null;
    mockOldProduct = null;
  }

  @Test
  public void getProductDiff_nameChange() throws Exception {
    mockNewProduct.setName("new Name");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
  }

  @Test
  public void getProductDiff_nullCheck() throws Exception {
    mockNewProduct.setName("new Name");
    mockOldProduct.setBrand(null);
    mockOldProduct.setName(null);
    mockOldProduct.setDescription(null);
    mockOldProduct.setUniqueSellingPoint(null);
    mockOldProduct.setProductStory(null);
    mockOldProduct.getProductItems().get(0).setUpcCode(null);
    mockNewProduct.setDescription(null);
    mockNewProduct.setProductStory("");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
  }

  @Test
  public void getProductDiff_AttrValuesChange() throws Exception {

    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Changed Descriptive Value");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(CollectionUtils.isNotEmpty((Collection) histList.get(0).getNewValue()));
    assertTrue(CollectionUtils.isNotEmpty((Collection) histList.get(0).getOldValue()));
  }

  @Test
  public void getProductDiff_ItemGoodsLimitChanged() throws Exception {
    mockNewProduct.getProductItems().get(0).setDangerousGoodsLevel(5);
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getNewValue() != null);
    assertTrue(histList.get(0).getOldValue() != null);
  }

  @Test
  public void getProductDiff_ItemUPCCodeChanged() throws Exception {
    mockNewProduct.getProductItems().get(0).setUpcCode("000111AAA");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getNewValue() != null);
    assertTrue(histList.get(0).getOldValue() != null);
  }

  @Test
  public void getProductDiff_productDimensionChanged() throws Exception {
    mockNewProduct.setHeight(3D);
    mockNewProduct.setWidth(5D);
    mockNewProduct.setLength(3D);
    mockNewProduct.setWeight(5D);
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 4);
  }

  @Test
  public void getProductDiff_productDescriptionChanged() throws Exception {
    mockNewProduct.setDescription("<p>changed description</p>".getBytes());
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
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
  public void getProductDiff_productStoryChanged() throws Exception {
    mockNewProduct.setProductStory("story changed");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getFieldName().equals(ProductChangeUtil.PRODUCT_STORY_FIELD));
    assertForParagraphChars(histList);
  }

  @Test
  public void getProductDiff_productUSPChanged() throws Exception {
    mockNewProduct.setUniqueSellingPoint("<p>USP changed</p>");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(histList.size(), 1);
    assertTrue(histList.get(0).getFieldName().equals(ProductChangeUtil.USP_FIELD));
    assertForParagraphChars(histList);
  }

  @Test
  public void getProductDiff_moreThanOneItemChanged() throws Exception {
    mockNewProduct.setProductStory("story changed");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    mockNewProduct.setDescription("changed description".getBytes());
    mockNewProduct.getProductItems().get(0).setUpcCode("000111AAA");
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Changed Descriptive Value");

    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(histList));
    assertEquals(4, histList.size());
  }

  @Test
  public void getProductDiff_categoryChangeAndNewAttributeAdded() throws Exception {
    mockNewProduct.setProductStory("story changed");
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    mockNewProduct.setDescription("changed description".getBytes());
    mockNewProduct.getProductItems().get(0).setUpcCode("000111AAA");
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("Changed Descriptive Value");
    mockNewProduct.getProductAttributes().get(0).setId(null);

    List<ProductFieldHistory> response = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(response));
    assertEquals(5, response.size());
    assertEquals(ATTRIBUTE_DELETED_AFTER_CATEGORY_CHANGE, response.get(4).getNewValue());
  }

  @Test
  public void getProductDiff_NothingChanged() throws Exception {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }

  @Test
  public void getProductDiff_NoneTypeDescriptiveAttributeChange() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    mockOldProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }

  @Test
  public void getProductDiff_NoneTypeDescriptiveAttributeChangeNewProduct() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductAttribute attr1 = new ProductAttribute();
    ProductAttributeValue val2 = new ProductAttributeValue();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValue predefinedVal = new PredefinedAllowedAttributeValue();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValue> valueList = new ArrayList<>();
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    attr1.setAttribute(new Attribute());
    mockOldProduct.getProductAttributes().add(attr1);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct,
        mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }

  @Test
  public void promoChangeTest() {
    mockOldProduct.setPromoSKU(true);
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), PROMO_SKU);
  }

  @Test
  public void categoryChangeTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    List<ProductCategory> productCategories = getProductCategories();
    productCategories.get(0).getCategory().setName("Name 1");
    mockOldProduct.setProductCategories(productCategories);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), CATEGORY);
  }

  @Test
  public void urlVideoChangeTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    mockNewProduct.setUrl("url");
    mockOldProduct.setUrl("url edited");
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), URL_VIDEO);
  }

  @Test
  public void colourFamilyTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Colour");
    Attribute attribute = new Attribute();
    attribute.setName("Family Colour");
    productItemAttributeValue.setAttribute(attribute);
    ProductItemAttributeValue newProductItemAttributeValue = new ProductItemAttributeValue();
    newProductItemAttributeValue.setAttribute(attribute);
    attribute.setName("Family Colour");
    newProductItemAttributeValue.setAttribute(attribute);
    newProductItemAttributeValue.setValue("Colour edited");
    mockNewProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(newProductItemAttributeValue));
    mockOldProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(productItemAttributeValue));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), "Product Item1 Family Colour");
  }

  @Test
  public void colourFamilyWithNewValueWhenExistingValueIsEmptyTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue(StringUtils.EMPTY);
    Attribute attribute = new Attribute();
    attribute.setName("Family Colour");
    productItemAttributeValue.setAttribute(attribute);
    ProductItemAttributeValue newProductItemAttributeValue = new ProductItemAttributeValue();
    newProductItemAttributeValue.setAttribute(attribute);
    attribute.setName("Family Colour");
    newProductItemAttributeValue.setAttribute(attribute);
    newProductItemAttributeValue.setValue("Red");
    mockNewProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(newProductItemAttributeValue));
    mockOldProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(productItemAttributeValue));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals("Red", histList.get(0).getNewValue());
  }

  @Test
  public void colourFamilyWithSameValueTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue("Red");
    Attribute attribute = new Attribute();
    attribute.setName("Family Colour");
    productItemAttributeValue.setAttribute(attribute);
    ProductItemAttributeValue newProductItemAttributeValue = new ProductItemAttributeValue();
    newProductItemAttributeValue.setAttribute(attribute);
    attribute.setName("Family Colour");
    newProductItemAttributeValue.setAttribute(attribute);
    newProductItemAttributeValue.setValue("Red");
    mockNewProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(newProductItemAttributeValue));
    mockOldProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(productItemAttributeValue));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }

  @Test
  public void imageChangeTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    productItemImage.setLocationPath("Old location path");
    mockOldProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ProductItemImage newProductItemImage = new ProductItemImage();
    newProductItemImage.setMainImages(true);
    newProductItemImage.setLocationPath("New location path");
    mockNewProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(newProductItemImage));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), "Product Item1 Item images added ");
  }

  @Test
  public void ItemImageChangeTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemImage productItemImage = new ProductItemImage();
    ProductItem productItem = new ProductItem();
    productItem.setGeneratedItemName(ITEM_NAME);
    productItemImage.setProductItem(productItem);
    productItemImage.setMainImages(false);
    productItemImage.setLocationPath("Old location path");
    mockOldProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ProductItemImage newProductItemImage = new ProductItemImage();
    newProductItemImage.setMainImages(false);
    newProductItemImage.setLocationPath("New location path");
    ProductItem productItem1 = new ProductItem();
    productItem.setGeneratedItemName(ITEM_NAME_2);
    productItemImage.setProductItem(productItem);
    mockNewProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(newProductItemImage));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), "Product Item1 Item images added ");
  }

  @Test
  public void ItemImageChangeWithMfdTrueImagesTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemImage productItemImage = new ProductItemImage();
    ProductItem productItem = new ProductItem();
    productItem.setGeneratedItemName(ITEM_NAME);
    productItemImage.setProductItem(productItem);
    productItemImage.setMainImages(false);
    productItemImage.setLocationPath("Old location path");
    productItemImage.setMarkForDelete(true);
    mockOldProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ProductItemImage newProductItemImage = new ProductItemImage();
    newProductItemImage.setMainImages(false);
    newProductItemImage.setLocationPath("New location path");
    newProductItemImage.setMarkForDelete(true);
    productItem.setGeneratedItemName(ITEM_NAME_2);
    productItemImage.setProductItem(productItem);
    mockNewProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(newProductItemImage));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
  }

  @Test
  public void ItemImageChangeTestWithLocationPathNotChanged() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemImage productItemImage = new ProductItemImage();
    ProductItem productItem = new ProductItem();
    productItem.setGeneratedItemName(ITEM_NAME);
    productItemImage.setProductItem(productItem);
    productItemImage.setMainImages(false);
    productItemImage.setLocationPath("Old location path");
    mockOldProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ProductItemImage newProductItemImage = new ProductItemImage();
    newProductItemImage.setMainImages(false);
    newProductItemImage.setLocationPath("Old location path");
    productItem.setGeneratedItemName(ITEM_NAME_2);
    productItemImage.setProductItem(productItem);
    mockNewProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(newProductItemImage));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    Assertions.assertNotNull(histList);
  }

  @Test
  public void imageChangeWithReviewPendingTrueTest() {
    mockOldProduct.setReviewPending(true);
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setMainImages(true);
    productItemImage.setLocationPath("Old location path");
    mockOldProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(productItemImage));
    ProductItemImage newProductItemImage = new ProductItemImage();
    newProductItemImage.setMainImages(true);
    newProductItemImage.setLocationPath("New location path");
    mockNewProduct.getProductItems().get(0).setProductItemImages(Arrays.asList(newProductItemImage));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertTrue(CollectionUtils.isEmpty(histList));
  }

  @Test
  public void colourFamilyNullTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
    productItemAttributeValue.setValue(NO_COLOR);
    Attribute attribute = new Attribute();
    attribute.setName(NON_FAMILY_COLOR);
    productItemAttributeValue.setAttribute(attribute);
    ProductItemAttributeValue newProductItemAttributeValue = new ProductItemAttributeValue();
    newProductItemAttributeValue.setAttribute(attribute);
    attribute.setName(NON_FAMILY_COLOR);
    newProductItemAttributeValue.setAttribute(attribute);
    newProductItemAttributeValue.setValue(NO_COLOR);
    mockNewProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(newProductItemAttributeValue));
    mockOldProduct.getProductItems().get(0).setProductItemAttributeValues(Arrays.asList(productItemAttributeValue));
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertTrue(histList.isEmpty());
  }

  @Test
  public void getProductDiff_BrandChange() throws Exception {
    mockNewProduct.setBrand(BRAND);
    mockNewProduct.getProductAttributes().get(0).setProductAttributeName("Brand");
    mockNewProduct.getProductAttributes().get(0).getAttribute().setName("Brand");
    mockOldProduct.getProductAttributes().get(0).setProductAttributeName("Brand");
    mockOldProduct.getProductAttributes().get(0).getAttribute().setName("Brand");
    PredefinedAllowedAttributeValue predefinedVal = new PredefinedAllowedAttributeValue();
    predefinedVal.setValue("<p>Samsung</p>");
    mockOldProduct.getProductAttributes().get(0).getProductAttributeValues().get(1)
        .setPredefinedAllowedAttributeValue(predefinedVal);
    mockNewProduct.setProductCategories(getProductCategories());
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(BRAND);
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(1)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    mockOldProduct.setProductCategories(getProductCategories());
    mockOldProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertTrue(histList.get(0).toString().equals(BRAND_CHANGE_HISTORY_NOTE));
  }

  @Test
  public void getProductDiff_WithBrandAndAttrValuesChange() throws Exception {
    mockNewProduct.getProductAttributes().get(0).setProductAttributeName("Brand");
    mockOldProduct.getProductAttributes().get(0).setProductAttributeName("Brand");
    PredefinedAllowedAttributeValue predefinedVal = new PredefinedAllowedAttributeValue();
    predefinedVal.setValue("<p>Samsung</p>");
    mockOldProduct.getProductAttributes().get(0).getProductAttributeValues().get(1)
        .setPredefinedAllowedAttributeValue(predefinedVal);
    mockNewProduct.setProductCategories(getProductCategories());
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(BRAND);
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(1)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    mockOldProduct.setProductCategories(getProductCategories());
    mockOldProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    mockNewProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
  }

  private Product getProduct() {
    Product oldProduct = new Product();
    oldProduct.setName("testProduct1");
    oldProduct.setBrand("Samsung");
    oldProduct.setDescription("<p>some description</p>".getBytes());
    oldProduct.setHeight(1.2D);
    oldProduct.setLength(1.2D);
    oldProduct.setWeight(3D);
    oldProduct.setWidth(1.2D);
    oldProduct.setProductStory("<p>product story ....</p>");
    oldProduct.setUniqueSellingPoint("<p>usp</p>");
    oldProduct.setProductAttributes(getProductAttributes());
    oldProduct.setProductItems(getProductItems());
    return oldProduct;
  }

  private List<ProductItem> getProductItems() {
    ProductItem item = new ProductItem();
    item.setGeneratedItemName("Product Item1");
    item.setId("123");
    item.setDangerousGoodsLevel(2);
    item.setUpcCode("abc");
    List<ProductItem> itemList = new ArrayList<>();
    itemList.add(item);
    return itemList;
  }

  private List<ProductAttribute> getProductAttributes() {
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    Attribute attribute = new Attribute();
    attribute.setName("product Attr1");
    ProductAttribute attr1 = new ProductAttribute();
    attr1.setProductAttributeName("product Attr1");
    attr1.setId("id1");
    attr1.setAttribute(attribute);
    ProductAttributeValue val1 = new ProductAttributeValue();
    val1.setId("123");
    val1.setDescriptiveAttributeValue("test");
    val1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);

    ProductAttributeValue val2 = new ProductAttributeValue();
    val2.setId("1234");
    val2.setDescriptiveAttributeValue("<p>test123</p>");
    PredefinedAllowedAttributeValue predefinedVal = new PredefinedAllowedAttributeValue();
    predefinedVal.setValue("<p>predefinedVal1</p>");
    val2.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    val2.setPredefinedAllowedAttributeValue(predefinedVal);
    List<ProductAttributeValue> valueList = new ArrayList<>();
    valueList.add(val1);
    valueList.add(val2);
    attr1.setProductAttributeValues(valueList);
    productAttributeList.add(attr1);
    return productAttributeList;
  }

  private List<ProductCategory> getProductCategories() {
    List<ProductCategory> productCategories = new ArrayList<>();
    ProductCategory productCategory = new ProductCategory();
    productCategory.setId(ID);
    productCategory.setCategory(new Category());
    productCategory.getCategory().setName("Name");
    productCategories.add(productCategory);
    return productCategories;
  }

  @Test
  public void getCategoryIdsTest() throws Exception {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    Set<String> response = mockProductChangeUtil.getCategoryIds(mockNewProduct, mockOldProduct);
    assertTrue(CollectionUtils.isNotEmpty(response));
    assertEquals(1, response.size());
  }

  @Test
  public void getProductDiff_vatApplicableTest() {
    mockNewProduct.setProductCategories(getProductCategories());
    mockOldProduct.setProductCategories(getProductCategories());
    mockNewProduct.getProductItems().get(0).setVatApplicable(Boolean.TRUE);
    mockOldProduct.getProductItems().get(0).setVatApplicable(Boolean.FALSE);
    List<ProductFieldHistory> histList = mockProductChangeUtil.getProductDiff(mockNewProduct, mockOldProduct);
    assertNotNull(histList);
    assertEquals(histList.get(0).getFieldName(), ITEM_NAME + StringUtils.SPACE + VAT_APPLICABLE);
  }

  @Test
  public void isDimensionFieldsChangedTest() {
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory("Name", "Name", "Name");
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory("Panjang", "10", "15");

    boolean isDimensionChanges = mockProductChangeUtil.isDimensionFieldsChanged(Arrays.asList(productFieldHistory1, productFieldHistory2));

    Assertions.assertTrue(isDimensionChanges);
  }

  @Test
  public void isDimensionFieldsChangedGoodLevelTest() {
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory("Name", "Name", "Name");
    ProductFieldHistory productFieldHistory2 = new ProductFieldHistory("Dangerous Good Level", "1", "2");

    boolean isDimensionChanges = mockProductChangeUtil.isDimensionFieldsChanged(Arrays.asList(productFieldHistory1, productFieldHistory2));

    Assertions.assertTrue(isDimensionChanges);
  }

  @Test
  public void isDimensionFieldsChangedNoUpdateTest() {
    ProductFieldHistory productFieldHistory1 = new ProductFieldHistory("Name", "Name", "Name");

    boolean isDimensionChanges = mockProductChangeUtil.isDimensionFieldsChanged(Arrays.asList(productFieldHistory1));

    Assertions.assertFalse(isDimensionChanges);
  }

}



