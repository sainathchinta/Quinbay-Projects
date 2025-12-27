package com.gdn.x.productcategorybase.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;
import org.springframework.util.CollectionUtils;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public class AggregateUtilTest {

  private AggregateUtil aggregateUtil = new AggregateUtil();

  /*Product*/
  private static final String NAME = "NAME";
  private static final String PRODUCT_CODE = "PRODUCT CODE";
  private static final Double LENGTH = 1.0;
  private static final Double WIDTH = 2.0;
  private static final Double WEIGHT = 3.0;
  private static final Double HEIGHT = 4.0;
  private static final Double SHIPPING_WEIGHT = 5.0;
  private static final boolean PRODUCT_MARK_FOR_DELETE = false;
  private static final String ID = "ID";
  private static final String STORE_ID = "STORE ID";
  private static final byte[] DESCRIPTION = "DESCRIPTION".getBytes();
  private static final String SPECIFICATION_DETAIL = "SPECIFICATION DETAIL";
  private static final String PRODUCT_STORY = "PRODUCT STORY";
  private static final String BRAND = "BRAND";
  private static final String UNIQUE_SELLING_POINT = "UNIQUE SELLING POINT";
  private static final String UOM = "UOM";
  private static final boolean ACTIVATED = true;
  private static final boolean VIEWABLE = true;
  private static final String URL = "URL";

  /*Catalog*/
  private static final String CATALOG_CODE = "CATALOG CODE";
  private static final CatalogType CATALOG_TYPE = CatalogType.MASTER_CATALOG;

  /*Category*/
  private static final String CATEGORY_CODE = "CATEGORY CODE";
  private static final Integer SEQUENCE = 1;
  private static final boolean DISPLAY = true;
  private static final Integer LOGISTIC_ADJUSTMENT = 1;
  private static final boolean WARANTY = false;
  private static final boolean NEED_IDENTITY = false;
  private static final String PARENT_ID = "PARENT ID";

  /*ProductAttribute*/
  private static final boolean OWN_BY_PRODUCT_ITEM = true;

  /*Attribute*/
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE CODE";
  private static final AttributeType ATTRIBUTE_TYPE = AttributeType.PREDEFINED_ATTRIBUTE;
  private static final boolean SKU_VALUE = true;

  /*ProductAttributeValue*/
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "DESCRIPTIVE ATTRIBUTE VALUE";
  private static final DescriptiveAttributeValueType DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE = DescriptiveAttributeValueType.PREDEFINED;

  /*AllowedAttributeValue*/
  private static final String ALLOWED_ATTRIBUTE_CODE = "ALLOWED ATTRIBUTE CODE";

  /*PredefinedAllowedAttributeValue*/
  private static final String PREDIFINED_ALLOWED_ATTRIBUTE_CODE = "PREDIFINED_ALLOWED_ATTRIBUTE_CODE";
  private static final String VALUE = "VALUE";

  /*ProductImage*/
  private static final boolean MAIN_IMAGE = true;
  private static final String LOCATION_PATH = "LOCATION PATH";

  /*ProductItem*/
  private static final String UPC_CODE = "UPC CODE";
  private static final String SKU_CODE = "SKU CODE";
  private static final Integer DANGEROUS_GOODS_LEVEL = 1;

  @BeforeEach
  public void setup(){
    MockitoAnnotations.initMocks(this);
  }

  private static <T> List<T> toList(T ...object) {
    List<T> result = new ArrayList<>();
    for(T o : object) {
      result.add(o);
    }
    return result;
  }

  @Test
  public void testToProductDomainEventModel(){
    Product product = initProduct();
    ProductDomainEventModel result = aggregateUtil.toProductDomainEventModel(product);
    asssertToProductDomainEventModel(result);
  }

  @Test
  public void testToProductDomainEventModelError(){
    Product product = spy(initProduct());
    when(product.getDescription()).thenThrow(RuntimeException.class);
    ProductDomainEventModel result = aggregateUtil.toProductDomainEventModel(product);
    verify(product).getDescription();
    assertNull(result);
  }

  @Test
  public void testToAggregateProductCategoryDomainEventModel(){
    ProductCategory productCategory = initProductCategory();
    AggregateProductCategoryDomainEventModel result = aggregateUtil.toAggregateProductCategoryDomainEventModel(toList(productCategory));
    assertToAggregateProductCategoryDomainEventModel(result);
  }

  @Test
  public void testToAggregateProductCategoryDomainEventModelError(){
    ProductCategory productCategory = spy(initProductCategory());
    when(productCategory.getProduct()).thenThrow(RuntimeException.class);
    AggregateProductCategoryDomainEventModel result = aggregateUtil.toAggregateProductCategoryDomainEventModel(toList(productCategory));
    verify(productCategory).getProduct();
    assertNull(result);
  }

  @Test
  public void testToAggregateProductCategoryDomainEventModelError_createProductCategoryDomainEventModel(){
    ProductCategory productCategory = initProductCategory();
    Category category = spy(initCategory());
    productCategory.setCategory(category);
    when(category.getDescription()).thenThrow(RuntimeException.class);
    AggregateProductCategoryDomainEventModel result = aggregateUtil.toAggregateProductCategoryDomainEventModel(toList(productCategory));
    verify(category).getDescription();
    assertNull(result.getProductCategories().iterator().next().getCategory());
  }

  @Test
  public void testToAggregateProductCategoryDomainEventModelError_createCatalogDomainEventModel(){
    ProductCategory productCategory = initProductCategory();
    Catalog catalog = spy(initCatalog());
    Category category = initCategory();
    category.setCatalog(catalog);
    productCategory.setCategory(category);
    when(catalog.getName()).thenThrow(RuntimeException.class);
    AggregateProductCategoryDomainEventModel result = aggregateUtil.toAggregateProductCategoryDomainEventModel(toList(productCategory));
    verify(catalog).getName();
    assertNull(result.getProductCategories().iterator().next().getCategory().getCatalog());
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModel(){
    ProductAttribute productAttribute = initProductAttribute();
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    assertToAggregateProductAttributeDomainEventModel(result);
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError(){
    ProductAttribute productAttribute = spy(initProductAttribute());
    when(productAttribute.getProduct()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(productAttribute).getProduct();
    assertNull(result);
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_createProductAttributeDomainEventModel(){
    ProductAttribute productAttribute = spy(initProductAttribute());
    when(productAttribute.getAttribute()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(productAttribute).getAttribute();
    assertTrue(CollectionUtils.isEmpty(result.getProductAttributes()));
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_createAttributeDomainEventModel(){
    ProductAttribute productAttribute = initProductAttribute();
    Attribute attribute = spy(initAttribute());
    productAttribute.setAttribute(attribute);
    when(attribute.getName()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(attribute).getName();
    assertNull(result.getProductAttributes().iterator().next().getAttribute());
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_createProductAttributeValueDomainEventModels(){
    ProductAttribute productAttribute = initProductAttribute();
    List<ProductAttributeValue> productAttributeValues = spy(initProductAttributeValues());
    productAttribute.setProductAttributeValues(productAttributeValues);
    doThrow(RuntimeException.class).when(productAttributeValues).stream();
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(productAttributeValues).stream();
    assertEquals(result.getProductAttributes().iterator().next().getProductAttributeValues().size(),0);
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_toProductAttributeValueDomainEventModel(){
    ProductAttribute productAttribute = initProductAttribute();
    ProductAttributeValue productAttributeValue = spy(initProductAttributeValue());
    List<ProductAttributeValue> productAttributeValues = Arrays.asList(productAttributeValue);
    productAttribute.setProductAttributeValues(productAttributeValues);
    when(productAttributeValue.getAllowedAttributeValue()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(productAttributeValue).getAllowedAttributeValue();
    assertEquals(result.getProductAttributes().iterator().next().getProductAttributeValues().size(), 0);
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_createAllowedAttributeValueDomainEventModel(){
    ProductAttribute productAttribute = initProductAttribute();
    AllowedAttributeValue allowedAttributeValue = spy(initAllowedAttributeValue());
    ProductAttributeValue productAttributeValue = initProductAttributeValue();
    productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
    List<ProductAttributeValue> productAttributeValues = Arrays.asList(productAttributeValue);
    productAttribute.setProductAttributeValues(productAttributeValues);
    when(allowedAttributeValue.getAllowedAttributeCode()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(allowedAttributeValue).getAllowedAttributeCode();
    assertNull(result.getProductAttributes().iterator().next().getProductAttributeValues().iterator().next().getAllowedAttributeValue());
  }

  @Test
  public void testToAggregateProductAttributeDomainEventModelError_createPredefinedAllowedAttributeValueDomainEventModel(){
    ProductAttribute productAttribute = initProductAttribute();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = spy(initPredefinedAllowedAttributeValue());
    ProductAttributeValue productAttributeValue = initProductAttributeValue();
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    List<ProductAttributeValue> productAttributeValues = Arrays.asList(productAttributeValue);
    productAttribute.setProductAttributeValues(productAttributeValues);
    when(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode()).thenThrow(RuntimeException.class);
    AggregateProductAttributeDomainEventModel result = aggregateUtil.toAggregateProductAttributeDomainEventModel(toList(productAttribute));
    verify(predefinedAllowedAttributeValue).getPredefinedAllowedAttributeCode();
    assertNull(result.getProductAttributes().iterator().next().getProductAttributeValues().iterator().next().getPredefinedAllowedAttributeValue());
  }

  @Test
  public void testToAggregateImageDomainEventModel(){
    ProductImage productImage = initProductImage();
    AggregateImageDomainEventModel result = aggregateUtil.toAggregateImageDomainEventModel(toList(productImage));
    assertToAggregateImageDomainEventModel(result);
  }

  @Test
  public void testToAggregateImageDomainEventModelError(){
    ProductImage productImage = spy(initProductImage());
    when(productImage.getProduct()).thenThrow(RuntimeException.class);
    AggregateImageDomainEventModel result = aggregateUtil.toAggregateImageDomainEventModel(toList(productImage));
    verify(productImage).getProduct();
    assertNull(result);
  }

  @Test
  public void testToAggregateImageDomainEventModelError_createImageDomainEventModel(){
    ProductImage productImage = spy(initProductImage());
    when(productImage.getLocationPath()).thenThrow(RuntimeException.class);
    AggregateImageDomainEventModel result = aggregateUtil.toAggregateImageDomainEventModel(toList(productImage));
    verify(productImage).getLocationPath();
    assertTrue(CollectionUtils.isEmpty(result.getImages()));
  }

  @Test
  public void testToAggregateProductItemDomainEventModel(){
    ProductItem productItem = initProductItem();
    AggregateProductItemDomainEventModel result = aggregateUtil.toAggregateProductItemDomainEventModel(toList(productItem));
    assertToAggregateProductItemDomainEventModel(result);
  }

  @Test
  public void testToAggregateProductItemDomainEventModelError(){
    ProductItem productItem = spy(initProductItem());
    when(productItem.getProduct()).thenThrow(RuntimeException.class);
    AggregateProductItemDomainEventModel result = aggregateUtil.toAggregateProductItemDomainEventModel(toList(productItem));
    verify(productItem).getProduct();
    assertNull(result);
  }

  @Test
  public void testToAggregateProductItemDomainEventModelError_createProductItemDomainEventModel(){
    ProductItem productItem = spy(initProductItem());
    when(productItem.getGeneratedItemName()).thenThrow(RuntimeException.class);
    AggregateProductItemDomainEventModel result = aggregateUtil.toAggregateProductItemDomainEventModel(toList(productItem));
    verify(productItem).getGeneratedItemName();
    assertTrue(CollectionUtils.isEmpty(result.getProductItems()));
  }

  @Test
  public void testToAggregateProductItemDomainEventModelError_toImageDomainEventModel(){
    ProductItem productItem = initProductItem();
    List<ProductItemImage> productItemImages = spy(initProductItemImages());
    productItem.setProductItemImages(productItemImages);
    doThrow(RuntimeException.class).when(productItemImages).stream();
    AggregateProductItemDomainEventModel result = aggregateUtil.toAggregateProductItemDomainEventModel(toList(productItem));
    verify(productItemImages).stream();
    assertEquals(result.getProductItems().iterator().next().getImages().size(),0);
  }

  @Test
  public void testToAggregateProductItemDomainEventModelError_createImageDomainEventModel(){
    ProductItem productItem = initProductItem();
    ProductItemImage productItemImage = spy(initProductItemImage());
    List<ProductItemImage> productItemImages = Arrays.asList(productItemImage);
    productItem.setProductItemImages(productItemImages);
    when(productItemImage.getLocationPath()).thenThrow(RuntimeException.class);
    AggregateProductItemDomainEventModel result = aggregateUtil.toAggregateProductItemDomainEventModel(toList(productItem));
    verify(productItemImage).getLocationPath();
    assertEquals(result.getProductItems().iterator().next().getImages().size(),0);
  }

  private Product initProduct(){
    Product result = new Product();
    result.setName(NAME);
    result.setProductCode(PRODUCT_CODE);
    result.setLength(LENGTH);
    result.setWidth(WIDTH);
    result.setWeight(WEIGHT);
    result.setHeight(HEIGHT);
    result.setShippingWeight(SHIPPING_WEIGHT);
    result.setMarkForDelete(PRODUCT_MARK_FOR_DELETE);
    result.setId(ID);
    result.setStoreId(STORE_ID);
    result.setDescription(DESCRIPTION);
    result.setSpecificationDetail(SPECIFICATION_DETAIL);
    result.setProductStory(PRODUCT_STORY);
    result.setBrand(BRAND);
    result.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    result.setUom(UOM);
    result.setActivated(ACTIVATED);
    result.setViewable(VIEWABLE);
    result.setUrl(URL);

    return result;
  }

  private void asssertToProductDomainEventModel(ProductDomainEventModel result){
    Assertions.assertEquals(NAME,result.getName());
    Assertions.assertEquals(PRODUCT_CODE,result.getProductCode());
    Assertions.assertEquals(LENGTH,result.getLength());
    Assertions.assertEquals(WIDTH,result.getWidth());
    Assertions.assertEquals(WEIGHT,result.getWeight());
    Assertions.assertEquals(HEIGHT,result.getHeight());
    Assertions.assertEquals(SHIPPING_WEIGHT,result.getShippingWeight());
    Assertions.assertEquals(PRODUCT_MARK_FOR_DELETE,result.isproductMarkForDelete());
    Assertions.assertEquals(ID,result.getId());
    Assertions.assertEquals(STORE_ID,result.getStoreId());
    Assertions.assertEquals(DESCRIPTION,result.getDescription());
    Assertions.assertEquals(SPECIFICATION_DETAIL,result.getSpecificationDetail());
    Assertions.assertEquals(PRODUCT_STORY,result.getProductStory());
    Assertions.assertEquals(BRAND,result.getBrand());
    Assertions.assertEquals(UNIQUE_SELLING_POINT,result.getUniqueSellingPoint());
    Assertions.assertEquals(UOM,result.getUom());
    Assertions.assertEquals(ACTIVATED,result.isActivated());
    Assertions.assertEquals(VIEWABLE,result.isViewable());
    Assertions.assertEquals(URL,result.getUrl());
  }

  private ProductCategory initProductCategory(){
    ProductCategory result = new ProductCategory();
    result.setProduct(initProduct());
    result.setCategory(initCategory());

    return result;
  }

  private void assertToAggregateProductCategoryDomainEventModel(AggregateProductCategoryDomainEventModel result){
    Assertions.assertEquals(PRODUCT_CODE,result.getProductCode());
    assertToProductCategoryDomainEventModel(result.getProductCategories().iterator().next());
  }

  private void assertToProductCategoryDomainEventModel(ProductCategoryDomainEventModel result){
    assertToCategoryDomainEventModel(result.getCategory());
  }

  private Category initCategory(){
    Category result = new Category();
    result.setId(ID);
    result.setName(NAME);
    result.setCategoryCode(CATEGORY_CODE);
    result.setSequence(SEQUENCE);
    result.setDescription(DESCRIPTION);
    result.setDisplay(DISPLAY);
    result.setLogisticAdjustment(LOGISTIC_ADJUSTMENT);
    result.setWarranty(WARANTY);
    result.setNeedIdentity(NEED_IDENTITY);
    result.setActivated(ACTIVATED);
    result.setViewable(VIEWABLE);
    result.setCatalog(initCatalog());
    Category parent = new Category();
    parent.setId(PARENT_ID);
    result.setParentCategory(parent);

    return result;
  }

  private void assertToCategoryDomainEventModel(CategoryDomainEventModel result){
    Assertions.assertEquals(ID,result.getId());
    Assertions.assertEquals(NAME,result.getName());
    Assertions.assertEquals(CATEGORY_CODE,result.getCategoryCode());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
    Assertions.assertEquals(new String(DESCRIPTION),new String(result.getDescription()));
    Assertions.assertEquals(DISPLAY,result.isDisplay());
    Assertions.assertEquals(LOGISTIC_ADJUSTMENT,result.getLogisticAdjustment());
    Assertions.assertEquals(WARANTY,result.isWarranty());
    Assertions.assertEquals(NEED_IDENTITY,result.isNeedIdentity());
    Assertions.assertEquals(ACTIVATED,result.isActivated());
    Assertions.assertEquals(VIEWABLE,result.isViewable());
    assertToCatalogDomainEventModel(result.getCatalog());
    Assertions.assertEquals(PARENT_ID,result.getParentCategoryId());
  }

  private Catalog initCatalog(){
    Catalog result = new Catalog();
    result.setName(NAME);
    result.setCatalogCode(CATALOG_CODE);
    result.setCatalogType(CATALOG_TYPE);

    return result;
  }

  private void assertToCatalogDomainEventModel(CatalogDomainEventModel result){
    Assertions.assertEquals(NAME,result.getName());
    Assertions.assertEquals(CATALOG_CODE,result.getCatalogCode());
    Assertions.assertEquals(CATALOG_TYPE.toString(),result.getCatalogType());
  }

  private ProductAttribute initProductAttribute(){
    ProductAttribute result = new ProductAttribute();
    result.setProduct(initProduct());
    result.setAttribute(initAttribute());
    result.setProductAttributeName(NAME);
    result.setOwnByProductItem(OWN_BY_PRODUCT_ITEM);
    result.setSequence(SEQUENCE);
    result.setProductAttributeValues(initProductAttributeValues());

    return result;
  }

  private void assertToAggregateProductAttributeDomainEventModel(AggregateProductAttributeDomainEventModel result){
    Assertions.assertEquals(PRODUCT_CODE,result.getProductCode());
    assertToProductAttributeDomainEventModel(result.getProductAttributes().iterator().next());
  }

  private void assertToProductAttributeDomainEventModel(ProductAttributeDomainEventModel result){
    assertToAttributeDomainEventModel(result.getAttribute());
    Assertions.assertEquals(NAME,result.getProductAttributeName());
    Assertions.assertEquals(OWN_BY_PRODUCT_ITEM,result.isOwnByProductItem());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
    assertToProductAttributeValueDomainEventModels(result.getProductAttributeValues());
  }

  private Attribute initAttribute(){
    Attribute result = new Attribute();
    result.setName(NAME);
    result.setAttributeCode(ATTRIBUTE_CODE);
    result.setAttributeType(ATTRIBUTE_TYPE);
    result.setDescription(DESCRIPTION);
    result.setSkuValue(SKU_VALUE);

    return result;
  }

  private void assertToAttributeDomainEventModel(AttributeDomainEventModel result){
    Assertions.assertEquals(NAME,result.getName());
    Assertions.assertEquals(ATTRIBUTE_CODE,result.getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_TYPE.toString(),result.getAttributeType());
    Assertions.assertEquals(new String(DESCRIPTION),new String(result.getDescription()));
    Assertions.assertEquals(SKU_VALUE,result.isSkuValue());
  }

  private List<ProductAttributeValue> initProductAttributeValues(){
    List<ProductAttributeValue> result = new ArrayList<>();
    result.add(initProductAttributeValue());

    return result;
  }

  private void assertToProductAttributeValueDomainEventModels(List<ProductAttributeValueDomainEventModel> result){
    result.stream().forEach(val -> assertToProductAttributeValueDomainEventModel(val));
  }

  private ProductAttributeValue initProductAttributeValue(){
    ProductAttributeValue result = new ProductAttributeValue();
    result.setAllowedAttributeValue(initAllowedAttributeValue());
    result.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    result.setDescriptiveAttributeValueType(DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE);
    result.setPredefinedAllowedAttributeValue(initPredefinedAllowedAttributeValue());

    return result;
  }

  private void assertToProductAttributeValueDomainEventModel(ProductAttributeValueDomainEventModel result){
    assertToAllowedAttributeValueDomainEventModel(result.getAllowedAttributeValue());
    Assertions.assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE,result.getDescriptiveAttributeValue());
    Assertions.assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE.toString(),result.getDescriptiveAttributeValueType());
    assertToPredefinedAllowedAttributeValueDomainEventModel(result.getPredefinedAllowedAttributeValue());
  }

  private AllowedAttributeValue initAllowedAttributeValue(){
    AllowedAttributeValue result = new AllowedAttributeValue();
    result.setAttribute(initAttribute());
    result.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    result.setSequence(SEQUENCE);
    result.setValue(VALUE);

    return result;
  }

  private void assertToAllowedAttributeValueDomainEventModel(AllowedAttributeValueDomainEventModel result){
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,result.getAllowedAttributeCode());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
    Assertions.assertEquals(VALUE,result.getValue());
  }

  private PredefinedAllowedAttributeValue initPredefinedAllowedAttributeValue(){
    PredefinedAllowedAttributeValue result = new PredefinedAllowedAttributeValue();
    result.setAttribute(initAttribute());
    result.setPredefinedAllowedAttributeCode(PREDIFINED_ALLOWED_ATTRIBUTE_CODE);
    result.setSequence(SEQUENCE);
    result.setValue(VALUE);

    return result;
  }

  private void assertToPredefinedAllowedAttributeValueDomainEventModel(PredefinedAllowedAttributeValueDomainEventModel result){
    Assertions.assertEquals(PREDIFINED_ALLOWED_ATTRIBUTE_CODE,result.getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
    Assertions.assertEquals(VALUE,result.getValue());
  }

  private ProductImage initProductImage(){
    ProductImage result = new ProductImage();
    result.setProduct(initProduct());
    result.setMainImages(MAIN_IMAGE);
    result.setLocationPath(LOCATION_PATH);
    result.setSequence(SEQUENCE);

    return result;
  }

  private void assertToAggregateImageDomainEventModel(AggregateImageDomainEventModel result){
    Assertions.assertEquals(PRODUCT_CODE,result.getProductCode());
    asssertToImageDomainEventModel(result.getImages().iterator().next());
  }

  private void asssertToImageDomainEventModel(ImageDomainEventModel result){
    Assertions.assertEquals(MAIN_IMAGE,result.isMainImage());
    Assertions.assertEquals(LOCATION_PATH,result.getLocationPath());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
  }

  private ProductItem initProductItem(){
    ProductItem result = new ProductItem();
    result.setProduct(initProduct());
    result.setGeneratedItemName(NAME);
    result.setUpcCode(UPC_CODE);
    result.setSkuCode(SKU_CODE);
    result.setActivated(ACTIVATED);
    result.setViewable(VIEWABLE);
    result.setProductItemImages(initProductItemImages());
    result.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);

    return result;
  }

  private void assertToAggregateProductItemDomainEventModel(AggregateProductItemDomainEventModel result){
    Assertions.assertEquals(PRODUCT_CODE,result.getProductCode());
    assertToProductItemDomainEventModel(result.getProductItems().iterator().next());
  }

  private void assertToProductItemDomainEventModel(ProductItemDomainEventModel result){
    Assertions.assertEquals(NAME,result.getGeneratedItemName());
    Assertions.assertEquals(UPC_CODE,result.getUpcCode());
    Assertions.assertEquals(SKU_CODE,result.getSkuCode());
    Assertions.assertEquals(ACTIVATED,result.isActivated());
    Assertions.assertEquals(VIEWABLE,result.isViewable());
    assertToImageDomainEventModels(result.getImages());
    Assertions.assertEquals(DANGEROUS_GOODS_LEVEL,result.getDangerousGoodsLevel());
  }

  private List<ProductItemImage> initProductItemImages(){
    List<ProductItemImage> result = new ArrayList<>();
    result.add(initProductItemImage());

    return result;
  }

  private void assertToImageDomainEventModels(List<ImageDomainEventModel> result){
    result.stream().forEach(val -> assertToImageDomainEventModel(val));
  }

  private ProductItemImage initProductItemImage(){
    ProductItemImage result = new ProductItemImage();
    result.setMainImages(MAIN_IMAGE);
    result.setLocationPath(LOCATION_PATH);
    result.setSequence(SEQUENCE);

    return result;
  }

  private void assertToImageDomainEventModel(ImageDomainEventModel result){
    Assertions.assertEquals(MAIN_IMAGE,result.isMainImage());
    Assertions.assertEquals(LOCATION_PATH,result.getLocationPath());
    Assertions.assertEquals(SEQUENCE,result.getSequence());
  }

}
