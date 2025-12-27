package com.gdn.mta.product.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import com.gdn.mta.product.entity.MergeStatus;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

/**
 * Created by Kesha on 05/09/16.
 */

public class MergeProductsUtilityTest {

  private MergeProductsUtility mergeProductsUtility;

  @Mock
  private Product mockMasterDataProduct;

  @Mock
  private Product mockDuplicateProduct;

  @BeforeEach
  public void setUp() throws Exception {
    mockMasterDataProduct=getMockProduct(false);
    mockDuplicateProduct=getMockProduct(false);
    mockMasterDataProduct.setId("id1");
    mockDuplicateProduct.setId("id2");
    mergeProductsUtility=new MergeProductsUtility(mockMasterDataProduct,mockDuplicateProduct);
  }

  @Test
  public void merge_whenSkuTrueAttribute_IgnoreValidation_success() throws Exception {
    mockDuplicateProduct.getProductAttributes().get(0).getAttribute().setSkuValue(true);
    mockMasterDataProduct.getProductAttributes().get(0).getAttribute().setSkuValue(true);
    mockDuplicateProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("different value");
    MergeStatus mergeStatus=mergeProductsUtility.merge(false);
    assertTrue(mergeStatus.equals(MergeStatus.NO_UPDATE));
  }

  @Test
  public void merge_whenSkuFalseAttribute_ValueDontMatch_fail() throws Exception {
    mockDuplicateProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("different value");
    MergeStatus mergeStatus=mergeProductsUtility.merge(false);
    assertTrue(mergeStatus.equals(MergeStatus.NO_UPDATE));
  }

  @Test
  public void merge_AttrExistInDuplicate_NotInMaster_Fail() throws Exception {
    Attribute attribute = new Attribute();
    attribute.setName("sku value true attribute new");
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setSkuValue(false);
    attribute.setId("attr2-id-random");
    ProductAttributeValue value = new ProductAttributeValue();
    value.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase
        .DescriptiveAttributeValueType.SINGLE);
    value.setDescriptiveAttributeValue("test- different-random");
    value.setId("prod-att-val2-id-random");
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    productAttribute.getProductAttributeValues().add(value);
    productAttribute.setId("prod-attr-id2-random");
    mockDuplicateProduct.getProductAttributes().add(productAttribute);
    MergeStatus mergeStatus=mergeProductsUtility.merge(false);
    assertTrue(mergeStatus.equals(MergeStatus.INVALID));
  }

  @Test
  public void merge_ForceUpdate_Non_Defining_AttributeSame_valueDiff_Pass() throws Exception {
    mockDuplicateProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("different value");
    MergeStatus mergeStatus=mergeProductsUtility.merge(true);
    assertTrue(mergeStatus.equals(MergeStatus.NO_UPDATE));
  }

  @Test
  public void merge_DefiningAttr_Diff_updateMasterData() throws Exception {
    ProductAttributeValue defValue1 = new ProductAttributeValue();
    defValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    defValue1.setId("prod-att-val3");
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId("allowed-att-val3");
    allowedAttributeValue.setValue("def value two");
    defValue1.setAllowedAttributeValue(allowedAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setActivated(true);
    productItem.setDangerousGoodsLevel(0);
    productItem.setProductItemImages(new ArrayList<>());
    productItem.setSkuCode("123");
    productItem.setUpcCode("123");
    productItem.setProduct(mockDuplicateProduct);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem);
    productItems.add(productItem);

    List<ProductItemImage> defaultProductImages = new ArrayList<>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setStoreId("10001");
    productItemImage.setLocationPath("/434/magic-toys_test1_full01.jpg");
    productItemImage.setMainImages(true);
    productItemImage.setSequence(123);
    defaultProductImages.add(productItemImage);
    productItem.setProductItemImages(defaultProductImages);

    List<ProductImage> productImages = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath("/343/magic.jpg");
    productImage.setMarkForDelete(false);
    productImages.add(productImage);
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath("/343/magic.jpg");
    productImage1.setMarkForDelete(true);
    productImages.add(productImage1);
    mockMasterDataProduct.setProductImages(productImages);
    mockDuplicateProduct.setProductItems(productItems);
    mockDuplicateProduct.getProductAttributes().get(1).getProductAttributeValues().add(defValue1);
    defValue1.setProductAttribute(mockDuplicateProduct.getProductAttributes().get(1));
    mockDuplicateProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("different value");
    MergeStatus mergeStatus=mergeProductsUtility.merge(false);
    assertTrue(mockMasterDataProduct.getProductItems()
        .contains(mockDuplicateProduct.getProductItems().get(0)));
    assertEquals(mockMasterDataProduct.getProductItems().get(0).getProductItemImages().get(0)
        .getLocationPath(), mockMasterDataProduct.getProductImages().get(0).getLocationPath());
    assertTrue(mergeStatus.equals(MergeStatus.MASTER_DATA_UPDATE));
  }

  @Test
  public void merge_ForceUpdate_DefiningAttr_Diff_noUpdateMasterData() throws Exception {
    ProductAttributeValue defValue1 = new ProductAttributeValue();
    defValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    defValue1.setId("prod-att-val3");
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId("allowed-att-val3");
    allowedAttributeValue.setValue("def value two");
    defValue1.setAllowedAttributeValue(allowedAttributeValue);
    ProductItem productItem = new ProductItem();
    productItem.setActivated(true);
    productItem.setDangerousGoodsLevel(0);
    productItem.setProductItemImages(new ArrayList<>());
    productItem.setSkuCode("123");
    productItem.setUpcCode("123");
    productItem.setProduct(mockDuplicateProduct);
    List<ProductItem> productItems = new ArrayList<>();
    productItems.add(productItem);
    productItems.add(productItem);

    List<ProductItemImage> defaultProductImages = new ArrayList<>();
    ProductItemImage productItemImage = new ProductItemImage();
    productItemImage.setStoreId("10001");
    productItemImage.setLocationPath("/434/magic-toys_test1_full01.jpg");
    productItemImage.setMainImages(true);
    productItemImage.setSequence(123);
    defaultProductImages.add(productItemImage);
    productItem.setProductItemImages(defaultProductImages);

    List<ProductImage> productImages = new ArrayList<>();
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath("/343/magic.jpg");
    productImages.add(productImage);
    mockMasterDataProduct.setProductImages(productImages);
    mockDuplicateProduct.setProductItems(productItems);
    mockDuplicateProduct.getProductAttributes().get(1).getProductAttributeValues().add(defValue1);
    defValue1.setProductAttribute(mockDuplicateProduct.getProductAttributes().get(1));
    mockDuplicateProduct.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .setDescriptiveAttributeValue("different value");
    MergeStatus mergeStatus=mergeProductsUtility.merge(true);
    assertTrue(mockMasterDataProduct.getProductItems()
        .contains(mockDuplicateProduct.getProductItems().get(0)));
    assertEquals(mockMasterDataProduct.getProductItems().get(0).getProductItemImages().get(0)
        .getLocationPath(), mockMasterDataProduct.getProductImages().get(0).getLocationPath());
    assertTrue(mergeStatus.equals(MergeStatus.NO_UPDATE));
  }


  private Product getMockProduct(boolean skuValue) {
    Product product = new Product();
    product.setBrand("test-brand");
    product.setId("123");
    product.setProductAttributes(new ArrayList<ProductAttribute>());
    Attribute attribute = new Attribute();
    attribute.setName("test");
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setSkuValue(skuValue);
    attribute.setId("attr1-id");
    ProductAttributeValue value = new ProductAttributeValue();
    value.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase
        .DescriptiveAttributeValueType.SINGLE);
    value.setDescriptiveAttributeValue("test");
    value.setId("prod-att-val1");
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    productAttribute.getProductAttributeValues().add(value);
    productAttribute.setId("prod-attr-id1");
    product.getProductAttributes().add(productAttribute);

    Attribute defAttribue = new Attribute();
    defAttribue.setName("test");
    defAttribue.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    defAttribue.setSkuValue(skuValue);
    defAttribue.setId("attr2-id");
    ProductAttributeValue defValue1 = new ProductAttributeValue();
    defValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    defValue1.setId("prod-att-val2");
    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setId("allowed-att-val1");
    allowedAttributeValue.setValue("def value one");
    defValue1.setAllowedAttributeValue(allowedAttributeValue);
    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setAttribute(defAttribue);
    productAttribute2.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
    productAttribute2.getProductAttributeValues().add(defValue1);
    productAttribute2.setId("prod-attr-id2");
    product.getProductAttributes().add(productAttribute2);
    defValue1.setProductAttribute(productAttribute2);
    product.getProductAttributes().add(productAttribute);
    value.setProductAttribute(productAttribute);
    return product;
  }

}
