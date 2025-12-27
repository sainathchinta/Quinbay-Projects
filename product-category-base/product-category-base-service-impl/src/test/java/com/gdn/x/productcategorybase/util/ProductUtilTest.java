package com.gdn.x.productcategorybase.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;

public class ProductUtilTest {
  private static final String CATEGORY_CODE_1 = "CATGEORY_CODE_1";
  private static final String CATEGORY_CODE_2 = "CATGEORY_CODE_2";
  private static final String NAME_1 = "PRODUCT_NAME_1";
  private static final String NAME_2 = "PRODUCT_NAME_2";
  private static final String BRAND_1 = "BRAND_1";
  private static final String BRAND_2 = "BRAND_2";
  private static final String USP_1 = "USP_1";
  private static final String USP_2 = "USP_2";
  private static final String DESCRIPTION_1 = "DESCRIPTION_1";
  private static final String DESCRIPTION_2 = "DESCRIPTION_2";
  private static final String LOCATION_PATH_1 = "LocationPath1";
  private static final String LOCATION_PATH_2 = "LocationPath2";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String ATTRIBUTE_CODE_3 = "attributeCode3";
  private static final String ATTRIBUTE_CODE_4 = "attributeCode4";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String ATTRIBUTE_NAME_2 = "attributeName2";
  private static final String ATTRIBUTE_VALUE = "attributeValue";

  private Product product1;
  private Product product2;
  private ProductCategory productCategory2;

  @BeforeEach
  public void setUp() {
    Category category1 = new Category();
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setCategoryCode(CATEGORY_CODE_2);

    ProductCategory productCategory1 = new ProductCategory(null, category1, null);
    productCategory2 = new ProductCategory(null, category2, null);

    product1 = new Product();
    product1.setName(NAME_1);
    product1.setBrand(BRAND_1);
    product1.setUniqueSellingPoint(USP_1);
    product1.setDescription(DESCRIPTION_1.getBytes());
    product1.setProductCategories(Arrays.asList(productCategory1));
    product1.setProductItems(Arrays.asList(new ProductItem()));

    product2 = new Product();
    product2.setName(NAME_1);
    product2.setBrand(BRAND_1);
    product2.setUniqueSellingPoint(USP_1);
    product2.setDescription(DESCRIPTION_1.getBytes());
    product2.setProductCategories(Arrays.asList(productCategory1));

  }

  @Test
  public void isProductDetailChangedCategoryTrueTest() {
    product2.setProductCategories(Arrays.asList(productCategory2));
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedNameTrueTest() {
    product2.setName(NAME_2);
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedBrandTrueTest() {
    product2.setBrand(BRAND_2);
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedUSPTrueTest() {
    product2.setUniqueSellingPoint(USP_2);
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedDescriptionTrueTest() {
    product2.setDescription(DESCRIPTION_2.getBytes());
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedNewDescriptionNullTest() {
    product1.setDescription(null);
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedOldDescriptionNullTest() {
    product2.setDescription(null);
    boolean result = ProductUtil.isProductDetailChanged(product1, product2);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductDetailChangedFalseTest() {
    boolean result = ProductUtil.isProductDetailChanged(product1, product1);
    Assertions.assertFalse(result);
  }

  private SimpleMasterProductUpdateDTO getSimpleMasterProductUpdateDTOTest() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setLength(1.0);
    simpleMasterProductUpdateDTO.setWidth(1.0);
    simpleMasterProductUpdateDTO.setHeight(1.0);
    simpleMasterProductUpdateDTO.setWeight(1.0);
    simpleMasterProductUpdateDTO.setShippingWeight(1.0);
    return simpleMasterProductUpdateDTO;
  }

  private void setDimentionsForProduct1() {
    product1.setLength(1.0);
    product1.setWidth(1.0);
    product1.setHeight(1.0);
    product1.setWeight(1.0);
    product1.setShippingWeight(1.0);
  }

  @Test
  public void isDimensionUpdatedLengthChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setLength(2.0);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }


  @Test
  public void isDimensionUpdatedWidthChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setWidth(2.0);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionUpdatedHeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setHeight(2.0);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionUpdatedWeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setWeight(2.0);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionUpdatedshippingWeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setShippingWeight(2.0);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionUpdatedLengthChangedNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setLength(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionUpdatedWidthChangedNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setWidth(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionUpdatedHeightChangedNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setHeight(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionUpdatedWeightChangedNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setWeight(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionUpdatedshippingWeightChangedNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setShippingWeight(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionUpdatedshippingWeightChangedBothNull() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    simpleMasterProductUpdateDTO.setShippingWeight(null);
    product1.setShippingWeight(null);
    boolean result = ProductUtil.isDimensionUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedLengthChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setLength(1.0);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedWidthChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setWidth(1.0);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedHeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setHeight(1.0);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedWeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setWeight(1.0);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedshippingWeightChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setShippingWeight(1.0);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void getSimpleMasterProductUpdateDTOFromProductTest() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = getSimpleMasterProductUpdateDTOTest();
    setDimentionsForProduct1();
    product1.setHeight(2.0);
    ProductUtil.getSimpleMasterProductUpdateDTOFromProduct(simpleMasterProductUpdateDTO, product1);
    Assertions.assertTrue(simpleMasterProductUpdateDTO.getHeight() == 2.0);
  }

  @Test
  public void isDimensionOrDgLevelUpdateDgLevelChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(1);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdateAllChanged() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setLength(1.0);
    simpleMasterProductUpdateDTO.setWidth(1.0);
    simpleMasterProductUpdateDTO.setHeight(1.0);
    simpleMasterProductUpdateDTO.setWeight(1.0);
    simpleMasterProductUpdateDTO.setShippingWeight(1.0);
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(1);
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdateNoChange() {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    boolean result = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);
    Assertions.assertFalse(result);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedLengthTest() {
    product1.setLength(1.0);
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setLength(1.0);
    boolean result1 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    simpleMasterProductUpdateDTO.setLength(null);
    boolean result2 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    simpleMasterProductUpdateDTO.setLength(2.0);
    boolean result3 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    Assertions.assertFalse(result1);
    Assertions.assertTrue(result2);
    Assertions.assertTrue(result3);
  }

  @Test
  public void isDimensionOrDgLevelUpdatedDgLevelTest() {
    product1.getProductItems().get(0).setDangerousGoodsLevel(0);
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(0);
    boolean result1 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(null);
    boolean result2 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    simpleMasterProductUpdateDTO.setDangerousGoodsLevel(1);
    boolean result3 = ProductUtil.isDimensionOrDgLevelUpdated(product1, simpleMasterProductUpdateDTO);

    Assertions.assertFalse(result1);
    Assertions.assertTrue(result2);
    Assertions.assertTrue(result3);
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesTest() {
    Product product = getProduct();

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);

    Assertions.assertTrue(product.getProductImages().get(0).isCommonImage());
    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertTrue(product.getProductItems().get(0).getProductItemImages().get(0).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
    Assertions.assertTrue(product.getProductItems().get(1).getProductItemImages().get(0).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesNullItemImageTest() {
    Product product = getProduct();
    product.setProductItems(new ArrayList<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);


    Assertions.assertFalse(product.getProductImages().get(0).isCommonImage());
    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
  }

  private Product getProduct() {
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
    productItem2.setProductItemImages(Arrays.asList(productItemImage1));

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2));
    product.setProductItems(Arrays.asList(productItem1, productItem2));

    return product;
  }

  @Test
  public void getProductAttributesEligibleForAutoFill() {
    Product product = new Product();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    product.setProductCategories(Arrays.asList(productCategory));
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    categoryAttribute.setAttribute(attribute);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProductAttributeValues(Arrays.asList(productAttributeValue));
    productAttribute.setAttribute(attribute);
    product.setProductAttributes(Arrays.asList(productAttribute));

    ProductUtil.getProductAttributesEligibleForAutoFill(product);

    productAttributeValue.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    ProductUtil.getProductAttributesEligibleForAutoFill(product);

    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    ProductUtil.getProductAttributesEligibleForAutoFill(product);

    productAttribute.setProductAttributeValues(null);
    ProductUtil.getProductAttributesEligibleForAutoFill(product);

    category.setExtractionType(ExtractionType.TEXT);
    ProductUtil.getProductAttributesEligibleForAutoFill(product);
  }

  @Test
  public void getCategoryNewAttributesEligibleForAutoFillTest() {
    Product product = new Product();
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setExtractionType(ExtractionType.INLINE_TEXT);
    productCategory.setCategory(category);
    product.setProductCategories(Arrays.asList(productCategory));
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setName(ATTRIBUTE_NAME);
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    categoryAttribute.setAttribute(attribute);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));

    ProductUtil.getCategoryNewAttributesEligibleForAutoFill(product);

    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    ProductUtil.getCategoryNewAttributesEligibleForAutoFill(product);

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    ProductUtil.getCategoryNewAttributesEligibleForAutoFill(product);

    category.setCategoryAttributes(new ArrayList<>());
    ProductUtil.getCategoryNewAttributesEligibleForAutoFill(product);

    category.setExtractionType(ExtractionType.TEXT);
    ProductUtil.getCategoryNewAttributesEligibleForAutoFill(product);
  }

  @Test
  public void extractAndSetValueTest() {
    Product product = new Product();
    product.setProductAttributes(new ArrayList<>());

    HashMap<String, String> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    Attribute attribute = new Attribute();
    attribute.setName(ATTRIBUTE_NAME);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(new ProductAttributeValue()));

    List<AttributeHistoryResponse>
        attributeHistoryResponseList = ProductUtil.extractAndSetValue(product, Arrays.asList(productAttribute), Arrays.asList(productAttribute), attributeValueAndNameMap);
    Assertions.assertEquals(2, attributeHistoryResponseList.size());

    attribute.setName(ATTRIBUTE_NAME_2);
    attributeHistoryResponseList = ProductUtil.extractAndSetValue(product, Arrays.asList(productAttribute), Arrays.asList(productAttribute), attributeValueAndNameMap);
    Assertions.assertEquals(0, attributeHistoryResponseList.size());
  }

  @Test
  public void updateExtractedAttributeValueDsAttributeNameMappingTest() {
    HashMap<String, String> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    Attribute attribute = new Attribute();
    attribute.setDsAttributeName(ATTRIBUTE_NAME);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(new ProductAttributeValue()));

    Assertions.assertTrue(ProductUtil.updateExtractedAttributeValue(productAttribute, attributeValueAndNameMap));
    Assertions.assertEquals(ATTRIBUTE_VALUE, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    Assertions.assertTrue(productAttribute.isExtractedValue());

    attribute.setDsAttributeName(ATTRIBUTE_NAME_2);
    Assertions.assertFalse(ProductUtil.updateExtractedAttributeValue(productAttribute, attributeValueAndNameMap));
  }

  @Test
  public void updateExtractedAttributeValueTest() {
    HashMap<String, String> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    Attribute attribute = new Attribute();
    attribute.setName(ATTRIBUTE_NAME);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    productAttribute.setProductAttributeValues(Arrays.asList(new ProductAttributeValue()));

    Assertions.assertTrue(ProductUtil.updateExtractedAttributeValue(productAttribute, attributeValueAndNameMap));
    Assertions.assertEquals(ATTRIBUTE_VALUE, productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    Assertions.assertTrue(productAttribute.isExtractedValue());

    attribute.setName(ATTRIBUTE_NAME_2);
    Assertions.assertFalse(ProductUtil.updateExtractedAttributeValue(productAttribute, attributeValueAndNameMap));
  }

  @Test
  public void isExtractedValueValidTest() {
    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);

    Assertions.assertTrue(ProductUtil.isExtractedValueValid(attributeValueAndNameMap.entrySet().iterator().next()));

    attributeValueAndNameMap.put(ATTRIBUTE_NAME, StringUtils.EMPTY);
    Assertions.assertFalse(ProductUtil.isExtractedValueValid(attributeValueAndNameMap.entrySet().iterator().next()));

    attributeValueAndNameMap.put(ATTRIBUTE_NAME, new HashMap<>());
    Assertions.assertFalse(ProductUtil.isExtractedValueValid(attributeValueAndNameMap.entrySet().iterator().next()));

    attributeValueAndNameMap.put(ATTRIBUTE_NAME, null);
    Assertions.assertFalse(ProductUtil.isExtractedValueValid(attributeValueAndNameMap.entrySet().iterator().next()));

    attributeValueAndNameMap.put(StringUtils.EMPTY, ATTRIBUTE_VALUE);
    Assertions.assertFalse(ProductUtil.isExtractedValueValid(attributeValueAndNameMap.entrySet().iterator().next()));
  }

  @Test
  public void isAttributeExtractionResponseValidTest() {
    HashMap<String, Object> attributeValueAndNameMap = new HashMap<>();
    attributeValueAndNameMap.put(ATTRIBUTE_NAME, ATTRIBUTE_VALUE);
    MatrixAttributeExtractionResponse value = new MatrixAttributeExtractionResponse();
    value.setExtractedAttributes(attributeValueAndNameMap);
    SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse =
        new SingleBaseResponse<>(true, null, null, value);
    Assertions.assertTrue(ProductUtil.isAttributeExtractionResponseValid(matrixAttributeExtractionResponse));

    value.setExtractedAttributes(null);
    Assertions.assertFalse(ProductUtil.isAttributeExtractionResponseValid(matrixAttributeExtractionResponse));

    matrixAttributeExtractionResponse.setValue(null);
    Assertions.assertFalse(ProductUtil.isAttributeExtractionResponseValid(matrixAttributeExtractionResponse));

    Assertions.assertFalse(ProductUtil.isAttributeExtractionResponseValid(null));
  }

  @Test
  public void isAttributeEligibleForAutoFillTest() {
    Attribute attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    Assertions.assertTrue(ProductUtil.isAttributeEligibleForAutoFill(attribute));

    attribute.setSkuValue(true);
    Assertions.assertFalse(ProductUtil.isAttributeEligibleForAutoFill(attribute));

    attribute.setVariantCreation(true);
    Assertions.assertFalse(ProductUtil.isAttributeEligibleForAutoFill(attribute));

    attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    Assertions.assertFalse(ProductUtil.isAttributeEligibleForAutoFill(attribute));
  }

  @Test
  public void isProductValueEmptyTest() {
    Assertions.assertFalse(ProductUtil.isProductValueEmpty(new ArrayList<>()));

    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    Assertions.assertTrue(ProductUtil.isProductValueEmpty(Arrays.asList(productAttributeValue)));

    productAttributeValue.setDescriptiveAttributeValue(Constants.HYPHEN);
    Assertions.assertTrue(ProductUtil.isProductValueEmpty(Arrays.asList(productAttributeValue)));

    productAttributeValue.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    Assertions.assertFalse(ProductUtil.isProductValueEmpty(Arrays.asList(productAttributeValue)));
  }

  @Test
  public void resetExtractedAttributeValueFlagTest() {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setExtractedValue(true);
    Product product = new Product();
    product.setProductAttributes(Arrays.asList(productAttribute));

    ProductUtil.resetExtractedAttributeValueFlag(new Product(), false);
    ProductUtil.resetExtractedAttributeValueFlag(new Product(), true);
    ProductUtil.resetExtractedAttributeValueFlag(product, true);

    Assertions.assertFalse(product.getProductAttributes().get(0).isExtractedValue());
  }

  @Test
  public void validateProductItemAttributesTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    attribute1.setMarkForDelete(true);

    Attribute attribute2 = new Attribute();
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);

    Attribute attribute3 = new Attribute();
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ATTRIBUTE_CODE_3);

    Attribute attribute4 = new Attribute();
    attribute4.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute4.setAttributeCode(ATTRIBUTE_CODE_4);
    attribute4.setVariantCreation(true);

    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setMarkForDelete(true);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttribute(attribute1);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    ProductItemAttributeValue productItemAttributeValue4 = new ProductItemAttributeValue();
    productItemAttributeValue4.setAttribute(attribute3);
    ProductItemAttributeValue productItemAttributeValue5 = new ProductItemAttributeValue();
    productItemAttributeValue5.setAttribute(attribute4);

    ProductItem productItem1 = new ProductItem();
    productItem1.setMarkForDelete(true);
    ProductItem productItem2 = new ProductItem();
    productItem2.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3,
            productItemAttributeValue4, productItemAttributeValue5));
    ProductItem productItem3 = new ProductItem();
    productItem3.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3,
            productItemAttributeValue4, productItemAttributeValue5));

    Product product = new Product();
    product.setProductItems(Arrays.asList(productItem1, productItem2, productItem3));

    ProductUtil.validateProductItemAttributes(product, true);
  }

  @Test
  public void validateProductItemAttributesErrorTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    attribute1.setMarkForDelete(true);

    Attribute attribute2 = new Attribute();
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);

    Attribute attribute3 = new Attribute();
    attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute3.setAttributeCode(ATTRIBUTE_CODE_3);

    Attribute attribute4 = new Attribute();
    attribute4.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute4.setAttributeCode(ATTRIBUTE_CODE_4);
    attribute4.setVariantCreation(true);

    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setMarkForDelete(true);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttribute(attribute1);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);
    ProductItemAttributeValue productItemAttributeValue4 = new ProductItemAttributeValue();
    productItemAttributeValue4.setAttribute(attribute3);
    ProductItemAttributeValue productItemAttributeValue5 = new ProductItemAttributeValue();
    productItemAttributeValue5.setAttribute(attribute4);

    ProductItem productItem1 = new ProductItem();
    productItem1.setMarkForDelete(true);
    ProductItem productItem2 = new ProductItem();
    productItem2.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3,
            productItemAttributeValue4, productItemAttributeValue5));
    ProductItem productItem3 = new ProductItem();
    productItem3.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3,
            productItemAttributeValue4));

    Product product = new Product();
    product.setProductItems(Arrays.asList(productItem1, productItem2, productItem3));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> ProductUtil.validateProductItemAttributes(product, true));
  }

  @Test
  public void validateProductItemAttributesNoDefiningAttributeTest() {
    Attribute attribute1 = new Attribute();
    attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attribute1.setAttributeCode(ATTRIBUTE_CODE);
    attribute1.setMarkForDelete(true);

    Attribute attribute2 = new Attribute();
    attribute2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    attribute2.setAttributeCode(ATTRIBUTE_CODE_2);

    ProductItemAttributeValue productItemAttributeValue1 = new ProductItemAttributeValue();
    productItemAttributeValue1.setMarkForDelete(true);
    ProductItemAttributeValue productItemAttributeValue2 = new ProductItemAttributeValue();
    productItemAttributeValue2.setAttribute(attribute1);
    ProductItemAttributeValue productItemAttributeValue3 = new ProductItemAttributeValue();
    productItemAttributeValue3.setAttribute(attribute2);

    ProductItem productItem1 = new ProductItem();
    productItem1.setMarkForDelete(true);
    ProductItem productItem2 = new ProductItem();
    productItem2.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3));
    ProductItem productItem3 = new ProductItem();
    productItem3.setProductItemAttributeValues(
        Arrays.asList(productItemAttributeValue1, productItemAttributeValue2, productItemAttributeValue3));

    Product product = new Product();
    product.setProductItems(Arrays.asList(productItem1, productItem2, productItem3));

    ProductUtil.validateProductItemAttributes(product, true);
  }

  @Test
  public void validateProductItemAttributesNoItemsTest() {
    ProductItem productItem1 = new ProductItem();
    productItem1.setMarkForDelete(true);

    Product product = new Product();
    product.setProductItems(Arrays.asList(productItem1));

    ProductUtil.validateProductItemAttributes(product, true);
  }

  @Test
  public void validateProductItemAttributesSwitchOffTest() {
    ProductItem productItem1 = new ProductItem();
    productItem1.setMarkForDelete(true);

    Product product = new Product();
    product.setProductItems(Arrays.asList(productItem1));

    ProductUtil.validateProductItemAttributes(product, false);
  }

  @Test
  public void validateProductBasicInfoBatchSize() {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    ProductUtil.validateProductBasicInfoBatchSize(productCodeList, 10);
  }

  @Test
  public void validateProductBasicInfoBatchSize_False_Case() {
    List<String> productCodeList = Arrays.asList("code1", "code2");
    try {
      ProductUtil.validateProductBasicInfoBatchSize(productCodeList, 1);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertTrue(e.getErrorMessage().contains(ErrorMessage.PRODUCT_BATCH_SIZE_EXCEEDED.getMessage()));
    }
  }
}
