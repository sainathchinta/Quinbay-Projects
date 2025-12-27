package com.gdn.x.productcategorybase.controller.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.exception.ValidationException;

public class ValueTypeUtilTest {

  private static final String SIZE_CHART_VALUE_TYPE_DELIMITER = " \u200C- ";
  private static final String VALUE_1 = "6";
  private static final String VALUE_TYPE_1 = "UK";
  private static final String VALUE_2 = "7";
  private static final String ATTRIBUTE_CODE_1 = "AT-1";
  private static final String ATTRIBUTE_CODE_2 = "AT-2";
  private static final String ATTRIBUTE_ID = "id";

  private Product product;
  private ProductAttribute productAttributeDefining;
  private ProductAttribute productAttributeDescriptive;
  private ProductAttributeValue productAttributeValue;
  private AllowedValueResponse allowedValueResponse1;
  private AllowedValueResponse allowedValueResponse2;
  ProductItemAttributeValueRequest itemAttributeValueRequest;


  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);
    productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setAllowedAttributeValue(new AllowedAttributeValue());
    productAttributeValue.getAllowedAttributeValue().setValue(VALUE_1);
    productAttributeValue.getAllowedAttributeValue().setValueType(VALUE_TYPE_1);
    productAttributeDefining = new ProductAttribute();
    productAttributeDefining.setAttribute(new Attribute());
    productAttributeDefining.getAttribute().setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDefining.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productAttributeDefining.setProductAttributeValues(List.of(productAttributeValue));
    productAttributeDescriptive = new ProductAttribute();
    productAttributeDescriptive.setAttribute(new Attribute());
    productAttributeDescriptive.getAttribute().setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeDescriptive.getAttribute().setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    product = new Product();
    product.setProductAttributes(List.of(productAttributeDefining, productAttributeDescriptive));

    allowedValueResponse1 = new AllowedValueResponse();
    allowedValueResponse1.setValue(VALUE_1);
    allowedValueResponse1.setValueType(VALUE_TYPE_1);
    allowedValueResponse2 = new AllowedValueResponse();
    allowedValueResponse2.setValue(VALUE_2);
    itemAttributeValueRequest = new ProductItemAttributeValueRequest();
    itemAttributeValueRequest.setAttribute(new AttributeRequest());
    itemAttributeValueRequest.getAttribute().setId(ATTRIBUTE_ID);
    itemAttributeValueRequest.getAttribute().setAttributeCode(ATTRIBUTE_CODE_1);
    itemAttributeValueRequest.setValue(VALUE_1);
  }

  @Test
  public void getAttributeCodeValueAndValueTypeMapTest() {
    Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap = ValueTypeUtil.
        getAttributeCodeValueAndValueTypeMap(product);
    Assertions.assertEquals(1, attributeCodeValueAndValueTypeMap.keySet().size());
    Assertions.assertTrue(attributeCodeValueAndValueTypeMap.keySet().contains(ATTRIBUTE_CODE_1));
    Assertions.assertEquals(VALUE_TYPE_1, attributeCodeValueAndValueTypeMap.get(ATTRIBUTE_CODE_1).get(VALUE_1));
  }

  @Test
  public void getAttributeCodeValueAndValueTypeMapNullTest() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0).setAllowedAttributeValue(null);
    product.getProductAttributes().get(1).setAttribute(null);
    Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap = ValueTypeUtil.
        getAttributeCodeValueAndValueTypeMap(product);
    Assertions.assertEquals(1, attributeCodeValueAndValueTypeMap.size());
    Assertions.assertEquals(0, attributeCodeValueAndValueTypeMap.get(ATTRIBUTE_CODE_1).size());
  }

  @Test
  public void concatAttributeIdValueAndValueTypeWithNoValueTypeTest() {
    String hash = ValueTypeUtil.concatAttributeIdValueAndValueType(itemAttributeValueRequest,
        new HashMap<>(), SIZE_CHART_VALUE_TYPE_DELIMITER, true, true);
    Assertions.assertEquals(StringUtils.SPACE + ATTRIBUTE_ID + StringUtils.SPACE + VALUE_1, hash);
  }

  @Test
  public void concatAttributeIdValueAndValueTypeWithNoValueTypeTest2() {
    Assertions.assertThrows(ValidationException.class,
        () -> ValueTypeUtil.concatAttributeIdValueAndValueType(null, new HashMap<>(), SIZE_CHART_VALUE_TYPE_DELIMITER,
            true, true));
  }

  @Test
  public void concatAttributeIdValueAndValueTypeWithNoValueTypeTest3() {
    Assertions.assertThrows(ValidationException.class,
        () -> ValueTypeUtil.concatAttributeIdValueAndValueType(new ProductItemAttributeValueRequest(), new HashMap<>(),
            SIZE_CHART_VALUE_TYPE_DELIMITER, true, true));
  }

  @Test
  public void concatAttributeIdValueAndValueTypeTest() {
    Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap = new HashMap<>();
    attributeCodeValueAndValueTypeMap.put(ATTRIBUTE_CODE_1, new HashMap<>());
    attributeCodeValueAndValueTypeMap.get(ATTRIBUTE_CODE_1).put(VALUE_1, VALUE_TYPE_1);
    String hash = ValueTypeUtil.concatAttributeIdValueAndValueType(itemAttributeValueRequest,
        attributeCodeValueAndValueTypeMap, SIZE_CHART_VALUE_TYPE_DELIMITER, true, false);
    Assertions.assertEquals(StringUtils.SPACE + ATTRIBUTE_ID + StringUtils.SPACE + VALUE_1
        + SIZE_CHART_VALUE_TYPE_DELIMITER + VALUE_TYPE_1, hash);
  }

}
