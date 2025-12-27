package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.AttributeCodeValueValueTypeDetails;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;

public class ValueTypeUtilTest {

  private static final String SIZE_CHART_VALUE_TYPE_DELIMITER = " \u200C- ";
  private static final String VALUE_1 = "6";
  private static final String VALUE_TYPE_1 = "UK";
  private static final String VALUE_AND_VALUE_TYPE_1 = "UK \u200C- 6";
  private static final String VALUE_2 = "7";
  private static final String VALUE_TYPE_2 = "US";
  private static final String VALUE_AND_VALUE_TYPE_2 = "US \u200C- 7";
  private static final String ATTRIBUTE_CODE_1 = "AT-1";
  private static final String ATTRIBUTE_CODE_2 = "AT-2";
  private static final String ATTRIBUTE_CODE_VALUE = "AT-1-6";
  private static final String ATTRIBUTE_CODE_VALUE_VALUE_TYPE = "AT-1-6 \u200C- UK";

  private ProductL3UpdateRequest productL3UpdateRequest;
  private ProductLevel3AttributeRequest attributeRequest;
  private ProductVariantPriceStockAndImagesRequest itemRequest;
  private Product product;
  private ProductAttribute productAttributeDefining;
  private ProductAttribute productAttributeDescriptive;
  private ProductAttributeValue productAttributeValue;
  private AllowedValueResponse allowedValueResponse1;
  private AllowedValueResponse allowedValueResponse2;
  private ProductAttributeDetailDTO productAttributeDetailDTO;
  private AttributeCodeValueValueTypeDetails attributeDetails;
  private TreeMap attributesValueMap = new TreeMap();
  private TreeMap attributesValueTypeMap = new TreeMap();
  private ProductLevel3 productLevel3 = new ProductLevel3();
  private ProductLevel3Attribute productLevel3Attribute1 = new ProductLevel3Attribute();
  private ProductLevel3Attribute productLevel3Attribute2 = new ProductLevel3Attribute();
  private ProductLevel3Attribute productLevel3Attribute3 = new ProductLevel3Attribute();

  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);

    attributeRequest = new ProductLevel3AttributeRequest();
    attributeRequest.setValues(List.of(VALUE_AND_VALUE_TYPE_1));
    itemRequest = new ProductVariantPriceStockAndImagesRequest();
    itemRequest.getAttributesMap().put(ATTRIBUTE_CODE_1, VALUE_AND_VALUE_TYPE_1);
    itemRequest.getAttributesMap().put(ATTRIBUTE_CODE_2, VALUE_AND_VALUE_TYPE_2);
    itemRequest.setAttributesValueTypeMap(null);
    productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setAttributes(new ArrayList<>());
    productL3UpdateRequest.getAttributes().add(attributeRequest);
    productL3UpdateRequest.setProductItems(List.of(itemRequest));

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

    productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeDetailDTO.setAttributeValue(VALUE_1);

    attributeDetails = new AttributeCodeValueValueTypeDetails();
    attributeDetails.setValueTypeAdditionForDefiningAttributes(true);
    attributeDetails.setSizeChartValueTypeDelimiter(SIZE_CHART_VALUE_TYPE_DELIMITER);
    attributeDetails.setExistingAttributeCodeValueAndValueTypeMap(new HashMap<>());
    attributeDetails.getExistingAttributeCodeValueAndValueTypeMap().put(ATTRIBUTE_CODE_1, new HashMap<>());
    attributeDetails.getExistingAttributeCodeValueAndValueTypeMap().get(ATTRIBUTE_CODE_1).put(VALUE_1, VALUE_TYPE_1);

    productLevel3Attribute1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productLevel3Attribute1.setAttributeCode(ATTRIBUTE_CODE_1);
    productLevel3Attribute1.setValues(Arrays.asList(VALUE_1));
    productLevel3Attribute1.setValueType(VALUE_TYPE_1);

    productLevel3Attribute2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productLevel3Attribute2.setAttributeCode(ATTRIBUTE_CODE_1);
    productLevel3Attribute2.setValues(Arrays.asList(VALUE_2));
    productLevel3Attribute2.setValueType(VALUE_TYPE_1);

    productLevel3Attribute3.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productLevel3Attribute3.setAttributeCode(ATTRIBUTE_CODE_2);
    productLevel3Attribute3.setValues(Arrays.asList(VALUE_1));
    productLevel3Attribute3.setValueType(VALUE_TYPE_2);

    productLevel3.setAttributes(new ArrayList<>());
    productLevel3.getAttributes().add(new ProductLevel3Attribute());
    productLevel3.getAttributes().add(productLevel3Attribute1);
    productLevel3.getAttributes().add(productLevel3Attribute2);
    productLevel3.getAttributes().add(productLevel3Attribute3);
  }

  @Test
  public void separateValueTypeFromValueTest() {
    productL3UpdateRequest.getAttributes().get(0).setValues(new ArrayList<>());
    productL3UpdateRequest.getAttributes().get(0).setAttributeCode(ATTRIBUTE_CODE_1);
    productL3UpdateRequest.getAttributes().get(0).getValues().add("UK \u200C- 6");
    productL3UpdateRequest.getAttributes().get(0).getValues().add("UK \u200C- XL");
    productL3UpdateRequest.getAttributes().get(0).getValues().add("UK \u200C- 05");
    productL3UpdateRequest.getAttributes().get(0).getValues().add("UK \u200C- 11");
    ValueTypeUtil.separateValueTypeFromValue(productL3UpdateRequest, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_TYPE_1, productL3UpdateRequest.getAttributes().get(0).getValueType());
    Assertions.assertEquals(VALUE_1, productL3UpdateRequest.getAttributes().get(0).getValues().get(0));
    Assertions.assertEquals(VALUE_TYPE_1,
        productL3UpdateRequest.getProductItems().get(0).getAttributesValueTypeMap().get(ATTRIBUTE_CODE_1));
    Assertions.assertEquals(VALUE_1,
        productL3UpdateRequest.getProductItems().get(0).getAttributesMap().get(ATTRIBUTE_CODE_1));
    Assertions.assertEquals(VALUE_TYPE_2,
        productL3UpdateRequest.getProductItems().get(0).getAttributesValueTypeMap().get(ATTRIBUTE_CODE_2));
    Assertions.assertEquals(VALUE_2,
        productL3UpdateRequest.getProductItems().get(0).getAttributesMap().get(ATTRIBUTE_CODE_2));
  }

  @Test
  public void separateValueTypeFromValueWithoutSeparationTest() {
    productL3UpdateRequest.getAttributes().get(0).setValueType(VALUE_TYPE_1);
    productL3UpdateRequest.getAttributes().get(0).setValues(List.of(VALUE_1));
    productL3UpdateRequest.getAttributes().add(new ProductLevel3AttributeRequest());
    itemRequest.getAttributesMap().put(ATTRIBUTE_CODE_1, VALUE_1);
    itemRequest.getAttributesMap().put(ATTRIBUTE_CODE_2, " ");
    itemRequest.setAttributesValueTypeMap(new TreeMap<>());
    itemRequest.getAttributesValueTypeMap().put(ATTRIBUTE_CODE_1, VALUE_TYPE_1);
    ValueTypeUtil.separateValueTypeFromValue(productL3UpdateRequest, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_TYPE_1, productL3UpdateRequest.getAttributes().get(0).getValueType());
    Assertions.assertEquals(VALUE_1, productL3UpdateRequest.getAttributes().get(0).getValues().get(0));
    Assertions.assertEquals(VALUE_TYPE_1,
        productL3UpdateRequest.getProductItems().get(0).getAttributesValueTypeMap().get(ATTRIBUTE_CODE_1));
    Assertions.assertEquals(VALUE_1,
        productL3UpdateRequest.getProductItems().get(0).getAttributesMap().get(ATTRIBUTE_CODE_1));
  }

  @Test
  public void separateValueTypeFromValueNullTest() {
    ValueTypeUtil.separateValueTypeFromValue(productL3UpdateRequest, SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertNull(productL3UpdateRequest.getAttributes().get(0).getValueType());
  }

  @Test
  public void getAttributeCodeValueAndValueTypeMapTest() {
    Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeMap(product);
    Assertions.assertEquals(1, attributeCodeValueAndValueTypeMap.keySet().size());
    Assertions.assertTrue(attributeCodeValueAndValueTypeMap.keySet().contains(ATTRIBUTE_CODE_1));
    Assertions.assertEquals(VALUE_TYPE_1, attributeCodeValueAndValueTypeMap.get(ATTRIBUTE_CODE_1).get(VALUE_1));
  }

  @Test
  public void getAttributeCodeValueAndValueTypeMapNullTest() {
    product.getProductAttributes().get(0).getProductAttributeValues().get(0).setAllowedAttributeValue(null);
    product.getProductAttributes().get(1).setAttribute(null);
    Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeMap(product);
    Assertions.assertEquals(1, attributeCodeValueAndValueTypeMap.size());
    Assertions.assertEquals(0, attributeCodeValueAndValueTypeMap.get(ATTRIBUTE_CODE_1).size());
  }

  @Test
  public void getValueTypeAndValueTest() {
    String valueTypeAndValue =
        ValueTypeUtil.getValueTypeAndValue(VALUE_TYPE_1, VALUE_1, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_AND_VALUE_TYPE_1, valueTypeAndValue);
  }

  @Test
  public void getValueTypeAndValueBlankValueTypeTest() {
    String valueTypeAndValue = ValueTypeUtil.getValueTypeAndValue("  ", VALUE_1, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_1, valueTypeAndValue);
  }

  @Test
  public void getValueTypeAndValueTurnOffTest() {
    String valueTypeAndValue =
        ValueTypeUtil.getValueTypeAndValue(VALUE_TYPE_1, VALUE_1, SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertEquals(VALUE_1, valueTypeAndValue);
  }

  @Test
  public void getValueTypeAndValuesTest() {
    List<String> valueTypeAndValues =
        ValueTypeUtil.getValueTypeAndValue(VALUE_TYPE_1, List.of(VALUE_1), SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_AND_VALUE_TYPE_1, valueTypeAndValues.get(0));
  }

  @Test
  public void getValueTypeAndValueBlankValuesTypeTest() {
    List<String> valueTypeAndValues =
        ValueTypeUtil.getValueTypeAndValue("  ", List.of(VALUE_1), SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_1, valueTypeAndValues.get(0));
  }

  @Test
  public void getValueTypeAndValuesTurnOffTest() {
    List<String> valueTypeAndValues =
        ValueTypeUtil.getValueTypeAndValue(VALUE_TYPE_1, List.of(VALUE_1), SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertEquals(VALUE_1, valueTypeAndValues.get(0));
  }

  @Test
  public void getValueTest() {
    String value = ValueTypeUtil.getValue(VALUE_AND_VALUE_TYPE_1, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_1, value);
  }

  @Test
  public void getValueWithNoDelimiterTest() {
    String value = ValueTypeUtil.getValue(VALUE_2, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_2, value);
  }

  @Test
  public void getValueEmptyValueTest() {
    String value = ValueTypeUtil.getValue(null, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertNull(value);
  }

  @Test
  public void getValueTurnOffTest() {
    String value = ValueTypeUtil.getValue(VALUE_AND_VALUE_TYPE_1, SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertEquals(VALUE_AND_VALUE_TYPE_1, value);
  }


  @Test
  public void getValuesTest() {
    List<String> values =
        ValueTypeUtil.getValues(List.of(VALUE_AND_VALUE_TYPE_1), SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_1, values.get(0));
  }

  @Test
  public void getValuesWithNoDelimiterTest() {
    List<String> values = ValueTypeUtil.getValues(List.of(VALUE_2), SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(VALUE_2, values.get(0));
  }

  @Test
  public void getValuesEmptyValueTest() {
    List<String> values = ValueTypeUtil.getValues(null, SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(0, values.size());
  }

  @Test
  public void getValuesTurnOffTest() {
    List<String> values =
        ValueTypeUtil.getValues(List.of(VALUE_AND_VALUE_TYPE_1), SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertEquals(VALUE_AND_VALUE_TYPE_1, values.get(0));
  }

  @Test
  public void getAttributeCodeValueAndValueTypeStringTest() {
    attributesValueMap.put(ATTRIBUTE_CODE_1, VALUE_1);
    attributesValueTypeMap.put(ATTRIBUTE_CODE_1, VALUE_TYPE_1);
    String attributeCodeValueAndValueType =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeString(attributesValueMap.firstEntry(), attributesValueTypeMap,
            SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(ATTRIBUTE_CODE_VALUE_VALUE_TYPE, attributeCodeValueAndValueType);
  }

  @Test
  public void getAttributeCodeValueAndValueTypeStringNullValueTypeTest() {
    attributesValueMap.put(ATTRIBUTE_CODE_1, VALUE_1);
    attributesValueTypeMap.put(ATTRIBUTE_CODE_1, VALUE_TYPE_1);
    String attributeCodeValueAndValueType =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeString(attributesValueMap.firstEntry(), null,
            SIZE_CHART_VALUE_TYPE_DELIMITER, true);
    Assertions.assertEquals(ATTRIBUTE_CODE_VALUE, attributeCodeValueAndValueType);
  }

  @Test
  public void getAttributeCodeValueAndValueTypeStringTurnOffTest() {
    attributesValueMap.put(ATTRIBUTE_CODE_1, VALUE_1);
    attributesValueTypeMap.put(ATTRIBUTE_CODE_1, VALUE_TYPE_1);
    String attributeCodeValueAndValueType =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeString(attributesValueMap.firstEntry(), attributesValueTypeMap,
            SIZE_CHART_VALUE_TYPE_DELIMITER, false);
    Assertions.assertEquals(ATTRIBUTE_CODE_VALUE, attributeCodeValueAndValueType);
  }

  @Test
  public void getAttributeCodeValueAndValueTypeString1Test() {
    String attributeCodeValueAndValueType =
        ValueTypeUtil.getAttributeCodeValueAndValueTypeString(productAttributeDetailDTO, attributeDetails);
    Assertions.assertEquals(ATTRIBUTE_CODE_VALUE_VALUE_TYPE, attributeCodeValueAndValueType);
  }

  @Test
  public void validateValueAndValueTypeTest() {
    ApiErrorCode apiErrorCode = ValueTypeUtil.validateValueAndValueType(productLevel3, true, true);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void validateValueAndValueTypeTest2() {
    ApiErrorCode apiErrorCode = ValueTypeUtil.validateValueAndValueType(productLevel3, true, false);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void validateValueAndValueTypeDuplicateValueTest() {
    productLevel3Attribute2.setValues(Arrays.asList(VALUE_1));
    ApiErrorCode apiErrorCode = ValueTypeUtil.validateValueAndValueType(productLevel3, true, true);
    Assertions.assertEquals(ApiErrorCode.DUPLICATE_ATTRIBUTE_VALUE_NOT_ALLOWED, apiErrorCode);
  }

  @Test
  public void validateValueAndValueTypeMultiValueTypeTest() {
    productLevel3Attribute2.setValueType(VALUE_TYPE_2);
    ApiErrorCode apiErrorCode = ValueTypeUtil.validateValueAndValueType(productLevel3, true, true);
    Assertions.assertEquals(ApiErrorCode.MULTI_VALUE_TYPE_NOT_ALLOWED, apiErrorCode);
  }

  @Test
  public void concatenateValueWithValueTypeWithNullMasterDataTest() {
    ProductL3Response productL3Response = new ProductL3Response();
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, new ProductL3DetailsResponse(), false, "-",
        true);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeWithNullMasterDataTest4() {
    ProductL3Response productL3Response = new ProductL3Response();
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, new ProductL3DetailsResponse(), true, "-",
        false);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeWithNullMasterDataTest3() {
    ProductL3Response productL3Response = new ProductL3Response();
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, new ProductL3DetailsResponse(), false, "-",
        false);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeWithNullMasterDataTest2() {
    ProductL3DetailsResponse ProductL3DetailsResponse = new ProductL3DetailsResponse();
    ValueTypeUtil.concatenateValueWithValueType(getProductL3Response(), ProductL3DetailsResponse, true, "-",
        true);
    Assertions.assertNotNull(ProductL3DetailsResponse);
  }


  @Test
  public void concatenateValueWithValueTypeWithNullMasterDataTest5() {
    ProductL3Response productL3Response = new ProductL3Response();
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, new ProductL3DetailsResponse(), true, "-",
        true);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeTest6() {
    ProductL3Response productL3Response = getProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO.setValueType("UK");
    masterDataAllowedAttributeValueDTO.setValue("Test");
    masterDataAllowedAttributeValueDTO.setAllowedAttributeValueCode("AT-000001");
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO2 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO2.setValueType("UK");
    masterDataAllowedAttributeValueDTO2.setValue("Test1");
    masterDataAllowedAttributeValueDTO2.setAllowedAttributeValueCode("AT-000002");
    masterDataProductAttributeValueDTO2.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO2);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    productL3Response.getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(
            Arrays.asList(masterDataProductAttributeValueDTO, masterDataProductAttributeValueDTO2,
                masterDataProductAttributeValueDTO3));
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, getProductL3DetailsResponse(), true, "-", false);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeTest8() {
    ProductL3Response productL3Response = getProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO.setValueType("UK");
    masterDataAllowedAttributeValueDTO.setValue("Test");
    masterDataAllowedAttributeValueDTO.setAllowedAttributeValueCode("AT-000001");
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO2 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO2.setValueType("UK");
    masterDataAllowedAttributeValueDTO2.setValue("Test1");
    masterDataAllowedAttributeValueDTO2.setAllowedAttributeValueCode("AT-000002");
    masterDataProductAttributeValueDTO2.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO2);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    productL3Response.getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(
            Arrays.asList(masterDataProductAttributeValueDTO, masterDataProductAttributeValueDTO2,
                masterDataProductAttributeValueDTO3));
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, getProductL3DetailsResponse(), false, "-", true);
    Assertions.assertNotNull(productL3Response);
  }

  @Test
  public void concatenateValueWithValueTypeTest7() {
    ProductL3Response productL3Response = getProductL3Response();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO.setValueType(null);
    masterDataAllowedAttributeValueDTO.setValue("Test");
    masterDataAllowedAttributeValueDTO.setAllowedAttributeValueCode("AT-000001");
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO2 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO2.setValueType("UK");
    masterDataAllowedAttributeValueDTO2.setValue("Test1");
    masterDataAllowedAttributeValueDTO2.setAllowedAttributeValueCode("AT-000002");
    masterDataProductAttributeValueDTO2.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO2);
    productL3Response.getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(
            Arrays.asList(masterDataProductAttributeValueDTO, masterDataProductAttributeValueDTO2));
    ValueTypeUtil.concatenateValueWithValueType(productL3Response, getProductL3DetailsResponse(), true, "-", true);
    Assertions.assertNotNull(productL3Response);
  }


  private ProductL3Response getProductL3Response() {
    ProductL3Response productL3Response = new ProductL3Response();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    masterDataAttributeDTO.setAttributeCode("AT-000001");
    masterDataAttributeDTO.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode("AT-000001");
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    MasterDataAttributeDTO masterDataAttributeDTO3 = new MasterDataAttributeDTO();
    masterDataAttributeDTO3.setAttributeCode("AT-000001");
    masterDataAttributeDTO3.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO.setMasterDataAttribute(
        new MasterDataAttributeDTO(MasterDataAttributeType.DEFINING_ATTRIBUTE, false, "", false, "", "AT-00001", "",
            false));
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(
        new MasterDataAttributeDTO(MasterDataAttributeType.DEFINING_ATTRIBUTE, false, "", false, "", "AT-00001", "",
            false));
    masterDataProductAttributeDTO2.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO3 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO3.setMasterDataAttribute(
        new MasterDataAttributeDTO(MasterDataAttributeType.DEFINING_ATTRIBUTE, false, "", false, "", "AT-00001", "",
            false));
    masterDataProductAttributeDTO3.setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataProductAttributeDTO3.setMasterDataAttribute(masterDataAttributeDTO3);
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO, masterDataProductAttributeDTO2, masterDataProductAttributeDTO3));
    productL3Response.setMasterDataProduct(masterDataProductDTO);
    return productL3Response;
  }

  private ProductL3DetailsResponse getProductL3DetailsResponse() {
    ProductLevel3AttributeResponse productLevel3AttributeResponse = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse.setValueType("UK");
    productLevel3AttributeResponse.setValues(Arrays.asList("Test"));
    productLevel3AttributeResponse.setAttributeCode("AT-000001");
    productLevel3AttributeResponse.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductLevel3AttributeResponse productLevel3AttributeResponse2 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse2.setValueType("US");
    productLevel3AttributeResponse2.setValues(Arrays.asList("Test1"));
    productLevel3AttributeResponse2.setAttributeCode("AT-000001");
    productLevel3AttributeResponse2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductLevel3AttributeResponse productLevel3AttributeResponse3 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse3.setValueType("US");
    productLevel3AttributeResponse3.setValues(Arrays.asList("Test1"));
    productLevel3AttributeResponse3.setAttributeCode("AT-000001");
    ProductLevel3AttributeResponse productLevel3AttributeResponse4 = new ProductLevel3AttributeResponse();
    productLevel3AttributeResponse4.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productLevel3AttributeResponse4.setAttributeCode("Attribute-3");
    productLevel3AttributeResponse4.setValues(Arrays.asList("NewValue"));
    productLevel3AttributeResponse3.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    ProductL3DetailsResponse productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setAttributes(
        Arrays.asList(productLevel3AttributeResponse, productLevel3AttributeResponse2,
            productLevel3AttributeResponse3, productLevel3AttributeResponse4));
    return productL3DetailsResponse;
  }

  @Test
  public void getValueAndValueTypeMapTest() {
    ValueTypeUtil.getValueAndValueTypeMap(null, new HashMap<>());
    ValueTypeUtil.getValueAndValueTypeMap(new ProductAndAttributeDetailResponse(), new HashMap<>());
    Map<String, String> valueAndValueTypeMap = new HashMap<>();
    ValueTypeUtil.getValueAndValueTypeMap(new ProductAndAttributeDetailResponse(), valueAndValueTypeMap);
    ProductAndAttributeDetailResponse productAndAttributeDetailResponse = new ProductAndAttributeDetailResponse();
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setProductAttributeValues(new ArrayList<>());
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_1);
    productAttributeResponse.setAttribute(attributeResponse);
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    productAndAttributeDetailResponse.getProductAttributeResponses().get(0).getAttribute().setSizeAttribute(true);
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeResponse.setProductAttributeValues(Collections.singletonList(productAttributeValueResponse));
    productAndAttributeDetailResponse.setProductAttributeResponses(Collections.singletonList(productAttributeResponse));
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    AllowedAttributeValueResponse attributeValueResponse = new AllowedAttributeValueResponse();
    attributeValueResponse.setValue(VALUE_2);
    productAndAttributeDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .setAllowedAttributeValue(attributeValueResponse);
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    productAndAttributeDetailResponse.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().setValueType(VALUE_TYPE_1);
    ValueTypeUtil.getValueAndValueTypeMap(productAndAttributeDetailResponse, valueAndValueTypeMap);
    Assertions.assertEquals(valueAndValueTypeMap.get(ATTRIBUTE_CODE_1 + VALUE_2), VALUE_TYPE_1);
  }

}
