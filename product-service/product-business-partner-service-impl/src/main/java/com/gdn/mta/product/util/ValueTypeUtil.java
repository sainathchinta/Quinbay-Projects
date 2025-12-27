package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.IntStream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;

import com.gda.mta.product.dto.AttributeCodeValueValueTypeDetails;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

public class ValueTypeUtil {

  public static void separateValueTypeFromValue(ProductL3UpdateRequest productL3UpdateRequest,
      String sizeChartValueTypeDelimiter, boolean valueTypeAdditionForDefiningAttributes) {
    if (valueTypeAdditionForDefiningAttributes) {

      unwindAttributeRequestBasedOnValues(productL3UpdateRequest);

      // At L1 level in the request
      for (ProductLevel3AttributeRequest attributeRequest : Optional.ofNullable(
          productL3UpdateRequest.getAttributes()).orElse(new ArrayList<>())) {
        processProductAttribute(sizeChartValueTypeDelimiter, attributeRequest);
      }

      // At L4 level in the request
      for (ProductVariantPriceStockAndImagesRequest item : Optional.ofNullable(
          productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>())) {
        processItemAttributes(sizeChartValueTypeDelimiter, item);
      }

    }
  }

  private static void unwindAttributeRequestBasedOnValues(ProductL3UpdateRequest productL3UpdateRequest) {
    // Request is as follows { attributeCode : "ATT-01", values : ["S", "M", "XL"] } we want to split this into below
    //{ attributeCode : "ATT-01", values : ["S"] }, { attributeCode : "ATT-01", values : ["M"] }, { attributeCode : "ATT-01", values : ["XL"] }

    List<ProductLevel3AttributeRequest> generatedProductLevel3AttributeRequests = new ArrayList<>();
    List<ProductLevel3AttributeRequest> attributeRequests =
        Optional.ofNullable(productL3UpdateRequest.getAttributes()).orElse(new ArrayList<>());
    for (ProductLevel3AttributeRequest attributeRequest : attributeRequests) {
      if (CollectionUtils.isNotEmpty(attributeRequest.getValues()) && attributeRequest.getValues().size() > 1) {
        IntStream.range(1, attributeRequest.getValues().size()).forEach(
            i -> generateProductLevel3AttributeRequest(attributeRequest, i, generatedProductLevel3AttributeRequests));
        attributeRequest.setValues(new ArrayList<>(List.of(attributeRequest.getValues().get(0))));
      }
    }
    attributeRequests.addAll(generatedProductLevel3AttributeRequests);
  }

  private static void generateProductLevel3AttributeRequest(ProductLevel3AttributeRequest attributeRequest, int i,
      List<ProductLevel3AttributeRequest> generatedProductLevel3AttributeRequests) {
    ProductLevel3AttributeRequest generatedProductLevel3AttributeRequest = new ProductLevel3AttributeRequest();
    BeanUtils.copyProperties(attributeRequest, generatedProductLevel3AttributeRequest, "values");
    generatedProductLevel3AttributeRequest.setValues(new ArrayList<>(List.of(attributeRequest.getValues().get(i))));
    generatedProductLevel3AttributeRequests.add(generatedProductLevel3AttributeRequest);
  }

  private static void processItemAttributes(String sizeChartValueTypeDelimiter,
      ProductVariantPriceStockAndImagesRequest item) {
    for (String attributeCode : Optional.ofNullable(item.getAttributesMap())
        .orElse(new TreeMap<>()).keySet()) {
      if (contains(List.of(item.getAttributesMap().get(attributeCode)), sizeChartValueTypeDelimiter)) {
        String[] valueTypeAndValue = item.getAttributesMap().get(attributeCode).split(
            sizeChartValueTypeDelimiter);
        if (Objects.isNull(item.getAttributesValueTypeMap())) {
          item.setAttributesValueTypeMap(new TreeMap<>());
        }
        item.getAttributesValueTypeMap().put(attributeCode, valueTypeAndValue[0]);
        item.getAttributesMap().put(attributeCode, valueTypeAndValue[1]);
      }
    }
  }

  private static void processProductAttribute(String sizeChartValueTypeDelimiter,
      ProductLevel3AttributeRequest attributeRequest) {
    if (contains(attributeRequest.getValues(), sizeChartValueTypeDelimiter)) {
      String[] valueTypeAndValue = attributeRequest.getValues().get(0).split(
          sizeChartValueTypeDelimiter);
      attributeRequest.setValueType(valueTypeAndValue[0]);
      attributeRequest.setValues(List.of(valueTypeAndValue[1]));
    }
  }

  private static boolean contains(List<String> value, String delimiter) {
    return CollectionUtils.isNotEmpty(value) && StringUtils.isNotBlank(value.get(0)) && value.get(0)
        .contains(delimiter);
  }

  public static List<String> getValues(List<String> valueTypeAndValues,
      String delimiter, boolean valueTypeAdditionForDefiningAttributes) {
    if (valueTypeAdditionForDefiningAttributes) {
      List<String> values = new ArrayList<>();
      for (String valueTypeAndValue : Optional.ofNullable(valueTypeAndValues).orElse(new ArrayList<>())) {
        values.add(getValue(valueTypeAndValue, delimiter, true));
      }
      return values;
    }
    return valueTypeAndValues;
  }

  public static String getValue(String valueTypeAndValue, String delimiter,
      boolean valueTypeAdditionForDefiningAttributes) {
    if (valueTypeAdditionForDefiningAttributes && StringUtils.isNotBlank(valueTypeAndValue)) {
      String[] valueTypeAndValueArray = valueTypeAndValue.split(delimiter);
      if (valueTypeAndValueArray.length == Constants.TWO) {
        return valueTypeAndValueArray[1];
      }
    }
    return valueTypeAndValue;
  }

  public static List<String> getValueTypeAndValue(String valueType, List<String> values, String delimiter,
      boolean valueTypeAdditionForDefiningAttributes) {
    if (valueTypeAdditionForDefiningAttributes && StringUtils.isNotBlank(valueType)) {
      List<String> valueTypeAndValues = new ArrayList<>();
      for (String value : values) {
        valueTypeAndValues.add(getValueTypeAndValue(valueType, value, delimiter, true));
      }
      return valueTypeAndValues;
    }
    return values;
  }

  public static String getValueTypeAndValue(String valueType, String value, String delimiter,
      boolean valueTypeAdditionForDefiningAttributes) {
    if (valueTypeAdditionForDefiningAttributes && StringUtils.isNotBlank(valueType)) {
      return valueType + delimiter + value;
    }
    return value;
  }

  public static Map<String, Map<String, String>> getAttributeCodeValueAndValueTypeMap(Product product) {
    Map<String, Map<String, String>> attributeMap = new HashMap<>();
    Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>()).stream()
        .filter(attributeRequest -> Objects.nonNull(attributeRequest.getAttribute()))
        .filter(attributeRequest -> AttributeType.DEFINING_ATTRIBUTE.equals(attributeRequest.getAttribute().getAttributeType()))
        .forEach(attributeRequest -> updateAttributeMap(attributeMap, attributeRequest));
    return attributeMap;
  }

  private static void updateAttributeMap(Map<String, Map<String, String>> attributeMap,
      ProductAttribute attributeRequest) {
    String attributeCode = attributeRequest.getAttribute().getAttributeCode();
    attributeMap.putIfAbsent(attributeCode, new HashMap<>());
    attributeRequest.getProductAttributeValues().stream()
        .filter(productAttributeValue -> Objects.nonNull(productAttributeValue.getAllowedAttributeValue()))
        .forEach(productAttributeValue -> attributeMap.get(attributeCode).put(
            productAttributeValue.getAllowedAttributeValue().getValue(),
            productAttributeValue.getAllowedAttributeValue().getValueType()));
  }

  public static String getAttributeCodeValueAndValueTypeString(ProductAttributeDetailDTO productAttributeDetailDTO,
      AttributeCodeValueValueTypeDetails attributeDetails) {

    String attributeCode = productAttributeDetailDTO.getAttributeCode();
    String value = productAttributeDetailDTO.getAttributeValue();
    String valueType = Optional.ofNullable(attributeDetails.getExistingAttributeCodeValueAndValueTypeMap())
            .orElse(new HashMap<>()).getOrDefault(attributeCode, new HashMap<>()).getOrDefault(value, null);

    return getAttributeCodeValueAndValueTypeString(attributeCode, value, valueType,
        attributeDetails.getSizeChartValueTypeDelimiter(), attributeDetails.isValueTypeAdditionForDefiningAttributes());
  }

  public static String getAttributeCodeValueAndValueTypeString(Map.Entry<String, String> itemAttribute,
      TreeMap<String, String> attributesValueTypeMap,  String sizeChartValueTypeDelimiter,
      boolean valueTypeAdditionForDefiningAttributes) {
    String attributeCode = itemAttribute.getKey();
    String value = itemAttribute.getValue();
    String valueType = Optional.ofNullable(attributesValueTypeMap)
        .orElse(new TreeMap<>()).getOrDefault(attributeCode, null);

    return getAttributeCodeValueAndValueTypeString(attributeCode, value,
         valueType, sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes);
  }

  private static String getAttributeCodeValueAndValueTypeString(String attributeCode, String value,
      String valueType, String sizeChartValueTypeDelimiter, boolean valueTypeAdditionForDefiningAttributes) {

    StringBuilder attributeCodeValueAndValueType =
        new StringBuilder().append(attributeCode).append(Constants.HYPHEN).append(value);
    if (valueTypeAdditionForDefiningAttributes && !StringUtils.isBlank(valueType)) {
      attributeCodeValueAndValueType.append(sizeChartValueTypeDelimiter).append(valueType);
    }
    return attributeCodeValueAndValueType.toString();
  }

  public static ApiErrorCode validateValueAndValueType(ProductLevel3 product, boolean valueTypeAdditionForDefiningAttributes, boolean uniqueValueTypeAdditionEnabled) {
    if (valueTypeAdditionForDefiningAttributes) {
      Map<String, Set<String>> attributeCodeAndValueMap = new HashMap<>();
      Map<String, String> attributeCodeAndValueTypeMap = new HashMap<>();
      ApiErrorCode apiErrorCode = null;
      for (ProductLevel3Attribute attribute : Optional.ofNullable(product.getAttributes()).orElse(new ArrayList<>())) {
        if (AttributeType.DEFINING_ATTRIBUTE.toString().equals(attribute.getAttributeType())) {
          apiErrorCode = validateAndUpdateDuplicateValues(attribute, attributeCodeAndValueMap);
          if (Objects.nonNull(apiErrorCode)) {
            return apiErrorCode;
          }
          apiErrorCode = validateAndUpdateMultiValueType(attribute, attributeCodeAndValueTypeMap, uniqueValueTypeAdditionEnabled);
          if (Objects.nonNull(apiErrorCode)) {
            return apiErrorCode;
          }
        }
      }
    }
    return null;
  }

  private static ApiErrorCode validateAndUpdateDuplicateValues(ProductLevel3Attribute attribute,
      Map<String, Set<String>> attributeCodeAndValueMap) {
    if (!attributeCodeAndValueMap.containsKey(attribute.getAttributeCode())) {
      attributeCodeAndValueMap.put(attribute.getAttributeCode(), new HashSet<>());
    }
    for (String value : Optional.ofNullable(attribute.getValues()).orElse(new ArrayList<>())) {
      if (attributeCodeAndValueMap.get(attribute.getAttributeCode()).contains(value)) {
        return ApiErrorCode.DUPLICATE_ATTRIBUTE_VALUE_NOT_ALLOWED;
      }
      attributeCodeAndValueMap.get(attribute.getAttributeCode()).add(value);
    }
    return null;
  }

  private static ApiErrorCode validateAndUpdateMultiValueType(ProductLevel3Attribute attribute,
      Map<String, String> attributeCodeAndValueTypeMap, boolean uniqueValueTypeAdditionEnabled) {
    if (uniqueValueTypeAdditionEnabled) {
      if (attributeCodeAndValueTypeMap.containsKey(attribute.getAttributeCode())) {
        if (!StringUtils.equals(attributeCodeAndValueTypeMap.get(attribute.getAttributeCode()),
            attribute.getValueType())) {
          return ApiErrorCode.MULTI_VALUE_TYPE_NOT_ALLOWED;
        }
      } else {
        attributeCodeAndValueTypeMap.put(attribute.getAttributeCode(), attribute.getValueType());
      }
    }
    return null;
  }

  public static void concatenateValueWithValueType(ProductL3Response productL3Response,
      ProductL3DetailsResponse productL3DetailsResponse, boolean concatenateValueWithValueType,
      String sizeChartValueTypeDelimiter, boolean valueTypeAdditionForDefiningAttributes) {
      Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap = new HashMap<>();
      populateAttributeCodeAndValueAndValueTypeMap(productL3Response, attributeCodeValueAndValueTypeMap);
      for (ProductLevel3AttributeResponse productLevel3AttributeResponse : productL3DetailsResponse.getAttributes())
        updateProductLevel3AttributeResponse(sizeChartValueTypeDelimiter, productLevel3AttributeResponse,
            attributeCodeValueAndValueTypeMap, concatenateValueWithValueType, valueTypeAdditionForDefiningAttributes);
  }

  private static void updateProductLevel3AttributeResponse(String sizeChartValueTypeDelimiter,
      ProductLevel3AttributeResponse productLevel3AttributeResponse,
      Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap, boolean concatenateValueWithValueType,
      boolean valueTypeAdditionForDefiningAttributes) {
    if (AttributeType.DEFINING_ATTRIBUTE.name().equals(productLevel3AttributeResponse.getAttributeType())) {
      Map<String, String> valueAndValueTypeMap =
          attributeCodeValueAndValueTypeMap.get(productLevel3AttributeResponse.getAttributeCode());
      String valueType = Optional.ofNullable(valueAndValueTypeMap).orElse(new HashMap<>())
          .getOrDefault(productLevel3AttributeResponse.getValues().get(0), null);
      productLevel3AttributeResponse.setValueType(valueType);
      if (StringUtils.isNotBlank(valueType) && concatenateValueWithValueType && valueTypeAdditionForDefiningAttributes) {
        productLevel3AttributeResponse.setValues(Collections.singletonList(
            attributeCodeValueAndValueTypeMap.get(productLevel3AttributeResponse.getAttributeCode())
                .get(productLevel3AttributeResponse.getValues().get(0)).concat(sizeChartValueTypeDelimiter)
                .concat(productLevel3AttributeResponse.getValues().get(0))));
      } else {
        productLevel3AttributeResponse.setValues(
            Collections.singletonList(productLevel3AttributeResponse.getValues().get(0)));
      }
    }
  }

  private static void populateAttributeCodeAndValueAndValueTypeMap(ProductL3Response productL3Response,
      Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap) {
    if (Objects.nonNull(productL3Response.getMasterDataProduct())) {
      for (MasterDataProductAttributeDTO masterDataProductAttributeDTO : productL3Response.getMasterDataProduct()
          .getMasterDataProductAttributes()) {
        if (MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
            masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeType())) {
          List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS =
              masterDataProductAttributeDTO.getMasterDataProductAttributeValues();
          for (MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO : masterDataProductAttributeValueDTOS) {

            if (Objects.nonNull(masterDataProductAttributeValueDTO.getAllowedAttributeValue()))
              populateValueAndValueTypeMap(attributeCodeValueAndValueTypeMap, masterDataProductAttributeDTO,
                  masterDataProductAttributeValueDTO);
          }
        }
      }

    }
  }

  private static void populateValueAndValueTypeMap(Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap,
      MasterDataProductAttributeDTO masterDataProductAttributeDTO,
      MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO) {
    String attributeCode = masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeCode();
    String value = masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValue();
    String valueType = masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValueType();

    if (!attributeCodeValueAndValueTypeMap.containsKey(attributeCode)) {
      Map<String, String> valueAndValueTypeMap = new HashMap<>();
      valueAndValueTypeMap.put(value, valueType);
      attributeCodeValueAndValueTypeMap.put(attributeCode, valueAndValueTypeMap);
    } else {
      attributeCodeValueAndValueTypeMap.get(attributeCode).put(value, valueType);
    }
  }

  public static void getValueAndValueTypeMap(ProductAndAttributeDetailResponse productAttributesByProductId,
      Map<String, String> valueAndValueTypeMap) {
    if (Objects.nonNull(productAttributesByProductId) && CollectionUtils.isNotEmpty(
        productAttributesByProductId.getProductAttributeResponses())
        && productAttributesByProductId.getProductAttributeResponses().stream()
        .filter(productAttributeResponse -> Objects.nonNull(productAttributeResponse.getAttribute()))
        .anyMatch(productAttributeResponse -> productAttributeResponse.getAttribute().isSizeAttribute())) {
      List<ProductAttributeValueResponse> sizeAttributeValues =
          productAttributesByProductId.getProductAttributeResponses().stream()
              .filter(productAttributeResponse -> productAttributeResponse.getAttribute().isSizeAttribute()).findFirst()
              .map(ProductAttributeResponse::getProductAttributeValues).orElse(new ArrayList<>());
      String sizeAttributeCode = productAttributesByProductId.getProductAttributeResponses().stream()
          .filter(productAttributeResponse -> productAttributeResponse.getAttribute().isSizeAttribute()).findFirst()
          .orElse(new ProductAttributeResponse()).getAttribute().getAttributeCode();
      for (ProductAttributeValueResponse attributeValueResponse : sizeAttributeValues) {
        if (Objects.nonNull(attributeValueResponse.getAllowedAttributeValue()) && StringUtils.isNotEmpty(
            attributeValueResponse.getAllowedAttributeValue().getValueType())) {
          valueAndValueTypeMap.put(sizeAttributeCode + attributeValueResponse.getAllowedAttributeValue().getValue(),
              attributeValueResponse.getAllowedAttributeValue().getValueType());
        }
      }
    }
  }

}
