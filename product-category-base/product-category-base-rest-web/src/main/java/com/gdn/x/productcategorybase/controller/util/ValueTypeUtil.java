package com.gdn.x.productcategorybase.controller.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import com.google.api.client.util.ArrayMap;

public class ValueTypeUtil {

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

  public static String concatAttributeIdValueAndValueType(ProductItemAttributeValueRequest itemAttribute,
      Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap,
      String sizeChartValueTypeDelimiter, boolean valueTypeAdditionForDefiningAttributes,
      boolean validateItemAttributeNull) {
    if (validateItemAttributeNull) {
      ValidationUtil.checkParameter(Objects.nonNull(itemAttribute) && Objects.nonNull(itemAttribute.getAttribute()),
          ErrorMessage.INVALID_ADD_VARIANT_REQUEST);
    }
    String attributeId = itemAttribute.getAttribute().getId();
    String value = itemAttribute.getValue();
    if (valueTypeAdditionForDefiningAttributes) {
      String attributeCode = itemAttribute.getAttribute().getAttributeCode();
      String valueType = attributeCodeValueAndValueTypeMap.getOrDefault(attributeCode, new ArrayMap<>())
          .getOrDefault(value, null);
      if (StringUtils.isNotBlank(valueType)) {
        return StringUtils.SPACE + attributeId + StringUtils.SPACE + value + sizeChartValueTypeDelimiter + valueType;
      }
    }
    return StringUtils.SPACE + attributeId + StringUtils.SPACE + value;
  }
}