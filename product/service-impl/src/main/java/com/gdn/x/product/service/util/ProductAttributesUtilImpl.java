package com.gdn.x.product.service.util;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

import jakarta.annotation.PostConstruct;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.model.entity.PristineDataItem;

@Service
public class ProductAttributesUtilImpl implements ProductAttributesUtil {

  private static final String COMMA_DELIMITER = ",";

  private Map<String, String> categoryListingParametersMap;

  private  Map<String, String> attributeNameTranslationMap;

  @Value("${pristine.product.listing.parameters}")
  private String pristineListingParameters;

  @Value("${pristine.product.attribute.name.translation.map}")
  private String pristineProductAttributeNameMap;

  @PostConstruct
  public void initValue() throws Exception {
    categoryListingParametersMap = this.objectMapper
        .readValue(pristineListingParameters, new TypeReference<Map<String, String>>() {
        });
    attributeNameTranslationMap = this.objectMapper
        .readValue(pristineProductAttributeNameMap, new TypeReference<Map<String, String>>() {
        });
  }

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public Map<String, String> translatePristineListingAttributeName(Map<String, String> attributes,
      String[] categoryListingParameterKey) {
    if (Objects.isNull(attributes)) {
      attributes = new HashMap<>();
    }
    if (Objects.nonNull(categoryListingParameterKey) && categoryListingParameterKey.length > 0
        && attributes.entrySet().size() > 0) {
      Map<String, String> pristineListingAttribute = new LinkedHashMap<>();
      for (String attribute : categoryListingParameterKey) {
        String value = attributes.get(attribute.toUpperCase());
        pristineListingAttribute.put(attributeNameTranslationMap.get(attribute), value);
      }
      return pristineListingAttribute;
    } else {
      return attributes;
    }
  }

  @Override
  public String[] getCategoryListingParameterKey(PristineDataItem pristineDataItem) throws IOException {
    String categoryAttributes = categoryListingParametersMap.get(pristineDataItem.getPristineCategory());
    if (StringUtils.isNotBlank(categoryAttributes)) {
      return categoryAttributes.split(COMMA_DELIMITER);
    }
    return null;
  }

  @Override
  public Map<String, String> getAttributeNameTranslationMap() throws IOException {
    return attributeNameTranslationMap;
  }
}
