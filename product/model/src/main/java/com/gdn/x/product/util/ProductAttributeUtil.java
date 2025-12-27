package com.gdn.x.product.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.ProductAttributeDetail;

/**
 * Created by govind on 09/08/2018 AD.
 */
public class ProductAttributeUtil {

  /**
   *  Get descriptive attributes detail for a product
   * @param masterDataProductAttributes
   * @return
   */
  public static List<ProductAttributeDetail> fetchDescriptiveAttributeFromMasterDataProduct(
      List<MasterDataProductAttribute> masterDataProductAttributes) {
    List<ProductAttributeDetail> attributeDetails = new ArrayList<ProductAttributeDetail>();
    if (CollectionUtils.isNotEmpty(masterDataProductAttributes)) {
      masterDataProductAttributes.stream().forEach(productAttribute -> {
        MasterDataAttribute attribute = productAttribute.getMasterDataAttribute();
        if (Objects.nonNull(attribute) && !attribute.isSkuValue()) {
          if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attribute.getAttributeType())) {
            getDescriptiveAttribute(productAttribute, attributeDetails);
          } else if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE
              .equals(attribute.getAttributeType())) {
            getPredefinedAttribute(productAttribute, attributeDetails);
          }
        }
      });
    }
    return attributeDetails;
  }

  /**
   * Get descriptive attribute
   * @param productAttribute
   * @param attributeDetails
   */
  private static void getDescriptiveAttribute(MasterDataProductAttribute productAttribute,
      List<ProductAttributeDetail> attributeDetails) {
    MasterDataAttribute attribute = productAttribute.getMasterDataAttribute();
    productAttribute.getMasterDataProductAttributeValues().stream()
        .filter(attributeValue -> !attributeValue.isMarkForDelete())
        .forEach(attributeValue -> {
      attributeDetails.add(
          new ProductAttributeDetail(attribute.getAttributeCode(), attribute.getAttributeName(),
              attributeValue.getDescriptiveAttributeValue()));
    });
  }

  /**
   * Get Predefined Attribute
   * @param productAttribute
   * @param attributeDetails
   */
  private static void getPredefinedAttribute(MasterDataProductAttribute productAttribute,
      List<ProductAttributeDetail> attributeDetails) {
    MasterDataAttribute attribute = productAttribute.getMasterDataAttribute();
    productAttribute.getMasterDataProductAttributeValues().stream()
        .filter(attributeValue -> !attributeValue.isMarkForDelete())
        .forEach(attributeValue -> {
          String value = Optional.ofNullable(attributeValue.getPredefinedAllowedAttributeValue())
              .map(predefinedValue -> predefinedValue.getValue())
              .orElse(null);
          String valueEn = Optional.ofNullable(attributeValue.getPredefinedAllowedAttributeValue())
              .map(PredefinedAllowedAttributeValue::getValueEn)
              .orElse(null);
          attributeDetails.add(new ProductAttributeDetail(attribute.getAttributeCode(),
              attribute.getAttributeName(), value, valueEn));
        });
  }
}
