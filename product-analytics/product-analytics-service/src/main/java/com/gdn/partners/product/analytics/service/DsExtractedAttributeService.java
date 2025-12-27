package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import model.AttributeUpdateEventModel;

public interface DsExtractedAttributeService {

  /**
   * to update ds extracted attribute
   *
   * @param attributeUpdateEventModel attributeUpdateEventModel
   */
  void updateDsExtractedAttribute(AttributeUpdateEventModel attributeUpdateEventModel);

  /**
   * get ds extracted attribute by attribute code
   *
   * @param attributeCode
   * @return
   */
  DSExtractionEntity getDsExtractedAttributeByAttributeCode(String attributeCode);
}
