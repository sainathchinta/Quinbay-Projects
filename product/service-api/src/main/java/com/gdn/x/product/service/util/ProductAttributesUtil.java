package com.gdn.x.product.service.util;

import java.io.IOException;
import java.util.Map;

import com.gdn.x.product.model.entity.PristineDataItem;

public interface ProductAttributesUtil {

  /**
   * Translates pristine listing attribute name as per the category listing parameter key
   * @param attributes
   * @param categoryListingParameterKey
   * @return attributesMap with translated key name
   * @throws IOException
   */
  Map<String, String> translatePristineListingAttributeName(Map<String, String> attributes,
                                                            String[] categoryListingParameterKey);

  /**
   * Gets category listing parameter key
   * @param pristineDataItem
   * @return categoryListingParameterkey
   * @throws IOException
   */
  String[] getCategoryListingParameterKey(PristineDataItem pristineDataItem) throws IOException;

  /**
   * Gets attribute name translation map
   * @return attributeNameTranslationMap
   * @throws IOException
   */
  Map<String, String> getAttributeNameTranslationMap() throws IOException;

}
