package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.entity.Attribute;

/**
 * Created by govind on 06/11/2018 AD.
 */
public interface AttributeRepositoryCustom {

  /**
   * Get attribute list by using filter and pageable.
   *
   * @param storeId         String
   * @param attributeType   AttributeType
   * @param name            String
   * @param sortByFieldName String
   * @param sortOrder       String
   * @param pageable        Pageable
   * @param sizeAttribute   Boolean
   * @return Page<Attribute>
   */
  Page<Attribute> findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(
    String storeId, AttributeType attributeType, String name, String sortByFieldName,
    String sortOrder, Pageable pageable, Boolean sizeAttribute);
}
