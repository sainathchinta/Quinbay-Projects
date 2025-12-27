package com.gdn.x.productcategorybase.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;

public interface PredefinedAllowedAttributeValueRepository
    extends JpaRepository<PredefinedAllowedAttributeValue, String> {
  
  String QUERY_DELETE_BY_ATTRIBUTE_ID_AND_CODE =
      "UPDATE pcc_predefined_allowed_attribute_value SET mark_for_delete = TRUE, updated_date = CURRENT_TIMESTAMP WHERE store_id = ?1 AND attribute_id = ?2 AND predefined_allowed_attribute_code = ?3";
  
  String QUERY_UNDELETE_BY_ATTRIBUTE_ID_AND_CODE =
      "UPDATE pcc_predefined_allowed_attribute_value SET mark_for_delete = FALSE, updated_date = CURRENT_TIMESTAMP WHERE store_id = ?1 AND attribute_id = ?2 AND predefined_allowed_attribute_code = ?3";

  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute,
      String value);

  PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId,
      Attribute attribute, String value);
  
  PredefinedAllowedAttributeValue findTopByStoreIdAndAttributeAttributeCodeAndValueAndMarkForDeleteFalseOrderByUpdatedDateDesc(
      String storeId, String attributeCode, String value);

  PredefinedAllowedAttributeValue findByStoreIdAndAttributeAttributeCodeAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(
      String storeId, String attributeCode, String predefinedAllowedAttributeCode);
  
	List<PredefinedAllowedAttributeValue> findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(
			List<String> attributeCodes, List<String> values);

  PredefinedAllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId,
      Attribute attribute, String value);

  Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
      String storeId, String attributeId, String value, Pageable pageable);

  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(String storeId,
      String attributeId, String value);

  PredefinedAllowedAttributeValue findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  List<PredefinedAllowedAttributeValue> findByStoreIdAndIdInAndMarkForDeleteFalse(String storeId, Set<String> ids);

  Page<PredefinedAllowedAttributeValue> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  /**
   * get predefined allowed attribute values based on attribute id and sort it based on sequence
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   */
  Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(
      String storeId, String attributeId, Pageable pageable);

  /**
   * get get predefined allowed attribute values based on attribute id and sort it based on value
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   */
  Page<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(String storeId,
      String attributeId, Pageable pageable);

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByAttributeCode(String attributeCode);
  
  PredefinedAllowedAttributeValue findByStoreIdAndPredefinedAllowedAttributeCodeAndMarkForDeleteFalse(String storeId,
      String predefinedAllowedAttributeCode);

  List<PredefinedAllowedAttributeValue> findByStoreIdAndPredefinedAllowedAttributeCodeInAndMarkForDeleteFalse(
      String storeId, Set<String> predefinedAllowedAttributeCodes);
  
  @Modifying(clearAutomatically = false)
  @Query(value = PredefinedAllowedAttributeValueRepository.QUERY_DELETE_BY_ATTRIBUTE_ID_AND_CODE, nativeQuery = true)
  void deleteByStoreIdAndAttributeIdAndCode(String storeId, String attributeId, String code) throws Exception;
  
  @Modifying(clearAutomatically = false)
  @Query(value = PredefinedAllowedAttributeValueRepository.QUERY_UNDELETE_BY_ATTRIBUTE_ID_AND_CODE, nativeQuery = true)
  void undeleteByStoreIdAndAttributeIdAndCode(String storeId, String attributeId, String code) throws Exception;

  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalse(
      String storeId, String attributeId);

  List<PredefinedAllowedAttributeValue> findByStoreIdAndIdIn(String storeId, Set<String> ids);

  /**
   * get get predefined allowed attribute values based on attribute id and value and sort it based on mark for delete
   *
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId,
      Attribute attribute, String value);

  /**
   * Fetch predefined values by ignore case
   *
   * @param storeId     storeId String
   * @param attributeId attributeId String
   * @param value       value to query with lower case
   * @return Return the value
   */
  List<PredefinedAllowedAttributeValue> findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
    String storeId, String attributeId, String value);
}
