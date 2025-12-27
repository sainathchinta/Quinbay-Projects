package com.gdn.x.productcategorybase.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;

public interface AllowedAttributeValueRepository extends JpaRepository<AllowedAttributeValue, String> {
  String OPTION_SUMMARY_DTO =
      "com.gdn.x.productcategorybase.dto.AttributeOptionDTO";

  List<AllowedAttributeValue> findByStoreIdAndAttributeAndValue(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndAttributeAndValueAndMarkForDeleteTrue(String storeId, Attribute attribute,
      String value);

  AllowedAttributeValue findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  List<AllowedAttributeValue> findByStoreIdAndIdInAndMarkForDeleteFalse(String storeId, Set<String> ids);

  Page<AllowedAttributeValue> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  /**
   * Get allowed attribute values based on attribute id and sort it based on sequence
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   */
  Page<AllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderBySequenceAsc(String storeId, String attributeId,
      Pageable pageable);

  /**
   * Get allowed attribute values based on attribute id and sort it alphabetically based on value
   *
   * @param storeId
   * @param attributeId
   * @param pageable
   * @return
   */
  Page<AllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalseOrderByValueAsc(String storeId, String attributeId,
      Pageable pageable);

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByAttributeCode(String attributeCode);
  
  List<AllowedAttributeValue> findByAttributeAttributeCodeInAndValueInAndMarkForDeleteFalse(
      List<String> attributeCodes, List<String> values);
  
  /**
   * Get defining attribute options by given attributeCode and search keyword
   * @param attributeCode
   * @param keyword
   * @return
   */
  @Query("select new " + OPTION_SUMMARY_DTO + "(COALESCE(av.value, ''), av.allowedAttributeCode) "
      + "from AllowedAttributeValue av JOIN av.attribute a "
      + "where av.markForDelete = false and a.attributeCode = :attributeCode and LOWER(av.value) like %:keyword% "
      + "order by av.value")
  Page<AttributeOptionDTO> getDefiningAttributeOptionByAttributeCodeAndKeyword(
      @Param("attributeCode") String attributeCode, @Param("keyword") String keyword, Pageable pageable);
  
  /**
   * Get predefine attribute options by given attributeCode and search keyword
   * @param attributeCode
   * @param keyword
   * @return
   */
  @Query("select new " + OPTION_SUMMARY_DTO
      + "(COALESCE(pav.value, ''), pav.predefinedAllowedAttributeCode) "
      + "from PredefinedAllowedAttributeValue pav JOIN pav.attribute a "
      + "where pav.markForDelete = false and a.attributeCode = :attributeCode and LOWER(pav.value) like %:keyword% "
      + "order by pav.value")
  Page<AttributeOptionDTO> getPredefineAttributeOptionByAttributeCodeAndKeyword(
      @Param("attributeCode") String attributeCode, @Param("keyword") String keyword, Pageable pageable);

  List<AllowedAttributeValue> findByStoreIdAndAttributeIdAndMarkForDeleteFalse(String storeId, String attributeId);

  AllowedAttributeValue findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(String storeId, String attributeId,
      String value);

  List<AllowedAttributeValue> findByStoreIdAndIdIn(String storeId, Set<String> ids);

  /**
   * Get allowed attribute value based on allowed attribute code
   *
   * @param storeId
   * @param allowedAttributeCode
   * @return
   */
  AllowedAttributeValue findByStoreIdAndAllowedAttributeCode(String storeId, String allowedAttributeCode);


  /**
   * Get allowed attribute value based on attribute and value
   *
   * @param storeId
   * @param attribute
   * @param value
   * @return
   */
  List<AllowedAttributeValue> findByStoreIdAndAttributeAndValueOrderByMarkForDelete(String storeId, Attribute attribute,
      String value);
}
