package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.AttributeTypeDTO;
import com.gdn.x.productcategorybase.entity.Attribute;

public interface AttributeRepository extends JpaRepository<Attribute, String> , AttributeRepositoryCustom{
   String ATTRIBUTE_SUMMARY_DTO =
      "com.gdn.x.productcategorybase.dto.AttributeSummaryDTO";
  String ATTRIBUTE_BASIC_DETAIL_DTO =
      "com.gdn.x.productcategorybase.dto.AttributeBasicDetailDTO";

  List<Attribute> findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String attributeCode);

  Page<Attribute> findByStoreIdAndAttributeCodeContainingIgnoreCaseAndMarkForDeleteFalse(String storeId,
      String attributeCode, Pageable pageable);
  
  List<Attribute> findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(String storeId, List<String> attributeCodes);

  List<Attribute> findByStoreIdAndNameAndMarkForDeleteFalse(String storeId, String name);

  List<Attribute> findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(String storeId, AttributeType attributeType);

  Page<Attribute> findByStoreIdAndAttributeTypeAndMarkForDeleteFalse(String storeId, AttributeType attributeType,
      Pageable pageable);

  Attribute findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  Attribute findByStoreIdAndAttributeCode(String storeId, String attributeCode);

  List<Attribute> findByStoreIdAndMarkForDeleteFalse(String storeId);

  List<Attribute> findByIdInAndStoreIdAndMarkForDeleteFalse(List<String> idList, String storeId);

  Page<Attribute> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  Page<Attribute> findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(String storeId, String name,
      Pageable pageable);

  @Query(value = "SELECT a FROM Attribute a WHERE a.storeId = :storeId AND a.name LIKE :name% AND a.markForDelete = false")
  Page<Attribute> findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(@Param("storeId")String storeId, @Param("name") String name, Pageable pageable);

  List<Attribute> findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(String storeId);

  Page<Attribute> findByStoreIdAndSearchAbleFalseAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<Attribute> findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(String storeId);

  Page<Attribute> findByStoreIdAndSearchAbleTrueAndMarkForDeleteFalse(String storeId, Pageable pageable);

  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByAttributeCode(String attributeCode);

  @Query(value = "select cat.category_code, cat.name cat_name, a.attribute_code, a.attribute_type, a.name attr_name, " +
      "a.is_sku_value, a.is_basic_view, COALESCE(av.value, '') defined_value, COALESCE(pav.value, '') predefined_value, "
      + "a.variant_creation, a.mandatory, a.variant_creating_ui, "
      + "cat.name_english cat_name_english, a.name_english attr_name_english, COALESCE(av.value_type, '') value_type "
      + "from pcc_category cat "
      + "left join pcc_category_attribute c on c.category_id = cat.id and c.mark_for_delete = false "
      + "left join pcc_attribute a on a.id = c.attribute_id and a.mark_for_delete = false "
      + "left join pcc_allowed_attribute_value av on a.id = av.attribute_id and av.mark_for_delete = false "
      + "left join pcc_predefined_allowed_attribute_value pav on a.id = pav.attribute_id and pav.mark_for_delete = false "
      + "where cat.category_code = :categoryCode and a.name != 'Brand' "
      + "order by c.sequence, a.name, av.sequence ", nativeQuery = true)
    List<Object[]> getAttributeDetailByCategoryCode(@Param("categoryCode") String categoryCode);
    
  @Query("select new " + ATTRIBUTE_SUMMARY_DTO
      + "(a.attributeCode, a.attributeType, a.name, a.isBasicView, a.skuValue, a.mandatory, a.screeningMandatory,"
      + "a.variantCreatingUI, a.variantCreation) "
      + "from CategoryAttribute c JOIN c.category cat JOIN c.attribute a "
      + "where c.markForDelete = false and cat.categoryCode = :categoryCode and a.name != 'Brand' and a.storeId = :storeId "
      + "order by c.sequence, a.name")
  List<AttributeSummaryDTO> getAttributeDetailByCategoryCodeWithoutOption(@Param("storeId") String storeId,
      @Param("categoryCode") String categoryCode);

  @Query(value = "select new com.gdn.x.productcategorybase.dto.AttributeTypeDTO(a.attributeCode, a.id,"
      + " a.attributeType, a.sortType, a.mandatory) from Attribute a where a.storeId = :storeId "
      + "and a.attributeCode = :attributeCode and a.markForDelete = false")
  AttributeTypeDTO getAttributeTypeInfoByAttributeCode(@Param("storeId") String storeId,
      @Param("attributeCode") String attributeCode);

  List<Attribute> findByStoreIdAndAttributeTypeAndNameOrNameEnglishAndMarkForDeleteFalseOrderByCreatedDate(
      String storeId, AttributeType attributeType, String name, String nameEnglish);

  @Query("select new " + ATTRIBUTE_BASIC_DETAIL_DTO + "(a.attributeCode, a.attributeType, a.sizeAttribute) "
      + "from CategoryAttribute c JOIN c.category cat JOIN c.attribute a "
      + "where c.markForDelete = false and cat.categoryCode = :categoryCode and a.name != 'Brand' and a.storeId = :storeId "
      + "order by c.sequence, a.name")
  List<AttributeBasicDetailDTO> getAttributeBasicDetailByCategoryCode(@Param("storeId") String storeId,
      @Param("categoryCode") String categoryCode);

  Attribute findByStoreIdAndDsAttributeNameAndMarkForDeleteFalse(String storeId,
      String dsAttributeName);

  @Query(value = "select cat.category_code, cat.name cat_name, a.attribute_code, a.attribute_type, a.name attr_name, " +
    "a.is_sku_value, a.is_basic_view, COALESCE(av.value, '') defined_value, COALESCE(pav.value, '') predefined_value, "
    + "a.variant_creation, a.mandatory, a.variant_creating_ui, "
    + "cat.name_english cat_name_english, a.name_english attr_name_english, COALESCE(av.value_type, '') value_type "
    + "from pcc_category cat "
    + "left join pcc_category_attribute c on c.category_id = cat.id and c.mark_for_delete = false "
    + "left join pcc_attribute a on a.id = c.attribute_id and a.mark_for_delete = false and a.ds_extraction = false "
    + "left join pcc_allowed_attribute_value av on a.id = av.attribute_id and av.mark_for_delete = false "
    + "left join pcc_predefined_allowed_attribute_value pav on a.id = pav.attribute_id and pav.mark_for_delete = false "
    + "where cat.category_code = :categoryCode and a.name != 'Brand' "
    + "order by c.sequence, a.name, av.sequence ", nativeQuery = true)
  List<Object[]> getAttributeDetailByCategoryCodeIgnoreDs(@Param("categoryCode") String categoryCode);

}
