package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.Dimension;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface DimensionRepository
    extends JpaRepository<Dimension, String>, DimensionRepositoryCustom {

  /**
   * fetch the dimension based on dimension name
   *
   * @param storeId
   * @param name
   * @return
   */
  Dimension findByStoreIdAndNameIgnoreCase(String storeId, String name);

  /**
   * get the sequence of current dimension code
   *
   * @param key
   * @return
   */
  @Query(value = "select get_sequence(?1) as sequence", nativeQuery = true)
  long getSequenceByAttributeCode(String key);

  Dimension findByStoreIdAndMarkForDeleteAndDimensionCode(String storeId, boolean markForDelete,
      String dimensionCode);

  /**
   * to find dimension
   *
   * @param storeId
   * @param id
   * @return
   */
  Dimension findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);
}
